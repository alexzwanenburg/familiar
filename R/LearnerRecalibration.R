learner.recalibrate_model <- function(object, data_obj, time_max=NULL) {

  if(class(object) != "familiarModel"){
    stop("Only familiarModel objects can be recalibrated.")
  }
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- outcome_pred <- NULL

  # Extract constants
  outcome_type <- object@outcome_type

  # Number of cross-validation folds.
  n_cv_folds        <- 3

  # Initial empty prediction list and empty calibration list
  pred_list         <- list()
  calibration_list  <- list()

  # Create a 3-fold cross-validation
  iter_list         <- .create_cv(sample_identifiers=data_obj@data$subject_id, n_folds=n_cv_folds,
                                  outcome_type=outcome_type, data=data_obj@data, stratify=TRUE)

  # Train new models using the learner. This is done to avoid biasing the calibration mode. The generated models are used to predict validation data, which are
  # subsequently used to calibrate the logistic linking function.
  for(ii in seq_len(n_cv_folds)){

    # Select training data and test data
    data_train      <- select_data_from_samples(data=data_obj, samples=iter_list$train_list[[ii]])
    data_test       <- select_data_from_samples(data=data_obj, samples=iter_list$valid_list[[ii]])

    # Create familiar model
    cv_model        <- methods::new("familiarModel",
                                    outcome_type = object@outcome_type,
                                    learner = object@learner,
                                    hyperparameters = object@hyperparameters,
                                    signature = object@signature,
                                    req_feature_cols = object@req_feature_cols,
                                    class_levels = object@class_levels)

    # Train the model - the recalibration flag is set to FALSE to avoid an infinite loop, as this would involve calling learner.calibrate_model again.
    cv_model        <- train(object=cv_model, data=data_train, get_recalibration=FALSE, get_additional_info=FALSE)

    # Generate a prediction table
    pred_list[[ii]] <- predict(object=cv_model, newdata=data_test, allow_recalibration=FALSE, time_max=time_max)

  }

  # Combine prediction tables to one table
  dt_pred           <- data.table::rbindlist(pred_list)

  rm(data_train, data_test, cv_model, pred_list)

  if(outcome_type %in% c("binomial", "multinomial")){
    # Get class levels and class probability column names
    class_levels    <- object@class_levels
    prob_cols       <- getClassProbabilityColumns(dt=dt_pred, outcome_type=outcome_type)

    # Build a logistic model on top of the predicted class probabilities for each predicted class probability
    for(ii in seq_len(length(prob_cols))){
      # Select current probability column
      curr_prob_col <- prob_cols[ii]

      # Set positive class flag
      dt_pred[, "pos_class":= outcome==class_levels[ii]]

      # Parse formula
      model_formula <- stats::reformulate(termlabels=curr_prob_col, response="pos_class")

      # Remove NA from the data.table
      dt_pred       <- dt_pred[is.finite(get(curr_prob_col)), ]

      # If the prediction data table returns no or just one (valid) entry, calibration is not possible.
      if(nrow(dt_pred) <= 1){ return(NULL) }

      # Create calibration model
      calibration_list[[curr_prob_col]] <- stats::glm(model_formula, data=dt_pred, family=stats::binomial(link="logit"))
    }

  } else if(outcome_type == "survival"){

    # Parse formula
    formula        <- stats::reformulate(termlabels="outcome_pred", response=quote(survival::Surv(outcome_time, outcome_event)))

    # Remove NA from the table
    dt_pred        <- dt_pred[is.finite(outcome_pred)]

    # If the prediction data table returns no or just one (valid) entry, calibration is not possible.
    if(nrow(dt_pred) <= 1){ return(NULL) }

    # Generate model
    model_ctrl     <- survival::coxph.control(iter.max=100)
    model_obj      <- tryCatch({ coxph(formula, data=dt_pred, control=model_ctrl, y=FALSE) }, error=function(err) { return(NULL) } )

    # Return NULL if the model cannot be trained
    if(is.null(model_obj)) { return(NULL) }

    calibration_list[[1]] <- model_obj
  }

  # Return list of calibration models
  return(calibration_list)
}




learner.apply_calibration <- function(dt, learner, outcome_type, calibration_model){

  # Suppress NOTES due to non-standard evaluation in data.table
  prob_sum <- NULL

  # Return the input outcome data table dt if calibration models are missing
  if(is.null(calibration_model)) {
    return(dt)
  }

  if(outcome_type %in% c("binomial", "multinomial")){
    # Determine probability columns
    prob_cols <- getClassProbabilityColumns(dt=dt, outcome_type=outcome_type)

    # Iterate over calibration models and reconstruct the outcome data table
    for(ii in seq_len(length(prob_cols))){
      # Get name of current probability column
      curr_prob_col <- prob_cols[ii]

      # Predict calibrated probabilities using the calibration model for the current column.
      pred_prob     <- stats::predict.glm(object=calibration_model[[curr_prob_col]], newdata=dt, type="response")

      # Replace column contents with predicted probabilities.
      dt[, (curr_prob_col):=pred_prob]
    }

    # Normalise predicted probabilities to 1
    dt[, "prob_sum":=rowSums(.SD, na.rm=TRUE), .SDcols=prob_cols]
    dt[, (prob_cols):=lapply(.SD, "/", prob_sum), .SDcols=prob_cols]

    # Drop sum of probabilities
    dt[, "prob_sum":=NULL]

    # Update predicted outcome with class with maximum predicted probability
    class_levels   <- get_outcome_levels(x=dt, outcome_type=outcome_type)
    max_prob_class <- factor(class_levels[dt[, max.col(.SD), .SDcols=prob_cols]], levels=class_levels)
    dt[, "outcome_pred_class":=max_prob_class]

  } else if(outcome_type == "survival") {

    # Predict cox PH relative risk
    pred_outc  <- predict(object=calibration_model[[1]], newdata=dt, type="risk")

    # Replace in table
    dt[, "outcome_pred":=pred_outc]
  }
  return(dt)
}

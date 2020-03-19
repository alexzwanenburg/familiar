learner.survreg.outcome <- function(learner=NULL, outcome_type=NULL, object=NULL){

  if(!is.null(object)){
    learner      <- object@learner
    outcome_type <- object@outcome_type
  }

  if(outcome_type=="survival"){
    return(TRUE)
  } else {
    return(FALSE)
  }
}



learner.survreg.param <- function(data_obj, learner){

  # Initialise list and declare hyperparameter entries
  param <- list()
  param$sign_size <- list()
  param$distribution <- list()

  # If data_obj is explicitly set to NULL, return the list with hyperparameter names only
  if(is.null(data_obj)) { return(param) }


  ##### Signature size #########################################################
  param$sign_size <- .get_default_sign_size(data_obj=data_obj, restrict_samples=TRUE)


  ##### Outcome distribution ###################################################
  
  # Randomisation of distribution depends on selected learner.
  if(learner=="survival_regr"){
    distribution_default <- c("weibull", "exponential", "gaussian", "logistic", "loglogistic", "lognormal")
  
  } else {
    distribution_default <- stringi::stri_replace_first_fixed(str=learner, pattern="survival_regr_", replacement="")
  }
  
  # Set the distribution parameter
  param$distribution <- .set_hyperparameter(default=distribution_default, type="factor", range=distribution_default,
                                            randomise=TRUE)
  
  # Return hyper-parameters
  return(param)
}



learner.survreg.train <- function(object, data_obj){

  # Extract information
  outcome_type <- object@outcome_type
  param        <- object@hyperparameters

  # Initiate model_list list
  model_list   <- list()

  # Use effect coding to convert categorical data into encoded data
  contr_list   <- getContrasts(dt=data_obj@data, method="dummy", drop_levels=FALSE, outcome_type=outcome_type)

  # Extract data table with contrasts
  dt           <- contr_list$dt_contrast

  # Find feature columns in data table
  feature_cols <- get_feature_columns(x=dt, outcome_type=outcome_type)

  # Parse formula
  formula      <- stats::reformulate(termlabels=feature_cols, response=quote(Surv(outcome_time, outcome_event)))

  # Generate model -- NOTE: survreg is imported directly to allow access to the predict generic which was not exported by the survival package
  model_ctrl   <- survival::survreg.control(iter.max=100)
  model_obj    <- tryCatch({ survreg(formula, data=dt, control=model_ctrl, y=FALSE, dist=param$distribution) }, error=function(err) { return(NULL) } )

  # Append model to list
  model_list$model <- model_obj

  # Update model status
  if(is.null(model_obj)) { model_list$model_trained <- FALSE }
  else {                   model_list$model_trained <- TRUE }

  # Add the contrast references
  model_list$contrast_ref <- contr_list$dt_ref

  return(model_list)
}



learner.survreg.test <- function(object, data_obj, extra_output){

  # Extract information
  outcome_type <- object@outcome_type

  # Use effect coding to convert categorical data into encoded data
  contr_list   <- getContrasts(dt=data_obj@data, method="dummy", drop_levels=FALSE, outcome_type=outcome_type)

  # Extract data table with contrasts
  dt           <- contr_list$dt_contrast

  # Generate skeleton of prediction data table from dt
  dt_pred <- dt[, get_non_feature_columns(x=object), with=FALSE]

  if(model_is_trained(object=object)){
    # Predict outcome for new data
    pred_outc <- predict(object=object@model$model, newdata=dt)

    # Perform consistency checks for valid outcomes and convergence
    if(any(is.finite(pred_outc)) & object@model$model$iter < 100 ){
      pred_success <- TRUE
    } else {
      pred_success <- FALSE
    }
  } else {
    pred_success <- FALSE
  }

  # Update prediction data table with predicted values
  if(pred_success){
    dt_pred$outcome_pred <- pred_outc

    # Produce additional output, i.e. time to failure for different survival quantiles
    if(extra_output){

      # Survival quantiles from 1.00 to 0.01
      surv_quantiles  <- seq(from=1.00, to=0.01, by=-0.01)

      # Get estimated failure times
      failure_times <- predict(object=object@model$model, newdata=dt, type="quantile", p=1-surv_quantiles)
      
      # Parse failure times to a matrix in a single-sample result
      if(!is.matrix(failure_times)){
        failure_times <- matrix(data=failure_times, ncol=length(failure_times))
      }
      
      # Cast to data.table
      failure_table <- cbind(data.table::data.table("subject_id"=dt_pred$subject_id),
                             data.table::as.data.table(failure_times))
      
      # Melt to long format
      failure_table <- data.table::melt(failure_table, id.vars="subject_id", variable.name="quantile_variable",
                                        value.name="failure_time")
      
      # Create conversion table to convert temporary variables into survival quantiles
      conversion_table <- data.table::data.table("quantile_variable"=paste0("V", seq_len(length(surv_quantiles))),
                                                 "quantile"=surv_quantiles)
      # Add in survival quantiles
      failure_table <- merge(x=failure_table, y=conversion_table, on="quantile_variable")
      
      # Drop the quantile_variable column
      failure_table[, "quantile_variable":=NULL]

      # Generate a prediction list
      prediction_list <- list("predictions"   = dt_pred,
                              "failure_times" = failure_table)

      return(prediction_list)
    }

  } else {
    dt_pred <- createNonValidPredictionTable(dt=dt, outcome_type=outcome_type)

    # Generate an empty prediction list
    if(extra_output){
      prediction_list <- list("predictions"   = dt_pred,
                              "failure_times" = NULL)

      return(prediction_list)
    }
  }

  # Return prediction data table
  return(dt_pred)
}


learner.survreg.vimp <- function(object){

  # Generate variable importance data table
  if(model_is_trained(object=object)){
    coef_p_val        <- regrLocTest(regr_fit_obj=object@model$model)
    coef_p_val        <- coef_p_val[names(coef_p_val)!="(Intercept)"]
    coef_names        <- names(coef_p_val)[names(coef_p_val)!="(Intercept)"]

    dt_vimp           <- data.table::data.table("score"=coef_p_val, "name"=coef_names)
    dt_vimp           <- applyContrastReference(dt=dt_vimp, dt_ref=object@model$contrast_ref, method="min")
    dt_vimp$rank      <- data.table::frank(dt_vimp$score, ties.method="min")
    dt_vimp$multi_var <- TRUE
  } else {
    dt_vimp           <- getEmptyVimp()
  }

  return(dt_vimp)
}



learner.survreg.prediction_type <- function(){

  # Survival regression models predict expected survival times, not risks
  return("expected_survival_time")
}



learner.survreg.calibration_info <- function(object, data_obj, time_max=NULL){

  if(object@outcome_type=="survival" & model_is_trained(object=object)) {

    # Determine baseline survival.
    calibration_info <- learner.survival.baseline_survival(data_obj=data_obj)

  } else {
    calibration_info <- NULL
  }

  return(calibration_info)
}



learner.survreg.get_calibration <- function(object, data_obj, time_max){

  if(object@outcome_type=="survival"){
    # Get survival probabilities
    probability_table <- learner.survival_probability.accelerated_failure_time(object=object,
                                                                               data_obj=data_obj,
                                                                               time_max=time_max)
    
    # Obtain calibration data.
    calibration_table <- learner.calibration.survival(object=object,
                                                      probability_table=probability_table,
                                                      time_max=time_max)
  }

  return(calibration_table)
}

learner.cox.outcome <- function(learner=NULL, outcome_type=NULL, object=NULL){

  if(!is.null(object)){
    learner      <- object@learner
    outcome_type <- object@outcome_type
  }

  if(outcome_type == "survival"){
    return(TRUE)
  } else {
    return(FALSE)
  }
}



learner.cox.param <- function(data_obj){

  # Initialise list and declare hyperparameter entries
  param    <- list()
  param$sign_size <- list()

  # If data_obj is explicitly NULL, return the list with hyperparameter names only
  if(is.null(data_obj)) { return(param) }

  ##### Signature size #####
  param$sign_size <- .get_default_sign_size(data_obj=data_obj, restrict_samples=TRUE)

  return(param)
}



learner.cox.train <- function(object, data_obj){

  # Extract outcome type
  outcome_type <- object@outcome_type

  # Initiate model_list list
  model_list   <- list()

  # Use effect coding to convert categorical data into encoded data - this is required to deal with factors with missing/new levels
  # between training and test data sets.
  contr_list   <- getContrasts(dt=data_obj@data, method="dummy", drop_levels=FALSE, outcome_type=outcome_type)

  # Extract data table with contrasts
  dt           <- contr_list$dt_contrast

  # Find feature columns in data table
  feature_cols <- get_feature_columns(x=dt, outcome_type=outcome_type)

  # Parse formula
  formula      <- stats::reformulate(termlabels=feature_cols, response=quote(survival::Surv(outcome_time, outcome_event)))

  # Generate model -- NOTE: coxph was directly imported to allow access to predict and summary functions that were not exported in survival
  model_ctrl   <- survival::coxph.control(iter.max=100)
  model_obj    <- tryCatch({ coxph(formula, data=dt, control=model_ctrl, y=FALSE) }, error=function(err) { return(NULL) } )

  # Append model to list
  model_list$model <- model_obj

  # Update model status based on existence and timely convergence
  if(is.null(model_obj)){
    model_list$model_trained <- FALSE
  } else if(model_list$model$iter>=100){
    model_list$model_trained <- FALSE
  } else {
    model_list$model_trained <- TRUE
  }

  # Add the contrast references to model_list
  model_list$contrast_ref <- contr_list$dt_ref

  return(model_list)
}


learner.cox.vimp <- function(object){

  # Generate variable importance data table
  if(model_is_trained(object)){
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


learner.cox.test <- function(object, data_obj){

  # Determine if the model was trained correctly and predict new data
  if(model_is_trained(object)){

    # Extract the outcome type
    outcome_type <- object@outcome_type

    # Use effect coding to convert categorical data into encoded data
    contr_list <- getContrasts(dt=data_obj@data, method="dummy", drop_levels=FALSE, outcome_type=outcome_type)

    # Extract data table with contrasts
    dt         <- contr_list$dt_contrast

    # Generate skeleton of prediction data table from dt
    dt_pred    <- dt[, get_non_feature_columns(x=object), with=FALSE]

    # Predict outcome for new data
    pred_outc  <- predict(object=object@model$model, newdata=dt, type="risk")

    # Update prediction data table with predicted values
    dt_pred$outcome_pred <- pred_outc

  } else {
    dt_pred    <- createNonValidPredictionTable(dt=data_obj@data, outcome_type=object@outcome_type)
  }

  # Return prediction data table
  return(dt_pred)
}



learner.cox.prediction_type <- function(){

  # Cox proportional hazards models predict relative risks
  return("hazard_ratio")
}



learner.cox.calibration_info <- function(object, data_obj, time_max=NULL){
  
  if(object@outcome_type == "survival"){
    # Determine baseline survival.
    calibration_info <- learner.survival.baseline_survival(data_obj=data_obj)
      
  } else {
    calibration_info <- NULL
  }

  return(calibration_info)
}



learner.cox.get_calibration <- function(object, data_obj, time_max){

  # Get survival probabilities
  probability_table <- learner.survival_probability.relative_risk(object=object, data_obj=data_obj, time_max=time_max)

  # Assess calibration using a standard function for models that predict relative risks
  calibration_table <- learner.calibration.survival(object=object, probability_table=probability_table, time_max=time_max)

  return(calibration_table)
}

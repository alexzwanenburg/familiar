learner.test_learner.outcome <- function(){
  return(TRUE)
}


learner.test_learner.param <- function(data_obj, learner){
  
  # Initialise list and declare hyperparameter entries
  param    <- list()
  param$sign_size <- list()
  param$family    <- list()
  param$risk_type <- list()
  
  # If no data object is not provided, return the list with hyperparameter names only
  if(is.null(data_obj)) { return(param) }
  
  ##### Signature size #########################################################
  param$sign_size <- .set_hyperparameter(default=1, type="integer", range=c(1,1),
                                         valid_range=c(1, 1), randomise=FALSE)
  
  
  ##### Model family ###########################################################
  
  # Read family string by parsing the learner.
  learner_name <- stringi::stri_replace_first_regex(str=learner, pattern="__test", replace="")
  learner_name <- stringi::stri_replace_first_regex(str=learner_name, pattern="_", replace="")
  
  # Split into family and (optional) risk type
  fam <- stringi::stri_split_fixed(str=learner_name, pattern="_", n=2)[[1]][1]
  
  # Set family parameter.
  param$family <- .set_hyperparameter(default=fam, type="factor", range=c("invalid", "perfect", "intercept", "missing"),
                                      randomise=FALSE)
  
  
  ##### Risk type for calibration testing ######################################
  risk_type <- learner.test_learner.get_test_risk_type(learner=learner, outcome_type=data_obj@outcome_type)
  
  # Set risk type parameter
  param$risk_type <- .set_hyperparameter(default=risk_type, type="factor",
                                         range=c("relative_risk", "accelerated_life"),
                                         randomise=FALSE)
  
  return(param)
}


learner.test_learner.get_test_risk_type <- function(learner, outcome_type){
  
  # Return "none" if the outcome is not a survival outcome.
  if(!outcome_type %in% c("survival")){
    return("none")
  }
  
  learner_name <- stringi::stri_replace_first_regex(str=learner, pattern="__test", replace="")
  learner_name <- stringi::stri_replace_first_regex(str=learner_name, pattern="_", replace="")
  
  # Split into family and (optional) risk type
  learner_split <- stringi::stri_split_fixed(str=learner_name, pattern="_", n=2)[[1]]
  
  if(length(learner_split) == 1){
    return("relative_risk")
    
  } else {
    return(learner_split[2])
  }
}


learner.test_learner.train <- function(object, data_obj){
  
  # Initialise a model list
  model_list   <- list()
  
  # Append model to list
  if(object@hyperparameters$family == "missing"){
    model_list$feature_name <- character(0)
    
  } else {
    model_list$model <- object@hyperparameters$family
    model_list$feature_name <- get_feature_columns(x=data_obj)
  }
  
  return(model_list)
}



learner.test_learner.vimp <- function(object){
  
  # Create a variable importance table.
  if(!model_is_trained(object=object)){
    vimp_table <- getEmptyVimp()
    
  } else if(object@model$model == "invalid"){
    vimp_table <- getEmptyVimp()
    
  } else {
    vimp_table <- data.table::data.table("score"=1,
                                         "name"=object@model$feature_name,
                                         "rank"=1,
                                         "multi_var"=TRUE)
  }

  return(vimp_table)
}



learner.test_learner.test <- function(object, data_obj, extra_output=FALSE){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome_time <- outcome <- outcome_pred_class <- NULL
  
  # Extract data
  outcome_type <- object@outcome_type
  
  if(!model_is_trained(object)){
    if(extra_output){
      return(list("predictions" = createNonValidPredictionTable(dt=data_obj@data, outcome_type=outcome_type)))
      
    } else {
      return(createNonValidPredictionTable(dt=data_obj@data, outcome_type=outcome_type))
    }
    
  } else if(object@model$model == "invalid") {
    if(extra_output){
      return(list("predictions"=createNonValidPredictionTable(dt=data_obj@data, outcome_type=outcome_type)))
      
    } else {
      return(createNonValidPredictionTable(dt=data_obj@data, outcome_type=outcome_type))
    }
    
  } else if(object@model$model == "perfect") {
    
    # Generate skeleton of prediction data table from the data object
    prediction_table <- data_obj@data[, get_non_feature_columns(x=object), with=FALSE]
    
    if(outcome_type %in% c("binomial", "multinomial")){

      # Extract class levels from the predictions
      class_levels <- object@class_levels
      
      # Copy outcome as the predicted class
      prediction_table[, "outcome_pred_class":=outcome]
      
      # Add class probabilities
      predicted_probability_cols <- get_class_probability_columns(outcome_type=outcome_type, class_levels=class_levels)
      
      # Fill out the probability columns
      for(ii in seq_along(class_levels)){
        prediction_table[, (predicted_probability_cols[ii]):=0.0]
        prediction_table[outcome_pred_class==class_levels[ii], (predicted_probability_cols[ii]):=1.0]
      }

    } else if(outcome_type %in% c("continuous", "count")){
      # Copy outcome as the predicted value.
      prediction_table[, "outcome_pred":=outcome]
      
    } else if(outcome_type %in% c("survival")){
      
      # Get the risk type
      risk_type <- learner.test_learner.get_test_risk_type(learner=object@learner, outcome_type=object@outcome_type)
      
      # Copy outcome time as predicted time.
      if(risk_type == "relative_risk"){
        prediction_table[, "outcome_pred":=exp(-outcome_time)]
        
      } else if(risk_type == "accelerated_life"){
        prediction_table[, "outcome_pred":=outcome_time]
        
        if(extra_output){
          # TODO implement survreg like survival tables.
          browser()
          
          return(list("predictions"=prediction_table,
                      "failure_times"=NULL))
        }
        
      } else if(risk_type == "random_forest"){
        prediction_table[, "outcome_pred":=exp(-outcome_time)]
        
        if(extra_output){
          # TODO implement random-forest like survival tables.
          browser()
          
          return(list("predictions"=prediction_table,
                      "survival"=NULL))
        }
        
      } else {
        ..error_reached_unreachable_code("learner.test_learner.test_unknown_risk_type")
      }
      
    }

  } else if(object@model$model == "intercept"){
    
    # Generate skeleton of prediction data table from the data object
    prediction_table <- data_obj@data[, get_non_feature_columns(x=object), with=FALSE]
    
    if(outcome_type %in% c("binomial", "multinomial")){
      # Extract class levels from the predictions
      class_levels <- object@class_levels
      
      # Set the last class as predicted outcome
      prediction_table[, "outcome_pred_class":=tail(class_levels, n=1)]
      prediction_table$outcome_pred_class <- factor(prediction_table$outcome_pred_class, levels=class_levels)

      # Add class probabilities
      predicted_probability_cols <- get_class_probability_columns(outcome_type=outcome_type, class_levels=class_levels)
      
      # Fill out the probability columns
      for(ii in seq_along(class_levels)){
        prediction_table[, (predicted_probability_cols[ii]):=0.0]
        
        prediction_table[outcome_pred_class==class_levels[ii], (predicted_probability_cols[ii]):=1.0]
      }
      
    } else if(outcome_type %in% c("continuous", "count")){
      # Copy outcome as the predicted value.
      prediction_table[, "outcome_pred":=mean(outcome, na.rm=TRUE)]
      
    } else if(outcome_type %in% c("survival")){
      
      # Get the risk type
      risk_type <- learner.test_learner.get_test_risk_type(learner=object@learner, outcome_type=object@outcome_type)
      
      if(risk_type == "relative_risk"){
        # For relative risk intercept equals 0.0
        prediction_table[, "outcome_pred":=0.0]
        
        
      } else if(risk_type == "accelerated_life"){
        # Use mean outcome time for AFT models
        prediction_table[, "outcome_pred":=mean(outcome_time, na.rm=TRUE)]
        
        if(extra_output){
          # TODO implement survreg like survival tables.
          browser()
          
          return(list("predictions"=prediction_table,
                      "failure_times"=NULL))
        }
        
      } else if(risk_type == "random_forest"){
        # Use cumulative hazard of 1.0 for random forest.
        prediction_table[, "outcome_pred":=1.0]
        
        if(extra_output){
          # TODO implement random-forest like survival tables.
          browser()
          
          return(list("predictions"=prediction_table,
                      "survival"=NULL))
        }
        
      } else {
        ..error_reached_unreachable_code("learner.test_learner.test_unknown_risk_type")
      }
    }
  }
  
  # Return prediction table
  return(prediction_table)
}



learner.test_learner.get_calibration <- function(object, data_obj, time_max=NULL){
  
  # Extract outcome type from the familiar model.
  outcome_type <- object@outcome_type
  
  if(outcome_type %in% c("binomial", "multinomial")) {
    # Parse calibration
    calibration_table <- learner.calibration.categorical(object=object, data_obj=data_obj)
    
  } else if(outcome_type %in% c("continuous", "count")) {
    # Parse calibration
    calibration_table <- learner.calibration.regression(object=object, data_obj=data_obj)
    
  } else if(outcome_type %in% c("survival", "competing_risk")){
    
    # Find the risk type.
    risk_type <- learner.test_learner.get_test_risk_type(learner=object@learner, outcome_type=object@outcome_type)
    
    # Get survival probabilities.
    if(risk_type == "relative_risk"){
      probability_table <- learner.survival_probability.relative_risk(object=object, data_obj=data_obj, time_max=time_max)

    } else if(object@outcome_type=="survival" & risk_type=="accelerated_life"){
      probability_table <- learner.survival_probability.accelerated_failure_time(object=object, data_obj=data_obj, time_max=time_max)

    } else if(object@outcome_type=="survival" & risk_type=="random_forest"){
      probability_table <- learner.survival_probability.random_forest(object=object, data_obj=data_obj, time_max=time_max)
    }

    # Assess calibration using a standard function for models that predict relative risks
    calibration_table <- learner.calibration.survival(object=object, probability_table=probability_table, time_max=time_max)
  }
  
  return(calibration_table)
}


learner.test_learner.prediction_type <- function(learner, outcome_type){
  
  risk_type <- learner.test_learner.get_test_risk_type(learner=learner, outcome_type=outcome_type)
  
  # For survival outcomes, return hazard ratio.
  if(outcome_type=="survival" & risk_type == "relative_risk"){
    return("hazard_ratio")
    
  } else if(outcome_type %in% c("survival") & risk_type == "accelerated_life"){
    return("expected_survival_time")
    
  } else if(outcome_type %in% c("survival") & risk_type == "random_forest"){
    return("sum_cumulative_hazard")
    
  } else {
    return(NULL)
  }
}



learner.test_learner.calibration_info <- function(object, data_obj, time_max=NULL){
  
  if(object@outcome_type == "survival"){
    # Determine baseline survival.
    calibration_info <- learner.survival.baseline_survival(data_obj=data_obj)
    
  } else if(object@outcome_type %in% c("count", "continuous")) {
    # Determine range of outcomes.
    calibration_info <- learner.calibration.regression.outcome_range(data_obj=data_obj)
    
  } else {
    calibration_info <- NULL
  }
  
  return(calibration_info)
}

learner.naive_bayes.outcome <- function(learner=NULL, outcome_type=NULL, object=NULL){

  # Extract details from the familiar model
  if(!is.null(object)){
    outcome_type <- object@outcome_type
  }

  if(outcome_type %in% c("binomial", "multinomial")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}



learner.naive_bayes.param <- function(data_obj){

    # Initialise list and declare hyperparameter entries.
  param <- list()
  param$sign_size <- list()

  # If data_obj is explicitly set to NULL, return the list with hyperparameter
  # names only.
  if(is.null(data_obj)) { return(param) }

  ##### Signature size #########################################################
  param$sign_size <- .get_default_sign_size(data_obj=data_obj, restrict_samples=TRUE)

  return(param)
}



learner.naive_bayes.train <- function(object, data_obj){

  # Initiate model_list list
  model_list   <- list()

  # Find feature columns in data table
  feature_cols <- get_feature_columns(x=data_obj)

  # Select feature columns which lack variance
  feature_no_var_cols <- feature_cols[sapply(feature_cols, function(ii, dt) (is_singular_data(dt[[ii]])), dt=data_obj@data)]
  valid_feature_cols  <- feature_cols[!feature_cols %in% feature_no_var_cols]

  # Check if there is more than one valid feature column left
  if(length(valid_feature_cols)==0) {
    model_list$model_trained <- FALSE
  } else {
    # Generate a formula with only feature columns that have some variance
    formula           <- stats::reformulate(valid_feature_cols, response=quote(outcome))

    # Generate model -- NOTE: NaiveBayes is imported through NAMESPACE as the predict function is not exported by klaR
    model_obj         <- tryCatch({ NaiveBayes(formula, data=data_obj@data) }, error= function(err) { return(NULL) } )

    # Append model to list
    model_list$model  <- model_obj

    # Set flag to indicate whether the model trained correctly
    if(is.null(model_obj)){
      model_list$model_trained <- FALSE
    } else {
      model_list$model_trained <- TRUE
    }
  }

  return(model_list)
}



learner.naive_bayes.test <- function(object, data_obj){

  # Extract outcome type
  outcome_type <- object@outcome_type

  # Determine if the model was trained correctly and predict new data
  if(model_is_trained(object=object)){

    # Generate skeleton of prediction data table from dt
    dt_pred      <- data_obj@data[, get_non_feature_columns(x=object), with=FALSE]

    # Get predicted classes and probabilities for data table dt
    outcome_list <- predict(object=object@model$model, newdata=data_obj@data)

    # Set predicted class to the prediction table
    dt_pred[, "outcome_pred_class":=outcome_list$class]

    # Get outcome levels
    class_levels <- object@class_levels

    # Add class probabilities
    outcome_pred_class_prob_cols <- getClassProbabilityColumns(outcome_type=outcome_type, class_levels=class_levels)
    dt_pred      <- cbind(dt_pred, data.table::as.data.table(outcome_list$posterior))
    data.table::setnames(dt_pred, old=class_levels, new=outcome_pred_class_prob_cols)

  } else {
    dt_pred      <- createNonValidPredictionTable(dt=data_obj@data, outcome_type=outcome_type)
  }

  # Return prediction data table
  return(dt_pred)
}



learner.naive_bayes.vimp <- function(object){
  # Generate variable importance data table
  
  if(!model_is_trained(object=object)){
    vimp_table <- getEmptyVimp()
    
  } else { 
    # There is no inherent variable importance for the naive Bayes classifier,
    # so we pass the features which were used in the model.
    vimp_table <- data.table::data.table("score"=as.double(NA),
                                         "name"=object@signature,
                                         "rank"=1,
                                         "multi_var"=TRUE)
  }
  
  # Return variable importance data table
  return(vimp_table)
}



learner.naive_bayes.get_calibration <- function(object, data_obj){

  if(object@outcome_type %in% c("binomial", "multinomial") & learner.naive_bayes.outcome(object=object)) {

    # Parse calibration
    dt_calibr <- learner.calibration.categorical(object=object, data_obj=data_obj)

  } else {
    stop()
  }

  return(dt_calibr)
}

learner.knn.outcome <- function(learner=NULL, outcome_type=NULL, object=NULL){

  # Extract data from the familiarModel object if available
  if(!is.null(object)){
    learner      <- object@learner
    outcome_type <- object@outcome_type
  }

  if(outcome_type %in% c("binomial", "multinomial")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}



learner.knn.param <- function(data_obj, learner){

  # Initialise list and declare hyperparameter entries
  param           <- list()
  param$sign_size <- list()
  param$k         <- list()
  param$gamma     <- list()

  # If data_obj is explicitly NULL, return the list with hyperparameter names only
  if(is.null(data_obj)) { return(param) }

  # Get the number of subjects
  n_samples <- nrow(data_obj@data)
  n_classes <- length(get_outcome_class_levels(x=data_obj))

  
  ##### Signature size #########################################################
  param$sign_size <- .get_default_sign_size(data_obj=data_obj)


  ##### Number of nearest neighbours k #########################################
  
  # Define the range for the number of nearest neighbour clusters.
  k_range <- c(1, ceiling(2*n_samples^(1/3)))
  
  # Define the default value.
  k_default <- c(1, 2, 5, 10, 20)
  k_default <- k_default[k_default >= k_range[1] & k_default <= k_range[2]]
  
  param$k <- .set_hyperparameter(default=k_default, type="integer", range=k_range,
                                 valid_range=c(n_classes, Inf), randomise=TRUE)
  
  
  ##### Radial basis function kernel gamma #####################################
  
  # Gamma is based on the log10 scale with a 10^-5 offset, so that gamma=-5 leads
  # to no distance weighting.
  if(learner == "k_nearest_neighbours"){
    gamma_default <- -5
    gamma_range <- c(-5, -5)
  } else if(learner=="k_nearest_neighbours_radial") {
    gamma_default <- c(-5, -3, -1, 0, 1, 3, 5)
    gamma_range <- c(-5, 5)
  }
  
  # Set the gamma parameter.
  param$gamma <- .set_hyperparameter(default=gamma_default, type="numeric", range=gamma_range,
                                     valid_range=c(-5, Inf), randomise=ifelse(length(gamma_default) > 1, TRUE, FALSE))
  
  return(param)
}



learner.knn.train <- function(object, data_obj){

  # Extract information
  param        <- object@hyperparameters

  # Initiate model_list list
  model_list       <- list()

  # Find feature columns in data table
  feature_cols     <- get_feature_columns(x=data_obj)

  # Generate a formula with only feature columns that have some variance
  formula          <- stats::reformulate(feature_cols, response=quote(outcome))

  # Generate model -- NOTE: sknn is directly imported through NAMESPACE as predict is not exported by klaR
  model_obj        <- sknn(formula, data=data_obj@data, kn=param$k, gamma=10^param$gamma - 10^-5)

  # Append model to list
  model_list$model <- model_obj

  return(model_list)
}



learner.knn.test <- function(object, data_obj){

  # Extract the outcome type
  outcome_type <- object@outcome_type

  if(!model_is_trained(object)){
    return(createNonValidPredictionTable(dt=data_obj@data, outcome_type=outcome_type))
  }
  
  # Generate skeleton of prediction data table from dt
  dt_pred      <- data_obj@data[, get_non_feature_columns(x=object), with=FALSE]

  # Get predicted classes and probabilities for data table dt
  outcome_list <- predict(object=object@model$model, newdata=data_obj@data)

  # Set predicted class to the prediction table
  dt_pred[, "outcome_pred_class":=outcome_list$class]

  # Get outcome levels
  class_levels <- get_outcome_class_levels(x=object)

  # Add class probabilities
  outcome_pred_class_prob_cols <- get_class_probability_name(x=class_levels)
  dt_pred <- cbind(dt_pred, data.table::as.data.table(outcome_list$posterior))
  data.table::setnames(dt_pred, old=class_levels, new=outcome_pred_class_prob_cols)

  return(dt_pred)
}



learner.knn.vimp <- function(object){
  # Generate variable importance data table

  if(!model_is_trained(object)){
    return(getEmptyVimp())
  }
  
  # There is no inherent variable importance for the k-nearest neighbours classifier, so we pass the features which were used in the model.
  dt_vimp        <- data.table::data.table("score"=as.double(NA), "name"=object@signature, "rank"=1, "multi_var"=TRUE)

  # Return variable importance data table
  return(dt_vimp)
}



learner.knn.get_calibration <- function(object, data_obj){

  if(object@outcome_type %in% c("binomial", "multinomial") & learner.knn.outcome(object=object)) {

    # Parse calibration
    dt_calibr <- learner.calibration.categorical(object=object, data_obj=data_obj)

  } else {
    stop()
  }

  return(dt_calibr)
}

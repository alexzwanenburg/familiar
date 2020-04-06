learner.svm.is_svm <- function(learner){
  
  # Check if the learner is an SVM.
  if(!stringi::stri_startswith_fixed(str=learner, pattern="svm")){
    return(FALSE)
  }
  
  # Attempt to decompose the string. If not succesfull, a NULL is returned.
  if(is.null(learner.svm.decompose_learner_string(learner=learner))){
    return(FALSE)
  } else {
    return(TRUE)
  }
}



learner.svm.decompose_learner_string <- function(learner){
  
  # Strip svm
  learner <- stringi::stri_replace_first_fixed(str=learner, replacement="", pattern="svm")

  # Extract the type of svm (c, c_bound, nu, spoc, kbb, eps, eps_bound)
  svm_types <- c("c", "c_bound", "nu", "eps", "eps_bound")
  if(any(stringi::stri_startswith_fixed(str=learner, pattern=sapply(svm_types, function(ii)(paste0("_", ii)))))){
    
    # Select the appropriate svm type
    current_type <- svm_types[stringi::stri_startswith_fixed(str=learner, pattern=sapply(svm_types, function(ii)(paste0("_", ii))))]
    
    # Select the longest match. For example, "c" and "c_bound" may be found from the same "svm_c_bound" string.
    current_type <- current_type[which.max(sapply(current_type, nchar))]
    
    # Strip from learner
    learner <- stringi::stri_replace_first_fixed(str=learner, replacement="", pattern=paste0("_", current_type))
    
  } else {
    # There may be no matches, e.g. "svm", "svm_radial" etc. revert to default type.
    current_type <- ""
  }
  
  # Extract the svm kernel
  svm_kernels <- c("vanilla", "linear", "rbf", "radial", "poly", "polynomial", "tanh", "sigmoid", "bessel", "laplace", "anova")
  if(any(stringi::stri_startswith_fixed(str=learner, pattern=sapply(svm_kernels, function(ii) (paste0("_", ii)))))){
    
    # Select the appropriate svm kernel
    current_kernel <- svm_kernels[stringi::stri_startswith_fixed(str=learner, pattern=sapply(svm_kernels, function(ii) (paste0("_", ii))))]
    
    # Strip from learner
    learner <- stringi::stri_replace_first_fixed(str=learner, replacement="", pattern=paste0("_", current_kernel))
    
  } else {
    # There may be no matches, e.g. "svm", "svm_c_bound" etc. revert to default kernels.
    current_kernel <- ""
  }
  
  # If everything is correct, an empty ("") learner string should remain.
  # Otherwise, return NULL.
  if(learner == ""){
    return(list("type"=current_type, "kernel"=current_kernel))
  } else {
    return(NULL)
  }
}



learner.svm.outcome <- function(learner=NULL, outcome_type=NULL, object=NULL){
  
  # Extract details from the familiar model
  if(!is.null(object)){
    outcome_type <- object@outcome_type
  }
  
  if(!is.null(object)){
    learner <- object@learner
  }
  
  if(outcome_type %in% c("binomial", "multinomial")){
    # Restrict options to those that can return probabilities.
    svm_type <- learner.svm.decompose_learner_string(learner=learner)$type

    if(svm_type %in% c("", "nu", "c", "c_bound")){
      return(TRUE)
      
    } else {
      return(FALSE)
    }
    
  } else if(outcome_type %in% c("continuous", "count")){
    # Restrict options for regression problems.
    svm_type <- learner.svm.decompose_learner_string(learner=learner)$type
    
    # Check if the svm-type is allowed.
    if(svm_type %in% c("", "nu", "eps", "eps_bound")){
      return(TRUE)
      
    } else {
      return(FALSE)
    }
    
  } else {
    return(FALSE)
  }
  
}



learner.svm.param <- function(data_obj, learner){

  # Extract svm type and kernel
  decomposed_learner <- learner.svm.decompose_learner_string(learner=learner)
  svm_type <- decomposed_learner$type
  svm_kernel <- decomposed_learner$kernel
  
  # Check if a kernel was provided through the learner, otherwise use "radial" by default
  if(svm_kernel == ""){
    svm_kernel <- "radial"
  }
  
  # Initialise list and declare hyperparameter entries
  # Note that some hyperparameters may not be required, dependent on the kernel and svm-type used
  param    <- list()
  param$sign_size   <- list()
  param$svm_type    <- list()
  param$kernel      <- list()
  param$c           <- list()
  param$epsilon     <- list()
  param$nu          <- list()
  
  if(svm_kernel %in% c("rbf", "radial")){
    param$sigma <- list()
    
  } else if(svm_kernel %in% c("poly", "polynomial")){
    param$degree <- param$scale <- param$offset <- list()
    
  } else if(svm_kernel %in% c("tanh", "sigmoid")){
    param$scale <- param$offset <- list()
    
  } else if(svm_kernel %in% c("bessel")){
    param$sigma <- param$order <- param$degree <- list()
    
  } else if(svm_kernel %in% c("laplace")){
    param$sigma <- list()
    
  } else if(svm_kernel %in% c("anova")){
    param$sigma <- param$degree <- list()
  }
  
    # If data_obj is explicitly NULL, return the list with hyperparameter names
  # only.
  if(is.null(data_obj)) { return(param) }
  
  # Extract outcome type.
  outcome_type <- data_obj@outcome_type

  
  ##### Signature size #########################################################
  param$sign_size <- .get_default_sign_size(data_obj=data_obj)
  
  
  ##### SVM type ###############################################################
  
  if(svm_type == ""){
    # The default type of SVM depends on the type of outcome:
    # * C-classification for categorical data.
    # * eps-regression for continuous outcome data
    
    if(outcome_type %in% c("binomial", "multinomial")){
      svm_type <- "C-svc"
      
    } else if(outcome_type %in% c("continuous", "count")){
      svm_type <- "eps-svr"
      
    } else {
      ..error_reached_unreachable_code("learner.svm.param_no_default_svm_type")
    }
  } else if(svm_type == "c"){
    svm_type <- "C-svc"
    
  } else if(svm_type == "c_bound"){
    svm_type <- "C-bsvc"
    
  } else if(svm_type == "eps"){
    svm_type <- "eps-svr"
    
  } else if(svm_type == "eps_bound"){
    svm_type <- "eps-bsvr"
    
  } else if(svm_type == "nu"){
    # nu-SVM is available for regression and categorical outcomes
    if(outcome_type %in% c("binomial", "multinomial")){
      svm_type <- "nu-svc"
      
    } else if(outcome_type %in% c("continuous", "count")){
      svm_type <- "nu-svr"
    } else {
      ..error_reached_unreachable_code("learner.svm.param_no_available_nu_svm_type")
    }
    
  } else {
    ..error_reached_unreachable_code("learner.svm.param_unknown_svm_type")
  }

  # Set the type of svm that will be performed.
  param$svm_type <- .set_hyperparameter(default=svm_type, type="factor", range=svm_type, randomise=FALSE)
  
  
  ##### SVM kernel #############################################################
  
  # A default kernel has been set earlier. Convert the svm_kernel to the
  # internal naming used by kernlab.
  if(svm_kernel %in% c("vanilla", "linear")){
    svm_kernel <- "vanilladot"
    
  } else if(svm_kernel %in% c("rbf", "radial")){
    svm_kernel <- "rbfdot"
    
  } else if(svm_kernel %in% c("poly", "polynomial")){
    svm_kernel <- "polydot"
    
  } else if(svm_kernel %in% c("tanh", "sigmoid")){
    svm_kernel <- "tanhdot"
    
  } else if(svm_kernel %in% c("bessel")){
    svm_kernel <- "besseldot"
    
  } else if(svm_kernel %in% c("laplace")){
    svm_kernel <- "laplacedot"
    
  } else if(svm_kernel %in% c("anova")){
    svm_kernel <- "anovadot"
  }
  
  # Set the svm kernel.
  param$kernel <- .set_hyperparameter(default=svm_kernel, type="factor", range=svm_kernel,
                                      randomise=FALSE)
  
  
  ##### constraints violation cost C ###########################################
  
  # This parameter defines the cost for constraint violations. It is expressed
  # on a log10 scale.
  param$c <- .set_hyperparameter(default=c(-3, -1, -0, 1, 3), type="numeric", range=c(-5, 3),
                                 valid_range=c(-Inf, Inf), randomise=TRUE)
  
  
  if(svm_type %in% c("eps-svr", "nu-svr", "eps-bsvr")){
    ##### Error tolerance epsilon ##############################################
    
    # This parameter defines the error tolerance for regression SVM. It is
    # expressed on a log10 scale.
    param$epsilon <- .set_hyperparameter(default=c(-5, -3, -1, 0, 1), type="numeric", range=c(-5, 1),
                                         valid_range=c(-Inf, Inf), randomise=TRUE)
    
  } else {
    param$epsilon <- NULL
  }
  
  if(svm_type %in% c("nu-svc", "nu-svr")){
    ##### Error bounds parameter nu ############################################
    
    # nu is expressed on a log10 scale.
    param$nu <- .set_hyperparameter(default=c(-5, -3, -1, 0, 1), type="numeric", range=c(-5, 1),
                                    valid_range=c(-Inf, Inf), randomise=TRUE)
    
  } else {
    param$nu <- NULL
  }
  
  
  if(svm_kernel %in% c("rbfdot", "besseldot", "laplacedot", "anovadot")){
    ##### Inverse kernel width sigma ###########################################
    
    # sigma is expressed on a log10 scale
    param$sigma <- .set_hyperparameter(default=c(-5, -3, -1, 1, 3, 5), type="numeric", range=c(-5, 5),
                                       valid_range=c(-Inf, Inf), randomise=TRUE)
  }
  
  if(svm_kernel %in% c("polydot", "besseldot", "anovadot")){
    ##### Polynomial degree ####################################################
    
    # polydot, besseldot and anovadot expect positive integer degrees.
    param$degree <- .set_hyperparameter(default=c(1, 2, 3, 4, 5), type="integer", range=c(1, 5),
                                        valid_range=c(1, Inf), randomise=TRUE)
  }
  
  if(svm_kernel %in% c("polydot", "tanhdot")){
    ##### Distance scale parameter #############################################
    
    # scale is expressed on a log10 scale. Note that as the feature data is
    # rescaled internally, we should not expect a scale much larger then 1. 
    param$scale <- .set_hyperparameter(default=c(-3, -1, 0), type="numeric", range=c(-5, log10(2)),
                                       valid_range=c(-Inf, Inf), randomise=TRUE)
    
    
    ##### Kernel offset parameter ##############################################
    
    # As feature data is rescaled internally by kernlab, we should not expect
    # offsets outside the [0, 1] range. Also, negative values are not allowed
    # for either kernel.
    param$offset <- .set_hyperparameter(default=c(0.0, 0.2, 0.5, 1.0), type="numeric", range=c(0, 1),
                                        valid_range=c(0, Inf), randomise=TRUE)
  }
  
  if(svm_kernel %in% c("besseldot")){
    ##### Bessel function order ################################################
    
    # The order of the Bessel function should be a non-negative integer. Note
    # that kernlab does not complain about non-integer values, but this results
    # in models that are the same as if they were generated using a floor
    # operation on the order parameter.
    param$order <- .set_hyperparameter(default=c(0, 1, 2, 3, 5), type="integer", range=c(0, 10),
                                       valid_range=c(0, Inf), randomise=TRUE)
    
  }

  return(param)
}



learner.svm.train <- function(object, data_obj){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- NULL
  
  # Extract internal variables
  outcome_type <- object@outcome_type
  parameter_list <- object@hyperparameters
  
  # Initiate model_list list
  model_list   <- list()
  
  # Determine whether probability fits are required
  fit_probability <- outcome_type %in% c("binomial", "multinomial")
  
  # Convert counts to a log scale
  if(outcome_type == "count") {
    data_obj@data   <- data.table::copy(data_obj@data)[, "outcome":=log1p(outcome)]
  }
  
  # Find feature columns in data table
  feature_cols <- get_feature_columns(x=data_obj)
  
  # Parse formula
  formula      <- stats::reformulate(termlabels=feature_cols, response=quote(outcome))
  
  ##### Set svm parameters #####################################################
  svm_parameter_list <- list("type"=parameter_list$svm_type,
                             "kernel"=parameter_list$kernel,
                             "C"=10^(parameter_list$c))
  
  # Set nu-parameter (which not all svm types use).
  if(!is.null(parameter_list$nu)){
    svm_parameter_list$nu <- 10^(parameter_list$nu)
  }
  
  # Set epsilon parameter (which not all svm types use).
  if(!is.null(parameter_list$epsilon)){
    svm_parameter_list$epsilon <- 10^(parameter_list$epsilon)
  }
  
  
  ##### Set kernel-specific parameters #########################################
  kernel_parameter_list <- list()
  if(!is.null(parameter_list$sigma)){
    kernel_parameter_list$sigma <- 10^parameter_list$sigma
  }
  
  if(!is.null(parameter_list$degree)){
    kernel_parameter_list$degree <- parameter_list$degree
  }
  
  if(!is.null(parameter_list$scale)){
    kernel_parameter_list$scale <- parameter_list$scale
  }
  
  if(!is.null(parameter_list$offset)){
    kernel_parameter_list$offset <- parameter_list$offset
  }
  
  if(!is.null(parameter_list$order)){
    kernel_parameter_list$order <- parameter_list$order
  }

  # Fit the SVM model using kernlab::ksvm
  model_obj <- quiet(do.call(kernlab::ksvm, args=append(list("x"=formula, "data"=data_obj@data, "kpar"=kernel_parameter_list,
                                                             "prob.model"=fit_probability, "fit"=FALSE, "cross"=0L),
                                                        svm_parameter_list)))

  # Append model to list
  model_list$model           <- model_obj
  model_list$fit_probability <- fit_probability
  
  # Update model status
  model_list$model_trained <- !is.null(model_obj)
  
  return(model_list)
}



learner.svm.test <- function(object, data_obj){
  
  # Extract internal variables
  outcome_type <- object@outcome_type
  
  if(model_is_trained(object=object)){
    
    # Generate skeleton of prediction data table from dt
    predictions_table <- data_obj@data[, get_non_feature_columns(x=object), with=FALSE]
    
    if(outcome_type %in% c("binomial", "multinomial")){

      # Obtain predicted classes.
      predictions_table[, "outcome_pred_class":=kernlab::predict(object=object@model$model, newdata=data_obj@data,
                                                                 type="response")]
      # Set the class order in the outcome.
      predictions_table$outcome_pred_class <- factor(predictions_table$outcome_pred_class,
                                                     levels=get_outcome_class_levels(x=object))
      
      # Predict probabilities and convert to data.table
      predicted_probs <- kernlab::predict(object=object@model$model, newdata=data_obj@data, type="probabilities")
      predicted_probs <- data.table::as.data.table(predicted_probs)
      
      # Update the column names for the predicted probabilities
      data.table::setnames(x=predicted_probs, old=colnames(predicted_probs),
                           new=getClassProbabilityColumns(outcome_type=outcome_type, class_levels=colnames(predicted_probs)))
      
      # Reorder probability columns.
      data.table::setcolorder(predicted_probs, neworder=getClassProbabilityColumns(outcome_type=outcome_type,
                                                                                   class_levels=get_outcome_class_levels(x=object)))
      
      # Add probability columns to the predictions table.
      predictions_table <- cbind(predictions_table, predicted_probs)
      
    } else if(outcome_type %in% c("continuous")){
      # Predict regression outcomes.
      predictions_table[, "outcome_pred":=kernlab::predict(object=object@model$model, newdata=data_obj@data)]
      
    } else if(outcome_type %in% c("count")){
      # Add predicted count, after conversion to original scale.
      predictions_table[, "outcome_pred":=expm1(kernlab::predict(object=object@model$model, newdata=data_obj@data))]
      
    } else {
      ..error_reached_unreachable_code("learner.svm.test_svm_not_implemented_for_outcome_type")
    }
    
  } else {
    predictions_table <- createNonValidPredictionTable(dt=data_obj@data, outcome_type=outcome_type)
  }
  
  # Return prediction data table
  return(predictions_table)
}



learner.svm.vimp <- function(object){
  # Generate variable importance data table
  
  if(!model_is_trained(object)){
    return(getEmptyVimp())
  }
  
  # There is currently no variable importance for the e1071 implementation of SVM, so we pass the features which were used in the model.
  dt_vimp        <- data.table::data.table("score"=as.double(NA), "name"=object@signature, "rank"=1, "multi_var"=TRUE)
  
  # Return variable importance data table
  return(dt_vimp)
}



learner.svm.calibration_info <- function(object, data_obj){
  
  if(object@outcome_type %in% c("continuous", "count")){
    # Determine range of outcomes.
    calibration_info <- learner.calibration.regression.outcome_range(data_obj=data_obj)
    
  } else {
    calibration_info <- NULL
  }
  
  return(calibration_info)
}



learner.svm.get_calibration <- function(object, data_obj){
  
  # Extract internal variables
  outcome_type <- object@outcome_type
  
  if(outcome_type %in% c("binomial", "multinomial") & learner.svm.outcome(object=object)) {
    
    # Parse calibration
    dt_calibr <- learner.calibration.categorical(object=object, data_obj=data_obj)
    
  } else if(outcome_type %in% c("count", "continuous") & learner.svm.outcome(object=object)) {
    
    # Parse calibration
    dt_calibr <- learner.calibration.regression(object=object, data_obj=data_obj)
    
  } else {
    stop()
  }
  
  return(dt_calibr)
}

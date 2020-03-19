vimp.main <- function(method, purpose, outcome_type=NULL, param=NULL, data_obj=NULL){
  # This is a convenience function for access to method-related functions.
  # This functions should only be called through its interfaces

  ##### Univariable statistical methods #####

  # Concordance-based methods
  if(method == "concordance"){
    if(purpose=="variable_importance"){
      dt_vimp       <- vimp.concordance.vimp(data_obj=data_obj)
    } else if(purpose=="parameters") {
      param         <- vimp.concordance.param(data_obj=data_obj, method=method)
    } else if(purpose=="outcome"){
      type_is_valid <- vimp.concordance.outcome(method=method, outcome_type=outcome_type)
    } else if(purpose=="base_learner"){
      base_learner  <- vimp.concordance.learner(method=method, outcome_type=outcome_type)
    }
  }

  # Impurity-based methods
  if(method %in% c("gini", "mdl", "relieff_exp_rank", "gain_ratio")){
    if(purpose=="variable_importance"){
      dt_vimp       <- vimp.corelearn.vimp(data_obj=data_obj, method=method)
    } else if(purpose=="parameters") {
      param         <- vimp.corelearn.param(data_obj=data_obj, method=method)
    } else if(purpose=="outcome"){
      type_is_valid <- vimp.corelearn.outcome(method=method, outcome_type=outcome_type)
    } else if(purpose=="base_learner"){
      base_learner  <- vimp.corelearn.learner(method=method, outcome_type=outcome_type)
    }
  }

  # Correlation-based methods
  if(method %in% c("pearson", "spearman", "kendall")){
    if(purpose=="variable_importance"){
      dt_vimp       <- vimp.correlation.vimp(data_obj=data_obj, method=method)
    } else if(purpose=="parameters") {
      param         <- vimp.correlation.param(data_obj=data_obj, method=method)
    } else if(purpose=="outcome"){
      type_is_valid <- vimp.correlation.outcome(method=method, outcome_type=outcome_type)
    } else if(purpose=="base_learner"){
      base_learner  <- vimp.correlation.learner(method=method, outcome_type=outcome_type)
    }
  }

  ##### Mutual information-based statistical methods #####

  # Mutual information
  if(method %in% c("mim", "mifs", "mrmr")){
    if(purpose=="variable_importance"){
      dt_vimp       <- vimp.mutual_information.vimp(data_obj=data_obj, method=method)
    } else if(purpose=="parameters") {
      param         <- vimp.mutual_information.param(data_obj=data_obj, method=method)
    } else if(purpose=="outcome"){
      type_is_valid <- vimp.mutual_information.outcome(method=method, outcome_type=outcome_type)
    } else if(purpose=="base_learner"){
      base_learner  <- vimp.mutual_information.learner(method=method, outcome_type=outcome_type)
    }
  }


  ##### Regression-based methods #####

  # Regression-based methods
  if(method %in% c("univariate_regression", "multivariate_regression")){
    if(purpose=="variable_importance"){
      dt_vimp       <- vimp.regression.vimp(data_obj=data_obj, method=method, param=param)
    } else if(purpose=="parameters") {
      param         <- vimp.regression.param(data_obj=data_obj, method=method)
    } else if(purpose=="outcome"){
      type_is_valid <- vimp.regression.outcome(method=method, outcome_type=outcome_type)
    } else if(purpose=="base_learner"){
      base_learner  <- vimp.regression.learner(method=method, outcome_type=outcome_type)
    }
  }

  # Elastic net penalised regression models
  if(method %in% c("elastic_net", "elastic_net_gaussian",  "elastic_net_poisson", "elastic_net_binomial","elastic_net_multinomial",
                    "elastic_net_cox", "lasso", "lasso_gaussian", "lasso_poisson", "lasso_binomial", "lasso_multinomial", "lasso_cox",
                   "ridge", "ridge_gaussian", "ridge_gaussian", "ridge_poisson", "ridge_binomial", "ridge_multinomial", "ridge_cox")){
    if(purpose=="variable_importance"){
      dt_vimp       <- learner.net.vimp(data_obj=data_obj, param=param, method=method)
    } else if(purpose=="parameters"){
      base_learner  <- learner.net.learner(method=method, outcome_type=outcome_type)
      param         <- learner.net.param(data_obj=data_obj, learner=base_learner)
    } else if(purpose=="outcome") {
      type_is_valid <- learner.net.outcome(method=method, outcome_type=outcome_type)
    } else if(purpose=="base_learner"){
      base_learner  <- learner.net.learner(method=method, outcome_type=outcome_type)
    }
  }

  ##### Random forest-based methods #####

  # Random forest variable importance methods
  if(method %in% c("random_forest_permutation", "random_forest_minimum_depth", "random_forest_variable_hunting",
                   "random_forest_rfsrc_permutation", "random_forest_rfsrc_minimum_depth", "random_forest_rfsrc_variable_hunting")){
    if(purpose=="variable_importance"){
      dt_vimp       <- learner.rf_rfsrc.vimp(data_obj=data_obj, param=param, method=method)
    } else if(purpose=="parameters"){
      base_learner  <- learner.rf_rfsrc.learner(method=method, outcome_type=outcome_type)
      param         <- learner.rf_rfsrc.param(data_obj=data_obj)
    } else if(purpose=="outcome") {
      type_is_valid <- learner.rf_rfsrc.outcome(method=method, outcome_type=outcome_type)
    } else if(purpose=="base_learner"){
      base_learner  <- learner.rf_rfsrc.learner(method=method, outcome_type=outcome_type)
    }
  }


  # Maximally selected rank random forest variable importance methods
  if(method %in% c("random_forest_ranger_holdout_permutation", "random_forest_ranger_permutation", "random_forest_ranger_impurity")){
    if(purpose=="variable_importance"){
      dt_vimp       <- learner.rf_ranger.vimp(data_obj=data_obj, param=param, method=method)
    } else if(purpose=="parameters"){
      base_learner  <- learner.rf_ranger.learner(method=method, outcome_type=outcome_type)
      param         <- learner.rf_ranger.param(data_obj=data_obj)
    } else if(purpose=="outcome") {
      type_is_valid <- learner.rf_ranger.outcome(method=method, outcome_type=outcome_type)
    } else if(purpose=="base_learner"){
      base_learner  <- learner.rf_ranger.learner(method=method, outcome_type=outcome_type)
    }
  }

  ##### TEST Method -- do not use in normal runs ###############################
  if(method %in% c("__test_empty")){
    if(purpose=="variable_importance"){
      dt_vimp       <- vimp.test.vimp(data_obj=data_obj)
    } else if(purpose=="parameters") {
      param         <- vimp.test.param(data_obj=data_obj, method=method)
    } else if(purpose=="outcome"){
      type_is_valid <- vimp.test.outcome(method=method, outcome_type=outcome_type)
    } else if(purpose=="base_learner"){
      base_learner  <- vimp.test.learner(method=method, outcome_type=outcome_type)
    }
  }
  
  ##### Other methods #####

  # Random feature selection and no feature selection
  if(method %in% c("random", "none", "signature_only")){
    if(purpose=="variable_importance"){
      dt_vimp       <- getEmptyVimp()
    } else if(purpose=="parameters"){
      param         <- list()
    } else if(purpose=="outcome"){
      type_is_valid <- TRUE
    } else if(purpose=="base_learner"){
      base_learner  <- NULL
    }
  }

  # Return objects
  if(purpose=="variable_importance"){
    # Return variable importance data table
    return(dt_vimp)

  } else if(purpose=="parameters") {
    # Return method parameters
    return(param)

  } else if(purpose=="outcome"){
    # Return whether the outcome is valid for the selected feature selection method
    # This may be unset if the method is not specified correctly
    if(exists("type_is_valid", where=environment())) {
      return(type_is_valid)
    } else {
      return(NULL)
    }
  } else if(purpose=="base_learner"){
    # Return base learner
    return(base_learner)
  }
}


vimp.get_base_learner <- function(method, outcome_type=outcome_type){

  # Get base learner from method
  base_learner <- vimp.main(method=method, outcome_type=outcome_type, purpose="base_learner")

  # Return base learner
  return(base_learner)
}



vimp.assess_variable_importance <- function(data_obj, method, param=NULL){

  # Get variable importance
  dt_vimp <- vimp.main(method=method, purpose="variable_importance", data_obj=data_obj, param=param)

  # Return variable importance
  return(dt_vimp)

}



vimp.get_fs_parameters <- function(data_obj, method, outcome_type, names_only=FALSE){

  # Extract model parameters of the method
  param   <- vimp.main(method=method, outcome_type=outcome_type, purpose="parameters", data_obj=data_obj)

  # Extract names from parameter list
  if(names_only==TRUE){
    param <- names(param)
  }

  # Return hyperparameter list, or names or parameters
  return(param)
}



vimp.check_outcome_type <- function(method, outcome_type){

  # Determine if the outcome type is valid for the current feature selection method
  type_is_valid <- vimp.main(method=method, outcome_type=outcome_type, purpose="outcome")

  # Check if type_is_valid has been set. If not, the method has not been specified and an error should be given
  # If it has been set, but is FALSE, the current feature selection method is incompatible with the outcome type provided.
  if(is.null(type_is_valid)){
    stop(paste0(method, " is not a valid feature selection method. Please check the vignette for available feature selection methods."))
  } else if(type_is_valid==FALSE) {
    stop(paste0(method, " is not available for \"", outcome_type, "\" outcomes."))
  }
}



vimp.check_fs_parameters <- function(method, user_param, outcome_type){

  # Check if current method is defined in the user parameter list
  if(is.null(user_param[[method]])) { return(NULL) }

  # Get parameters defined by the model
  defined_param <- vimp.get_fs_parameters(data_obj=NULL, method=method, outcome_type=outcome_type, names_only=TRUE)

  if(is.null(defined_param)){ return(NULL) }

  # Parse names of the user-defined parameters
  user_param    <- names(user_param[[method]])

  # Determine if parameters are incorrectly set
  if(!all(user_param %in% defined_param)){

    # Determine incorrectly defined user parameters
    undefined_user_param <- user_param[!user_param %in% defined_param]

    # Notify the user
    logger.stop(paste0("Configuration: \"", paste(undefined_user_param, collapse="\", \""), "\" are invalid parameters. Valid parameters are: \"",
                       paste(defined_param, collapse="\", \"")), "\".")
  }
}



vimp.update_fs_parameters <- function(param_list, param_preset_list=NULL){
  # Check parameters in param_list and update with param_preset_list, which is user-provided

  # Check if there are any parameters which can be updated; otherwise return
  if(is.null(param_list)){ return(NULL) }

  # Initiate empty list for feature selection parameters
  parsed_param_list <- list()

  # Iterate over the parameters in the list
  for(ii in seq_len(length(param_list))){

    # Get the name of the current parameter
    param_name <- names(param_list)[ii]

    # If the parameter is present in the preset list, we replace the current value with the value in the preset list
    if(!is.null(param_preset_list[[param_name]])){

      # Determine the parameter type
      param_type <- param_list[[param_name]]$type

      # Read from the user-provided list and convert type
      if(param_type %in% c("numeric", "integer")){
        parsed_param_list[[param_name]] <- as.numeric(param_preset_list[[param_name]])
      } else if(param_type == "logical"){
        parsed_param_list[[param_name]] <- as.logical(param_preset_list[[param_name]])
      } else if(param_type %in% c("factor", "character")){
        parsed_param_list[[param_name]] <- as.character(param_preset_list[[param_name]])
      }

    } else {
      parsed_param_list[[param_name]] <- param_list[[param_name]]$default
    }
  }

  # Return the update parameter list
  return(parsed_param_list)
}

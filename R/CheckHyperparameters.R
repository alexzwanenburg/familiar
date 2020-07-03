#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

.parse_hyperparameters <- function(data, parameter_list, outcome_type, fs_method=NULL, learner=NULL){

  # Check if any hyperparameters have been set
  if(length(parameter_list) == 0){ return(parameter_list) }
  
  if(!is.null(fs_method)){
    is_vimp <- TRUE
  } else if(!is.null(learner)){
    is_vimp <- FALSE
  } else {
    ..error_reached_unreachable_code("check_hyperparameters_fs_method_and_learner_both_null")
  } 
  
  # Find 
  if(is_vimp){
    user_function_names <- names(parameter_list)
    all_functions <- fs_method
  } else{
    user_function_names <- names(parameter_list)
    all_functions <- learner
  }
  
  # Check whether all names in the hyperparameter list correspond to a fs_method or learner.
  unknown_function <- setdiff(user_function_names, all_functions)
  if(length(unknown_function) > 0){
    stop(paste("Could not match all entries in the parameter list to", ifelse(is_vimp, "feature selection methods.", "learners."),
               "Failed to match:", paste0(unknown_function, collapse=", ")))
  }
  
  # Check for duplicates
  if(anyDuplicated(user_function_names)){
    stop(paste("Parameters for one or more of the", ifelse(is_vimp, "feature selection methods", "learners"),
               "are defined more than once."))
  }
  
  # Package data into a data object.
  data <- methods::new("dataObject", data=data, is_pre_processed=FALSE, outcome_type=outcome_type)
  
  # Iterate over the feature selection methods or learners to parse and check the hyperparameters
  out_list <- lapply(user_function_names, .check_hyperparameters, in_list=parameter_list, data=data, is_vimp=is_vimp)
  
  # Set names of the input parameters
  names(out_list) <- user_function_names

  return(out_list)
}



.check_hyperparameters <- function(user_function_name, in_list, data, is_vimp){

  # Select current parameter list
  in_list <- in_list[[user_function_name]]
  
  # Obtain presets
  if(is_vimp){
    preset_list <- .get_preset_hyperparameters(data=data, fs_method=user_function_name, names_only=FALSE)
  } else {
    preset_list <- .get_preset_hyperparameters(data=data, learner=user_function_name, names_only=FALSE)
  }
  

  # Update user function name for passing into error messages
  user_function_name <- paste(user_function_name, ifelse(is_vimp, "feature selection method", "learner"))
  
  # Check if the desired function actually has parameters.
  if(length(preset_list)==0){
    stop(paste("The", user_function_name, "has no associated parameters."))
  }
  
  # Check if there are any parameters in the user-provided list that do not appear in the preset list
  unknown_parameter <- setdiff(names(in_list), names(preset_list))
  if(length(unknown_parameter) > 0){
    stop(paste("Could not match all parameters provided for the", user_function_name, "to a valid parameter. Failed to match:",
               paste0(unknown_parameter, collapse=", ")))
  }
  
  # Check if there are any duplicate parameters
  if(anyDuplicated(names(in_list))){
    stop(paste("One or more parameters provided for the", user_function_name, "appear more than once."))
  }
  
  # Iterate over the individual parameters and parse/check individual parameters
  out_list <- lapply(names(in_list), function(parameter_name, in_list, user_function_name, preset_list){
    .check_single_hyperparameter(parameter_name,
                                 user_function_name=user_function_name,
                                 x=in_list[[parameter_name]],
                                 preset_list=preset_list[[parameter_name]])
  }, user_function_name=user_function_name, in_list=in_list, preset_list=preset_list)
  
  # Set names of the out_list
  names(out_list) <- names(in_list)

  return(out_list)
}



.check_single_hyperparameter <- function(parameter_name, user_function_name, x, preset_list){

  if(is.list(x) & length(x) == 1){
    # Unlist list. This happens when the hyperparameter settings come from a configuration file.
    x <- unlist(x) 
  }
  
  if(is.character(x)){
    # Divide by comma
    x <- stringi::stri_split(str=x, fixed=",")[[1]]
    
    # Remove whitespace
    x <- stringi::stri_replace_all(str=x, fixed=" ", replacement="")
  }
  
  # Update the parameter_name for passing into error warnings
  parameter_name <- paste(parameter_name, "of the", user_function_name)
  
  # Convert input values
  x <- .perform_type_conversion(x=x, to_type=preset_list$type, var_name=parameter_name, req_length=1, allow_more=TRUE)
  
  # Check if the parameter has the allowed values
  if(!is.null(preset_list$valid_range)){
    valid_range <- preset_list$valid_range
  } else {
    valid_range <- preset_list$range
  }

  if(preset_list$type %in% c("numeric", "integer")){
    sapply(x, .check_number_in_valid_range, var_name=parameter_name, range=valid_range)
  } else {
    .check_parameter_value_is_valid(x=x, var_name=parameter_name, values=valid_range)
  }
  
  # For parameters that are not randomised only a single value is allowed
  if(!preset_list$randomise & length(x) > 1){
    ..error_variable_has_too_many_values(x, var_name=parameter_name, req_length=1, allow_fewer=FALSE)
  }
  
  return(x)
}



.get_preset_hyperparameters <- function(data=NULL, fs_method=NULL, learner=NULL, outcome_type=NULL, names_only=FALSE){

  if(!is.null(fs_method)){
    is_vimp <- TRUE
  } else if(!is.null(learner)){
    is_vimp <- FALSE
  } else {
    ..error_reached_unreachable_code("get_preset_hyperparameters_fs_method_and_learner_both_null")
  }
  
  if(is_empty(data)){
    names_only <- TRUE
  }
  
  # Internal error checks. We should be able to obtain the outcome_type.
  if(is.null(data) & is.null(outcome_type)){
    ..error_reached_unreachable_code("get_preset_hyperparameters_outcome_type_missing")
  } else if(!is(data, "dataObject") & is.null(outcome_type)){
    ..error_reached_unreachable_code("get_preset_hyperparameters_outcome_type_missing")
  }
  
  if(is.null(outcome_type)){
    outcome_type <- data@outcome_type
  }
  
  # Find the parameter list
  if(is_vimp){
    preset_list <- vimp.get_fs_parameters(data=data,
                                          method=fs_method,
                                          outcome_type=outcome_type, 
                                          names_only=names_only)
    
  } else {
    preset_list <- learner.get_model_hyperparameters(data=data,
                                                     learner=learner,
                                                     outcome_type=outcome_type,
                                                     names_only=names_only)
  }
  
  return(preset_list)
}



.update_hyperparameters <- function(parameter_list, user_list=NULL){
  
  # Check if any parameters are provided
  if(length(parameter_list) == 0){ return(parameter_list) }
  
  for(parameter_name in names(parameter_list)){
    
    if(!is.null(user_list[[parameter_name]])){
      
      # Obtain values provided by the user
      user_values <- user_list[[parameter_name]]
      
      # The signature size depends on the number of features, which may not be a
      # priori known by the user. To avoid errors, we force sign_size to be in
      # the dynamically determined preset range. All values over the maximum
      # range are silently set to the edge of the range. We don't enforce
      # uniqueness, as the number of values affects how this parameter is
      # interpreted.
      if(parameter_name == "sign_size"){
        user_values[user_values > parameter_list[[parameter_name]]$range[2] ] <- parameter_list[[parameter_name]]$range[2]
      }
      
      if(length(user_values) == 1){
        # User provides one value for a parameter
        parameter_list[[parameter_name]]$init_config <- user_values
        parameter_list[[parameter_name]]$randomise <- FALSE
        
      } else if(length(user_values) > 1){
        # User provides multiple values for a parameter
        parameter_list[[parameter_name]]$init_config <- user_values
        parameter_list[[parameter_name]]$range <- user_values
        parameter_list[[parameter_name]]$randomise <- TRUE
        
      } else {
        # This code should never be reached as such cases should be captures by
        # .parse_hyperparameters earlier in the workflow.
        ..error_reached_unreachable_code("update_hyperparameters_zero_length_argument")
      }
    }
  }
  
  return(parameter_list)
}



.any_randomised_hyperparameters <- function(parameter_list){
  # Hyper-parameters are randomised by the sequential model boosting algorithm.
  # However, some hyperparameters may not require randomisation, or settings
  # were provided by the user. In case no hyper-parameters are to be randomised,
  # SMBO is not required, and the algorithm is the stop early. Therefore, this
  # function is used to check whether there are any randomisable
  # hyper-parameters.
  
  if(length(parameter_list) == 0){ return(FALSE) }

  # Determine if any parameters require randomisation
  requires_randomisation <- sapply(parameter_list, function(list_entry) (list_entry$randomise))
  
  # Return FALSE if no feature is randomised, and TRUE if any feature is randomised.
  return(sum(requires_randomisation) > 0)
}


.set_hyperparameter <- function(default, type, range, valid_range=NULL, randomise=FALSE, distribution=NULL){
  
  # Initialise list
  hyperparameter <- list()
  
  # Set default values
  hyperparameter$init_config <- default
  
  # Set type of hyperparameter
  hyperparameter$type <- type
  if(!type %in% c("numeric", "integer", "factor", "logical")){
    # Check typos
    ..error_reached_unreachable_code("set_hyperparameter_unknown_type")
  }
  
  # Set range
  hyperparameter$range <- range
  
  # Set the range of valid values
  if(is.null(valid_range)){
    hyperparameter$valid_range <- range
  } else {
    hyperparameter$valid_range <- valid_range
  }
  
  # Set randomisation
  hyperparameter$randomise <- randomise
  
  if(!is.null(distribution)){
    hyperparameter$rand_distr <- distribution
    if(!distribution %in% c("log")){
      # Check typos
      ..error_reached_unreachable_code("set_hyperparameter_unknown_distribution")
    }
  }
  
  return(hyperparameter)
}

#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
#' @include VimpS4Concordance.R
#' @include VimpS4Correlation.R
#' @include VimpS4MutualInformation.R
#' @include VimpS4OtherMethods.R
#' @include VimpS4Regression.R
NULL



setMethod("promote_vimp_method", signature(object="familiarVimpMethod"),
          function(object){
            
            # Extract vimp_method.
            method <- object@vimp_method
            
            if(method %in% .get_available_concordance_vimp_method()){
              # Concordance-based methods.
              object <- methods::new("familiarConcordanceVimp", object)
              
            } else if(method %in% .get_available_univariate_mutual_information_vimp_method()){
              # Mutual information maximisation.
              object <- methods::new("familiarCoreUnivariateMutualInfoVimp", object)
              
            } else if(method %in% .get_available_multivariate_mutual_information_vimp_method()){
              # Multivariate information methods.
              object <- methods::new("familiarCoreMultivariateMutualInfoVimp", object)
              
            } else if(method %in% .get_available_correlation_vimp_methods()){
              # Correlation-based methods.
              object <- methods::new("familiarCorrelationVimp", object)
              
            } else if(method %in% .get_available_corelearn_gini_vimp_method()){
              # Gini measure.
              object <- methods::new("familiarCoreLearnGiniVimp", object)
              
            } else if(method %in% .get_available_corelearn_mdl_vimp_method()){
              # MDL variable importance method.
              object <- methods::new("familiarCoreLearnMDLVimp", object)
              
            } else if(method %in% .get_available_corelearn_relieff_exp_rank_vimp_method()){
              # ReliefF with exponentially decreasing rank.
              object <- methods::new("familiarCoreLearnRelieffExpRankVimp", object)
              
            } else if(method %in% .get_available_corelearn_gain_ratio_vimp_method()){
              # Gain ratio measure.
              object <- methods::new("familiarCoreLearnGainRatioVimp", object)
              
            } else if(method %in% .get_available_regression_vimp_methods()){
              # Regression-based methods.
              object <- methods::new("familiarRegressionVimp", object)
            
            } else if(method %in% .get_available_glmnet_ridge_vimp_methods()){
              # Ridge penalised regression model-based methods.
              
              # Create a familiarModel and promote to the right class.
              object <- methods::new("familiarModel",
                                     fs_method = "none",
                                     learner = method,
                                     outcome_type = object@outcome_type,
                                     hyperparameters = object@hyperparameters,
                                     outcome_info = object@outcome_info,
                                     feature_info = object@feature_info,
                                     required_features = object@required_features,
                                     run_table = object@run_table,
                                     project_id = object@project_id)
              
              # Promote to the correct subclass.
              object <- promote_learner(object)
              
            } else if(method %in% .get_available_glmnet_lasso_vimp_methods()){
              # Lasso penalised regression model-based methods.
              
              # Create a familiarModel and promote to the right class.
              object <- methods::new("familiarModel",
                                     fs_method = "none",
                                     learner = method,
                                     outcome_type = object@outcome_type,
                                     hyperparameters = object@hyperparameters,
                                     outcome_info = object@outcome_info,
                                     feature_info = object@feature_info,
                                     required_features = object@required_features,
                                     run_table = object@run_table,
                                     project_id = object@project_id)
              
              # Promote to the correct subclass.
              object <- promote_learner(object)
              
            } else if(method %in% .get_available_glmnet_elastic_net_vimp_methods()){
              # Elastic net penalised regression model-based methods.
              
              # Create a familiarModel and promote to the right class.
              object <- methods::new("familiarModel",
                                     fs_method = "none",
                                     learner = method,
                                     outcome_type = object@outcome_type,
                                     hyperparameters = object@hyperparameters,
                                     outcome_info = object@outcome_info,
                                     feature_info = object@feature_info,
                                     required_features = object@required_features,
                                     run_table = object@run_table,
                                     project_id = object@project_id)
              
              # Promote to the correct subclass.
              object <- promote_learner(object)
               
            } else if(method %in% .get_available_rfsrc_vimp_methods()){
              # Random forest variable importance methods.
              
              # Create a familiarModel and promote to the right class.
              object <- methods::new("familiarModel",
                                     fs_method = "none",
                                     learner = "random_forest_rfsrc",
                                     outcome_type = object@outcome_type,
                                     hyperparameters = object@hyperparameters,
                                     outcome_info = object@outcome_info,
                                     feature_info = object@feature_info,
                                     required_features = object@required_features,
                                     run_table = object@run_table,
                                     project_id = object@project_id)
              
              # Promote to the correct subclass.
              object <- promote_learner(object)
              
              # Set the variable importance parameters for the method.
              object <- ..set_vimp_parameters(object, method=method)

            } else if(method %in% .get_available_ranger_vimp_methods()){
              # Ranger random forest variable importance methods.
              
              # Create a familiarModel and promote to the right class.
              object <- methods::new("familiarModel",
                                     fs_method = "none",
                                     learner = "random_forest_ranger",
                                     outcome_type = object@outcome_type,
                                     hyperparameters = object@hyperparameters,
                                     outcome_info = object@outcome_info,
                                     feature_info = object@feature_info,
                                     required_features = object@required_features,
                                     run_table = object@run_table,
                                     project_id = object@project_id)
              
              # Promote to the correct subclass.
              object <- promote_learner(object)
              
              # Set the variable importance parameters for the method.
              object <- ..set_vimp_parameters(object, method=method)
              
            } else if(method %in% .get_available_random_vimp_methods()){
              # Random, none and signature only methods.
              object <- methods::new("familiarRandomVimp", object)
              
            } else if(method %in% .get_available_none_vimp_methods()){
              # Random, none and signature only methods.
              object <- methods::new("familiarNoneVimp", object)
              
            } else if(method %in% .get_available_signature_only_vimp_methods()){
              # Random, none and signature only methods.
              object <- methods::new("familiarSignatureVimp", object)
              
            }
            
            return(object)
          })



.get_vimp_hyperparameters <- function(data, method, outcome_type, names_only=FALSE){

  # Get the outcome type from the data object, if available
  if(!is.null(data)) outcome_type <- data@outcome_type
  
  # Create familiarModel
  vimp_method_object <- methods::new("familiarVimpMethod",
                                     vimp_method=method,
                                     outcome_type=outcome_type)
  
  # Set up the specific model
  vimp_method_object <- promote_vimp_method(vimp_method_object)
  
  # Variable importance hyperparameters
  model_hyperparameters <- get_default_hyperparameters(vimp_method_object, data=data)
  
  # Extract names from parameter list
  if(names_only==TRUE){
    model_hyperparameters <- names(model_hyperparameters)
  }

  # Return hyperparameter list, or names of parameters
  return(model_hyperparameters)
}



vimp.check_outcome_type <- function(method, outcome_type, as_flag=FALSE){

  # Create familiarModel
  vimp_method_object <- methods::new("familiarVimpMethod",
                                     vimp_method=method,
                                     outcome_type=outcome_type)
  
  # Set up the specific model
  vimp_method_object <- promote_vimp_method(vimp_method_object)
  
  # Check validity.
  vimp_method_available <- is_available(vimp_method_object)
  
  if(as_flag) return(vimp_method_available)
  
  # Check if the vimp method or familiar model has been successfully promoted.
  if(!is_subclass(class(vimp_method_object)[1], "familiarVimpMethod") & !is_subclass(class(vimp_method_object)[1], "familiarModel")){
    stop(paste0(method, " is not a valid variable importance method. Please check the vignette for available methods."))
  }

  if(!vimp_method_available) {
    stop(paste0(method, " is not available for \"", outcome_type, "\" outcomes."))
  }
  
  # Check that the required package can be loaded.
  require_package(x=vimp_method_object,
                  purpose=paste0("to assess variable importance using the ", method, " method"),
                  message_type="backend_error")
}



vimp.check_fs_parameters <- function(method, user_param, outcome_type){

  # Check if current method is defined in the user parameter list
  if(is.null(user_param[[method]])) { return(NULL) }

  # Get parameters defined by the model
  defined_param <- .get_vimp_hyperparameters(data=NULL, method=method, outcome_type=outcome_type, names_only=TRUE)

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



setMethod("prepare_vimp_object", signature(data="data.table"),
          function(data, vimp_method, vimp_method_parameter_list=list(), ...){
            
            # Convert data to dataObject.
            data <- do.call(as_data_object, args=c(list("data"=data),
                                                   list(...)))
            
            return(do.call(prepare_vimp_object, args=c(list("data"=data,
                                                            "vimp_method"=vimp_method,
                                                            "vimp_method_parameter_list"=vimp_method_parameter_list),
                                                       list(...))))
          })



setMethod("prepare_vimp_object", signature(data="dataObject"),
          function(data, vimp_method, vimp_method_parameter_list=list(), ...){
            
            #####Prepare settings###############################################
            
            # Reconstitute settings from the data.
            settings <- extract_settings_from_data(data)
            
            # Update some missing settings that can be fixed within this method.
            settings$data$train_cohorts <- unique(data@data[[get_id_columns(single_column="batch")]])
            
            # Parse the remaining settings that are important. Remove
            # outcome_type from ... This prevents an error caused by multiple
            # matching arguments.
            dots <- list(...)
            dots$parallel <- NULL
            dots$fs_method <- NULL
            dots$fs_method_parameter <- NULL
            dots$learner <- NULL
            
            # Create setting_hyperparam so that it can be parsed correctly.
            if(!vimp_method %in% names(vimp_method_parameter_list) & length(vimp_method_parameter_list) > 0){
              setting_hyperparam <- list()
              setting_hyperparam[[vimp_method]] <- vimp_method_parameter_list
              
            } else {
              setting_hyperparam <- vimp_method_parameter_list
            }
            
            settings <- do.call(.parse_general_settings,
                                args=c(list("settings"=settings,
                                            "data"=data@data,
                                            "parallel"=FALSE,
                                            "fs_method"=vimp_method,
                                            "learner"="glm",
                                            "fs_method_parameter"=setting_hyperparam),
                                       dots))
            
            # Push settings to the backend.
            .assign_settings_to_global(settings=settings)
            

            
            #####Prepare featureInfo objects####################################
            
            # Create a list of featureInfo objects.
            feature_info_list <- .get_feature_info_data(data=data@data,
                                                        file_paths=NULL,
                                                        project_id=character(),
                                                        outcome_type=settings$data$outcome_type)
            
            # Extract the generic data.
            feature_info_list <- feature_info_list[["generic"]]
            
            # Perform some pre-processing (i.e. remove singular features)
            feature_info_list <- .determine_preprocessing_parameters(cl=NULL,
                                                                     data=data,
                                                                     feature_info_list=feature_info_list,
                                                                     settings=settings,
                                                                     verbose=FALSE)
            
            #####Prepare hyperparameters########################################
            
            # Get default hyperparameters.
            param_list <- .get_preset_hyperparameters(data=data,
                                                      fs_method=vimp_method,
                                                      names_only=FALSE)
            
            # Update with user-provided settings.
            param_list <- .update_hyperparameters(parameter_list=param_list,
                                                  user_list=settings$fs$param[[vimp_method]])
            
            # Determine which hyperparameters still need to be specified.
            unset_parameters <- sapply(param_list, function(hyperparameter_entry) hyperparameter_entry$randomise)
            
            # Mark sign-size as set, as it is not used for variable importance.
            if("sign_size" %in% names(unset_parameters)) unset_parameters["sign_size"] <- FALSE
            
            # Raise an error if any hyperparameters were not set.
            if(any(unset_parameters)){
              stop(paste0("The following hyperparameters need to be specified: ",
                          paste0(names(unset_parameters)[unset_parameters], collapse=", ")))
            }
            
            # Obtain the final list of hyperparameters.
            param_list <- lapply(param_list, function(hyperparameter_entry) hyperparameter_entry$init_config)
            
            
            #####Prepare vimp object#########################################
            
            # Create a familiar variable importance method.
            object <- methods::new("familiarVimpMethod",
                                   outcome_type = settings$data$outcome_type,
                                   vimp_method = vimp_method,
                                   hyperparameters = param_list,
                                   outcome_info = data@outcome_info,
                                   feature_info = feature_info_list,
                                   required_features = find_required_features(features=get_feature_columns(data),
                                                                              feature_info_list=feature_info_list))
            
            # Promote object to correct subclass.
            object <- promote_vimp_method(object)

            # Return in list.
            return(object)
          })

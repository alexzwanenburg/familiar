#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setMethod("train", signature(data="data.table"),
          function(data, learner, hyperparameter_list=list(), ...){
            
            # Load settings from ellipsis
            settings <- do.call(.parse_initial_settings, args=c(list("experimental_design"="fs+mb"),
                                                                list(...)))
            
            # Convert data.table to a dataObject.
            data <- do.call(.load_data, args=append(list("data"=data), settings$data))
            
            # Update settings
            settings <- .update_initial_settings(data=data, settings=settings)
            
            # Parse data
            data <- .finish_data_preparation(data = data,
                                             sample_id_column = settings$data$sample_col,
                                             batch_id_column = settings$data$batch_col,
                                             outcome_column = settings$data$outcome_col,
                                             outcome_type = settings$data$outcome_type,
                                             include_features = settings$data$include_features,
                                             class_levels = settings$data$class_levels,
                                             censoring_indicator=settings$data$censoring_indicator,
                                             event_indicator=settings$data$event_indicator,
                                             competing_risk_indicator=settings$data$competing_risk_indicator)
            
            # Derive experimental design
            experiment_setup <- extract_experimental_setup(experimental_design=settings$data$exp_design,
                                                           file_dir=NULL,
                                                           verbose=FALSE)
            
            # Check experiment settings
            settings <- .update_experimental_design_settings(section_table=experiment_setup,
                                                             data=data,
                                                             settings=settings)
            
            # Create dataObject from data.
            data <- methods::new("dataObject",
                                 data=data,
                                 is_pre_processed=FALSE,
                                 outcome_type=settings$data$outcome_type)
            
            return(do.call(train, args=c(list("data"=data,
                                              "learner"=learner,
                                              "settings"=settings,
                                              "hyperparameter_list"=hyperparameter_list),
                                         list(...))))
          })


setMethod("train", signature(data="dataObject"),
          function(data, learner, hyperparameter_list=list(), settings=NULL, ...){

            #####Prepare settings###############################################
            
            if(is.null(settings)){
              # Load settings from ellipsis
              settings <- do.call(.parse_initial_settings, args=c(list("experimental_design"="fs+mb"),
                                                                  list(...)))
              
              # Update settings
              settings <- .update_initial_settings(data=data@data, settings=settings)
              
              # Derive experimental design
              experiment_setup <- extract_experimental_setup(experimental_design=settings$data$exp_design,
                                                             file_dir=NULL,
                                                             verbose=FALSE)
              
              # Check experiment settings
              settings <- .update_experimental_design_settings(section_table=experiment_setup,
                                                               data=data@data,
                                                               settings=settings)
            }
            
            # Parse the remaining settings that are important. Remove
            # outcome_type from ... This prevents an error caused by multiple
            # matching arguments.
            dots <- list(...)
            dots$parallel <- NULL
            dots$fs_method <- NULL
            dots$hyperparameter <- NULL

            # Create hyperparam setting so that it can be parsed correctly.
            if(!learner %in% names(hyperparameter_list) & length(hyperparameter_list) > 0){
              setting_hyperparam <- list()
              setting_hyperparam[[learner]] <- hyperparameter_list
              
            } else {
              setting_hyperparam <- hyperparameter_list
            }
            
            settings <- do.call(.parse_general_settings,
                                args=c(list("settings"=settings,
                                            "data"=data@data,
                                            "parallel"=FALSE,
                                            "fs_method"="none",
                                            "learner"=learner,
                                            "hyperparameter"=setting_hyperparam),
                                       dots))
            
            # Push settings to the backend.
            .assign_settings_to_global(settings=settings)
            
            
            #####Prepare outcome_info###########################################
            
            # Create a generic outcome object
            outcome_info <- create_outcome_info(settings=settings)
            
            
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
            
            # Get the features names.
            selected_features <- get_feature_columns(data)
            
            # Find features that are required for processing the data
            required_features <- find_required_features(features=selected_features,
                                                        feature_info_list=feature_info_list)
            
            # Find important features, i.e. those that constitute the signature either individually or as part of a cluster
            important_features <- find_important_features(features=selected_features,
                                                          feature_info_list=feature_info_list)
            
            #####Prepare hyperparameters########################################
            
            # Get default hyperparameters.
            param_list <- .get_preset_hyperparameters(data=data,
                                                      learner=learner,
                                                      names_only=FALSE)
            
            # Update with user-provided settings.
            param_list <- .update_hyperparameters(parameter_list=param_list,
                                                  user_list=settings$mb$hyper_param[[learner]])
            
            # Determine which hyperparameters still need to be specified.
            unset_parameters <- sapply(param_list, function(hyperparameter_entry) hyperparameter_entry$randomise)
            
            # Raise an error if any hyperparameters were not set.
            if(any(unset_parameters)){
              stop(paste0("The following hyperparameters need to be specified: ",
                          paste0(names(unset_parameters)[unset_parameters], collapse=", ")))
            }
            
            # Obtain the final list of hyperparameters.
            param_list <- lapply(param_list, function(hyperparameter_entry) hyperparameter_entry$init_config)

            
            #####Prepare model and data#########################################
            
            # Create familiar model
            object <- methods::new("familiarModel",
                                   outcome_type = settings$data$outcome_type,
                                   learner = learner,
                                   fs_method = "none",
                                   hyperparameters = param_list,
                                   signature = selected_features,
                                   req_feature_cols =  required_features,
                                   important_features = important_features,
                                   feature_info = feature_info_list,
                                   outcome_info = outcome_info,
                                   settings=settings$eval)
            
            # Preprocess data.
            data <- process_input_data(object=object,
                                       data=data,
                                       is_pre_processed=data@is_pre_processed,
                                       stop_at="clustering")
            
            # Add package version/
            object <- add_package_version(object=object)
            
            # Train model.
            object <- .train(object=object, data=data, get_additional_info=TRUE)
            
            return(object)
          })

#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setMethod("train", signature(data="data.table"),
          function(data, learner, hyperparameter_list=list(), create_bootstrap=FALSE, ...){
            
            # Convert data to dataObject.
            data <- do.call(as_data_object, args=c(list("data"=data),
                                                   list(...)))
            
            return(do.call(train, args=c(list("data"=data,
                                              "learner"=learner,
                                              "hyperparameter_list"=hyperparameter_list,
                                              "create_bootstrap"=create_bootstrap),
                                         list(...))))
          })


setMethod("train", signature(data="dataObject"),
          function(data, learner, hyperparameter_list=list(), create_bootstrap=FALSE, create_novelty_detector=FALSE, ...){
            
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
            dots$hyperparameter <- NULL

            # Create setting_hyperparam so that it can be parsed correctly.
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
            outcome_info <- data@outcome_info
            
            
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
            
            # Remove invariant features from the data
            data <- filter_features(data=data,
                                    available_features=get_available_features(feature_info_list=feature_info_list))
            
            # Get the features names.
            selected_features <- get_feature_columns(data)
            
            # Find features that are required for processing the data
            required_features <- find_required_features(features=selected_features,
                                                        feature_info_list=feature_info_list)
            
            # Find important features, i.e. those that constitute the signature either individually or as part of a cluster
            model_features <- find_model_features(features=selected_features,
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
                                   required_features =  required_features,
                                   model_features = model_features,
                                   run_table = get_placeholder_run_table(),
                                   feature_info = feature_info_list,
                                   outcome_info = outcome_info,
                                   settings=settings$eval,
                                   project_id = 0)

            # Add package version/
            object <- add_package_version(object=object)
            
            # Process data.
            data <- process_input_data(object=object,
                                       data=data,
                                       stop_at="clustering")
            
            # Create bootstraps.
            if(create_bootstrap){
              data <- select_data_from_samples(data=data,
                                               samples=fam_sample(x=data@data,
                                                                  replace=TRUE))
            }
            
            # Train model.
            object <- .train(object=object, data=data, get_additional_info=TRUE)
            
            # Add novelty detector
            if(create_novelty_detector){
              object <- .train_novelty_detector(object=object,
                                                data=data)
            }

            return(object)
          })



get_placeholder_run_table <- function(){
  return(data.table::data.table("run_id"=1L, "data_id"=1L, "can_pre_process"=TRUE, "perturbation"="main", "perturb_level"=1))
}

#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

#####complete_familiar_ensemble#####
setMethod("complete_familiar_ensemble", signature(object="familiarEnsemble"),
          function(object, dir_path=NULL) {
            # Fills out missing data from a familiarEnsemble based on attached models and internal logic
            # Four slots are expected to be filled: model_list, run_table, learner, fs_method
            # The remaining slots are set here.
            
            # Load models
            object <- load_models(object=object, dir_path=dir_path)
            
            # Add outcome_type and class levels to object.
            object@outcome_type <- object@model_list[[1]]@outcome_type
            object@outcome_info <- .aggregate_outcome_info(x=lapply(object@model_list, function(list_elem) (list_elem@outcome_info)))
              
            # Find all required features
            required_features <- unique(unlist(lapply(object@model_list, function(fam_model) (fam_model@required_features))))
            
            # Find all important features
            model_features <- unique(unlist(lapply(object@model_list, function(fam_model) (fam_model@model_features))))
            
            # Aggregate feature information
            feature_info_list <- lapply(required_features, collect_and_aggregate_feature_info, object=object, stop_at="imputation")
            names(feature_info_list) <- required_features
            
            # Aggregate calibration info
            calibration_info <- extract_calibration_info(object=object)

            # Generate a new version of the ensemble to avoid unnecessary copying
            fam_ensemble <- methods::new("familiarEnsemble",
                                         model_list = object@model_list,
                                         outcome_type = object@outcome_type,
                                         outcome_info = object@outcome_info,
                                         data_column_info = object@model_list[[1]]@data_column_info,
                                         learner = object@learner,
                                         fs_method = object@fs_method,
                                         required_features = required_features,
                                         model_features = model_features,
                                         feature_info = feature_info_list,
                                         run_table = object@run_table,
                                         calibration_info = calibration_info,
                                         settings = object@model_list[[1]]@settings,
                                         is_anonymised = all(extract_from_slot(object_list=object@model_list, slot_name="is_anonymised")),
                                         project_id = object@model_list[[1]]@project_id)
            
            # Add package version to the ensemble
            fam_ensemble <- add_package_version(object=fam_ensemble)
            
            return(fam_ensemble)
          })


setMethod("show", signature(object="familiarEnsemble"),
          function(object){
            
            # Flag to show additional information.
            show_additional_information <- TRUE
            
            if(!is_model_loaded(object)){
              cat(paste0("An ensemble of ", length(object@model_list), " ",
                         object@learner, " ",
                         ifelse(length(object@model_list) == 1, "model", "models"),
                         " (v", object@familiar_version, ").\n"))
              
            } else {
              cat(paste0("An ensemble of ", length(object@model_list), " ",
                         object@learner, " ",
                         ifelse(length(object@model_list) == 1, "model", "models"),
                         " (v", object@familiar_version, ").\n"))
              
              # Determine how many models are trained.
              model_trained <- sapply(object@model_list, model_is_trained)
              
              if(length(model_trained) == 1){
                show(object@model_list[[1]])
                
                # Update the flag to prevent showing too much information.
                show_additional_information <- FALSE
                
              } else {
                if(all(model_trained)){
                  cat("All models were successfully trained.\n")
                  
                } else if(any(model_trained)){
                  cat(paste0(sum(model_trained), " of ", length(model_trained), " models were successfully trained.\n"))
                  
                } else {
                  cat("No model was successfully trained.\n")
                }
                
              }
            }
            
            if(show_additional_information){
              
              # Outcome details
              cat("\nThe following outcome was modelled:\n")
              show(object@outcome_info)
              
              # Details concerning variable importance.
              cat(paste0("\nVariable importance was determined using the ", object@fs_method, " variable importance method.\n"))
              
              # Details concerning model features:
              cat("\nThe following features were used in the ensemble:\n")
              lapply(object@model_features, function(x, object) cat(.show_simple_feature_info(object@feature_info[[x]], line_end=".\n")), object=object)
            }
          })



#####assess_calibration (ensemble)#####
setMethod("assess_calibration", signature(object="familiarEnsemble"),
          function(object, data, eval_times=NULL, is_pre_processed=FALSE){
            
            # Load eval_times from the object settings attribute, if it is not provided.
            if(is.null(eval_times)){
              eval_times <- object@settings$eval_times
            }
            
            # Check eval_times argument
            if(object@outcome_type %in% c("survival")){
              sapply(eval_times, .check_number_in_valid_range, var_name="eval_times", range=c(0.0, Inf), closed=c(FALSE, TRUE))
            }
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)){
              ..error_ensemble_models_not_loaded()
            }
            
            # This function is the same for familiarModel and familiarEnsemble objects
            return(.assess_calibration(object=object, data=data, eval_times=eval_times, is_pre_processed=is_pre_processed))
          })

#####compute_calibration_data (ensemble, dataObject)#####
setMethod("compute_calibration_data", signature(object="familiarEnsemble", data="dataObject"),
          function(object, data, time=NULL){
            
            # This function is the same for familiarModel and familiarEnsemble objects
            return(.compute_calibration_data(object=object, data=data, time=time))
          })


.get_available_stratification_ensemble_methods <- function(){
  return(c("ensemble_mode", "ensemble_mean", "mean_threshold", "median_threshold"))
}


#####assess_stratification (ensemble)#####
setMethod("assess_stratification", signature(object="familiarEnsemble"),
          function(object,
                   data=NULL,
                   ensemble_method=waiver(),
                   time_max=waiver(),
                   risk_group_list=NULL,
                   stratification_ensemble_method=waiver(), ...){
            
            # Only assess stratification for survival outcomes.
            if(!object@outcome_type %in% c("survival")) return(NULL)
            
            # Load time_max from the object settings attribute, if it is not provided.
            if(is.waive(time_max) & object@outcome_type %in% c("survival")){
              time_max <- object@settings$time_max
            }
            
            # Check time_max argument
            if(object@outcome_type %in% c("survival")){
              .check_number_in_valid_range(time_max, var_name="time_max",
                                           range=c(0.0, Inf), closed=c(FALSE, TRUE))
            }
            
            # Obtain ensemble method from stored settings, if required.
            if(is.waive(ensemble_method)) ensemble_method <- object@settings$ensemble_method
            
            # Check ensemble_method argument
            .check_parameter_value_is_valid(x=ensemble_method, var_name="ensemble_method",
                                            values=.get_available_ensemble_prediction_methods())
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)) ..error_ensemble_models_not_loaded()

            # Generate risk groups
            if(is.null(risk_group_list)){
              # Extract risk groups for kaplan-meier survival data
              risk_group_list <- assign_risk_groups(object=object,
                                                    data=data,
                                                    ensemble_method=ensemble_method,
                                                    stratification_ensemble_method=stratification_ensemble_method,
                                                    time=time_max,
                                                    verbose=FALSE)
            }
            
            # Assess stratification
            test_results <- lapply(risk_group_list, function(risk_group_table, object){
              
              # Extract test data
              test_data <- learner.assess_stratification(risk_group_table=risk_group_table)
              
              # Add model name
              test_data$logrank <- add_model_name(data=test_data$logrank, object=object)
              test_data$hr_ratio <- add_model_name(data=test_data$hr_ratio, object=object)
              
              return(test_data)
            }, object=object)
            
            return(list("data"=risk_group_list, "test"=test_results, "time_max"=time_max))
          })


#####assign_risk_groups#####
setMethod("assign_risk_groups", signature(object="familiarEnsemble", data="dataObject"),
          function(object,
                   data,
                   ensemble_method,
                   time,
                   stratification_ensemble_method=waiver(),
                   verbose=FALSE){
            # Assign risk groups, perform log-rank tests and determine survival curves for each group
            
            # Suppress NOTES due to non-standard evaluation in data.table
            risk_group <- value <- NULL
            
            # Only assess stratification for survival outcomes.
            if(!object@outcome_type %in% c("survival")) return(NULL)
            
            # Message extraction start
            if(verbose){
              logger.message(paste0("Assigning risk groups based on predicted values and cutoffs."))
            }
            
            # Load risk ensemble method from object settings, if not provided.
            if(is.waive(stratification_ensemble_method)){
              stratification_ensemble_method <- object@settings$strat_ensemble_method
            }
            
            # Test stratification_ensemble_method to see if it is valid.
            .check_parameter_value_is_valid(x=stratification_ensemble_method,
                                            var_name="stratification_ensemble_method",
                                            values=.get_available_stratification_ensemble_methods())
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)) ..error_ensemble_models_not_loaded()
            
            # Determine the stratification methods
            strat_methods <- unique(extract_from_slot(object_list=object@model_list,
                                                      slot_name="km_info",
                                                      slot_element="stratification_method",
                                                      na.rm=TRUE))
            
            # Iterate over stratification methods
            risk_group_data <- lapply(strat_methods, function(stratification_method, object, data, time, ensemble_method, method){
              
              if(method %in% c("ensemble_mode", "ensemble_mean")){
                # Use cut-offs on models and aggregate grouping
                
                # Collect risk group data for each model.
                risk_group_data <- lapply(object@model_list, function(object, data, time, stratification_method){
                  
                  # Assign risk groups
                  risk_group_data <- assign_risk_groups(object=object,
                                                        data=data,
                                                        stratification_method=stratification_method,
                                                        time=time)[[stratification_method]]
                  
                  if(is_empty(risk_group_data)) return(NULL)
                  
                  return(risk_group_data)
                },
                data=data,
                time=time,
                stratification_method=stratification_method)
                
                # Parse data to single data.table
                risk_group_data <- data.table::rbindlist(risk_group_data)
                
                # Check that the data is not empty
                if(is_empty(risk_group_data)) return(NULL)
                
                # Create risk groups according to the corresponding method.
                if(method == "ensemble_mean"){
                  risk_group_data <- risk_group_data[, list("risk_group"=learner.get_mean_risk_group(risk_group)),
                                                     by=c(get_id_columns(), "outcome_time", "outcome_event")]
                  
                } else if(method == "ensemble_mode"){
                  risk_group_data <- risk_group_data[, list("risk_group"=get_mode(risk_group)),
                                                     by=c(get_id_columns(), "outcome_time", "outcome_event")]
                }
                
              } else if(method %in% c("mean_threshold", "median_threshold")){
                # Aggregate cut-offs and use ensemble predictions
                
                # Extract stratification parameters
                km_parameters <- extract_from_slot(object_list=object@model_list,
                                                   slot_name="km_info",
                                                   slot_element="parameters",
                                                   na.rm=TRUE)
                
                # Predict values.
                prediction_table <- .predict(object=object,
                                            data=data,
                                            time=time,
                                            ensemble_method=ensemble_method)
                
                # Check if there are any valid predictions.
                if(!any_predictions_valid(prediction_table=prediction_table, outcome_type=object@outcome_type)) return(NULL)
                
                # Collect cutoffs
                cutoff <- lapply(km_parameters, function(km_info, strat_method){
                  if(strat_method == km_info$method){
                    cutoff_data <- data.table::data.table("value"=km_info$cutoff, "index"=seq_len(length(km_info$cutoff)))
                  } else {
                    cutoff_data <- data.table::data.table("value"=numeric(0), "index"=integer(0))
                  }
                  
                  return(cutoff_data)
                  
                }, strat_method=stratification_method)
                
                # Parse to list
                cutoff <- data.table::rbindlist(cutoff)

                # Generate cutoff values
                if(method == "mean_threshold"){
                  # Mean cutoff value
                  cutoff <- cutoff[, list("value"=mean(value)), by="index"]$value
                  
                } else if(method == "median_threshold"){
                  # Median cutoff value
                  cutoff <- cutoff[, list("value"=stats::median(value)), by="index"]$value
                  
                }
                
                # Get risk groups
                risk_group <- learner.apply_risk_threshold(object=object,
                                                           predicted_values=prediction_table$predicted_outcome,
                                                           cutoff=cutoff)
                
                # Generate data table
                risk_group_data <- prediction_table
                risk_group_data[, ":="("predicted_outcome"=NULL,
                                       "risk_group"=risk_group)]
                
              }
              
              # Add stratification method
              risk_group_data[, "strat_method":=stratification_method]
              
              # Add model name
              risk_group_data <- add_model_name(data=risk_group_data, object=object)
              
              return(risk_group_data)
              
            },
            object=object,
            data=data,
            ensemble_method=ensemble_method,
            method=stratification_ensemble_method,
            time=time)
            
            # Add names to the risk group list
            names(risk_group_data) <- strat_methods
            
            return(risk_group_data)
          })


#####get_prediction_type#####
setMethod("get_prediction_type", signature(object="familiarEnsemble"),
          function(object, ...){
            return(do.call(get_prediction_type, args=c(list("object" = ..get_model(object=object, ii=1)),
                                                       list(...))))
          })



#####..get_model (numeric, familiarEnsemble) ######
setMethod("..get_model", signature(ii="numeric", object="familiarEnsemble"),
          function(ii, object){
            if(ii > length(object@model_list)) ..error_reached_unreachable_code(paste0("..get_model,familiarEnsemble: the requested index (", ii,
                                                                                       ") exceeds the total number of models in the ensemble (", length(object@model_list)))
            
            # If the model is already attached, dispatch to calling function.
            if(is(object@model_list[[ii]], "familiarModel")) return(object@model_list[[ii]])
            
            # If the model has been detached, load, and then dispatch.
            # First check if the file exists.
            if(!file.exists(object@model_list[[ii]])){
              
              model_file_name <- ..get_model_file_path(ii=ii, object=object)
              
              # Obtain the directory path stored in the file_paths environment
              # variable (if any).
              mb_dir_path <- tryCatch({get_file_paths()$mb_dir}, error=function(err)(return(NULL)))
             
              # Using the file directory indicated by the file_paths environment
              # variable.
              if(!is.null(mb_dir_path)){
                # File is directly located in the given directory
                file_path_1 <- file.path(mb_dir_path,
                                         basename(object@model_list[[ii]]))
                
                if(file.exists(file_path_1)) return(load_familiar_object(file_path_1))
                
                # File is located in a subdirectory of mb_dir_path.
                file_path_2 <- file.path(get_object_dir_path(dir_path=mb_dir_path,
                                                             object_type="familiarModel",
                                                             learner=object@learner,
                                                             fs_method=object@fs_method),
                                         basename(object@model_list[[ii]]))
                
                if(file.exists(file_path_2)) return(load_familiar_object(file_path_2))
              }
              
              # Using the model_dir_path slot of the ensemble.
              if(!is.null(object@model_dir_path)){
                # File is directly located in the given directory
                file_path_1 <- file.path(object@model_dir_path,
                                         basename(object@model_list[[ii]]))
                
                if(file.exists(file_path_1)) return(load_familiar_object(file_path_1))
              }
              
              stop(paste0("..get_model,familiarEnsemble: cannot find the indicated familiarModel", object@model_list[[ii]]))
              
            } else {
              return(load_familiar_object(object@model_list[[ii]]))
            }
          })



#####..get_model (missing, familiarEnsemble) ######
setMethod("..get_model", signature(ii="missing", object="familiarEnsemble"),
          function(ii, object, ...){
            
            # Dispatch get_model.
            return(lapply(seq_along(object@model_list), ..get_model, object=object, ...))
          })



#####..get_model_file_path (numeric, familiarEnsemble) ######
setMethod("..get_model_file_path", signature(ii="numeric", object="familiarEnsemble"),
          function(ii, object, dir_path=NULL){
            
            if(ii > length(object@model_list)) ..error_reached_unreachable_code(paste0("..get_model_file_path,familiarEnsemble: the requested index (", ii,
                                                                                       ") exceeds the total number of models in the ensemble (", length(object@model_list)))
            
            # There are three directories a file can be located, namely:
            #
            # * dir_path: a user-specified path
            #
            # * mb_dir_path: based on file_paths environment variable
            #
            # * model_dir_path: based on the directory path stored with the
            # object.
            #
            # These possibilities are ordered by precedence.

            # Try to find dir_path
            if(!is.null(dir_path)){
              
              # Check if the dir_path actually points to a file, and strip the
              # file instead. Note that file.exists also returns TRUE if the
              # path is a directory.
              if(file.exists(dir_path) & !dir.exists(dir_path)) dir_path <- dirname(dir_path)
              
              # If the directory does not exist, set to NULL.
              if(!dir.exists(dir_path)) dir_path <- NULL
            }
            
            # Obtain the directory path stored in the file_paths environment
            # variable (if any).
            mb_dir_path <- tryCatch({get_file_paths()$mb_dir}, error=function(err)(return(NULL)))
            
            # Obtain the directory path from the familiarEnsemble object.
            model_dir_path <- object@model_dir_path
            
            # Identify the basic model file name.
            if(is(object@model_list[[ii]], "familiarModel")){
              # Generate the file name from the model.
              model_file_name <- get_object_name(object=object@model_list[[ii]], abbreviated=FALSE)
              model_file_name <- paste0(model_file_name, ".RDS")
              
            } else {
              # Generate the file name from the stored string.
              model_file_name <- basename(object@model_list[[ii]])
            }
              
            # Using the user-provided dir_path
            if(!is.null(dir_path)){
              # File is directly located in the given directory
              file_path_1 <- file.path(dir_path,
                                       model_file_name)
              
              if(file.exists(file_path_1)) return(file_path_1)
              
              # File is located in a subdirectory of dir_path.
              file_path_2 <- file.path(get_object_dir_path(dir_path=dir_path,
                                                           object_type="familiarModel",
                                                           learner=object@learner,
                                                           fs_method=object@fs_method),
                                       model_file_name)
              
              if(file.exists(file_path_2)) return(file_path_2)
            }
            
            # Using the file directory indicated by the file_paths environment
            # variable.
            if(!is.null(mb_dir_path)){
              # File is directly located in the given directory
              file_path_3 <- file.path(mb_dir_path,
                                       model_file_name)
              
              if(file.exists(file_path_3)) return(file_path_3)
              
              # File is located in a subdirectory of dir_path.
              file_path_4 <- file.path(get_object_dir_path(dir_path=mb_dir_path,
                                                           object_type="familiarModel",
                                                           learner=object@learner,
                                                           fs_method=object@fs_method),
                                       model_file_name)
              
              if(file.exists(file_path_4)) return(file_path_4)
            }
            
            # Using the model_dir_path slot of the ensemble. This takes least
            # precedence, as this might have changed after creating the ensemble
            # method.
            if(!is.na(model_dir_path)){
              # File is directly located in the given directory.
              file_path_5 <- file.path(model_dir_path,
                                       model_file_name)
              
              if(file.exists(file_path_5)) return(file_path_5)
              
              # File is located in a subdirectory of model_dir_path.
              file_path_6 <- file.path(get_object_dir_path(dir_path=model_dir_path,
                                                           object_type="familiarModel",
                                                           learner=object@learner,
                                                           fs_method=object@fs_method),
                                       model_file_name)
              
              if(file.exists(file_path_6)) return(file_path_6)
            }
            
            # Explicitly return NULL if the model could not be found on any
            # known paths.
            return(NULL)
          })



#####..get_model_file_path (missing, familiarEnsemble) ######
setMethod("..get_model_file_path", signature(ii="missing", object="familiarEnsemble"),
          function(ii, object, ...){
            
            # Dispatch get_model.
            return(lapply(seq_along(object@model_list), ..get_model_file_path, object=object, ...))
          })



#####..update_model_list#####
setMethod("..update_model_list", signature(object="familiarEnsemble"),
          function(object, dir_path=NULL, auto_detach=FALSE){
            
            # Determine if models can detach. Models cannot detach if detaching
            # them would lead them to be lost, i.e. there are no files on a
            # known drive location drive.
            if(!..can_detach_models(object=object, dir_path=dir_path)){
              auto_detach <- FALSE
              object@auto_detach <- FALSE
              object@model_dir_path <- NA_character_
            }
            
            if(auto_detach | object@auto_detach) object <- detach_models(object)
            
            for(ii in seq_along(object@model_list)){
              
              # Skip if the current entry is an attached familiarModel object.
              if(is(object@model_list[[ii]], "familiarModel")) next()
              
              # Stop if the current entry is not a character.
              if(!is.character(object@model_list[[ii]])) stop("The current entry in the model list is not a character string or a familiarModel object.")
              
              # Skip if the entry points to a file and not a directory.
              if(file.exists(object@model_list[[ii]]) & !dir.exists(object@model_list[[ii]])) next()
              
              # Identify the file path, if any.
              model_file_path <- ..get_model_file_path(ii=ii, object=object, dir_path=dir_path)
              
              # If there is a file path, add this to the model list, and update
              # the model_dir_path attribute.
              if(!is.null(model_file_path)){
                object@model_list[[ii]] <- model_file_path
                object@model_dir_path <- dirname(model_file_path)
              }
            }
            
            # Check if the models are either attached or are located on a known
            # drive location.
            model_exists <- sapply(object@model_list, function(list_entry){
              if(is(list_entry, "familiarModel")){
                return(TRUE)
                
              } else if(file.exists(list_entry)){
                return(TRUE)
                
              } else {
                return(FALSE)
              }
            })
            
            # Throw an error if any model does not exist.
            if(any(!model_exists)) stop(paste0("The following models in the ensemble could not be found: ",
                                               paste_s(unlist(object@model_list[!model_exists]))))
            
            # Check the model_dir_path slot if auto_detach is on. In that case
            # model_dir_path is required to find the models.
            if(auto_detach | object@auto_detach){
              
              # Check if all models are attached, because in that case we may
              # still need to determine the drive location.
              if(is_model_loaded(object)){
                # Identify the file path, if any.
                model_file_path <- ..get_model_file_path(ii=1, object=object, dir_path=dir_path)
                
                # Set the model_file_path explicitly.
                if(!is.null(model_file_path)) object@model_dir_path <- dirname(model_file_path)
              }
              
              # Check that a model directory path has been set and exists.
              if(is.na(object@model_dir_path)) stop("The model directory could not be established.")
              if(!dir.exists(object@model_dir_path)) stop("The model directory could not be established.")
            }
            
            return(object)
          })



#####..can_detach_models (numeric, familiarEnsemble) #######
setMethod("..can_detach_models", signature(ii="numeric", object="familiarEnsemble"),
          function(ii, object, dir_path=NULL){
            # Check whether a model can be detached without losing it. We do
            # this by checking whether a valid file path can be generated. If
            # not, ..get_model_file_path will return NULL.
            model_file_path <- ..get_model_file_path(ii=ii, object=object, dir_path=dir_path)
            
            return(!is.null(model_file_path))
          })



#####..can_detach_models (missing, familiarEnsemble) #######
setMethod("..can_detach_models", signature(ii="missing", object="familiarEnsemble"),
          function(ii, object, dir_path=NULL){
            # Check whether a model can be detached without losing it.
            can_detach <- sapply(seq_along(object@model_list), ..can_detach_models, object=object, dir_path=dir_path)
            
            # Return TRUE when all models can be detached.
            return(all(can_detach))
          })



#####load_models#####
setMethod("load_models", signature(object="familiarEnsemble"),
          function(object, dir_path=NULL){

            # Update model list as a precaution. This also checks that models
            # can actually be attached.
            object <- ..update_model_list(object=object,
                                          dir_path=dir_path)
            
            # Do not attach models if auto_detach is set to TRUE.
            if(!object@auto_detach){
              object@model_list <- ..get_model(object=object)
            }
            
            return(object)
          })



#####is_model_loaded#####
setMethod("is_model_loaded", signature(object="familiarEnsemble"),
          function(object){
            return(all(sapply(object@model_list, is, class2="familiarModel")))
          })


#####detach_models#####
setMethod("detach_models", signature(object="familiarEnsemble"),
          function(object){
            # Unload the models in the familiarEnsemble
            model_list <- object@model_list
            
            # Iterate over the entries in the model list.
            for(ii in seq_along(object@model_list)){
              
              # Check if the entry contains a familiarModel object that can be detached.
              if(is(object@model_list[[ii]], "familiarModel")){
                
                # Check if the model can be detached.
                if(..can_detach_models(ii=ii, object=object)){
                  
                  # Get the model file name.
                  model_file_name <- ..get_model_file_path(ii=ii, object=object)
                  
                  # Update the entry
                  model_list[[ii]] <- model_file_name
                }
              }
            }
            
            # Attach the model list to the object and return the object.
            object@model_list <- model_list
            
            return(object)
          })



#####add_model_name (ensemble)#####
setMethod("add_model_name", signature(data="ANY", object="familiarEnsemble"),
          function(data, object){
            
            # This is the same for objects of the familiarModel and familiarEnsemble classes
            return(.add_model_name(data=data, object=object))
          })

######get_object_name (ensemble)#####
setMethod("get_object_name", signature(object="familiarEnsemble"),
          function(object, abbreviated=FALSE){
            
            # Extract data and run id
            ensemble_data_id <- object@run_table$ensemble_data_id
            ensemble_run_id  <- object@run_table$ensemble_run_id
            
            if(abbreviated){
              # Create an abbreviated name
              model_name <- paste0("ensemble", ".", ensemble_data_id, ".", ensemble_run_id)
            } else {
              # Create the full name of the model
              model_name <- get_object_file_name(learner=object@learner, fs_method=object@fs_method, project_id=object@project_id, data_id=ensemble_data_id,
                                                 run_id=ensemble_run_id, object_type="familiarEnsemble", is_ensemble=TRUE, with_extension=FALSE)
            }
            
            return(model_name)
          })

######model_is_trained (ensemble)#####
setMethod("model_is_trained", signature(object="familiarEnsemble"),
          function(object){
            # Check if a model was trained
            if(is.null(object@model_list)){
              # Check if a model is present
              return(FALSE)
              
            } else if(length(object@model_list) == 0){
              # No models were attached
              return(FALSE)
              
            } else {
              # Iterate over the models and check if any was trained
              return(any(sapply(object@model_list, model_is_trained)))
              
            }
          })


#####add_package_version (ensemble)#####
setMethod("add_package_version", signature(object="familiarEnsemble"),
          function(object){
            
            # Set version of familiar
            return(.add_package_version(object=object))
          })

######save (ensemble)######
setMethod("save", signature(list="familiarEnsemble", file="character"),
          function(list, file) {
            .save(object=list, dir_path=file)
          })

.get_available_ensemble_prediction_methods <- function(){
  return(c("median", "mean"))
}

ensemble_prediction <- function(object, prediction_data, ensemble_method="mean"){
  
  # Determine prediction column names
  if(object@outcome_type %in% c("binomial", "multinomial")){
    
    class_levels <- get_outcome_class_levels(x=object)
    
    # Probability columns
    prediction_columns <- get_class_probability_name(x=class_levels)
    
  } else if(object@outcome_type %in% c("continuous", "count", "survival")){
    prediction_columns <- "predicted_outcome"
    
  } else if(object@outcome_type == "competing_risk"){
    ..error_outcome_type_not_implemented(outcome_type=object@outcome_type)
    
  } else {
    ..error_no_known_outcome_type(outcome_type=object@outcome_type)
  }
  
  # Also determine if novelty is part of the columns.
  if("novelty" %in% colnames(prediction_data)){
    prediction_columns <- c(prediction_columns, "novelty")
  }
  
  # Set the ensemble aggregation function.
  if(ensemble_method == "mean"){
    FUN <- mean
    
  } else if(ensemble_method == "median"){
    FUN <- stats::median
    
  } else {
    stop("Ensemble method ", ensemble_method, " has not been implemented.")
  }
  
  # Compute the ensemble predictions:
  prediction_data <- prediction_data[, .do_ensemble_prediction(data=.SD, prediction_columns=prediction_columns, FUN=FUN),
                                     by=eval(get_non_feature_columns(x=object))]
  
  # Add the predicted class, if required
  if(object@outcome_type %in% c("binomial", "multinomial")){

    # Identify the name of the most probable class
    new_predicted_class <- class_levels[max.col(prediction_data[, c(prediction_columns), with=FALSE])]
    
    # Add the names as the predicted outcome
    prediction_data[, "predicted_class":=new_predicted_class]
    
    # Convert to a factor
    prediction_data$predicted_class <- factor(prediction_data$predicted_class,
                                              levels=class_levels)
  }
  
  return(prediction_data)
}



.do_ensemble_prediction <- function(data, prediction_columns, FUN){

  # Calculate ensemble value for the prediction columns
  ensemble_pred <- lapply(prediction_columns, function(ii_col, data) (FUN(data[[ii_col]], na.rm=TRUE)), data=data)
  names(ensemble_pred) <- prediction_columns

  return(ensemble_pred)
}



.collect_and_aggregate_calibration_info <- function(model_list, outcome_type){
  # Suppress NOTES due to non-standard evaluation in data.table
  min_value <- max_value <- NULL
  
  # Collect all survival tables
  calibration_data <- lapply(model_list, function(fam_model) (fam_model@calibration_info))
  
  # Remove empty entries
  calibration_data[sapply(calibration_data, is.null)] <- NULL
  
  if(length(calibration_data) == 0) return(NULL)
  
  if(outcome_type == "survival"){
    # Find the unique times at which survival was estimated
    eval_times <- sort(unique(unlist(sapply(calibration_data, function(survival_table) (survival_table$time)))))
    
    # Interpolate calibration data at the evaluating points
    calibration_data <- lapply(calibration_data, function(survival_table, eval_times){
      
      # Find columns to iterate over
      surv_columns <- colnames(survival_table)
      surv_columns <- surv_columns[!surv_columns=="time"]
      
      # Define initial table
      interp_table <- data.table::data.table("time"=eval_times)
      for(current_col in surv_columns){
        # Interpolated the columns at all the time points
        interp_table[, (current_col):=stats::approx(x=survival_table$time, y=survival_table[[current_col]],
                                                    xout=eval_times, rule=2, method="linear")$y]
        
        # Handle infinites appearing in e.g. the variance column
        interp_table[is.infinite(get(current_col)), (current_col):=as.double(NA)]
      }
      
      return(interp_table)
    }, eval_times=eval_times)
    
    # Concatenate the list of tables into one
    calibration_data <- data.table::rbindlist(calibration_data)
    
    # Find columns to iterate over
    surv_columns <- colnames(calibration_data)
    surv_columns <- surv_columns[!surv_columns=="time"]
    
    # Compute the mean survival information for each time point
    calibration_data <- calibration_data[, lapply(.SD, mean, na.rm=TRUE), by="time", .SDcols=surv_columns]
    
  } else if(outcome_type %in% c("continuous", "count")){
    
    # Concatenate to single matrix.
    calibration_data <- data.table::rbindlist(calibration_data)
    
    # Extract min and max values.
    calibration_data <- calibration_data[,  list("min_value"=min(min_value),
                                                 "max_value"=max(max_value))]
  }
  
  # Return calibration data
  return(calibration_data)
}

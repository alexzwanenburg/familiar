#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

#####complete_familiar_ensemble#####
setMethod("complete_familiar_ensemble", signature(object="familiarEnsemble"),
          function(object, dir_path=NULL) {
            # Fills out missing data from a familiarEnsemble based on attached
            # models and internal logic.

            # Load models
            object <- ..update_model_list(object=object, dir_path=dir_path)
            model_list <- ..get_model(object=object)
            
            # Determine which models were trained.
            mask <- sapply(model_list, model_is_trained)
            
            # If no models were trained, select all models, otherwise select
            # only the models that were trained.
            if(any(mask)) model_list <- model_list[mask]
            
            # Add outcome_type and outcome_info to object. These slots are
            # required in some of the other aggregators.
            object@outcome_type <- model_list[[1]]@outcome_type
            object@outcome_info <- .aggregate_outcome_info(x=lapply(model_list, function(list_elem) (list_elem@outcome_info)))
              
            # Find all required features
            required_features <- unique(unlist(lapply(model_list, function(fam_model) (fam_model@required_features))))
            
            # Find all important features for the model.
            model_features <- unique(unlist(lapply(model_list, function(fam_model) (fam_model@model_features))))
            
            # Find all important features for novelty detection.
            novelty_features <- unique(unlist(lapply(model_list, function(fam_model) (fam_model@novelty_features))))
            
            # Aggregate feature information
            if(length(required_features) > 0){
              feature_info_list <- lapply(required_features,
                                          .collect_and_aggregate_feature_info,
                                          object=object,
                                          model_list=model_list,
                                          stop_at="imputation")
              
              # Add name to features.
              names(feature_info_list) <- required_features
              
            } else {
              feature_info_list <- NULL
            }
            
            # Aggregate calibration info
            calibration_info <- .collect_and_aggregate_calibration_info(model_list=model_list,
                                                                        outcome_type=object@outcome_type)
            
            # Generate a new version of the ensemble to avoid unnecessary
            # copying.
            fam_ensemble <- methods::new("familiarEnsemble",
                                         model_list = object@model_list,
                                         outcome_type = object@outcome_type,
                                         outcome_info = object@outcome_info,
                                         data_column_info = model_list[[1]]@data_column_info,
                                         learner = object@learner,
                                         fs_method = object@fs_method,
                                         required_features = required_features,
                                         model_features = model_features,
                                         novelty_features = novelty_features,
                                         feature_info = feature_info_list,
                                         run_table = object@run_table,
                                         calibration_info = calibration_info,
                                         model_dir_path = object@model_dir_path,
                                         auto_detach = object@auto_detach,
                                         settings = model_list[[1]]@settings,
                                         is_anonymised = all(extract_from_slot(object_list=model_list, slot_name="is_anonymised")),
                                         project_id = model_list[[1]]@project_id)
            
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



.get_available_stratification_ensemble_methods <- function(){
  return(c("ensemble_mode", "ensemble_mean", "mean_threshold", "median_threshold"))
}



#####get_prediction_type (familiarEnsemble)#####
setMethod("get_prediction_type", signature(object="familiarEnsemble"),
          function(object, ...){
            # This dispatches to get_prediction_type for the familiarModel
            # subclass if a model is attached, and to character if not. The
            # latter option then loads the familiarModel.
            return(do.call(get_prediction_type, args=c(list("object"=object@model_list[[1]]),
                                                       list(...))))
          })

#####get_prediction_type (character)#####
setMethod("get_prediction_type", signature(object="character"),
          function(object, ...){
            object <- load_familiar_object(object)
            
            return(do.call(get_prediction_type, args=c(list("object"=object),
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
              
              # Obtain the file name of the model.
              model_file_name <- ..get_model_file_path(ii=ii, object=object)
              
              # Load the model if the file exists.
              if(!is.null(model_file_name)) return(load_familiar_object(model_file_name))
              
              # If no model could be found, throw an error.
              stop(paste0("..get_model,familiarEnsemble: cannot find the indicated familiarModel", object@model_list[[ii]]))
              
            } else {
              return(load_familiar_object(object@model_list[[ii]]))
            }
          })



#####..get_model (missing, familiarEnsemble) ######
setMethod("..get_model", signature(ii="missing", object="familiarEnsemble"),
          function(ii, object, ...){
            
            if(length(object@model_list) > 0){
              # Dispatch get_model.
              return(lapply(seq_along(object@model_list), ..get_model, object=object, ...))
              
            } else {
              return(NULL)
            }
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
            if(length(object@model_list) > 0){
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
            }
            
            # Check the model_dir_path slot if auto_detach is on. In that case
            # model_dir_path is required to find the models.
            if((auto_detach | object@auto_detach) & length(object@model_list) > 0){
              
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
            
            # Check if there are any models in the ensemble.
            if(length(object@model_list) > 0){
              
              # Check whether a model can be detached without losing it.
              can_detach <- sapply(seq_along(object@model_list), ..can_detach_models, object=object, dir_path=dir_path)
              
              # Return TRUE when all models can be detached.
              return(all(can_detach))
              
            } else {
              return(TRUE)
            }
          })



#####load_models#####
setMethod("load_models", signature(object="familiarEnsemble"),
          function(object, dir_path=NULL, suppress_auto_detach=FALSE, drop_untrained=FALSE){

            # Skip if there no models on the list.
            if(length(object@model_list) == 0) return(object)
            
            # Update model list as a precaution. This also checks that models
            # can actually be attached.
            object <- ..update_model_list(object=object,
                                          dir_path=dir_path)
            
            # Do not attach models if auto_detach is set to TRUE.
            if(!object@auto_detach | suppress_auto_detach){
              object@model_list <- ..get_model(object=object)
            }
            
            # Drop models that were not trained.
            if(drop_untrained){
              # Determine which models have been trained.
              trained_model_mask <- sapply(object@model_list, model_is_trained)
              
              # Drop untrained models.
              object@model_list <- object@model_list[trained_model_mask]
            }
            
            return(object)
          })



#####is_model_loaded#####
setMethod("is_model_loaded", signature(object="familiarEnsemble"),
          function(object){
            if(length(object@model_list) > 0 & !object@auto_detach){
              # Check that all models are present.
              return(all(sapply(object@model_list, is, class2="familiarModel")))
              
            } else if(length(object@model_list) > 0 & object@auto_detach) {
              # Return TRUE if all models are dynamically loaded.
              return(TRUE)
              
            } else {
              # If there are no models, return TRUE.
              return(TRUE)
            }
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


#####add_model_name (familiarDataElement, familiarEnsemble)------------------------
setMethod("add_model_name", signature(data="familiarDataElement", object="familiarEnsemble"),
          function(data, object){
            
            # Determine the model name
            model_name <- get_object_name(object=object, abbreviated=TRUE)
            
            if(is.null(data@identifiers)){
              data@identifiers <- list("ensemble_model_name" = model_name)
              
            } else {
              data@identifiers[["ensemble_model_name"]] <- model_name
            }
            
            return(data)
          })


#####set_object_name (familiarEnsemble)#####

#' @title Set the name of a `familiarEnsemble` object.
#'  
#' @description Set the `name` slot using the object name.
#'
#' @param x A `familiarEnsemble` object.
#' 
#' @return A `familiarEnsemble` object with a generated or a provided name.
#' @md
#' @keywords internal
setMethod("set_object_name", signature(x="familiarEnsemble"),
          function(x, new=NULL){
            
            if(x@project_id == 0 & is.null(new)){
              # Generate a random object name. A project_id of 0 means that the
              # objects was auto-generated (i.e. through object conversion). We
              # randomly generate characters and add a time stamp, so that
              # collision is practically impossible.
              slot(object=x, name="name") <- paste0(as.character(as.numeric(format(Sys.time(),"%H%M%S"))),
                                                    "_", stringi::stri_rand_strings(1, 20, '[A-Z]'))
              
            } else if(is.null(new)) {
              # Generate a sensible object name.
              slot(object=x, name="name") <- get_object_name(object=x)
              
            } else {
              slot(object=x, name="name") <- new
            }
            
            return(x)
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

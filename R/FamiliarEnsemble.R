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
            important_features <- unique(unlist(lapply(object@model_list, function(fam_model) (fam_model@important_features))))
            
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
                                         important_features = important_features,
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



#####extract_calibration_info#####
setMethod("extract_calibration_info", signature(object="familiarEnsemble"),
          function(object){

            # Suppress NOTES due to non-standard evaluation in data.table
            min_value <- max_value <- NULL
            
            # Collect all survival tables
            calibration_data <- lapply(object@model_list, function(fam_model) (fam_model@calibration_info))

            # Remove empty entries
            calibration_data[sapply(calibration_data, is.null)] <- NULL
            
            if(length(calibration_data) == 0){
              return(NULL)
            }
            
            if(object@outcome_type == "survival"){
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
              
            } else if(object@outcome_type %in% c("continuous", "count")){
              
              # Concatenate to single matrix.
              calibration_data <- data.table::rbindlist(calibration_data)
              
              # Extract min and max values.
              calibration_data <- calibration_data[,  list("min_value"=min(min_value),
                                                           "max_value"=max(max_value))]
            }
            
            # Return calibration data
            return(calibration_data)
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
                                                     by=c("subject_id", "cohort_id", "repetition_id", "outcome_time", "outcome_event")]
                  
                } else if(method == "ensemble_mode"){
                  risk_group_data <- risk_group_data[, list("risk_group"=get_mode(risk_group)),
                                                     by=c("subject_id", "cohort_id", "repetition_id", "outcome_time", "outcome_event")]
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
          function(object){
            return(get_prediction_type(object@model_list[[1]]))
          })



#####load_models#####
setMethod("load_models", signature(object="familiarEnsemble"),
          function(object, dir_path=NULL){

            # Check if the models are already loaded
            if(is_model_loaded(object=object)){
              return(object)
              
            } else {
              # Create a place holder model list
              model_list <- list()

              # Attempt to create a dir_path if it is not provided.
              if(is.null(dir_path)){

                file_paths <- tryCatch({get_file_paths()}, error=function(err)(return(NULL)))
                
                if(is.null(file_paths)){
                  stop("A path to the main directory containing the familiarData objects is required. This is typically the path to the \"trained_models\" folder.")
                  
                } else {
                  # Get the model directory from file_paths
                  dir_path <- file_paths$mb_dir
                }
              }
              
              # Iterate over the different models by name
              for(file_name in object@model_list){

                # Generate file path
                file_path_1 <- normalizePath(file.path(get_object_dir_path(dir_path=dir_path, object_type="familiarModel",
                                                                           learner=object@learner, fs_method=object@fs_method),
                                                       file_name), mustWork=FALSE)
                
                # Generate a second file path in case the first one does not work
                file_path_2 <- normalizePath(file.path(get_object_dir_path(dir_path=dir_path, object_type="familiarModel",
                                                                           file_name)), mustWork=FALSE)
                
                # Check if the file can be read. If it is the case, the model is loaded, and the path it was on is stored.
                if(file.exists(file_path_1)){
                  loaded_model <- readRDS(file_path_1)
                  selected_path <- file_path_1
                  
                } else if(file.exists(file_path_2)) {
                  loaded_model <- readRDS(file_path_2)
                  selected_path <- file_path_2
                  
                } else {
                  logger.stop(paste0("Model file could not be found at ", file_path_1, " or ", file_path_2,
                                     ". Please check whether the path is correct."))
                }
                
                # Check if the file is a familiarModel
                if(!is(loaded_model, "familiarModel")){
                  logger.stop(paste0("The model at ", selected_path, " is not a familiarModel object."))
                }
                
                # Add to model list
                model_list <- append(model_list, loaded_model)
                
              }

              # Append list of model to object and return object
              object@model_list <- model_list

              return(object)
            }
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
            if(is(object@model_list[[1]], "familiarModel")){
              model_list <- list()
              
              # Iterate over the familiar models
              for(fam_model in object@model_list){
                
                # Generate the file name
                model_name <- get_object_name(object=fam_model, abbreviated=FALSE)
                model_name <- paste0(model_name, ".RDS")
                
                # Append to list
                model_list <- append(model_list, model_name)
              }
              
              # Append the model list to the object and return the object
              object@model_list <- model_list
              
              return(object)
            } else {
              return(object)
            }
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



ensemble_failure_times <- function(prediction_list){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  failure_time <- NULL
  
  # Aggregate failure tables as data.tables in a long data format
  failure_table <- rbind_list_list(prediction_list, list_elem="failure_times")
  
  # Keep only finite values
  failure_table <- failure_table[is.finite(failure_time)]
  
  # Return NULL if the table is empty
  if(is_empty(failure_table)){
    return(NULL)
  }
  
  # Compute mean failure time for each sample and quantile
  failure_table <- failure_table[, list("failure_time"=mean(failure_time, na.rm=FALSE)), by=c("subject_id", "quantile")]
  
  return(failure_table)
}


ensemble_survival_matrix <- function(prediction_list){

  
  # Suppress NOTES due to non-standard evaluation in data.table
  survival_probability <- NULL
  
  # Determine the unique event times over the entire data set.
  unique_times <- sort(unique(unlist(sapply(prediction_list, function(list_entry) (list_entry$survival$event_time)))))
  
  # Iterate over the individual samples and estimate survival probabilities at the common set of event times
  survival_table <- data.table::rbindlist(lapply(prediction_list, function(list_entry, unique_times){
    
    # Extract survival table
    survival_table <- list_entry$survival
    
    # Check if the table is present and not empty
    if(is.null(survival_table)){
      return(NULL)
    } else if(nrow(survival_table) == 0){
      return(NULL)
    }
    
    # Interpolate the survival probabilities for each patient
    survival_table <- data.table::rbindlist(lapply(split(survival_table, by="subject_id"), function(sample_table, unique_times){
    
      # Find the current subject_id
      curr_subject_id <- unique(sample_table$subject_id)
      
      # Create a linear fit at the unique times
      data_fit <- stats::approx(x=sample_table$event_time, y=sample_table$survival_probability, xout=unique_times, rule=2)
      
      return(data.table::data.table("subject_id"=curr_subject_id, "survival_probability"=data_fit$y, "event_time"=data_fit$x))
      
    }, unique_times=unique_times))
    
    return(survival_table)
    
  }, unique_times=unique_times))

  # Compute the mean survival probability for each sample at each unique time point
  survival_table <- survival_table[, list("survival_probability"=mean(survival_probability, na.rm=TRUE)), by=c("subject_id", "event_time")]
  
  return(survival_table)
}

#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

.get_available_prediction_type_arguments <- function(){
  return(c("novelty", "default", "survival_probability", "risk_stratification"))
}


#####.predict (familiarEnsemble)#####
setMethod(".predict", signature(object="familiarEnsemble"),
          function(object, data, allow_recalibration=TRUE, is_pre_processed=FALSE, time=NULL, type="default", dir_path=NULL, ensemble_method="median", ...) {
            # Predict function for a model ensemble. This will always return
            # ensemble information, and not details corresponding to the
            # individual models.
            
            # Test if the models are loaded, and load the models if they are
            # not.
            object <- load_models(object, dir_path=dir_path)
            
            # Extract predictions for each individual model
            if(length(object@model_list) > 0){
              predict_list <- lapply(object@model_list,
                                     .predict,
                                     data=data,
                                     allow_recalibration=allow_recalibration,
                                     is_pre_processed=is_pre_processed,
                                     time=time,
                                     type=type,
                                     ...)
              
            } else {
              # Process data
              data <- process_input_data(object=object,
                                         data=data,
                                         is_pre_processed=is_pre_processed,
                                         stop_at="clustering",
                                         keep_novelty="novelty" %in% type)
              
              # Generate a placeholder table.
              predict_list <- list(get_placeholder_prediction_table(object=object, data=data))
            }
            
            
            ##### Ensemble predictions #####
            # Generate ensemble predictions
            if(all(type %in% .get_available_prediction_type_arguments())){
              prediction_data <- ensemble_prediction(object=object,
                                                     prediction_data=data.table::rbindlist(predict_list),
                                                     ensemble_method=ensemble_method)
            }

            # Return data
            return(prediction_data)
          })


#####.predict (familiarModel)#####
setMethod(".predict", signature(object="familiarModel"),
          function(object, data, allow_recalibration=TRUE, is_pre_processed=FALSE, time=NULL, type="default", ...) {
            
            # Prepare input data
            data <- process_input_data(object=object,
                                       data=data,
                                       is_pre_processed=is_pre_processed,
                                       stop_at="clustering",
                                       keep_novelty="novelty" %in% type)
            
            # Set empty prediction table that can be updated
            prediction_table <- NULL
            
            
            ##### User specified type ##########################################
            if(any(!type %in% .get_available_prediction_type_arguments())){
              # Select the first non-standard type.
              type <- setdiff(type, .get_available_prediction_type_arguments())[1]
              
              # Select only features used by the model.
              data <- select_features(data=data,
                                      features=features_after_clustering(features=object@model_features,
                                                                         feature_info_list=object@feature_info))
              
              # Call predict from ..predict method with the given type and the
              # unspecified extra arguments in ... .
              prediction_element <- ..predict(object=object,
                                              data=data,
                                              time=time,
                                              type=type,
                                              ...)
              return(prediction_element)
            }
            
            ##### Novelty ######################################################
            if("novelty" %in% type){
              
              # Predict instance novelty.
              prediction_table <- .predict_novelty(object=object,
                                                   data=data)
              
              # Keep only model features in data for the remaining analysis.
              data <- select_features(data=data,
                                      features=features_after_clustering(features=object@model_features,
                                                                         feature_info_list=object@feature_info))
            }
            
            ##### Default response #############################################
            if("default" %in% type){
              
              # Predict using the model and the standard type.
              temp_prediction_table <- ..predict(object=object,
                                                 data=data,
                                                 time=time)
              
              # Recalibrate the results.
              if(allow_recalibration){
                temp_prediction_table <- learner.apply_calibration(object=object,
                                                                   predictions=temp_prediction_table)
              }
              
              # Add new columns to existing prediction table, if necessary.
              if(is.null(prediction_table)){
                prediction_table <- temp_prediction_table
                
              } else if(!is_empty(temp_prediction_table)){
                prediction_table <- merge(x=prediction_table,
                                          y=temp_prediction_table,
                                          by=get_non_feature_columns(x=object))
              }
            }
            
            ##### Survival probability #########################################
            if("survival_probability" %in% type) {
              # Prediction survival probabilities,
              temp_prediction_table <- ..predict_survival_probability(object=object,
                                                                      data=data,
                                                                      time=time)
              
              # Add new columns to existing prediction table, if necessary.
              if(is.null(prediction_table)){
                prediction_table <- temp_prediction_table
                
              } else if(!is_empty(temp_prediction_table)){
                # Merge with the prediction table
                prediction_table <- merge(x=prediction_table,
                                          y=temp_prediction_table,
                                          by=get_non_feature_columns(x=object))
              }
            }
            
            ##### Risk stratification ##########################################
            if("risk_stratification" %in% type){
              # Prediction survival probabilities,
              temp_prediction_table <- ..predict_risk_stratification(object=object,
                                                                     data=data,
                                                                     time=time)
              
              # Add new columns to existing prediction table, if necessary.
              if(is.null(prediction_table)){
                prediction_table <- temp_prediction_table
                
              } else if(!is_empty(temp_prediction_table)){
                prediction_table <- merge(x=prediction_table,
                                          y=temp_prediction_table,
                                          by=get_non_feature_columns(x=object))
              }
            }
            
            if(!is_empty(prediction_table)){
              # Order output columns.
              data.table::setcolorder(prediction_table,
                                      neworder=colnames(get_placeholder_prediction_table(object=object, data=data)))
            }  

            return(prediction_table)  
          })


#####.predict (character)#####
setMethod(".predict", signature(object="character"),
          function(object, data, ...){
            
            # Load object.
            object <- load_familiar_object(object)
            
            return(do.call(.predict, args=c(list("object"=object,
                                                 "data"=data),
                                            list(...))))
          })



#####.predict_novelty (familiarModel)#####
setMethod(".predict_novelty", signature(object="familiarModel"),
          function(object, data, is_pre_processed=FALSE){
            
            # Prepare input data
            data <- process_input_data(object=object,
                                       data=data,
                                       is_pre_processed=is_pre_processed,
                                       stop_at="clustering",
                                       keep_novelty=TRUE)
            
            # Get a placeholder prediction table.
            prediction_table <- get_placeholder_prediction_table(object=object,
                                                                 data=data,
                                                                 type="novelty")
            
            # Return NA if there is no novelty detector.
            if(is.null(object@novelty_detector)) return(prediction_table)
            
            # Return empty if there is no data.
            if(is_empty(data)) return(prediction_table)
            
            # Find and replace ordered features.
            ordered_features <- colnames(data@data)[sapply(data@data, is.ordered)]
            for(current_feature in ordered_features){
              data@data[[current_feature]] <- factor(x=data@data[[current_feature]],
                                                     levels=levels(data@data[[current_feature]]),
                                                     ordered=FALSE)
            }
            
            # Find novelty values.
            novelty_values <- predict(object=object@novelty_detector,
                                      newdata=data@data)
            
            # Store the novelty values in the table.
            prediction_table[, "novelty":=novelty_values]
            
            return(prediction_table)
          })


#####.predict_novelty (character)#####
setMethod(".predict_novelty", signature(object="character"),
          function(object, ...){
            
            # Load object.
            object <- load_familiar_object(object)
            
            return(do.call(.predict_novelty, args=c(list("object"=object),
                                                    list(...))))
          })


#####.predict_risk_stratification (familiarModel)#####
setMethod(".predict_risk_stratification", signature(object="familiarModel"),
          function(object, data, time, stratification_threshold=NULL, stratification_method=NULL, ...){
            # Only assess stratification for survival outcomes.
            if(!object@outcome_type %in% c("survival")) return(NULL)
            
            # Allow for settings the stratification threshold explicitly.
            if(is.null(stratification_threshold)){
              
              if(!is.null(stratification_method)){
                # Select threshold values for the given method.
                
                # First check if the method exists.
                if(!stratification_method %in% object@km_info$stratification_method){
                  stop(paste0("Data for stratification method ", stratification_method, " was not found with the object."))
                }
                
                # Select stratification threshold for the given method.
                stratification_threshold <- object@km_info$parameters[[stratification_method]]$cutoff
                
              } else {
                # Select stratification threshold for the first method.
                stratification_threshold <- object@km_info$parameters[[1]]$cutoff
              }
            }
            
            # Generate an empty prediction table.
            prediction_table <- get_placeholder_prediction_table(object=object,
                                                                 data=data,
                                                                 type="risk_stratification")
            
            # Check again if a stratification threshold is now set.
            if(is.null(stratification_threshold)) return(prediction_table)
            
            # Predict for the instances in data.
            predictions <- .predict(object=object,
                                    data=data,
                                    time=time)
            
            # Check for valid predictions,
            if(is_empty(predictions)) return(prediction_table)
            if(!any_predictions_valid(prediction_table=predictions, outcome_type=object@outcome_type)) return(prediction_table)
            
            # Find risk groups for each instance.
            assigned_risk_group <- learner.apply_risk_threshold(object=object,
                                                                predicted_values=predictions$predicted_outcome,
                                                                cutoff=stratification_threshold)
            
            # Set to prediction table.
            prediction_table[, "risk_group":=assigned_risk_group]
            
            return(prediction_table)
          })


#####.predict_risk_stratification (character)#####
setMethod(".predict_risk_stratification", signature(object="character"),
          function(object, ...){
            
            # Load object.
            object <- load_familiar_object(object)
            
            return(do.call(.predict_risk_stratification, args=c(list("object"=object),
                                                                list(...))))
          })


ensemble_prediction <- function(object,
                                prediction_data,
                                ensemble_method="median",
                                type="default"){
  
  # Check that type has correct values.
  sapply(type, .check_parameter_value_is_valid, var_name="type", values=.get_available_prediction_type_arguments())
  
  # Check that prediction data are not empty.
  if(is_empty(prediction_data)) return(prediction_data)
  
  ensemble_prediction_data <- NULL
  
  # Default predictions.
  if(any(type %in% c("default", "novelty", "survival_probability"))){
    ensemble_prediction_data <- .ensemble_prediction(object=object,
                                                     prediction_data=prediction_data,
                                                     ensemble_method=ensemble_method,
                                                     type=type)
  }
  
  # Risk group prediction.
  if(any(type %in% c("risk_stratification"))){
    temp_ensemble_prediction_data <- .ensemble_risk_prediction(object=object,
                                                               prediction_data=prediction_data,
                                                               ensemble_method=ensemble_method)
    
    if(is.null(ensemble_prediction_data)){
      ensemble_prediction_data <- temp_ensemble_prediction_data
      
    } else {
      ensemble_prediction_data[temp_ensemble_prediction_data, on=.NATURAL]
    }
  }
  
  return(ensemble_prediction_data)
}



.ensemble_prediction <- function(object,
                                 prediction_data,
                                 ensemble_method,
                                 type){
  
  # Specify prediction columns
  prediction_columns <- character(0L)
  
  # Determine column names for default predictions.
  if("default" %in% type){
    
    if(object@outcome_type %in% c("binomial", "multinomial")){
      
      # Find the class levels.
      class_levels <- get_outcome_class_levels(x=object)
      
      # Probability columns
      prediction_columns <- c(prediction_columns,
                              get_class_probability_name(x=class_levels))
      
    } else if(object@outcome_type %in% c("continuous", "count", "survival")){
      # Outcome columns
      prediction_columns <- c(prediction_columns,
                              "predicted_outcome")
      
    } else if(object@outcome_type == "competing_risk"){
      ..error_outcome_type_not_implemented(outcome_type=object@outcome_type)
      
    } else {
      ..error_no_known_outcome_type(outcome_type=object@outcome_type)
    }
  }
  
  # Determine if novelty is part of the columns.
  if("novelty" %in% type){
    prediction_columns <- c(prediction_columns,
                            "novelty")
  }
  
  # Determine if survival probabilities were computed.
  if("survival_probability" %in% type){
    prediction_columns <- c(prediction_columns,
                            "survival_probability")
  }
  
  # Check whether prediction_data is empty.
  if(is_empty(prediction_data)) return(prediction_data)
  
  # Set the ensemble aggregation function.
  if(ensemble_method == "mean"){
    FUN <- mean
    
  } else if(ensemble_method == "median"){
    FUN <- stats::median
    
  } else {
    stop("Ensemble method ", ensemble_method, " has not been implemented.")
  }
  
  # Compute the ensemble predictions:
  prediction_data <- prediction_data[,
                                     .do_ensemble_prediction(data=.SD, prediction_columns=prediction_columns, FUN=FUN),
                                     by=eval(get_non_feature_columns(x=object))]
  
  # Add the predicted class, if required
  if(object@outcome_type %in% c("binomial", "multinomial") & "default" %in% type){
    
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


.ensemble_risk_prediction <- function(object, prediction_data, ensemble_method){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  risk_group <- NULL
  
  # Create risk groups according to the corresponding method.
  if(ensemble_method == "mean"){
    prediction_data <- prediction_data[, list("risk_group"=learner.get_mean_risk_group(risk_group)),
                                       by=c(get_id_columns(), "outcome_time", "outcome_event")]
    
  } else if(ensemble_method == "median"){
    prediction_data <- prediction_data[, list("risk_group"=get_mode(risk_group)),
                                       by=c(get_id_columns(), "outcome_time", "outcome_event")]
  }
  
  return(prediction_data)
}


.get_available_ensemble_prediction_methods <- function(){
  return(c("median", "mean"))
}

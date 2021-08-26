#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

.get_available_prediction_type_arguments <- function(){
  return(c("novelty", "default", "survival_probability", "risk_stratification"))
}


#####.predict (familiarEnsemble)#####
setMethod(".predict", signature(object="familiarEnsemble"),
          function(object,
                   data,
                   allow_recalibration=TRUE,
                   is_pre_processed=FALSE,
                   time=NULL, type="default",
                   dir_path=NULL,
                   ensemble_method="median",
                   stratification_threshold=NULL,
                   stratification_method=NULL,
                   percentiles=NULL,
                   ...,
                   aggregate_results=TRUE) {
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
                                     stratification_threshold=stratification_threshold,
                                     stratification_method=stratification_method,
                                     ...)
              
            } else {

              # Generate a placeholder table.
              predict_list <- list(get_placeholder_prediction_table(object=object, data=data, type=type))
            }
            
            ##### Ensemble predictions #####
            # Generate ensemble predictions
            if(all(type %in% .get_available_prediction_type_arguments())){
              if(aggregate_results){
                
                # Determine class levels.
                if(object@outcome_type %in% c("binomial", "multinomial")){
                  class_levels <- get_outcome_class_levels(x=object)
                  
                } else {
                  class_levels <- NULL
                }
                
                # Ensemble predictions are done using the
                # .compute_data_element_estimates method.
                data_element <- methods::new("familiarDataElementPredictionTable",
                                             data = data.table::rbindlist(predict_list),
                                             detail_level = "ensemble",
                                             percentiles = percentiles,
                                             type = type,
                                             ensemble_method = ensemble_method,
                                             estimation_type = ifelse(is.null(percentiles), "point", "bootstrap_confidence_interval"),
                                             outcome_type = object@outcome_type,
                                             class_levels = class_levels,
                                             grouping_column = get_non_feature_columns(object))
                
                # Compute ensemble values.
                data_element <- .compute_data_element_estimates(x=data_element,
                                                                object=object)
                
                # Check that the data element is not empty.
                if(is_empty(data_element)) return(get_placeholder_prediction_table(object=object, data=data, type=type))
                
                # Extract data from data element.
                return(data_element[[1]]@data)
                
              } else {
                
                return(data.table::rbindlist(predict_list, use.names=TRUE, fill=TRUE))
              }
              
            } else {
              
              # Return list with custom types.
              return(predict_list)
            }
          })


#####.predict (familiarModel)#####
setMethod(".predict", signature(object="familiarModel"),
          function(object,
                   data,
                   allow_recalibration=TRUE,
                   is_pre_processed=FALSE,
                   time=NULL,
                   type="default",
                   stratification_threshold=NULL,
                   stratification_method=NULL,
                   ...) {
            
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
              temp_prediction_table <- .predict_risk_stratification(object=object,
                                                                    data=data,
                                                                    time=time,
                                                                    stratification_threshold=stratification_threshold,
                                                                    stratification_method=stratification_method,)
              
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
                                      neworder=colnames(get_placeholder_prediction_table(object=object, data=data, type=type)))
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



.get_available_ensemble_prediction_methods <- function(){
  return(c("median", "mean"))
}



any_predictions_valid <- function(prediction_table, outcome_type){
  
  if(is_empty(prediction_table)) return(FALSE)
  
  if(outcome_type %in% c("continuous", "count")){
    return(any(is.finite(prediction_table$predicted_outcome)))
    
  } else if(outcome_type %in% c("survival", "competing_risk")){
    if("predicted_outcome" %in% colnames(prediction_table)){
      return(any(is.finite(prediction_table$predicted_outcome)))
      
    } else if("survival_probability" %in% colnames(prediction_table)){
      return(any(is.finite(prediction_table$survival_probability)))
      
    } else if("risk_group" %in% colnames(prediction_table)){
      return(any(!is.na(prediction_table$risk_group)))
      
    } else {
      return(FALSE)
    }
    
  } else if(outcome_type %in% c("binomial", "multinomial")){
    return(!all(is.na(prediction_table$predicted_class)))
    
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }
}


remove_nonvalid_predictions <- function(prediction_table, outcome_type){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  predicted_outcome <- predicted_class <- survival_probability <- risk_group <- NULL
  
  if(is_empty(prediction_table)) return(prediction_table)
  
  # Check predicted outcome columns.
  if(outcome_type %in% c("survival", "competing_risk")){
    if("predicted_outcome" %in% colnames(prediction_table)){
      prediction_table <- prediction_table[is.finite(predicted_outcome), ]
    }
    
    if("survival_probability" %in% colnames(prediction_table)){
      prediction_table <- prediction_table[is.finite(survival_probability), ]
    }
    
    if("risk_group" %in% colnames(prediction_table)){
      prediction_table <- prediction_table[!is.na(risk_group), ]
    }
    
  } else if(outcome_type %in% c("continuous", "count")){
    prediction_table <- prediction_table[is.finite(predicted_outcome), ]
    
  } else if(outcome_type %in% c("binomial", "multinomial")){
    prediction_table <- prediction_table[!is.na(predicted_class), ]
    
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }
  
  return(prediction_table)
}



remove_missing_outcomes <- function(prediction_table, outcome_type){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- outcome_time <- outcome_event <- NULL
  
  if(is_empty(prediction_table)) return(prediction_table)
  
  # Check predicted outcome columns.
  if(outcome_type %in% c("survival", "competing_risk")){
    prediction_table <- prediction_table[is.finite(outcome_time) & !is.na(outcome_event), ]
    
  } else if(outcome_type %in% c("count", "continuous")){
    prediction_table <- prediction_table[is.finite(outcome), ]
    
  } else if(outcome_type %in% c("binomial", "multinomial")){
    prediction_table <- prediction_table[!is.na(outcome), ]
    
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }
  
  return(prediction_table)
}

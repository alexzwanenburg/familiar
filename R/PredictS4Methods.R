#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

.get_available_prediction_type_arguments <- function(){
  return(c("novelty", "default", "survival_probability", "risk_stratification"))
}



#'@title Model predictions for familiar models and model ensembles
#'
#'@description Fits the model or ensemble of models to the data and shows the
#'  result.
#'
#'@param object A familiar model or ensemble of models that should be used for
#'  prediction. This can also be a path to the ensemble model, one or more paths
#'  to models, or a list of models.
#'@param newdata Data to which the models are fitted. `familiar` performs checks
#'  on the data to ensure that all features required for fitting the model are
#'  present, and no additional levels are present in categorical features.
#'  Unlike other `predict` methods, `newdata` cannot be missing in `familiar`,
#'  as training data are not stored with the models.
#'@param type Type of prediction made. The following values are directly
#'  supported:
#'
#'  * `default`: Default prediction, i.e. value estimates for `count` and
#'  `continuous` outcomes, predicted class probabilities and class for
#'  `binomial` and `multinomial` and the model response for `survival` outcomes.
#'
#'  * `survival_probability`: Predicts survival probabilities at the time
#'  specified by `time`. Only applicable to `survival` outcomes. Some models may
#'  not allow for predicting survival probabilities based on their response.
#'
#'  * `novelty`: Predicts novelty of each sample, which can be used for
#'  out-of-distribution detection.
#'
#'  * `risk_stratification`: Predicts the strata to which the data belongs. Only
#'  for `survival` outcomes.
#'
#'  Other values for type are passed to the fitting method of the actual
#'  underlying model. For example for generalised linear models (`glm`) `type`
#'  can be `link`, `response` or `terms` as well. Some of these model-specific
#'  prediction types may fail to return results if the model has been trimmed.
#'
#'@param time Time at which the response (`default`) or survival probability
#'  (`survival_probability`) should be predicted for `survival` outcomes. Some
#'  models have a response that does not depend on `time`, e.g. `cox`, whereas
#'  others do, e.g. `random_forest`.
#'@param dir_path Path to the folder containing the models. Ensemble objects are
#'  stored with the models detached. In case the models were moved since
#'  creation, `dir_path` can be used to specify the current folder.
#'  Alternatively the `update_model_dir_path` method can be used to update the
#'  path.
#'@param stratification_method Selects the stratification method from which the
#'  threshold values should be selected. If the model or ensemble of models does
#'  not contain thresholds for the indicated method, an error is returned. In
#'  addition this argument is ignored if a `stratification_threshold` is set.
#'@param stratification_threshold Threshold value(s) used for stratifying
#'  instances into risk groups. If this parameter is specified,
#'  `stratification_method` and any threshold values that come with the model
#'  are ignored, and `stratification_threshold` is used instead.
#'@param percentiles Currently unused.
#'@param ... to be documented.
#'
#'@inheritParams extract_data
#'
#'@details This method is used to predict values for instances specified by the
#'  `newdata` using the model or ensemble of models specified by the `object`
#'  argument.
#'
#'@return A `data.table` with predicted values.
#'@exportMethod predict
#'@md
#'@rdname predict-methods
setGeneric("predict")

#####predict (familiarModel)#####
#'@rdname predict-methods
setMethod("predict", signature(object="familiarModel"),
          function(object,
                   newdata,
                   type="default",
                   time=NULL,
                   dir_path=NULL,
                   ensemble_method="median",
                   stratification_threshold=NULL,
                   stratification_method=NULL,
                   percentiles=NULL,
                   ...){
            # Create ensemble.
            object <- as_familiar_ensemble(object=object)
            
            # Create predictions.
            predictions <- predict(object=object,
                                   newdata=newdata,
                                   type=type,
                                   time=time,
                                   dir_path=NULL,
                                   ensemble_method=ensemble_method,
                                   stratification_threshold=NULL,
                                   stratification_method=NULL,
                                   percentiles=NULL,
                                   ...)
            
            return(predictions)
          })



#####predict (familiarEnsemble)#####
#'@rdname predict-methods
setMethod("predict", signature(object="familiarEnsemble"),
          function(object,
                   newdata,
                   type="default",
                   time=NULL,
                   dir_path=NULL,
                   ensemble_method="median",
                   stratification_threshold=NULL,
                   stratification_method=NULL,
                   percentiles=NULL,
                   ...){
            
            if(missing(newdata)) stop("newdata must be provided.")
            if(is_empty(newdata)) ..error_data_set_is_empty()
            
            # Parse newdata to data object
            data <- as_data_object(data=newdata,
                                   object=object,
                                   check_stringency="external")
            
            # Propagate to .predict
            predictions <- .predict(object=object,
                                    data=data,
                                    type=type,
                                    time=time,
                                    dir_path=dir_path,
                                    ensemble_method=ensemble_method,
                                    stratification_threshold=stratification_threshold,
                                    stratification_method=stratification_method,
                                    percentiles=percentiles)
            
            if(type %in% .get_available_prediction_type_arguments()){
              # Find non-feature columns.
              non_feature_columns <- get_non_feature_columns(object)
              prediction_columns <- setdiff(colnames(predictions), non_feature_columns)
              
              # Update the table with predictions by removing the non-feature
              # columns.
              predictions <- data.table::copy(predictions[, mget(prediction_columns)])
            }
            
            return(predictions)
          })


#####predict (familiarNoveltyDetector)------------------------------------------
#'@rdname predict-methods
setMethod("predict", signature(object="familiarNoveltyDetector"),
          function(object,
                   newdata,
                   type="novelty",
                   ...){
            
            # Propagate to .predict
            predictions <- .predict(object=object,
                                    data=data,
                                    type=type)
            
            if(type %in% .get_available_prediction_type_arguments()){
              # Find non-feature columns.
              non_feature_columns <- get_non_feature_columns(object)
              prediction_columns <- setdiff(colnames(predictions), non_feature_columns)
              
              # Update the table with predictions by removing the non-feature
              # columns.
              predictions <- data.table::copy(predictions[, mget(prediction_columns)])
            }
            
            return(predictions)
          })


#####predict (list)#####
#'@rdname predict-methods
setMethod("predict", signature(object="list"),
          function(object,
                   newdata,
                   type="default",
                   time=NULL,
                   dir_path=NULL,
                   ensemble_method="median",
                   stratification_threshold=NULL,
                   stratification_method=NULL,
                   percentiles=NULL,
                   ...){
            # Create ensemble.
            object <- as_familiar_ensemble(object=object)
            
            # Create predictions.
            predictions <- predict(object=object,
                                   newdata=newdata,
                                   type=type,
                                   time=time,
                                   dir_path=dir_path,
                                   ensemble_method=ensemble_method,
                                   stratification_threshold=NULL,
                                   stratification_method=NULL,
                                   percentiles=NULL,
                                   ...)
            
            return(predictions)
          })



#####predict (character)#####
#'@rdname predict-methods
setMethod("predict", signature(object="character"),
          function(object,
                   newdata,
                   type="default",
                   time=NULL,
                   dir_path=NULL,
                   ensemble_method="median",
                   stratification_threshold=NULL,
                   stratification_method=NULL,
                   percentiles=NULL,
                   ...){
            # Create ensemble.
            object <- as_familiar_ensemble(object=object)
            
            # Create predictions.
            predictions <- predict(object=object,
                                   newdata=newdata,
                                   type=type,
                                   time=time,
                                   dir_path=dir_path,
                                   ensemble_method=ensemble_method,
                                   stratification_threshold=NULL,
                                   stratification_method=NULL,
                                   percentiles=NULL,
                                   ...)
            
            return(predictions)
          })


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
                                                   data=data,
                                                   type=type)
              
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
                                          y=unique(temp_prediction_table),
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
                                          y=unique(temp_prediction_table),
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
                                          y=unique(temp_prediction_table),
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



#####.predict (familiarNoveltyDetector)-----------------------------------------
setMethod(".predict", signature(object="familiarNoveltyDetector"),
          function(object, data, type="novelty", is_pre_processed=FALSE, ...){
            
            # Prepare input data.
            data <- process_input_data(object=object,
                                       data=data,
                                       is_pre_processed=is_pre_processed,
                                       stop_at="clustering")
            
            # Predict novelty.
            prediction_table <- ..predict(object=object,
                                          data=data,
                                          type=type)
            
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
          function(object, data, type="novelty", is_pre_processed=FALSE, ...){
            
            return(.predict(object=object@novelty_detector,
                            data=data,
                            type=type,
                            is_pre_processed=is_pre_processed))
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



any_predictions_valid <- function(prediction_table, outcome_type=NULL, type="default"){
  
  if(is_empty(prediction_table)) return(FALSE)
  
  if(type %in% c("default", "survival_probability", "risk_stratification")){
    if(is.null(outcome_type)){
      ..error_reached_unreachable_code("any_predictions_valid: outcome_type was not provided.")
    }
    
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
  } else if(type %in% c("novelty")) {
    return(any(is.finite(prediction_table$novelty)))
    
  } else {
    ..error_reached_unreachable_code("any_predictions_valid: unknown type encountered")
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



#####remove_missing_outcomes (data.table)#####
setMethod("remove_missing_outcomes", signature(data="data.table"),
          function(data, outcome_type){
            # Suppress NOTES due to non-standard evaluation in data.table
            outcome <- outcome_time <- outcome_event <- NULL
            
            if(is_empty(data)) return(data)
            
            # Check predicted outcome columns.
            if(outcome_type %in% c("survival", "competing_risk")){
              data <- data[is.finite(outcome_time) & !is.na(outcome_event), ]
              
            } else if(outcome_type %in% c("count", "continuous")){
              data <- data[is.finite(outcome), ]
              
            } else if(outcome_type %in% c("binomial", "multinomial")){
              data <- data[!is.na(outcome), ]
              
            } else {
              ..error_no_known_outcome_type(outcome_type)
            }
            
            return(data)
          })


#####remove_missing_outcomes (dataObject)#####
setMethod("remove_missing_outcomes", signature(data="dataObject"),
          function(data, outcome_type=NULL){
            
            if(is_empty(data)) return(data)
            
            if(is.null(outcome_type)) outcome_type <- data@outcome_type
            
            # Update data attribute
            data@data <- remove_missing_outcomes(data=data@data,
                                                 outcome_type=outcome_type)
            
            return(data)
          })


#####remove_missing_outcomes (ANY)#####
setMethod("remove_missing_outcomes", signature(data="ANY"),
          function(data, outcome_type=NULL){
            
            if(is_empty(data)) return(data)
            
            ..error_reached_unreachable_code("remove_missing_outcomes,ANY: data does not have a known object class, nor is empty.")
          })

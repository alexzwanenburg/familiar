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

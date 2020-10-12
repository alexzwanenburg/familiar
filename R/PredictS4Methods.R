#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#####.predict (model)#####
setMethod(".predict", signature(object="familiarModel"),
          function(object, data, allow_recalibration=TRUE, is_pre_processed=FALSE, time=NULL, type=NULL, novelty=FALSE, ...) {
            
            # Prepare input data
            data <- process_input_data(object=object,
                                       data=data,
                                       is_pre_processed=is_pre_processed,
                                       stop_at="clustering",
                                       keep_novelty=novelty)
            
            # Add novelty.
            if(novelty){
              # Determine novelty
              novelty_values <- .predict_novelty(object=object,
                                                 data=data)
              
              # Keep only model features in data.
              data <- select_features(data=data,
                                      features=features_after_clustering(features=object@model_features,
                                                                         feature_info_list=object@feature_info))
            }
            
            if(is.null(type)){
              
              # Predict using the model and the standard type.
              prediction_table <- ..predict(object=object,
                                            data=data,
                                            time=time)
              
              if(allow_recalibration){
                # Recalibrate the results.
                prediction_table <- learner.apply_calibration(object=object,
                                                              predictions=prediction_table)
              }
              
            } else if(type == "survival_probability") {
              # Prediction survival probabilities,
              prediction_table <- ..predict_survival_probability(object=object,
                                                                 data=data,
                                                                 time=time)
              
            } else {
              # Predict using a user-specified type.
              prediction_table <- ..predict(object=object,
                                            data=data,
                                            time=time,
                                            type=type)
            }
            
            # Order output columns.
            data.table::setcolorder(prediction_table,
                                    neworder=colnames(get_placeholder_prediction_table(object=object, data=data)))
            
            # Add novelty to the prediction table
            if(novelty) prediction_table[, "novelty":=novelty_values]
            
            return(prediction_table)  
          })


#####.predict (ensemble)#####
setMethod(".predict", signature(object="familiarEnsemble"),
          function(object, data, allow_recalibration=TRUE, is_pre_processed=FALSE, time=NULL, type=NULL, dir_path=NULL, ensemble_method="median", novelty=FALSE, ...) {
            # Predict function for a model ensemble. This will always return
            # ensemble information, and not details corresponding to the
            # individual models.
            
            # Test if the models are loaded, and load the models if they are
            # not.
            object <- load_models(object, dir_path=dir_path)
            
            # Extract predictions for each individual model
            predict_list <- lapply(object@model_list,
                                   .predict,
                                   data=data,
                                   allow_recalibration=allow_recalibration,
                                   is_pre_processed=is_pre_processed,
                                   time=time,
                                   type=type,
                                   novelty=novelty)
            
            ##### Ensemble predictions #####
            
            # Generate ensemble predictions
            prediction_data <- ensemble_prediction(object=object,
                                                   prediction_data=data.table::rbindlist(predict_list),
                                                   ensemble_method=ensemble_method)
            
            # Return data
            return(prediction_data)
          })


#####.predict_novelty#####
setMethod(".predict_novelty", signature(object="familiarModel"),
          function(object, data, is_pre_processed=FALSE){
            
            # Prepare input data
            data <- process_input_data(object=object,
                                       data=data,
                                       is_pre_processed=is_pre_processed,
                                       stop_at="clustering",
                                       keep_novelty=TRUE)
            
            # Return NA if there is no novelty detector.
            if(is.null(object@novelty_detector)) return(rep(NA_real_, times=nrow(data@data)))
            
            # Return empty if there is no data.
            if(is_empty(data)) return(numeric(0))
            
            # Find and replace ordered features.
            ordered_features <- colnames(data@data)[sapply(data@data, is.ordered)]
            for(current_feature in ordered_features){
              data@data[[current_feature]] <- factor(x=data@data[[current_feature]],
                                                     levels=levels(data@data[[current_feature]]),
                                                     ordered=FALSE)
            }
            
            return(predict(object=object@novelty_detector,
                           newdata=data@data))
          })

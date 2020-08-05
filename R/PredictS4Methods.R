#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#####.predict (model)#####
setMethod(".predict", signature(object="familiarModel"),
          function(object, data, allow_recalibration=TRUE, is_pre_processed=FALSE, time=NULL, type=NULL, ...) {
            
            # Prepare input data
            data <- process_input_data(object=object,
                                       data=data,
                                       is_pre_processed=is_pre_processed,
                                       stop_at="clustering")
            
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
            
            
            return(prediction_table)  
          })


#####.predict (ensemble)#####
setMethod(".predict", signature(object="familiarEnsemble"),
          function(object, data, allow_recalibration=TRUE, is_pre_processed=FALSE, time=NULL, type=NULL, dir_path=NULL, ensemble_method="median", ...) {
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
                                   type=type)
            
            ##### Ensemble predictions #####
            
            # Generate ensemble predictions
            prediction_data <- ensemble_prediction(object=object,
                                                   prediction_data=data.table::rbindlist(predict_list),
                                                   ensemble_method=ensemble_method)
            
            # Return data
            return(prediction_data)
          })

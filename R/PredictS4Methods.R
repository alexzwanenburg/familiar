#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#####predict (model)#####
setMethod("predict", signature(object="familiarModel"),
          function(object, newdata, allow_recalibration=TRUE, is_pre_processed=FALSE, time_max=NULL, extra_output=FALSE) {
            
            # Read internal
            outcome_type <- object@outcome_type
            
            # Prepare input data
            newdata <- process_input_data(object=object,
                                          data=newdata,
                                          is_pre_processed=is_pre_processed)
            
            if(!extra_output){
              # Generate a prediction table.
              predictions <- learner.main(learner=object@learner, purpose="test", data_obj=newdata, object=object, time_max=time_max)
              
              # Apply calibration (if applicable - this is determined within the
              # function based on outcome_type and the presence of calibration
              # models).
              if(allow_recalibration==TRUE){
                predictions  <- learner.apply_calibration(object=object,
                                                          predictions=predictions)
              }
              
              # Return prediction table
              return(dt_pred)
              
            } else {
              # Return extra output in addition to predicted outcomes
              prediction_list <- learner.main(learner=object@learner,
                                              purpose="test",
                                              data_obj=newdata,
                                              object=object,
                                              time_max=time_max,
                                              extra_output=TRUE)
              
              # Apply calibration (if applicable - this is determined within the
              # function based on outcome_type and the presence of calibration
              # models)
              if(allow_recalibration){
                prediction_list$predictions <- learner.apply_calibration(object=object,
                                                                         predictions=predictions)
              }
              
              # Return prediction list
              return(prediction_list)
            }
            
          })


#####predict (ensemble)#####
setMethod("predict", signature(object="familiarEnsemble"),
          function(object, newdata, allow_recalibration=TRUE, is_pre_processed=FALSE, time_max=NULL, dir_path=NULL, extra_output=FALSE) {
            # Predict function for a model ensemble. This will always return
            # ensemble information, and not details corresponding to the
            # individual models.
            
            # Test if the models are loaded, and load the if not
            object <- load_models(object, dir_path=NULL)
            
            # Extract predictions for each individual model
            predict_list <- lapply(object@model_list, predict, newdata=newdata, allow_recalibration=allow_recalibration,
                                   extra_output=extra_output, is_pre_processed=is_pre_processed)
            
            ##### Ensemble predictions #####
            
            # Extract predictions
            if(extra_output & !is.null(predict_list[[1]]$predictions)) {
              prediction_data <- data.table::rbindlist(lapply(predict_list, function(list_entry) (list_entry$predictions)))
            } else {
              prediction_data <- data.table::rbindlist(predict_list)
            }
            
            # Generate 
            prediction_data <- ensemble_prediction(object=object, prediction_data=prediction_data, ensemble_method="mean")
            
            ##### Ensemble other output #####
            if(extra_output){
              
              # Prepare a return list
              output_list <- list("predictions"=prediction_data)
              
              if(object@outcome_type == "survival"){
                
                # Ensemble survival probabilities
                if(any(sapply(predict_list, function(list_entry) (!is.null(list_entry$survival))))){
                  output_list$survival <- ensemble_survival_matrix(prediction_list=predict_list)
                  
                }
                
                # Ensemble failure times
                if(any(sapply(predict_list, function(list_entry) (!is.null(list_entry$failure_times))))){
                  
                  # Create a failure_time ensemble
                  output_list$failure_times <- ensemble_failure_times(prediction_list=predict_list)
                  
                }
              }
              
              # Return the output list
              return(output_list)
            }
            
            # Return data
            return(prediction_data)
          })

setMethod("predict", signature(object="character"),
          function(object, ...){
            # Object is a path, or a vector of paths.
            browser()
          })

setMethod("predict", signature(object="list"),
          function(object, ...){
            # Object is a list. This can either be a list of paths, or a list of familiarModels.
            
            browser()
            
          })

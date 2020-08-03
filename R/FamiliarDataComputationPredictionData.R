#'@title Internal function to extract predicted values from models.
#'
#'@description Collects predicted values from models in a `familiarEnsemble`.
#'
#'@inheritParams extract_data
#'
#'@return A list with single-model and ensemble predictions.
#'@md
#'@keywords internal
setGeneric("extract_predictions",
           function(object,
                    data,
                    cl=NULL,
                    is_pre_processed=FALSE,
                    ensemble_method=waiver(),
                    eval_times=waiver(),
                    message_indent=0L,
                    verbose=FALSE,
                    ...) standardGeneric("extract_predictions"))

#####extract_predictions#####
setMethod("extract_predictions", signature(object="familiarEnsemble"),
          function(object,
                   data,
                   cl=NULL,
                   is_pre_processed=FALSE,
                   ensemble_method=waiver(),
                   eval_times=waiver(),
                   message_indent=0L,
                   verbose=FALSE,
                   ...){
            # Extract predictions from the data using the models in the
            # ensemble. Note: we do not call the predict function on the
            # familiarEnsemble directly as this would cause predict to become
            # highly convoluted, in particular with generating both single and
            # ensemble predictions.
            
            # Message extraction start
            if(verbose){
              logger.message(paste0("Computing ensemble predictions for the dataset."),
                             indent=message_indent)
            }
            
            # Load eval_times from the object settings attribute, if it is not provided.
            if(is.waive(eval_times)) eval_times <- object@settings$eval_times
            
            # Check eval_times argument
            if(object@outcome_type %in% c("survival")){
              sapply(eval_times, .check_number_in_valid_range, var_name="eval_times", range=c(0.0, Inf), closed=c(FALSE, TRUE))
            }
            
            # Obtain ensemble method from stored settings, if required.
            if(is.waive(ensemble_method)) ensemble_method <- object@settings$ensemble_method
            
            # Check ensemble_method argument
            .check_parameter_value_is_valid(x=ensemble_method, var_name="ensemble_method",
                                            values=.get_available_ensemble_prediction_methods())
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)) ..error_ensemble_models_not_loaded()
            
            # Aggregate data. It does not make sense to keep duplicate rows here.
            data <- aggregate_data(data=data)
            
            # Extract data for the individual models and the ensemble.
            performance_data <- universal_extractor(object=object,
                                                    cl=cl,
                                                    FUN=.extract_predictions,
                                                    individual_model_ci=FALSE,
                                                    data=data,
                                                    is_pre_processed=is_pre_processed,
                                                    ensemble_method=ensemble_method,
                                                    eval_times=eval_times,
                                                    message_indent=message_indent + 1L,
                                                    verbose=verbose)
            
            return(performance_data)
          })

.extract_predictions <- function(object, data, eval_times, ensemble_method, is_pre_processed, ...){
  
  if(object@outcome_type %in% c("survival")){
    # Compute predictions at each of the evaluation time points.
    prediction_data <- lapply(eval_times, function(time, object, data, ensemble_method, is_pre_processed){
      
      # Obtain prediction data
      prediction_data <- .predict(object=object,
                                  data=data,
                                  time=time,
                                  ensemble_method=ensemble_method,
                                  is_pre_processed=is_pre_processed)
      
      # Check that the prediction data is not empty.
      if(is_empty(prediction_data)) return(NULL)
                                           
      # Add an eval_time column.
      prediction_data[, "evaluation_time":=time]
      
      # Reorder columns
      data.table::setcolorder(x=prediction_data,
                              neworder=c(setdiff(colnames(prediction_data), c("predicted_outcome", "eval_time")),
                                                 "eval_time", "predicted_outcome"))
      
      return(prediction_data)
    },
    object=object,
    data=data,
    ensemble_method=ensemble_method,
    is_pre_processed=is_pre_processed)
    
    # Combine to single data.table
    prediction_data <- data.table::rbindlist(prediction_data)
    
  } else {
    # Compute performance data.
    prediction_data <- .predict(object=object,
                                data=data,
                                ensemble_method=ensemble_method,
                                is_pre_processed=is_pre_processed)
  }
  
  if(is_empty(prediction_data)) return(NULL)
  
  return(list("model_data"=prediction_data))
}

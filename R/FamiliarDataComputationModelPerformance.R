
#'@title Internal function to extract performance metrics.
#'
#'@description Computes and collects discriminative performance metrics from a
#'  `familiarEnsemble`.
#'
#'@inheritParams extract_data
#'
#'@details This method computes credibility intervals for the ensemble model, at
#'  the level of `confidence_level`. This is a general method. Metrics with
#'  known, theoretically derived confidence intervals, nevertheless have a
#'  credibility interval computed.
#'
#'@return A list with data.tables for single and ensemble model assessments.
#'@md
#'@keywords internal
setGeneric("extract_performance",
           function(object,
                    data,
                    cl=NULL,
                    metric=waiver(),
                    ensemble_method=waiver(),
                    eval_times=waiver(),
                    confidence_level=waiver(),
                    bootstrap_ci_method=waiver(),
                    compute_model_ci=waiver(),
                    compute_ensemble_ci=waiver(),
                    aggregate_ci=waiver(),
                    is_pre_processed=FALSE,
                    message_indent=0L,
                    verbose=FALSE,
                    ...) standardGeneric("extract_performance"))

#####extract_performance#####
setMethod("extract_performance", signature(object="familiarEnsemble"),
          function(object,
                   data,
                   cl=NULL,
                   metric=waiver(),
                   ensemble_method=waiver(),
                   eval_times=waiver(),
                   confidence_level=waiver(),
                   bootstrap_ci_method=waiver(),
                   compute_model_ci=waiver(),
                   compute_ensemble_ci=waiver(),
                   aggregate_ci=waiver(),
                   is_pre_processed=FALSE,
                   message_indent=0L,
                   verbose=FALSE,
                   ...){
            
            # Message extraction start
            if(verbose){
              logger.message(paste0("Computing performance metrics for models on the dataset."),
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
            
            # Load confidence alpha from object settings attribute if not
            # provided externally.
            if(is.waive(confidence_level)) confidence_level <- object@settings$confidence_level
            
            # Check confidence_level input argument
            .check_number_in_valid_range(x=confidence_level, var_name="confidence_level",
                                         range=c(0.0, 1.0), closed=c(FALSE, FALSE))
            
            # Check ensemble_method argument
            .check_parameter_value_is_valid(x=ensemble_method, var_name="ensemble_method",
                                            values=.get_available_ensemble_prediction_methods())
            
            # Load the bootstrap method
            if(is.waive(bootstrap_ci_method)) bootstrap_ci_method <- object@settings$bootstrap_ci_method
            
            .check_parameter_value_is_valid(x=bootstrap_ci_method, var_name="bootstrap_ci_methpd",
                                            values=.get_available_bootstrap_confidence_interval_methods())
            
            if(is.waive(compute_model_ci)) compute_model_ci <- "none"
            if(is.waive(compute_ensemble_ci)) compute_ensemble_ci <- "all"
            if(is.waive(aggregate_ci)) aggregate_ci <- "none"
            
            # Load metric(s) from the object settings attribute if not provided
            # externally.
            if(is.waive(metric)) metric <- object@settings$metric
            
            # Check metric input argument
            sapply(metric, metric.check_outcome_type, object=object)
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)) ..error_ensemble_models_not_loaded()
            
            # Extract data for the individual models and the ensemble.
            performance_data <- universal_extractor(object=object,
                                                    cl=cl,
                                                    FUN=.extract_model_performance_data,
                                                    data=data,
                                                    is_pre_processed=is_pre_processed,
                                                    ensemble_method=ensemble_method,
                                                    metric=metric,
                                                    eval_times=eval_times,
                                                    confidence_level=confidence_level,
                                                    compute_model_ci=any(c("all", "model_performance") %in% compute_model_ci),
                                                    compute_ensemble_ci=any(c("all", "model_performance") %in% compute_ensemble_ci),
                                                    aggregate_ci=any(c("all", "model_performance") %in% aggregate_ci),
                                                    bootstrap_ci_method=bootstrap_ci_method,
                                                    message_indent=message_indent + 1L,
                                                    verbose=verbose)
            
            return(performance_data)
          })



.extract_model_performance_data <- function(object, eval_times, determine_ci, bootstrap_ci_method,
                                            aggregate_ci, confidence_level, verbose, cl, message_indent, ...){
  
  if(object@outcome_type %in% c("survival")){
    # Compute performance data at each of the evaluation time points.
    performance_data <- lapply(eval_times,
                               ..extract_model_performance_data,
                               object=object,
                               determine_ci=determine_ci,
                               confidence_level=confidence_level,
                               bootstrap_ci_method=bootstrap_ci_method,
                               cl=cl,
                               verbose=verbose,
                               message_indent=message_indent,
                               ...)
    
    # Concatenate lists.
    performance_data <- list("model_data"=rbind_list_list(performance_data, "model_data"),
                             "bootstrap_data"=rbind_list_list(performance_data, "bootstrap_data"),
                             "confidence_level"=confidence_level)
    
  } else {
    # Compute performance data.
    performance_data <- do.call(..extract_model_performance_data,
                                args=c(list("object"=object,
                                            "determine_ci"=determine_ci,
                                            "bootstrap_ci_method"=bootstrap_ci_method,
                                            "confidence_level"=confidence_level,
                                            "cl"=cl,
                                            "verbose"=verbose,
                                            "message_indent"=message_indent),
                                       list(...)))
  }
  
  if(determine_ci & aggregate_ci){
  
    # Aggregate the data by computing the bootstrap confidence intervals.
    performance_data$model_data <- .compute_bootstrap_ci(x0=performance_data$model_data,
                                                         xb=performance_data$bootstrap_data,
                                                         target_column="value",
                                                         bootstrap_ci_method=bootstrap_ci_method,
                                                         additional_splitting_variable="metric",
                                                         confidence_level=confidence_level,
                                                         cl=cl,
                                                         verbose=verbose,
                                                         message_indent=message_indent)
    
    # Set the bootstrap_data to NULL.
    performance_data$bootstrap_data <- NULL
    
  } else if(determine_ci){
    # Add the bootstrap confidence interval method
    performance_data$bootstrap_ci_method <- bootstrap_ci_method
  }
  
  return(performance_data)
}



..extract_model_performance_data <- function(eval_times=NULL, object, data, cl=NULL, is_pre_processed,
                                             ensemble_method, metric, confidence_level,
                                             determine_ci, bootstrap_ci_method, verbose, message_indent=0L){
  
  # Perform a safety check.
  if(length(eval_times) > 1) ..error_reached_unreachable_code("..extract_model_performance_data: more than one value for eval_times")
  
  # Predict class probabilities.
  prediction_data <- .predict(object=object,
                              data=data,
                              time=eval_times,
                              ensemble_method=ensemble_method,
                              is_pre_processed=is_pre_processed)
  
  # Check if any predictions are valid.
  if(!any_predictions_valid(prediction_data, outcome_type=object@outcome_type)) return(NULL)
  
  # Iterate over class levels.
  performance_data <- lapply(metric,
                             .compute_model_performance,
                             data=prediction_data,
                             object=object,
                             time=eval_times,
                             confidence_level=confidence_level,
                             determine_ci=determine_ci,
                             cl=cl,
                             message_indent=message_indent,
                             verbose=verbose)
  
  # Concatenate lists.
  performance_data <- list("model_data"=rbind_list_list(performance_data, "model_data"),
                           "bootstrap_data"=rbind_list_list(performance_data, "bootstrap_data"),
                           "confidence_level"=confidence_level)
  
  return(performance_data)
}



.compute_model_performance <- function(metric, data, object, time=NULL, confidence_level,
                                       determine_ci, cl=NULL, message_indent=0L, verbose){
  
  # Check if the data has more than 1 row.
  if(nrow(data) <= 1) return(list("confidence_level"=confidence_level))
  
  # Compute data from the model.
  model_data <- ..compute_model_performance(metric=metric,
                                            data=data,
                                            time=time,
                                            object=object)
  
  if(determine_ci){
    if(verbose & !is.null(time)) logger.message(paste0("Computing bootstrap confidence interval data for the \"", metric, "\" metric at time ", time, "."),
                                                indent=message_indent)
    if(verbose & is.null(time)) logger.message(paste0("Computing bootstrap confidence interval data for the \"", metric, "\" metric."),
                                               indent=message_indent)
    
    # Bootstrap the data.
    bootstrap_data <- bootstrapper(data=data,
                                   alpha= 1.0 - confidence_level,
                                   FUN=..compute_model_performance,
                                   metric=metric,
                                   time=time,
                                   object=object,
                                   cl=cl,
                                   verbose=verbose)
    
  } else {
    bootstrap_data <- NULL
  }
  
  # Add to list and return
  return(list("model_data"=model_data,
              "bootstrap_data"=bootstrap_data,
              "confidence_level"=confidence_level))
}



..compute_model_performance <- function(metric,
                                        data,
                                        time,
                                        object){
  
  # Compute the metric score.
  browser()
  score <- compute_metric_score(metric=metric,
                                object=object)
  
  if(!is.finite(score)) return(NULL)
  
  if(!is.null(time)){
    performance_data <- data.table::data.table("evaluation_time"=time,
                                               "metric"=metric,
                                               "value"=score)
    
  } else {
    performance_data <- data.table::data.table("metric"=metric,
                                               "value"=score)
  }
  
  return(performance_data)
}

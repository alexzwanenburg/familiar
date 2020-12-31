#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarDataElementModelPerformance",
         contains="familiarDataElement",
         prototype = methods::prototype(value_column="value"))

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
                    detail_level=waiver(),
                    estimation_type=waiver(),
                    aggregate_results=waiver(),
                    confidence_level=waiver(),
                    bootstrap_ci_method=waiver(),
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
                   detail_level=waiver(),
                   estimation_type=waiver(),
                   aggregate_results=waiver(),
                   confidence_level=waiver(),
                   bootstrap_ci_method=waiver(),
                   is_pre_processed=FALSE,
                   message_indent=0L,
                   verbose=FALSE,
                   ...){
            
            # Message extraction start
            if(verbose){
              logger.message(paste0("Computing model performance metrics on the dataset."),
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
            
            # Check the level detail
            if(is.waive(detail_level)) detail_level <- object@settings$detail_level
            
            .check_parameter_value_is_valid(x=detail_level, var_name="detail_level",
                                            values=c("ensemble", "hybrid", "model"))
            
            # Check the estimation type
            if(is.waive(estimation_type)) estimation_type <- object@settings$estimation_type
            
            .check_parameter_value_is_valid(x=estimation_type, var_name="estimation_type",
                                            values=c("point", "bias_correction", "bc", "bootstrap_confidence_interval", "bci"))
            
            # Check whether results should be aggregated.
            if(is.waive(aggregate_results)) aggregate_results <- object@settings$aggregate_results
            
            aggregate_results <- tolower(aggregate_results)
            .check_parameter_value_is_valid(x=aggregate_results, var_name="aggregate_results",
                                            values=c(.get_available_data_elements(confidence_interval_only=TRUE),
                                                     "true", "false", "none", "all", "default"))
            # Set as TRUE/FALSE
            aggregate_results <- any(aggregate_results %in% c("true", "all", "model_performance"))
            
            # Load metric(s) from the object settings attribute if not provided
            # externally.
            if(is.waive(metric)) metric <- object@settings$metric
            
            # Check metric input argument
            sapply(metric, metric.check_outcome_type, object=object)
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)) ..error_ensemble_models_not_loaded()
            
            # Generate a prototype data element.
            proto_data_element <- new("familiarDataElementModelPerformance",
                                      detail_level = detail_level,
                                      estimation_type = estimation_type,
                                      confidence_level = confidence_level,
                                      bootstrap_ci_method = bootstrap_ci_method,
                                      value_column = "value")
            
            # Generate elements to send to dispatch.
            performance_data <- extract_dispatcher(FUN=.extract_model_performance_data,
                                                   has_internal_bootstrap=TRUE,
                                                   cl=cl,
                                                   object=object,
                                                   data=data,
                                                   proto_data_element=proto_data_element,
                                                   is_pre_processed=is_pre_processed,
                                                   ensemble_method=ensemble_method,
                                                   metric=metric,
                                                   eval_times=eval_times,
                                                   aggregate_results=aggregate_results,
                                                   message_indent=message_indent + 1L,
                                                   verbose=verbose)
            browser()
           
            return(performance_data)
          })



.extract_model_performance_data <- function(object,
                                            proto_data_element,
                                            eval_times=NULL,
                                            aggregate_results,
                                            cl,
                                            ...){
  
  # Add model name.
  proto_data_element <- add_model_name(proto_data_element, object=object)
  
  # Add evaluation time as a identifier to the data element.
  if(length(eval_times) > 0 & object@outcome_type == "survival"){
    data_elements <- add_data_element_identifier(x=proto_data_element, evaluation_time=eval_times)
    
  } else {
    data_elements <- list(proto_data_element)
  }
  
  # Iterate over data elements.
  performance_data <- lapply(data_elements,
                             ..extract_model_performance_data,
                             object=object,
                             aggregate_results=aggregate_results,
                             cl=cl,
                             ...)
  
  return(performance_data)
}



..extract_model_performance_data <- function(data_element,
                                             object,
                                             data,
                                             cl=NULL,
                                             is_pre_processed,
                                             ensemble_method,
                                             metric,
                                             aggregate_results,
                                             progress_bar=FALSE,
                                             verbose=FALSE,
                                             message_indent,
                                             ...){
  
  # Message the user concerning the time at which metrics are computed. This is
  # only relevant for survival analysis.
  if(length(data_element@identifiers$evaluation_time) > 0 & progress_bar){
    logger.message(paste0("Computing metric value at time ", data_element@identifiers$evaluation_time, "."),
                   indent=message_indent)
  }
  
  # Predict class probabilities.
  prediction_data <- .predict(object=object,
                              data=data,
                              time=data_element@identifiers$evaluation_time,
                              ensemble_method=ensemble_method,
                              is_pre_processed=is_pre_processed)
  
  # Check if any predictions are valid.
  if(!any_predictions_valid(prediction_data, outcome_type=object@outcome_type)) return(NULL)
  
  # Add metric as an identifier.
  data_elements <- add_data_element_identifier(x=data_element,
                                               metric=metric)
  
  # Add bootstrap data.
  bootstrap_data <- add_data_element_bootstrap(x=data_elements,
                                              ...)
  
  # Iterate over elements.
  data_elements <- fam_mapply(cl=cl,
                              assign=NULL,
                              FUN=.compute_model_performance,
                              data_element=bootstrap_data$data_element,
                              bootstrap=bootstrap_data$bootstrap,
                              MoreArgs=list("object"=object,
                                            "data"=prediction_data),
                              progress_bar=progress_bar)
  
  # Merge data elements
  data_elements <- merge_data_elements(data_elements)
  
  if(aggregate_results) data_elements <- .compute_data_element_estimates(x=data_elements)
  
  return(data_elements)
}



.compute_model_performance <- function(data_element, object, data, bootstrap){
  
  # Bootstrap the data.
  if(bootstrap) data <- get_bootstrap_sample(data=data)
  
  # Compute the score
  score <- compute_metric_score(metric=data_element@identifiers$metric,
                                object=object,
                                data=data,
                                time=data_element@identifiers$evaluation_time)
  
  # Set the value, if finite.
  if(is.finite(score)) data_element@data <- list("value"=unname(score))
  
  return(data_element)
}

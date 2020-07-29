
#'@title Internal function to extract permutation variable importance.
#'
#'@description Computes and collects permutation variable importance from a
#'  `familiarEnsemble`.
#'
#'@inheritParams extract_data
#'
#'@details This function also computes credibility intervals for the ensemble
#'  model, at the level of `confidence_level`.
#'
#'@return A list with data.tables for single and ensemble model assessments.
#'@md
#'@keywords internal
setGeneric("extract_permutation_vimp",
           function(object,
                    data,
                    cl=NULL,
                    metric=waiver(),
                    ensemble_method=waiver(),
                    eval_times=waiver(),
                    confidence_level=waiver(),
                    bootstrap_ci_method=waiver(),
                    aggregate_ci=waiver(),
                    is_pre_processed=FALSE,
                    message_indent=0L,
                    verbose=FALSE,
                    ...) standardGeneric("extract_permutation_vimp"))

#####extract_permutation_vimp#####
setMethod("extract_permutation_vimp", signature(object="familiarEnsemble"),
          function(object,
                   data,
                   cl=NULL,
                   metric=waiver(),
                   ensemble_method=waiver(),
                   eval_times=waiver(),
                   confidence_level=waiver(),
                   bootstrap_ci_method=waiver(),
                   aggregate_ci=waiver(),
                   is_pre_processed=FALSE,
                   message_indent=0L,
                   verbose=FALSE,
                   ...){

            # Message extraction start
            if(verbose){
              logger.message(paste0("Computing permutation variable importance for models in the dataset."),
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
            
            # Load the aggregate_ci parameter, if required.
            if(is.waive(aggregate_ci)) aggregate_ci <- object@settings$aggregate_ci
            
            
            # Load metric(s) from the object settings attribute if not provided
            # externally.
            if(is.waive(metric)) metric <- object@settings$metric
            
            # Check metric input argument
            sapply(metric, metric.check_outcome_type, outcome_type=object@outcome_type)
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)) ..error_ensemble_models_not_loaded()
            
            # Extract data for the individual models and the ensemble.
            vimp_data <- universal_extractor(object=object,
                                             cl=cl,
                                             FUN=.extract_permutation_vimp,
                                             individual_model_ci=FALSE,
                                             data=data,
                                             is_pre_processed=is_pre_processed,
                                             ensemble_method=ensemble_method,
                                             metric=metric,
                                             eval_times=eval_times,
                                             confidence_level=confidence_level,
                                             aggregate_ci=any(c("all", "permutation_vimp") %in% aggregate_ci),
                                             bootstrap_ci_method=bootstrap_ci_method,
                                             message_indent=message_indent + 1L,
                                             verbose=verbose)
            
            return(vimp_data)
          })



.extract_permutation_vimp <- function(cl,
                                      object,
                                      data,
                                      is_pre_processed,
                                      eval_times,
                                      ensemble_method,
                                      metric,
                                      determine_ci,
                                      bootstrap_ci_method,
                                      aggregate_ci,
                                      confidence_level,
                                      verbose,
                                      message_indent){
  
  if(object@outcome_type %in% c("survival")){
    # Compute permutation variable importance data at each of the evaluation time points.
    vimp_data <- lapply(eval_times,
                        ..extract_permutation_vimp,
                        object=object,
                        data=data,
                        cl=cl,
                        metric=metric,
                        is_pre_processed=is_pre_processed,
                        ensemble_method=ensemble_method,
                        determine_ci=determine_ci,
                        confidence_level=confidence_level,
                        verbose=verbose,
                        message_indent=message_indent)
    
    # Concatenate lists.
    vimp_data <- list("model_data"=rbind_list_list(vimp_data, "model_data"),
                      "bootstrap_data"=rbind_list_list(vimp_data, "bootstrap_data"),
                      "confidence_level"=confidence_level)
    
  } else {
    # Compute permutation variable importance data.
    vimp_data <- ..extract_permutation_vimp(object=object,
                                            data=data,
                                            is_pre_processed=is_pre_processed,
                                            cl=cl,
                                            metric=metric,
                                            ensemble_method=ensemble_method,
                                            determine_ci=determine_ci,
                                            confidence_level=confidence_level,
                                            verbose=verbose,
                                            message_indent=message_indent)
  }
  
  if(determine_ci & aggregate_ci){
    
    # Aggregate the data by computing the bootstrap confidence intervals.
    vimp_data$model_data <- .compute_bootstrap_ci(x0=vimp_data$model_data,
                                                  xb=vimp_data$bootstrap_data,
                                                  target_column="value",
                                                  bootstrap_ci_method=bootstrap_ci_method,
                                                  additional_splitting_variable=c("metric", "feature"),
                                                  confidence_level=confidence_level,
                                                  cl=cl,
                                                  verbose=verbose,
                                                  message_indent=message_indent)
    
    # Set the bootstrap_data to NULL.
    vimp_data$bootstrap_data <- NULL
    
  } else if(determine_ci){
    # Add the bootstrap confidence interval method
    vimp_data$bootstrap_ci_method <- bootstrap_ci_method
  }
  
  return(vimp_data)
}



..extract_permutation_vimp <- function(eval_times=NULL,
                                       object,
                                       data,
                                       is_pre_processed,
                                       cl,
                                       metric,
                                       n_reshuffle=21,
                                       ensemble_method,
                                       determine_ci,
                                       confidence_level,
                                       verbose,
                                       message_indent){

  # Perform a safety check.
  if(length(eval_times) > 1) ..error_reached_unreachable_code("..extract_permutation_vimp: more than one value for eval_times")
  
  # Load input data. We stop at the imputation step because we want to work with
  # the unclustered input features.
  data <- process_input_data(object=object,
                             data=data,
                             is_pre_processed=is_pre_processed,
                             stop_at="imputation")
  
  # Perform some checks to see if there is any data to be 
  if(is_empty(data)) return(NULL)
  if(nrow(data@data) < 5) return(NULL)
  
  # Iterate over features.
  vimp_data <-lapply(get_feature_columns(data), function(shuffled_features,
                                                         object,
                                                         data,
                                                         cl,
                                                         confidence_level,
                                                         determine_ci,
                                                         time,
                                                         metric,
                                                         n_reshuffle,
                                                         ensemble_method,
                                                         message_indent,
                                                         verbose){
    
    # Compute the point estimate for permutation vimp.
    vimp_data <- .compute_permutation_vimp(shuffled_features=shuffled_features,
                                           object=object,
                                           data=data,
                                           time=time,
                                           metric=metric,
                                           n_reshuffle=n_reshuffle,
                                           ensemble_method)
    
    if(determine_ci){
      if(verbose & !is.null(time)) logger.message(paste0("Computing bootstrap confidence interval data for variable imputation of ",
                                                         paste_s(shuffled_features), ifelse(length(shuffled_features) == 1, " feature", "features"),
                                                         " at time ", time, "."),
                                                  indent=message_indent)
      if(verbose & is.null(time)) logger.message(paste0("Computing bootstrap confidence interval data for variable imputation of ",
                                                        paste_s(shuffled_features), ifelse(length(shuffled_features) == 1, " feature", "features"),
                                                        "."),
                                                 indent=message_indent)
      
      # Bootstrap the data.
      bootstrap_data <- bootstrapper(data=data,
                                     alpha= 1.0 - confidence_level,
                                     FUN=.compute_permutation_vimp,
                                     shuffled_features=shuffled_features,
                                     object=object,
                                     time=time,
                                     metric=metric,
                                     n_reshuffle=1L,
                                     ensemble_method=ensemble_method,
                                     cl=cl,
                                     verbose=verbose)
      
    } else {
      bootstrap_data <- NULL
    }
    
    return(list("model_data"=vimp_data,
                "bootstrap_data"=bootstrap_data))
  },
  object=object,
  data=data,
  cl=cl,
  confidence_level=confidence_level,
  determine_ci=determine_ci,
  time=eval_times,
  metric=metric,
  n_reshuffle=n_reshuffle,
  ensemble_method=ensemble_method,
  message_indent=message_indent,
  verbose=verbose)
  
  # Concatenate lists.
  vimp_data <- list("model_data"=rbind_list_list(vimp_data, "model_data"),
                    "bootstrap_data"=rbind_list_list(vimp_data, "bootstrap_data"),
                    "confidence_level"=confidence_level)
  
  return(vimp_data)
}



.compute_permutation_vimp <- function(shuffled_features,
                                      object,
                                      data,
                                      time,
                                      metric,
                                      n_reshuffle,
                                      ensemble_method){
 
  # Make the unshuffled prediction.
  unshuffled_prediction <- .predict(object=object,
                                    data=data,
                                    time=time,
                                    ensemble_method=ensemble_method)
  
  # Check if any predictions are valid.
  if(!any_predictions_valid(unshuffled_prediction, outcome_type=object@outcome_type)) return(NULL)
  
  # Compute the unshuffled metrics.
  unshuffled_metrics <- lapply(metric,
                               ..compute_model_performance,
                               data=unshuffled_prediction,
                               time=time,
                               object=object)
  
  # Concatenate lists
  unshuffled_metrics <- data.table::rbindlist(unshuffled_metrics)
  
  # Determine the reshuffles.
  shuffle_ids <- lapply(seq_len(n_reshuffle), function(ii, n) sample.int(n=n), n=nrow(data@data))
  
  # Compute data from shuffled datasets.
  shuffled_metrics <- lapply(shuffle_ids,
                             .compute_shuffle_permutation_vimp,
                             shuffled_features=shuffled_features,
                             object=object,
                             data=data,
                             time=time,
                             metric=metric,
                             ensemble_method=ensemble_method)
  
  # Concatenate lists
  shuffled_metrics <- data.table::rbindlist(shuffled_metrics)
  
  # Check if 
  if(is_empty(shuffled_metrics)) return(NULL)
  
  permutation_data <- lapply(metric, function(selected_metric, unshuffled_metrics, shuffled_metrics, time, shuffled_features){
    
    # Obtain the unshuffled values.
    unshuffled_value <- unshuffled_metrics[metric==selected_metric]$value
    
    # Obtain the shuffled values.
    shuffled_value <- shuffled_metrics[metric==selected_metric]$value
    
    if(length(shuffled_value) == 0) return(NULL)
    
    # Compute the median value.
    shuffled_value <- stats::median(shuffled_value, na.rm=TRUE)
    
    # Check that the median value is finite.
    if(!is.finite(shuffled_value)) return(NULL)
    
    # Return data.
    if(!is.null(time)){
      return(data.table::data.table("feature"=shuffled_features,
                                    "eval_time"=time,
                                    "metric"=selected_metric,
                                    "value"=unshuffled_value - shuffled_value))
      
    } else {
      return(data.table::data.table("feature"=shuffled_features,
                                    "metric"=selected_metric,
                                    "value"=unshuffled_value - shuffled_value))
    }
  },
  unshuffled_metrics=unshuffled_metrics,
  shuffled_metrics=shuffled_metrics,
  time=time,
  shuffled_features=shuffled_features)
  
  return(data.table::rbindlist(permutation_data))
}



.compute_shuffle_permutation_vimp <- function(shuffle_id,
                                              shuffled_features,
                                              object,
                                              data,
                                              time,
                                              metric,
                                              ensemble_method){
  
  # Make a local copy of data
  data@data <- data.table::copy(data@data)
  
  # Iterate over features and shuffle them.
  for(ii in shuffled_features) data.table::set(x=data@data, j=ii, value=data@data[[ii]][shuffle_id])
  
  # Predict for the shuffled data
  shuffled_prediction <- .predict(object=object,
                                  data=data,
                                  time=time,
                                  ensemble_method=ensemble_method)
  
  # Check if any predictions are valid.
  if(!any_predictions_valid(shuffled_prediction, outcome_type=object@outcome_type)) return(NULL)
  
  # Compute the unshuffled metrics.
  shuffled_metrics <- lapply(metric,
                             ..compute_model_performance,
                             data=shuffled_prediction,
                             time=time,
                             object=object)
  
  return(data.table::rbindlist(shuffled_metrics))
}

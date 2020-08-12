#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
#' @include MetricS4AUC.R

as_metric <- function(metric,
                      object=NULL,
                      outcome_type=NULL,
                      ...){
  
  if(is.null(outcome_type)){
    if(is(object, "familiarModel") | is(object, "familiarEnsemble")) outcome_type <- object@outcome_type
  }
  
  if(metric %in% .get_available_auc_roc_metrics()){
    metric_object <- methods::new("familiarMetricAUCROC",
                                  outcome_type=outcome_type,
                                  ...)
  }
  browser()
  
  return(metric_object)
}



#####is_available#####
setMethod("is_available", signature(object="familiarMetric"),
          function(object, ...) return(FALSE))



#####is_higher_better#####
setMethod("is_higher_better", signature(metric="familiarMetric"),
          function(metric, ...) return(metric@higher_better))



#####compute_metric_score#####
setMethod("compute_metric_score", signature(metric="familiarMetric"),
          function(metric, data, ...) return(NA_real_))



#####compute_objective_score####
setMethod("compute_objective_score", signature(metric="familiarMetric"),
          function(metric, data=NULL, value=NULL, ...){
            browser()
            # Check that a baseline value was set
            if(is.null(metric@baseline_value)){
              
              # Set the baseline value.
              metric <- set_metric_baseline_value(metric=metric, data=data, ...)
              
              # Check again
              if(is.null(metric@baseline_value)) ..error_reached_unreachable_code("compute_objective_score: baseline_value was not set.")
            }
            
            # Compute the value, if not provided.
            if(is.null(value)) value <- compute_metric_score(metric=metric,
                                                             data=data)
            
            # Get the baseline_value
            baseline_value <- metric@baseline_value
            
            # Determine the optimal value, i.e. the best value attainable.
            optimal_value <- ifelse(is_higher_better(metric),
                                    max(metric@value_range),
                                    min(metric@value_range))
            
            # If the baseline value is already perfect, use a small offset instead.
            if(baseline_value == optimal_value) baseline_value <- ifelse(is_higher_better(metric), optimal_value - 1E-5, optimal_value + 1E-5)
            
            # Compute the objective_value
            objective_value <- ifelse(is_higher_better(metric),
                                      (value - baseline_value) / (optimal_value - baseline_value),
                                      (baseline_value - value) / (baseline_value - optimal_value))
            
            # Ensure that all objective scores fall in the [-1, 1] range.
            if(is.na(objective_value)){
              objective_value <- NA_real_
              
            } else if(objective_value < -1.0) {
              objective_value <- -1.0
              
            } else if(objective_value > 1.0) {
              ..error_reached_unreachable_code(paste0("compute_objective_score: objective value exceeds the maximum of 1.0: ", objective_value))
            }
            
            return(objective_value)
          })



#####set_metric_baseline_value#####
setMethod("set_metric_baseline_value", signature(metric="familiarMetric"),
          function(metric, object=NULL, data=NULL){
            browser()
            # Obtain or create 
            if(is(object, "familiarModel") | is(object, "familiarVimpMethod" | is(object, "familiarEnsemble"))){
              outcome_info <- object@outcome_info
              
            } else if(is(data, "dataObject")) {
              if(is(data@outcome_info, "outcomeInfo")){
                outcome_info <- object@outcome_info
                
              } else {
                # Compute outcome information from scratch.
                outcome_info <- create_outcome_info_from_data(data=data@data)
                outcome_info <- .compute_outcome_distribution_data(object=outcome_info,
                                                                   data=data@data)
              }
              
            } else if(data.table::is.data.table(data)) {
              # Compute outcome information from scratch.
              outcome_info <- create_outcome_info_from_data(data=data)
              outcome_info <- .compute_outcome_distribution_data(object=outcome_info,
                                                                 data=data)
              
            } else {
              ..error_reached_unreachable_code("set_metric_baseline_value: baseline_value could not be set using the provided data.")
            }
            
            # Get a placeholder prediction table.
            prediction_table <- get_placeholder_prediction_table(object=outcome_info,
                                                                 data=data)
            
            # We need to identify the data source for determining baseline
            # values.
            if(metric@outcome_type %in% c("binomial", "multinomial")){
              
              # Get the frequency table and find the class with the majority.
              frequency_table <- object@distribution$frequency
              majority_class <- frequency_table$outcome[which.max(frequency_table$count)]
              
              # Fill the prediction_table.
              prediction_table[, "predicted_class":=majority_class]
              
              # Define probabilities columns
              outcome_probability_columns <- get_class_probability_name(object)
              
              for(ii in seq_along(outcome_probability_columns)){
                
                # Update the predicted probabilities with 1.0 for the majority
                # class and 0.0 for minority classes.
                if(outcome_probability_columns[ii] == get_class_probability_name(majority_class)){
                  prediction_table[, (outcome_probability_columns[ii]):=1.0]
                  
                } else{
                  prediction_table[, (outcome_probability_columns[ii]):=0.0]
                }
              } 
              
            } else if(metric@outcome_type %in% c("count", "continuous")){
              
              # Baseline median value.
              median_value <- object@distribution$median
              
              # Fill the prediction_table.
              prediction_table[, "predicted_outcome":=median_value]
              
            } else if(metric@outcome_type %in% c("survival")){
              
              # Median baseline survival
              median_value <- sum(c(min(object@distribution$survival_probability$survival_probability),
                                    max(object@distribution$survival_probability$survival_probability))) / 2.0
              
              # Fill the prediction_table.
              prediction_table[, "predicted_outcome":=median_value]
              
            } else {
              ..error_outcome_type_not_implemented(metric@outcome_type)
            }
            
            # Compute metric value
            metric@baseline_value <- compute_metric_score(metric=metric,
                                                          data=prediction_table)
            
            return(metric)
          })



metric.check_outcome_type <- function(metric, object=NULL, outcome_type=NULL){
  
  # Obtain outcome_type
  if(is.null(outcome_type) & !is.null(object)) outcome_type <- object@outcome_type
  
  # Initialise metric
  metric_object <- as_metric(metric=metric,
                             outcome_type=outcome_type)
  
  # Check if the metric is available.
  if(!is_subclass(class(metric_object)[1], "familiarMetric")){
    stop(paste0(metric, " is not a valid metric. Please check the vignette for available performance metrics."))
    
  } else if(!is_available(metric_object)){
    stop(paste0("The ", metric, " metric is not available for ", outcome_type, " outcomes."))
  }
}



metric.get_metric_default_range <- function(metric, object=NULL, outcome_type=NULL){
  # Get default range of metric scores, e.g. for plotting metric values.
  
  # Obtain outcome_type
  if(is.null(outcome_type) & !is.null(object)) outcome_type <- object@outcome_type
  
  # Initialise metric object.
  metric_object <- as_metric(metric=metric,
                             outcome_type=outcome_type)
  
  return(metric_object@value_range)
}


metric.is_higher_score_better <- function(metric, object=NULL, outcome_type=NULL){
  
  # Obtain outcome_type
  if(is.null(outcome_type) & !is.null(object)) outcome_type <- object@outcome_type
  
  # Initialise metric object.
  metric_object <- as_metric(metric=metric,
                             outcome_type=outcome_type)
  
  return(metric_object@higher_better)
}

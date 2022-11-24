#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarMetricRegression",
         contains="familiarMetric",
         slots=list("robust"="character"),
         prototype=list("robust"="none"))

setMethod("initialize", signature(.Object="familiarMetricRegression"),
          function(.Object, metric, ...){
            
            # Update with parent class first.
            .Object <- callNextMethod()
            
            if(endsWith(x=metric, suffix="_trim")){
              .Object@robust <- "trim"
              .Object@metric <- sub_last(x=metric,
                                         pattern="_trim",
                                         replacement="",
                                         fixed=TRUE)
              
            } else if(endsWith(x=metric, suffix="_winsor")){
              .Object@robust <- "winsor"
              .Object@metric <- sub_last(x=metric,
                                         pattern="_winsor",
                                         replacement="",
                                         fixed=TRUE)
              
            } else {
              # Default setting.
              .Object@robust <- "none"
              .Object@metric <- metric
            }
            
            return(.Object)
          })

setMethod("is_available", signature(object="familiarMetricRegression"),
          function(object, ...){
            return(object@outcome_type %in% c("count", "continuous"))
          })


.get_available_regression_metrics <- function(){
  return(c(.get_available_mae_metrics(),
           .get_available_mlae_metrics(),
           .get_available_mse_metrics(),
           .get_available_msle_metrics(),
           .get_available_medea_metrics(),
           .get_available_rmse_metrics(),
           .get_available_rmsle_metrics(),
           .get_available_r_squared_metrics(),
           .get_available_explained_variance_metrics(),
           .get_available_rse_metrics(),
           .get_available_rrse_metrics(),
           .get_available_rae_metrics()))
} 


#####Mean absolute error########################################################
setClass("familiarMetricMAE",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "Mean Absolute Error",
                                      value_range = c(0.0, Inf),
                                      higher_better = FALSE))


.get_available_mae_metrics <- function(){
  return(paste_c(c("mae", "mean_absolute_error"), c("", "_trim", "_winsor")))
} 



#####compute_metric_score (Mean absolute error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricMAE"),
          function(metric, data, ...){
            
            # Prepare data for computing metric values.
            data <- ..process_data_for_regression_metrics(metric=metric,
                                                          data=data)
            
            if(is_empty(data)) return(callNextMethod())
            
            # Compute the mean absolute error.
            score <- sum(abs(data$outcome - data$predicted_outcome)) / nrow(data)
            
            return(score)
          })



#####Relative absolute error####################################################
setClass("familiarMetricRAE",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "Relative Absolute Error",
                                      value_range = c(0.0, Inf),
                                      higher_better = FALSE))

.get_available_rae_metrics <- function(){
  return(paste_c(c("rae", "relative_absolutive_error"), c("", "_trim", "_winsor")))
}


#####compute_metric_score (Relative absolute error)#############################
setMethod("compute_metric_score", signature(metric="familiarMetricRAE"),
          function(metric, data, ...){
            
            # Prepare data for computing metric values.
            data <- ..process_data_for_regression_metrics(metric=metric,
                                                          data=data)
            
            if(is_empty(data)) return(callNextMethod())
            
            y_mean <- mean(data$outcome)
            nom <- sum(abs(data$outcome - data$predicted_outcome))
            denom <- sum(abs(y_mean - data$outcome))
            
            # Check if denominator is not 0.
            if(!denom == 0){
              # Denominator not 0
              score <- nom / denom
              
            } else if (nom == denom) {
              # Denominator = nominator = 0
              score <- 0.0
              
            } else {
              # Denominator = 0. Would be inf otherwise.
              return(callNextMethod())
            }
            
            return(score)
          })


#####Mean log absolute error####################################################
setClass("familiarMetricMLAE",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "Mean Log Absolute Error",
                                      value_range = c(0.0, Inf),
                                      higher_better = FALSE))


.get_available_mlae_metrics <- function(){
  return(paste_c(c("mlae", "mean_log_absolute_error"), c("", "_trim", "_winsor")))
} 



#####compute_metric_score (Mean log absolute error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricMLAE"),
          function(metric, data, ...){
            
            # Prepare data for computing metric values.
            data <- ..process_data_for_regression_metrics(metric=metric,
                                                          data=data)
            
            if(is_empty(data)) return(callNextMethod())
            
            # Compute the mean log absolute error.
            score <- sum(log1p(abs(data$outcome - data$predicted_outcome))) / nrow(data)
            
            return(score)
          })



#####Mean squared error#########################################################
setClass("familiarMetricMSE",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "Mean Squared Error",
                                      value_range = c(0.0, Inf),
                                      higher_better = FALSE))


.get_available_mse_metrics <- function(){
  return(paste_c(c("mse", "mean_squared_error"), c("", "_trim", "_winsor")))
} 



#####compute_metric_score (Mean squared error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricMSE"),
          function(metric, data, ...){
            
            # Prepare data for computing metric values.
            data <- ..process_data_for_regression_metrics(metric=metric,
                                                          data=data)
            
            if(is_empty(data)) return(callNextMethod())
            
            # Compute the mean squared error.
            score <- sum((data$outcome - data$predicted_outcome)^2) / nrow(data)
            
            return(score)
          })


#####Relative squared error#####################################################
setClass("familiarMetricRSE",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "Relative Squared Error",
                                      value_range = c(0.0, Inf),
                                      higher_better = FALSE))


.get_available_rse_metrics <- function(){
  return(paste_c(c("rse", "relative_squared_error"), c("", "_trim", "_winsor")))
} 



#####compute_metric_score (Root relative squared error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricRSE"),
          function(metric, data, ...){
            
            # Prepare data for computing metric values.
            data <- ..process_data_for_regression_metrics(metric=metric,
                                                          data=data)
            
            if(is_empty(data)) return(callNextMethod())
            
            # Compute the mean squared error.
            y_mean <- mean(data$outcome)
            nom <- sum((data$outcome - data$predicted_outcome)^2)
            denom <- sum((y_mean - data$outcome)^2)
            
            # Check if denominator is not 0.
            if(!denom == 0){
              # Denominator not 0
              score <- nom / denom
              
            } else if (nom == denom) {
              # Denominator = nominator = 0
              score <- 0.0
              
            } else {
              # Denominator = 0. Would be inf otherwise.
              return(callNextMethod())
            }
            
            return(score)
          })



#####Mean squared log error#####################################################
setClass("familiarMetricMSLE",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "Mean Squared Log Error",
                                      value_range = c(0.0, Inf),
                                      higher_better = FALSE))


.get_available_msle_metrics <- function(){
  return(paste_c(c("msle", "mean_squared_log_error"), c("", "_trim", "_winsor")))
} 



#####compute_metric_score (Mean squared log error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricMSLE"),
          function(metric, data, ...){
            
            # Prepare data for computing metric values.
            data <- ..process_data_for_regression_metrics(metric=metric,
                                                          data=data)
            
            if(is_empty(data)) return(callNextMethod())
            
            # Compute the mean squared log error.
            score <- suppressWarnings(sum((log1p(data$outcome) - log1p(data$predicted_outcome))^2, na.rm=TRUE)) / nrow(data)
            
            if(!is.finite(score)) return(callNextMethod())
            
            return(score)
          })



#####Median absolute error######################################################
setClass("familiarMetricMedianAE",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "Median Absolute Error",
                                      value_range = c(0.0, Inf),
                                      higher_better = FALSE))


.get_available_medea_metrics <- function(){
  return(paste_c(c("medae", "median_absolute_error"), c("", "_trim", "_winsor")))
} 



#####compute_metric_score (Median absolute error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricMedianAE"),
          function(metric, data, ...){
            
            # Prepare data for computing metric values.
            data <- ..process_data_for_regression_metrics(metric=metric,
                                                          data=data)
            
            if(is_empty(data)) return(callNextMethod())
            
            # Compute the median absolute error.
            score <- stats::median(x=abs(data$outcome - data$predicted_outcome))
            
            return(score)
          })



#####Root mean square error#####################################################
setClass("familiarMetricRMSE",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "Root Mean Square Error",
                                      value_range = c(0.0, Inf),
                                      higher_better = FALSE))


.get_available_rmse_metrics <- function(){
  return(paste_c(c("rmse", "root_mean_square_error"), c("", "_trim", "_winsor")))
} 



#####compute_metric_score (Root mean square error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricRMSE"),
          function(metric, data, ...){
            
            # Prepare data for computing metric values.
            data <- ..process_data_for_regression_metrics(metric=metric,
                                                          data=data)
            
            if(is_empty(data)) return(callNextMethod())
            
            # Compute the root mean square error.
            score <- sqrt(sum((data$outcome - data$predicted_outcome)^2) / nrow(data))
            
            return(score)
          })



#####Root relative squared error################################################
setClass("familiarMetricRRSE",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "Root Relative Squared Error",
                                      value_range = c(0.0, Inf),
                                      higher_better = FALSE))


.get_available_rrse_metrics <- function(){
  return(paste_c(c("rrse", "root_relative_squared_error"), c("", "_trim", "_winsor")))
} 



#####compute_metric_score (Root relative squared error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricRRSE"),
          function(metric, data, ...){
            
            # Prepare data for computing metric values.
            data <- ..process_data_for_regression_metrics(metric=metric,
                                                          data=data)
            
            if(is_empty(data)) return(callNextMethod())
            
            # Compute the mean squared error.
            y_mean <- mean(data$outcome)
            nom <- sum((data$outcome - data$predicted_outcome)^2)
            denom <- sum((y_mean - data$outcome)^2)
            
            # Check if denominator is not 0.
            if(!denom == 0){
              # Denominator not 0
              score <- sqrt(nom / denom)
              
            } else if (nom == denom) {
              # Denominator = nominator = 0
              score <- 0.0
              
            } else {
              # Denominator = 0. Would be inf otherwise.
              return(callNextMethod())
            }
            
            return(score)
          })



#####Root mean square log error#####################################################
setClass("familiarMetricRMSLE",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "Root Mean Square Log Error",
                                      value_range = c(0.0, Inf),
                                      higher_better = FALSE))


.get_available_rmsle_metrics <- function(){
  return(paste_c(c("rmsle", "root_mean_square_log_error"), c("", "_trim", "_winsor")))
} 



#####compute_metric_score (Root mean square log error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricRMSLE"),
          function(metric, data, ...){
            
            # Prepare data for computing metric values.
            data <- ..process_data_for_regression_metrics(metric=metric,
                                                          data=data)
            
            if(is_empty(data)) return(callNextMethod())
            
            # Compute the root mean square log error.
            score <- sqrt(sum((log1p(data$outcome) - log1p(data$predicted_outcome))^2) / nrow(data))
            
            if(!is.finite(score)) return(callNextMethod())
            
            return(score)
          })


#####R squared##################################################################
setClass("familiarMetricR2",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "R squared",
                                      value_range = c(-Inf, 1),
                                      higher_better = TRUE))


.get_available_r_squared_metrics <- function(){
  return(paste_c(c("r2_score", "r_squared"), c("", "_trim", "_winsor")))
} 



#####compute_metric_score (R squared)#####
setMethod("compute_metric_score", signature(metric="familiarMetricR2"),
          function(metric, data, ...){
            
            # Prepare data for computing metric values.
            data <- ..process_data_for_regression_metrics(metric=metric,
                                                          data=data)
            
            if(is_empty(data)) return(callNextMethod())
           
            y_mean <- mean(data$outcome)
            nom <- sum((data$outcome - data$predicted_outcome)^2)
            denom <- sum((data$outcome - y_mean)^2)
            
            # Check if denominator is not 0.
            if(!denom == 0){
              # Denominator not 0
              score <- 1.0 - nom / denom
              
            } else if (nom == denom) {
              # Denominator = nominator = 0
              score <- 1.0
              
            } else {
              # Denominator = 0. Would be -inf otherwise.
              return(callNextMethod())
            }
            
            return(score)
          })



#####Explained variance#########################################################
setClass("familiarMetricExplainedVariance",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "Explained Variance",
                                      value_range = c(-Inf, 1),
                                      higher_better = TRUE))


.get_available_explained_variance_metrics <- function(){
  return(paste_c(c("explained_variance"), c("", "_trim", "_winsor")))
} 



#####compute_metric_score (Explained variance)#####
setMethod("compute_metric_score", signature(metric="familiarMetricExplainedVariance"),
          function(metric, data, ...){
            
            # Prepare data for computing metric values.
            data <- ..process_data_for_regression_metrics(metric=metric,
                                                          data=data)
            
            if(is_empty(data)) return(callNextMethod())
            
            # Explained variance
            y_err_mean <- mean(data$outcome - data$predicted_outcome)
            y_mean <- mean(data$outcome)
            nom   <- sum((data$outcome - data$predicted_outcome - y_err_mean)^2)
            denom <- sum((data$outcome - y_mean)^2)
            
            # Check if denominator is not 0.
            if(!denom == 0){
              # Denominator not 0
              score <- 1.0 - nom / denom
              
            } else if (nom == denom) {
              # Denominator = nominator = 0
              score <- 1.0
              
            } else {
              # Denominator = 0. Would be -inf otherwise.
              return(callNextMethod())
            }
            
            return(score)
          })



..process_data_for_regression_metrics <- function(metric, data, fraction=0.05){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- predicted_outcome <- NULL
  
  # Remove any entries that lack valid predictions.
  data <- remove_nonvalid_predictions(prediction_table=data,
                                      outcome_type=metric@outcome_type)
  
  # Remove any entries that lack observed values.
  data <- remove_missing_outcomes(data=data,
                                  outcome_type=metric@outcome_type)
  
  if(is_empty(data)) return(data)
  
  if(metric@robust == "trim"){
    # Compute absolute prediction error
    abs_error <- abs(data$outcome - data$predicted_outcome)
    
    # Determine threshold for truncating the data.
    threshold <- stats::quantile(x=abs_error, probs=1-fraction, na.rm=TRUE)
    
    # Set mask to identify instances that should be kept.
    mask <- abs_error <= threshold[1]
    
    # Filter data to only include instances within the mask.
    data <- data[mask, ]
    
  } else if(metric@robust == "winsor"){
    # Compute absolute prediction error
    abs_error <- abs(data$outcome - data$predicted_outcome)
    
    # Determine threshold for truncating the data.
    threshold <- stats::quantile(x=abs_error, probs=1-fraction, na.rm=TRUE)
    
    # Set mask to identify instances that should be updated.
    mask <- !(abs_error <= threshold[1])
    
    # Update data by winsorising outlier data.
    data[mask & outcome > predicted_outcome, "predicted_outcome":=outcome - threshold]
    data[mask & outcome < predicted_outcome, "predicted_outcome":=outcome + threshold]
  }
  
  return(data)
}

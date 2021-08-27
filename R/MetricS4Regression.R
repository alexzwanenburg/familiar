#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarMetricRegression",
         contains="familiarMetric")


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
           .get_available_explained_variance_metrics()))
} 


#####Mean absolute error########################################################
setClass("familiarMetricMAE",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "Mean Absolute Error",
                                      value_range = c(0.0, Inf),
                                      higher_better = FALSE))


.get_available_mae_metrics <- function() return(c("mae", "mean_absolute_error"))



#####compute_metric_score (Mean absolute error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricMAE"),
          function(metric, data, ...){
            
            # Remove any entries that lack valid predictions.
            data <- remove_nonvalid_predictions(prediction_table=data,
                                                outcome_type=metric@outcome_type)
            
            # Remove any entries that lack observed values.
            data <- remove_missing_outcomes(data=data,
                                            outcome_type=metric@outcome_type)
            
            if(is_empty(data)) return(callNextMethod())
            
            # Compute the mean absolute error.
            score <- sum(abs(data$outcome - data$predicted_outcome)) / nrow(data)
            
            return(score)
          })



#####Mean log absolute error####################################################
setClass("familiarMetricMLAE",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "Mean Log Absolute Error",
                                      value_range = c(0.0, Inf),
                                      higher_better = FALSE))


.get_available_mlae_metrics <- function() return(c("mlae", "mean_log_absolute_error"))



#####compute_metric_score (Mean log absolute error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricMLAE"),
          function(metric, data, ...){
            
            # Remove any entries that lack valid predictions.
            data <- remove_nonvalid_predictions(prediction_table=data,
                                                outcome_type=metric@outcome_type)
            
            # Remove any entries that lack observed values.
            data <- remove_missing_outcomes(data=data,
                                            outcome_type=metric@outcome_type)
            
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


.get_available_mse_metrics <- function() return(c("mse", "mean_squared_error"))



#####compute_metric_score (Mean squared error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricMSE"),
          function(metric, data, ...){
            
            # Remove any entries that lack valid predictions.
            data <- remove_nonvalid_predictions(prediction_table=data,
                                                outcome_type=metric@outcome_type)
            
            # Remove any entries that lack observed values.
            data <- remove_missing_outcomes(data=data,
                                            outcome_type=metric@outcome_type)
            
            if(is_empty(data)) return(callNextMethod())
            
            # Compute the mean squared error.
            score <- sum((data$outcome - data$predicted_outcome)^2) / nrow(data)
            
            return(score)
          })


#####Mean squared log error#########################################################
setClass("familiarMetricMSLE",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "Mean Squared Log Error",
                                      value_range = c(0.0, Inf),
                                      higher_better = FALSE))


.get_available_msle_metrics <- function() return(c("msle", "mean_squared_log_error"))



#####compute_metric_score (Mean squared log error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricMSLE"),
          function(metric, data, ...){
            
            # Remove any entries that lack valid predictions.
            data <- remove_nonvalid_predictions(prediction_table=data,
                                                outcome_type=metric@outcome_type)
            
            # Remove any entries that lack observed values.
            data <- remove_missing_outcomes(data=data,
                                            outcome_type=metric@outcome_type)
            
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


.get_available_medea_metrics <- function() return(c("medae", "median_absolute_error"))



#####compute_metric_score (Median absolute error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricMedianAE"),
          function(metric, data, ...){
            
            # Remove any entries that lack valid predictions.
            data <- remove_nonvalid_predictions(prediction_table=data,
                                                outcome_type=metric@outcome_type)
            
            # Remove any entries that lack observed values.
            data <- remove_missing_outcomes(data=data,
                                            outcome_type=metric@outcome_type)
            
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


.get_available_rmse_metrics <- function() return(c("rmse", "root_mean_square_error"))



#####compute_metric_score (Root mean square error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricRMSE"),
          function(metric, data, ...){
            
            # Remove any entries that lack valid predictions.
            data <- remove_nonvalid_predictions(prediction_table=data,
                                                outcome_type=metric@outcome_type)
            
            # Remove any entries that lack observed values.
            data <- remove_missing_outcomes(data=data,
                                            outcome_type=metric@outcome_type)
            
            if(is_empty(data)) return(callNextMethod())
            
            # Compute the root mean square error.
            score <- sqrt(sum((data$outcome - data$predicted_outcome)^2) / nrow(data))
            
            return(score)
          })



#####Root mean square log error#####################################################
setClass("familiarMetricRMSLE",
         contains="familiarMetricRegression",
         prototype=methods::prototype(name = "Root Mean Square Log Error",
                                      value_range = c(0.0, Inf),
                                      higher_better = FALSE))


.get_available_rmsle_metrics <- function() return(c("rmsle", "root_mean_square_log_error"))



#####compute_metric_score (Root mean square log error)#####
setMethod("compute_metric_score", signature(metric="familiarMetricRMSLE"),
          function(metric, data, ...){
            
            # Remove any entries that lack valid predictions.
            data <- remove_nonvalid_predictions(prediction_table=data,
                                                outcome_type=metric@outcome_type)
            
            # Remove any entries that lack observed values.
            data <- remove_missing_outcomes(data=data,
                                            outcome_type=metric@outcome_type)
            
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


.get_available_r_squared_metrics <- function() return(c("r2_score", "r_squared"))



#####compute_metric_score (R squared)#####
setMethod("compute_metric_score", signature(metric="familiarMetricR2"),
          function(metric, data, ...){
            
            # Remove any entries that lack valid predictions.
            data <- remove_nonvalid_predictions(prediction_table=data,
                                                outcome_type=metric@outcome_type)
            
            # Remove any entries that lack observed values.
            data <- remove_missing_outcomes(data=data,
                                            outcome_type=metric@outcome_type)
            
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


.get_available_explained_variance_metrics <- function() return(c("explained_variance"))



#####compute_metric_score (Explained variance)#####
setMethod("compute_metric_score", signature(metric="familiarMetricExplainedVariance"),
          function(metric, data, ...){
            
            # Remove any entries that lack valid predictions.
            data <- remove_nonvalid_predictions(prediction_table=data,
                                                outcome_type=metric@outcome_type)
            
            # Remove any entries that lack observed values.
            data <- remove_missing_outcomes(data=data,
                                            outcome_type=metric@outcome_type)
            
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

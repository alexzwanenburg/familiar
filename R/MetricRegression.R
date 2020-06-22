.get_available_regression_metrics <- function(){
  return(c("mae", "mean_absolute_error",
           "mlae", "mean_log_absolute_error",
           "mse", "mean_squared_error",
           "msle", "mean_squared_log_error",
           "medae", "median_absolute_error",
           "rmse", "root_mean_square_error",
           "explained_variance",
           "r2_score"))
}

metric.regression.calc <- function(dt, metric, outcome_type){

  if(metric %in% c("mae", "mean_absolute_error")){
    # Mean absolute error
    score <- sum(abs(dt$outcome - dt$predicted_outcome)) / nrow(dt)

  } else if(metric %in% c("mlae", "mean_log_absolute_error")){
    # Mean log absolute error
    score <- sum(log1p(abs(dt$outcome - dt$predicted_outcome))) / nrow(dt)
    
  } else if(metric %in% c("mse", "mean_squared_error")){
    # Mean squared error
    score <- sum((dt$outcome - dt$predicted_outcome)^2) / nrow(dt)

  } else if(metric %in% c("msle", "mean_squared_log_error")){
    # Mean squared logarithmic error
    score <- sum((log1p(dt$outcome) - log1p(dt$predicted_outcome))^2) / nrow(dt)
    
    if(!is.finite(score)){
      score <- as.double(NA)
    }

  } else if(metric %in% c("medae", "median_absolute_error")){
    # Median absolute error
    score <- stats::median(x=abs(dt$outcome - dt$predicted_outcome))

  } else if(metric %in% c("rmse", "root_mean_square_error")){
    # Root mean square error
    score <- sqrt(sum((dt$outcome - dt$predicted_outcome)^2) / nrow(dt))

  } else if (metric %in% c("r2_score", "explained_variance")){
    
    if(metric == "r2_score"){
      # R2-score (coefficient of determination)
      y_mean <- mean(dt$outcome)
      nom    <- sum((dt$outcome - dt$predicted_outcome)^2)
      denom  <- sum((dt$outcome - y_mean)^2)
      
    } else if(metric == "explained_variance"){
      # Explained variance
      y_err_mean <- mean(dt$outcome-dt$predicted_outcome)
      y_mean <- mean(dt$outcome)
      nom   <- sum((dt$outcome - dt$predicted_outcome - y_err_mean)^2)
      denom <- sum((dt$outcome - y_mean)^2)
      
    } else {
      ..error_reached_unreachable_code("metric.regression.calc.unavailable_variance_method")
    }

    # Check if denominator is not 0.
    if(!denom==0){
      # Denominator not 0
      score <- 1.0 - nom / denom
    } else if (nom==denom) {
      # Denominator = nominator = 0
      score <- 1.0
    } else {
      # Denominator = 0. Would be -inf otherwise.
      score <- as.double(NA)
    }
  } else {
    ..error_reached_unreachable_code("metric.regression.calc.unknown_metric")
  }

  return(score)
}


metric.regression.default_range <- function(metric){
  if(metric %in% c("mae", "mean_absolute_error", "mlae", "mean_log_absolute_error", "mse",
                   "mean_squared_error", "msle", "mean_squared_log_error", "medae",
                   "median_absolute_error", "rmse", "root_mean_square_error")){
    # For these metrics the best score is 0.0. Higher scores indicate worse
    # model fits.
    default_range <- c(Inf, 0.0)
    
  } else if(metric %in% c("r2_score", "explained_variance")){
    # For these metrics the best score is 1.0. There is no lower bound.
    default_range <- c(-Inf, 1.0)
  }
  
  return(default_range)
}



metric.regression.objective_range <- function() {
  # Range of objective values for regression metrics. Note that 1.0 is best, 0.0
  # is the value produced by an intercept-only model, and -1.0 is a cutoff for
  # worse scores.
  return(c(-1, 1))
}



metric.regression.outcome <- function(metric, outcome_type){
  if(metric %in% .get_available_regression_metrics() & outcome_type %in% c("continuous", "count")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}



metric.regression.to_objective <- function(metric, score, score_mean, outcome_type){
  # Converts the metric to a objective function representation for maximisation.

  if(!is.finite(score)) { return(as.double(NA)) }

  if(metric %in% c("mae", "mean_absolute_error", "mlae", "mean_log_absolute_error", "mse",
                   "mean_squared_error", "msle", "mean_squared_log_error", "medae",
                   "median_absolute_error", "rmse", "root_mean_square_error")){
    # For these metrics, the best score is 0, and worse scores are higher than
    # 0.

    # Avoid division by 0.
    if(score_mean==0.0) { score_mean <- 1.0 }

    # Map scores to [-inf, 1), with 1 being optimal (score=0)
    obj_score <- 1.0 - score / score_mean

    # Limit range to (-1,1). 0.0 is reached when the model produces a score
    # equal to an intercept model merely fitting the mean.
    if(obj_score < -1.0){ obj_score <- -1.0 }

  } else if(metric %in% c("r2_score", "explained_variance")){
    # These metrics have an optimal score of 1 by themselves.

    # Copy score to obj_score
    obj_score <- score

    # Limit range to (-1,1). 0.0 is reached when the model produces a score
    # equal to an intercept model merely fitting the mean.
    if(obj_score < -1.0){ obj_score <- -1.0 }
  }

  return(obj_score)
}


metric.regression.higher_better <- function(metric){
  if(metric %in% c("mae", "mean_absolute_error", "mlae", "mean_log_absolute_error",
                   "mse", "mean_squared_error", "msle", "mean_squared_log_error",
                   "medae", "median_absolute_error", "rmse", "root_mean_square_error")){
    return(FALSE)
    
  } else {
    return(TRUE)
  }
}

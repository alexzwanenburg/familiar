.get_available_bootstrap_confidence_interval_methods <- function(){
  return(c("bc", "percentile"))
}


.compute_bootstrap_ci <- function(x0, xb, target_column, bootstrap_ci_method="bc",
                                  additional_splitting_variable=NULL,
                                  confidence_level=0.95){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  bootstrap_id <- NULL
  
  # Perform some simple consistency checks.
  if(is_empty(x0)) return(x0)
  if(is_empty(xb)) return(x0)
  
  if(!(data.table::is.data.table(x0) & data.table::is.data.table(xb))){
    ..error_reached_unreachable_code(".compute_bootstrap_ci: both x0 and xb should be data.tables.")
  }
  
  if(!target_column %in% colnames(x0) | !target_column %in% colnames(xb)){
    ..error_reached_unreachable_code(".compute_bootstrap_ci: target column does not appear in the data.tables.")
  }
  
  if(!is.null(additional_splitting_variable)){
    if(!all(additional_splitting_variable %in% colnames(x0))){
      ..error_reached_unreachable_code(".compute_bootstrap_ci: not all splitting variables were found in the x0 data.table.")
    }
  }
  
  # Set splitting variables.
  splitting_variables <- c("data_set", "learner", "fs_method", "pos_class", "eval_time", additional_splitting_variable)
  
  # Add a bootstrap_id column to facilitate merger with bootstrap data.
  x0[, "bootstrap_id":=0L]
  
  # Combine datasets.
  data <- data.table::rbindlist(list(x0, xb), use.names=TRUE)
  
  # Select only splitting variables that appear in data.
  splitting_variables <- intersect(colnames(data), splitting_variables)
  
  # Split the data and compute confidence intervals.
  data <- lapply(split(data, by=splitting_variables, keep.by=TRUE),
                 function(data, target_column, confidence_level, bootstrap_ci_method){
                   
                   # Select point estimate and bootstrap estimates.
                   x0 <- data[bootstrap_id == 0, ][[target_column]]
                   xb <- data[bootstrap_id != 0, ][[target_column]]
                   
                   if(length(x0) != 1) ..error_reached_unreachable_code(".compute_bootstrap_ci: no, or more than one point estimate in split.")
                   
                   # Compute bootstrap confidence interval for the current
                   # split.
                   ci_data <- ..bootstrap_ci(x=xb,
                                             x_0=x0,
                                             confidence_level=confidence_level,
                                             bootstrap_ci_method=bootstrap_ci_method)
                   
                   # Make a copy and drop the bootstrap_id column.
                   output_data <- data.table::copy(data[bootstrap_id == 0, ])[, "bootstrap_id":=NULL]
                   
                   # Replace the value in the target column by the median in
                   # ci_data.
                   output_data[, (target_column):=ci_data$median]
                   
                   # Add confidence interval boundaries.
                   output_data[, ":="("ci_low"=ci_data$ci_low,
                                      "ci_up"=ci_data$ci_up)]
                   
                   return(output_data)
                 },
                 target_column=target_column,
                 confidence_level=confidence_level,
                 bootstrap_ci_method=bootstrap_ci_method)
  
  # Combine into a single dataset
  data <- data.table::rbindlist(data, use.names=TRUE)
  
  return(data)
}



..bootstrap_ci <- function(x, x_0=NULL, confidence_level=0.95, bootstrap_ci_method="percentile"){
  
  # Test significance level alpha.
  if(confidence_level >= 1.0){
    stop("A 100% confidence interval does not exist.")
  } else if(confidence_level <= 0.0){
    stop("The confidence interval cannot be smaller than 0.")
  }
  
  # Define empty summary list.
  empty_list <- list("median"=NA_real_,
                     "ci_low"=NA_real_,
                     "ci_up"=NA_real_)
  
  # Select finite values.
  x <- x[is.finite(x)]
  
  if(length(x) == 0) return(empty_list)
  
  # If no x_0 is provided, the percentile method should be used.
  if(length(x_0) == 0) bootstrap_ci_method <- "percentile"
  
  if(bootstrap_ci_method == "percentile"){
    # Follows the percentile method of Efron, B. & Hastie, T. Computer Age
    # Statistical Inference. (Cambridge University Press, 2016).
    
    # Define percentiles based on the alpha level..
    percentiles <- c((1.0 - confidence_level) / 2.0,
                     1.0 - (1.0 - confidence_level) / 2.0)
    
    # Compute percentiles within the data set
    percentile_values <- stats::quantile(x, probs=percentiles, names=FALSE)
    
    # Generate a summary list
    summary_list <- list("median"=stats::median(x),
                         "ci_low"=percentile_values[1],
                         "ci_up"=percentile_values[2])
    
  } else if(bootstrap_ci_method == "bc"){
    # Follows the bias-corrected (BC) method of Efron, B. & Hastie, T. Computer
    # Age Statistical Inference. (Cambridge University Press, 2016).
    if(length(x_0) > 1){
      stop(paste0("The full-data estimate should have length 1. Found: length ", length(x_0), "."))
    } 
    
    if(!is.finite(x_0)) return(empty_list)
    
    # Compute the bias-correction value. In absence of bias, z_0 equals 0.
    z_0 <- stats::qnorm(sum(x <= x_0) / length(x))
    
    if(!is.finite(z_0)){
      if(all(x == x_0)){
        return(list("median"=x_0,
                    "ci_low"=x_0,
                    "ci_up"=x_0))
        
      } else{
        return(empty_list)
      }
    } 
    
    # Define the z-statistic for bounds of the confidence interval.
    z_alpha <- stats::qnorm(c((1.0 - confidence_level) / 2.0,
                              1.0 - (1.0 - confidence_level) / 2.0))
    
    # Define bias-corrected percentiles.
    percentiles <- stats::pnorm((2.0 * z_0 + z_alpha))
    
    # Compute percentiles within the data set
    percentile_values <- stats::quantile(x, probs=percentiles, names=FALSE)
    
    # Generate a summary list
    summary_list <- list("median"=x_0,
                         "ci_low"=percentile_values[1],
                         "ci_up"=percentile_values[2])
  }
  
  
  return(summary_list)
}

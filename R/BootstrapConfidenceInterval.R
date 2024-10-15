.get_available_bootstrap_confidence_interval_methods <- function() {
  return(c("bc", "percentile"))
}



..bootstrap_ci <- function(
    x,
    x_0 = NULL,
    confidence_level = NULL,
    percentiles = NULL,
    bootstrap_ci_method = "percentile",
    ...
) {
  
  # Test confidence level
  if (!is.null(confidence_level)) {
    if (confidence_level >= 1.0) {
      ..error(
        "A 100% confidence interval does not exist.",
        error_class = "input_argument_error"
      )
      
    } else if (confidence_level <= 0.0) {
      ..error(
        "The confidence interval cannot be smaller than 0.",
        error_class = "input_argument_error"
      )
    }
    
    # Compute percentiles from confidence level, and add the median.
    percentiles <- c(
      0.5,
      (1.0 - confidence_level) / 2.0,
      1.0 - (1.0 - confidence_level) / 2.0
    )
  }
  
  if (!is.null(percentiles)) {
    if (any(percentiles > 1.0)) {
      ..error(
        "Percentiles cannot be greater than 1.",
        error_class = "input_argument_error"
      )
      
    } else if (any(percentiles < 0.0)) {
      ..error(
        "Percentiles cannot be smaller than 0.",
        error_class = "input_argument_error"
      )
    }
  }
  
  if (is.null(confidence_level) && is.null(percentiles)) {
    ..error_reached_unreachable_code(
      "..bootstrap_ic: confidence_interval and percentiles arguments cannot both be NULL."
    )
  }
  
  # For values that are not numeric, return mode. Note that the name "median" is
  # used for compatibility purposes.
  if (!is.numeric(x)) return(list("median" = get_mode(x)))
  
  # Set up percentile names.
  if (!is.null(confidence_level)) {
    # When a confidence level is provided, use standard naming.
    percentile_names <- c("median", "ci_low", "ci_up")
    
  } else {
    # Select unique percentiles as a precaution.
    percentiles <- unique(percentiles)
    
    # When confidence intervals are not provided, use naming based on the
    # percentiles. Determine the number of decimal digits that should be used to
    # parse the numbers (minimum 2).
    percentile_names_digits <- nchar(sub(".*\\.", "", percentiles))
    percentile_names_digits <- max(c(percentile_names_digits, 2L))
    
    # Set percentile names by passing them through the column name
    # checker.
    percentile_names <- .replace_illegal_column_name(
      paste0("q_", format(percentiles, nsmall = percentile_names_digits))
    )
  }
  
  # Create an empty list.
  empty_list <- as.list(rep(NA_real_, length(percentile_names)))
  names(empty_list) <- percentile_names
  
  # Select finite values.
  x <- x[is.finite(x)]
  
  if (length(x) == 0L) return(empty_list)
  
  # If no x_0 is provided, the percentile method should be used.
  if (length(x_0) == 0L) bootstrap_ci_method <- "percentile"
  
  if (bootstrap_ci_method == "percentile") {
    # Follows the percentile method of Efron, B. & Hastie, T. Computer Age
    # Statistical Inference. (Cambridge University Press, 2016).
   
    # Compute percentiles within the data set
    percentile_values <- stats::quantile(
      x,
      probs = percentiles,
      names = FALSE
    )
    
    # Generate a summary list
    summary_list <- as.list(percentile_values)
    names(summary_list) <- percentile_names
    
  } else if (bootstrap_ci_method == "bc") {
    # Follows the bias-corrected (BC) method of Efron, B. & Hastie, T. Computer
    # Age Statistical Inference. (Cambridge University Press, 2016).
    if (length(x_0) > 1L) {
      ..error(
        "The full-data point estimate should have length 1. Found: length ", length(x_0), "."
      )
    } 
    
    if (!is.finite(x_0)) return(empty_list)
    
    # Compute the bias-correction value. In absence of bias, z_0 equals 0.
    z_0 <- stats::qnorm(sum(x <= x_0) / length(x))
    
    if (!is.finite(z_0)) {
      # This means that we are dealing with an edge case where sum(x <= x_0) is
      # either 0 or the length of x. In that case we determine the confidence
      # interval with the percentile method.
      summary_list <- ..bootstrap_ci(
        x = x,
        x_0 = x_0,
        confidence_level = confidence_level,
        percentiles = percentiles,
        bootstrap_ci_method = "percentile"
      )
      
      return(summary_list)
    }
    
    # Define the z-statistic for bounds of the confidence interval.
    z_alpha <- stats::qnorm(percentiles)
    
    # Define bias-corrected percentiles.
    bc_percentiles <- stats::pnorm(2.0 * z_0 + z_alpha)
    
    # Compute percentiles within the data set
    percentile_values <- stats::quantile(
      x,
      probs = bc_percentiles,
      names = FALSE
    )
    
    # Generate a summary list
    summary_list <- as.list(percentile_values)
    names(summary_list) <- percentile_names
  }
  
  return(summary_list)
}


..bootstrap_bias_correction <- function(x) {
  
  # Define empty summary list.
  empty_list <- list("median" = NA_real_)
  
  # Select finite values.
  x <- x[is.finite(x)]
  
  if (length(x) == 0L) return(empty_list)
  
  # Compute the median value over the bootstraps.
  if (is.numeric(x)) {
    summary_list <- list("median" = stats::median(x))
    
  } else if (is.ordered(x)) {
    # Determine the mean average risk group. This requires discretisation
    # as rounding toward the nearest group would overinflate center groups.
    x_levels <- levels(x)
    n <- nlevels(x)
    
    # Discretise bins floor((mu - 1) / ((n-1) / n)) + 1. See fixed bin size
    # discretisation.
    x_num <- floor(n * (mean(as.numeric(x), na.rm = TRUE) - 1.0) / (n - 1.0)) + 1.0
    
    # Check if the x_num still falls within the range.
    x_num <- ifelse(x_num > n, n, x_num)
    
    # Re-encode and add to list.
    summary_list <- list(
      "median" = factor(x_levels[x_num], levels = x_levels, ordered = TRUE)
    )
    
  } else {
    summary_list <- list("median" = get_mode(x))
  }
  
  return(summary_list)
}

.get_available_stratification_methods <- function() {
  return(c("median", "fixed", "optimised", "mean", "mean_trim", "mean_winsor"))
}



.find_survival_grouping_thresholds <- function(object, data) {
  if (!object@outcome_type %in% c("survival")) {
    ..error_reached_unreachable_code(paste0(
      ".find_survival_grouping_thresholds: only available for ",
      "survival outcome. Found: ", object@outcome_type))
  }

  # Load settings to find survival thresholds
  settings <- get_settings()

  # Set time_max
  time_max <- settings$eval$time_max

  # Generate prediction table
  prediction_table <- .predict(
    object = object,
    data = data,
    allow_recalibration = TRUE,
    time = time_max)
browser()
  # Check if any predictions are valid.
  if (!any_predictions_valid(
    prediction_table,
    outcome_type = object@outcome_type)) {
    return(NULL)
  }

  # Remove data with missing predictions.
  prediction_table <- remove_nonvalid_predictions(
    prediction_table,
    outcome_type = object@outcome_type)

  km_info_list <- list()

  # Iterate over stratification methods
  for (cut_off_method in settings$eval$strat_method) {
    if (cut_off_method == "median") {
      # Identify threshold
      cutoff <- .find_quantile_threshold(
        object = object,
        prediction_table = prediction_table,
        quantiles = 0.5)
      
    } else if (cut_off_method == "fixed") {
      # Identify thresholds
      cutoff <- .find_quantile_threshold(
        object = object,
        prediction_table = prediction_table,
        quantiles = settings$eval$strat_quant_threshold)
      
    } else if (cut_off_method == "optimised") {
      # Identify threshold
      cutoff <- .find_maxstat_threshold(prediction_table = prediction_table)
      
    } else if (cut_off_method %in% c("mean", "mean_winsor", "mean_trim")) {
      # Identify threshold
      cutoff <- .find_mean_threshold(
        object = object,
        prediction_table = prediction_table,
        method = cut_off_method)
      
    } else {
      ..error_reached_unreachable_code(paste0(
        ".find_survival_grouping_thresholds: encountered an unknown ",
        "threshold type:", cut_off_method))
    }

    # Find corresponding sizes of the generated groups
    risk_group <- .apply_risk_threshold(
      object = object,
      predicted_values = prediction_table$predicted_outcome,
      cutoff = cutoff)
    
    group_size <- .get_risk_group_sizes(risk_group = risk_group)

    # Populate method_list
    method_list <- list(
      "method" = cut_off_method,
      "cutoff" = cutoff,
      "group_size" = group_size)

    # Attach method_list to the general km_info_list
    km_info_list[[cut_off_method]] <- method_list
  }

  # Add stratification methods
  out_list <- list(
    "stratification_method" = settings$eval$strat_method,
    "parameters" = km_info_list,
    "time_max" = time_max)

  return(out_list)
}



.find_mean_threshold <- function(object, prediction_table, method) {
  # Select finite values.
  x <- prediction_table$predicted_outcome[is.finite(prediction_table$predicted_outcome)]

  if (method == "mean_trim") x <- trim(x)
  if (method == "mean_winsor") x <- winsor(x)

  return(mean(x))
}



.find_quantile_threshold <- function(object, prediction_table, quantiles) {
  
  if (get_prediction_type(object = object) %in% c("expected_survival_time")) {
    # For time-like predictions, we should use the complements of the provided
    # quantiles.
    quantiles <- abs(1 - quantiles)
  }
  # Order quantiles in ascending order
  quantiles <- quantiles[order(quantiles)]

  # Return threshold values
  return(stats::quantile(
    x = prediction_table$predicted_outcome,
    probs = quantiles,
    names = FALSE,
    na.rm = TRUE))
}



.find_maxstat_threshold <- function(prediction_table) {
  # Check whether the model always predicted the same value
  if (stats::var(prediction_table$predicted_outcome) == 0) {
    return(prediction_table$predicted_outcome[1])
  }

  require_package(
    x = "maxstat",
    purpose = "to determine an optimal risk threshold")

  # Perform maxstat test
  h <- tryCatch(
    maxstat::maxstat.test(
      survival::Surv(outcome_time, outcome_event) ~ predicted_outcome,
      data = prediction_table,
      smethod = "LogRank",
      minprop = 0.10,
      maxprop = 0.90),
    error = identity)

  # Check that maxstat.test did not produce an error.
  if (inherits(h, "error")) {
    return(mean(prediction_table$predicted_outcome, na.rm = TRUE))
  }

  # Check if at least 4 unique values are present for the smoothing spline
  if (length(h$cuts) < 4) {
    return(unname(h$estimate))
  }

  # Smoothed scores
  spline_fit <- tryCatch(
    stats::smooth.spline(x = h$cuts, y = h$stats)$fit,
    error = identity)

  # Capture error.
  if (inherits(spline_fit, "error")) {
    return(unname(h$estimate))
  }

  # Predict scores on a fine grid
  x_sample_cuts <- seq(
    from = min(h$cuts),
    to = max(h$cuts),
    length.out = 100)
  
  test_scores <- stats::predict(
    object = spline_fit, 
    x = x_sample_cuts)$y

  return(x_sample_cuts[which.max(test_scores)])
}



.get_risk_group_sizes <- function(risk_group) {
  # Suppress NOTES due to non-standard evaluation in data.table
  indicated_group <- NULL

  # Find group sizes
  group_table <- data.table::data.table("indicated_group" = risk_group)
  group_table <- group_table[, list("group_size" = .N), by = "indicated_group"][order(indicated_group)]

  # Get group sizes and set names
  group_sizes <- group_table$group_size / length(risk_group)
  names(group_sizes) <- group_table$indicated_group

  return(group_sizes)
}



.apply_risk_threshold <- function(object, predicted_values, cutoff) {
  # Initialise risk group
  risk_group <- rep.int(1, times = length(predicted_values))

  # Determine inversion. We assume that risk groups go from 1 (low risk) to k
  # (high risk), with k-1 being the number of provided cutoff values.
  invert <- !get_prediction_type(object = object) %in% .get_available_risklike_prediction_types()

  # Iterate over cutoffs and define risk groups
  for (current_cutoff in cutoff) {
    if (invert) {
      risk_group <- risk_group + as.numeric(predicted_values < current_cutoff)
    } else {
      risk_group <- risk_group + as.numeric(predicted_values >= current_cutoff)
    }
  }

  # Convert to factor
  risk_group <- .assign_risk_group_names(
    risk_group = risk_group,
    cutoff = cutoff)

  # Replace non-finite predicted values by NA.
  risk_group[!is.finite(predicted_values)] <- NA

  # Return risk groups
  return(risk_group)
}



.assign_risk_group_names <- function(risk_group, cutoff) {
  # Determine the number of risk groups
  n_groups <- length(cutoff) + 1

  if (n_groups == 2) {
    # Stratification into low and high-risk groups
    y <- factor(
      x = risk_group, 
      levels = seq_len(n_groups), 
      labels = c("low", "high"), 
      ordered = TRUE)
    
  } else if (n_groups == 3) {
    # Stratification into low, moderate and high-risk groups
    y <- factor(
      x = risk_group,
      levels = seq_len(n_groups),
      labels = c("low", "moderate", "high"),
      ordered = TRUE)
    
  } else {
    # Assign numbers
    y <- factor(
      x = risk_group, 
      levels = seq_len(n_groups),
      ordered = TRUE)
  }

  return(y)
}



get_mean_risk_group <- function(risk_group) {
  # Determine the mean average risk group. This requires discretisation
  # as rounding toward the nearest group would overinflate center groups.
  group_names <- levels(risk_group)
  n <- length(group_names)

  # Discretise bins floor((mu - 1) / ((n-1) / n)) + 1. See fixed bin size
  # discretisation.
  risk_group_num <- floor(n * (mean(as.numeric(risk_group), na.rm = TRUE) - 1) / (n - 1)) + 1

  # Check if the risk_group_num still falls within the range
  risk_group_num <- ifelse(risk_group_num > n, n, risk_group_num)

  return(factor(
    x = group_names[risk_group_num],
    levels = group_names, 
    ordered = TRUE))
}

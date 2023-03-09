#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R

as_metric <- function(
    metric,
    object = NULL,
    outcome_type = NULL,
    ...) {
  
  # Find the outcome type
  if (is.null(outcome_type)) {
    if (
      is(object, "familiarModel") ||
      is(object, "familiarEnsemble") ||
      is(object, "familiarVimpMethod")) {
      outcome_type <- object@outcome_type
    }
  }

  if (metric %in% .get_available_auc_roc_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricAUCROC",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricAUCROC", ...)))
    
  } else if (metric %in% .get_available_brier_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricBrier",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricBrier", ...)))
    
  } else if (metric %in% .get_available_accuracy_metrics()) {
    metric_object <- do.call(
      methods::new, 
      args = c(
        list(
          "Class" = "familiarMetricAccuracy",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricAccuracy", ...)))
    
  } else if (metric %in% .get_available_balanced_accuracy_metrics()) {
    metric_object <- do.call(
      methods::new, 
      args = c(
        list(
          "Class" = "familiarMetricBalancedAccuracy",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricBalancedAccuracy", ...)))
    
  } else if (metric %in% .get_available_balanced_error_rate_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricBalancedErrorRate",
          "metric" = metric,
          "outcome_type" = outcome_type
        ),
        .sanitise_dots("familiarMetricBalancedErrorRate", ...)))
    
  } else if (metric %in% .get_available_cohen_kappa_metrics()) {
    metric_object <- do.call(
      methods::new, 
      args = c(
        list(
          "Class" = "familiarMetricCohenKappa",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricCohenKappa", ...)))
    
  } else if (metric %in% .get_available_f1_score_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricF1Score",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricF1Score", ...)))
    
  } else if (metric %in% .get_available_fdr_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricFDR",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricFDR", ...)))
    
  } else if (metric %in% .get_available_informedness_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricInformedness",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricInformedness", ...)))
    
  } else if (metric %in% .get_available_markedness_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricMarkedness",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricMarkedness", ...)))
    
  } else if (metric %in% .get_available_mcc_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricMCC",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricMCC", ...)))
    
  } else if (metric %in% .get_available_npv_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricNPV",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricNPV", ...)))
    
  } else if (metric %in% .get_available_ppv_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricPPV",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricPPV", ...)))
    
  } else if (metric %in% .get_available_sensitivity_metrics()) {
    metric_object <- do.call(
      methods::new, 
      args = c(
        list(
          "Class" = "familiarMetricSensitivity",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricSensitivity", ...)))
    
  } else if (metric %in% .get_available_specificity_metrics()) {
    metric_object <- do.call(
      methods::new, 
      args = c(
        list(
          "Class" = "familiarMetricSpecificity",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricSpecificity", ...)))
    
  } else if (metric %in% .get_available_youden_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricYouden",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricYouden", ...)))
    
  } else if (metric %in% .get_available_mae_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricMAE",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricMAE", ...)))
    
  } else if (metric %in% .get_available_rae_metrics()) {
    metric_object <- do.call(
      methods::new, 
      args = c(
        list(
          "Class" = "familiarMetricRAE",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricRAE", ...)))
    
  } else if (metric %in% .get_available_mlae_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricMLAE",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricMLAE", ...)))
    
  } else if (metric %in% .get_available_mse_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricMSE",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricMSE", ...)))
    
  } else if (metric %in% .get_available_rse_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricRSE",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricRSE", ...)))
    
  } else if (metric %in% .get_available_msle_metrics()) {
    metric_object <- do.call(
      methods::new, 
      args = c(
        list(
          "Class" = "familiarMetricMSLE",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricMSLE", ...)))
    
  } else if (metric %in% .get_available_medea_metrics()) {
    metric_object <- do.call(
      methods::new, 
      args = c(
        list(
          "Class" = "familiarMetricMedianAE",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricMedianAE", ...)))
    
  } else if (metric %in% .get_available_rmse_metrics()) {
    metric_object <- do.call(
      methods::new, 
      args = c(
        list(
          "Class" = "familiarMetricRMSE",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricRMSE", ...)))
    
  } else if (metric %in% .get_available_rrse_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricRRSE",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricRRSE", ...)))
    
  } else if (metric %in% .get_available_rmsle_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricRMSLE",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricRMSLE", ...)))
    
  } else if (metric %in% .get_available_r_squared_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricR2",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots("familiarMetricR2", ...)))
    
  } else if (metric %in% .get_available_explained_variance_metrics()) {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetricExplainedVariance",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots(
          "familiarMetricExplainedVariance", ...)))
    
  } else if (metric %in% .get_available_concordance_index_harrell()) {
    metric_object <- do.call(
      methods::new, 
      args = c(
        list(
          "Class" = "familiarMetricConcordanceIndexHarrell",
          "metric" = metric,
          "outcome_type" = outcome_type,
          "object" = object),
        .sanitise_dots(
          "familiarMetricConcordanceIndexHarrell", ...)))
    
  } else {
    metric_object <- do.call(
      methods::new,
      args = c(
        list(
          "Class" = "familiarMetric",
          "metric" = metric,
          "outcome_type" = outcome_type),
        .sanitise_dots(
          "familiarMetric", ...)))
  }
  
  return(metric_object)
}



# is_available -----------------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarMetric"),
  function(object, ...) {
    return(FALSE)
  }
)



# is_higher_better -------------------------------------------------------------
setMethod(
  "is_higher_better",
  signature(metric = "familiarMetric"),
  function(metric, ...) {
    return(metric@higher_better)
  }
)



# is_higher_better -------------------------------------------------------------
setMethod(
  "is_higher_better",
  signature(metric = "character"),
  function(metric, ...) {
    # Create metric objects.
    metric_object_list <- lapply(
      metric,
      function(metric, dots) {
        return(do.call(
          as_metric,
          args = c(
            list("metric" = metric),
            dots)))
      },
      dots = list(...))

    # Check that the metrics are available.
    if (!all(sapply(metric_object_list, is_available))) {
      stop(paste0(
        "is_higher_better: the following metrics are not available for ",
        metric_object_list[[1]]@outcome_type, " outcomes: ",
        paste_s(metric[!sapply(metric_object_list, is_available)])))
    }

    # Determine which metrics have a higher value that is better.
    higher_better_flags <- lapply(metric_object_list, is_higher_better)

    # Set metric names.
    names(higher_better_flags) <- metric

    # Return flags.
    return(unlist(higher_better_flags))
  }
)



# compute_metric_score (metric object) -----------------------------------------
setMethod(
  "compute_metric_score",
  signature(metric = "familiarMetric"),
  function(metric, data, ...) {
    # This is a fall-back method. This method should be defined for all
    # subclasses.
    return(NA_real_)
  }
)



# compute_metric_score (character) ---------------------------------------------
setMethod(
  "compute_metric_score",
  signature(metric = "character"),
  function(metric, data, object, ...) {
    if (!is(object, "familiarModel") && !is(object, "familiarEnsemble")) {
      stop(paste0(
        "compute_metric_score: object should be a familiarModel ",
        "or familiarEnsemble object."))
    }

    # Create metric objects.
    metric_object_list <- lapply(
      metric,
      as_metric,
      object = object)

    # Check that the metrics are available.
    if (!all(sapply(metric_object_list, is_available))) {
      stop(paste0(
        "compute_metric_score: the following metrics are not available for ",
        object@outcome_type, " outcomes: ",
        paste_s(metric[!sapply(metric_object_list, is_available)])))
    }

    # Create prediction table, if one is absent.
    if (is(data, "dataObject")) {
      data <- do.call(
        .predict,
        args = c(
          list(
            "object" = object,
            "data" = data),
          list(...)))
    }

    # Compute metric values.
    metric_values <- lapply(
      metric_object_list,
      function(metric, data, object, dots) {
        do.call(
          compute_metric_score,
          args = c(
            list(
              "metric" = metric,
              "data" = data,
              "object" = object),
            dots))
      },
      data = data,
      object = object,
      dots = list(...)
    )
    
    # Set names.
    names(metric_values) <- metric

    return(unlist(metric_values))
  }
)



# compute_objective_score ------------------------------------------------------
setMethod(
  "compute_objective_score",
  signature(metric = "familiarMetric"),
  function(metric, data = NULL, value = NULL, ...) {
    # Check that a baseline value was set
    if (is.null(metric@baseline_value)) {
      # Set the baseline value.
      metric <- set_metric_baseline_value(
        metric = metric,
        data = data, ...)

      # Check again
      if (is.null(metric@baseline_value)) {
        ..error_reached_unreachable_code(
          "compute_objective_score: baseline_value was not set.")
      } 
    }

    # Compute the value, if not provided.
    if (is.null(value)) {
      value <- compute_metric_score(
        metric = metric,
        data = data)
    }

    # Get the baseline_value
    baseline_value <- metric@baseline_value

    if (!is.finite(baseline_value)) {
      # Set a NA value for the objective.
      objective_value <- NA_real_
      
    } else {
      # Determine the optimal value, i.e. the best value attainable.
      optimal_value <- ifelse(
        is_higher_better(metric),
        max(metric@value_range),
        min(metric@value_range))

      # If the baseline value is already perfect, use a small offset instead.
      if (baseline_value == optimal_value) {
        baseline_value <- ifelse(
          is_higher_better(metric),
          optimal_value - 1E-5,
          optimal_value + 1E-5)
      }

      # Compute the objective_value
      objective_value <- ifelse(
        is_higher_better(metric),
        (value - baseline_value) / (optimal_value - baseline_value),
        (baseline_value - value) / (baseline_value - optimal_value)
      )
    }

    # Ensure that all objective scores fall in the [-1, 1] range.
    if (!is.finite(objective_value)) {
      objective_value <- NA_real_
    } else if (objective_value < -1.0) {
      objective_value <- -1.0
    } else if (objective_value > 1.0) {
      ..error_reached_unreachable_code(paste0(
        "compute_objective_score: objective value exceeds the maximum of 1.0: ",
        objective_value))
    }

    return(objective_value)
  }
)



# set_metric_baseline_value ----------------------------------------------------
setMethod(
  "set_metric_baseline_value",
  signature(metric = "familiarMetric"),
  function(metric, object = NULL, data) {
    # Obtain or create
    if (
      is(object, "familiarModel") ||
      is(object, "familiarVimpMethod") ||
      is(object, "familiarEnsemble")) {
      outcome_info <- object@outcome_info
      
    } else if (is(data, "dataObject")) {
      
      if (is(data@outcome_info, "outcomeInfo")) {
        outcome_info <- object@outcome_info
        
      } else {
        # Compute outcome information from scratch.
        outcome_info <- create_outcome_info_from_data(data = data@data)
        outcome_info <- .compute_outcome_distribution_data(
          object = outcome_info,
          data = data@data)
      }
      
    } else if (data.table::is.data.table(data)) {
      # Compute outcome information from scratch.
      outcome_info <- create_outcome_info_from_data(data = data)
      outcome_info <- .compute_outcome_distribution_data(
        object = outcome_info,
        data = data)
      
    } else {
      ..error_reached_unreachable_code(paste0(
        "set_metric_baseline_value: baseline_value could not be set ",
        "using the provided data."))
    }

    # Get a placeholder prediction table.
    prediction_table <- get_placeholder_prediction_table(
      object = outcome_info,
      data = data)

    # We need to identify the data source for determining baseline values.
    if (metric@outcome_type %in% c("binomial", "multinomial")) {
      # Get the frequency table and find the class with the majority.
      frequency_table <- outcome_info@distribution$frequency
      majority_class <- frequency_table$outcome[which.max(frequency_table$count)]

      # Fill the prediction_table.
      prediction_table[, "predicted_class" := majority_class]

      # Define probabilities columns
      outcome_probability_columns <- get_class_probability_name(object)

      for (ii in seq_along(outcome_probability_columns)) {
        # Update the predicted probabilities with 1.0 for the majority
        # class and 0.0 for minority classes.
        if (outcome_probability_columns[ii] == get_class_probability_name(majority_class)) {
          prediction_table[, (outcome_probability_columns[ii]) := 1.0]
        } else {
          prediction_table[, (outcome_probability_columns[ii]) := 0.0]
        }
      }
      
    } else if (metric@outcome_type %in% c("count", "continuous")) {
      # Baseline median value.
      median_value <- outcome_info@distribution$median

      # Fill the prediction_table.
      prediction_table[, "predicted_outcome" := median_value]
      
    } else if (metric@outcome_type %in% c("survival")) {
      # Median baseline survival
      if (!is.null(outcome_info@distribution$survival_probability)) {
        mean_survival_probability <- sum(c(
          min(outcome_info@distribution$survival_probability$survival_probability),
          max(outcome_info@distribution$survival_probability$survival_probability)
        )) / 2.0
        
      } else {
        mean_survival_probability <- NA_real_
      }

      # Fill the prediction_table.
      prediction_table[, "predicted_outcome" := mean_survival_probability]
      
    } else {
      ..error_outcome_type_not_implemented(metric@outcome_type)
    }

    # Compute metric value
    metric@baseline_value <- compute_metric_score(
      metric = metric,
      data = prediction_table)

    # Check the baseline value is a finite value. If it isn't set the value to
    # the extreme value of the range.
    if (!is.finite(metric@baseline_value)) {
      metric@baseline_value <- ifelse(
        is_higher_better(metric),
        min(metric@value_range),
        max(metric@value_range))
    }

    return(metric)
  }
)



.check_metric_outcome_type <- function(
    metric,
    object = NULL, 
    outcome_type = NULL,
    as_flag = FALSE) {
  
  # Obtain outcome_type
  if (is.null(outcome_type) && !is.null(object)) {
    outcome_type <- object@outcome_type
  }

  # Initialise metric
  metric_object <- as_metric(
    metric = metric,
    outcome_type = outcome_type)

  # Check if the metric is available.
  metric_available <- is_available(metric_object)

  if (as_flag) return(metric_available)

  # Check if the metric is available.
  if (!is_subclass(class(metric_object)[1], "familiarMetric")) {
    stop(paste0(
      metric, " is not a valid metric. ",
      "Please check the vignette for available performance metrics."))
    
  } else if (!metric_available) {
    stop(paste0(
      "The ", metric, " metric is not available for ",
      outcome_type, " outcomes."))
  }
  
  return(invisible(TRUE))
}



.get_metric_default_range <- function(
    metric,
    object = NULL,
    outcome_type = NULL) {
  # Get default range of metric scores, e.g. for plotting metric values.

  # Obtain outcome_type
  if (is.null(outcome_type) && !is.null(object)) {
    outcome_type <- object@outcome_type
  }

  # Initialise metric object.
  metric_object <- as_metric(
    metric = metric,
    outcome_type = outcome_type)

  return(metric_object@value_range)
}



metric.compute_optimisation_score <- function(
    score_table,
    optimisation_function,
    replace_na = TRUE) {
  # Compute an optimisation score from validation and training scores. This
  # optimisation score is typically computed for each set of hyperparameters
  # (param_id) and subsample (run_id).
  #
  # For hyperparameter optimisation scores are aggregated as follows:
  # validation and training scores --> optimisation score --> summary score.

  # Suppress NOTES due to non-standard evaluation in data.table
  optimisation_score <- training <- validation <- NULL

  # Select the correct optimisation function.
  optimisation_fun <- switch(optimisation_function,
    "max_validation" = metric.optim_score.max_validation,
    "validation" = metric.optim_score.max_validation,
    "balanced" = metric.optim_score.balanced,
    "stronger_balance" = metric.optim_score.stronger_balance,
    "validation_minus_sd" = metric.optim_score.max_validation,
    "validation_25th_percentile" = metric.optim_score.max_validation,
    "model_estimate" = metric.optim_score.max_validation,
    "model_estimate_minus_sd" = metric.optim_score.max_validation,
    "model_balanced_estimate" = metric.optim_score.balanced,
    "model_balanced_estimate_minus_sd" = metric.optim_score.balanced)

  # Find identifier columns.
  id_columns <- intersect(
    colnames(score_table),
    c("param_id", "run_id"))

  # Create formula
  formula <- stats::reformulate(
    termlabels = "data_set",
    response = paste0(c(id_columns, "metric"), collapse = " + "))

  # Cast objective score wide by data_set.
  optimisation_table <- data.table::dcast(
    data = score_table[, mget(c(id_columns, "metric", "data_set", "objective_score"))],
    formula,
    value.var = "objective_score")

  # Compute optimisation score based on objective scores.
  optimisation_table <- optimisation_table[, list(
    "optimisation_score" = optimisation_fun(
      training = training,
      validation = validation)),
    by = c(id_columns, "metric")]
  
  # Replace NA entries with the minimum optimisation score.
  if (replace_na) {
    optimisation_table[
      is.na(optimisation_score),
      optimisation_score := ..get_replacement_optimisation_score()]
  }

  # Average optimisation score over metrics.
  optimisation_table <- optimisation_table[, list(
    "optimisation_score" = mean(optimisation_score, na.rm = TRUE)),
    by = id_columns]

  return(optimisation_table)
}



metric.summarise_optimisation_score <- function(
    score_table,
    method,
    replace_na = TRUE) {
  # Compute a summary score either directly from optimisation scores, or using a
  # model. This optimisation score is typically computed for each set of
  # hyperparameters.
  #
  # For hyperparameter optimisation scores are aggregated as follows:
  # validation and training scores --> optimisation score --> summary score.

  # Suppress NOTES due to non-standard evaluation in data.table
  optimisation_score <- NULL

  # Find identifier columns.
  id_columns <- intersect(
    colnames(score_table),
    "param_id")

  # Obtain the aggregation method.
  aggregation_method <- switch(method,
    "improvement_empirical_probability" = stats::median,
    "improvement_probability" = mean,
    "expected_improvement" = mean,
    "upper_confidence_bound" = mean,
    "bayes_upper_confidence_bound" = mean,
    "median" = stats::median,
    "mean" = mean,
    "max" = max,
    "min" = min)

  # Compute the mean optimisation score, overall, or per parameter id.
  score_table <- score_table[, list(
    "optimisation_score" = aggregation_method(optimisation_score, na.rm = TRUE)),
    by = id_columns]

  # Replace NA entries with the minimum optimisation score.
  if (replace_na) {
    score_table[
      is.na(optimisation_score), 
      optimisation_score := ..get_replacement_optimisation_score()]
  }

  return(score_table)
}



metric.optim_score.max_validation <- function(training = NULL, validation) {
  return(validation)
}



metric.optim_score.balanced <- function(training, validation) {
  # Start with the validation score.
  value <- validation

  # Penalise by difference between training and validation.
  value <- value - abs(validation - training)

  # Check that the value is finite.
  if (!is.finite(value)) return(value)

  # Add penalty term to models that perform worse than naive models on the
  # training data, i.e. have a objective score below 0.0. We could also write
  # value + training, but I think this way its clearer that a penalty is
  # intended.
  if (training < 0.0) value <- value - abs(training)

  return(value)
}



metric.optim_score.stronger_balance <- function(training, validation) {
  # Start with the validation score.
  value <- validation

  # Penalise by difference between training and validation.
  value <- value - 2.0 * abs(validation - training)

  # Check that the value is finite.
  if (!is.finite(value)) return(value)

  # Add penalty term to models that perform worse than naive models on the
  # training data, i.e. have a objective score below 0.0. We could also write
  # value + training, but I think this way its clearer that a penalty is
  # intended.
  if (training < 0.0) value <- value - 5.0 * abs(training)

  return(value)
}



.get_available_optimisation_functions <- function(hyperparameter_learner = NULL) {
  # All optimisation functions.
  all_optimisation_functions <- c(
    "validation", "max_validation", "balanced", "stronger_balance",
    "validation_minus_sd", "validation_25th_percentile", "model_estimate",
    "model_estimate_minus_sd", "model_balanced_estimate", "model_balanced_estimate_minus_sd"
  )

  if (is.null(hyperparameter_learner)) {
    return(all_optimisation_functions)
    
  } else if (hyperparameter_learner %in% c("random", "random_search")) {
    # Random search does not return an estimate that can be used for
    # optimisation.
    return(setdiff(
      all_optimisation_functions,
      c("model_estimate", "model_estimate_minus_sd", "model_balanced_estimate",
        "model_balanced_estimate_minus_sd")))
  }

  return(all_optimisation_functions)
}



..get_replacement_optimisation_score <- function() {
  return(-9.0)
}



.get_all_metrics <- function() {
  # Returns a list of all metrics.
  
  metrics <- c(
    .get_available_auc_roc_metrics(),
    .get_available_brier_metrics(),
    .get_available_confusion_matrix_metrics(),
    .get_available_regression_metrics(),
    .get_available_concordance_index_metrics()
  )
  
  return(metrics)
}



.get_default_metric <- function(outcome_type) {
  if (outcome_type %in% c("binomial", "multinomial")) {
    default_metric <- "auc_roc"
  } else if (outcome_type == "continuous") {
    default_metric <- "mse"
  } else if (outcome_type == "count") {
    default_metric <- "msle"
  } else if (outcome_type == "survival") {
    default_metric <- "concordance_index"
  } else if (outcome_type == "competing_risk") {
    ..error_outcome_type_not_implemented(outcome_type)
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }
  
  return(default_metric)
}

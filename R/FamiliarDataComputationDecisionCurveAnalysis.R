#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

# familiarDataElementDecisionCurve object --------------------------------------

setClass(
  "familiarDataElementDecisionCurve",
  contains = "familiarDataElement",
  prototype = methods::prototype(
    value_column = "net_benefit",
    grouping_column = "threshold_probability"))

# exract_decision_curve_data (generic) -----------------------------------------

#'@title Internal function to extract decision curve analysis data.
#'
#'@description Computes decision curve analysis data from a `familiarEnsemble`
#'  object. Calibration tests are performed based on expected (predicted) and
#'  observed outcomes. For all outcomes, calibration-at-the-large and
#'  calibration slopes are determined. Furthermore, for all but survival
#'  outcomes, a repeated, randomised grouping Hosmer-Lemeshow test is performed.
#'  For survival outcomes, the Nam-D'Agostino and Greenwood-Nam-D'Agostino tests
#'  are performed.
#'
#'@inheritParams extract_data
#'
#'@return A list with data.tables containing calibration test information for
#'  the ensemble model.
#'@md
#'@keywords internal
setGeneric(
  "extract_decision_curve_data",
  function(
    object,
    data,
    cl = NULL,
    ensemble_method = waiver(),
    evaluation_times = waiver(),
    detail_level = waiver(),
    estimation_type = waiver(),
    aggregate_results = waiver(),
    confidence_level = waiver(),
    bootstrap_ci_method = waiver(),
    is_pre_processed = FALSE,
    message_indent = 0L,
    verbose = FALSE,
    ...) {
    standardGeneric("extract_decision_curve_data")
  }
)


# extract_decision_curve_data (familiarEnsemble) -------------------------------
setMethod(
  "extract_decision_curve_data",
  signature(object = "familiarEnsemble"),
  function(
    object,
    data,
    cl = NULL,
    ensemble_method = waiver(),
    evaluation_times = waiver(),
    detail_level = waiver(),
    estimation_type = waiver(),
    aggregate_results = waiver(),
    confidence_level = waiver(),
    bootstrap_ci_method = waiver(),
    is_pre_processed = FALSE,
    message_indent = 0L,
    verbose = FALSE,
    ...) {
    
    # Decision curve analysis is only available for categorical and
    # survival outcomes.
    if (object@outcome_type %in% c("count", "continuous")) return(NULL)
    
    # Message extraction start
    logger.message(
      paste0("Computing data for decision curve analysis."),
      indent = message_indent,
      verbose = verbose)
    
    # Load evaluation_times from the object settings attribute, if it is not provided.
    if (is.waive(evaluation_times)) evaluation_times <- object@settings$eval_times
    
    # Check evaluation_times argument
    if (object@outcome_type %in% c("survival")) {
      sapply(
        evaluation_times,
        .check_number_in_valid_range,
        var_name = "evaluation_times",
        range = c(0.0, Inf),
        closed = c(FALSE, TRUE))
    }
    
    # Obtain ensemble method from stored settings, if required.
    if (is.waive(ensemble_method)) ensemble_method <- object@settings$ensemble_method
    
    # Check ensemble_method argument
    .check_parameter_value_is_valid(
      x = ensemble_method,
      var_name = "ensemble_method",
      values = .get_available_ensemble_prediction_methods())
    
    # Load confidence alpha from object settings attribute if not
    # provided externally.
    if (is.waive(confidence_level)) confidence_level <- object@settings$confidence_level
    
    # Check confidence_level input argument
    .check_number_in_valid_range(
      x = confidence_level,
      var_name = "confidence_level",
      range = c(0.0, 1.0),
      closed = c(FALSE, FALSE))
    
    # Load the bootstrap method
    if (is.waive(bootstrap_ci_method)) bootstrap_ci_method <- object@settings$bootstrap_ci_method
    
    .check_parameter_value_is_valid(
      x = bootstrap_ci_method,
      var_name = "bootstrap_ci_method",
      values = .get_available_bootstrap_confidence_interval_methods())
    
    # Check the level detail.
    detail_level <- .parse_detail_level(
      x = detail_level,
      object = object,
      default = "hybrid",
      data_element = "decision_curve_analyis")
    
    # Check the estimation type.
    estimation_type <- .parse_estimation_type(
      x = estimation_type,
      object = object,
      default = "bootstrap_confidence_interval",
      data_element = "decision_curve_analyis",
      detail_level = detail_level,
      has_internal_bootstrap = TRUE)
    
    # Check whether results should be aggregated.
    aggregate_results <- .parse_aggregate_results(
      x = aggregate_results,
      object = object,
      default = TRUE,
      data_element = "decision_curve_analyis")
    
    # Test if models are properly loaded
    if (!is_model_loaded(object = object)) ..error_ensemble_models_not_loaded()
    
    # Generate a prototype data element.
    proto_data_element <- new(
      "familiarDataElementDecisionCurve",
      detail_level = detail_level,
      estimation_type = estimation_type,
      confidence_level = confidence_level,
      bootstrap_ci_method = bootstrap_ci_method)
    
    # Generate elements to send to dispatch.
    dca_data <- extract_dispatcher(
      FUN = .extract_decision_curve_data,
      has_internal_bootstrap = TRUE,
      cl = cl,
      object = object,
      data = data,
      proto_data_element = proto_data_element,
      is_pre_processed = is_pre_processed,
      ensemble_method = ensemble_method,
      evaluation_times = evaluation_times,
      aggregate_results = aggregate_results,
      message_indent = message_indent + 1L,
      verbose = verbose)
    
    return(dca_data)
  }
)



.extract_decision_curve_data <- function(
    object,
    data,
    proto_data_element,
    evaluation_times = NULL,
    cl,
    ensemble_method,
    is_pre_processed,
    ...) {
  
  # Ensure that the object is loaded
  object <- load_familiar_object(object)
  
  # Add model name.
  proto_data_element <- add_model_name(proto_data_element, object = object)
  
  # Add evaluation time as a identifier to the data element.
  if (length(evaluation_times) > 0 && object@outcome_type == "survival") {
    data_elements <- add_data_element_identifier(
      x = proto_data_element,
      evaluation_time = evaluation_times)
    
  } else if (object@outcome_type %in% c("binomial", "multinomial")) {
    
    # Predict class probabilities.
    prediction_data <- .predict(
      object = object,
      data = data,
      ensemble_method = ensemble_method,
      is_pre_processed = is_pre_processed)
    
    # Check if any predictions are valid.
    if (!all_predictions_valid(
      prediction_data,
      outcome_type = object@outcome_type)) {
      return(NULL)
    }
    
    # Remove data with missing outcomes.
    prediction_data <- remove_missing_outcomes(
      data = prediction_data,
      outcome_type = object@outcome_type)
    
    # Check that any prediction data remain.
    if (is_empty(prediction_data)) return(NULL)
    
    # Determine class levels
    outcome_class_levels <- get_outcome_class_levels(object)
    
    # Select only one outcome class for binomial outcomes.
    if (object@outcome_type == "binomial") outcome_class_levels <- outcome_class_levels[2]
    
    # Add positive class as an identifier.
    data_elements <- add_data_element_identifier(
      x = proto_data_element,
      positive_class = outcome_class_levels)
    
  } else {
    data_elements <- list(proto_data_element)
  }
  
  # Dispatch to function
  if (object@outcome_type %in% c("binomial", "multinomial")) {
    dca_data <- lapply(
      data_elements,
      .compute_dca_data_categorical,
      data = prediction_data,
      cl = cl,
      ...)
    
  } else if (object@outcome_type %in% c("survival")) {
    dca_data <- lapply(
      data_elements,
      .compute_dca_data_survival,
      data = data,
      object = object,
      ensemble_method = ensemble_method,
      is_pre_processed = is_pre_processed,
      cl = cl,
      ...)
    
  } else {
    ..error_outcome_type_not_implemented(object@outcome_type)
  }
  
  return(dca_data)
}



.compute_dca_data_categorical <- function(
    data_element,
    data,
    aggregate_results,
    cl = NULL,
    progress_bar = FALSE,
    verbose = FALSE,
    message_indent = 0L,
    ...) {
  
  # Check if the data has more than 1 row.
  if (nrow(data) <= 1) return(NULL)
  
  if (length(data_element@identifiers$positive_class) > 0 && progress_bar) {
    logger.message(
      paste0(
        "Computing decision curves for the \"",
        data_element@identifiers$positive_class, "\" class."),
      indent = message_indent,
      verbose = verbose)
  }
  
  # Set test probabilities
  threshold_probabilities <- seq(
    from = 0.000, 
    to = 1.000,
    by = 0.005)
  
  # Add bootstrap data.
  bootstrap_data <- add_data_element_bootstrap(x = data_element, ...)
  
  # Iterate over elements.
  data_elements <- fam_mapply(
    cl = cl,
    assign = NULL,
    FUN = .compute_dca_data_categorical_model,
    data_element = bootstrap_data$data_element,
    bootstrap = bootstrap_data$bootstrap,
    bootstrap_seed = bootstrap_data$seed,
    MoreArgs = list(
      "data" = data,
      "threshold_probabilities" = threshold_probabilities),
    progress_bar = progress_bar,
    chopchop = TRUE)
  
  # Merge data elements
  data_elements <- merge_data_elements(data_elements)
  
  if (aggregate_results) data_elements <- .compute_data_element_estimates(x = data_elements)
  
  return(data_elements)
}



.compute_dca_data_categorical_model <- function(
    data_element,
    data,
    threshold_probabilities,
    bootstrap,
    bootstrap_seed) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- probability <- is_positive <- NULL
  
  # Get the positive class.
  positive_class <- data_element@identifiers$positive_class
  
  # Bootstrap the data.
  if (bootstrap) {
    data <- get_bootstrap_sample(
      data = data,
      seed = bootstrap_seed)
  } 
  
  # Make a local copy
  data <- data.table::copy(data)
  data.table::setnames(
    x = data,
    old = get_class_probability_name(positive_class),
    new = "probability")
  
  # Determine positive output.
  data[, "is_positive" := outcome == positive_class]
  
  # Keep only probability and is_positive
  data <- data[, c("probability", "is_positive")]
  
  # Order by inverse probability.
  data <- data[order(-probability)]
  
  # Determine the number of samples.
  n <- nrow(data)
  
  # Compute intervention data.
  # Determine maximum number of true and false positives.
  n_max_true_positive <- sum(data$is_positive)
  n_max_false_positive <- sum(!data$is_positive)
  
  # Compute benefit for the situation an intervention always happens.
  intervention_net_benefit <- n_max_true_positive / n - 
    n_max_false_positive / n * (threshold_probabilities / (1.0 - threshold_probabilities))
  
  # Copy data element for intervention.
  intervention_data_element <- data_element
  
  # Set the data attribute.
  intervention_data_element@data <- data.table::data.table(
    "threshold_probability" = threshold_probabilities,
    "net_benefit" = intervention_net_benefit)
  
  # Set the curve type identifier.
  intervention_data_element <- add_data_element_identifier(
    x = intervention_data_element,
    curve_type = "intervention_all")
  
  # Determine the number of true and false positives to determine the net
  # benefit of the model.
  data[, ":="(
    "n_true_positive" = cumsum(is_positive),
    "n_false_positive" = cumsum(!is_positive))]
  
  # Compute benefit for the model.
  model_net_benefit <- ..compute_dca_data_net_benefit(data, threshold_probabilities)
  
  if (is.null(model_net_benefit)) return(NULL)
  
  # Set the data attribute
  data_element@data <- data.table::data.table(
    "threshold_probability" = threshold_probabilities,
    "net_benefit" = model_net_benefit)
  
  # Set the curve type identifier.
  data_element <- add_data_element_identifier(
    x = data_element,
    curve_type = "model")
  
  return(c(
    data_element,
    intervention_data_element))
}



.compute_dca_data_survival <- function(
    data_element,
    data,
    object,
    ensemble_method,
    aggregate_results,
    is_pre_processed,
    cl = NULL,
    progress_bar = FALSE,
    verbose = FALSE,
    message_indent = 0L,
    ...) {
  
  # Predict survival probabilities.
  data <- .predict(
    object = object,
    data = data,
    time = data_element@identifiers$evaluation_time,
    ensemble_method = ensemble_method,
    is_pre_processed = is_pre_processed,
    type = "survival_probability")
  
  # Check if any predictions are valid.
  if (!all_predictions_valid(
    data,
    outcome_type = object@outcome_type)) {
    return(NULL)
  }
  
  # Remove data with missing outcomes.
  data <- remove_missing_outcomes(
    data = data,
    outcome_type = object@outcome_type)
  
  # Check that any prediction data remain.
  if (is_empty(data)) return(NULL)
  
  # Check if the data has more than 1 row.
  if (nrow(data) <= 1) return(NULL)
  
  # Message the user concerning the time at which the decision curves are
  # computed. This is only relevant for survival analysis, where survival
  # probability is time depend.
  if (length(data_element@identifiers$evaluation_time) > 0 && progress_bar) {
    logger.message(
      paste0(
        "Computing decision curves at time ", 
        data_element@identifiers$evaluation_time, "."),
      indent = message_indent,
      verbose = verbose)
  }
  
  # Set test probabilities
  threshold_probabilities <- seq(
    from = 0.000,
    to = 1.000,
    by = 0.005)
  
  # Add bootstrap data.
  bootstrap_data <- add_data_element_bootstrap(x = data_element, ...)
  
  # Iterate over elements.
  data_elements <- fam_mapply(
    cl = cl,
    assign = NULL,
    FUN = .compute_dca_data_survival_model,
    data_element = bootstrap_data$data_element,
    bootstrap = bootstrap_data$bootstrap,
    bootstrap_seed = bootstrap_data$seed,
    MoreArgs = list(
      "data" = data,
      "threshold_probabilities" = threshold_probabilities),
    progress_bar = progress_bar,
    chopchop = TRUE)
  
  # Merge data elements
  data_elements <- merge_data_elements(data_elements)
  
  if (aggregate_results) data_elements <- .compute_data_element_estimates(x = data_elements)
  
  return(data_elements)
}


.compute_dca_data_survival_model <- function(
    data_element,
    data,
    threshold_probabilities,
    bootstrap,
    bootstrap_seed) {
  
  # Bootstrap the data.
  if (bootstrap) {
    data <- get_bootstrap_sample(
      data = data,
      seed = bootstrap_seed)
  }
  
  # Compute benefit for the situation an intervention always happens.
  intervention_net_benefit <- ..compute_dca_data_net_benefit_survival(
    data = data,
    x = threshold_probabilities,
    evaluation_time = data_element@identifiers$evaluation_time,
    intervention = TRUE)
  
  # Copy data element for intervention.
  intervention_data_element <- data_element
  
  # Set the data attribute.
  intervention_data_element@data <- data.table::data.table(
    "threshold_probability" = threshold_probabilities,
    "net_benefit" = intervention_net_benefit)
  
  # Set the curve type identifier.
  intervention_data_element <- add_data_element_identifier(
    x = intervention_data_element,
    curve_type = "intervention_all")
  
  # Compute benefit for the model.
  model_net_benefit <- ..compute_dca_data_net_benefit_survival(
    data = data,
    x = threshold_probabilities,
    evaluation_time = data_element@identifiers$evaluation_time,
    intervention = FALSE)
  
  if (is.null(model_net_benefit)) return(NULL)
  
  # Set the data attribute
  data_element@data <- data.table::data.table(
    "threshold_probability" = threshold_probabilities,
    "net_benefit" = model_net_benefit)
  
  # Set the curve type identifier.
  data_element <- add_data_element_identifier(
    x = data_element,
    curve_type = "model")
  
  return(c(
    data_element,
    intervention_data_element))
}



..compute_dca_data_net_benefit <- function(data, x) {
  # Compute net benefit for models.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  n_true_positive <- n_false_positive <- probability <- net_benefit <- NULL
  
  # Determine maximum number of true and false positives.
  n_max_true_positive <- max(data$n_true_positive)
  
  # Determine the number of samples.
  n <- nrow(data)
  
  # Determine net benefit.
  data[, ":="(
    "net_benefit" = n_true_positive / n -
      n_false_positive / n * (probability / (1.0 - probability)))]
  
  # If the predicted probability occurs more than once, select the lowest net
  # benefit.
  data <- data[, list("net_benefit" = min(net_benefit)), by = "probability"]

  # Check if the data has more than 1 row.
  if (nrow(data) <= 1) return(NULL)
  
  # Compute net benefit at the test probabilities.
  net_benefit <- suppressWarnings(stats::approx(
    x = data$probability,
    y = data$net_benefit,
    xout = x,
    yleft = n_max_true_positive / n,
    yright = -Inf,
    method = "linear")$y)
  
  return(net_benefit)
}



..compute_dca_data_net_benefit_survival <- function(
    data,
    x,
    evaluation_time,
    intervention = FALSE) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  survival_probability <- outcome_event <- outcome_time <- NULL
  death <- censored <- n <- NULL
  
  # Prepare net benefit.
  net_benefit <- numeric(length(x))
  
  # Determine the total group size.
  n_group_size <- nrow(data)
  
  # Define probability thresholds that will be evaluated. When the intervention
  # curve is required, only one threshold (0.0) is used.
  if (intervention) {
    p_threshold <- 0.0
    
  } else {
    p_threshold <- x
  }
  
  # We want to avoid running too many computations. Therefore, we will only
  # compute the number of true positives and false positives when the group size
  # changes.
  previous_group_size <- n_group_size + 1
  for (ii in seq_along(p_threshold)) {
    # Select the group of patients
    surv_group <- data.table::copy(data[survival_probability >= x[ii]])
    
    # Get the total group size of the group where predicted survival probability
    # exceeds the threshold probability.
    n_surv_group <- nrow(surv_group)
    
    if (n_surv_group == previous_group_size) {
      # True positive and false positive numbers did not change. This is always
      # skipped in the first iteration, because the previous group size is set
      # to a value that lies beyond the possible survival group size.
      n_true_positive <- n_true_positive
      n_false_positive <- n_false_positive
      
    } else if (n_surv_group == 0) {
      # There are no true and false positives, because there are no positives
      # with survival probability greater than the threshold.
      n_true_positive <- n_false_positive <- 0
      
    } else {
      # Create the basic part of the Kaplan-Meier data by summing the number of
      # deaths and censored patients at the end of each interval. We limit
      # ourselves to those samples that were censored or had an event prior to
      # the evaluation time. The remaining patients do not affect the survival
      # probability.
      surv_group <- surv_group[
        outcome_time <= evaluation_time,
        list(
          "death" = sum(outcome_event == 1),
          "censored" = sum(outcome_event == 0)),
        by = "outcome_time"][order(outcome_time)]
      
      if (nrow(surv_group) > 0) {
        # Add group sizes at the start of each interval.
        surv_group[, "n" := n_surv_group - data.table::shift(
          cumsum(death + censored),
          n = 1,
          fill = 0,
          type = "lag")]
        
        # Compute the probability of survival in the interval
        surv_group[, "survival_in_interval" := (n - death) / n]
        
        # Compute survival probability.
        survival_probability <- prod(surv_group$survival_in_interval)
        
      } else {
        # All samples survived longer than evaluation_time.
        survival_probability <- 1.0
      }
      
      # Compute the number of true and false positives according to Vickers et
      # al. 2008 (10.1186/1147-6947-8-53)
      n_true_positive <- (1.0 - survival_probability) * n_surv_group
      n_false_positive <- survival_probability * n_surv_group
    }
    
    # Update the previous group size.
    previous_group_size <- n_surv_group
    
    # Compute net benefit.
    if (intervention) {
      net_benefit <- n_true_positive / n_group_size - 
        n_false_positive / n_group_size * (x / (1.0 - x))
      
    } else {
      net_benefit[ii] <- n_true_positive / n_group_size - 
        n_false_positive / n_group_size * (p_threshold[ii] / (1.0 - p_threshold[ii]))
      
      if (p_threshold[ii] == 1.0) net_benefit[ii] <- 0.0
    }
  }
  
  return(net_benefit)
}


# export_decision_curve_analysis_data (generic) --------------------------------

#'@title Extract and export decision curve analysis data.
#'
#'@description Extract and export decision curve analysis data in a
#'  familiarCollection.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams as_familiar_collection
#'
#'@details Data is usually collected from a `familiarCollection` object.
#'  However, you can also provide one or more `familiarData` objects, that will
#'  be internally converted to a `familiarCollection` object. It is also
#'  possible to provide a `familiarEnsemble` or one or more `familiarModel`
#'  objects together with the data from which data is computed prior to export.
#'  Paths to the previous files can also be provided.
#'
#'  All parameters aside from `object` and `dir_path` are only used if `object`
#'  is not a `familiarCollection` object, or a path to one.
#'  
#'  Decision curve analysis data is computed for categorical outcomes, i.e.
#'  binomial and multinomial, as well as survival outcomes.
#'
#'@return A list of data.table (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_decision_curve_analysis_data
#'@md
#'@rdname export_decision_curve_analysis_data-methods
setGeneric(
  "export_decision_curve_analysis_data",
  function(
    object,
    dir_path = NULL,
    aggregate_results = TRUE,
    ...) {
    standardGeneric("export_decision_curve_analysis_data")
  }
)



# export_decision_curve_analysis_data (collection) -----------------------------

#'@rdname export_decision_curve_analysis_data-methods
setMethod(
  "export_decision_curve_analysis_data",
  signature(object = "familiarCollection"),
  function(
    object,
    dir_path = NULL,
    aggregate_results = TRUE, 
    ...) {
    
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    return(.export(
      x = object,
      data_slot = "decision_curve_data",
      dir_path = dir_path,
      aggregate_results = aggregate_results,
      type = "decision_curve_analysis",
      subtype = "data"))
  }
)



# export_decision_curve_analysis_data (general) --------------------------------

#'@rdname export_decision_curve_analysis_data-methods
setMethod(
  "export_decision_curve_analysis_data",
  signature(object = "ANY"),
  function(
    object, 
    dir_path = NULL, 
    aggregate_results = TRUE, 
    ...) {
    
    # Attempt conversion to familiarCollection object.
    object <- do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = object,
          "data_element" = "decision_curve_analyis",
          "aggregate_results" = aggregate_results),
        list(...)))
    
    return(do.call(
      export_decision_curve_analysis_data,
      args = c(
        list(
          "object" = object,
          "dir_path" = dir_path,
          "aggregate_results" = aggregate_results),
        list(...))))
  }
)

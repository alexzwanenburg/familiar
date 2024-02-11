#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
#' @include PredictionTable.R
NULL


# familiarDataElementRiskStrata (object) ---------------------------------------
setClass(
  "familiarDataElementRiskStrata",
  contains = "familiarDataElement"
)

# familiarDataElementRiskHazardRatio (object) ----------------------------------
setClass(
  "familiarDataElementRiskHazardRatio",
  contains = "familiarDataElement"
)

# familiarDataElementRiskLogrank (object) --------------------------------------
setClass(
  "familiarDataElementRiskLogrank",
  contains = "familiarDataElement"
)



# extract_risk_stratification_data (generic) -----------------------------------

#'@title Internal function to extract stratification data.
#'
#'@description Computes and extracts stratification data from a
#'  `familiarEnsemble` object. This includes the data required to draw
#'  Kaplan-Meier plots, as well as logrank and hazard-ratio tests between the
#'  respective risk groups.
#'
#'@inheritParams .extract_data
#'
#'@return A list with data.tables containing information concerning risk group
#'  stratification.
#'@md
#'@keywords internal
setGeneric(
  "extract_risk_stratification_data",
  function(
    object,
    data,
    cl = NULL,
    is_pre_processed = FALSE,
    ensemble_method = waiver(),
    detail_level = waiver(),
    confidence_level = waiver(),
    message_indent = 0L,
    verbose = FALSE, 
    ...
  ) {
    standardGeneric("extract_risk_stratification_data")
  }
)



# extract_risk_stratification_data (familiarEnsemble) --------------------------
setMethod(
  "extract_risk_stratification_data",
  signature(object = "familiarEnsemble"),
  function(
    object,
    data,
    cl = NULL,
    is_pre_processed = FALSE,
    ensemble_method = waiver(),
    detail_level = waiver(),
    confidence_level = waiver(),
    stratification_method = waiver(),
    message_indent = 0L,
    verbose = FALSE,
    ...
  ) {
    
    # This mostly follows the same routines as extract_prediction_data. In
    # addition, tests are created during export.
    
    # Only assess stratification for survival outcomes.
    if (!object@outcome_type %in% c("survival")) return(NULL)
    
    # Message extraction start
    logger_message(
      paste0("Assessing stratification into risk groups."),
      indent = message_indent,
      verbose = verbose
    )
    
    # Load confidence alpha from object settings attribute if not provided
    # externally.
    if (is.waive(confidence_level)) {
      confidence_level <- object@settings$confidence_level
    }
    
    # Check confidence_level input argument
    .check_number_in_valid_range(
      x = confidence_level, 
      var_name = "confidence_level",
      range = c(0.0, 1.0),
      closed = c(FALSE, FALSE)
    )
    
    # Obtain ensemble method from stored settings, if required.
    if (is.waive(ensemble_method)) {
      ensemble_method <- object@settings$ensemble_method
    }
    
    # Check ensemble_method argument
    .check_parameter_value_is_valid(
      x = ensemble_method, 
      var_name = "ensemble_method",
      values = .get_available_ensemble_prediction_methods()
    )
    
    # Check the level detail.
    detail_level <- .parse_detail_level(
      x = detail_level,
      object = object,
      default = "ensemble",
      data_element = "risk_stratification_data"
    )
    
    # Test if models are properly loaded
    if (!is_model_loaded(object = object)) ..error_ensemble_models_not_loaded()
    
    # Load models.
    model_list <- ..get_model(object = object)
    if (is_empty(model_list)) return(NULL)
    
    # Check available stratification methods.
    available_stratification_method <- unique(unlist(lapply(
      model_list,
      function(fam_model) (fam_model@km_info$stratification_method)
    )))
    
    # Check that any are available.
    if (is.null(available_stratification_method)) return(NULL)
    
    # Prepare the stratification_method variable for comparison. The default is
    # to use all methods present in the dataset.
    if (is.waive(stratification_method)) {
      stratification_method <- available_stratification_method
    }
    
    # Check that the stratification method is valid.
    .check_parameter_value_is_valid(
      x = stratification_method,
      var_name = "stratification_method",
      values = available_stratification_method
    )

    # Aggregate data. It does not make sense to keep duplicate rows here.
    data <- aggregate_data(data = data)
    
    # Generate a prototype data element.
    proto_data_element <- new(
      "predictionTableRiskGroups",
      detail_level = detail_level,
      confidence_level = confidence_level,
      estimation_type = "point"
    )
    
    # Generate elements to send to dispatch.
    performance_data <- extract_dispatcher(
      FUN = .extract_risk_stratification_data,
      has_internal_bootstrap = FALSE,
      cl = cl,
      object = object,
      data = data,
      aggregate_results = TRUE,
      stratification_method = stratification_method,
      proto_data_element = proto_data_element,
      is_pre_processed = is_pre_processed,
      ensemble_method = ensemble_method,
      time = object@settings$time_max,
      message_indent = message_indent + 1L,
      verbose = verbose
    )
    
    return(performance_data)
  }
)



# extract_risk_stratification_data (prediction table) --------------------------
setMethod(
  "extract_risk_stratification_data",
  signature(object = "predictionTableRiskGroups"),
  function(object, ...) {
    if (is_empty(object)) return(NULL)
    
    return(object)
  }
)




# extract_risk_stratification_data (prediction table) --------------------------
setMethod(
  "extract_risk_stratification_data",
  signature(object = "familiarDataElementPredictionTable"),
  function(object, ...) {
    ..warning_no_data_extraction_from_prediction_table("risk stratification")
    
    return(NULL)
  }
)



.extract_risk_stratification_data <- function(
    object,
    proto_data_element,
    cl = NULL,
    stratification_method,
    ...
) {
  # Add model name.
  proto_data_element <- add_model_name(proto_data_element, object = object)
  
  # Add stratification methods.
  data_elements <- add_data_element_identifier(
    x = proto_data_element,
    stratification_method = stratification_method
  )
  
  # Iterate over data elements.
  data_elements <- lapply(
    data_elements,
    ..extract_risk_stratification_data,
    object = object,
    cl = cl,
    ...
  )
  
  return(data_elements)
}



..extract_risk_stratification_data <- function(
    data_element,
    object,
    data,
    cl = NULL,
    is_pre_processed,
    ensemble_method,
    time,
    ...
) {
  # Ensure that the object is loaded
  object <- load_familiar_object(object)
  
  # Compute performance data.
  prediction_data <- .predict(
    object = object,
    data = data,
    ensemble_method = ensemble_method,
    time = time,
    type = "risk_stratification",
    stratification_method = data_element@identifiers$stratification_method,
    is_pre_processed = is_pre_processed
  )

  if (is_empty(prediction_data)) return(NULL)
  
  # We will actually use prediction_data from here, but need to update some
  # attributes.
  prediction_data@detail_level <- data_element@detail_level
  prediction_data@estimation_type <- data_element@estimation_type
  prediction_data@confidence_level <- data_element@confidence_level
  prediction_data@bootstrap_ci_method <- data_element@bootstrap_ci_method
  prediction_data@is_aggregated <- FALSE
  prediction_data@identifiers <- c(prediction_data@identifiers, data_element@identifiers)
  
  return(prediction_data)
}



.compute_risk_stratification_curves <- function(x, time_range = NULL) {
  
  if (is_empty(x)) return(NULL)
  if (!all_predictions_valid(x)) return(NULL)
  
  if (!x@is_aggregated) {
    ..error_reached_unreachable_code(
      ".compute_risk_stratification_curves: expecting aggregated data.")
  }

  # Make local copy.
  data <- .copy(x)
  
  # Remove data with missing predictions.
  data <- remove_invalid_predictions(data)
  
  # Remove data with missing outcomes.
  data <- filter_missing_outcome(data, outcome_type = "survival")
  
  # Check that any prediction data remain.
  if (is_empty(data)) return(NULL)
  
  # Create new element with strata based on x.
  data_element <- methods::new(
    "familiarDataElementRiskStrata",
    x,
    grouping_column = c(
      setdiff(x@grouping_column, c(get_non_feature_columns(x = "survival"))),
      "group"
    ),
    value_column = "survival"
  )
  
  # Collect strata
  data <- .merge_slots_into_data(data)
  strata <- data@data[, ..compute_risk_stratification_curves(
    .SD, 
    confidence_level = data_element@confidence_level,
    time_range = time_range),
    by = c(data_element@grouping_column),
    .SDcols = c("outcome_time", "outcome_event")
  ]
  
  # Check that strata are not empty.
  if (is_empty(strata)) return(NULL)
  
  # Store strata data.
  data_element@data <- strata
  
  return(data_element)
}



..compute_risk_stratification_curves <- function(x, confidence_level, time_range) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  survival <- ci_low <- ci_up <- time <- NULL
  
  if (is_empty(x)) return(NULL)
  
  # Obtain Kaplan-Meier survival curves.
  km_fit <- tryCatch(
    survival::survfit(
      Surv(outcome_time, outcome_event) ~ 1,
      data = x,
      conf.int = confidence_level
    ),
    error = identity
  )
  
  # Check if the survival curve could be generated at all. Causes could be lack
  # of events, no events beyond the first time point, etc.
  if (inherits(km_fit, "error")) return(NULL)
  
  # Extract plotting information
  km_data <- data.table::data.table(
    "time" = km_fit$time,
    "group_size" = km_fit$n.risk,
    "n_event" = km_fit$n.event,
    "n_censor" = km_fit$n.censor,
    "survival" = km_fit$surv,
    "ci_low" = km_fit$lower,
    "ci_up" = km_fit$upper
  )
  
  # Add in data at time = 0 if necessary.
  if (min(km_data$time) > 0) {
    km_data <- rbind(data.table::data.table(
      "time" = 0.0,
      "group_size" = km_fit$n,
      "n_event" = 0,
      "n_censor" = 0,
      "survival" = 1.00,
      "ci_low" = km_fit$lower[1],
      "ci_up" = 1.0),
      km_data
    )
  }
  
  # Update absent censoring (notably when survival is 0).
  km_data[survival == 0 & is.na(ci_low), "ci_low" := 0.0]
  km_data[survival == 0 & is.na(ci_up), "ci_up" := 0.0]
  
  # In rare circumstances (i.e. single-sample risk groups), ci_low may be
  # missing at the initial time point.
  km_data[time == 0 & is.na(ci_low), "ci_low" := 0.0]
  
  if (!is.null(time_range)) {
    # Add an entry at the proximal and distal range edges. This prevents the
    # curve from being cut off prematurely, or starting too late.
    if (
      is_empty(km_data[time == time_range[1]]) &&
      time_range[1] > min(km_data$time) &&
      time_range[1] < max(km_data$time)
    ) {
      # Select the closest entry prior to the proximal edge, make changes and
      # introduce it back into the data.
      proximal_data <- tail(km_data[time < time_range[1]][order(time)], n = 1L)
      proximal_data[, ":="(
        "time" = time_range[1],
        "n_event" = 0,
        "n_censor" = 0
      )]
      km_data <- rbind(km_data, proximal_data)[order(time)]
    }
    
    if (
      is_empty(km_data[time == time_range[2]]) &&
      time_range[2] > min(km_data$time) &&
      time_range[2] < max(km_data$time)
    ) {
      # Do the same for the distal edge.
      distal_data <- tail(km_data[time < time_range[2]][order(time)], n = 1)
      distal_data[, ":="(
        "time" = time_range[2],
        "n_event" = 0,
        "n_censor" = 0
      )]
      km_data <- rbind(km_data, distal_data)[order(time)]
    }
    
    # Limit strata to the time range.
    km_data <- km_data[time >= time_range[1] & time <= time_range[2]]
  }
  
  return(as.list(km_data))
}



.compute_risk_stratification_tests <- function(x, time_range) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome_time <- NULL
  
  if (is_empty(x)) return(NULL)
  
  if (!x@is_aggregated) {
    ..error_reached_unreachable_code(
      ".compute_risk_stratification_test: expecting aggregated data.")
  }
  
  # Make local copy.
  x <- .copy(x)
  
  # Remove data with missing predictions or missing outcomes.
  x <- remove_invalid_predictions(x)
  x <- filter_missing_outcome(x, outcome_type = "survival")
  
  # Check that any prediction data remain.
  if (is_empty(x)) return(NULL)
  
  # Right-censor the data to the desired time window.
  if (!is.null(time_range)) {
    # Right censor data past the time range.
    x@data[outcome_time > time_range[2], "outcome_event" := 0]
  } 

  # Check that 2 (or more risk groups) are present.
  if (length(x@groups) < 2) return(NULL)
  
  # Get data from logrank tests.
  logrank_data <- .compute_risk_stratification_logrank_test(
    x = x,
    time_range = time_range
  )
  
  # Get data from hazard ratio tests.
  hr_data <- .compute_risk_stratification_hazard_ratio_test(
    x = x,
    confidence_level = x@confidence_level
  )
  
  # Return test results
  return(list(
    "logrank" = logrank_data,
    "hazard_ratio" = hr_data
  ))
}



.compute_risk_stratification_logrank_test <- function(x, time_range) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  p_value <- NULL
  
  # Create new element with strata based on x.
  data_element <- methods::new(
    "familiarDataElementRiskLogrank",
    x,
    grouping_column = c(setdiff(
      x@grouping_column,
      c(get_non_feature_columns(x = "survival"))
    )),
    value_column = c("p_value", "p_value_adjusted")
  )
  
  # Compute logrank test over all risk groups.
  data <- .as_data_table(x)
  overall_results <- data[
    ,
    ..compute_risk_stratification_logrank_test(x = .SD),
    by = c(data_element@grouping_column),
    .SDcols = c("outcome_time", "outcome_event", "group")
  ]
  
  # Get all pairs
  pairs <- utils::combn(
    x = x@groups,
    m = 2L,
    simplify = FALSE
  )
  
  # Compute logrank test results over all risk groups.
  pairwise_results <- data[, dmapply(
    ..compute_risk_stratification_logrank_test,
    selected_groups = pairs,
    MoreArgs = list("x" = .SD)),
    by = c(data_element@grouping_column),
    .SDcols = c("outcome_time", "outcome_event", "group")
  ]
  
  # Compute multiple-testing corrected p-value.
  pairwise_results[
    ,
    "p_value_adjusted" := stats::p.adjust(p_value, method = "holm"),
    by = c(data_element@grouping_column)
  ]
  
  # Combine to single table and return.
  data_element@data <- data.table::rbindlist(
    list(overall_results, pairwise_results), use.names = TRUE, fill = TRUE
  )
  
  return(data_element)
}



..compute_risk_stratification_logrank_test <- function(x, selected_groups = NULL) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  group <- NULL
  
  # Select indicated risk groups and make sure they are both present.
  if (!is.null(selected_groups)) x <- x[group %in% selected_groups]
  
  # Determine the number of groups.
  n_groups <- data.table::uniqueN(x$group)
  
  # Check that 2 (or more risk groups) are present.
  if (n_groups < 2) return(NULL)
  
  # Determine chi-square of log-rank test
  chi_sq <- tryCatch(
    survival::survdiff(
      survival::Surv(time = outcome_time, event = outcome_event) ~ group,
      data = x,
      subset = NULL,
      na.action = "na.omit"
    )$chisq,
    error = identity
  )
  
  # Check if the test statistic could be computed. Causes could be lack of
  # events, no events beyond the first time point, etc.
  if (inherits(chi_sq, "error")) return(NULL)
  
  # Derive  p-value
  p_value  <- stats::pchisq(
    q = chi_sq,
    df = n_groups - 1,
    lower.tail = FALSE
  )
  
  # In case all groups should be compared, add a place-holder name.
  if (is.null(selected_groups)) {
    group_1 <- group_2 <- "all"
    
  } else {
    group_1 <- selected_groups[1]
    group_2 <- selected_groups[2]
  }
  
  # Set data.
  test_data <- data.table::data.table(
    "group_1" = group_1,
    "group_2" = group_2,
    "p_value" = p_value
  )
  
  return(as.list(test_data))
}



.compute_risk_stratification_hazard_ratio_test <- function(x, confidence_level) {
  # Suppress NOTES due to non-standard evaluation in data.table
  p_value <- NULL
  
  # Create new element with strata based on x.
  data_element <- methods::new(
    "familiarDataElementRiskHazardRatio",
    x,
    grouping_column = c(setdiff(
      x@grouping_column,
      get_non_feature_columns(x = "survival")
    )),
    value_column = c("p_value", "p_value_adjusted")
  )
  
  # Isolate data.
  data <- .as_data_table(x)
  data$group <- droplevels(data$group)
  
  # Use the first group as reference for ordinal variables.
  reference_groups <- levels(data$group)[1]
  
  # Compute hazard test results over all risk groups.
  hr_results <- data[
    ,
    dmapply(
      ..compute_risk_stratification_hazard_ratio_test,
      reference_group = reference_groups,
      MoreArgs = list(
        "x" = .SD,
        "confidence_level" = confidence_level
      )
    ),
    by = c(data_element@grouping_column),
    .SDcols = c("outcome_time", "outcome_event", "group")
  ]
  
  # Compute multiple-testing corrected p-value.
  hr_results[
    ,
    "p_value_adjusted" := stats::p.adjust(p_value, method = "holm"),
    by = c(data_element@grouping_column)
  ]
  
  # Store to data element.
  data_element@data <- hr_results
  
  return(data_element)
}



..compute_risk_stratification_hazard_ratio_test <- function(
    x,
    reference_group,
    confidence_level
) {
  
  # Determine the number of groups.
  n_groups <- data.table::uniqueN(x$group)
  
  # Check that 2 (or more risk groups) are present.
  if (n_groups < 2) return(NULL)
  
  # Check if the reference group is present.
  if (!any(x$group == reference_group)) return(NULL)
  
  # Make local copy of x.
  x <- data.table::copy(x)
  
  # Force the group column to be categorical.
  if (is.factor(x$group)) {
    x$group <- droplevels(x$group)
    
  } else {
    x$group <- as.factor(x$group)
  }
  
  # Reset the reference risk group
  if (!is.ordered(x$group)) {
    x$group <- stats::relevel(
      x$group,
      ref = reference_group
    )
  }
  
  # Create Cox proportional hazards model.
  model <- suppressWarnings(tryCatch(
    survival::coxph(
      survival::Surv(time = outcome_time, event = outcome_event) ~ group,
      data = x
    ),
    error = identity
  ))
  
  # Check if the Cox PH model could not be computed. Causes could be lack of
  # events, no events beyond the first time point, etc.
  if (inherits(model, "error")) return(NULL)
  
  # Extract summary information concerning hazard ratios
  summary_info <- summary(model, confint = confidence_level)
  
  # Fill test data.
  test_data <- data.table::data.table(
    "reference_group" = reference_group,
    "group" = levels(x$group)[-1],
    "hazard_ratio" = summary_info$conf.int[, 1],
    "ci_low" = summary_info$conf.int[, 3],
    "ci_up" = summary_info$conf.int[, 4],
    "p_value" = summary_info$coefficients[, 5]
  )
  
  # Return test results
  return(as.list(test_data))
}



# export_risk_stratification_data (generic) ------------------------------------

#'@title Extract and export sample risk group stratification and associated
#'  tests.
#'
#'@description Extract and export sample risk group stratification and
#'  associated tests for data in a familiarCollection.
#'
#'@param export_strata Flag that determines whether the raw data or strata are
#'  exported.
#'@param time_range Time range for which strata should be created. If `NULL`,
#'  the full time range is used.
#'@inheritParams export_all
#'@inheritParams export_univariate_analysis_data
#'
#'@inheritDotParams extract_risk_stratification_data
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
#'  Three tables are exported in a list:
#'
#'  * `data`: Contains the assigned risk group for a given sample, along with
#'  its reported survival time and censoring status.
#'
#'  * `hr_ratio`: Contains the hazard ratio between different risk groups.
#'
#'  * `logrank`: Contains the results from the logrank test between different
#'  risk groups.
#'
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_risk_stratification_data
#'@md
#'@rdname export_risk_stratification_data-methods
setGeneric(
  "export_risk_stratification_data",
  function(
    object,
    dir_path = NULL,
    export_strata = TRUE,
    time_range = NULL,
    export_collection = FALSE,
    ...
  ) {
    standardGeneric("export_risk_stratification_data")
  }
)



# export_risk_stratification_data (collection) ---------------------------------

#'@rdname export_risk_stratification_data-methods
setMethod(
  "export_risk_stratification_data",
  signature(object = "familiarCollection"),
  function(
    object,
    dir_path = NULL,
    export_strata = TRUE,
    time_range = NULL,
    export_collection = FALSE,
    ...
  ) {
    
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    if (!is.null(time_range)) {
      # Check that time_range is valid.
      .check_argument_length(
        time_range, 
        var_name = "time_range",
        min = 2,
        max = 2
      )
      
      sapply(
        time_range,
        .check_number_in_valid_range,
        var_name = "time_range",
        range = c(0, Inf)
      )
    }
    
    if (export_strata) {
      # Compute kaplan-meier curves.
      strata_data <- lapply(
        object@km_data,
        .compute_risk_stratification_curves,
        time_range = time_range
      )
      
      # Determine hazard ratio and logrank tests. We do this here because the
      # data needs to be aggregated.
      test_data <- lapply(
        object@km_data,
        .compute_risk_stratification_tests,
        time_range = time_range
      )
      
      # Export raw data.
      raw_data <- .export(
        x = object,
        data_slot = "km_data",
        dir_path = dir_path,
        aggregate_results = TRUE,
        type = "stratification",
        subtype = "data"
      )
      
      # Export strata.
      strata_data <- .export(
        x = object,
        data_elements = strata_data,
        dir_path = dir_path,
        aggregate_results = TRUE,
        type = "stratification",
        subtype = "strata"
      )
      
      # Export logrank data.
      logrank_data <- .export(
        x = object,
        data_elements = test_data,
        dir_path = dir_path,
        aggregate_results = TRUE,
        object_class = "familiarDataElementRiskLogrank",
        type = "stratification",
        subtype = "logrank"
      )
      
      # Export hazard ratio data.
      hazard_ratio_data <- .export(
        x = object,
        data_elements = test_data,
        dir_path = dir_path,
        aggregate_results = TRUE,
        object_class = "familiarDataElementRiskHazardRatio",
        type = "stratification",
        subtype = "hazard_ratio"
      )
      
      data_list <- list(
        "data" = raw_data,
        "strata" = strata_data,
        "logrank" = logrank_data,
        "hazard_ratio_data" = hazard_ratio_data
      )
      
      if (export_collection) {
        data_list <- c(
          data_list,
          list("collection" = object)
        )
      } 
      
      return(data_list)
      
    } else {
      return(.export(
        x = object,
        data_slot = "km_data",
        dir_path = dir_path,
        aggregate_results = TRUE,
        type = "stratification",
        subtype = "data",
        export_collection = export_collection
      ))
    }
  }
)



# export_risk_stratification_data (general) ------------------------------------

#'@rdname export_risk_stratification_data-methods
setMethod(
  "export_risk_stratification_data",
  signature(object = "ANY"),
  function(
    object,
    dir_path = NULL,
    export_strata = TRUE,
    time_range = NULL,
    export_collection = FALSE,
    ...
  ) {
    
    # Attempt conversion to familiarCollection object.
    object <- do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = object,
          "data_element" = "risk_stratification_data"
        ),
        list(...)
      )
    )
    
    return(do.call(
      export_risk_stratification_data,
      args = c(
        list(
          "object" = object,
          "dir_path" = dir_path,
          "export_strata" = export_strata,
          "export_collection" = export_collection
        ),
        list(...)
      )
    ))
  }
)

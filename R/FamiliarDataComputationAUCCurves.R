#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
#' @include PredictionTable.R
NULL

# familiarDataElementAUCCurve object definition --------------------------------

setClass(
  "familiarDataElementAUCCurve",
  contains = "familiarDataElement",
  prototype = methods::prototype(
    value_column = "y",
    grouping_column = "x"
  )
)



# extract_auc_data (generic) ---------------------------------------------------

#'@title Internal function to extract area under the ROC curve information.
#'
#'@description Computes the ROC curve from a `familiarEnsemble`.
#''
#'@inheritParams .extract_data
#'
#'@details This function also computes credibility intervals for the ROC curve
#'  for the ensemble model, at the level of `confidence_level`. In the case of
#'  multinomial outcomes, an AUC curve is computed per class in a
#'  one-against-all fashion.
#'
#'  To allow plotting of multiple AUC curves in the same plot and the use of
#'  ensemble models, the AUC curve is evaluated at 0.01 (1-specificity) intervals.
#'
#'@return A list with data.tables for single and ensemble model ROC curve data.
#'@md
#'@keywords internal
setGeneric(
  "extract_auc_data",
  function(
    object,
    data,
    cl = NULL,
    ensemble_method = waiver(),
    detail_level = waiver(),
    estimation_type = waiver(),
    aggregate_results = waiver(),
    confidence_level = waiver(),
    bootstrap_ci_method = waiver(),
    is_pre_processed = FALSE,
    message_indent = 0L,
    verbose = FALSE,
    ...
  ) {
    standardGeneric("extract_auc_data")
  }
)



# extract_auc_data (familiarEnsemble) ------------------------------------------
setMethod(
  "extract_auc_data",
  signature(object = "familiarEnsemble"),
  function(
    object,
    data,
    cl = NULL,
    ensemble_method = waiver(),
    detail_level = waiver(),
    estimation_type = waiver(),
    aggregate_results = waiver(),
    confidence_level = waiver(),
    bootstrap_ci_method = waiver(),
    is_pre_processed = FALSE,
    message_indent = 0L,
    verbose = FALSE,
    ...
  ) {
    # Extract data for plotting AUC curves.
    
    # AUC data can only be prepared for binomial and multinomial outcomes
    if (!object@outcome_type %in% c("binomial", "multinomial")) return(NULL)
    
    # Message start of auc computations
    logger_message(
      paste0("Computing receiver-operating characteristic curves."),
      indent = message_indent,
      verbose = verbose
    )
    
    if (is.waive(ensemble_method)) ensemble_method <- object@settings$ensemble_method
    if (is.waive(confidence_level)) confidence_level <- object@settings$confidence_level
    if (is.waive(bootstrap_ci_method)) bootstrap_ci_method <- object@settings$bootstrap_ci_method
    
    # Check whether results should be aggregated.
    aggregate_results <- .parse_aggregate_results(
      x = aggregate_results,
      object = object,
      default = TRUE,
      data_element = "auc_data"
    )
    
    proto_data_element <- .create_extract_auc_data_object(
      object = object,
      ensemble_method = ensemble_method,
      detail_level = detail_level,
      estimation_type = estimation_type,
      confidence_level = confidence_level,
      bootstrap_ci_method = bootstrap_ci_method
    )
    
    # Determine whether a single curve is obtained for point estimates.
    # When more than one model exists, these may be averaged for hybrid
    # estimation types.
    is_single_curve <- detail_level == "ensemble" || length(object@model_list) == 1L
    
    # Generate elements to send to dispatch.
    roc_data <- extract_dispatcher(
      FUN = .extract_roc_curve_data,
      has_internal_bootstrap = TRUE,
      cl = cl,
      object = object,
      data = data,
      proto_data_element = proto_data_element,
      is_pre_processed = is_pre_processed,
      ensemble_method = ensemble_method,
      aggregate_results = aggregate_results,
      is_single_curve = is_single_curve,
      message_indent = message_indent + 1L,
      verbose = verbose
    )
    
    return(roc_data)
  }
)



# extract_auc_data (prediction table) ------------------------------------------

setMethod(
  "extract_auc_data",
  signature(object = "familiarDataElementPredictionTable"),
  function(
    object,
    data,
    cl = NULL,
    ensemble_method = waiver(),
    detail_level = waiver(),
    estimation_type = waiver(),
    aggregate_results = waiver(),
    confidence_level = waiver(),
    bootstrap_ci_method = waiver(),
    is_pre_processed = FALSE,
    message_indent = 0L,
    verbose = FALSE,
    ...
  ) {
    # Extract data for plotting AUC curves.
    
    # AUC data can only be prepared for binomial and multinomial outcomes
    if (!is(object, "predictionTableClassification")) {
      ..warning_no_data_extraction_from_prediction_table("AUC curve data")
      
      return(NULL)
    }

    # Message start of auc computations
    logger_message(
      paste0("Computing receiver-operating characteristic curves."),
      indent = message_indent,
      verbose = verbose
    )
    
    if (is.waive(ensemble_method)) {
      ensemble_method <- "median"
      if (methods::.hasSlot(object, "ensemble_method")) ensemble_method <- object@ensemble_method
    }
    
    # Default Values.
    if (is.waive(detail_level)) detail_level <- "ensemble"
    if (is.waive(estimation_type)) estimation_type <- "bootstrap_confidence_interval" 
    if (is.waive(confidence_level)) confidence_level <- 0.95
    if (is.waive(bootstrap_ci_method)) bootstrap_ci_method <- "bc"
    if (is.waive(aggregate_results)) aggregate_results <- TRUE
    
    # Check whether results should be aggregated.
    aggregate_results <- .parse_aggregate_results(
      x = aggregate_results,
      object = object,
      default = TRUE,
      data_element = "auc_data"
    )
    
    # Copy object to prevent changing the provided object by reference.
    object <- .copy(object)
    
    proto_data_element <- .create_extract_auc_data_object(
      object = object,
      ensemble_method = ensemble_method,
      detail_level = detail_level,
      estimation_type = estimation_type,
      confidence_level = confidence_level,
      bootstrap_ci_method = bootstrap_ci_method
    )
    
    # Determine whether a single curve is obtained for point estimates.
    is_single_curve <- TRUE
    
    # Generate elements to send to dispatch.
    roc_data <- extract_dispatcher(
      FUN = .extract_roc_curve_data,
      has_internal_bootstrap = TRUE,
      cl = cl,
      object = object,
      data = data,
      proto_data_element = proto_data_element,
      is_pre_processed = is_pre_processed,
      ensemble_method = ensemble_method,
      aggregate_results = aggregate_results,
      is_single_curve = is_single_curve,
      message_indent = message_indent + 1L,
      verbose = verbose
    )
    
    return(roc_data)
  }
)



.create_extract_auc_data_object <- function(
    object,
    ensemble_method,
    detail_level,
    estimation_type,
    confidence_level,
    bootstrap_ci_method
) {
  # Check confidence_level input argument.
  .check_number_in_valid_range(
    x = confidence_level, 
    var_name = "confidence_level",
    range = c(0.0, 1.0),
    closed = c(FALSE, FALSE)
  )
  
  # Check bootstrap_ci_method argument.
  .check_parameter_value_is_valid(
    x = bootstrap_ci_method, 
    var_name = "bootstrap_ci_method",
    values = .get_available_bootstrap_confidence_interval_methods()
  )
  
  # Check ensemble_method argument.
  .check_parameter_value_is_valid(
    x = ensemble_method,
    var_name = "ensemble_method",
    values = .get_available_ensemble_prediction_methods()
  )
  
  # Check the level detail.
  detail_level <- .parse_detail_level(
    x = detail_level,
    object = object,
    default = "hybrid",
    data_element = "auc_data"
  )
  
  # Check the estimation type.
  estimation_type <- .parse_estimation_type(
    x = estimation_type,
    object = object,
    default = "bootstrap_confidence_interval",
    data_element = "auc_data",
    detail_level = detail_level,
    has_internal_bootstrap = TRUE
  )
  
  # Generate a prototype data element.
  proto_data_element <- new(
    "familiarDataElementAUCCurve",
    detail_level = detail_level,
    estimation_type = estimation_type,
    confidence_level = confidence_level,
    bootstrap_ci_method = bootstrap_ci_method
  )
  
  return(proto_data_element)
}



.extract_roc_curve_data <- function(
    object,
    proto_data_element,
    is_single_curve,
    cl = NULL,
    ...
) {
  
  # Ensure that the object is loaded
  object <- load_familiar_object(object)
  
  # Add model name.
  proto_data_element <- add_model_name(
    proto_data_element,
    object = object
  )
  
  # Update is_single_curve
  is_single_curve <- proto_data_element@estimation_type == "point" && is_single_curve
  
  if (!object@outcome_type %in% c("binomial", "multinomial")) {
    ..error_outcome_type_not_implemented(object@outcome_type)
  }
  
  return(..extract_roc_curve_data(
    object = object,
    data_element = proto_data_element,
    is_single_curve = is_single_curve,
    cl = cl,
    ...
  ))
}



# ..extract_roc_curve_data (generic) -------------------------------------------
setGeneric(
  "..extract_roc_curve_data",
  function(object, ...) standardGeneric("..extract_roc_curve_data")
)



# ..extract_roc_curve_data (model, ensemble) -----------------------------------
setMethod(
  "..extract_roc_curve_data",
  signature(object = "familiarModelUnion"),
  function(
    object,
    data,
    ensemble_method,
    is_pre_processed,
    ...
  ) {
    
    # Predict class probabilities.
    prediction_data <- .predict(
      object = object,
      data = data,
      ensemble_method = ensemble_method,
      is_pre_processed = is_pre_processed
    )
    
    return(..extract_roc_curve_data(
      object = prediction_data,
      ...
    ))
  }
)



# ..extract_roc_curve_data (classification) ------------------------------------
setMethod(
  "..extract_roc_curve_data",
  signature(object = "predictionTableClassification"),
  function(
    object,
    data_element,
    cl,
    is_single_curve,
    ...
  ) {
    
    # Check if any predictions are valid.
    if (!all_predictions_valid(object)) return(NULL)
    
    # Remove data with missing outcomes.
    object <- filter_missing_outcome(object)
    
    # Check that any prediction data remain.
    if (is_empty(object)) return(NULL)
    
    # Determine class levels
    outcome_class_levels <- get_outcome_class_levels(object)
    
    # Select only one outcome type for binomial outcomes.
    if (object@outcome_type == "binomial") outcome_class_levels <- outcome_class_levels[2]
    
    # Add positive class as an identifier.
    data_elements <- add_data_element_identifier(
      x = data_element,
      positive_class = outcome_class_levels
    )
    
    # Compute ROC data.
    roc_data <- lapply(
      data_elements,
      .compute_auc_data_categorical,
      data = object,
      cl = cl,
      is_single_curve = is_single_curve,
      ...
    )
    
    return(roc_data)
  }
)



.compute_auc_data_categorical <- function(
    data_element,
    data,
    aggregate_results,
    is_single_curve,
    cl = NULL,
    progress_bar = FALSE,
    verbose = FALSE,
    message_indent = 0L,
    ...
) {
  
  # Check if the data has more than 1 row.
  if (nrow(data) <= 1) return(NULL)
  
  if (length(data_element@identifiers$positive_class) > 0 && progress_bar) {
    logger_message(
      paste0(
        "Computing ROC and Precision-Recall curves for the \"",
        data_element@identifiers$positive_class, "\" class."),
      indent = message_indent,
      verbose = verbose)
  }
  
  # Set test probabilities
  threshold_probabilities <- seq(
    from = 0.000,
    to = 1.000,
    by = 0.005
  )
  
  # Add bootstrap data.
  bootstrap_data <- add_data_element_bootstrap(
    x = data_element,
    ...
  )
  
  # Iterate over elements.
  data_elements <- fam_mapply(
    cl = cl,
    assign = NULL,
    FUN = ..compute_auc_data_categorical,
    data_element = bootstrap_data$data_element,
    bootstrap = bootstrap_data$bootstrap,
    bootstrap_seed = bootstrap_data$seed,
    MoreArgs = list(
      "data" = data,
      "x" = threshold_probabilities,
      "is_single_curve" = is_single_curve),
    progress_bar = progress_bar,
    chopchop = TRUE
  )
  
  # Merge data elements
  data_elements <- merge_data_elements(data_elements)
  
  if (aggregate_results) data_elements <- .compute_data_element_estimates(x = data_elements)
  
  return(data_elements)
}


..compute_auc_data_categorical <- function(
    data_element,
    data,
    x,
    is_single_curve,
    bootstrap,
    bootstrap_seed
) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- probability <- ppv <- tpr <- fpr <- is_positive <- NULL
  n_true_positive <- n_false_positive <- sorting_index <- NULL
  
  # Get the positive class.
  positive_class <- data_element@identifiers$positive_class
  
  # Bootstrap the data.
  if (bootstrap) {
    data <- get_bootstrap_sample(
      data = data,
      seed = bootstrap_seed
    )
  }
  
  # Make a local copy
  data <- data.table::copy(.as_data_table(data))
  data.table::setnames(
    x = data,
    old = positive_class,
    new = "probability"
  )
  
  # Determine the number of positive and negative outcomes.
  n_positive <- sum(data$outcome == positive_class)
  n_negative <- sum(data$outcome != positive_class)
  
  # Determine whether the observed outcome was positive.
  data[, "is_positive" := outcome == positive_class]
  
  # Keep only probability and is_positive
  data <- data[, c("probability", "is_positive")]
    
  # Order by inverse probability.
  data <- data[order(-probability)]
  
  # Determine the number of true and false positives.
  data[, ":="(
    "n_true_positive" = cumsum(is_positive),
    "n_false_positive" = cumsum(!is_positive),
    "n_true_negative" = n_negative - cumsum(!is_positive),
    "n_false_negative" = n_positive - cumsum(is_positive)
  )]
  
  # Insert initial values.
  data <- data.table::rbindlist(
    list(
      data.table::data.table(
        "n_true_positive" = 0L,
        "n_false_positive" = 0L,
        "n_true_negative" = n_negative,
        "n_false_negative" = n_positive
      ),
      data
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  # Select unique data.
  data <- unique(
    data,
    by = c(
      "n_true_positive", "n_false_positive",
      "n_true_negative", "n_false_negative"
    )
  )
  
  # Compute TPR / recall (sensitivity)
  if (n_positive > 0) {
    data[, "tpr" := n_true_positive / n_positive]
    
  } else {
    data[, "tpr" := 0.0]
  }
  
  # Compute FPR (1-specificity)
  if (max(data$n_false_positive) > 0) {
    data[, "fpr" := n_false_positive / n_negative]
    
  } else {
    data[, "fpr" := 0.0]
  }
  
  # Compute precision / positive predictive value
  if (max(data$n_true_positive + data$n_false_positive) > 0) {
    data[, "ppv" := n_true_positive / (n_true_positive + n_false_positive)]
    
    # By convention, the initial value is 1.0.
    data[n_true_positive + n_false_positive == 0, "ppv" := 1.0]
  } else {
    data[, "ppv" := 0.0]
  }
  
  # Insert a sorting index.
  data[, "sorting_index" := .I]
  
  # Copy the data element as prototype for ROC curve data.
  data_element_roc <- data_element
  
  # Copy relevant columns.
  data_auc_roc <- data[, mget(c("tpr", "fpr"))]
  
  # Add start and end points.
  data_auc_roc <- data.table::rbindlist(
    list(
      data.table::data.table("tpr" = 0.0, "fpr" = 0.0),
      data_auc_roc,
      data.table::data.table("tpr" = 1.0, "fpr" = 1.0)
    ),
    use.names = TRUE
  )
  
  # Select minimum and maximum sensitivity at each fpr.
  data_auc_roc <- unique(data_auc_roc[, list("tpr" = c(min(tpr), max(tpr))), by = "fpr"])
  
  # Select minimum and maximum fpr at each sensitivity to find edge values.
  data_auc_roc <- unique(data_auc_roc[, list("fpr" = c(min(fpr), max(fpr))), by = "tpr"])
  
  # Merge fpr and tpr with data to get the sorting index.
  data_auc_roc <- merge(
    x = data_auc_roc,
    y = data[, mget(c("tpr", "fpr", "sorting_index"))],
    by = c("tpr", "fpr")
  )
  
  # Fill out potentially missing sorting indices.
  data_auc_roc[tpr == 0.0 & fpr == 0.0 & is.na(sorting_index), "sorting_index" := 0L]
  data_auc_roc[tpr == 1.0 & fpr == 1.0 & is.na(sorting_index), "sorting_index" := n_positive + n_negative + 1L]
  
  # Order data by sorting index.
  data_auc_roc <- data_auc_roc[order(sorting_index)]
  
  if (is_single_curve) {
    # Extract data.
    x <- data_auc_roc$fpr
    y_auc_roc <- data_auc_roc$tpr
    
    # Make sure these curves are not aggregated further.
    data_element_roc@is_aggregated <- TRUE
    
  } else {
    # Interpolate at the values in x. This follows Davis J, Goadrich M. The
    # relationship between Precision-Recall and ROC curves. Proceedings of the
    # 23rd international conference on Machine learning. New York, NY, USA:
    # Association for Computing Machinery; 2006. pp. 233–240.
    y_auc_roc <- suppressWarnings(stats::approx(
      x = data_auc_roc$fpr,
      y = data_auc_roc$tpr,
      xout = x,
      yleft = 0.0,
      yright = 1.0,
      method = "linear",
      ties = "ordered"
    )$y)
  }
  
  # Set ROC curve data.
  data_element_roc@data <- data.table::data.table(
    "x" = x,
    "y" = y_auc_roc
  )
  
  # Add curve type as an identifier.
  data_element_roc <- add_data_element_identifier(
    data_element_roc,
    curve_type = "roc"
  )
  
  # Copy the data element as prototype for precisions-recall curve data.
  data_element_pr <- data_element
  
  # Get relevant columns
  data_auc_pr <- data[, mget(c("tpr", "ppv"))]
  
  # Find edge values.
  data_auc_pr <- unique(data_auc_pr[, list("ppv" = c(min(ppv), max(ppv))), by = "tpr"])
  data_auc_pr <- unique(data_auc_pr[, list("tpr" = c(min(tpr), max(tpr))), by = "ppv"])
  
  # Merge ppv and tpr with data to get the sorting index.
  data_auc_pr <- merge(
    x = data_auc_pr,
    y = data[, mget(c("tpr", "ppv", "sorting_index"))],
    by = c("tpr", "ppv")
  )
  
  # Order data by sorting index.
  data_auc_pr <- data_auc_pr[order(sorting_index)]
  
  if (is_single_curve) {
    # Extract required values.
    x <- data_auc_pr$tpr
    y_auc_pr <- data_auc_pr$ppv
    
    # Make sure these curves are not aggregated further.
    data_element_pr@is_aggregated <- TRUE
    
  } else {
    # Interpolate at the values in x.
    y_auc_pr <- suppressWarnings(stats::approx(
      x = data_auc_pr$tpr,
      y = data_auc_pr$ppv,
      xout = x,
      yleft = 1.0,
      yright = n_positive / (n_positive + n_negative),
      method = "linear",
      ties = "ordered"
    )$y)
  }
  
  # Set PR curve data.
  data_element_pr@data <- data.table::data.table(
    "x" = x,
    "y" = y_auc_pr
  )
  
  # Add curve type as an identifier.
  data_element_pr <- add_data_element_identifier(
    data_element_pr,
    curve_type = "pr"
  )
  
  return(c(
    data_element_roc,
    data_element_pr
  ))
}



# export_auc_data (generic) ----------------------------------------------------

#'@title Extract and export ROC and Precision-Recall curves.
#'
#'@description Extract and export ROC and Precision-Recall curves for models in
#'  a familiarCollection.
#'
#'@inheritParams export_all
#'@inheritParams export_univariate_analysis_data
#'
#'@inheritDotParams extract_auc_data
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
#'  ROC curve data are exported for individual and ensemble models. For ensemble
#'  models, a credibility interval for the ROC curve is determined using
#'  bootstrapping for each metric. In case of multinomial outcomes, ROC-curves
#'  are computed for each class, using a one-against-all approach.
#'
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_auc_data
#'@md
#'@rdname export_auc_data-methods
setGeneric(
  "export_auc_data",
  function(
    object,
    dir_path = NULL,
    aggregate_results = TRUE,
    export_collection = FALSE,
    ...) {
    standardGeneric("export_auc_data")
  }
)



# export_auc_data (collection) -------------------------------------------------

#'@rdname export_auc_data-methods
setMethod(
  "export_auc_data",
  signature(object = "familiarCollection"),
  function(
    object,
    dir_path = NULL,
    aggregate_results = TRUE,
    export_collection = FALSE,
    ...) {
    
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    return(.export(
      x = object,
      data_slot = "auc_data",
      dir_path = dir_path,
      aggregate_results = aggregate_results,
      type = "performance",
      subtype = "auc_curves",
      export_collection = export_collection))
  }
)

# export_auc_data (general) ----------------------------------------------------

#'@rdname export_auc_data-methods
setMethod(
  "export_auc_data",
  signature(object = "ANY"),
  function(
    object,
    dir_path = NULL,
    aggregate_results = TRUE,
    export_collection = FALSE,
    ...) {
    
    # Attempt conversion to familiarCollection object.
    object <- do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = object,
          "data_element" = "auc_data",
          "aggregate_results" = aggregate_results),
        list(...)))
    
    return(do.call(
      export_auc_data,
      args = c(
        list(
          "object" = object,
          "dir_path" = dir_path,
          "aggregate_results" = aggregate_results,
          "export_collection" = export_collection),
        list(...))))
  }
)

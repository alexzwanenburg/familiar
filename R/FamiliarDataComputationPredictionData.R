#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# extract_predictions (generic) ------------------------------------------------

#'@title Internal function to extract predicted values from models.
#'
#'@description Collects predicted values from models in a `familiarEnsemble`.
#'
#'@inheritParams .extract_data
#'
#'@return A list with single-model and ensemble predictions.
#'@md
#'@keywords internal
setGeneric(
  "extract_predictions",
  function(
    object,
    data,
    cl = NULL,
    is_pre_processed = FALSE,
    ensemble_method = waiver(),
    evaluation_times = waiver(),
    detail_level = waiver(),
    estimation_type = waiver(),
    aggregate_results = waiver(),
    confidence_level = waiver(),
    message_indent = 0L,
    verbose = FALSE,
    ...
  ) {
    standardGeneric("extract_predictions")
  }
)



# extract_predictions (familiarEnsemble) ---------------------------------------
setMethod(
  "extract_predictions",
  signature(object = "familiarEnsemble"),
  function(
    object,
    data,
    cl = NULL,
    is_pre_processed = FALSE,
    ensemble_method = waiver(),
    evaluation_times = waiver(),
    detail_level = waiver(),
    estimation_type = waiver(),
    aggregate_results = waiver(),
    confidence_level = waiver(),
    message_indent = 0L,
    verbose = FALSE,
    ...
  ) {
    # Extract predictions from the data using the models in the ensemble. Note:
    # we do not call the predict function on the familiarEnsemble directly as
    # this would cause predict to become highly convoluted, in particular with
    # generating both single and ensemble predictions.
    
    # Message extraction start
    logger_message(
      paste0("Computing ensemble predictions for the dataset."),
      indent = message_indent,
      verbose = verbose)
    
    # Load evaluation_times from the object settings attribute, if it is not
    # provided.
    if (is.waive(evaluation_times)) {
      evaluation_times <- object@settings$eval_times
    }
    
    # Check evaluation_times argument
    if (object@outcome_type %in% c("survival")) {
      sapply(
        evaluation_times, 
        .check_number_in_valid_range, 
        var_name = "evaluation_times",
        range = c(0.0, Inf),
        closed = c(FALSE, TRUE))
    }
    
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
      closed = c(FALSE, FALSE))
    
    # Obtain ensemble method from stored settings, if required.
    if (is.waive(ensemble_method)) {
      ensemble_method <- object@settings$ensemble_method
    }
    
    # Check ensemble_method argument
    .check_parameter_value_is_valid(
      x = ensemble_method, 
      var_name = "ensemble_method",
      values = .get_available_ensemble_prediction_methods())
    
    # Check the level detail.
    detail_level <- .parse_detail_level(
      x = detail_level,
      object = object,
      default = "ensemble",
      data_element = "prediction_data")
    
    # Check the estimation type.
    estimation_type <- .parse_estimation_type(
      x = estimation_type,
      object = object,
      default = "point",
      data_element = "prediction_data",
      detail_level = detail_level,
      has_internal_bootstrap = FALSE)
    
    # Check whether results should be aggregated.
    aggregate_results <- .parse_aggregate_results(
      x = aggregate_results,
      object = object,
      default = TRUE,
      data_element = "prediction_data")
    
    # Test if models are properly loaded
    if (!is_model_loaded(object = object)) ..error_ensemble_models_not_loaded()
    
    # Aggregate data. It does not make sense to keep duplicate rows here.
    data <- aggregate_data(data = data)
    
    # Generate a prototype data element.
    proto_data_element <- new(
      "familiarDataElementPredictionTable",
      detail_level = detail_level,
      confidence_level = confidence_level,
      estimation_type = estimation_type)
    
    # Generate elements to send to dispatch.
    performance_data <- extract_dispatcher(
      FUN = .extract_predictions,
      has_internal_bootstrap = FALSE,
      aggregate_results = aggregate_results,
      cl = cl,
      object = object,
      data = data,
      proto_data_element = proto_data_element,
      is_pre_processed = is_pre_processed,
      ensemble_method = ensemble_method,
      evaluation_times = evaluation_times,
      message_indent = message_indent + 1L,
      verbose = verbose)
    
    return(performance_data)
  }
)



# extract_predictions (prediction table) ---------------------------------------
setMethod(
  "extract_predictions",
  signature(object = "familiarDataElementPredictionTable"),
  function(object, ...) {
    # A prediction table is already a prediction table :)
    return(.complete(object))
  }
)



.extract_predictions <- function(
    object,
    proto_data_element,
    evaluation_times = NULL,
    cl,
    ...) {
  
  # Ensure that the object is loaded
  object <- load_familiar_object(object)
  
  # Add model name.
  proto_data_element <- add_model_name(
    proto_data_element,
    object = object)
  
  # Add evaluation time as a identifier to the data element.
  if (length(evaluation_times) > 0 && object@outcome_type == "survival") {
    data_elements <- add_data_element_identifier(
      x = proto_data_element,
      evaluation_time = evaluation_times)
    
  } else {
    data_elements <- list(proto_data_element)
  }
  
  # Iterate over data elements.
  prediction_data <- lapply(
    data_elements,
    ..extract_predictions,
    object = object,
    cl = cl,
    ...)
  
  return(prediction_data)
}



..extract_predictions <- function(
    data_element,
    object,
    data,
    cl = NULL,
    is_pre_processed,
    ensemble_method,
    ...) {
  
  if (object@outcome_type %in% c("survival", "competing_risk")) {
    type <- .get_available_prediction_type_arguments()
    
  } else {
    type <- c("default", "novelty")
  }
  
  data_element_list <- list()
  for (ii in seq_along(type)) {
    data_element_list[[ii]] <- .predict(
      object = object,
      data = data,
      ensemble_method = ensemble_method,
      time = data_element@identifiers$evaluation_time,
      type = type[ii],
      is_pre_processed = is_pre_processed,
      aggregate_results = FALSE
    )
  }
  
  data_element_list <- unlist(data_element_list)
  data_element_list <- data_element_list[!sapply(data_element_list, is_empty)]
  if (is_empty(data_element_list)) return(NULL)
  
  # Inherit from data_element
  data_element_list <- lapply(
    data_element_list,
    function(x, ref) {
      x@ensemble_method <- ref@ensemble_method
      x@identifiers <- c(x@identifiers, ref@identifiers)
      x@detail_level <- ref@detail_level
      x@estimation_type <- ref@estimation_type
      x@confidence_level <- ref@confidence_level
      x@bootstrap_ci_method <- ref@bootstrap_ci_method
      
      return(.merge_slots_into_data(x))
      
    },
    ref = data_element
  )
  
  return(data_element_list)
}



# ..compute_data_element_estimates (familiarDataElementPredictionTable) --------
setMethod(
  "..compute_data_element_estimates",
  signature(x = "familiarDataElementPredictionTable"),
  function(
    x,
    x_list = NULL,
    object,
    ...
  ) {
    
    # It might be that x was only used to direct to this method.
    if (!is.null(x_list)) x <- x_list
    if (!is.list(x)) x <- list(x)

    # Identify the estimation types of the current data elements.
    estimation_type <- sapply(x, function(x) (x@estimation_type))
    
    # Check percentiles and confidence level.
    if (
      any(estimation_type %in% c("bci", "bootstrap_confidence_interval")) &&
      is.null(x[[1]]@percentiles) &&
      is.null(x[[1]]@confidence_level)
    ) {
      ..error_reached_unreachable_code(paste0(
        "..compute_data_element_estimates: percentiles and confidence_level ",
        "cannot both be NULL."
      ))
    }

    # Determine the bootstrap_ci_method and the aggregation function
    if (any(estimation_type %in% c("bci", "bootstrap_confidence_interval"))) {
      ensemble_method <- ifelse(x[[1]]@ensemble_method == "median", "percentile", "bc")
      
    } else {
      ensemble_method <- x[[1]]@ensemble_method
    }
    
    # Collate the data.
    if (any(estimation_type %in% c("bci", "bootstrap_confidence_interval"))) {
      
      # Check that a point estimation is present.
      if (!any(estimation_type %in% c("point"))) {
        # Add point estimate, if not already present.
        x <- c(x, list(.add_point_estimate_from_elements(x)))
        
        # Update estimation type.
        estimation_type <- sapply(x, function(x) (x@estimation_type))
      }
      
      # Select point estimate.
      point_values <- data.table::as.data.table(x[estimation_type == "point"][[1]]@data)
      point_values[, "estimation_type" := "point"]
      
      # Select bootstrap values.
      bootstrap_values <- data.table::as.data.table(
        x[estimation_type %in% c("bci", "bootstrap_confidence_interval")][[1]]@data)
      bootstrap_values[, "estimation_type" := "bootstrap_confidence_interval"]
      
      # Combine to single table.
      data <- data.table::rbindlist(
        list(point_values, bootstrap_values),
        use.names = TRUE,
        fill = TRUE)
      
      # Copy the familiarDataElement.
      y <- x[estimation_type %in% c("bci", "bootstrap_confidence_interval")][[1]]
      y@data <- NULL
      
    } else {
      # Select values.
      data <- data.table::as.data.table(x[[1]]@data)
      data[, "estimation_type" := "ensemble"]
      
      # Copy the familiarDataElement.
      y <- x[[1]]
      y@data <- NULL
    }
    
    # Compute prediction data.
    y@data <- ..compute_ensemble_prediction_estimates(
      x = y,
      data = data,
      ensemble_method = ensemble_method
    )
    
    # Update value column
    y@value_column <- setdiff(
      names(y@data),
      y@grouping_column)
    
    return(y)
  }
)


# ..compute_ensemble_prediction_estimates (generic) ----------------------------
setGeneric(
  "..compute_ensemble_prediction_estimates",
  function(x, ...) standardGeneric("..compute_ensemble_prediction_estimates")
)



# export_prediction_data (generic) ---------------------------------------------

#'@title Extract and export predicted values.
#'
#'@description Extract and export the values predicted by single and ensemble
#'  models in a familiarCollection.
#'
#'@inheritParams export_all
#'@inheritParams export_univariate_analysis_data
#'
#'@inheritDotParams extract_predictions
#'@inheritDotParams as_familiar_collection
#'
#'@details Data, such as model performance and calibration information, is
#'  usually collected from a `familiarCollection` object. However, you can also
#'  provide one or more `familiarData` objects, that will be internally
#'  converted to a `familiarCollection` object. It is also possible to provide a
#'  `familiarEnsemble` or one or more `familiarModel` objects together with the
#'  data from which data is computed prior to export. Paths to the previous
#'  files can also be provided.
#'
#'  All parameters aside from `object` and `dir_path` are only used if `object`
#'  is not a `familiarCollection` object, or a path to one.
#'
#'  Both single and ensemble predictions are exported.
#'
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_prediction_data
#'@md
#'@rdname export_prediction_data-methods
setGeneric(
  "export_prediction_data",
  function(
    object,
    dir_path = NULL,
    export_collection = FALSE,
    ...) {
    standardGeneric("export_prediction_data")
  }
)



# #export_prediction_data (collection) -----------------------------------------

#'@rdname export_prediction_data-methods
setMethod(
  "export_prediction_data",
  signature(object = "familiarCollection"),
  function(
    object,
    dir_path = NULL,
    export_collection = FALSE,
    ...) {
    
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    prediction_table_classes <- sapply(object@prediction_data, class)
    if (is_empty(prediction_table_classes)) return(NULL)
    
    prediction_objects <- lapply(object@prediction_data, .copy)
    prediction_objects <- lapply(prediction_objects, .complete)
    prediction_objects <- merge_data_elements(
      prediction_objects,
      as_data = "all",
      as_grouping_column = TRUE,
      force_data_table = TRUE
    )
    
    # Merge prediction objects into a single table.
    if (length(prediction_objects) > 1) {
      export_object <- methods::new(
        "familiarDataElementPredictionTable",
        data = prediction_objects[[1]]@data,
        value_column = prediction_objects[[1]]@value_column,
        grouping_column = prediction_objects[[1]]@grouping_column
      )
      
      for (ii in seq_along(prediction_objects)) {
        if (ii == 1) next
        
        grouping_columns <- intersect(export_object@grouping_column, prediction_objects[[ii]]@grouping_column)
        
        export_object@data <- merge(
          x = export_object@data,
          y = prediction_objects[[ii]]@data[, mget(c(grouping_columns, prediction_objects[[ii]]@value_column))],
          by = grouping_columns,
          all = TRUE,
          sort = FALSE
        )
        export_object@value_column <- union(export_object@value_column, prediction_objects[[ii]]@value_column)
        export_object@grouping_column <- union(export_object@grouping_column, prediction_objects[[ii]]@grouping_column)
      }
    }
    
    object@prediction_data <- list(export_object)
    
    return(.export(
      x = object,
      data_slot = "prediction_data",
      dir_path = dir_path,
      type = "prediction",
      subtype = NULL,
      export_collection = export_collection
    ))
  }
)



# export_prediction_data (general) ---------------------------------------------

#'@rdname export_prediction_data-methods
setMethod(
  "export_prediction_data",
  signature(object = "ANY"),
  function(
    object,
    dir_path = NULL,
    export_collection = FALSE,
    ...) {
    
    # Attempt conversion to familiarCollection object.
    object <- do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = object,
          "data_element" = "prediction_data"),
        list(...)))
    
    return(do.call(
      export_prediction_data,
      args = c(
        list(
          "object" = object,
          "dir_path" = dir_path,
          "export_collection" = export_collection),
        list(...))))
  }
)

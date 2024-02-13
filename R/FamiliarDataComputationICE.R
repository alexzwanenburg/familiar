#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

# familiarDataElementIndividualConditionalExpectation object -------------------
setClass(
  "familiarDataElementIndividualConditionalExpectation",
  contains = "familiarDataElement"
)



# familiarDataElementPartialDependence object ----------------------------------
setClass(
  "familiarDataElementPartialDependence",
  contains = "familiarDataElement"
)



# extract_ice (generic) --------------------------------------------------------

#'@title Internal function to extract data for individual conditional
#'  expectation plots.
#'
#'@description Computes data for individual conditional expectation plots and
#'  partial dependence plots for the model(s) in a `familiarEnsemble` object.
#'
#'@param features Names of the feature or features (2) assessed simultaneously.
#'  By default `NULL`, which means that all features are assessed one-by-one.
#'@param feature_x_range When one or two features are defined using `features`,
#'  `feature_x_range` can be used to set the range of values for the first
#'  feature. For numeric features, a vector of two values is assumed to indicate
#'  a range from which `n_sample_points` are uniformly sampled. A vector of more
#'  than two values is interpreted as is, i.e. these represent the values to be
#'  sampled. For categorical features, values should represent a (sub)set of
#'  available levels.
#'@param feature_y_range As `feature_x_range`, but for the second feature in
#'  case two features are defined.
#'@param n_sample_points Number of points used to sample continuous features.
#'@inheritParams .extract_data
#'
#'@return A data.table containing individual conditional expectation plot data.
#'@md
#'@keywords internal
setGeneric(
  "extract_ice",
  function(
    object,
    data,
    cl = NULL,
    features = NULL,
    feature_x_range = NULL,
    feature_y_range = NULL,
    n_sample_points = 50L,
    ensemble_method = waiver(),
    evaluation_times = waiver(),
    sample_limit = waiver(),
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
    standardGeneric("extract_ice")
  }
)



# extract_ice (familiarEnsemble) -----------------------------------------------
setMethod(
  "extract_ice",
  signature(object = "familiarEnsemble"),
  function(
    object,
    data,
    cl = NULL,
    features = NULL,
    feature_x_range = NULL,
    feature_y_range = NULL,
    n_sample_points = 50L,
    ensemble_method = waiver(),
    evaluation_times = waiver(),
    sample_limit = waiver(),
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
    
    # Message extraction start
    if (is.null(features)) {
      logger_message(
        paste0(
          "Computing individual conditional expectation and partial dependence ",
          "data for features in the dataset."
        ),
        indent = message_indent,
        verbose = verbose
      )
      
    } else {
      logger_message(
        paste0(
          "Computing individual conditional expectation and partial dependence ",
          "data for the selected features."
        ),
        indent = message_indent,
        verbose = verbose
      )
    }
    
    # Load evaluation_times from the object settings attribute, if it is not provided.
    if (is.waive(evaluation_times)) evaluation_times <- object@settings$eval_times
    
    # Check evaluation_times argument
    if (object@outcome_type %in% c("survival")) {
      sapply(
        evaluation_times,
        .check_number_in_valid_range,
        var_name = "evaluation_times",
        range = c(0.0, Inf),
        closed = c(FALSE, TRUE)
      )
    }
    
    # Check n_sample_points argument
    .check_number_in_valid_range(
      x = n_sample_points,
      var_name = "n_sample_points",
      range = c(1, Inf)
    )
    
    # Obtain ensemble method from stored settings, if required.
    if (is.waive(ensemble_method)) ensemble_method <- object@settings$ensemble_method
    
    # Check ensemble_method argument
    .check_parameter_value_is_valid(
      x = ensemble_method,
      var_name = "ensemble_method",
      values = .get_available_ensemble_prediction_methods()
    )
    
    # Load confidence alpha from object settings attribute if not provided
    # externally.
    if (is.waive(confidence_level)) confidence_level <- object@settings$confidence_level
    
    # Check confidence_level input argument
    .check_number_in_valid_range(
      x = confidence_level,
      var_name = "confidence_level",
      range = c(0.0, 1.0),
      closed = c(FALSE, FALSE)
    )
    
    # Load the bootstrap method
    if (is.waive(bootstrap_ci_method)) bootstrap_ci_method <- object@settings$bootstrap_ci_method
    
    .check_parameter_value_is_valid(
      x = bootstrap_ci_method,
      var_name = "bootstrap_ci_method",
      values = .get_available_bootstrap_confidence_interval_methods()
    )
    
    # Check the sample limit.
    sample_limit <- .parse_sample_limit(
      x = sample_limit,
      object = object,
      default = Inf,
      data_element = "ice_data"
    )
    
    # Check the level detail.
    detail_level <- .parse_detail_level(
      x = detail_level,
      object = object,
      default = "hybrid",
      data_element = "ice_data"
    )
    
    # Check the estimation type.
    estimation_type <- .parse_estimation_type(
      x = estimation_type,
      object = object,
      default = "bootstrap_confidence_interval",
      data_element = "ice_data",
      detail_level = detail_level,
      has_internal_bootstrap = FALSE
    )
    
    # Check whether results should be aggregated.
    aggregate_results <- .parse_aggregate_results(
      x = aggregate_results,
      object = object,
      default = TRUE,
      data_element = "ice_data"
    )
    
    # Test if models are properly loaded
    if (!is_model_loaded(object = object)) ..error_ensemble_models_not_loaded()
    
    # Test if any model in the ensemble was successfully trained.
    if (!model_is_trained(object = object)) return(NULL)
    
    # Generate a prototype data element.
    proto_data_element <- new(
      "familiarDataElementIndividualConditionalExpectation",
      detail_level = detail_level,
      estimation_type = estimation_type,
      confidence_level = confidence_level,
      bootstrap_ci_method = bootstrap_ci_method
    )
    
    # Generate elements to send to dispatch.
    ice_data <- extract_dispatcher(
      FUN = .extract_ice,
      has_internal_bootstrap = FALSE,
      cl = cl,
      object = object,
      data = data,
      features = features,
      feature_x_range = feature_x_range,
      feature_y_range = feature_y_range,
      sample_limit = sample_limit,
      n_sample_points = n_sample_points,
      proto_data_element = proto_data_element,
      is_pre_processed = is_pre_processed,
      ensemble_method = ensemble_method,
      evaluation_times = evaluation_times,
      aggregate_results = TRUE,
      message_indent = message_indent + 1L,
      verbose = verbose
    )
    
    return(ice_data)
  }
)



# extract_ice (prediction table) -----------------------------------------------
setMethod(
  "extract_ice",
  signature(object = "familiarDataElementPredictionTable"),
  function(object, ...) {
    ..warning_no_data_extraction_from_prediction_table("individual conditional expectation")
    
    return(NULL)
  }
)



.extract_ice <- function(
    object,
    data,
    proto_data_element,
    evaluation_times = NULL,
    features = NULL,
    sample_limit,
    aggregate_results,
    n_models,
    is_pre_processed = FALSE,
    cl,
    message_indent = 0L,
    verbose = FALSE,
    progress_bar = FALSE,
    ...
) {
  
  # Ensure that the object is loaded
  object <- load_familiar_object(object)
  
  # Add model name.
  proto_data_element <- add_model_name(proto_data_element, object = object)
  
  # Retrieve input data.
  data <- process_input_data(
    object = object,
    data = data,
    stop_at = "signature",
    is_pre_processed = is_pre_processed
  )
  
  # Maintain only important features. The current set is based on the
  # required features.
  data <- filter_features(
    data = data,
    available_features = object@model_features
  )
  
  # Check if the input data is not empty
  if (is_empty(data)) return(NULL)
  
  # Select samples up to sample_limit.
  data <- get_subsample(
    data = data,
    size = sample_limit,
    seed = 0L
  )
  
  # Aggregate data.
  data <- aggregate_data(data)
  
  # Add evaluation time as a identifier to the data element.
  if (length(evaluation_times) > 0 && object@outcome_type == "survival") {
    data_elements <- add_data_element_identifier(
      x = proto_data_element, 
      evaluation_time = evaluation_times
    )
    
  } else {
    data_elements <- list(proto_data_element)
  }
  
  if (!is.null(features)) {
    
    # Check that the features exist in the data set.
    if (!all(features %in% c(object@model_features))) {
      rlang::warn(paste0(
        "Data for individual conditional expectation or partial dependence plots ",
        "could not be computed for ",
        paste_s(setdiff(features, object@model_features)),
        " feature(s) as they are not used by the model."
      ))
    }
    
    # Add features as identifier.
    if (length(features) == 1) {
      data_elements <- add_data_element_identifier(
        x = data_elements,
        feature_x = features
      )
      
    } else if (length(features) == 2) {
      
      if (length(unique(features)) != 2) {
        rlang::abort(paste0(
          "Data for individual conditional expectation or partial dependence plots ",
          "could not be computed as the provided features are not unique: ",
          paste_s(features), "."
        ))
      }
      
      data_elements <- add_data_element_identifier(
        x = data_elements,
        feature_x = features[1]
      )
      data_elements <- add_data_element_identifier(
        x = data_elements,
        feature_y = features[2]
      )
      
    } else {
      rlang::abort(paste0(
        "Data for individual conditional expectation or partial dependence plots cannot ",
        "be computed for more than 2 features simultaneously. Found: ",
        paste_s(features), "."
      ))
    }
    
  } else {
    # Add features as identifier.
    data_elements <- add_data_element_identifier(
      x = data_elements,
      feature_x = object@model_features
    )
  }
  
  # Iterate over elements.
  data_elements <- fam_mapply(
    cl = cl,
    assign = NULL,
    FUN = ..extract_ice_data,
    data_element = data_elements,
    MoreArgs = c(
      list(
        "data" = data,
        "object" = object,
        "verbose" = verbose && !progress_bar && n_models == 1,
        "message_indent" = message_indent
      ),
      list(...)
    ),
    progress_bar = progress_bar,
    chopchop = TRUE
  )
  
  # Flatten list of data elements.
  data_elements <- unlist(data_elements)
  if (!is.list(data_elements)) data_elements <- list(data_elements)
  
  # Merge data elements
  data_elements <- merge_data_elements(data_elements)
  
  if (aggregate_results) data_elements <- .compute_data_element_estimates(x = data_elements)
  
  return(data_elements)
}



..extract_ice_data <- function(
    cl = NULL,
    data_element,
    data,
    object,
    aggregate_results,
    feature_x_range = NULL,
    feature_y_range = NULL,
    n_sample_points,
    ensemble_method,
    verbose = FALSE,
    message_indent,
    ...
) {
  # Divide feature(s) into points.

  # Generate range
  feature_x_range <- .create_feature_range(
    feature_info = object@feature_info,
    feature = data_element@identifiers$feature_x,
    column_type = class(data@data[[data_element@identifiers$feature_x]]),
    feature_range = feature_x_range,
    n = n_sample_points
  )
  
  # Add feature values.
  data_elements <- add_data_element_identifier(
    x = data_element,
    feature_x_value = feature_x_range
  )
  
  # Mention feature.
  message_str <- paste0(
    "Computing ICE / PD curves for \"", data_element@identifiers$feature_x, "\""
  )
  
  if (!is.null(data_element@identifiers$feature_y)) {
    feature_y_range <- .create_feature_range(
      feature_info = object@feature_info,
      feature = data_element@identifiers$feature_y,
      column_type = class(data@data[[data_element@identifiers$feature_y]]),
      feature_range = feature_y_range,
      n = n_sample_points
    )
    
    # Add feature values.
    data_elements <- add_data_element_identifier(
      x = data_elements,
      feature_y_value = feature_y_range
    )
    
    # Mention feature.
    message_str <- c(
      message_str, 
      paste0(" and \"", data_element@identifiers$feature_y, "\"")
    )
  }
  
  # Add evaluation time.
  if (length(data_element@identifiers$evaluation_time) > 0) {
    message_str <- c(
      message_str,
      paste0(" at time ", data_element@identifiers$evaluation_time, ".")
    )
    
  } else {
    message_str <- c(message_str, ".")
  }
  
  logger_message(
    paste0(message_str, collapse = ""),
    indent = message_indent,
    verbose = verbose
  )
  
  # Iterate over elements.
  data_elements <- lapply(
    data_elements,
    ...extract_ice_data,
    data = data,
    object = object,
    ensemble_method = ensemble_method
  )
  
  return(data_elements)
}



...extract_ice_data <- function(
    data_element,
    data,
    object,
    ensemble_method
) {
  
  # Make a local copy of the data.
  data@data <- data.table::copy(data@data)
  
  # Replace the feature indicated by feature_x by the value in feature_x_value.
  data@data[, (data_element@identifiers$feature_x) := data_element@identifiers$feature_x_value]
  
  # Replace the feature indicated by feature_y by the value in feature_y_value.
  if (!is.null(data_element@identifiers$feature_y)) {
    data@data[, (data_element@identifiers$feature_y) := data_element@identifiers$feature_y_value]
  }

  # Predict both primary outcomes and novelty
  type <- ifelse(
    object@outcome_type %in% c("survival", "competing_risk"),
    "survival_probability", 
    "default"
  )
  
  # Compute performance data.
  prediction_data <- .predict(
    object = object,
    data = data,
    ensemble_method = ensemble_method,
    time = data_element@identifiers$evaluation_time,
    type = type,
    aggregate_results = TRUE
  )

  # Compute novelty values.
  novelty_data <- .predict(
    object = object,
    data = data,
    ensemble_method = ensemble_method,
    time = data_element@identifiers$evaluation_time,
    type = "novelty",
    aggregate_results = TRUE
  )
  
  # Check that valid prediction data were generated.
  if (!any_predictions_valid(prediction_data)) return(NULL)
  prediction_data <- .drop_reference_data(prediction_data)
  prediction_data <- .merge_slots_into_data(prediction_data)
  prediction_data <- remove_invalid_predictions(prediction_data)
  
  # Check if removing invalid predictions leaves any data.
  if (is_empty(prediction_data)) return(NULL)
  
  novelty_data <- .drop_reference_data(novelty_data)
  novelty_data <- .merge_slots_into_data(novelty_data)
  novelty_data <- remove_invalid_predictions(novelty_data)
  
  if (object@outcome_type %in% c("binomial", "multinomial")) {
    
    # Determine class levels.
    class_levels <- get_outcome_class_levels(object)
    
    if (object@outcome_type == "binomial") {
      used_class_levels <- class_levels[2]
      prediction_data@value_column <- c(
        setdiff(prediction_data@value_column, class_levels),
        used_class_levels
      )
      
    } else {
      used_class_levels <- class_levels
    }
    
    # Make sure that probability is returned.
    prediction_data <- .convert_value_to_grouping_column(
      prediction_data,
      new_grouping_column = used_class_levels, 
      new_grouping_column_name = "positive_class", 
      new_value_column_name = "probability"
    )
  }
  
  # Create ice and pd plot data.
  data_elements <- .create_ice_and_pd_objects(
    data_element,
    prediction_data = prediction_data,
    novelty_data = novelty_data
  )
  
  return(data_elements)
}



.create_feature_range <- function(
    feature_info,
    feature, 
    feature_range, 
    n, 
    column_type
) {
  
  # Find the feature information associated with the feature.
  feature_info <- feature_info[[feature]]
  
  # Check that the feature info is present.
  if (is.null(feature_info)) {
    rlang::abort(
      paste0("Feature information could not be found for the ", feature, " feature.")
    )
  }
  
  # Determine if the feature is categorical or numerical.
  if (feature_info@feature_type == "factor") {
    
    # Get the levels.
    feature_levels <- feature_info@levels
    
    if (is.null(feature_range)) {
      feature_range <- factor(feature_levels, levels = feature_levels)
      
    } else if (!all(feature_range %in% feature_levels)) {
      rlang::abort(paste0(
        "One or more levels defined in the feature range for creating ",
        "individual conditional expectation and partial dependence plots do not ",
        "match levels found in training data: ",
        paste_s(setdiff(feature_range, feature_levels)),
        ". Check for misspelled levels."
      ))
    }
    # If not null, and no mismatches occur, use feature_range directly.
    
  } else if (feature_info@feature_type == "numeric") {
    
    if (!is.null(feature_range)) {
      # Check that values are numeric.
      if (!is.numeric(feature_range)) {
        rlang::abort(paste0(
          "Numeric values are required to define the feature range for creating ",
          "individual conditional expectation and partial dependence plots."
        ))
      } 
      
      # Check that all values are finite.
      if (any(!is.finite(feature_range))) {
        rlang::abort(paste0(
          "Numeric values for creating individual conditional expectation and partial dependence ",
          " plots should be finite. NA and infinite values are not allowed."
        ))
      }
        
      # Sort values.
      feature_range <- sort(unique(feature_range))
      
      # If two values are defined, interpret as range, and sample from it.
      # If not, use feature_range directly.
      if (length(feature_range) == 2) {
        feature_range <- stats::approx(
          x = c(0.00, 1.00),
          y = feature_range,
          n = n,
          method = "linear"
        )$y
      }
      
    } else {
      # Create the range of values from the feature distribution.
      if (n == 1) {
        feature_range <- as.numeric(feature_info@distribution$fivenum)[3]
        
      } else {
        # Sample the five-number summary.
        feature_range <- stats::spline(
          x = c(0.00, 0.25, 0.50, 0.75, 1.00),
          y = as.numeric(feature_info@distribution$fivenum),
          n = n,
          method = "hyman"
        )$y
      }
    }
    
    # Convert to integer if required.
    if (any(column_type == "integer")) feature_range <- as.integer(feature_range)
    
    # Select unique values.
    feature_range <- unique(feature_range)
    
  } else {
    ..error_reached_unreachable_code(paste0(
      ".create_feature_range: encountered unknown feature type (",
      feature_info@feature_type, ") for the ", feature, " feature."
    ))
  }
  
  return(feature_range)
}



.create_ice_and_pd_objects <- function(
    data_element,
    prediction_data,
    novelty_data
) {
  # Create ice and pd data elements.
  ice_data_element <- data_element

  if (is_empty(novelty_data)) {
    ice_data_element@data <- data.table::copy(.as_data_table(prediction_data))
    ice_data_element@grouping_column <- prediction_data@grouping_column
    ice_data_element@value_column <- prediction_data@value_column
    
  } else {
    data <- merge(
      x = data.table::copy(.as_data_table(prediction_data)),
      y = data.table::copy(.as_data_table(novelty_data)),
      by = intersect(prediction_data@grouping_column, novelty_data@grouping_column),
      all = TRUE
    )
    
    ice_data_element@data <- data
    ice_data_element@grouping_column <- union(prediction_data@grouping_column, novelty_data@grouping_column)
    ice_data_element@value_column <- union(prediction_data@value_column, novelty_data@value_column)
  }
  
  # Update pd data element.
  pd_data_element <- .create_pd_object(ice_data_element)
  
  return(list(ice_data_element, pd_data_element))
}
  


.create_pd_object <- function(ice_data_element) {
  
  # Create partial dependence data.
  pd_data_element <- methods::new(
    "familiarDataElementPartialDependence",
    ice_data_element
  )
  
  # Select grouping columns.
  grouping_columns <- setdiff(
    ice_data_element@grouping_column, get_id_columns()
  )
  
  if (length(grouping_columns) == 0) grouping_columns <- NULL
  pd_data_element@grouping_column <- grouping_columns
  
  # Average data.
  if (length(grouping_columns) > 0) {
    pd_data_element@data <- pd_data_element@data[
      ,
      lapply(.SD, mean, na.rm = TRUE),
      .SDcols = ice_data_element@value_column,
      by = c(grouping_columns)
    ]
    
  } else {
    pd_data_element@data <- pd_data_element@data[
      ,
      lapply(.SD, mean, na.rm = TRUE),
      .SDcols = ice_data_element@value_column
    ]
  }
  
  return(pd_data_element)
}



.update_ice_and_pd_output <- function(
    ice_data,
    pd_data,
    outcome_type,
    anchor_values = NULL,
    n_samples = NULL,
    seed
) {
  
  if (is_empty(ice_data)) {
    return(list(
      "ice_data" = ice_data,
      "pd_data" = pd_data
    ))
  } 
  
  # Find anchor value for the x-feature. It will be NULL if the current feature
  # does does not appear in anchor_values.
  x_anchor <- tryCatch(
    anchor_values[[ice_data@identifiers$feature_x]],
    error = function(err) (return(NULL))
  )
  
  if (!is.null(x_anchor)) {
    if (length(x_anchor) > 1) {
      rlang::abort(paste0(
        "Only a single value can be provided as an anchor value for the ",
        ice_data@identifiers$feature_x, " feature."
      ))
    } 
  }
  
  # Find anchor value for the y-feature. It will be NULL if the current feature
  # does does not appear in anchor_values, or there is not y-feature associated
  # with the data.
  y_anchor <- tryCatch(
    anchor_values[[ice_data@identifiers$feature_y]],
    error = function(err) (return(NULL))
  )
  
  if (!is.null(y_anchor)) {
    if (length(y_anchor) > 1) {
      rlang::abort(paste0(
        "Only a single value can be provided as an anchor value for the ",
        ice_data@identifiers$feature_y, " feature."
      ))
    }
  }

  # Rename main value column to a consist name.
  if (outcome_type %in% c("binomial", "multinomial")) {
    old_value_column <- "probability"
    
  } else if (outcome_type %in% c("continuous", "survival")) {
    old_value_column <- "predicted_outcome"
    
  } else {
    ..error_outcome_type_not_implemented(outcome_type)
  }
  
  # Update confidence interval names.
  if (ice_data@estimation_type %in% c("bci", "bootstrap_confidence_interval")) {
    old_value_column <- paste0(old_value_column, c("", "_ci_low", "_ci_up"))
    new_value_column <- c("value", "value_ci_low", "value_ci_up")
    
  } else {
    new_value_column <- "value"
  }
  
  # Replace column names.
  data.table::setnames(
    x = ice_data@data,
    old = old_value_column,
    new = new_value_column
  )
  data.table::setnames(
    x = pd_data@data, 
    old = old_value_column,
    new = new_value_column
  )
  
  # Update value column attributes.
  ice_data@value_column <- c(
    new_value_column,
    setdiff(ice_data@value_column, old_value_column)
  )
  pd_data@value_column <- c(
    new_value_column,
    setdiff(pd_data@value_column, old_value_column)
  )

  if (!is.null(x_anchor) || !is.null(y_anchor)) {
    
    # Update individual conditional expectation data.
    ice_data <- ..update_ice_and_pd_output(
      x = ice_data,
      x_anchor = x_anchor,
      y_anchor = y_anchor,
      value_column = new_value_column,
      outcome_type = outcome_type,
      anchor_values = anchor_values
    )
    
    # Update partial dependence data.
    pd_data <- .create_pd_object(ice_data)
  }
  
  if (!is.null(n_samples)) {
    # Select up to n_samples. These samples are random between experiments, but
    # fixed within one, as we explicitly set the random seed to be used.
    cropped_ice_data <- lapply(
      split(ice_data@data, by = "data_set", drop = TRUE),
      ..restrict_ice_samples,
      n_samples = n_samples,
      seed = seed)
    
    # Replace data attribute with the limited sample list.
    ice_data@data <- data.table::rbindlist(
      cropped_ice_data,
      use.names = TRUE)
  }
  
  return(list(
    "ice_data" = ice_data,
    "pd_data" = pd_data
  ))
}



..update_ice_and_pd_output <- function(
    x,
    x_anchor = NULL,
    y_anchor = NULL, 
    value_column, 
    outcome_type, 
    anchor_values = NULL) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  feature_x_value <- feature_x_value <- feature_y_value <- value <- NULL
  
  # Subtract anchor values.
  if (!is.null(x_anchor) && !is.null(y_anchor)) {
  
    # Set grouping columns.
    grouping_columns <- setdiff(
      x@grouping_column,
      c("feature_x_value", "feature_y_value"))
    
    # Subtract anchor value for 2D plots with x and y anchors.
    if (length(grouping_columns) > 0) {
      x@data[
        ,
        (value_column) := lapply(
          .SD,
          ..anchor_ice_values_2D,
          x = feature_x_value,
          x_anchor = x_anchor,
          y = feature_y_value,
          y_anchor = y_anchor,
          value_offset = value,
          x_name = x@identifiers$feature_x,
          y_name = x@identifiers$feature_y
        ),
        by = c(grouping_columns),
        .SDcols = value_column
      ]
      
    } else {
      x@data[
        ,
        (value_column) := lapply(
          .SD,
          ..anchor_ice_values_1D,
          x = feature_x_value,
          x_anchor = x_anchor,
          y = feature_y_value,
          y_anchor = y_anchor,
          value_offset = value,
          x_name = x@identifiers$feature_x,
          y_name = x@identifiers$feature_y
        ),
        .SDcols = value_column
      ]
    }
    
  } else if (!is.null(x_anchor)) {
    # Set grouping columns.
    grouping_columns <- setdiff(x@grouping_column, c("feature_x_value"))
    
    # Subtract anchor value for x anchor.
    if (length(grouping_columns) > 0) {
      x@data[
        ,
        (value_column) := lapply(
          .SD,
          ..anchor_ice_values_1D,
          x = feature_x_value,
          x_anchor = x_anchor,
          value_offset = value,
          name = x@identifiers$feature_x
        ),
        by = c(grouping_columns),
        .SDcols = value_column
      ]
      
    } else {
      x@data[
        ,
        (value_column) := lapply(
          .SD,
          ..anchor_ice_values_1D,
          x = feature_x_value,
          x_anchor = x_anchor,
          value_offset = value,
          name = x@identifiers$feature_x
        ),
        .SDcols = value_column
      ]
    }
    
  } else if (!is.null(y_anchor)) {
    # Set grouping columns.
    grouping_columns <- setdiff(x@grouping_column, c("feature_y_value"))
    
    # Subtract anchor value for y anchor.
    if (length(grouping_columns) > 0) {
      x@data[
        ,
        (value_column) := lapply(
          .SD,
          ..anchor_ice_values_1D,
          x = feature_y_value,
          x_anchor = y_anchor,
          value_offset = value,
          name = x@identifiers$feature_y
        ),
        by = c(grouping_columns),
        .SDcols = value_column
      ]
      
    } else {
      x@data[
        ,
        (value_column) := lapply(
          .SD,
          ..anchor_ice_values_1D,
          x = feature_y_value,
          x_anchor = y_anchor,
          value_offset = value,
          name = x@identifiers$feature_y
        ),
        .SDcols = value_column
      ]
    }
    
  } else {
    # No anchor values were set.
    return(x)
  }
  
  return(x)
}



..restrict_ice_samples <- function(x, n_samples, seed = NULL) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  sample <- NULL
  
  if (is.null(n_samples)) return(x)

  if (is.null(x$sample)) {
    ..error_reached_unreachable_code("sample column was not defined")
  }
  
  # Select unique sample identifiers.
  sample_identifiers <- unique(x$sample)
  
  # Do not sample if the number of available samples is smaller than n_samples.
  if (length(sample_identifiers) < n_samples) return(x)
  
  # Select samples.
  selected_identifiers <- fam_sample(
    x = sample_identifiers,
    size = n_samples,
    replace = FALSE,
    seed = seed
  )
  
  x <- x[sample %in% selected_identifiers]
  
  return(x)
}



..anchor_ice_values_1D <- function(
    value_in, 
    x, 
    x_anchor, 
    value_offset, 
    name) {
  
  # Check anchor value.
  x_anchor <- .check_anchor_value(
    x = x,
    x_anchor = x_anchor,
    name = name)
  
  if (is.numeric(x)) {
    # Set anchor value
    if (x_anchor %in% x) {
      anchor_value <- value_offset[x == x_anchor]
        
    } else {
      # Interpolate.
      anchor_value <- stats::spline(
        x = x,
        y = value_offset,
        xout = x_anchor)$y
    }
  } else {
    # Set anchor value by selecting the value corresponding to x_anchor.
    anchor_value <- value_offset[x == x_anchor]
  }
  
  # Subtract the anchor value.
  return(value_in - anchor_value)
}



..anchor_ice_values_2D <- function(
    value_in, 
    x,
    x_anchor, 
    y, 
    y_anchor, 
    value_offset, 
    x_name, 
    y_name) {
  # This is a bit more tricky since we need to do a surface interpolation which
  # R does not support out of the box. The akima package has a weird licence.
  
  # Check anchor values.
  x_anchor <- .check_anchor_value(
    x = x,
    x_anchor = x_anchor, 
    name = x_name)
  y_anchor <- .check_anchor_value(
    x = y, 
    x_anchor = y_anchor, 
    name = y_name)
  
  if (x_anchor %in% x && y_anchor %in% y) {
    # Simple - no interpolation needed.
    anchor_value <- value_offset[x == x_anchor & y == y_anchor]
    
  } else if (x_anchor %in% x) {
    # Interpolation in y.
    anchor_value <- stats::spline(
      x = y[x == x_anchor],
      y = value_offset[x == x_anchor],
      xout = y_anchor)$y
    
  } else if (y_anchor %in% y) {
    # Interpolation in x.
    anchor_value <- stats::spline(
      x = x[y == y_anchor],
      y = value_offset[y == y_anchor],
      xout = x_anchor)$y
    
  } else {
    require_package(
      x = "ranger",
      purpose = "to anchor ICE/PD plot curves")
    
    # Interpolation using random forest.
    model <- ranger::ranger(
      value ~ x + y,
      data = data.table::data.table(
        "x" = x,
        "y" = y,
        "value" = value_offset),
      num.trees = 100,
      num.threads = 1L,
      seed = 1L)
    
    anchor_value <- predict(
      model,
      data.table::data.table(
        "x" = x_anchor,
        "y" = y_anchor),
      num.threads = 1L)$predictions
  }
  
  # Subtract the anchor value.
  return(value_in - anchor_value)
}



.check_anchor_value <- function(x, x_anchor, name) {
  # Checks the anchor value, and updates it if needed.
  
  if (is.numeric(x)) {
    if (!is.numeric(x_anchor)) {
      stop(paste0("Anchor value of the ", name, " feature should be numeric."))
    }
    
    # Check for out-of-range anchor values.
    if (x_anchor < min(x) || x_anchor > max(x)) {
      x_anchor_old <- x_anchor
      
      if (x_anchor < min(x)) x_anchor <- min(x)
      if (x_anchor > max(x)) x_anchor <- max(x)
      
      warning(paste0(
        "Anchor value (", x_anchor_old, ") of the ", name,
        " feature lies outside computed range. ",
        "The nearest value (", x_anchor, ") is used instead."))
    }
    
  } else if (is.factor(x)) {
    
    # Check that the anchor value appears in the levels of x.
    if (!x_anchor %in% levels(x)) {
      stop(paste0(
        "Anchor value (", x_anchor, ") of the ", name,
        " feature was not found among the computed levels (",
        paste_s(levels(x)), ")."))
    }
    
  } else {
    if (!x_anchor %in% x) {
      stop(paste0(
        "Anchor value (", x_anchor, ") of the ", name,
        " feature was not found among the available values."))
    }
  }
  
  return(x_anchor)
}



# export_ice_data (generic) ----------------------------------------------------

#'@title Extract and export individual conditional expectation data.
#'
#'@description Extract and export individual conditional expectation data.
#'
#'@inheritParams export_all
#'@inheritParams export_univariate_analysis_data
#'
#'@inheritDotParams extract_ice
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
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_ice_data
#'@md
#'@rdname export_ice_data-methods
setGeneric(
  "export_ice_data",
  function(
    object,
    dir_path = NULL,
    aggregate_results = TRUE,
    export_collection = FALSE,
    ...) {
    standardGeneric("export_ice_data")
  }
)



# export_ice_data (collection) -------------------------------------------------

#'@rdname export_ice_data-methods
setMethod(
  "export_ice_data",
  signature(object = "familiarCollection"),
  function(
    object,
    dir_path = NULL,
    aggregate_results = TRUE,
    export_collection = FALSE,
    ...) {
    
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    # Obtain individual conditional expectation plots.
    return(.export(
      x = object,
      data_slot = "ice_data",
      dir_path = dir_path,
      aggregate_results = aggregate_results,
      type = "explanation",
      subtype = "ice",
      object_class = "familiarDataElementIndividualConditionalExpectation",
      export_collection = export_collection))
  }
)



# export_ice_data (general) ----------------------------------------------------

#'@rdname export_ice_data-methods
setMethod(
  "export_ice_data",
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
          "data_element" = "ice_data",
          "aggregate_results" = aggregate_results),
        list(...)))
    
    return(do.call(
      export_ice_data,
      args = c(
        list(
          "object" = object,
          "dir_path" = dir_path,
          "aggregate_results" = aggregate_results,
          "export_collection" = export_collection),
        list(...))))
  }
)



# .export (familiarDataElementIndividualConditionalExpectation) ----------------
setMethod(
  ".export",
  signature(x = "familiarDataElementIndividualConditionalExpectation"),
  function(
    x,
    x_list, 
    aggregate_results = FALSE,
    ...) {

    if (aggregate_results) {
      x_list <- .compute_data_element_estimates(x_list)
    }
    
    # Determine identifiers that should be merged. Since the feature values of
    # the x and y features may be different (e.g. numeric and factor), merging
    # them would cause features values to merged incorrectly.
    merging_identifiers <- setdiff(
      names(x@identifiers),
      c("feature_x", "feature_y"))
    
    # Merge data elements.
    x <- merge_data_elements(
      x = x_list,
      as_data = merging_identifiers,
      as_grouping_column = TRUE,
      force_data_table = TRUE)
    
    return(x)
  }
)



# export_partial_dependence_data (generic) -------------------------------------

#'@title Extract and export partial dependence data.
#'
#'@description Extract and export partial dependence data.
#'
#'@inheritParams export_all
#'@inheritParams export_univariate_analysis_data
#'
#'@inheritDotParams extract_ice
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
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_partial_dependence_data
#'@md
#'@rdname export_partial_dependence_data-methods
setGeneric(
  "export_partial_dependence_data",
  function(
    object,
    dir_path = NULL,
    aggregate_results = TRUE,
    export_collection = FALSE,
    ...) {
    standardGeneric("export_partial_dependence_data")
  }
)



# export_partial_dependence_data (collection) ----------------------------------

#'@rdname export_partial_dependence_data-methods
setMethod(
  "export_partial_dependence_data",
  signature(object = "familiarCollection"),
  function(
    object,
    dir_path = NULL,
    aggregate_results = TRUE,
    export_collection = FALSE,
    ...) {
    
    # Make sure the collection object is updated.
    object <- update_object(object = object)
    
    # Obtain partial dependence
    return(.export(
      x = object,
      data_slot = "ice_data",
      dir_path = dir_path,
      aggregate_results = aggregate_results,
      type = "explanation",
      subtype = "pd",
      object_class = "familiarDataElementPartialDependence",
      export_collection = export_collection))
  }
)



# export_partial_dependence_data (general) -------------------------------------

#'@rdname export_partial_dependence_data-methods
setMethod(
  "export_partial_dependence_data",
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
          "data_element" = "ice_data",
          "aggregate_results" = aggregate_results),
        list(...)))
    
    return(do.call(
      export_partial_dependence_data,
      args = c(
        list(
          "object" = object,
          "dir_path" = dir_path,
          "aggregate_results" = aggregate_results,
          "export_collection" = export_collection),
        list(...))))
  }
)


# .export (familiarDataElementPartialDependence) -------------------------------
setMethod(
  ".export",
  signature(x = "familiarDataElementPartialDependence"),
  function(
    x,
    x_list,
    aggregate_results = FALSE,
    ...) {
    
    if (aggregate_results) {
      x_list <- .compute_data_element_estimates(x_list)
    }
    
    # Determine identifiers that should be merged. Since the feature values of
    # the x and y features may be different (e.g. numeric and factor), merging
    # them would cause features values to merged incorrectly.
    merging_identifiers <- setdiff(
      names(x@identifiers),
      c("feature_x", "feature_y"))
    
    # Merge data elements.
    x <- merge_data_elements(
      x = x_list,
      as_data = merging_identifiers,
      as_grouping_column = TRUE,
      force_data_table = TRUE)
    
    return(x)
  }
)

#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
#' @include FamiliarDataComputationPredictionData.R

# predictionTableRegression ----------------------------------------------------
setClass(
  "predictionTableRegression",
  contains = "familiarDataElementPredictionTable"
)

# predictionTableSurvival ------------------------------------------------------
setClass(
  "predictionTableSurvival",
  contains = "familiarDataElementPredictionTable",
  slots = list(
    "censored" = "character",
    "event" = "character"
  ),
  prototype = list(
    "censored" = NA_character_,
    "event" = NA_character_
  )
)

# predictionTableSurvivalHazardRatio -------------------------------------------
setClass(
  "predictionTableSurvivalHazardRatio",
  contains = "predictionTableSurvival"
)

# predictionTableSurvivalCumulativeHazard --------------------------------------
setClass(
  "predictionTableSurvivalCumulativeHazard",
  contains = "predictionTableSurvival"
)

# predictionTableSurvivalTime --------------------------------------------------
setClass(
  "predictionTableSurvivalTime",
  contains = "predictionTableSurvival"
)

# predictionTableSurvivalProbability -------------------------------------------
setClass(
  "predictionTableSurvivalProbability",
  contains = "predictionTableSurvival"
)

# predictionTableGrouping ------------------------------------------------------
setClass(
  "predictionTableGrouping",
  contains = "familiarDataElementPredictionTable",
  slots = list(
    "groups" = "character"
  ),
  prototype = list(
    "groups" = NA_character_
  )
)

# predictionTableRiskGroups ----------------------------------------------------
setClass(
  "predictionTableRiskGroups",
  contains = "predictionTableGrouping"
)

# predictionTableClassification ------------------------------------------------
setClass(
  "predictionTableClassification",
  contains = "familiarDataElementPredictionTable",
  slots = list(
    "classes" = "character",
    "thresholds" = "ANY"
  ),
  prototype = list(
    "classes" = NA_character_,
    "thresholds" = NULL
  )
)

# predictionTableNovelty -------------------------------------------------------
setClass(
  "predictionTableNovelty",
  contains = "familiarDataElementPredictionTable"
)


as_prediction_table <- function(
    x,
    type,
    y = waiver(),
    batch_id = waiver(),
    sample_id = waiver(),
    series_id = waiver(),
    repetition_id = waiver(),
    time = waiver(),
    classes = waiver(),
    censored_label = waiver(),
    event_label = waiver(),
    data = NULL) {
  
  # Create skeleton of object based on the provided type.
  if (type == "regression") {
    object <- methods::new("predictionTableRegression")
    
  } else if (type == "classification") {
    object <- methods::new("predictionTableClassification")
  
  } else if (type == "hazard_ratio") {
    object <- methods::new("predictionTableSurvivalHazardRatio")
    
  } else if (type == "cumulative_hazard") {
    object <- methods::new("predictionTableSurvivalCumulativeHazard")
  
  } else if (type == "expected_survival_time") {
    object <- methods::new("predictionTableSurvivalTime")
    
  } else if (type == "survival_probability") {
    object <- methods::new("predictionTableSurvivalProbability")
  
  } else if (type == "grouping") {
    object <- methods::new("predictionTableGrouping")
    
  } else if (type == "risk_stratification") {
    object <- methods::new("predictionTableRiskGroups")
    
  } else if (type == "novelty") {
    object <- methods::new("predictionTableNovelty")
    
  } else {
    rlang::abort(
      message = paste0(
        "The type argument was not recognized: ", type,
        "One of regression, classification, hazard_ratio, cumulative_hazard, ",
        "expected_survival_time, survival_probability, grouping, 
        risk_stratification or novelty is expected."
      )
    )
  }
  
  if (is_empty(x)) {
    return(object)
  }
  
  # Use x to set prediction_data attribute.
  object@prediction_data <- data.table::as.data.table(x)
  
  # Use y to set reference_data attribute.
  if (!is.waive(y) && !is.null(y)) {
    if (survival::is.Surv(y)) {
      object@reference_data <- data.table::data.table(
        "outcome_time" = y[, 1],
        "outcome_event" = y[, 2]
      )
    } else {
      object@reference_data <- as.data.table(y)
    }
    
  } else if (is(data, "dataObject")) {
    y <- data@data[, mget(.get_outcome_columns(data@outcome_type))]
    object@reference_data <- data.table::copy(y)
  }
  
  # Use batch_id, sample_id, series_id and repetition_id to set identifier_data
  # attribute.
  if (is.waive(batch_id) && is.waive(sample_id) && is.waive(series_id) && 
      is.waive(repetition_id) && is(data, "dataObject")) {
    
    batch_id <- data@data[[get_id_columns(single_column = "batch")]]
    sample_id <- data@data[[get_id_columns(single_column = "sample")]]
    series_id <- data@data[[get_id_columns(single_column = "series")]]
    repetition_id <- data@data[[get_id_columns(single_column = "repetition")]]
  }
  
  if (is.waive(batch_id)) {
    batch_id <- rep_len("generic_batch", nrow(object@prediction_data))
  }
  
  if (is.waive(sample_id)) {
    sample_id <- paste0("generic_sample_", seq_len(nrow(object@prediction_data)))
  }
  
  if (is.waive(series_id)) {
    series_id <- rep_len("1", nrow(object@prediction_data))
  }
  
  if (is.waive(repetition_id)) {
    repetition_id <- rep_len("1", nrow(object@prediction_data))
  }
  
  object@identifier_data <- data.table::data.table(
    "V1" = batch_id, "V2" = sample_id, "V3" = series_id, "V4" = repetition_id
  )
  
  data.table::setnames(object@identifier_data, new = get_id_columns())
  
  # Use time to set time attribute of prediction tables that have it.
  if (methods::.hasSlot(object, "time") && !is.waive(time)) {
    sapply(
      time,
      .check_number_in_valid_range,
      var_name = "time",
      range = c(0.0, Inf),
      closed = c(FALSE, TRUE))
    
    add_data_element_identifier(
      x = proto_data_element,
      evaluation_time = time
    )
  }
  
  # Use classes to set classes attribute of prediction tables that have it.
  if (methods::.hasSlot(object, "classes")) {
    if (is.waive(classes) && is(data, "dataObject")) {
      classes <- get_outcome_class_levels(data)
      
    } else if (is.waive(classes) && !is_empty(object@reference_data)) {
      if (is.factor(object@reference_data[[1]])) {
        classes <- levels(object@reference_data[[1]])
      } else {
        classes <- unique_na(object@reference_data[[1]])
      }
      
    } else if (is.waive(classes)) {
      classes <- paste0("class_", seq_len(ncol(object@prediction_data)))
    }
    
    object@classes <- classes
  }
  
  # Use censored_label to set censored attribute of prediction tables that have
  # it.
  if (methods::.hasSlot(object, "censored")) {
    if (is.waive(censored_label) && is(data, "dataObject")) {
      censored_label <- data@outcome_info@censored
      
    } else if (is.waive(censored_label)) {
      censored_label <- .get_available_default_censoring_indicator()
    }
    
    object@censored <- censored_label
  }
  
  # Use event_label to set event attribute of prediction tables that have it.
  if (methods::.hasSlot(object, "event")) {
    if (is.waive(event_label) && is(data, "dataObject")) {
      event_label <- data@outcome_info@event
      
    } else if (is.waive(event_label)) {
      event_label <- .get_available_default_event_indicator()
    }
    
    object@event <- event_label
  }
  
  # Infer outcome_type based on known information.
  if (is(data, "dataObject")) {
    object@outcome_type <- data@outcome_type
  }
  
  # Check the prediction table object for consistency.
  object <- complete_prediction_table(object)
  
  return(object)
}


# complete_prediction_table (generic) ------------------------------------------

setGeneric(
  "complete_prediction_table",
  function(object, ...) standardGeneric("complete_prediction_table")
)

# complete_prediction_table (general) ------------------------------------------
setMethod(
  "complete_prediction_table",
  signature(object = "familiarDataElementPredictionTable"),
  function(object) {
    # Check that prediction_data, identifier_data and reference_data have the
    # same number of instances.
    if (is_empty(object)) return(object)
    
    n_instances <- nrow(object@prediction_data)
    
    if (!is_empty(object@reference_data)) {
      if (n_instances != nrow(object@reference_data)) {
        rlang::abort(
          message = paste0(
            "The number of instances of predicted values (",
            n_instances, ") do not match the number of reference values (",
            nrow(object@reference_data), ")."),
          class = "prediction_table_error"
        )
      }
    }
    
    if (!is_empty(object@identifier_data)) {
      if (n_instances != nrow(object@identifier_data)) {
        rlang::abort(
          message = paste0(
            "The number of instances of predicted values (",
            n_instances, ") do not match the number of instances determined ",
            "from identifier data (", nrow(object@reference_data), ")."),
          class = "prediction_table_error"
        )
      }
    }
    
    return(object)
  }
)

## complete_prediction_table (regression) --------------------------------------
setMethod(
  "complete_prediction_table",
  signature(object = "predictionTableRegression"),
  function(object) {
    
    # Pass to superclass method first.
    object <- callNextMethod()
    
    if (is_empty(object)) return(object)
    
    # Check outcome type
    if (is.na(object@outcome_type)) object@outcome_type <- "continuous"
    if (object@outcome_type != "continuous") {
      ..error_reached_unreachable_code(
        "The outcome type of predictionTableRegression objects should be \"continuous\".")
    }
    
    # Check that one set of prediction data are provided.
    if (ncol(object@prediction_data) != 1) {
      rlang::abort(
        message = paste0(
          "Only one set of predicted values was expected, but (",
          ncol(object@prediction_data), ") sets were provided."),
        class = "prediction_table_error"
      )
    }
    
    # Update column name of prediction_data to the standard value.
    setnames(object@prediction_data, new = "predicted_outcome")
    
    # Check that prediction data are numeric.
    if (!is.numeric(object@prediction_data$predicted_outcome)) {
      rlang::abort(
        message = paste0(
          "Predicted values are expected to be numeric, but data with class ",
          paste0(class(object@prediction_data$predicted_outcome), collapse = ", "),
          " were encountered."),
        class = "prediction_table_error"
      )
    }
    
    if (!is_empty(object@reference_data)) {
      # Check that one set of reference data are provided.
      if (ncol(object@reference_data) != 1) {
        rlang::abort(
          message = paste0(
            "Only one set of reference values was expected, but (",
            ncol(object@reference_data), ") sets were provided."),
          class = "prediction_table_error"
        )
      }
      
      # Update column name of reference_data to the standard value.
      outcome_column <- get_outcome_columns(object@outcome_type)
      setnames(object@reference_data, new = outcome_column)
      
      # Check that reference data are numeric.
      if (!is.numeric(object@reference_data[[outcome_column]])) {
        rlang::abort(
          message = paste0(
            "Reference values are expected to be numeric, but data with class ",
            class(object@v[[outcome_column]]), " were encountered."),
          class = "prediction_table_error"
        )
      }
    }
    
    return(object)
  }
)


## complete_prediction_table (classification) ----------------------------------
setMethod(
  "complete_prediction_table",
  signature(object = "predictionTableClassification"),
  function(object) {
    
    ..in_valid_range <- function(x) {
      return (all(x >= 0.0, na.rm = TRUE) && all(x <= 1.0, na.rm = TRUE))
    }
    
    ..test_columns_numeric <- function(object) {
      if (!all(sapply(object@prediction_data, is.numeric))) {
        rlang::abort(
          message = paste0(
            "Predicted values are expected to be numeric, but data with classes ",
            paste0(unique(sapply(object@prediction_data, class)), collapse = ", "),
            " were encountered."),
          class = "prediction_table_error"
        )
      }
    }
    
    ..test_columns_probabilities <- function(object) {
      if (!all(sapply(object@prediction_data, ..in_valid_range))) {
        rlang::abort(
          message = paste0(
            "Predicted values are expected to be probabilities between 0.0 and 1.0, ",
            "but values outside this range were found."),
          class = "prediction_table_error"
        )
      }
    }
    
    # Pass to superclass method first.
    object <- callNextMethod()
    
    if (is_empty(object)) return(object)
    
    # Check outcome type
    if (is.na(object@outcome_type)) {
      object@outcome_type <- ifelse(length(object@classes) == 2, "binomial", "multinomial")
    }
    if (object@outcome_type %in% c("binomial", "multinomial")) {
      ..error_reached_unreachable_code(paste0(
        "The outcome type of predictionTableClassification objects should be 
        \"binomial\ or \"multinomial\"."))
    }
    
    if (object@outcome_type == "binomial") {
      # Under binomial, either 1 or 2 columns can be specified, both of which
      # should contain numeric values between 0.0 and 1.0.
      
      # Check that one or two sets of prediction data are provided.
      if (ncol(object@prediction_data) > 2) {
        rlang::abort(
          message = paste0(
            "For binomial (two-class) endpoints, either one or two sets of ",
            "predicted values are expected. However (",
            ncol(object@prediction_data), ") sets were provided."),
          class = "prediction_table_error"
        )
      }
      
      # Check that all columns are numeric.
      ..test_columns_numeric(object)
      
      # Check that all columns contain values between 0.0 and 1.0.
      ..test_columns_probabilities(object)
      
      if (length(object@classes) != 2) {
        rlang::abort(
          message = paste0(
            "Two classes are expected for binomial (two-class) but ",
            length(object@classes), " were found: ",
            paste0(object@classes, collapse = ", ")),
          class = "prediction_table_error"
        )
      }
      
      # Insert complementary column. First we determine the name of the
      # associated class.
      if (ncol(object@prediction_data) == 1) {
        if (all(colnames(object@prediction_data) %in% object@classes)) {
          positive_class <- colnames(object@prediction_data)
          
        } else {
          positive_class <- tail(object@classes, n = 1L)
          
          rlang::warn(
            message = paste0(
              "The positive class cannot be directly inferred from names of the ",
              "provided prediction data, and is assumed to be ", positive_class, "."),
            class = "prediction_table_warning"
          )
          
          data.table::setnames(object@prediction_data, new = positive_class)
        }
        
        negative_class <- setdiff(object@classes, positive_class)
        object@prediction_data[, (negative_class) := 1.0 - get(positive_class)]
      }
      
      # Update column names.
      if (!any(colnames(object@prediction_data) %in% object@classes)) {
        # Here neither of the sets of predicted values can be directly matched 
        # against a known class, and we infer that these appear in the order of 
        # classes.
        
        rlang::warn(
          message = paste0(
            "Neither set of predicted values (",
            paste0(colnames(object@prediction_data, collapse = ", ")),
            ") can be directly assigned to a corresponding class (",
            paste0(object@classes, collapse = ", "), "). We assume these sets ",
            "are ordered according to the provided classes."),
          class = "prediction_table_warning"
        )
        data.table::setnames(object@prediction_data, new = object@classes)
        
      } else if (!all(colnames(object@prediction_data) %in% object@classes)) {
        # Here one of the sets of predicted values can be matched against a
        # known class, but the other cannot. We assume that the unmatched class
        # corresponds to the unidentified set.
        
        unassigned_class <- setdiff(object@classes, colnames(object@prediction_data))
        unassigned_column <- setdiff(colnames(object@prediction_data), object@classes)
        rlang::warn(
          message = paste0(
            "One of the sets of predicted values (", unassigned_column,
            ") cannot be directly assigned to a corresponding class (",
            paste0(object@classes, collapse = ", "), "). We assume that this ",
            "set corresponds to the ", unassigned_class,  " class."),
          class = "prediction_table_warning"
        )
        data.table::setnames(
          x = object@prediction_data, 
          old = unassigned_column, 
          new = unassigned_class)
      }
      
    } else {
      # Under multinomial, 2 or more columns can be specified, all of which
      # should contain numeric values between 0.0 and 1.0.
      
      # Check that all columns are numeric.
      ..test_columns_numeric(object)
      
      # Check that all columns contain values between 0.0 and 1.0.
      ..test_columns_probabilities(object)
      
      # Check that at least two sets of prediction data are provided.
      if (ncol(object@prediction_data) <2) {
        rlang::abort(
          message = paste0(
            "For multinomial (multi-class) endpoints, at least two sets of ",
            "predicted values are expected. However (",
            ncol(object@prediction_data), ") sets were provided."),
          class = "prediction_table_error"
        )
      }
      
      # Check that at least two classes are provided.
      if (length(object@classes) < 2) {
        rlang::abort(
          message = paste0(
            "At least two classes are expected for multinomial (multi-class) ",
            "endpoints, but only ", length(object@classes), " were found: ",
            paste0(object@classes, collapse = ", ")),
          class = "prediction_table_error"
        )
      }
      
      # Check that the number of classes and provided sets match.
      if (length(object@classes) != length(colnames(object@prediction_data))) {
        rlang::abort(
          message = paste0(
            "The same number of classes and sets of predicted values are ",
            "expected. Found: ", length(object@classes), " classes and ",
            length(colnames(object@prediction_data)), " sets."),
          class = "prediction_table_error"
        )
      }
      
      # Update column names.
      if (!any(colnames(object@prediction_data) %in% object@classes)) {
        # Here neither of the sets of predicted values can be directly matched 
        # against a known class, and we infer that these appear in the order of 
        # classes.
        
        rlang::warn(
          message = paste0(
            "Neither set of predicted values (",
            paste0(colnames(object@prediction_data, collapse = ", ")),
            ") can be directly assigned to a corresponding class (",
            paste0(object@classes, collapse = ", "), "). We assume these sets ",
            "are ordered according to the provided classes."),
          class = "prediction_table_warning"
        )
        data.table::setnames(object@prediction_data, new = object@classes)
        
      } else if (!all(colnames(object@prediction_data) %in% object@classes)) {
        # Here at least one of the sets of predicted values can be matched
        # against a known class, but some others cannot. We cannot assume
        # anything.
        
        unassigned_class <- setdiff(object@classes, colnames(object@prediction_data))
        unassigned_column <- setdiff(colnames(object@prediction_data), object@classes)
        rlang::abort(
          message = paste0(
            "Some sets of predicted values (",
            paste0(unassigned_column, collapse = ", "),
            ") cannot be directly assigned to a corresponding class (",
            paste0(object@classes, collapse = ", "), ". Please name all ",
            "sets of predicted values according to their class."),
          class = "prediction_table_warning"
        )
        data.table::setnames(
          x = object@prediction_data, 
          old = unassigned_column, 
          new = unassigned_class)
      }
    }
    
    # Order columns in prediction data.
    data.table::setcolorder(object@prediction_data, object@classes)
    
    if (!is_empty(object@reference_data)) {
      # Check that one set of reference data are provided.
      if (ncol(object@reference_data != 1)) {
        rlang::abort(
          message = paste0(
            "Only one set of reference values was expected, but (",
            ncol(object@reference_data), ") sets were provided."),
          class = "prediction_table_error"
        )
      }
      
      # Update column name of reference_data to the standard value.
      outcome_column <- get_outcome_columns(object@outcome_type)
      setnames(object@reference_data, new = outcome_column)
      
      # Check that reference data only contains the known classes.
      missing_classes <- setdiff(
        unique_na(object@reference_data[[outcome_column]]),
        object@classes)
        
      if (length(missing_classes) > 0) {
        rlang::abort(
          message = paste0(
            "One or more reference values are present that are not listed as ",
            "classes: ", paste0(missing_classes, collapse = ", ")),
          class = "prediction_table_error"
        )
      }
    }
    
    return(object)
  }
)


## complete_prediction_table (survival) ----------------------------------------
setMethod(
  "complete_prediction_table",
  signature(object = "predictionTableSurvival"),
  function(object) {
    
    # Pass to superclass method first.
    object <- callNextMethod()
    
    if (is_empty(object)) return(object)
    
    # Check outcome type
    if (is.na(object@outcome_type)) object@outcome_type <- "survival"
    if (object@outcome_type != "survival") {
      ..error_reached_unreachable_code(
        "The outcome type of predictionTableSurvival objects should be \"survival\".")
    }
    
    # Check that one set of prediction data are provided.
    if (ncol(object@prediction_data) != 1) {
      rlang::abort(
        message = paste0(
          "Only one set of predicted values was expected, but (",
          ncol(object@prediction_data), ") sets were provided."),
        class = "prediction_table_error"
      )
    }
    
    # Update column name of prediction_data to the standard value.
    setnames(object@prediction_data, new = "predicted_outcome")
    
    # Check that prediction data are numeric.
    if (!is.numeric(object@prediction_data$predicted_outcome)) {
      rlang::abort(
        message = paste0(
          "Predicted values are expected to be numeric, but data with class ",
          paste0(class(object@prediction_data$predicted_outcome), collapse = ", "),
          " were encountered."),
        class = "prediction_table_error"
      )
    }
    
    if (!is_empty(object@reference_data)) {
      # Check that two sets of reference data are provided (time, status).
      if (ncol(object@reference_data) != 2) {
        rlang::abort(
          message = paste0(
            "Both time and status data are expected, but (",
            ncol(object@reference_data), ") set(s) of reference data were provided."),
          class = "prediction_table_error"
        )
      }
      
      # Update column name of reference_data to the standard value.
      outcome_column <- get_outcome_columns(object@outcome_type)
      setnames(object@reference_data, new = outcome_column)
      
      # Check that the time column is numeric and positive.
      time_column <- object@reference_data[[outcome_column[1]]]
      if (!is.numeric(time_column)) {
        rlang::abort(
          message = paste0(
            "Observed time is expected to be numeric, but data with class ",
            class(time_column, collapse = ", "),
            " were encountered."),
          class = "prediction_table_error"
        )
      }
      
      if (any(time_column < 0.0, na.rm = TRUE)) {
        negative_values <- time_column
        negative_values <- negative_values[is.finite(negative_values)]
        negative_values <- negative_values[negative_values < 0.0]
        
        rlang::abort(
          message = paste0(
            "Observed time can not be negative, but negative values were found: ",
            paste_sh(negative_values)),
          class = "prediction_table_error"
        )
      }
      
      # Check that the status columns contains 0s and 1s, or can be converted to
      # such values.
      status_column <- object@reference_data[[outcome_column[2]]]
      status_indicators <- as.character(unique_na(status_column))
      
      if (length(status_indicators) > 2) {
        rlang::abort(
          message = paste0(
            "Event status may only contain two values that indicate censoring ",
            "and event. Found: ", paste_sh(status_indicators)),
          class = "prediction_table_error"
        )
      }
      
      if (all(status_indicators %in% c(object@censored, object@event))) {
        # Convert to 0s and 1s.
        new_status_column <- rep_len(1L, length(status_column))
        new_status_column[status_column %in% object@censored] <- 0L
        
        object@reference_data <- data.table::copy(object@reference_data)
        object@reference_data[[outcome_column[2]]] <- new_status_column
        
      } else if (all(status_indicators %in% c("0", "1"))) {
        # Do nothing.
        
      } else if (all(status_indicators %in% c(
        .get_available_default_censoring_indicator()),
        .get_available_default_event_indicator())) {
        
        # Convert to 0s and 1s.
        new_status_column <- rep_len(1L, length(status_column))
        new_status_column[status_column %in% .get_available_default_censoring_indicator()] <- 0L
        
        object@reference_data <- data.table::copy(object@reference_data)
        object@reference_data[[outcome_column[2]]] <- new_status_column
        
      } else {
        
        # Find indicator values
        if (!all(is.na(c(object@censored, object@event)))) {
          indicator_values <- c(object@censored, object@event)
        } else {
          indicator_values <- c(
            .get_available_default_censoring_indicator(),
            .get_available_default_event_indicator())
        }
        
        rlang::abort(
          message = paste0(
            "Could not map potential status and event indicators (",
            paste_s(indicator_values), ") to values present in the status column (",
            paste_sh(status_indicators), ")."),
          class = "prediction_table_error"
        )
      }
    }
    
    return(object)
  }
)

## complete_prediction_table (novelty) -----------------------------------------
setMethod(
  "complete_prediction_table",
  signature(object = "predictionTableNovelty"),
  function(object) {
    
    # Pass to superclass method first.
    object <- callNextMethod()
    
    if (is_empty(object)) return(object)
    
    # Check outcome type
    object@outcome_type <- "unsupervised"
    
    # Check that one set of prediction data are provided.
    if (ncol(object@prediction_data) != 1) {
      rlang::abort(
        message = paste0(
          "Only one set of predicted values was expected, but (",
          ncol(object@prediction_data), ") sets were provided."),
        class = "prediction_table_error"
      )
    }
    
    # Update column name of prediction_data to the standard value.
    setnames(object@prediction_data, new = "novelty")
    
    # Check that prediction data are numeric.
    if (!is.numeric(object@prediction_data$novelty)) {
      rlang::abort(
        message = paste0(
          "Predicted values are expected to be numeric, but data with class ",
          paste0(class(object@prediction_data$novelty), collapse = ", "),
          " were encountered."),
        class = "prediction_table_error"
      )
    }
    
    return(object)
  }
)

## complete_prediction_table (grouping) ----------------------------------------
setMethod(
  "complete_prediction_table",
  signature(object = "predictionTableGrouping"),
  function(object) {
    
    # Pass to superclass method first.
    object <- callNextMethod()
    
    if (is_empty(object)) return(object)
    
    # Check outcome type
    object@outcome_type <- "unsupervised"
    
    # Check that one set of prediction data are provided.
    if (ncol(object@prediction_data != 1)) {
      rlang::abort(
        message = paste0(
          "Only one set of predicted values was expected, but (",
          ncol(object@prediction_data), ") sets were provided."),
        class = "prediction_table_error"
      )
    }
    
    # Update column name of prediction_data to the standard value.
    setnames(object@prediction_data, new = "group")
    
    # Check that prediction data contains the groups, or set groups if absent.
    if (all(!is.na(object@groups))) {
      group_labels <- unique_na(object@prediction_data$group)
      
      if (!all(group_indicators %in% object@groups)) {
        unknown_group_label <- setdiff(group_labels, object@groups)
        
        rlang::abort(
          message = paste0(
            "The set of predicted values contains group labels that could ",
            "not be matched to provided labels. The following labels were not ",
            "recognised: ", paste_s(unknown_group_label), ". ",
            "The following labels were expected: ", paste_s(object@groups)),
          class = "prediction_table_error"
        )
      }
    } else {
      
      if (is.factor(object@prediction_data$group)) {
        object@groups <- levels(object@prediction_data$group)
        
      } else {
        group_labels <- unique_na(object@prediction_data$group)
        if (length(group_labels) > NULL) object@groups <- sort(group_labels)
      }
    }
    
    return(object)
  }
)


# .is_merged_prediction_table (generic) ----------------------------------------
setGeneric(".is_merged_prediction_table", function(x, ...) standardGeneric(".is_merged_prediction_table"))

## .is_merged_prediction_table (general) ---------------------------------------
setMethod(
  ".is_merged_prediction_table",
  signature(x = "familiarDataElementPredictionTable"),
  function(x, ...) {
    return(!is_empty(x@grouping_column))
  }
)


# is_empty ---------------------------------------------------------------------
setMethod(
  "is_empty",
  signature(x = "familiarDataElementPredictionTable"),
  function(x, ...) {
    if (.is_merged_prediction_table(x)) {
      return(is_empty(x@data))
    } else {
      return(is_empty(x@prediction_data))
    }
  }
)




all_predictions_valid <- function(prediction_table) {
  # Return results when checking with the "all" function.
  return(.check_predictions_valid(
    object = prediction_table,
    check_type = "all"
  ))
}



any_predictions_valid <- function(prediction_table) {
  # Return results when checking with the "any" function.
  
  return(.check_predictions_valid(
    object = prediction_table,
    check_type = "any"
  ))
}



# .check_predictions_valid (generic) -------------------------------------------
setGeneric(".check_predictions_valid", function(object, ...) standardGeneric(".check_predictions_valid"))


# .check_predictions_valid (general) ----------------------------------------
setMethod(
  ".check_predictions_valid",
  signature(object = "familiarDataElementPredictionTable"),
  function(object, check_type) {
    
    if (is_empty(object)) return(FALSE)

    check_fun <- switch(
      check_type,
      "all" = all,
      "any" = any
    )
    
    data_slot <- ifelse(.is_merged_prediction_table(object), "data", "prediction_data")
    
    return(check_fun(is.finite(slot(object, data_slot)$predicted_outcome)))
  }
)


# .check_predictions_valid (classification) ------------------------------------
setMethod(
  ".check_predictions_valid",
  signature(object = "predictionTableClassification"),
  function(object, check_type) {
    
    if (is_empty(object)) return(FALSE)
    
    check_fun <- switch(
      check_type,
      "all" = all,
      "any" = any
    )
    
    data_slot <- ifelse(.is_merged_prediction_table(object), "data", "prediction_data")
    
    return(all(sapply(
      slot(object, data_slot), 
      function(x, check_fun) (check_fun(is.finite(x))),
      check_fun = check_fun)
    ))
  }
)


# .check_predictions_valid (novelty) ----------------------------------------
setMethod(
  ".check_predictions_valid",
  signature(object = "predictionTableNovelty"),
  function(object, check_type) {
    
    if (is_empty(object)) return(FALSE)
    
    check_fun <- switch(
      check_type,
      "all" = all,
      "any" = any
    )
    
    data_slot <- ifelse(.is_merged_prediction_table(object), "data", "prediction_data")
    
    return(check_fun(is.finite(slot(object, data_slot)$novelty)))
  }
)


# .check_predictions_valid (grouping) ----------------------------------------
setMethod(
  ".check_predictions_valid",
  signature(object = "predictionTableGrouping"),
  function(object, check_type) {
    
    if (is_empty(object)) return(FALSE)
    
    check_fun <- switch(
      check_type,
      "all" = all,
      "any" = any
    )
    
    data_slot <- ifelse(.is_merged_prediction_table(object), "data", "prediction_data")
    
    return(check_fun(!is.na(slot(object, data_slot)$group)))
  }
)



# remove_invalid_predictions (generic) -----------------------------------------
setGeneric("remove_invalid_predictions", function(object, ...) standardGeneric("remove_invalid_predictions"))


# remove_invalid_predictions (general) -----------------------------------------
setMethod(
  "remove_invalid_predictions",
  signature(object = "familiarDataElementPredictionTable"),
  function(object, ...) {
    if (is_empty(object)) return(object)
    
    if (.is_merged_prediction_table(object)) {
      instance_mask <- is.finite(object@data$predicted_outcome)
      
      object@data <- object@data[instance_mask, ]
      
    } else {
      instance_mask <- is.finite(object@prediction_data$predicted_outcome)
      
      object@identifier_data <- object@identifier_data[instance_mask, ]
      object@reference_data <- object@reference_data[instance_mask, ]
      object@prediction_data <- object@prediction_data[instance_mask, ]
    }
    
    return(object)
  }
)


# remove_invalid_predictions (classification) ----------------------------------
setMethod(
  "remove_invalid_predictions",
  signature(object = "predictionTableClassification"),
  function(object, ...) {
    if (is_empty(object)) return(object)
    
    if (.is_merged_prediction_table(object)) {
      instance_mask <- as.logical(
        do.call(pmin, lapply(object@data[, mget(object@value_column)], is.finite))
      )
      
      object@data <- object@data[instance_mask, ]
      
    } else {
      instance_mask <- as.logical(
        do.call(pmin, lapply(object@prediction_data, is.finite))
      )
      
      object@identifier_data <- object@identifier_data[instance_mask, ]
      object@reference_data <- object@reference_data[instance_mask, ]
      object@prediction_data <- object@prediction_data[instance_mask, ]
    }
    
    return(object)
  }
)


# remove_invalid_predictions (novelty) -----------------------------------------
setMethod(
  "remove_invalid_predictions",
  signature(object = "predictionTableNovelty"),
  function(object, ...) {
    if (is_empty(object)) return(object)
    
    if (.is_merged_prediction_table(object)) {
      instance_mask <- is.finite(object@data$novelty)
      
      object@data <- object@data[instance_mask, ]
      
    } else {
      instance_mask <- is.finite(object@prediction_data$novelty)
      
      object@identifier_data <- object@identifier_data[instance_mask, ]
      object@reference_data <- object@reference_data[instance_mask, ]
      object@prediction_data <- object@prediction_data[instance_mask, ]
    }
    
    return(object)
  }
)


# remove_invalid_predictions (general) -----------------------------------------
setMethod(
  "remove_invalid_predictions",
  signature(object = "predictionTableGrouping"),
  function(object, ...) {
    if (is_empty(object)) return(object)
    
    if (.is_merged_prediction_table(object)) {
      instance_mask <- is.finite(object@data$group)
      
      object@data <- object@data[instance_mask, ]
      
    } else {
      instance_mask <- is.finite(object@prediction_data$group)
      
      object@identifier_data <- object@identifier_data[instance_mask, ]
      object@reference_data <- object@reference_data[instance_mask, ]
      object@prediction_data <- object@prediction_data[instance_mask, ]
    }
    
    return(object)
  }
)



# filter_missing_outcome (general) ---------------------------------------------
setMethod(
  "filter_missing_outcome",
  signature(data = "familiarDataElementPredictionTable"),
  function(data, is_validation = FALSE, ...) {
    # This method removes instances where reference data are missing.
    # See other methods for filter_missing_outcome in DataObject.R.
    
    # Check if data itself or the reference data contained therein is empty.
    if (is_empty(data)) return(data)
    
    merged_table <- .is_merged_prediction_table(data)
    data_slot <- ifelse(merged_table, "data", "reference_data")
    
    if (is_empty(data@reference_data)) return(data)
    
    if (data@outcome_type %in% c("survival", "competing_risk")) {
      if (merged_table && !all(c("outcome_time", "outcome_event") %in% data@grouping_column)) return(data)
      
      outcome_is_valid <- is_valid_data(slot(object, data_slot)$outcome_time) &
        is_valid_data(slot(object, data_slot)$outcome_event)
      
    } else {
      if (merged_table && !("outcome" %in% data@grouping_column)) return(data)
      
      outcome_is_valid <- is_valid_data(slot(object, data_slot)$outcome)
    }
    
    if (is_validation) {
      # Check whether all outcome information is missing for validation. It may
      # be a prospective study. In that case, keep all data.
      if (all(!outcome_is_valid)) outcome_is_valid <- !outcome_is_valid
    }
    
    # Filter instances.
    if (merged_table) {
      data@data <- data@data[outcome_is_valid, ]
      
    } else {
      data@identifier_data <- data@identifier_data[outcome_is_valid, ]
      data@reference_data <- data@reference_data[outcome_is_valid, ]
      data@prediction_data <- data@prediction_data[outcome_is_valid, ]
    }
    
    return(data)
  }
)



setMethod(
  ".as_data_table",
  signature(x = "familiarDataElementPredictionTable"),
  function(x, ...) {
    x <- .merge_slots_into_data(x)
    
    return(x@data)
  }
)



# .merge_slots_into_data (general) ---------------------------------------------
setMethod(
  ".merge_slots_into_data",
  signature(x = "familiarDataElementPredictionTable"),
  function(x, ...) {
    # Merge identifier_data, reference_data and prediction_data and set
    # value column and grouping columns.
    
    # Do not merge prediction tables that are already merged.
    if (.is_merged_prediction_table(x)) return(x)
    
    # Set grouping and value columns.
    x@grouping_column <- c(colnames(x@identifier_data), colnames(x@reference_data))
    x@value_column <- colnames(x@prediction_data)
    
    # Merge data and set data attribute.
    x@data <- cbind(x@identifier_data, x@reference_data, x@prediction_data)
    
    # Empty data slots.
    slot(x, "identifier_data", check = FALSE) <- NULL
    slot(x, "reference_data", check = FALSE) <- NULL
    slot(x, "prediction_data", check = FALSE) <- NULL

    return(x)
  }
)


# .merge_slots_into_data (null) ------------------------------------------------
setMethod(
  ".merge_slots_into_data",
  signature(x = "NULL"),
  function(x, ...) {
    return(NULL)
  }
)


# .extract_slots_from_data (general) -------------------------------------------
setMethod(
  ".extract_slots_from_data",
  signature(x = "familiarDataElementPredictionTable"),
  function(x, ...) {
    # Extract identifier_data, reference_data and prediction_data from data.
    
    if (!.is_merged_prediction_table(x)) return(x)
    
    # Get reference columns.
    reference_columns <- get_outcome_columns(x@outcome_type)
    
    # Get identifier columns.
    identifier_columns <- setdiff(x@grouping_column, reference_columns)
    
    # Set identifier data.
    x@identifier_data <- x@data[, mget(identifier_columns)]
    
    # Set or generate reference columns.
    if (is.null(reference_columns)) {
      # Pass. Do not set reference data if none are required.
      
    } else if (all(reference_columns %in% colnames(x@data))) {
      x@reference_data <- x@data[, mget(reference_columns)]
        
    } else {
      # Pass do not set reference data it is missing.
    }
    
    # Set prediction data.
    x@prediction_data <- x@data[, mget(x@value_column)]
    
    # Reset grouping and value column attributes, and empty data.
    slot(x, "grouping_column", check = FALSE) <- NULL
    slot(x, "value_column") <- NA_character_
    slot(x, "data", check = FALSE) <- NULL
    
    return(x)
  }
)


# .extract_slots_from_data (null) ----------------------------------------------
setMethod(
  ".extract_slots_from_data",
  signature(x = "NULL"),
  function(x, ...) {
    
    return(NULL)
  }
)


# ..compute_ensemble_prediction_estimates (general) ----------------------------
setMethod(
  "..compute_ensemble_prediction_estimates",
  signature(x = "familiarDataElementPredictionTable"),
  function(x, data, ensemble_method, ...) {
    
    # Suppress NOTES due to non-standard evaluation in data.table
    estimation_type <- NULL
    
    # Check if ensembling is actually required -- computing ensemble values is 
    # somewhat expensive because it is done for each subset of data.
    if (data.table::uniqueN(data, by = x@grouping_column) == nrow(data)) {
      return(data[estimation_type == "point", mget(c(x@grouping_column, x@value_column))])
    }
    
    # Compute aggregate values.
    prediction_data <- data[
      ,
      ...compute_ensemble_estimates(
        x = .SD,
        prediction_columns = x@value_column,
        confidence_level = x@confidence_level,
        percentiles = x@percentiles,
        ensemble_method = ensemble_method,
        ...
      ),
      by = c(x@grouping_column),
      .SDcols = c("estimation_type", x@value_column)
    ]
    
    return(prediction_data)
  }
)


# ..compute_ensemble_prediction_estimates (general groups) ---------------------
setMethod(
  "..compute_ensemble_prediction_estimates",
  signature(x = "predictionTableGrouping"),
  function(x, data, ensemble_method, ...) {
    # Suppress NOTES due to non-standard evaluation in data.table
    estimation_type <- NULL
    
    # Check if ensembling is actually required -- the computation is somewhat
    # expensive because it is done on the subset of data.
    if (data.table::uniqueN(data, by = x@grouping_column) == nrow(data)) {
      return(data[estimation_type == "point", mget(c(x@grouping_column, x@value_column))])
    }
    
    # Compute aggregate values.
    browser()
    
    return(prediction_data)
  }
)



# ..compute_ensemble_prediction_estimates (risk strata) ------------------------
setMethod(
  "..compute_ensemble_prediction_estimates",
  signature(x = "predictionTableRiskGroups"),
  function(x, data, ensemble_method, ...) {
    # Suppress NOTES due to non-standard evaluation in data.table
    estimation_type <- NULL
    
    # Check if ensembling is actually required -- the computation is somewhat
    # expensive because it is done on the subset of data.
    if (data.table::uniqueN(data, by = x@grouping_column) == nrow(data)) {
      return(data[estimation_type == "point", mget(c(x@grouping_column, x@value_column))])
    }
    
    # Compute aggregate values.
    browser()

    return(prediction_data)
  }
)



...compute_ensemble_estimates <- function(
    x,
    prediction_columns,
    ensemble_method,
    confidence_level = NULL,
    percentiles = NULL
) {

  # Suppress NOTES due to non-standard evaluation in data.table
  estimation_type <- NULL
  
  if (ensemble_method %in% c("percentile", "bc")) {
    # Compute ensemble estimates using bootstraps.
    ensemble_values <- lapply(
      prediction_columns,
      function(ii_col, x) {
        return(..bootstrap_ci(
          x = x[estimation_type != "point"][[ii_col]],
          x0 = x[estimation_type == "point"][[ii_col]],
          bootstrap_ci_method = ensemble_method,
          confidence_level = confidence_level,
          percentiles = percentiles))
      },
      x = x
    )
    
  } else {
    # Compute ensemble estimates using statistical methods.
    if (ensemble_method == "median") {
      aggregation_fun <- stats::median
    } else {
      aggregation_fun <- mean
    }
    
    ensemble_values <- lapply(
      prediction_columns,
      function(ii_col, x, aggregation_fun) {
        return(aggregation_fun(x[[ii_col]], na.rm = TRUE))
      },
      x = x,
      aggregation_fun = aggregation_fun
    )
  }
  
  # Determine column names
  column_names <- lapply(
    seq_along(prediction_columns),
    function(ii, prediction_columns, ensemble_values, parse_percentile) {
      # If the content of element ii is not a list itself, it contains only a
      # single value that should be named.
      if (!is.list(ensemble_values[[ii]])) return(prediction_columns[ii])
      
      if (!parse_percentile) {
        # If the content of element ii is a list, assign the prediction column
        # name to the first column, and add the name to the remainder.
        
        column_names <- prediction_columns[ii]
        
        if (length(ensemble_values[[ii]]) > 1) {
          column_names <- c(
            column_names,
            paste0(prediction_columns[ii], "_", names(ensemble_values[[ii]])[2:length(ensemble_values[[ii]])]))
        }
        
        return(column_names)
        
      } else {
        # Assign the prediction column name to every column in case percentiles
        # are returned.
        return(paste0(prediction_columns[ii], "_", names(ensemble_values[[ii]])))
      }
    },
    prediction_columns = prediction_columns,
    ensemble_values = ensemble_values,
    parse_percentile = ensemble_method %in% c("percentile", "bc") && is.null(confidence_level) && !is.null(percentiles))
  
  # Flatten list of column names
  column_names <- unlist(column_names)
  
  # Flatten list of ensemble values.
  ensemble_values <- unlist(ensemble_values, recursive = FALSE)
  if (!is.list(ensemble_values)) ensemble_values <- as.list(ensemble_values)
  
  # Set column names.
  names(ensemble_values) <- column_names
  
  return(ensemble_values)
}

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
    sample_id = Waiver(),
    series_id = waiver(),
    repetition_id = waiver(),
    time = waiver(),
    classes = waiver(),
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
    ..error_unknown_prediction_type(type)
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
    }
    
    object@reference_data <- as.data.table(y)
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
  if (method::.hasSlot(object, "classes")) {
    if (is.waive(classes) && is(data, "dataObject")) {
      
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

## complete_prediction_table (general) -----------------------------------------
setMethod(
  "complete_prediction_table",
  signature(object = "predictionTable"),
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
      if (n_instances != object@identifier_data) {
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
    object <- callNextMethod(object)
    
    if (is_empty(object)) return(object)
    
    # Check outcome type
    if (is.na(object@outcome_type)) object@outcome_type <- "continuous"
    if (object@outcome_type != "continuous") {
      ..error_reached_unreachable_code(
        "The outcome type of predictionTableRegression objects should be \"continuous\".")
    }
    
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
    object <- callNextMethod(object)
    
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
    object <- callNextMethod(object)
    
    if (is_empty(object)) return(object)
    
    # Check outcome type
    if (is.na(object@outcome_type)) object@outcome_type <- "survival"
    if (object@outcome_type != "survival") {
      ..error_reached_unreachable_code(
        "The outcome type of predictionTableSurvival objects should be \"survival\".")
    }
    
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
      if (ncol(object@reference_data != 2)) {
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
      status_column <- object@prediction_data[[outcome_column[2]]]
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
    object <- callNextMethod(object)
    
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
    object <- callNextMethod(object)
    
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


# .merge_slots_into_data -------------------------------------------------------
setMethod(
  ".merge_slots_into_data",
  signature(x = "familiarDataElementPredictionTable"),
  function(x, ...) {
    # Merge identifier_data, reference_data and prediction_data and set
    # value column and grouping columns.
  }
)

 
# # merge_data_elements (predictionTable) ----------------------------------------
# setMethod(
#   "merge_data_elements",
#   signature(x = "predictionTable"),
#   function(
#     x,
#     x_list,
#     ...) {
#     
#     # Copy identifiers from any supplementary identifier attribute to the
#     # prediction_data attribute. The primary reason for doing so is to group and
#     # merge similar elements, but e.g. from different timepoints.
#     if (!is.null(as_data)) {
#       x_list <- lapply(x_list, .identifier_as_data_attribute)
#     }
#     
#     # Create return object.
#     y <- data.table::copy(x)
#     
#     # Collect prediction data.
#     y@prediction_data <- data.table::rbindlist(
#       lapply(x_list, function(x) (x@prediction_data)),
#       use.names = TRUE,
#       fill = TRUE
#     )
#     
#     # Collect identifier data.
#     y@identifier_data <- data.table::rbindlist(
#       lapply(x_list, function(x) (x@identifier_data)),
#       use.names = TRUE,
#       fill = FALSE
#     )
#     
#     # Collect reference data. Reference data may be absent, and we need to fill
#     # missing elements with NA.
#     missing_reference_data <- sapply(x_list, function(x) (is_empty(x@reference_data)))
#     if (!all(missing_reference_data) && any(missing_reference_data)) {
#       reference_col_name <- colnames(x_list[which(!missing_reference_data)][[1]]@reference_data)
#       
#       y@reference_data <- data.table::rbindlist(
#         lapply(
#           x_list,
#           function(x, ref_col) {
#             if (is_empty(x@reference_data)) return(x@reference_data)
# 
#             placeholder_ref <- data.table::data.table(
#               "temp_name" = rep_len(NA, nrow(object@prediction_data)))
# 
#             data.table::setnames(placeholder_ref, new = ref_col)
# 
#             return(placeholder_ref)
#           }
#         ),
#         use.names = TRUE,
#         fill = TRUE
#       )
#       
#     } else if (all(missing_reference_data)) {
#       y@reference_data <- NULL
#       
#     } else {
#       y@reference_data <- data.table::rbindlist(
#         lapply(x_list, function(x) (x@reference_data)),
#         use.names = TRUE,
#         fill = TRUE
#       )
#     }
#     
#     return(list(y))
#   }
# )
# 
# 
# 
# # .compute_data_element_estimates ----------------------------------------------
# setMethod(
#   ".compute_data_element_estimates",
#   signature(x = "predictionTable"),
#   function(x, x_list = NULL, ...) {
#     
#     # It might be that x was only used to direct to this method.
#     if (!is.null(x_list)) x <- x_list
#     if (!is.list(x)) x <- list(x)
#     
#     # Merge data.
#     x_list <- merge_data_elements(x = x)
#     
#     y <- lapply(x_list, ..compute_data_element_estimates, ...)
#   
#     return(data_elements)
#   }
# )
# 
# # ..compute_data_element_estimates ---------------------------------------------
# setMethod(
#   "..compute_data_element_estimates",
#   signature(x = "predictionTable"),
#   function(x, ...) {
#     
#     if (is_empty(x)) return(NULL)
#     
#     identifier_columns <- colnames(x@identifier_data)
#     reference_columns <- NULL
#     if (!is_empty(x@reference_data)) reference_columns <- colnames(x@reference_data)
#     value_columns <- colnames
#     
#       grouping_columns <- 
#     
#   }
# )

#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R

setClass(
  "predictionTableRegression",
  contains = "predictionTable"
)

setClass(
  "predictionTableSurvival",
  contains = "predictionTable",
  slots = list(
    "time" = "ANY",
    "censored" = "character",
    "event" = "character"
  ),
  prototype = list(
    "time" = Inf,
    "censored" = NA_character_,
    "event" = NA_character_
  )
)

setClass(
  "predictionTableSurvivalHazardRatio",
  contains = "predictionTableSurvival"
)

setClass(
  "predictionTableSurvivalCumulativeHazard",
  contains = "predictionTableSurvival"
)

setClass(
  "predictionTableSurvivalTime",
  contains = "predictionTableSurvival"
)

setClass(
  "predictionTableSurvivalProbability",
  contains = "predictionTableSurvival"
)

setClass(
  "predictionTableRiskGroups",
  contains = "predictionTable",
  slots = list(
    "time" = "ANY",
    "risk_groups" = "character",
    "thresholds" = "numeric"
  ),
  prototype = list(
    "time" = Inf,
    "risk_groups" = NA_character_,
    "thresholds" = NA_real_
  )
)

setClass(
  "predictionTableClassification",
  contains = "predictionTable",
  slots = list(
    "classes" = "character",
    "thresholds" = "ANY"
  ),
  prototype = list(
    "classes" = NA_character_,
    "thresholds" = NULL
  )
)

setClass(
  "predictionTableNovelty",
  contains = "predictionTable"
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
    
    object@time <- time
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
      
      # Check that the status columns contains 0s and 1s.
      
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

## complete_prediction_table (novelty) -----------------------------------------


## complete_prediction_table (risk groups) -------------------------------------

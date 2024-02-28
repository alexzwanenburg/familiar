..warning <- function(
    ...,
    warning_class = NULL,
    call = rlang::caller_env()
) {
  # Generic error function.
  message_string <- paste0(list(...), collapse = "")
  rlang::warn(
    message = message_string,
    class = union("familiar_warning", error_class),
    call = call
  )
}


..warning_missing_cohorts <- function(x, call = rlang::caller_env()) {
  message_string <- paste0(
    "Creating iterations: ",
    ifelse(length(x) > 1L, "Cohorts ", "Cohort "),
    paste_s(x),
    ifelse(length(x) > 1L, " were", " was"),
    " not found in the data table."
  )
  
  logger_warning(message_string, call = call)
  
  return(invisible(TRUE))
}



..warning_no_comparison_between_models <- function(
    call = rlang::caller_env()
) {
  logger_warning(
    paste0(
      "Cannot create plots to compare directly between models. ",
      "Please use the hybrid or ensemble detail levels."
    ),
    call = call
  )
  
  return(invisible(TRUE))
}



..warning_no_data_extraction_from_prediction_table <- function(
    data_element_description,
    call = rlang::caller_env()
) {
  message_string <- paste0(
    "Unable to compute ", data_element_description, " using prediction tables."
  )
  
  logger_warning(
    warn_str = message_string,
    warn_class = "prediction_table_no_data_extraction_warning",
    call = call
  )
  
  return(invisible(TRUE))
}



..warning_package_not_installed <- function(
    x, 
    purpose = NULL,
    call = rlang::caller_env()
) {
  
  # Only unique packages.
  x <- unique(x)
  
  # Basic error message.
  message_string <- ..message_missing_package(x = x, purpose = purpose)
  
  # Instructions for CRAN packages.
  message_string <- c(
    message_string,
    ..message_install_from_cran(x = x)
  )
  
  # Instructions for Bioconductor packages.
  message_string <- c(
    message_string,
    ..message_install_from_bioconductor(x = x)
  )
  
  rlang::warn(
    message = paste0(message_string, collapse = ""),
    class = c("familiar_warning", "package_missing"),
    call = rlang::caller_env()
  )
  
  return(invisible(TRUE))
}



..deprecation_count <- function(as_error = FALSE) {
  
  message_string <- "The \"count\" outcome type has been deprecated in familiar version 2.0.0."
  
  if (as_error) {
    rlang::abort(
      message = message_string,
      class = c("familiar_error", "deprecation_error")
    )
  }
  
  if (!.is_testing()) {
    rlang::warn(
      message = message_string,
      class = c("familiar_warning", "deprecation_warning"),
      .frequency = "once",
      .frequency_id = "deprecation_warning_count"
    )
  }
}



..deprecation_vgam <- function() {
  if (!.is_testing()) {
    rlang::warn(
      message = "The use of VGAM for multinomial logistic models has been deprecated since familiar version 2.0.0.",
      class = c("familiar_warning", "deprecation_warning"),
      .frequency = "once",
      .frequency_id = "deprecation_warning_vgam"
    )
  }
}



..deprecation_qvalue <- function() {
  if (!.is_testing()) {
    rlang::warn(
      message = "The qvalue package for computing q-values has been deprecated since familiar version 2.0.0.",
      class = c("familiar_warning", "deprecation_warning"),
      .frequency = "once",
      .frequency_id = "deprecation_warning_qvalue"
    )
  }
}



..deprecation_mboost <- function() {
  if (!.is_testing()) {
    rlang::warn(
      message = "The mboost package for gradient boosted models has been deprecated since familiar version 2.0.0.",
      class = c("familiar_warning", "deprecation_warning"),
      .frequency = "once",
      .frequency_id = "deprecation_warning_mboost"
    )
  }
}



..deprecation_rfsrc_variable_hunting <- function(as_error = FALSE) {
  
  message_string <- paste0(
    "The variable hunting feature selection method of randomForestSRC has been ",
    "deprecated in familiar version 2.0.0 due to stability issues."
  )
  
  if (as_error) {
    rlang::abort(
      message = message_string,
      class = c("familiar_error", "deprecation_error")
    )
  }
  
  if (!.is_testing()) {
    rlang::warn(
      message = message_string,
      class = c("familiar_warning", "deprecation_warning"),
      .frequency = "once",
      .frequency_id = "deprecation_warning_rfsrc_variable_hunting")
  }
}



..error <- function(
    ...,
    error_class = NULL,
    call = rlang::caller_env()
) {
  # Generic error function.
  message_string <- paste0(list(...), collapse = "")
  rlang::abort(
    message = message_string,
    class = union("familiar_error", error_class),
    call = call
  )
}



..error_no_predictions_possible <- function(
    object,
    prediction_type,
    call = rlang::caller_env()
) {
  message_string <- paste0(
    "Predictions of the ", prediction_type, " type are not possible ",
    "using the ", object@learner, " model (class: ", class(object)[1],
    ") for ", object@outcome_type, " outcomes."
  )
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "prediction_type_error"),
    call = call
  )
}



..error_no_known_outcome_type <- function(outcome_type, call = rlang::caller_env()) {
  message_string <- paste0(
    "Outcome type was not recognised. Found: ", outcome_type, ". ",
    "One of binomial, multinomial, continuous, or survival was expected."
  )
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "outcome_type_error"),
    call = call
  )
}



..error_outcome_type_not_implemented <- function(outcome_type, call = rlang::caller_env()) {
  message_string <- paste0(
    outcome_type, " is currently not supported, but scheduled for future implementation."
  )
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "outcome_type_error"),
    call = call
  )
}



..error_data_set_is_empty <- function(call = rlang::caller_env()) {
  rlang::abort(
    message = "The provided dataset does not contain any samples.",
    class = c("familiar_error", "dataset_error"),
    call = call
  )
}



..error_data_set_has_no_features <- function(call = rlang::caller_env()) {
  rlang::abort(
    message = "The provided dataset has no associated feature data.",
    class = c("familiar_error", "dataset_error"),
    call = call
  )
}



..error_input_missing_without_default <- function(
    var_name, 
    allow_config = FALSE,
    call = rlang::caller_env()
) {
  message_string <- paste0(
    var_name, " ", ifelse(allow_config, "tag/argument", "argument"), " is missing ",
    "and does not have a default setting. Please provide the ", var_name,
    ifelse(allow_config, " tag/argument.", " argument.")
  )
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "input_argument_error"),
    call = call
  )
}



..error_input_not_unique <- function(
    x, 
    var_name, 
    allow_config = FALSE,
    call = rlang::caller_env()
) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  n <- NULL
  
  # Identify duplicate values
  test_table <- data.table::data.table("value" = x)[, list("n" = .N), by = "value"]
  dupl_value <- test_table[n > 1L]$value
  
  message_string <- paste0(
    var_name, " ", ifelse(allow_config, "tag/argument", "argument"),
    " has ", ifelse(length(dupl_value) > 1L, "a duplicate value: ", "multiple duplicate values: "),
    paste_s(dupl_value)
  )
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "input_argument_error"),
    call = call
  )
}



..error_type_conversion_not_possible <- function(
    x,
    to_type,
    var_name,
    req_length = 1L,
    allow_more = FALSE,
    call = rlang::caller_env()
) {
  
  message_string <- paste0(
    "Conversion of input for ", var_name, " argument to the desired ", to_type, " class failed. ",
    "Please provide ", req_length, ifelse(allow_more, " or more ", " "),
    ifelse(req_length != 1L || allow_more, "input values ", "input value "),
    "of the correct type. Found: ", paste_s(x)
  )
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "input_argument_error"),
    call = call
  )
}



..error_variable_has_too_few_values <- function(
    x,
    var_name,
    req_length = 1L,
    allow_more = FALSE,
    call = rlang::caller_env()
) {
  
  # Handle req_length being a vector with two values.
  if (length(req_length) == 2L) {
    if (!is.finite(req_length[2L])) {
      req_length <- req_length[1L]
      allow_more <- TRUE
      
    } else if (diff(req_length) == 0L) {
      req_length <- req_length[1L]
      allow_more <- FALSE
    }
  }
  
  if (length(req_length) == 1L) {
    # The req_length argument specifies the exact number or the minimum number
    # of values.
    message_string <- paste0(
      "The ", var_name, " argument requires ",
      ifelse(allow_more, "at least ", "exactly "), req_length,
      ifelse(req_length == 1L, " value", " values"), ". ",
      length(x), ifelse(length(x) == 1L, " value was", " values were"), " found."
    )
    
  } else {
    # The req_length argument specifies a range for the number of values.
    message_string <- paste0(
      "The ", var_name, " argument requires between ",
      req_length[1L], " and ", req_length[2L], " values. ",
      ifelse(length(x) == 1L, " value was", " values were"), " found."
    )
  }
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "input_argument_error"),
    call = call
  )
}



..error_variable_has_too_many_values <- function(
    x,
    var_name,
    req_length = 1L,
    allow_fewer = FALSE,
    call = rlang::caller_env()
) {
  
  # Handle req_length being a vector with two values.
  if (length(req_length) == 2L) {
    if (!is.finite(req_length[1L])) {
      req_length <- req_length[2L]
      allow_fewer <- TRUE
      
    } else if (diff(req_length) == 0L) {
      req_length <- req_length[2L]
      allow_fewer <- FALSE
    }
  }
  
  if (length(req_length) == 1L) {
    # The req_length argument specifies the exact number or the maximum number
    # of values.
    message_string <- paste0(
      "The ", var_name, " argument requires ",
      ifelse(allow_fewer, "at most ", "exactly "), req_length,
      ifelse(req_length == 1L, " value", " values"), ". ",
      length(x), ifelse(length(x) == 1L, " value was", " values were"), " found."
    )
    
  } else {
    message_string <- paste0(
      "The ", var_name, " argument requires between ",
      req_length[1L], " and ", req_length[2L], " values. ",
      ifelse(length(x) == 1L, " value was", " values were"), " found."
    )
  }
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "input_argument_error"),
    call = call
  )
}



..error_value_outside_allowed_range <- function(
    x, 
    var_name, 
    range, 
    call = rlang::caller_env()
) {
  
  # Determine the string for the allowed value.
  if (is.infinite(range[1L]) && is.infinite(range[2L])) {
    allowed_value_text <- "be any real number"
    
  } else if (is.infinite(range[1L])) {
    allowed_value_text <- paste("have a value of", range[2L], "or less")
    
  } else if (is.infinite(range[2L])) {
    allowed_value_text <- paste("have a value of", range[1L], "or more")
    
  } else if (range[1L] - range[2L] == 0.0) {
    allowed_value_text <- paste("be exactly", range[1L])
    
  } else {
    allowed_value_text <- paste("have a value between", range[1L], "and", range[2L])
  }
  
  message_string <- paste0(
    var_name, " is expected to ", allowed_value_text,
    ". Found: ", paste_s(x)
  )
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "input_argument_error"),
    call = call
  )
}



..error_value_not_allowed <- function(
    x, 
    var_name, 
    values,
    call = rlang::caller_env()
) {
  
  # Replace NULL in x by "NULL"
  x[is.null(x)] <- "NULL"
  
  # Replace NULL in values by "NULL"
  values[is.null(values)] <- "NULL"
  
  # Identify those values that are not allowed
  forbidden_values <- setdiff(x, values)
  
  message_string <- paste0(
    var_name, " expects one of ", paste_s(values), " as input. ",
    "Found the following unknown values: ", paste_s(forbidden_values)
  )
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "input_argument_error"),
    call = call
  )
}



..error_type_not_valid <- function(
    x,
    var_name, 
    valid_type,
    call = rlang::caller_env()
) {
  
  message_string <- paste0(
    var_name, " has type ", paste_s(class(x)),
    " whereas ", paste_s(valid_type), " was expected."
  )
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "input_argument_error"),
    call = call
  )
}



..error_value_shared_between_variables <- function(
    x, 
    y, 
    var_name_x, 
    var_name_y,
    call = rlang::caller_env()
) {
  
  # Determine overlap
  overlap <- intersect(x, y)
  
  message_string <- paste0(
    ifelse(length(overlap) > 1L, "Multiple values were", "One value was"),
    " shared between ", var_name_x, " and ", var_name_y, ": ", paste_s(overlap),
    ". No overlap is allowed."
  )
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "input_argument_error"),
    call = call
  )
}



..error_reached_unreachable_code <- function(..., call = rlang::caller_env()) {
  message_string <- paste0(
    "This error should not occur, as this code should not be reachable by design. ",
    "Contact the package maintainer if you see this error.\n\t",
    paste0(list(...), collapse = "")
  )
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "developer_error"),
    call = call
  )
}



..error_ensemble_models_not_loaded <- function(call = rlang::caller_env()) {
  message_string <- paste0(
    "familiarModel objects were not loaded and attached the familiarEnsemble. ",
    "Please use the load_models method to load the models."
  )
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "object_load_error"),
    call = call
  )
}



..error_cannot_convert_to_familiar_object <- function(
    object, 
    expected_class, 
    call = rlang::caller_env()
) {
  
  # Determine what classes are allowed as object
  if (expected_class == "familiarModel") {
    allowed_class <- "familiarModel"
    
  } else if (expected_class == "familiarEnsemble") {
    allowed_class <- c("familiarModel", "familiarEnsemble")
    
  } else if (expected_class == "familiarData") {
    allowed_class <- c("familiarModel", "familiarEnsemble", "familiarData")
    
  } else if (expected_class == "familiarCollection") {
    allowed_class <- c("familiarModel", "familiarEnsemble", "familiarData", "familiarCollection")
  }
  
  message_string <- paste0(
    "The provided object cannot be converted from ", class(object), " to ", expected_class, ". ",
    "The object should have one of the following classes, or inherit it: ", paste_s(allowed_class)
  )
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "object_conversion_error"),
    call = call
  )
}



..error_data_not_prediction_table <- function(
    x, 
    expected_class,
    call = rlang::caller_env()
) {
  message_string <- paste0(
    "A prediction table object of class ", expected_class,
    " was expected, but ", class(x)[1], " was found."
  )
  
  rlang::abort(
    message = message_string,
    class = c("familiar_error", "incorrect_prediction_table"),
    call = call
  )
}



..error_package_not_installed <- function(
    x, 
    purpose = NULL,
    call = rlang::caller_env()
) {
  
  # Only unique packages.
  x <- unique(x)

  # Basic error message.
  err_message <- ..message_missing_package(x = x, purpose = purpose)
  
  # Instructions for CRAN packages.
  err_message <- c(
    err_message,
    ..message_install_from_cran(x = x))
  
  # Instructions for Bioconductor packages.
  err_message <- c(
    err_message,
    ..message_install_from_bioconductor(x = x))
  
  rlang::abort(
    message = paste0(err_message, collapse = ""),
    class = c("familiar_error", "package_missing"),
    call = call
  )
}



..error_message_no_training_data_available <- function() {
  return("No data was available to train the model.")
}



..error_message_no_features_selected_for_training <- function() {
  return("No features were selected to train the model on.")
}



..error_message_no_optimised_hyperparameters_available <- function() {
  return("Model hyperparameters were not optimised.")
}



..error_message_failed_model_coefficient_estimation <- function() {
  return("None of the model coefficients could be estimated.")
}

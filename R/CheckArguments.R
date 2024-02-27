.check_dots_is_parameter <- function(dots) {
  
  if (length(dots) > 0L) {
    # Find unmatched arguments.
    unmatched_args <- setdiff(dots, .get_all_parameter_names())
    
    if (length(unmatched_args) > 0L) {
      stop(paste0(
        "Configuration: one or more function arguments could not be matched ",
        "to arguments passed by summon_familiar as configuration parameters: ",
        paste_s(unmatched_args),
        "\nThese arguments may have been misspelled, or were deprecated or renamed."
      ))
    }
  }
  
  return(invisible(TRUE))
}



.check_configuration_tag_is_parameter <- function(config) {
  
  if (!is.null(config)) {
    # Find names of parent nodes.
    config_node_names <- names(config)
    
    # Find nodes that are specified differently by the user.
    unmatched_node_names <- setdiff(
      config_node_names,
      .get_all_configuration_parent_node_names()
    )
    
    if (length(unmatched_node_names) > 0L) {
      stop(paste0(
        "Configuration: one or more parent nodes in the configuration file could not be matched ",
        "to node names used by summon_familiar to group configuration parameters: ",
        paste_s(unmatched_node_names),
        "\nThese node names may have been misspelled, or were deprecated or renamed."
      ))
    }
    
    # Find names of configuration arguments.
    config_args <- unique(unlist(sapply(config, names)))
    
    # Find unmatched arguments.
    unmatched_args <- setdiff(config_args, .get_all_parameter_names())
    
    if (length(unmatched_args) > 0L) {
      stop(paste0(
        "Configuration: one or more parameters set in the configuration file could not be matched ",
        "to arguments passed by summon_familiar as configuration parameters: ",
        paste_s(unmatched_args),
        "\nThese parameters may have been misspelled, or were deprecated or renamed."
      ))
    }
  }
  
  return(invisible(TRUE))
}



.check_number_in_valid_range <- function(
    x,
    var_name,
    range,
    closed = c(TRUE, TRUE)
) {

  # Interpret single input range value as range containing only one value.
  if (length(range) == 1L) range <- c(range, range)
  
  # Set lower limit to -Inf
  if (is.na(range[1L])) range[1L] <- -Inf
  
  # Set upper limit to Inf
  if (is.na(range[2L])) range[2L] <- Inf

  # Some internal checks that should never be triggered.
  if (length(x) != 1L) {
    ..error_reached_unreachable_code(paste0(
      ".check_number_in_valid_range: x does not have length 1."
    ))
  }
  
  # Another internal checks that should never be triggered.
  if (range[2L] - range[1L] < 0.0) {
    ..error_reached_unreachable_code(paste0(
      ".check_number_in_valid_range: the range is inverted"
    ))
  }
  
  # Check that x is numeric or NA.
  if (!is.numeric(x) && !is.na(x)) {
    ..error_type_not_valid(
      x = x,
      var_name = var_name,
      valid_type = "numeric"
    )
  }
  
  if (!is.na(x)) {
    is_outside_range <- ifelse(
      closed[1L],
      x < range[1L],
      x <= range[1L]
    ) || ifelse(
      closed[2L],
      x > range[2L],
      x >= range[2L]
    )
    
  } else {
    # NA-values are outside the valid range.
    is_outside_range <- TRUE
  }
  
  if (is_outside_range) {
    ..error_value_outside_allowed_range(
      x = x,
      var_name = var_name,
      range = range
    )
  }
  
  return(invisible(TRUE))
}



.check_value_not_shared <- function(
    x,
    y,
    var_name_x,
    var_name_y
) {
  # If either or both are NULL, return NULL
  if (is.null(x) || is.null(y)) return(NULL)
  
  overlap_values <- intersect(x, y)
  if (length(overlap_values) > 0L) {
    ..error_value_shared_between_variables(
      x = x,
      y = y,
      var_name_x = var_name_x,
      var_name_y = var_name_y
    )
  }
  
  return(invisible(TRUE))
}



.check_argument_length <- function(
    x,
    var_name,
    min = 0L,
    max = Inf
) {
  if (length(x) < min) {
    ..error_variable_has_too_few_values(
      x = x,
      var_name = var_name,
      req_length = c(min, max)
    )
    
  } else if (length(x) > max) {
    ..error_variable_has_too_many_values(
      x = x,
      var_name = var_name,
      req_length = c(min, max)
    )
  }
  
  return(invisible(TRUE))
}



.check_parameter_value_is_valid <- function(
    x,
    var_name,
    values
) {

  # Check if NULL is an allowed value
  null_allowed <- any(is.null(values))
  
  if (length(x) == 1L) {
  
    # Check if x is NULL
    if (is.null(x) && null_allowed) {
      # If x is NULL and this is allowed, return to parent function
      return(NULL)
    }
  }
  
  if (any(is.null(x)) && !null_allowed) {
    # If any of x is NULL and this is not allowed, raise an error.
    ..error_value_not_allowed(
      x = x,
      var_name = var_name,
      values = values
    )
  }
  
  if (!all(x %in% values)) {
    # Throw an error if any x is not among the valid values
    ..error_value_not_allowed(
      x = x,
      var_name = var_name,
      values = values
    )
  }
  
  return(invisible(TRUE))
}



.perform_type_conversion <- function(
    x,
    to_type,
    var_name,
    req_length,
    allow_more = FALSE
) {

  # Specify conversion functions
  if (to_type %in% c("character", "factor")) {
    conv_function <- as.character
    
  } else if (to_type == "numeric") {
    conv_function <- as.numeric
    
  } else if (to_type == "integer") {
    conv_function <- as.integer
    
  } else if (to_type == "logical") {
    conv_function <- as.logical
    
  } else if (to_type == "list") {
    conv_function <- as.list
    
  } else {
    ..error_reached_unreachable_code(paste0(
      ".perform_type_conversion: the to_type argument was not recognised: ", to_type
    ))
  }
  
  # Attempt conversion
  x <- tryCatch(
    conv_function(x),
    warning = function(war) {
      ..error_type_conversion_not_possible(
        x = x,
        to_type = to_type,
        var_name = var_name,
        req_length = req_length,
        allow_more = allow_more
      )
    },
    error = function(err) {
      ..error_type_conversion_not_possible(
        x = x,
        to_type = to_type,
        var_name = var_name,
        req_length = req_length,
        allow_more = allow_more
      )
    }
  )
  
  # Test length of resulting vector
  if (length(x) < req_length) {
    ..error_variable_has_too_few_values(
      x = x,
      var_name = var_name,
      req_length = req_length,
      allow_more = allow_more
    )
    
  } else if (length(x) > req_length && !allow_more) {
    ..error_variable_has_too_many_values(
      x = x,
      var_name = var_name,
      req_length = req_length,
      allow_fewer = FALSE
    )
  }
  
  return(x)
}



.parse_arg <- function(
    x_config,
    x_var = waiver(),
    var_name,
    type,
    optional = FALSE,
    default = NULL
) {

  # There are two options for parsing a value:
  # 1. Using a configuration file (through x_config)
  # 2. Using a command-line variable (through x_var and ...)
  
  # If the variable with name x_var is provided in ..., this variable takes
  # precedence over x_config. Multiple x_var entries can be provided. The first
  # valid one is selected.
  if (!is.waive(x_var)) {
    # Variable set through a function argument
    x <- x_var
    
  } else if (!is.null(x_config)) {
    # Variable set through a configuration list
    x <- x_config
    
    # Trim whitespace and split variables
    if (type %in% c("character_list", "numeric_list", "integer_list", "logical_list") && length(x) > 0L) {
      # Divide by comma
      x <- strsplit_all(
        x = x,
        split = ",",
        fixed = TRUE
      )[[1L]]

      # Remove whitespace
      x <- gsub(
        x = x,
        pattern = " ",
        replacement = "",
        fixed = TRUE
      )
    }
    
  } else if (optional) {
    # Return default value for optional values that have not been set
    # externally.
    return(default)
    
  } else {
    # Required, but no default.
    ..error_input_missing_without_default(
      var_name = var_name,
      allow_config = TRUE
    )
  }
  
  # This point in the code can only be reached if the user provided a value.  
  # Check for presence of values
  if (length(x) == 0L && optional == FALSE) {
    # Throw an error as entry is required
    ..error_input_missing_without_default(
      var_name = var_name,
      allow_config = TRUE
    )
    
  } else if (isTRUE(all.equal("", x)) && optional == FALSE) {
    ..error_input_missing_without_default(
      var_name = var_name,
      allow_config = TRUE
    )
    
  } else if (length(x) == 0L || isTRUE(all.equal("", x))) {
    # Return the default value if no value is provided
    return(default)
  }

  # Convert logical and logical_list
  if (type %in% c("logical", "logical_list")) {
    # Replace entries
    x[tolower(x) %in% c("t", "true", "y", "yes", "1")] <- TRUE
    x[tolower(x) %in% c("f", "false", "n", "no", "0")] <- FALSE
  }
  
  if (type == "list") {
    # Convert to list. Note that lists can be empty.
    x <- .perform_type_conversion(
      x = x,
      to_type = type,
      var_name = var_name,
      req_length = 0L,
      allow_more = TRUE
    )
    
  } else if (type %in% c("character", "numeric", "integer", "logical")) {
    # Convert to type
    x <- .perform_type_conversion(
      x = x,
      to_type = type,
      var_name = var_name,
      req_length = 1L,
      allow_more = FALSE
    )
    
  } else if (type %in% c("character_list", "numeric_list", "integer_list", "logical_list")) {
    # Find basic type string.
    list_type <- sub_last(
      x = type,
      pattern = "_list",
      replacement = "",
      fixed = TRUE
    )
    
    # Convert to type
    x <- .perform_type_conversion(
      x = x,
      to_type = list_type,
      var_name = var_name,
      req_length = 1L,
      allow_more = TRUE
    )
    
    # Check for duplicates
    if (anyDuplicated(x)) {
      ..error_input_not_unique(
        x = x,
        var_name = var_name,
        allow_config = TRUE
      )
    }
    
  } else {
    # By design unreachable
    ..error_reached_unreachable_code(
      paste0(".parse_arg: the type argument was not recognised: ", type)
    )
  }
  
  return(x)
}

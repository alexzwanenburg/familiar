.check_number_in_valid_range <- function(x, var_name, range, closed=c(TRUE, TRUE)){

  # Interpret single input range value as range containing only one value.
  if(length(range)==1){ range <- c(range, range) }
  
  # Set lower limit to -Inf
  if(is.na(range[1])){ range[1] <- -Inf }
  
  # Set upper limit to Inf
  if(is.na(range[2])){ range[2] <- Inf }
  
  # Some internal checks that should never be triggered.
  if(length(x) != 1){
    ..error_reached_unreachable_code(".check_number_in_valid_range_x_not_length_1")
  }
  # Another internal checks that should never be triggered.
  if(range[2] - range[1] < 0.0){
    ..error_reached_unreachable_code(".check_number_in_valid_range_inverted_range")
  }
  
  is_outside_range <- ifelse(closed[1], x < range[1], x <= range[1]) | ifelse(closed[2], x > range[2], x >= range[2])
  
  if(is_outside_range){
    ..error_value_outside_allowed_range(x, var_name, range)
  }
}


.check_value_not_shared <- function(x, y, var_name_x, var_name_y){
  # If either or both are NULL, return NULL
  if(is.null(x) | is.null(y)) return(NULL)
  
  overlap_values <- intersect(x, y)
  if(length(overlap_values) > 0){
    ..error_value_shared_between_variables(x, y, var_name_x, var_name_y)
  }
}


.check_argument_length <- function(x, var_name, min=0, max=Inf){
  if(length(x) < min){
    ..error_variable_has_too_few_values(x=x, var_name=var_name, req_length=c(min, max))
    
  } else if(length(x) > max){
    ..error_variable_has_too_many_values(x=x, var_name=var_name, req_length=c(min, max))
  }
}


.check_parameter_value_is_valid <- function(x, var_name, values){

  # Check if NULL is an allowed value
  null_allowed <- ifelse(any(is.null(values)), TRUE, FALSE)
  
  if(length(x) == 1){
  
    # Check if x is NULL
    if(is.null(x) & null_allowed){
      # If x is NULL and this is allowed, return to parent function
      return(NULL)
    }
  }
  
  if(any(is.null(x)) & !null_allowed) {
    # If any of x is NULL and this is not allowed, raise an error.
    ..error_value_not_allowed(x=x, var_name=var_name, values=values)
  }
  
  if(any(!x %in% values)){
    # Throw an error if any x is not among the valid values
    ..error_value_not_allowed(x=x, var_name=var_name, values=values)
  }
}


.perform_type_conversion <- function(x, to_type, var_name, req_length, allow_more=FALSE){

  # Specify conversion functions
  if(to_type %in% c("character", "factor")){
    conv_function <- as.character
    
  } else if(to_type == "numeric"){
    conv_function <- as.numeric
    
  } else if(to_type == "integer"){
    conv_function <- as.integer
    
  } else if(to_type == "logical"){
    conv_function <- as.logical
    
  } else if(to_type == "list"){
    conv_function <- as.list
    
  } else {
    ..error_reached_unreachable_code(".perform_type_conversion_unknown_to_type")
  }
  
  # Attempt conversion
  x <- tryCatch({
    conv_function(x)
  }, warning = function(war){
    ..error_type_conversion_not_possible(x, to_type, var_name, req_length, allow_more)
  }, error = function(err){
    ..error_type_conversion_not_possible(x, to_type, var_name, req_length, allow_more)
  })
  
  # Test length of resulting vector
  if(length(x) < req_length){
    ..error_variable_has_too_few_values(x, var_name, req_length, allow_more)
  } else if(length(x) > req_length & !allow_more){
    ..error_variable_has_too_many_values(x, var_name, req_length, allow_fewer=FALSE)
  }
  
  return(x)
}


.parse_arg <- function(x_config, x_var=waiver(), var_name, type, optional=FALSE, default=NULL){

  # There are two options for parsing a value:
  # 1. Using a configuration file (through x_config)
  # 2. Using a command-line variable (through x_var and ...)
  
  # If the variable with name x_var is provided in ..., this variable takes precedence over x_config.
  # Multiple x_var entries can be provided. The first valid one is selected.
  if(!is.waive(x_var)){
    # Variable set through a function argument
    x <- x_var
    
  } else if(!is.null(x_config)){
    # Variable set through a configuration list
    x <- x_config
    
    # Trim whitespace and split variables
    if(type %in% c("character_list", "numeric_list", "integer_list", "logical_list") & length(x) > 0){
      # Divide by comma
      x <- stringi::stri_split(str=x, fixed=",")[[1]]
      
      # Remove whitespace
      x <- stringi::stri_replace_all(str=x, fixed=" ", replacement="")
    }
    
  } else if(optional){
    # Return default value for optional values that have not been set externally.
    return(default)
    
  } else {
    # Required, but no default.
    ..error_input_missing_without_default(var_name, TRUE)
  }
  
  # This point in the code can only be reached if the user provided a value.  
  # Check for presence of values
  if(length(x)==0 & optional==FALSE){
    # Throw an error as entry is required
    ..error_input_missing_without_default(var_name, TRUE)
    
  } else if(isTRUE(all.equal("", x)) & optional==FALSE){
    ..error_input_missing_without_default(var_name, TRUE)
    
  } else if(length(x)==0 | isTRUE(all.equal("", x))) {
    # Return the default value if no value is provided
    return(default)
  }

  # Convert logical and logical_list
  if(type %in% c("logical", "logical_list")){
    # Replace entries
    x[tolower(x) %in% c("t", "true", "y", "yes", "1")] <- TRUE
    x[tolower(x) %in% c("f", "false", "n", "no", "0")] <- FALSE
  }
  
  if(type == "list"){
    # Convert to list. Note that lists can be empty.
    x <- .perform_type_conversion(x=x, to_type=type, var_name=var_name, req_length=0, allow_more=TRUE)
    
  } else if(type %in% c("character", "numeric", "integer", "logical")) {
    # Convert to type
    x <- .perform_type_conversion(x=x, to_type=type, var_name=var_name, req_length=1, allow_more=FALSE)
    
  } else if(type %in% c("character_list", "numeric_list", "integer_list", "logical_list")){
    # Find basic type string
    list_type <- stringi::stri_replace(str=type, replacement="", fixed="_list", mode="last")
    
    # Convert to type
    x <- .perform_type_conversion(x=x, to_type=list_type, var_name=var_name, req_length=1, allow_more=TRUE)
    
    # Check for duplicates
    if(anyDuplicated(x)){
      ..error_input_not_unique(x=x, var_name=var_name, TRUE)
    }
    
  } else {
    # By design unreachable
    ..error_reached_unreachable_code(".parse_arg_unknown_type")
  }
  
  return(x)
}

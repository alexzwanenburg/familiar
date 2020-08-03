..error_no_known_outcome_type <- function(outcome_type){
  stop(paste0("Outcome type was not recognised. Found: ", outcome_type,
             ". One of binomial, multinomial, continuous, count, ",
             "survival was expected."))
}

..error_outcome_type_not_implemented <- function(outcome_type){
  stop(paste0(outcome_type, " is currently not supported, but scheduled for future implementation."))
}

..error_data_set_is_empty <- function(){
  stop("The provided data set does not contain any samples.")
}
..error_data_set_has_no_features <- function(){
  stop("The provided data has no associated feature data.")
}

..error_input_missing_without_default <- function(var_name, allow_config=FALSE){
  stop(paste0(var_name, " ", ifelse(allow_config, "tag/argument", "argument"), " is missing ",
              "and does not have a default setting. Please provide the ", var_name,
              ifelse(allow_config, " tag/argument.", " argument.")))
}

..error_input_not_unique <- function(x, var_name, allow_config=FALSE){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  n <- NULL
  
  # Identify duplicate values
  test_table <- data.table::data.table("value"=x)[, list("n"=.N), by="value"]
  dupl_value <- test_table[n > 1]$value
  
  stop(paste0(var_name, " ", ifelse(allow_config, "tag/argument", "argument"),
              " has ", ifelse(length(dupl_value) > 1, "a duplicate value: ", "multiple duplicate values: "),
              paste0(dupl_value, collapse=", ")))
}

..error_type_conversion_not_possible <- function(x, to_type, var_name, req_length=1, allow_more=FALSE){
  stop(paste0("Conversion of input for ", var_name, " argument to the desired ", to_type, " class failed.",
              " Please provide ", req_length, ifelse(allow_more, " or more ", " "),
              ifelse(req_length!=1 | allow_more, "input values ", "input value "),
              "of the correct type. Found: ", paste0(x, collapse=", ")))
}

..error_variable_has_too_few_values <- function(x, var_name, req_length=1, allow_more=FALSE){
  
  # Handle req_length being a vector with two values.
  if(length(req_length) == 2){
    if(!is.finite(req_length[2])){
      req_length <- req_length[1]
      allow_more <- TRUE
    } else if(diff(req_length) == 0){
      req_length <- req_length[1]
      allow_more <- FALSE
    }
  }
  
  if(length(req_length) == 1){
    # The req_length argument specifies the exact number or the minimum number
    # of values.
    stop(paste0("The ", var_name, "argument requires ", ifelse(allow_more, "at least ", "exactly "), req_length,
                ifelse(req_length==1, " value", " values"), ". ", length(x),
                ifelse(length(x)==1, " value was", " values were"), " found."))
    
  } else {
    # The req_length argument specifies a range for the number of values.
    stop(paste0("The ", var_name, "argument requires between ", req_length[1], " and ", req_length[2], " values. ",
                ifelse(length(x)==1, " value was", " values were"), " found."))
  }
}

..error_variable_has_too_many_values <- function(x, var_name, req_length=1, allow_fewer=FALSE){
  
  # Handle req_length being a vector with two values.
  if(length(req_length) == 2){
    if(!is.finite(req_length[1])){
      req_length <- req_length[2]
      allow_fewer <- TRUE
    } else if(diff(req_length) == 0){
      req_length <- req_length[2]
      allow_fewer <- FALSE
    }
  }
  
  if(length(req_length) == 1){
    # The req_length argument specifies the exact number or the maximum number
    # of values.
    stop(paste0("The ",var_name, " argument requires ", ifelse(allow_fewer, "at most ", "exactly "), req_length,
                ifelse(req_length==1, " value", " values"), ". ", length(x),
                ifelse(length(x)==1, " value was", " values were"), " found."))
    
  } else {
    
    stop(paste0("The ",var_name, " argument requires between", req_length[1], " and ", req_length[2], " values. ",
                ifelse(length(x)==1, " value was", " values were"), " found."))
  }
}

..error_value_outside_allowed_range <- function(x, var_name, range){
  
  # Determine the string for the allowed value.
  if(is.infinite(range[1]) & is.infinite(range[2])){
    allowed_value_text <- "be any real number"
  } else if(is.infinite(range[1])){
    allowed_value_text <- paste("have a value of", range[2], "or less")
  } else if(is.infinite(range[2])){
    allowed_value_text <- paste("have a value of", range[1], "or more")
  } else if(range[1] - range[2] == 0.0){
    allowed_value_text <- paste("be exactly", range[1])
  } else {
    allowed_value_text <- paste("have a value between", range[1], "and", range[2])
  }
  
  stop(paste0(var_name, " is expected to ", allowed_value_text,
              ". Found: ", x))
}

..error_value_not_allowed <- function(x, var_name, values){
  
  # Replace NULL in x by "NULL"
  x[is.null(x)] <- "NULL"
  
  # Replace NULL in values by "NULL"
  values[is.null(values)] <- "NULL"
  
  # Identify those values that are not allowed
  forbidden_values <- setdiff(x, values)
  
  stop(paste(var_name, "expects one of", paste_s(values), "as input.",
             "Found the following unknown values:", paste_s(forbidden_values)))
}

..error_ensemble_models_not_loaded <- function(){
  stop(paste("familiarModel objects were not loaded and attached the familiarEnsemble. Please use the load_models method to load the models."))
}


..error_reached_unreachable_code <- function(err_code=""){
  stop(paste("This error should not occur, as this code should not be reachable by design.",
             "Contact the package maintainer if you see this error.",
             err_code))
}

..error_cannot_convert_to_familiar_object <- function(object, expected_class){
  
  # Determine what classes are allowed as object
  if(expected_class == "familiarModel"){
    allowed_class <- "familiarModel"
    
  } else if(expected_class == "familiarEnsemble"){
    allowed_class <- c("familiarModel", "familiarEnsemble")
    
  } else if(expected_class == "familiarData"){
    allowed_class <- c("familiarModel", "familiarEnsemble", "familiarData")
    
  } else if(expected_class == "familiarCollection"){
    allowed_class <- c("familiarModel", "familiarEnsemble", "familiarData", "familiarCollection")
  }
  
  stop(paste("The provided object cannot be converted from ", class(object), " to ", expected_class,
             ". The object should have one of the following classes, or inherit it: ", paste0(allowed_class, collapse=", ")))
}


..error_value_shared_between_variables <- function(x, y, var_name_x, var_name_y){
  
  # Determine overlap
  overlap <- intersect(x, y)
  
  stop(paste0(ifelse(length(overlap) > 1, "Multiple values were", "One value was")),
       " shared between ", var_name_x, " and ", var_name_y, ": ", paste0(overlap, collapse=", "),
       ". No overlap is allowed.")
}

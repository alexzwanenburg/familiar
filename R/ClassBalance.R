#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


# create_instance_weights (dataObject) -----------------------------------------
setMethod(
  "create_instance_weights",
  signature(data = "dataObject"),
  function(data, method, outcome_type = NULL, ...) {
    
    if (is.null(outcome_type)) outcome_type <- data@outcome_type
    
    # Pass to data.table.
    return(create_instance_weights(
      data = data@data,
      method = method,
      outcome_type = outcome_type,
      ...
    ))
  }
)



# create_instance_weights (NULL) -----------------------------------------------
setMethod(
  "create_instance_weights",
  signature(data = "NULL"),
  function(data, method, ...) {
    return(NULL)
  }
)



# create_instance_weights (data.table) -----------------------------------------
setMethod(
  "create_instance_weights",
  signature(data = "data.table"),
  function(
    data,
    outcome_type,
    method = NULL,
    normalisation = "none",
    ...
  ) {
    
    # Check that an outcome-type is provided.
    if (is.null(outcome_type)) {
      ..error_reached_unreachable_code("create_instance_weights: an outcome type should be provided.")
    }
    
    # Only generate weights for outcomes with classes.
    if (!outcome_type %in% c("binomial", "multinomial")) return(NULL)
    
    # Return NULL if method equals NULL.
    if (is.null(method)) return(NULL)
    
    # Obtain id columns.
    id_columns <- get_id_columns(id_depth = "repetition")
    
    # Obtain data columns.
    outcome_columns <- get_outcome_columns(outcome_type)
    
    # Remove all feature columns.
    data <- data.table::copy(data[, mget(c(id_columns, outcome_columns))])
    
    if (method == "inverse_number_of_samples") {
      weights <- .compute_inverse_number_of_samples(data)
      
    } else if (method == "effective_number_of_samples") {
      weights <- .compute_effective_number_of_samples(data, ...)
      
    } else if (method == "none") {
      weights <- rep(1.0, nrow(data)) / nrow(data)
      
    } else {
      # Unknown method.
      ..error_reached_unreachable_code(paste0(
        "create_instance_weights: unknown class weighting method. Found: ", method
      ))
    }
    
    # Normalisation.
    if (normalisation == "sum_one") {
      weights <- weights / sum(weights)
      
    } else if (normalisation == "average_one") {
      weights <- weights * length(weights) / sum(weights)
      
    } else if (normalisation == "none") {
      weights <- weights
      
    } else {
      # Unknown normalisation method.
      ..error_reached_unreachable_code(paste0(
        "create_instance_weights: unknown normalisation method. Found: ", normalisation
      ))
    }
    
    return(weights)
  }
)



.compute_inverse_number_of_samples <- function(x, ...) {
  # Compute the number of samples in each class, and then weight by inverse
  # weights.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  n <- .NATURAL <- NULL
  
  # Sum the number of instances for each outcome.
  y <- x[, list("n" = .N), by = "outcome"]
  
  # Compute the sample weight for each outcome.
  y[, "sample_weight" := 1.0 / n]
  
  # Merge with x.
  return(x[y, on = .NATURAL]$sample_weight)
}



.compute_effective_number_of_samples <- function(x, beta, ...) {
  # Compute the number of samples in each class, and create weights by effective
  # number of samples. Based on Cui Y, Jia M, Lin T-Y, Song Y, Belongie S.
  # Class-balanced loss based on effective number of samples. 2019 IEEE/CVF
  # Conference on Computer Vision and Pattern Recognition (CVPR). IEEE; 2019.
  # pp. 9268–9277. 10.1109/cvpr.2019.00949
  
  # Suppress NOTES due to non-standard evaluation in data.table
  n <- .NATURAL <- NULL
  
  # Sum the number of instances for each outcome.
  y <- x[, list("n" = .N), by = "outcome"]
  
  # Compute the sample weight for each outcome.
  y[, "sample_weight" := (1.0 - beta) / (1.0 - beta^n)]
  
  # Merge with x.
  return(x[y, on = .NATURAL]$sample_weight)
}



..compute_effective_number_of_samples_beta <- function(x) {
  # Convert a negative input number from the model hyperparameters to a beta
  # parameter.
  
  # Check that the number is negative.
  if (x >= 0.0) {
    ..error_reached_unreachable_code(paste0(
      "..compute_effective_number_of_samples_beta: parameter should be negative. Found: ", x
    ))
  } 
  
  return(1.0 - 10.0^x)
}



.get_default_sample_weighting_method <- function(outcome_type) {
  
  # Set default method.
  default_method <- ifelse(
    outcome_type %in% c("binomial", "multinomial"),
    "inverse_number_of_samples",
    "none"
  )
  
  # Set hyperparameter.
  return(.set_hyperparameter(
    default = default_method,
    type = "factor",
    range = c("inverse_number_of_samples", "effective_number_of_samples", "none"),
    randomise = FALSE
  ))
}



.get_default_sample_weighting_beta <- function(method, outcome_type) {
  
  # Default values.
  if (
    "effective_number_of_samples" %in% method &&
    outcome_type %in% c("binomial", "multinomial")
  ) {
    default_values <- c(-4L, -3L, -2L, -1L)
    
  } else {
    default_values <- -2L
  }
  
  return(.set_hyperparameter(
    default = default_values,
    type = "integer",
    range = c(-6L, -1L),
    randomise = length(default_values) > 1L
  ))
}

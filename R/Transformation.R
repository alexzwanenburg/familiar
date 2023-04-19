#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass(
  "featureInfoParametersTransformationPowerTransform",
  contains = "featureInfoParameters",
  slots = list(
    "method" = "character",
    "transformer" = "ANY",
    "fitting_parameters" = "ANY"),
  prototype = list(
    "method" = "none",
    "transformer" = NULL,
    "fitting_parameters" = NULL)
  )
  
# NOTE: the classes below were used prior to version 1.5.0. These classes should
# not be removed to make sure that the corresponding objects can be updated. All
# related methods have been deprecated.

setClass(
  "featureInfoParametersTransformationNone",
  contains = "featureInfoParameters",
  slots = list(
    "reason" = "ANY"),
  prototype = list(
    "reason" = NULL))

setClass(
  "featureInfoParametersTransformationBoxCox",
  contains = "featureInfoParametersTransformationPowerTransform",
  slots = list(
    "lambda" = "numeric"),
  prototype = list(
    "lambda" = NA_real_))

setClass(
  "featureInfoParametersTransformationYeoJohnson",
  contains = "featureInfoParametersTransformationPowerTransform",
  slots = list(
    "lambda" = "numeric"),
  prototype = list(
    "lambda" = NA_real_))



.get_available_none_transformation_methods <- function() {
  return("none")
}

.get_available_box_cox_transformation_methods <- function() {
  return(c("box_cox", "box_cox_robust", "box_cox_non_shift"))
}

.get_available_yeo_johnson_transformation_methods <- function() {
  return(c("yeo_johnson", "yeo_johnson_robust", "box_cox_non_shift"))
}

.get_available_transformation_methods <- function(type = "all") {
  # Check type
  if (!type %in% c("all", "none", "box_cox", "yeo_johnson")) {
    ..error_reached_unreachable_code(paste0(
      ".get_available_transformation_methods: unspecified type: ", type))
  }

  available_transformation_method <- NULL

  if (type %in% c("none", "all")) {
    available_transformation_method <- c(
      available_transformation_method,
      .get_available_none_transformation_methods())
  }

  if (type %in% c("all", "box_cox")) {
    available_transformation_method <- c(
      available_transformation_method,
      .get_available_box_cox_transformation_methods())
  }

  if (type %in% c("all", "yeo_johnson")) {
    available_transformation_method <- c(
      available_transformation_method,
      .get_available_yeo_johnson_transformation_methods())
  }

  return(available_transformation_method)
}



create_transformation_parameter_skeleton <- function(
    feature_info_list,
    feature_names = NULL,
    transformation_method,
    transformation_lambda = NULL,
    transformation_optimisation_criterion = "mle",
    transformation_gof_p_value = NULL,
    .override_existing = FALSE) {
  # Creates a skeleton for the provided transformation method. If
  # transformation_lambda is provided (typically not), this value is updated as
  # well.

  # Determine feature names from the feature info list, if provided.
  if (is.null(feature_names)) feature_names <- names(feature_info_list)

  # Select only features that appear in the feature info list.
  feature_names <- intersect(
    names(feature_info_list),
    feature_names)

  # Skip step if no feature info objects are updated.
  if (is_empty(feature_names)) return(feature_info_list)

  # Check that method is applicable.
  .check_parameter_value_is_valid(
    x = transformation_method,
    var_name = "transformation_method",
    values = .get_available_transformation_methods())

  # Check that transformation_lambda is numeric. This is slightly redundant, as
  # this is also checked by the power.transform package.
  if (!is.null(transformation_lambda)) {
    .check_number_in_valid_range(
      x = transformation_lambda,
      var_name = "transformation_lambda",
      range = c(-Inf, Inf))
  }

  # Update familiar info objects with a feature transformation skeleton.
  updated_feature_info <- fam_lapply(
    X = feature_info_list[feature_names],
    FUN = .create_transformation_parameter_skeleton,
    method = transformation_method,
    lambda = transformation_lambda,
    optimisation_criterion = transformation_optimisation_criterion,
    gof_p_value = transformation_gof_p_value,
    .override_existing = .override_existing)

  # Provide names for the updated feature info objects.
  names(updated_feature_info) <- feature_names

  # Replace list elements.
  feature_info_list[feature_names] <- updated_feature_info

  return(feature_info_list)
}



.create_transformation_parameter_skeleton <- function(
    feature_info, 
    method,
    optimisation_criterion = "mle",
    gof_p_value = NULL,
    lambda = NULL, 
    .override_existing = FALSE) {
  # Check if transformation data was already completed, and does not require
  # being determined anew.
  if (feature_info_complete(feature_info@transformation_parameters) &&
      !.override_existing) {
    return(feature_info)
  }

  # Pass to underlying function that constructs the skeleton.
  object <- ..create_transformation_parameter_skeleton(
    feature_name = feature_info@name,
    feature_type = feature_info@feature_type,
    available = is_available(feature_info),
    method = method,
    optimisation_criterion = optimisation_criterion,
    gof_p_value = gof_p_value,
    lambda = lambda)

  # Update transformation_parameters slot.
  feature_info@transformation_parameters <- object

  return(feature_info)
}



..create_transformation_parameter_skeleton <- function(
    feature_name,
    feature_type = "numeric",
    available = TRUE,
    method,
    optimisation_criterion = "mle",
    gof_p_value = NULL,
    lambda = NULL) {
  # This is the lowest level function for creation transformation parameter
  # skeletons. This function always generates the same class of object. Fitting
  # parameters are passed on to power.transform::find_transformation_parameters.

  if (!require_package("power.transform", message_type = "silent")) {
    fitting_parameters <- list("method" = "none")
    
  } else if (feature_type != "numeric" || 
      !available || 
      (method %in% .get_available_none_transformation_methods())) {
    fitting_parameters <- list("method" = "none")
    
  } else if (method %in% .get_available_box_cox_transformation_methods()) {
    fitting_parameters <- list("method" = "box_cox")
    
  } else if (method %in% .get_available_yeo_johnson_transformation_methods()) {
    fitting_parameters <- list("method" = "yeo_johnson")
    
  } else {
    ..error_reached_unreachable_code(paste0(
      "create_transformation_parameter_skeleton: encountered an unknown transformation method: ",
      paste_s(method)))
  }
  
  # Set estimation method (optimisation criterion).
  fitting_parameters <- c(
    fitting_parameters,
    list("estimation_method" = optimisation_criterion)
  )
  
  # Set shift argument.
  fitting_parameters <- c(
    fitting_parameters,
    list("shift" = !grepl(pattern = "non_shift", x = method))
  )
  
  # Set robust argument
  fitting_parameters <- c(
    fitting_parameters,
    list("robust" = grepl(pattern = "robust", x = method))
  )
  
  # Set significance level for the empirical goodness-of-fit test.
  if (!is.null(gof_p_value)) {
    fitting_parameters <- c(
      fitting_parameters,
      list("empirical_gof_normality_p_value" = gof_p_value)
    )
  } 
  
  # Provide lambda.
  if (!is.null(lambda)) {
    fitting_parameters <- c(
      fitting_parameters,
      list("lambda" = lambda)
    )
    
    # Do not determine shift -- external lambda is only set using external
    # objects.
    fitting_parameters$shift <- FALSE
  }
  
  object <- methods::new(
    "featureInfoParametersTransformationPowerTransform",
    fitting_parameters = fitting_parameters
  )
  
  # Set the name of the object.
  object@name <- feature_name

  # Update the familiar version.
  object <- add_package_version(object = object)

  return(object)
}



add_transformation_parameters <- function(
    cl = NULL,
    feature_info_list,
    data,
    verbose = FALSE) {
  # Determine transformation parameters and add them to the feature_info_list.

  # Find feature columns.
  feature_names <- get_feature_columns(x = data)

  # Sanity check.
  if (!(setequal(
    feature_names,
    get_available_features(feature_info_list = feature_info_list)))) {
    ..error_reached_unreachable_code(paste0(
      "add_transformation_parameters: features in data and the feature info ",
      "list are expect to be the same, but were not."))
  }

  # Iterate over features.
  updated_feature_info <- fam_mapply(
    cl = cl,
    FUN = .add_transformation_parameters,
    feature_info = feature_info_list[feature_names],
    data = data@data[, mget(feature_names)],
    progress_bar = verbose,
    chopchop = TRUE)

  # Provide names for the updated feature info objects.
  names(updated_feature_info) <- feature_names

  # Replace list elements.
  feature_info_list[feature_names] <- updated_feature_info

  return(feature_info_list)
}



.add_transformation_parameters <- function(
    feature_info,
    data) {
  # Pass to underlying function that adds the feature info.
  object <- add_feature_info_parameters(
    object = feature_info@transformation_parameters,
    data = data)

  # Update transformation_parameters slot.
  feature_info@transformation_parameters <- object

  return(feature_info)
}



# add_feature_info_parameters (general, ANY) ---------------------------------
setMethod(
  "add_feature_info_parameters",
  signature(
    object = "featureInfoParametersTransformationPowerTransform",
    data = "ANY"),
  function(
    object,
    data,
    ...) {
    # Check if all required parameters have been set.
    if (feature_info_complete(object)) return(object)
    
    # Check if the power.transform package is present.
    if (!require_package("power.transform", message_type = "silent")) {
      object@method <- "none"
      object@complete <- TRUE
      
      return(object)
    }

    # The case where all data are missing is not handled by the power.transform
    # package, which (correctly) throws an error.
    if (is.numeric(data)) {
      if (all(!is.finite(data))) {
        object@fitting_parameters$method <- "none"
      }
    }
    
    # Create transformer using the power.transform package. Suppress specific
    # types of warnings related to the input data.
    transformer <- suppressWarnings(
      do.call(
        power.transform::find_transformation_parameters,
        args = c(
          list("x" = data),
          object@fitting_parameters)
      ),
      classes = c(
        "power_transform_no_transform",
        "power_transform_few_unique_values",
        "power_transform_transform_invalid_values"
      )
    )

    object@transformer <- transformer
    object@method <- power.transform::get_transformation_method(transformer)
    object@complete <- TRUE
    
    return(object)
  }
)



# apply_feature_info_parameters (general, ANY) ---------------------------------
setMethod(
  "apply_feature_info_parameters",
  signature(
    object = "featureInfoParametersTransformationPowerTransform", 
    data = "ANY"),
  function(
    object,
    data,
    invert = FALSE,
    ...) {
    
    # Check if the power.transform package is present.
    if (!require_package("power.transform", message_type = "silent")) {
      if (object@method == "none") {
        return(data)
        
      } else {
        require_package(
          "power.transform",
          purpose = "to transform features",
          message_type = "error")
      }
    }
    
    if (invert) {
      return(suppressWarnings(
        power.transform::revert_power_transform(
          y = data,
          transformer = object@transformer
        ),
        classes = "power_transform_transform_invalid_values")
      )
      
    } else {
      return(suppressWarnings(
        power.transform::power_transform(
          x = data,
          transformer = object@transformer,
          oob_action = "valid"
        ),
        classes = "power_transform_transform_invalid_values")
      )
    }
  }
)



..collect_and_aggregate_transformation_info <- function(
    feature_info_list, 
    instance_mask, 
    feature_name) {
  # Aggregate transformation parameters. This function exists so that it can be
  # tested as part of a unit test.

  # Suppress NOTES due to non-standard evaluation in data.table
  n <- method <- NULL
  
  none_object <- ..create_transformation_parameter_skeleton(
    feature_name = feature_name,
    method = "none")
  none_object@transformer <- power.transform::create_transformer_skeleton(method = "none")
  
  if (!any(instance_mask)) {
    return(list(
      "parameters" = none_object,
      "instance_mask" = instance_mask))
  }

  # Check the method of the transformation objects.
  object_method <- sapply(
    feature_info_list, 
    function(x) {
      if (is.null(x@transformation_parameters)) return("none")
      
      return(x@transformation_parameters@method)
    }
  )

  # Determine if there are any objects that exist and don't use the "none"
  # method.
  if (all(object_method[instance_mask] == "none")) {
    return(list(
      "parameters" = none_object,
      "instance_mask" = instance_mask))
  }

  # For the remaining objects, check which class occurs most.
  method_table <- data.table::data.table(
    "method" = object_method[instance_mask]
  )[, list("n" = .N), by = "method"]
  
  # Drop none transformations.
  method_table <- method_table[!method == "none"]

  # Select the method that occurs most often.
  most_common_method <- method_table[n == max(method_table$n), ]$method[1]

  # Update the instance mask.
  instance_mask <- instance_mask & object_method == most_common_method

  # Because both lambda and shift parameters may be varied, select only one
  # instance.
  selected_instance <- head(which(instance_mask), n = 1L)
  instance_mask <- logical(length(instance_mask))
  instance_mask[selected_instance] <- TRUE

  return(list(
    "parameters" = feature_info_list[[selected_instance]]@transformation_parameters,
    "instance_mask" = instance_mask))
}

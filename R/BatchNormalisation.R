#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


# batch normalisation container ------------------------------------------------
setClass(
  "featureInfoParametersBatchNormalisationContainer",
  contains = "featureInfoParameters",
  slots = list(
    "method" = "character",
    "batch_parameters" = "ANY",
    "type" = "character",
    "available" = "logical"
  ),
  prototype = list(
    "method" = NA_character_,
    "batch_parameters" = NULL,
    "type" = NA_character_,
    "available" = NA
  )
)


.get_available_batch_normalisation_methods <- function(type = "all") {
  
  # Find all basic normalisation.
  available_methods <- .get_available_normalisation_methods(type = type)
  
  if (type %in% c("combat", "combat_non_parametric", "all")) {
    available_methods <- c(
      available_methods,
      .get_available_combat_parametric_normalisation_methods()
    )
  }
  
  if (type %in% c("combat", "combat_parametric", "all")) {
    available_methods <- c(
      available_methods,
      .get_available_combat_non_parametric_normalisation_methods()
    )
  }
  
  return(available_methods)
}



create_batch_normalisation_parameter_skeleton <- function(
    feature_info_list,
    feature_names = NULL,
    normalisation_method,
    .override_existing = FALSE
) {
  
  # Creates a skeleton for the provided batch normalisation method.
  
  # Determine feature names from the feature info list, if provided.
  if (is.null(feature_names)) feature_names <- names(feature_info_list)
  
  # Select only features that appear in the feature info list.
  feature_names <- intersect(names(feature_info_list), feature_names)
  
  # Skip step if no feature info objects are updated.
  if (is_empty(feature_names)) return(feature_info_list)
  
  # Check that method is applicable.
  .check_parameter_value_is_valid(
    x = normalisation_method,
    var_name = "normalisation_method",
    values = .get_available_batch_normalisation_methods()
  )
  
  # Update familiar info objects with a feature normalisation skeleton.
  updated_feature_info <- fam_lapply(
    X = feature_info_list[feature_names],
    FUN = .create_batch_normalisation_parameter_skeleton,
    method = normalisation_method,
    .override_existing = .override_existing
  )
  
  # Provide names for the updated feature info objects.
  names(updated_feature_info) <- feature_names
  
  # Replace list elements.
  feature_info_list[feature_names] <- updated_feature_info
  
  return(feature_info_list)
}



.create_batch_normalisation_parameter_skeleton <- function(
    feature_info,
    method,
    .override_existing = FALSE
) {
  
  # Check if normalisation data was already completed, and does not require
  # being determined anew.
  if (feature_info_complete(feature_info@batch_normalisation_parameters) && !.override_existing) {
    return(feature_info)
  } 
  
  # Pass to underlying function that constructs the skeleton.
  object <- ..create_batch_normalisation_parameter_skeleton(
    feature_name = feature_info@name,
    feature_type = feature_info@feature_type,
    method = method,
    available = is_available(feature_info)
  )
  
  # Update normalisation_parameters slot.
  feature_info@batch_normalisation_parameters <- object
  
  return(feature_info)
}



..create_batch_normalisation_parameter_skeleton <- function(
    method,
    feature_name,
    feature_type = "numeric",
    available = TRUE
) {
  # This is the lowest level function for creating batch normalisation parameter
  # skeletons.
  
  # If the feature type is not numeric, or the feature is not available, change
  # the main method to "none".
  if (feature_type != "numeric" || !available) method <- "none"
  
  # Generate container.
  object <- methods::new(
    "featureInfoParametersBatchNormalisationContainer",
    "method" = method,
    "type" = feature_type,
    "available" = available
  )
  
  # Set the name of the object.
  object@name <- feature_name
  
  # Update the familiar version.
  object <- add_package_version(object = object)
  
  return(object)
}



add_batch_normalisation_parameters <- function(
    cl = NULL,
    feature_info_list,
    data,
    verbose = FALSE
) {
  # Determine normalisation parameters and add them to the feature_info_list.
  
  # Find feature columns.
  feature_names <- get_feature_columns(x = data)
  
  # Sanity check.
  if (!(setequal(feature_names, get_available_features(feature_info_list = feature_info_list)))) {
    ..error_reached_unreachable_code(paste0(
      "add_transformation_parameters: features in data and in the feature_info_list are ",
      "expected to be the same, but were not."
    ))
  }
  
  # Identify methods specified by the containers for different features.
  container_batch_methods <- sapply(
    feature_info_list[feature_names],
    function(x) (x@batch_normalisation_parameters@method)
  )
  
  # Iterate over methods. The names of features that are assessed using the same
  # batch normalisation method are dispatched to ensure computation within that
  # feature set only.
  for (container_batch_method in unique(container_batch_methods)) {
    
    # Find feature names.
    batch_method_feature_names <- feature_names[container_batch_methods == container_batch_method]
    
    # Check if a ComBat method is used, and pre-compute the z-matrix, if it is.
    if (container_batch_method %in% .get_available_batch_normalisation_methods(type = "combat")) {
      
      # Pre-compute z-matrix. Otherwise we need to compute for each feature,
      # over and over again.
      z <- .compute_combat_batch_normalisation_z_matrix(
        data = data@data,
        feature_names = batch_method_feature_names
      )
      
      if (container_batch_method %in% .get_available_batch_normalisation_methods(type = "combat_parametric")) {
        # Obtain ComBat data using the parametric solver.
        batch_parameter_data <- .combat_iterative_parametric_bayes_solver(
          z = z,
          cl = cl,
          progress_bar = FALSE
        )
        
      } else {
        # Obtain ComBat data using the non-parametric solver.
        batch_parameter_data <- .combat_iterative_parametric_bayes_solver(
          z = z,
          cl = cl,
          progress_bar = FALSE
        )
      }
      
    } else {
      batch_parameter_data <- NULL
    }
    
    # Iterate over features.
    updated_feature_info <- fam_mapply(
      cl = cl,
      FUN = .add_batch_normalisation_parameters,
      feature_info = feature_info_list[batch_method_feature_names],
      data = data@data[, mget(batch_method_feature_names)],
      MoreArgs = list(
        "batch_column_data" = data@data[[get_id_columns("batch", single_column = TRUE)]],
        "batch_parameter_data" = batch_parameter_data
      ),
      progress_bar = verbose,
      chopchop = TRUE
    )
    
    # Provide names for the updated feature info objects.
    names(updated_feature_info) <- batch_method_feature_names
    
    # Replace list elements.
    feature_info_list[batch_method_feature_names] <- updated_feature_info
  }
  
  return(feature_info_list)
}



.add_batch_normalisation_parameters <- function(
    feature_info,
    data,
    batch_column_data,
    batch_parameter_data = NULL
) {
  
  # Combine data and batch column data into a data.table again. The primary
  # reason for splitting is to prevent dispatching the full dataset to each
  # node.
  data <- data.table::data.table(
    "batch_column_id" = batch_column_data,
    "feature_value" = data
  )
  
  # Rename columns to original names.
  data.table::setnames(
    data,
    old = c("batch_column_id", "feature_value"),
    new = c(
      get_id_columns("batch", single_column = TRUE),
      feature_info@batch_normalisation_parameters@name
    )
  )
  
  # Pass to underlying function that adds the feature info.
  object <- add_feature_info_parameters(
    object = feature_info@batch_normalisation_parameters,
    data = data,
    batch_parameter_data = batch_parameter_data
  )
  
  # Update normalisation_parameters slot.
  feature_info@batch_normalisation_parameters <- object
  
  return(feature_info)
}



# add_feature_info_parameters (container, data object) -------------------------
setMethod(
  "add_feature_info_parameters",
  signature(object = "featureInfoParametersBatchNormalisationContainer", data = "dataObject"),
  function(
    object,
    data,
    ...
  ) {
    # Pass to method with signature data=data.table.
    return(add_feature_info_parameters(
      object = object,
      data = data@data,
      ...
    ))
  }
)


# add_feature_info_parameters (container, data.table) --------------------------
setMethod(
  "add_feature_info_parameters",
  signature(object = "featureInfoParametersBatchNormalisationContainer", data = "data.table"),
  function(
    object,
    data,
    ...
  ) {
    
    ## Populate container with batch parameter objects -------------------------
    
    # Find the batch identifiers in the data.
    batch_ids <- unique(data[[get_id_columns(id_depth = "batch")]])
    
    # Determine which normalisation objects already exist.
    visited_batch <- names(object@batch_parameters)
    
    # Remove batches that were already created.
    batch <- setdiff(batch_ids, visited_batch)
    
    if (length(batch) > 0L) {
      # Create batch objects.
      normalisation_objects <- fam_mapply(
        FUN = ..create_normalisation_parameter_skeleton,
        batch = batch,
        MoreArgs = list(
          "feature_name" = object@name,
          "feature_type" = object@type,
          "available" = object@available,
          "method" = object@method
        )
      )
      
      # Set names.
      names(normalisation_objects) <- batch
      
      # Append to the batch parameters.
      object@batch_parameters <- c(
        object@batch_parameters,
        normalisation_objects
      )
    }

    ## Determine batch parameters ----------------------------------------------
    
    # Select normalisation objects that have not been completed. 
    batch_to_update <- intersect(
      names(object@batch_parameters)[!sapply(object@batch_parameters, feature_info_complete)],
      batch_ids
    )
    
    if (length(batch_to_update) > 0L) {
      # Determine normalisation parameters within each batch.
      normalisation_objects <- fam_mapply(
        FUN = add_feature_info_parameters,
        object = object@batch_parameters[batch_to_update],
        data = split(data, by = get_id_columns(id_depth = "batch"))[batch_to_update],
        MoreArgs = list(...)
      )
      
      # Set names
      names(normalisation_objects) <- batch_to_update
      
      # Update batch parameters
      object@batch_parameters[batch_to_update] <- normalisation_objects
    }
    
    ## Check new none-class parameter sets -------------------------------------
    
    # Select parameter sets that are a) newly created, and b) were
    # updated. If these objects are none, for various reasons such as
    # small batches, attempt to replace them by an average object.
    batch_to_check <- intersect(batch, batch_to_update)
    
    if (length(batch_to_check) > 0L && object@method != "none") {
      # Generate a new object.
      replacement_normalisation_object <- ...collect_and_aggregate_normalisation_info(
        object_list = object@batch_parameters,
        instance_mask = rep_len(TRUE, length(object@batch_parameters)),
        feature_name = object@name
      )$parameters
      
      # Identify which new objects are none objects.
      batch_to_check <- batch_to_check[sapply(
        object@batch_parameters[batch_to_check],
        is,
        class2 = "featureInfoParametersNormalisationNone"
      )]
      
      if (length(batch_to_check) > 0L) {
        
        # Add batch attribute to normalisation objects.
        normalisation_objects <- lapply(
          batch_to_check,
          function(x, object) {
            # Add batch name
            object@batch <- x
            
            return(object)
          },
          object = replacement_normalisation_object
        )
        
        # Update names.
        names(normalisation_objects) <- batch_to_check
        
        # Replace these objects.
        object@batch_parameters[batch_to_check] <- normalisation_objects
      }
    }
    
    # Check that all objects are complete.
    if (!all(sapply(object@batch_parameters, feature_info_complete))) {
      ..error_reached_unreachable_code(paste0(
        "add_feature_info_parameters,featureInfoParametersBatchNormalisationContainer, data.table: ",
        "all normalisation objects in the container should be complete, but some were not."
      ))
    }
    
    # Mark complete.
    object@complete <- TRUE
    
    return(object)
  }
)


# apply_feature_info_parameters (container, ANY) -------------------------------
setMethod(
  "apply_feature_info_parameters",
  signature(object = "featureInfoParametersBatchNormalisationContainer", data = "ANY"),
  function(
    object,
    data,
    ...
  ) {
    
    # Suppress NOTES due to non-standard evaluation in data.table
    batch_normalisation_ordering_id <- NULL
    
    # Find the batch identifiers in the data.
    batch_ids <- unique(data[[get_id_columns(id_depth = "batch")]])
    
    # Sanity check: batch normalisation objects are available for all
    # batch identifiers.
    if (!all(batch_ids %in% names(object@batch_parameters))) {
      ..error_reached_unreachable_code(paste0(
        "apply_feature_info_parameters,featureInfoParametersBatchNormalisationContainer,ANY: ",
        "Batch normalisation objects are missing for one or more batches."
      ))
    }
    
    # Sanity check: all relevant batch normalisation objects are
    # complete.
    if (!all(sapply(object@batch_parameters[batch_ids], feature_info_complete))) {
      ..error_reached_unreachable_code(paste0(
        "apply_feature_info_parameters,featureInfoParametersBatchNormalisationContainer,ANY: ",
        "One or more batch normalisation objects are incomplete and may be missing parameters."
      ))
    }
    
    # Avoid updating by reference.
    data <- data.table::copy(data)
    
    # Insert ordering variable.
    data[, "batch_normalisation_ordering_id" := .I]
    
    # Transform per batch.
    data <- fam_mapply(
      FUN = apply_feature_info_parameters,
      object = object@batch_parameters[batch_ids],
      data = split(data, by = get_id_columns(id_depth = "batch"))[batch_ids],
      MoreArgs = list(...)
    )
    
    # Combine into a single data.table.
    data <- data.table::rbindlist(data, use.names = TRUE)
    
    # Order by ordering variable to maintain original order.
    data <- data[order(batch_normalisation_ordering_id)]
    
    # Extract the feature values and return.
    return(data[[object@name]])
  }
)



..collect_and_aggregate_batch_normalisation_info <- function(
    feature_info_list,
    instance_mask,
    feature_name
) {
  # Aggregate batch normalisation parameters. This function exists so that it
  # can be tested as part of a unit test.
  
  # Find all batches in the parameters.
  batch_ids <- unique(unlist(lapply(
    feature_info_list,
    function(x) {
      # Return NULL if parameters are missing.
      if (is.null(x@batch_normalisation_parameters)) return(NULL)
      
      return(names(x@batch_normalisation_parameters@batch_parameters))
    }
  )))
  
  # Main batch method
  main_batch_method <- unique(unlist(lapply(
    feature_info_list,
    function(x) {
      # Return NULL if parameters are missing.
      if (is.null(x@batch_normalisation_parameters)) return(NULL)
      
      return(x@batch_normalisation_parameters@method)
    }
  )))
  
  # Create parameter container
  batch_parameter_container <- ..create_batch_normalisation_parameter_skeleton(
    method = main_batch_method[1L],
    feature_name = feature_name
  )
  
  if (!any(instance_mask)) {
    # Create placeholder objects.
    batch_normalisation_objects <- lapply(
      batch_ids,
      function(batch, feature_name) {
        ..create_normalisation_parameter_skeleton(
          feature_name = feature_name,
          method = "none",
          batch = batch
        )
      },
      feature_name = feature_name
    )
    
    # Add names.
    names(batch_normalisation_objects) <- batch_ids
    
    # Add to container.
    batch_parameter_container@batch_parameters <- batch_normalisation_objects
    
    return(list(
      "parameters" = batch_parameter_container,
      "instance_mask" = instance_mask
    ))
  }
  
  # Initialise list of objects.
  batch_normalision_object_list <- list()
  
  # Iterate over batch identifiers and collect all batch identifiers.
  for (batch_id in batch_ids) {
    
    # Collect batch normalisation objects.
    batch_normalisation_objects <- lapply(
      feature_info_list[instance_mask],
      function(x, batch_id) {
        # Return NULL if parameters are missing.
        if (is.null(x@batch_normalisation_parameters)) return(NULL)
        
        # Return parameters for the specific batch.
        return(x@batch_normalisation_parameters@batch_parameters[[batch_id]])
      },
      batch_id = batch_id
    )
    
    # Collect aggregate object.
    batch_normalision_object_list[[batch_id]] <- ...collect_and_aggregate_normalisation_info(
      object_list = batch_normalisation_objects,
      instance_mask = rep_len(TRUE, length(batch_normalisation_objects)),
      feature_name = feature_name
    )$parameters
    
    # Set batch name.
    batch_normalision_object_list[[batch_id]]@batch <- batch_id
  }
  
  # Add batch objects to container.
  batch_parameter_container@batch_parameters <- batch_normalision_object_list
  
  return(list(
    "parameters" = batch_parameter_container,
    "instance_mask" = instance_mask
  ))
}



.check_batch_normalisation_assumptions <- function(
  data,
  normalisation_method
) {
  # Check that batches do not differ in outcome data. We can use the following
  # tests: 
  #
  # * Continuous: Kruskal-Wallis-test.
  # * Binomial / multinomial: Chi-squared test.
  # * Survival: Log-rank test.
  
  if (all(normalisation_method == "none")) return(invisible(TRUE))
  
  x <- data@data[, mget(get_outcome_columns(data))]
  g <- data@data[[get_id_columns("batch", single_column = TRUE)]]
  
  # Determine the number of batches.
  n_groups <- data.table::uniqueN(g)
  # Check that 2 (or more groups) are present.
  if (n_groups < 2L) return(invisible(TRUE))
  
  if (data@outcome_type == "continuous") {
    h <- tryCatch(
      stats::kruskal.test(
        x = x[[1L]],
        g = g, 
        na.action = "na.omit"
      ),
      error = identity
    )
    
    # Check if the test statistic could be computed.
    if (inherits(h, "error")) return(invisible(TRUE))
    p_value <- h$p.value
    
  } else if (data@outcome_type %in% c("binomial", "multinomial")) {
    h <- tryCatch(
      stats::chisq.test(x = x, y = g),
      error = identity
    )
    
    # Check if the test statistic could be computed.
    if (inherits(h, "error")) return(invisible(TRUE))
    p_value <- h$p.value
    
  } else if (data@outcome_type == "survival") {
    
    # Determine chi-square of log-rank test
    chi_sq <- tryCatch(
      survival::survdiff(
        survival::Surv(time = outcome_time, event = outcome_event) ~ group,
        data = data.table::data.table(
          outcome_time = x$outcome_time,
          outcome_event = x$outcome_event,
          group = g
        ),
        subset = NULL,
        na.action = "na.omit"
      )$chisq,
      error = identity
    )
    
    # Check if the test statistic could be computed. Causes could be lack of
    # events, no events beyond the first time point, etc.
    if (inherits(chi_sq, "error")) return(invisible(TRUE))
    
    # Derive  p-value
    p_value  <- stats::pchisq(
      q = chi_sq,
      df = n_groups - 1L,
      lower.tail = FALSE
    )
    
  } else {
    ..error_outcome_type_not_implemented(data@outcome_type)
  }
  
  if (p_value < 0.05) {
    logger_warning(
      paste0(
        "One or more batches have a statistically significant (p < 0.05) different outcome ",
        "compared to other batches. Note: a statistically significant outcome does ",
        "not mean that the difference is actually relevant. However, please assert ",
        "that batch normalisation does not remove important differences between batches."
      ),
      warn_class = "familiar_batch_outcome_difference"
    )
    return(invisible(FALSE))
  }
  
  
  return(invisible(TRUE))
}

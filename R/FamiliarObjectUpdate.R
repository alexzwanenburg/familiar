#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
#' @include Transformation.R
NULL

# update_object (generic) ------------------------------------------------------

#' @title Update familiar S4 objects to the most recent version.
#'
#' @description Provides backward compatibility for familiar objects exported to
#'   a file. This mitigates compatibility issues when working with files that
#'   become outdated as new versions of familiar are released, e.g. because
#'   slots have been removed.
#'
#' @param object A `familiarModel`, a `familiarEnsemble`, a `familiarData` or
#'   `familiarCollection` object.
#' @param ... Unused arguments.
#'
#' @return An up-to-date version of the respective S4 object.
#' @exportMethod update_object
#' @md
#' @rdname update_object-methods
setGeneric("update_object", function(object, ...) standardGeneric("update_object"))



## update_object (familiarModel) -----------------------------------------------

#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "familiarModel"),
  function(object, ...) {
    if (tail(object@familiar_version, n = 1L) < "0.0.0.54") {
      # Rename req_feature_cols to required_features
      attr(object, "required_features") <- attr(object, "req_feature_cols")
      attr(object, "req_feature_cols") <- NULL

      # Rename important_features to model_features
      attr(object, "model_features") <- attr(object, "important_features")
      attr(object, "important_features") <- NULL

      # Introduce missing novelty_features slot by copying model_features.
      attr(object, "novelty_features") <- attr(object, "model_features")

      # Remove signature altogether.
      attr(object, "signature") <- NULL

      # Check that the model has been successfully trained. Since version
      # 0.0.0.54, features from models that have not been trained will not be
      # retained for further evaluation.
      if (!model_is_trained(object)) {
        object@required_features <- NULL
        object@model_features <- NULL
        object@novelty_features <- NULL
      }

      # Add name attribute.
      attr(object, "name") <- NA_character_
    }

    if (tail(object@familiar_version, n = 1L) < "0.0.0.55") {
      # Add missing attributes.
      attr(object, "trimmed_function") <- list()
      attr(object, "learner_package") <- character(0L)
      attr(object, "learner_version") <- as.package_version("0.0.0")

      # Rename is_anonymised.
      attr(object, "is_trimmed") <- attr(object, "is_anonymised")
      attr(object, "is_anonymised") <- NULL
    }

    if (tail(object@familiar_version, n = 1L) < "1.0.0") {
      # Rename learner_package to package
      attr(object, "package") <- attr(object, "learner_package")
      if (is.na(object@package)) {
        methods::slot(object, "package", check = FALSE) <- NULL
      }

      # Rename learner_version to package_version
      attr(object, "package_version") <- attr(object, "learner_version")
      if (object@learner_version == as.package_version("0.0.0")) {
        methods::slot(object, "package_version", check = FALSE) <- NULL
      }

      # Remove learner_package and learner_version attributes.
      attr(object, "learner_package") <- NULL
      attr(object, "learner_version") <- NULL

      # Replace any novelty detector, since these are now proper classes.
      methods::slot(object, "novelty_detector", check = FALSE) <- NULL
    }

    if (tail(object@familiar_version, n = 1L) < "1.1.0") {
      # Add placeholder messages attribute.
      attr(object, "messages") <- list()
    }

    # Update attached feature info objects.
    feature_names <- names(object@feature_info)
    if (length(feature_names) > 0) {
      object@feature_info <- lapply(
        object@feature_info, 
        update_object)
      names(object@feature_info) <- feature_names
    }

    # Update attached novelty detector
    if (!is.null(object@novelty_detector)) {
      object@novelty_detector <- update_object(object@novelty_detector)
    }


    if (tail(object@familiar_version, n = 1L) < "1.3.0" &&
        is(object, "familiarGLM")) {
      # Add feature_order slot to familiarGLM objects.
      attr(object, "feature_order") <- character()
    }

    if (tail(object@familiar_version, n = 1L) < "1.4.0") {
      # Add a robust slot to familiarMetricRegression objects.
      for (ii in seq_along(object@hyperparameter_data$metric_object)) {
        if (is(object@hyperparameter_data$metric_object[[ii]], "familiarMetricRegression")) {
          attr(object@hyperparameter_data$metric_object[[ii]], "robust") <- "none"
        }
      }
    }

    if (!methods::validObject(object)) {
      stop("Could not update the familiarModel object to the most recent definition.")
    }

    # Update package version.
    object <- add_package_version(object = object)

    return(object)
  }
)



## update_object (familiarEnsemble) --------------------------------------------

#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "familiarEnsemble"),
  function(object, ...) {
    if (tail(object@familiar_version, n = 1L) < "0.0.0.54") {
      # Rename req_feature_cols to required_features
      attr(object, "required_features") <- attr(object, "req_feature_cols")
      attr(object, "req_feature_cols") <- NULL

      # Rename important_features to model_features
      attr(object, "model_features") <- attr(object, "important_features")
      attr(object, "important_features") <- NULL

      # Introduce missing novelty_features slot by copying
      # model_features.
      attr(object, "novelty_features") <- attr(object, "model_features")

      # Set default model_dir_path, auto_detach and name attributes.
      attr(object, "model_dir_path") <- NA_character_
      attr(object, "auto_detach") <- FALSE
      attr(object, "name") <- NA_character_
    }

    if (tail(object@familiar_version, n = 1L) < "0.0.0.55") {
      # Remove is_anonymised.
      attr(object, "is_anonymised") <- NULL
    }

    # Update attached feature info objects.
    feature_names <- names(object@feature_info)
    object@feature_info <- lapply(
      object@feature_info,
      update_object)
    names(object@feature_info) <- feature_names

    if (!methods::validObject(object)) {
      stop("Could not update the familiarEnsemble object to the most recent definition.")
    }

    # Update package version.
    object <- add_package_version(object = object)

    return(object)
  }
)


## update_object (familiarData) ------------------------------------------------

#' @rdname update_object-methods
setMethod(
  "update_object", signature(object = "familiarData"),
  function(object, ...) {
    if (tail(object@familiar_version, n = 1L) < "0.0.0.54") {
      # Rename req_feature_cols to required_features
      attr(object, "required_features") <- attr(object, "req_feature_cols")
      attr(object, "req_feature_cols") <- NULL

      # Rename important_features to model_features
      attr(object, "model_features") <- attr(object, "important_features")
      attr(object, "important_features") <- NULL

      # Rename mutual_correlation to feature_similarity
      attr(object, "feature_similarity") <- attr(object, "mutual_correlation")
      attr(object, "mutual_correlation") <- NULL

      # Add sample_similarity.
      attr(object, "sample_similarity") <- attr(list(), "non_existing_element")
    }

    if (tail(object@familiar_version, n = 1L) < "0.0.0.55") {
      # Remove is_anonymised.
      attr(object, "is_anonymised") <- NULL
    }

    if (tail(object@familiar_version, n = 1L) < "1.2.0") {
      # Update variable importance lists.
      # Update fs_vimp slot by iterating over its contents.
      object@fs_vimp <- lapply(
        object@fs_vimp,
        function(x, project_id, fs_method) {
          # Check if the current element is empty.
          if (is_empty(x)) return(x)
          
          # Iterate over variable importance.
          x@data <- lapply(
            split(x@data, by = c("data_id", "run_id")),
            function(x, fs_method, project_id) {
              as_vimp_table_object(
                x = list("vimp" = x, "fs_method" = fs_method),
                project_id = project_id)
            },
            fs_method = fs_method,
            project_id = project_id
          )
          
          return(x)
        },
        fs_method = object@fs_method,
        project_id = object@project_id)
      
      # Update model_vimp slot by iterating over its contents.
      object@model_vimp <- lapply(
        object@model_vimp,
        function(x, project_id, learner) {
          # Check if the current element is empty.
          if (is_empty(x)) return(x)
          
          # Iterate over variable importance.
          x@data <- lapply(
            split(x@data, by = c("data_id", "run_id")),
            function(x, learner, project_id) {
              as_vimp_table_object(
                x = list("vimp" = x, "fs_method" = learner),
                project_id = project_id)
            },
            learner = learner,
            project_id = project_id
          )
          
          return(x)
        },
        learner = object@learner,
        project_id = object@project_id)
      
      # Update feature_expressions slot by iterating over its contents.
      object@feature_expressions <- lapply(
        object@feature_expressions,
        function(x) {
          x@feature_info <- update_object(x@feature_info)
          
          return(x)
        })
    }
    
    if (tail(object@familiar_version, n = 1L) < "1.5.0") {
      # Update feature_expressions slot by iterating over its contents. This is
      # to account for changes in transformation objects, introduced with 1.5.0.
      object@feature_expressions <- lapply(
        object@feature_expressions,
        function(x) {
          x@feature_info <- update_object(x@feature_info)
          
          return(x)
        }
      )
    }
    
    if (!methods::validObject(object)) {
      stop("Could not update the familiarData object to the most recent definition.")
    }
    
    # Update package version.
    object <- add_package_version(object = object)

    return(object)
  }
)



## update_object (familiarCollection) ------------------------------------------

#' @rdname update_object-methods
setMethod(
  "update_object", signature(object = "familiarCollection"),
  function(object, ...) {
    if (tail(object@familiar_version, n = 1L) < "0.0.0.54") {
      # Rename req_feature_cols to required_features
      attr(object, "required_features") <- attr(object, "req_feature_cols")
      attr(object, "req_feature_cols") <- NULL

      # Rename important_features to model_features
      attr(object, "model_features") <- attr(object, "important_features")
      attr(object, "important_features") <- NULL

      # Rename collection_name to name
      attr(object, "name") <- attr(object, "collection_name")
      attr(object, "collection_name") <- NULL

      # Rename mutual_correlation to feature_similarity
      attr(object, "feature_similarity") <- attr(object, "mutual_correlation")
      attr(object, "mutual_correlation") <- NULL

      # Add sample_similarity.
      attr(object, "sample_similarity") <- attr(list(), "non_existing_element")
    }

    if (tail(object@familiar_version, n = 1L) < "0.0.0.55") {
      # Remove is_anonymised.
      attr(object, "is_anonymised") <- NULL
    }

    if (tail(object@familiar_version, n = 1L) < "1.2.0") {
      # Update fs_vimp slot by iterating over its contents.
      object@fs_vimp <- lapply(
        object@fs_vimp,
        function(x, project_id) {
          # Check if the current element is empty.
          if (is_empty(x)) return(x)
          
          # Iterate over variable importance.
          x@data <- lapply(
            split(x@data, by = c("data_id", "run_id")),
            function(x, fs_method, project_id) {
              as_vimp_table_object(
                x = list("vimp" = x, "fs_method" = fs_method),
                project_id = project_id)
            },
            fs_method = x@identifiers$fs_method,
            project_id = project_id)
          
          return(x)
        },
        project_id = object@project_id)
      
      # Update model_vimp slot by iterating over its contents.
      object@model_vimp <- lapply(
        object@model_vimp,
        function(x, project_id) {
          # Check if the current element is empty.
          if (is_empty(x)) return(x)
          
          # Iterate over variable importance.
          x@data <- lapply(
            split(x@data, by = c("data_id", "run_id")),
            function(x, fs_method, project_id) {
              as_vimp_table_object(
                x = list("vimp" = x, "fs_method" = fs_method),
                project_id = project_id)
            },
            fs_method = x@identifiers$learner,
            project_id = project_id)
          
          return(x)
        },
        project_id = object@project_id)

      # Update feature_expressions slot by iterating over its contents.
      object@feature_expressions <- lapply(
        object@feature_expressions,
        function(x) {
          x@feature_info <- update_object(x@feature_info)
          
          return(x)
        })
    }
    
    if (tail(object@familiar_version, n = 1L) < "1.5.0") {
      # Update feature_expressions slot by iterating over its contents. This is
      # to account for changes in transformation objects, introduced with 1.5.0.
      object@feature_expressions <- lapply(
        object@feature_expressions,
        function(x) {
          x@feature_info <- update_object(x@feature_info)
          
          return(x)
        }
      )
    }
    
    if (!methods::validObject(object)) {
      stop("Could not update the familiarCollection object to the most recent definition.")
    }
    
    # Update package version.
    object <- add_package_version(object = object)
    
    return(object)
  }
)



## update_object (vimpTable) ---------------------------------------------------
#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "vimpTable"),
  function(object, ...) {
    # Update package version.
    object <- add_package_version(object = object)

    return(object)
  }
)



## update_object (familiarNoveltyDetector) -------------------------------------
#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "familiarNoveltyDetector"),
  function(object, ...) {
    # Update attached feature info objects.
    feature_names <- names(object@feature_info)
    if (length(feature_names) > 0) {
      object@feature_info <- lapply(object@feature_info, update_object)
      names(object@feature_info) <- feature_names
    }

    if (!methods::validObject(object)) {
      stop("Could not update the familiarNoveltyDetector object to the most recent definition.")
    }

    # Update package version.
    object <- add_package_version(object = object)

    return(object)
  }
)



## update_object (featureInfo) -------------------------------------------------
#' @rdname update_object-methods
setMethod(
  "update_object", signature(object = "featureInfo"),
  function(object, ...) {
    
    # Add a placeholder familiar version slot if necessary.
    if (!methods::.hasSlot(object, "familiar_version")) {
      attr(object, "familiar_version") <- as.package_version("0.0.0")
    }

    if (tail(object@familiar_version, n = 1L) < "1.2.0") {
      # Prior to version 1.2.0, information used to process features was stored
      # in ad-hoc lists. Since version 1.2.0 these have been replaced by S4
      # objects, which allows for much cleaner testing and updating.
      
      # Check if the object is generic/unset or was filled.
      is_unset <- is.null(object@transformation_parameters) &&
        is.null(object@normalisation_parameters) &&
        is.null(object@batch_normalisation_parameters) &&
        is.null(object@imputation_parameters) &&
        is.null(object@cluster_parameters)

      # Only set attributes if a proper
      if (!is_unset) {
        ### Transformation -----------------------------------------------------
        
        # Upgrade transformation parameters to a proper S4 object.
        if (!is.null(object@transformation_parameters)) {
          object@transformation_parameters <- ..create_transformation_parameter_skeleton(
            feature_name = object@name,
            feature_type = object@feature_type,
            available = is_available(object),
            method = object@transformation_parameters$transform_method,
            lambda = object@transformation_parameters$transform_lambda)
          
        } else {
          object@transformation_parameters <- ..create_transformation_parameter_skeleton(
            feature_name = object@name,
            feature_type = object@feature_type,
            available = is_available(object),
            method = "none")
        }
        
        # Revise familiar version, because these now correspond basically to
        # version 1.4.8 and earlier. Version 1.5.0 introduces transformers from
        # power.transform.
        object@transformation_parameters@familiar_version[
          length(object@transformation_parameters@familiar_version)
        ] <- package_version("1.4.8")

        ### Normalisation ------------------------------------------------------

        # Upgrade normalisation parameters to a proper S4 object.
        if (!is.null(object@normalisation_parameters)) {
          object@normalisation_parameters <- ..create_normalisation_parameter_skeleton(
            feature_name = object@name,
            feature_type = object@feature_type,
            available = is_available(object),
            method = object@normalisation_parameters$norm_method,
            shift = object@normalisation_parameters$norm_shift,
            scale = object@normalisation_parameters$norm_scale)
          
        } else {
          object@normalisation_parameters <- ..create_normalisation_parameter_skeleton(
            feature_name = object@name,
            feature_type = object@feature_type,
            available = is_available(object),
            method = "none")
        }

        ### Batch normalisation ------------------------------------------------

        # Upgrade batch normalisation parameters to a proper S4 object.
        if (!is.null(object@batch_normalisation_parameters)) {
          # Determine the method used for batch normalisation.
          batch_normalisation_method <- unique(sapply(
            object@batch_normalisation_parameters,
            function(x) (x$norm_method)))
          batch_normalisation_method <- setdiff(
            batch_normalisation_method, c("none", "unknown"))
          
          if (is_empty(batch_normalisation_method)) {
            batch_normalisation_method <- "none"
          }
          
        } else {
          batch_normalisation_method <- "none"
        }

        # Add container.
        batch_normalisation_parameters <- ..create_batch_normalisation_parameter_skeleton(
          feature_name = object@name,
          feature_type = object@feature_type,
          available = is_available(object),
          method = batch_normalisation_method)

        # Update the container contents.
        batch_normalisation_parameters@batch_parameters <- mapply(
          function(x, batch, object) {
            return(..create_normalisation_parameter_skeleton(
              feature_name = object@name,
              feature_type = object@feature_type,
              available = is_available(object),
              method = x$norm_method,
              batch = batch,
              shift = x$norm_shift,
              scale = x$norm_scale))
          },
          x = object@batch_normalisation_parameters,
          batch = names(object@batch_normalisation_parameters),
          MoreArgs = list("object" = object))

        # Update names.
        names(batch_normalisation_parameters@batch_parameters) <- names(object@batch_normalisation_parameters)

        # Update batch normalisations parameters in the object.
        object@batch_normalisation_parameters <- batch_normalisation_parameters

        # Mark complete
        object@batch_normalisation_parameters@complete <- TRUE

        ### Imputation ---------------------------------------------------------

        # Update imputation parameters to a proper S4 object.
        imputation_method <- ifelse(
          is.null(object@imputation_parameters),
          "none",
          "simple")

        # Create an S4 skeleton.
        imputation_object <- ..create_imputation_parameter_skeleton(
          feature_name = object@name,
          feature_type = object@feature_type,
          available = is_available(object),
          method = imputation_method)

        # Attach to imputation object.
        if (!is.null(object@imputation_parameters)) {
          imputation_object@model <- object@imputation_parameters$common_value
        }

        # Set required features.
        imputation_object@required_features <- object@name

        # Mark complete
        imputation_object@complete <- TRUE

        # Attach to object.
        object@imputation_parameters <- imputation_object

        ### Clustering ---------------------------------------------------------

        if (!is.null(object@cluster_parameters)) {
          # Set parameters -- we can't really infer cluster linkage or the
          # similarity metric from available information, and set placeholders
          # that will allow the cluster parameter object to be formed.

          # Create temporary feature info object to avoid an incorrect check.
          temp_feature_object <- object
          temp_feature_object@cluster_parameters <- NULL

          cluster_parameter_options <- list(
            "feature_info" = temp_feature_object,
            "method" = "hclust",
            "cluster_cut_method" = "fixed_cut",
            "cluster_representation_method" = object@cluster_parameters$method,
            "cluster_linkage" = "average",
            "cluster_similarity_metric" = "mcfadden_r2",
            "cluster_similarity_threshold" = 0.3)

          # Form parameter object.
          cluster_parameter_object <- do.call(
            .create_cluster_parameter_skeleton,
            args = cluster_parameter_options)

          # Extract cluster parameters.
          cluster_parameter_object <- cluster_parameter_object@cluster_parameters

          # Set parameters.
          cluster_parameter_object@weight <- object@cluster_parameters$weight
          cluster_parameter_object@invert <- object@cluster_parameters$invert
          cluster_parameter_object@cluster_name <- ifelse(
            object@cluster_parameters$cluster_size > 1,
            object@cluster_parameters$cluster_name,
            object@name)
          cluster_parameter_object@cluster_size <- object@cluster_parameters$cluster_size
          cluster_parameter_object@required_features <- object@cluster_parameters$required_features

          # Previously, features that form the cluster were not stored as
          # information. To prevent issues with functions that generate a
          # cluster table, set the required features instead.
          cluster_parameter_object@cluster_features <- unique(c(
            object@cluster_parameters$required_features,
            object@name))

          # Mark complete.
          cluster_parameter_object@complete <- TRUE

          # Replace in the feature info object.
          object@cluster_parameters <- cluster_parameter_object
          
        } else {
          # Assume singular cluster.
          cluster_parameter_options <- list(
            "feature_info" = object,
            "method" = "none")

          # Form parameter object.
          cluster_parameter_object <- do.call(
            .create_cluster_parameter_skeleton,
            args = cluster_parameter_options)
          
          # Extract cluster parameters.
          cluster_parameter_object <- cluster_parameter_object@cluster_parameters

          # Set parameters.
          cluster_parameter_object@weight <- 1.0
          cluster_parameter_object@invert <- FALSE
          cluster_parameter_object@cluster_name <- object@name
          cluster_parameter_object@cluster_size <- 1L
          cluster_parameter_object@required_features <- object@name
          cluster_parameter_object@cluster_features <- object@name

          # Mark complete.
          cluster_parameter_object@complete <- TRUE

          # Replace in the feature info object.
          object@cluster_parameters <- cluster_parameter_object
        }
      }
    }
    
    # Update objects separately.
    object@transformation_parameters <- update_object(object =  object@transformation_parameters)
    object@normalisation_parameters <- update_object(object = object@normalisation_parameters)
    object@batch_normalisation_parameters <- update_object(object = object@batch_normalisation_parameters)
    object@imputation_parameters <- update_object(object = object@imputation_parameters)
    object@cluster_parameters <- update_object(object = object@cluster_parameters)
    
    if (!methods::validObject(object)) {
      stop("Could not update the featureInfo object to the most recent definition.")
    }

    # Update package version.
    object <- add_package_version(object = object)

    return(object)
  }
)



## update_object (featureInfoParametersTransformationPowerTransform) -----------

#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "featureInfoParametersTransformationPowerTransform"),
  function(object, ...) {
    
    if (tail(object@familiar_version, n = 1L) < "1.5.0") {
      # Transformation objects are now implemented using power.transform.
      if (is(object, "featureInfoParametersTransformationNone")) {
        transformer <- power.transform::create_transformer_skeleton(method = "none")
        object <- methods::new(
          "featureInfoParametersTransformationPowerTransform",
          name = object@name,
          familiar_version = object@familiar_version
        )
        
      } else if (is(object, "featureInfoParametersTransformationBoxCox")) {
        transformer <- power.transform::create_transformer_skeleton(
          method = "box_cox",
          lambda = object@lambda
        )
        object <- methods::new(
          "featureInfoParametersTransformationPowerTransform",
          name = object@name,
          familiar_version = object@familiar_version
        )
        
      } else if (is(object, "featureInfoParametersTransformationYeoJohnson")) {
        transformer <- power.transform::create_transformer_skeleton(
          method = "yeo_johnson",
          lambda = object@lambda
        )
        object <- methods::new(
          "featureInfoParametersTransformationPowerTransform",
          name = object@name,
          familiar_version = object@familiar_version
        )
        
      } else {
        # Without explicit class (original object pre v1.2.0).
        transformer <- power.transform::create_transformer_skeleton(
          method = object@fitting_parameters$method,
          lambda = object@fitting_parameters$lambda
        )
      }
      
      object@complete <- TRUE
      object@transformer <- transformer
      object@method <- power.transform::get_transformation_method(transformer)
    }
    
    if (!methods::validObject(object)) {
      stop(paste0(
        "Could not update the featureInfoParametersTransformationPowerTransform ",
        "object to the most recent definition."))
    }
    
    # Update package version.
    object <- add_package_version(object = object)
    
    return(object)
  }
)



## update_object (experimentData) ----------------------------------------------

#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "experimentData"),
  function(object, ...) {
    # Update feature info objects.
    if (!is.null(object@feature_info)) {
      for (feature_info_set in names(object@feature_info)) {
        object@feature_info[[feature_info_set]] <- lapply(
          object@feature_info[[feature_info_set]],
          update_object)
      }
    }

    # Update vimp tables.
    if (!is.null(object@vimp_table_list)) {
      for (vimp_method in names(object@vimp_table_list)) {
        object@vimp_table_list[[vimp_method]] <- lapply(
          object@vimp_table_list[[vimp_method]],
          update_object)
      }
    }

    if (!methods::validObject(object)) {
      stop("Could not update the experimentData object to the most recent definition.")
    }

    # Update package version.
    object <- add_package_version(object = object)

    return(object)
  }
)



## update_object (list) ------------------------------------------------------

#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "list"),
  function(object, ...) {
    # Pass to underlying methods.
    object <- lapply(
      object,
      update_object,
      ...)

    return(object)
  }
)



## update_object (ANY) ------------------------------------------------------
#' @rdname update_object-methods
setMethod(
  "update_object",
  signature(object = "ANY"),
  function(object, ...) {
    # Fallback method for missing or unknown items.
    return(object)
  }
)

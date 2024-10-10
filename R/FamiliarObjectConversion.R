#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# as_familiar_ensemble (generic) -----------------------------------------------

#' @title Conversion to familiarEnsemble object.
#'
#' @description Creates `familiarEnsemble` a object from `familiarModel`
#'   objects.
#'
#' @param object A `familiarEnsemble` object, or one or more `familiarModel`
#'   objects that will be internally converted to a `familiarEnsemble` object.
#'   Paths to such objects can also be provided.
#' @param ... Unused arguments.
#'
#' @return A `familiarEnsemble` object.
#' @exportMethod as_familiar_ensemble
#' @md
#' @rdname as_familiar_ensemble-methods
setGeneric(
  "as_familiar_ensemble",
  function(object, ...) standardGeneric("as_familiar_ensemble")
)



## as_familiar_ensemble (ensemble) ---------------------------------------------

#' @rdname as_familiar_ensemble-methods
setMethod(
  "as_familiar_ensemble",
  signature(object = "familiarEnsemble"),
  function(object, ...) {
    return(object)
  }
)



## as_familiar_ensemble (model) ------------------------------------------------

#' @rdname as_familiar_ensemble-methods
setMethod(
  "as_familiar_ensemble",
  signature(object = "familiarModel"),
  function(object, ...) {
    # A separate familiar model is encapsulated in a list, and then transformed.
    return(do.call(
      as_familiar_ensemble,
      args = list("object" = list(object))
    ))
  }
)


## as_familiar_ensemble (novelty) ----------------------------------------------

#' @rdname as_familiar_ensemble-methods
setMethod(
  "as_familiar_ensemble",
  signature(object = "familiarNoveltyDetector"),
  function(object, ...) {
    # A separate familiar novelty detector is encapsulated in a list, and then
    # transformed.
    return(do.call(
      as_familiar_ensemble,
      args = list("object" = list(object))
    ))
  }
)


## as_familiar_ensemble (list) -------------------------------------------------

#' @rdname as_familiar_ensemble-methods
setMethod(
  "as_familiar_ensemble",
  signature(object = "list"),
  function(object, ...) {
    # Load familiar objects. This does nothing if the list already contains only
    # familiar S4 objects, but will load any files from the path and will check
    # uniqueness of classes.
    object <- load_familiar_object(object = object)

    # Return the object if it contains a single familiarEnsemble.
    if (length(object) == 1L && all(sapply(object, is, "familiarEnsemble"))) {
      return(object[[1L]])
      
    } else if (
      !all(sapply(object, is, "familiarModel")) &&
      !all(sapply(object, is, "familiarNoveltyDetector"))
    ) {
      ..error(paste0(
        "familiarEnsemble objects can only be constructed from familiarModel ",
        "or familiarNoveltyDetector objects."
      ))
    }

    # Generate a placeholder pooling table
    run_table <- data.table::data.table(
      "data_id" = 0L,
      "run_id" = 0L,
      "can_pre_process" = TRUE, 
      "perturbation" = "new_data",
      "perturb_level" = 0L
    )

    vimp_method <- ifelse(
      methods::.hasSlot(object[[1L]], "fs_method"),
      object[[1L]]@fs_method,
      "none"
    )
    
    # Generate a skeleton familiarEnsemble
    fam_ensemble <- methods::new(
      "familiarEnsemble",
      model_list = object,
      learner = object[[1L]]@learner,
      fs_method = vimp_method,
      run_table = list(
        "run_table" = run_table, 
        "ensemble_data_id" = 0L, 
        "ensemble_run_id" = 0L
      )
    )

    # Add package version.
    fam_ensemble <- add_package_version(object = fam_ensemble)

    # Complete the ensemble using information provided by the model(s)
    fam_ensemble <- complete_familiar_ensemble(object = fam_ensemble)

    return(fam_ensemble)
  }
)



## as_familiar_ensemble (character) --------------------------------------------

#' @rdname as_familiar_ensemble-methods
setMethod(
  "as_familiar_ensemble",
  signature(object = "character"),
  function(object, ...) {
    # Interpret character as if it is a path, and pass to the same method for
    # list objects.
    return(do.call(
      as_familiar_ensemble,
      args = list("object" = as.list(object))
    ))
  }
)



## as_familiar_ensemble (general) ----------------------------------------------

#' @rdname as_familiar_ensemble-methods
setMethod(
  "as_familiar_ensemble", signature(object = "ANY"),
  function(object, ...) {
    # There familiar ensembles can only be generated from one of the above
    # functions.
    ..error_cannot_convert_to_familiar_object(
      object = object,
      expected_class = "familiarEnsemble"
    )
  }
)


# as_familiar_data (generic) ---------------------------------------------------

#' @title Conversion to familiarData object.
#'
#' @description Creates `familiarData` a object from `familiarEnsemble` or
#'   `familiarModel` objects.
#'
#' @param object A `familiarData` object, or a `familiarEnsemble` or
#'   `familiarModel` objects that will be internally converted to a
#'   `familiarData` object. Paths to such objects can also be provided.
#'
#' @param name Name of the `familiarData` object. If not set, a name is
#'   automatically generated.
#'
#' @inheritDotParams .extract_data
#'
#' @details The `data` argument is required if `familiarEnsemble` or
#'   `familiarModel` objects are provided.
#'
#' @return A `familiarData` object.
#' @exportMethod as_familiar_data
#' @md
#' @rdname as_familiar_data-methods
setGeneric("as_familiar_data", function(object, ...) standardGeneric("as_familiar_data"))



## as_familiar_data (data) -----------------------------------------------------

#' @rdname as_familiar_data-methods
setMethod(
  "as_familiar_data",
  signature(object = "familiarData"),
  function(object, ...) {
    return(object)
  }
)



## as_familiar_data (ensemble) -------------------------------------------------

#' @rdname as_familiar_data-methods
setMethod(
  "as_familiar_data",
  signature(object = "familiarEnsemble"),
  function(object, name = NULL, ...) {
    # Familiar data
    fam_data <- do.call(
      extract_data,
      args = c(
        list("object" = object),
        list(...)
      )
    )
    
    # Set a placeholder name or a user-provided name for the familiarData
    # object.
    fam_data <- set_object_name(x = fam_data, new = name)

    return(fam_data)
  }
)



## as_familiar_data (prediction table) -----------------------------------------

#' @rdname as_familiar_data-methods
setMethod(
  "as_familiar_data",
  signature(object = "familiarDataElementPredictionTable"),
  function(object, name = NULL, ...) {
    # Familiar data
    fam_data <- do.call(
      extract_data,
      args = c(
        list("object" = object),
        list(...)
      )
    )
    
    # Set a placeholder name or a user-provided name for the familiarData
    # object.
    fam_data <- set_object_name(x = fam_data, new = name)
    
    return(fam_data)
  }
)



## as_familiar_data (model) ----------------------------------------------------

#' @rdname as_familiar_data-methods
setMethod(
  "as_familiar_data",
  signature(object = "familiarModel"),
  function(object, ...) {
    # Push to the same method for lists. This creates a familiarEnsemble and
    # then allows for creation of a familiarData object.
    return(do.call(
      as_familiar_data,
      args = c(
        list("object" = list(object)),
        list(...)
      )
    ))
  }
)



## as_familiar_data (list) -----------------------------------------------------

#' @rdname as_familiar_data-methods
setMethod(
  "as_familiar_data",
  signature(object = "list"),
  function(object, ...) {
    # Load familiar objects. This does nothing if the list already contains only
    # familiar S4 objects, but will load any files from the path and will check
    # uniqueness of classes.
    object <- load_familiar_object(object = object)

    # Return the object if it contains a single familiarEnsemble.
    if (length(object) == 1L && all(sapply(object, is, "familiarData"))) {
      return(object[[1L]])
    }

    # Convert familiarModel(s) to familiarEnsemble.
    if (all(sapply(object, is, "familiarModel"))) {
      object <- list(as_familiar_ensemble(object = object))
    }

    # Check if a single familiarEnsemble has been supplied or generated.
    if (!all(sapply(object, is, "familiarEnsemble")) || length(object) > 1L) {
      ..error(paste0(
        "A familiarData object can only be constructed from a ",
        "single familiarEnsemble object."
      ))
      
    } else {
      object <- object[[1L]]
    }

    return(do.call(
      as_familiar_data,
      args = c(
        list("object" = object),
        list(...)
      )
    ))
  }
)



## as_familiar_data (character) ------------------------------------------------

#' @rdname as_familiar_data-methods
setMethod(
  "as_familiar_data",
  signature(object = "character"),
  function(object, ...) {
    # Pass to as_familiar_data method for lists to load objects there.
    return(do.call(
      as_familiar_data,
      args = c(
        list("object" = as.list(object)),
        list(...)
      )
    ))
  }
)



# as_familiar_data (general) ---------------------------------------------------

#' @rdname as_familiar_data-methods
setMethod(
  "as_familiar_data",
  signature(object = "ANY"),
  function(object, ...) {
    # There familiar ensembles can only be generated from one of the above
    # functions.
    ..error_cannot_convert_to_familiar_object(
      object = object,
      expected_class = "familiarData"
    )
  }
)



# as_familiar_collection (generic) ---------------------------------------------

#' @title Conversion to familiarCollection object.
#'
#' @description Creates a `familiarCollection` objects from `familiarData`,
#'   `familiarEnsemble` or `familiarModel` objects.
#'
#' @param object `familiarCollection` object, or one or more `familiarData`
#'   objects, that will be internally converted to a `familiarCollection`
#'   object. It is also possible to provide a `familiarEnsemble` or one or more
#'   `familiarModel` objects together with the data from which data is computed
#'   prior to export. Paths to such files can also be provided.
#' @param familiar_data_names Names of the dataset(s). Only used if the `object`
#'   parameter is one or more `familiarData` objects.
#' @param collection_name Name of the collection.
#'
#' @inheritDotParams .extract_data
#'
#' @details A `data` argument is expected if the `object` argument is a
#'   `familiarEnsemble` object or one or more `familiarModel` objects.
#'
#' @return A `familiarCollection` object.
#' @exportMethod as_familiar_collection
#' @md
#' @rdname as_familiar_collection-methods
setGeneric(
  "as_familiar_collection", 
  function(
    object,
    familiar_data_names = NULL,
    collection_name = NULL,
    ...
  ) {
    standardGeneric("as_familiar_collection")
  }
)



## as_familiar_collection (collection) -----------------------------------------

#' @rdname as_familiar_collection-methods
setMethod(
  "as_familiar_collection",
  signature(object = "familiarCollection"),
  function(object, ...) {
    return(object)
  }
)



## as_familiar_collection (data) -----------------------------------------------

#' @rdname as_familiar_collection-methods
setMethod(
  "as_familiar_collection",
  signature(object = "familiarData"),
  function(
    object,
    familiar_data_names = NULL,
    collection_name = NULL,
    ...
  ) {
    # Pass to as_familiar_collection for lists to load and process objects
    # there.
    return(do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = list(object),
          "familiar_data_names" = familiar_data_names,
          "collection_name" = collection_name
        ),
        list(...)
      )
    ))
  }
)



## as_familiar_collection (ensemble) -------------------------------------------

#' @rdname as_familiar_collection-methods
setMethod(
  "as_familiar_collection",
  signature(object = "familiarEnsemble"),
  function(
    object,
    familiar_data_names = NULL,
    collection_name = NULL,
    ...
  ) {
    # Pass to as_familiar_collection for lists to load and process objects
    # there.
    return(do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = list(object),
          "familiar_data_names" = familiar_data_names,
          "collection_name" = collection_name
        ),
        list(...)
      )
    ))
  }
)



## as_familiar_collection (model) ----------------------------------------------

#' @rdname as_familiar_collection-methods
setMethod(
  "as_familiar_collection",
  signature(object = "familiarModel"),
  function(
    object,
    familiar_data_names = NULL,
    collection_name = NULL,
    ...
  ) {
    # Pass to as_familiar_collection for lists to load and process objects
    # there.
    return(do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = list(object),
          "familiar_data_names" = familiar_data_names,
          "collection_name" = collection_name
        ),
        list(...)
      )
    ))
  }
)


## as_familiar_collection (prediction table) -----------------------------------

#' @rdname as_familiar_collection-methods
setMethod(
  "as_familiar_collection",
  signature(object = "familiarDataElementPredictionTable"),
  function(
    object,
    familiar_data_names = NULL,
    collection_name = NULL,
    ...
  ) {
    # Pass to as_familiar_collection for lists to load and process objects
    # there.
    return(do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = list(object),
          "familiar_data_names" = familiar_data_names,
          "collection_name" = collection_name
        ),
        list(...)
      )
    ))
  }
)



## as_familiar_collection (list) -----------------------------------------------

#' @rdname as_familiar_collection-methods
setMethod(
  "as_familiar_collection",
  signature(object = "list"),
  function(
    object,
    familiar_data_names = NULL,
    collection_name = NULL,
    ...
  ) {
    # Load familiar objects. This does nothing if the list already contains only
    # familiar S4 objects, but will load any files from the path and will check
    # uniqueness of classes.
    object <- load_familiar_object(object = object)

    # Return the object if it contains a single familiarCollection.
    if (length(object) == 1L && all(sapply(object, is, class2 = "familiarCollection"))) {
      return(object[[1L]])
      
    } else if (all(sapply(object, is, class2 = "familiarCollection"))) {
      ..error("Only a single familiarCollection can be returned.")
    }

    # Convert to familiarModel(s) to familiarData
    if (all(sapply(object, is, class2 = "familiarModel"))) {
      object <- do.call(
        as_familiar_data,
        args = c(
          list("object" = object),
          list(...)
        )
      )
      
      # Store in list, if required
      if (!is(object, "list")) object <- list(object)
    }

    # Convert familiarEnsemble to familiarData
    if (all(sapply(object, is, class2 = "familiarEnsemble")) && length(object) == 1L) {
      object <- do.call(
        as_familiar_data,
        args = c(
          list("object" = object),
          list(...)
        )
      )
      
      # Store in list, if required.
      if (!is(object, "list")) object <- list(object)
      
    } else if (all(sapply(object, is, class2 = "familiarEnsemble"))) {
      ..error("A familiarData object can only be constructed from a single familiarEnsemble object.")
    }

    # Convert prediction table objects to familiarData.
    if (all(sapply(object, is, class2 = "familiarDataElementPredictionTable"))) {
      object <- do.call(
        as_familiar_data,
        args = c(
          list("object" = object),
          list(...)
        )
      )
      
      # Store in list, if required.
      if (!is(object, "list")) object <- list(object)
    }
    
    # Check if all objects at this moments are familiarData objects.
    if (!all(sapply(object, is, class2 = "familiarData"))) {
      stop("Only familiarData objects can be used to construct a familiarCollection object.")
    }

    # Obtain names of the familiarData objects.
    object_names <- sapply(object, function(fam_data_obj) (fam_data_obj@name))

    # Check if all the datasets are unique.
    if (anyDuplicated(object_names)) {
      ..error(paste0(
        "familiarCollections cannot contain identical familiarData sets. ",
        "The following duplicates were found: ",
        paste_s(unique(object_names[duplicated(object_names)]))
      ))
    }

    # Check if names for the data are externally provided, and obtain them from
    # the familiarData objects otherwise.
    if (is.null(familiar_data_names)) {
      familiar_data_names <- object_names
    }

    # Set data names as a factor.
    if (!is.factor(familiar_data_names)) {
      familiar_data_names <- factor(familiar_data_names, levels = unique(familiar_data_names))
    }

    # Check if the collection has a name
    if (is.null(collection_name)) {
      collection_name <- "collection"
    } else {
      collection_name <- as.character(collection_name)
    }

    # Generate data names
    fam_collect <- methods::new("familiarCollection",
      name = collection_name,
      data_sets = sapply(
        object,
        function(fam_data_obj) (fam_data_obj@name)
      ),
      outcome_type = object[[1L]]@outcome_type,
      outcome_info = .aggregate_outcome_info(x = lapply(
        object, 
        function(list_elem) (if (methods::.hasSlot(list_elem, "outcome_info")) return(list_elem@outcome_info))
      )),
      fs_vimp = collect(
        x = object,
        data_slot = "fs_vimp",
        identifiers = c("fs_method")
      ),
      model_vimp = collect(
        x = object, 
        data_slot = "model_vimp",
        identifiers = c("fs_method", "learner")
      ),
      permutation_vimp = collect(
        x = object,
        data_slot = "permutation_vimp"
      ),
      hyperparameters = collect(
        x = object,
        data_slot = "hyperparameters",
        identifiers = c("fs_method", "learner")
      ),
      hyperparameter_data = NULL,
      required_features = unique(unlist(lapply(
        object,
        function(fam_data_obj) (fam_data_obj@required_features)
      ))),
      model_features = unique(unlist(extract_from_slot(
        object_list = object,
        slot_name = "model_features",
        na.rm = TRUE
      ))),
      learner = unique(sapply(
        object, 
        function(fam_data_obj) (fam_data_obj@learner)
      )),
      fs_method = unique(sapply(
        object,
        function(fam_data_obj) (fam_data_obj@fs_method)
      )),
      prediction_data = collect(
        x = object,
        data_slot = "prediction_data"
      ),
      confusion_matrix = collect(
        x = object, 
        data_slot = "confusion_matrix"
      ),
      decision_curve_data = collect(
        x = object,
        data_slot = "decision_curve_data"
      ),
      calibration_info = collect(
        x = object,
        data_slot = "calibration_info",
        identifiers = c("fs_method", "learner")
      ),
      calibration_data = collect(
        x = object,
        data_slot = "calibration_data"
      ),
      model_performance = collect(
        x = object,
        data_slot = "model_performance"
      ),
      km_info = collect(
        x = object, 
        data_slot = "km_info",
        identifiers = c("fs_method", "learner")
      ),
      km_data = collect(
        x = object,
        data_slot = "km_data"
      ),
      auc_data = collect(
        x = object, 
        data_slot = "auc_data"
      ),
      univariate_analysis = collect(
        x = object,
        data_slot = "univariate_analysis"
      ),
      feature_expressions = collect(
        x = object,
        data_slot = "feature_expressions"
      ),
      feature_similarity = collect(
        x = object, 
        data_slot = "feature_similarity"
      ),
      sample_similarity = collect(
        x = object,
        data_slot = "sample_similarity"
      ),
      ice_data = collect(
        x = object,
        data_slot = "ice_data"
      ),
      project_id = object[[1L]]@project_id
    )

    # Add a package version to the familiarCollection object
    fam_collect <- add_package_version(object = fam_collect)

    # Create labels for the data names for correct ordering of plots etc.
    fam_collect <- set_data_set_names(
      x = fam_collect,
      new = as.character(familiar_data_names),
      order = levels(familiar_data_names)
    )

    return(fam_collect)
  }
)


## as_familiar_collection (character) ------------------------------------------

#' @rdname as_familiar_collection-methods
setMethod(
  "as_familiar_collection",
  signature(object = "character"),
  function(
    object,
    familiar_data_names = NULL,
    collection_name = NULL,
    ...
  ) {
    # Pass to as_familiar_collection for lists to load and process objects
    # there.
    return(do.call(
      as_familiar_collection,
      args = c(
        list(
          "object" = as.list(object),
          "familiar_data_names" = familiar_data_names,
          "collection_name" = collection_name
        ),
        list(...)
      )
    ))
  }
)


## as_familiar_collection (generic) --------------------------------------------

#' @rdname as_familiar_collection-methods
setMethod(
  "as_familiar_collection",
  signature(object = "ANY"),
  function(object, ...) {
    # There familiar ensembles can only be generated from objects defined in the
    # previous methods.
    ..error_cannot_convert_to_familiar_object(
      object = object, 
      expected_class = "familiarCollection"
    )
  }
)


# load_familiar_object (character) ---------------------------------------------
setMethod(
  "load_familiar_object", 
  signature(object = "character"),
  function(object) {
    # Determine if file(s) exist
    existing_files <- sapply(object, file.exists)
    if (!all(existing_files)) {
      ..error(paste0(
        "Not all files could be found: ",
        paste_s(object[!existing_files])
      ))
    }

    # Load object
    fam_object <- lapply(object, readRDS)

    # Check that all objects have the correct class.
    if (!(
      all(sapply(fam_object, is, class2 = "familiarModel")) ||
      all(sapply(fam_object, is, class2 = "familiarNoveltyDetector")) ||
      all(sapply(fam_object, is, class2 = "familiarEnsemble")) ||
      all(sapply(fam_object, is, class2 = "familiarData")) ||
      all(sapply(fam_object, is, class2 = "familiarCollection"))
    )) {
      ..error(paste0(
        "Could not load familiar objects because they are not uniquely ",
        "familiarModel, familiarNoveltyDetector, familiarEnsemble, familiarData or ",
        "familiarCollection objects."
      ))
    }

    # Update the objects for backward compatibility
    fam_object <- lapply(fam_object, update_object)

    # If all the object(s) are familiarEnsemble, check the model list.
    if (all(sapply(fam_object, is, class2 = "familiarEnsemble"))) {
      fam_object <- mapply(
        ..update_model_list, 
        object = fam_object, 
        dir_path = object
      )
    }

    # Unlist if the input is singular.
    if (length(object) == 1L) fam_object <- fam_object[[1L]]

    return(fam_object)
  }
)



# load_familiar_object (list) --------------------------------------------------
setMethod(
  "load_familiar_object",
  signature(object = "list"),
  function(object) {
    # Load all objects in the list.
    fam_object <- lapply(object, load_familiar_object)

    # Check that all objects have the correct class.
    if (!(
      all(sapply(fam_object, is, class2 = "familiarModel")) ||
      all(sapply(fam_object, is, class2 = "familiarNoveltyDetector")) ||
      all(sapply(fam_object, is, class2 = "familiarEnsemble")) ||
      all(sapply(fam_object, is, class2 = "familiarData")) ||
      all(sapply(fam_object, is, class2 = "familiarCollection"))
    )) {
      ..error(paste0(
        "Could not load familiar objects because they are not uniquely ",
        "familiarModel, familiarNoveltyDetector, familiarEnsemble, familiarData ",
        "or familiarCollection objects."
      ))
    }

    # Update the objects for backward compatibility
    fam_object <- lapply(fam_object, update_object)

    return(fam_object)
  }
)



# load_familiar_object (general) -----------------------------------------------
setMethod(
  "load_familiar_object",
  signature(object = "ANY"),
  function(object) {
    # Return the object if it is a familiar S4 class object that has already
    # been loaded. Else throw an error.

    if (is_any(object, class2 = c(
      "familiarModel", "familiarNoveltyDetector",
      "familiarEnsemble", "familiarData", "familiarCollection"
    ))) {
      # Make sure the S4 object is updated.
      object <- update_object(object = object)

      return(object)
      
    } else {
      ..error(paste0(
        "The loaded object is not a familiar S4 object. Found: ",
        paste_s(class(object))
      ))
    }
  }
)

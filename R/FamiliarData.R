#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


# show (familiarData) ----------------------------------------------------------
setMethod(
  "show",
  signature(object = "familiarData"),
  function(object) {
    
    # Make sure the data object is updated.
    object <- update_object(object = object)
    
    # Create an initial descriptor.
    data_str <- paste0(
      "A dataset (", object@name, "; ",
      .familiar_version_string(object), ")")
    
    # Add the generating ensemble, if available.
    if (length(object@generating_ensemble) > 0) {
      data_str <- paste0(
        data_str, " created using ",
        object@generating_ensemble, ".\n")
      
    } else {
      data_str <- paste0(data_str, ".\n")
    }
    cat(data_str)
    
    # Details concerning the generating ensemble.
    cat(paste0(
      "\nThe ensemble that created this dataset contained of one or more ",
      object@learner,
      " models with variable importance computed by the ",
      object@fs_method,
      " variable importance method.\n"))
    
    # Outcome details
    cat("\nThe following outcome was modelled:\n")
    show(object@outcome_info)
  }
)



# save (familiarData) ----------------------------------------------------------
setMethod(
  "save",
  signature(
    list = "familiarData",
    file = "character"),
  function(list, file) {
    .save(object = list, dir_path = file)
  }
)



# get_object_name (familiarData) -----------------------------------------------
setMethod(
  "get_object_name",
  signature(object = "familiarData"),
  function(object, abbreviated = FALSE) {
    
    # Extract data and run id
    ensemble_data_id <- tail(object@pooling_table, n = 1L)$ensemble_data_id
    ensemble_run_id <- tail(object@pooling_table, n = 1L)$ensemble_run_id
    pool_data_id <- tail(object@pooling_table, n = 1L)$pool_data_id
    pool_run_id  <- tail(object@pooling_table, n = 1L)$pool_run_id
    
    data_pooling <- ifelse(
      tail(object@pooling_table, n = 1L)$data_perturb_level == 
        tail(object@pooling_table, n = 1L)$pool_perturb_level,
      "ensemble",
      "pool")
    
    if (abbreviated) {
      # Create an abbreviated name
      object_name <- paste(
        data_pooling,
        ensemble_data_id,
        ensemble_run_id,
        ifelse(object@is_validation, "validation", "development"),
        "data",
        sep = ".")
      
    } else {
      # Create the full name of the object
      object_name <- get_object_file_name(
        learner = object@learner,
        fs_method = object@fs_method,
        project_id = object@project_id,
        data_id = ensemble_data_id,
        run_id = ensemble_run_id,
        pool_data_id = pool_data_id,
        pool_run_id = pool_run_id,
        object_type = "familiarData",
        is_ensemble = data_pooling == "ensemble",
        is_validation = object@is_validation,
        with_extension = FALSE)
    }
    
    return(object_name)
  }
)



# add_package_version (familiarData) -------------------------------------------
setMethod(
  "add_package_version",
  signature(object = "familiarData"),
  function(object) {
    
    # Set version of familiar
    return(.add_package_version(object = object))
  }
)



# add_identifiers --------------------------------------------------------------
setMethod(
  "add_identifiers",
  signature(
    data = "ANY",
    object = "familiarData"),
  function(
    data,
    object,
    more_identifiers = NULL) {
    # Adds identifying columns to a table
    
    if (is_empty(data)) return(NULL)
    
    if (!inherits(data, "data.table")) {
      stop("\"data\" should be a data.table.")
    }
    
    # Check which identifiers should be added
    if (is.null(more_identifiers)) {
      if (!all(more_identifiers %in% c("fs_method", "learner"))) {
        stop(paste0(
          "Only feature selection methods (\"fs_method\") and learners (\"learner\") ",
          "can be added as additional identifiers."))
      }
    }
    id_order <- c("data_set", more_identifiers)
    
    if (nrow(data) >= 1L) {
      
      # Get the name of the data
      data_set <- object@name
      
      # Insert "model_name" column
      data[, "data_set" := data_set]
      
      if (any(id_order == "fs_method")) data[, "fs_method" := object@fs_method]
      
      if (any(id_order == "learner")) data[, "learner" := object@learner]
      
      # Reorder columns and move model_name to the front
      data.table::setcolorder(
        data,
        neworder = id_order)
      
      return(data)
      
    } else {
      # In case the table is empty, return an empty table with the model name
      # attached.
      empty_table <- data.table::data.table("data_set" = character(0))
      
      if (any(id_order == "fs_method")) {
        empty_table[, "fs_method" := character(0)]
      }
      
      if (any(id_order == "learner")) {
        empty_table[, "learner" := character(0)]
      }
      
      return(cbind(empty_table, data))
    }
  }
)



# set_object_name (familiarData) -----------------------------------------------

#' @title Set the name of a `familiarData` object.
#'  
#' @description Set the `name` slot using the object name.
#'
#' @param x A `familiarData` object.
#' 
#' @return A `familiarData` object with a generated or a provided name.
#' @md
#' @keywords internal
setMethod(
  "set_object_name",
  signature(x = "familiarData"),
  function(x, new = NULL) {
    
    if (x@project_id == 0 && is.null(new)) {
      # Generate a random object name. A project_id of 0 means that the objects
      # was auto-generated (i.e. through object conversion). We randomly
      # generate chracters and add a time stamp, so that collision is
      # practically impossible.
      slot(object = x, name = "name") <- paste0(
        as.character(as.numeric(format(Sys.time(), "%H%M%S"))),
        "_", rstring(n = 20L))
      
    } else if (is.null(new)) {
      # Generate a sensible object name.
      slot(object = x, name = "name") <- get_object_name(object = x)
      
    } else {
      slot(object = x, name = "name") <- new
    }
    
    return(x)
  }
)

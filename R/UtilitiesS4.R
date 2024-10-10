#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R


##### is_empty-------------------------------------------------------------------
setMethod(
  "is_empty",
  signature(x = "NULL"),
  function(x, ...) {
    return(TRUE)
  }
)

setMethod(
  "is_empty",
  signature(x = "data.table"),
  function(x, ...) {
    return(ncol(x) == 0L || nrow(x) == 0L)
  }
)

setMethod(
  "is_empty",
  signature(x = "character"),
  function(x, ...) {
    if (length(x) == 0L) {
      return(TRUE)
    } else if (all(x == "")) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
)

setMethod(
  "is_empty",
  signature(x = "dataObject"),
  function(x, allow_no_features = FALSE, ...) {
    if (x@delay_loading) {
      # Data is not empty until is has been properly loaded
      return(FALSE)
    } else if (is.null(x@data)) {
      # Data is empty if it is NULL
      return(TRUE)
    } else if (nrow(x@data) == 0L) {
      # Data is empty if the data table has no rows
      return(TRUE)
    } else if (!allow_no_features && !has_feature_data(x)) {
      # Data is empty if there are no features.
      return(TRUE)
    } else {
      # Data is present otherwise
      return(FALSE)
    }
  }
)

setMethod(
  "is_empty",
  signature(x = "list"),
  function(x, ...) {
    return(length(x) == 0L)
  }
)

setMethod(
  "is_empty", 
  signature(x = "vimpTable"),
  function(x, ...) {
    return(is_empty(x@vimp_table))
  }
)

setMethod(
  "is_empty",
  signature(x = "vector"),
  function(x, ...) {
    return(length(x) == 0L)
  }
)

setMethod(
  "is_empty",
  signature(x = "familiarDataElement"),
  function(x, ...) {
    return(is_empty(x@data))
  }
)



# get_outcome_class_levels------------------------------------------------------
setMethod(
  "get_outcome_class_levels",
  signature(x = "outcomeInfo"),
  function(x) {
    if (x@outcome_type %in% c("binomial", "multinomial")) {
      return(as.character(x@levels))
    } else {
      return(character(0L))
    }
  }
)

setMethod("get_outcome_class_levels", signature(x = "familiarModel"), function(x) {
  return(get_outcome_class_levels(x@outcome_info))
})

setMethod("get_outcome_class_levels", signature(x = "familiarEnsemble"), function(x) {
  return(get_outcome_class_levels(x@outcome_info))
})

setMethod("get_outcome_class_levels", signature(x = "familiarData"), function(x) {
  return(get_outcome_class_levels(x@outcome_info))
})

setMethod("get_outcome_class_levels", signature(x = "familiarCollection"), function(x) {
  return(get_outcome_class_levels(x@outcome_info))
})

setMethod("get_outcome_class_levels", signature(x = "data.table"), function(x, outcome_type) {
  return(.get_outcome_class_levels(data = x, outcome_type = outcome_type))
})

setMethod("get_outcome_class_levels", signature(x = "dataObject"), function(x) {
  return(.get_outcome_class_levels(data = x@data, outcome_type = x@outcome_type))
})

.get_outcome_class_levels <- function(data, outcome_type) {
  if (outcome_type %in% c("survival", "continuous", "competing_risk")) {
    return(character(0L))
    
  } else if (outcome_type %in% c("binomial", "multinomial")) {
    return(as.character(levels(data[[get_outcome_columns(x = outcome_type)]])))
    
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }
}



# get_class_probability_name----------------------------------------------------
setMethod(
  "get_class_probability_name",
  signature(x = "outcomeInfo"),
  function(x) {
    # Check outcome type
    if (x@outcome_type %in% c("binomial", "multinomial")) {
      # Find class levels
      class_levels <- get_outcome_class_levels(x = x)
      
      # Pass to the method for character strings.
      return(get_class_probability_name(x = class_levels))
    } else {
      return(NULL)
    }
  }
)

setMethod("get_class_probability_name", signature(x = "familiarModel"), function(x) {
  return(get_class_probability_name(x = x@outcome_info))
})

setMethod("get_class_probability_name", signature(x = "familiarEnsemble"), function(x) {
  return(get_class_probability_name(x = x@outcome_info))
})

setMethod("get_class_probability_name", signature(x = "familiarData"), function(x) {
  return(get_class_probability_name(x = x@outcome_info))
})

setMethod("get_class_probability_name", signature(x = "familiarCollection"), function(x) {
  return(get_class_probability_name(x = x@outcome_info))
})

setMethod("get_class_probability_name", signature(x = "ANY"), function(x) {
  # Backup for numeric and logical categories.
  return(get_class_probability_name(x = as.character(x)))
})

setMethod("get_class_probability_name", signature(x = "dataObject"), function(x) {
  outcome_info <- .get_outcome_info(x = x)

  return(get_class_probability_name(x = outcome_info))
})

setMethod(
  "get_class_probability_name",
  signature(x = "data.table"),
  function(x, outcome_type) {
    if (outcome_type %in% c("binomial", "multinomial")) {
      # Find class levels
      class_levels <- get_outcome_class_levels(x = x)
      
      # Pass to the method for character strings.
      return(get_class_probability_name(x = class_levels))
      
    } else {
      return(NULL)
    }
  }
)

setMethod(
  "get_class_probability_name", 
  signature(x = "character"),
  function(x) {
    # Create column names
    class_probability_columns <- .replace_illegal_column_name(
      column_name = paste0("predicted_class_probability_", x)
    )
    
    return(class_probability_columns)
  }
)



# get_outcome_name--------------------------------------------------------------
setMethod("get_outcome_name", signature(x = "familiarModel"), function(x) {
  return(get_outcome_name(x@outcome_info))
})

setMethod("get_outcome_name", signature(x = "familiarEnsemble"), function(x) {
  return(get_outcome_name(x@outcome_info))
})

setMethod("get_outcome_name", signature(x = "familiarData"), function(x) {
  return(get_outcome_name(x@outcome_info))
})

setMethod("get_outcome_name", signature(x = "familiarCollection"), function(x) {
  return(get_outcome_name(x@outcome_info))
})

setMethod("get_outcome_name", signature(x = "outcomeInfo"), function(x) {
  return(x@name)
})



# get_outcome_columns-----------------------------------------------------------
setMethod("get_outcome_columns", signature(x = "character"), function(x) {
  return(.get_outcome_columns(outcome_type = x))
})

setMethod("get_outcome_columns", signature(x = "dataObject"), function(x) {
  return(.get_outcome_columns(outcome_type = x@outcome_type))
})

setMethod("get_outcome_columns", signature(x = "outcomeInfo"), function(x) {
  return(.get_outcome_columns(outcome_type = x@outcome_type))
})

setMethod("get_outcome_columns", signature(x = "familiarModel"), function(x) {
  return(.get_outcome_columns(outcome_type = x@outcome_type))
})

setMethod("get_outcome_columns", signature(x = "familiarEnsemble"), function(x) {
  return(.get_outcome_columns(outcome_type = x@outcome_type))
})

# Internal function
.get_outcome_columns <- function(outcome_type) {
  # Set outcome columns
  if (outcome_type %in% c("survival")) {
    outcome_cols <- c("outcome_time", "outcome_event")
    
  } else if (outcome_type %in% c("binomial", "multinomial", "continuous")) {
    outcome_cols <- c("outcome")
    
  } else if (outcome_type == "unsupervised") {
    outcome_cols <- NULL
    
  } else if (outcome_type == "competing_risk") {
    ..error_outcome_type_not_implemented(outcome_type)
    
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }

  return(outcome_cols)
}



# get_non_feature_columns-------------------------------------------------------
setMethod("get_non_feature_columns", signature(x = "character"), function(x, id_depth = "repetition") {
  return(.get_non_feature_columns(outcome_type = x, id_depth = id_depth))
})

setMethod("get_non_feature_columns", signature(x = "outcomeInfo"), function(x, id_depth = "repetition") {
  return(.get_non_feature_columns(outcome_type = x@outcome_type, id_depth = id_depth))
})

setMethod("get_non_feature_columns", signature(x = "dataObject"), function(x, id_depth = "repetition") {
  return(.get_non_feature_columns(outcome_type = x@outcome_type, id_depth = id_depth))
})

setMethod("get_non_feature_columns", signature(x = "familiarVimpMethod"), function(x, id_depth = "repetition") {
  return(.get_non_feature_columns(outcome_type = x@outcome_type, id_depth = id_depth))
})

setMethod("get_non_feature_columns", signature(x = "familiarNoveltyDetector"), function(x, id_depth = "repetition") {
  return(.get_non_feature_columns(outcome_type = "unsupervised", id_depth = id_depth))
})

setMethod("get_non_feature_columns", signature(x = "familiarModel"), function(x, id_depth = "repetition") {
  return(.get_non_feature_columns(outcome_type = x@outcome_type, id_depth = id_depth))
})

setMethod("get_non_feature_columns", signature(x = "familiarEnsemble"), function(x, id_depth = "repetition") {
  return(.get_non_feature_columns(outcome_type = x@outcome_type, id_depth = id_depth))
})

# Internal function
.get_non_feature_columns <- function(outcome_type, id_depth = "repetition") {
  # Returns column names in data table which are not features

  # Find outcome columns
  outcome_columns <- get_outcome_columns(x = outcome_type)

  # Find the id-columns
  id_columns <- get_id_columns(id_depth = id_depth)

  # Generate the names of the non-feature columns
  return(c(outcome_columns, id_columns))
}



# get_feature_columns-----------------------------------------------------------
setMethod(
  "get_feature_columns",
  signature(x = "data.table"),
  function(x, outcome_type) {
    return(.get_feature_columns(
      column_names = colnames(x),
      outcome_type = outcome_type
    ))
  }
)

setMethod(
  "get_feature_columns",
  signature(x = "dataObject"),
  function(x) {
    return(.get_feature_columns(
      column_names = colnames(x@data),
      outcome_type = x@outcome_type
    ))
  }
)

# Internal function
.get_feature_columns <- function(column_names, outcome_type) {
  if (is.null(column_names)) {
    # Return 0-length character in case column_names is NULL.
    return(character(0L))
    
  } else if (length(column_names) == 0L) {
    # Return 0-length character in case column_names has no length.
    return(character(0L))
    
  } else {
    # Return all column names that are not related to identifiers and outcome.
    return(setdiff(column_names, get_non_feature_columns(x = outcome_type)))
  }
}


# get_n_features----------------------------------------------------------------
setMethod("get_n_features", signature(x = "data.table"), function(x, outcome_type) {
  return(length(get_feature_columns(x = x, outcome_type = outcome_type)))
})

setMethod("get_n_features", signature(x = "dataObject"), function(x) {
  return(length(get_feature_columns(x = x)))
})


# has_feature_data--------------------------------------------------------------
setMethod("has_feature_data", signature(x = "data.table"), function(x, outcome_type) {
  return(get_n_features(x = x, outcome_type = outcome_type) > 0L)
})

setMethod("has_feature_data", signature(x = "dataObject"), function(x, outcome_type) {
  return(get_n_features(x = x) > 0L)
})


# get_unique_row_names----------------------------------------------------------
setMethod(
  "get_unique_row_names",
  signature(x = "data.table"),
  function(x, include_batch = NULL, include_series = NULL) {
    
    if (is_empty(x)) return(character(0L))
    
    if (is.null(include_batch)) {
      # Determine if batch identifiers should be included, i.e. the same sample
      # identifiers appear in different batches.
      include_batch <- any(
        unique(
          x[, mget(get_id_columns(id_depth = "sample"))]
        )[, list("n" = .N), by = mget(get_id_columns(single_column = "sample"))]$n > 1L
      )
    }
    
    if (is.null(include_series)) {
      # Determine if series identifiers should be included, i.e. any unique
      # sample has multiple series.
      include_series <- any(
        unique(
          x[, mget(get_id_columns(id_depth = "series"))]
        )[, list("n" = .N), by = mget(get_id_columns(id_depth = "sample"))]$n > 1L
      )
    }
    
    # Get sample names.
    row_names <- as.character(x[[get_id_columns(single_column = "sample")]])
    
    if (any(c(include_batch, include_series))) {
      add_string <- " ("
      
      if (include_batch) {
        add_string <- paste0(add_string, x[[get_id_columns(single_column = "batch")]])
      }
      if (include_batch && include_series) {
        add_string <- paste0(add_string, "; ")
      }
      if (include_series) {
        add_string <- paste0(add_string, x[[get_id_columns(single_column = "series")]])
      }
      
      row_names <- paste0(row_names, add_string, ")")
    }
    
    return(row_names)
  }
)

setMethod(
  "get_unique_row_names",
  signature(x = "dataObject"),
  function(x, include_batch = NULL, include_series = NULL) {
    return(get_unique_row_names(
      x = x@data,
      include_batch = include_batch,
      include_series = include_series
    ))
  }
)



# get_n_samples ----------------------------------------------------------------
setMethod("get_n_samples", signature(x = "data.table"), function(x, id_depth = "sample") {
  return(.get_n_samples(x = x, id_depth = id_depth))
})

setMethod("get_n_samples", signature(x = "dataObject"), function(x, id_depth = "sample") {
  return(.get_n_samples(x = x@data, id_depth = id_depth))
})

setMethod("get_n_samples", signature(x = "NULL"), function(x, id_depth = "sample") {
  return(0L)
})


.get_n_samples <- function(x, id_depth) {
  # Check if x is empty.
  if (is_empty(x)) {
    return(0L)
  }

  # Find identifier columns.
  id_columns <- get_id_columns(id_depth = id_depth)

  # Return the number of rows with unique values for the combination of
  # identifier columns.
  return(nrow(unique(x[, mget(id_columns)])))
}


# encode_categorical_variables--------------------------------------------------
setMethod(
  "encode_categorical_variables",
  signature(
    data = "dataObject",
    object = "ANY"
  ),
  function(
    object = NULL,
    data,
    encoding_method = "effect", 
    drop_levels = TRUE,
    feature_columns = NULL
  ) {
    # Obtain feature columns if not provided.
    if (is.null(feature_columns)) feature_columns <- get_feature_columns(data)
    
    # Find encoding data
    encoding_data <- encode_categorical_variables(
      data = data@data,
      object = object,
      encoding_method = encoding_method,
      drop_levels = drop_levels,
      feature_columns = feature_columns
    )
    
    # Update data
    data@data <- encoding_data$encoded_data
    
    return(list(
      "encoded_data" = data, 
      "reference_table" = encoding_data$reference_table
    ))
  }
)


setMethod(
  "encode_categorical_variables", 
  signature(
    data = "data.table",
    object = "ANY"
  ),
  function(
    object = NULL,
    data, 
    encoding_method = "effect",
    drop_levels = TRUE,
    feature_columns = NULL, 
    outcome_type = NULL
  ) {
    # Determine columns with factors
    if (is.null(feature_columns) && !is.null(outcome_type)) {
      feature_columns <- get_feature_columns(
        x = data,
        outcome_type = outcome_type
      )
      
    } else if (is.null(feature_columns)) {
      feature_columns <- colnames(data)
    }

    # Return original if no variables are available.
    if (length(feature_columns) == 0L) {
      return(list(
        "encoded_data" = data, 
        "reference_table" = NULL
      ))
    }

    # Find column classes
    column_class <- lapply(
      feature_columns, 
      function(ii, data) (class(data[[ii]])),
      data = data
    )

    # Identify categorical columns
    factor_columns <- sapply(
      column_class, 
      function(selected_column_class) {
        return(any(selected_column_class %in% c("logical", "character", "factor")))
      }
    )
    factor_columns <- feature_columns[factor_columns]

    # Return original if no categorical variables are available.
    if (length(factor_columns) == 0L) {
      return(list(
        "encoded_data" = data,
        "reference_table" = NULL
      ))
    }

    # Initialise contrast list and reference list
    contrast_list <- list()
    reference_list <- list()

    # Iterate over columns and encode the categorical variables.
    for (ii in seq_along(factor_columns)) {
      # Extract data from current column.
      current_col <- factor_columns[ii]
      x <- data[[current_col]]

      # Drop missing levels
      if (drop_levels) x <- droplevels(x)

      if (is.factor(x)) {
        # Get names of levels and number of levels
        level_names <- levels(x)
        level_count <- nlevels(x)
        
      } else if (is.character(x)) {
        level_names <- unique(x)
        level_count <- length(level_names)
        
      } else if (is.logical(x)) {
        level_names <- c(FALSE, TRUE)
        level_count <- 2L
      }

      # Apply coding scheme
      if (encoding_method == "effect" || level_count == 1L) {
        # Effect/one-hot encoding
        curr_contrast_list <- lapply(
          level_names,
          function(ii, x) (as.numeric(x == ii)),
          x = x
        )

        # Check level_names for inconsistencies (i.e. spaces etc)
        contrast_names <- .replace_illegal_column_name(
          column_name = paste0(current_col, "_", level_names)
        )
        names(curr_contrast_list) <- contrast_names
        
      } else if (encoding_method == "dummy") {
        # Dummy encoding. The first level is used as a reference (0).
        curr_contrast_list <- lapply(
          level_names[2L:level_count],
          function(ii, x) (as.numeric(x == ii)),
          x = x
        )

        # Check level_names for inconsistencies (i.e. spaces etc)
        contrast_names <- .replace_illegal_column_name(
          column_name = paste0(current_col, "_", level_names[2L:level_count])
        )
        names(curr_contrast_list) <- contrast_names
        
      } else if (encoding_method == "numeric") {
        # Numeric encoding
        browser()
        curr_contrast_list <- list(as.numeric(x))

        # Set names
        contrast_names <- current_col
        names(curr_contrast_list) <- contrast_names
        
      } else {
        ..error_reached_unreachable_code(
          "encode_categorical_variables: unknown encoding method encountered."
        )
      }

      # Add list of generated contrast to contrast list
      contrast_list <- c(contrast_list, curr_contrast_list)

      # Add data table with reference and contrast names to the reference list
      reference_list[[ii]] <- data.table::data.table(
        "original_name" = current_col,
        "reference_name" = contrast_names
      )
    }

    # Check if the original data set contained any features other than
    # categorical features. Combine original data and contrasts into data table
    # with added contrast data, and get a list with reference names.
    if (length(factor_columns) == ncol(data)) {
      contrast_table <- data.table::as.data.table(contrast_list)
      
    } else {
      contrast_table <- cbind(
        data[, !factor_columns, with = FALSE],
        data.table::as.data.table(contrast_list)
      )
    }

    # Return as list
    return(list(
      "encoded_data" = contrast_table,
      "reference_table" = data.table::rbindlist(reference_list)
    ))
  }
)



# get_bootstrap_sample----------------------------------------------------------
setMethod(
  "get_bootstrap_sample",
  signature(data = "dataObject"),
  function(data, seed = NULL, ...) {
    if (.as_preprocessing_level(data) > "none") {
      # Indicating that some preprocessing has taken please.

      # Bootstrap the data element.
      data@data <- get_bootstrap_sample(
        data = data@data,
        seed = seed
      )
      
    } else if (length(data@sample_set_on_load) > 0L) {
      if (data.table::is.data.table(data@sample_set_on_load)) {
        data@sample_set_on_load <- get_bootstrap_sample(
          data = data@sample_set_on_load,
          seed = seed
        )
        
      } else {
        # Reshuffle the samples -- This is for backward compatibility.
        data@sample_set_on_load <- fam_sample(
          x = unique(data@sample_set_on_load),
          size = length(data@sample_set_on_load),
          replace = TRUE,
          seed = seed
        )
      }
    } else {
      ..error_reached_unreachable_code(paste0(
        "get_boostrap_sample,dataObject: could not identify a suitable ",
        "method for bootstrapping."
      ))
    }

    return(data)
  }
)

setMethod(
  "get_bootstrap_sample", 
  signature(data = "data.table"),
  function(data, seed = NULL, ...) {
    # Find identifier columns at the sample level, i.e. excluding repetitions
    # and series.
    id_columns <- intersect(
      get_id_columns(id_depth = "sample"),
      colnames(data)
    )

    if (length(id_columns) == 0L) {
      # Sample rows.
      row_ids <- fam_sample(
        x = seq_len(nrow(data)),
        size = nrow(data),
        replace = TRUE,
        seed = seed
      )

      # Create a subset.
      data <- data[row_ids, ]
      
    } else {
      # List unique samples.
      id_table <- unique(data[, mget(id_columns)])

      # Sample the unique rows of the identifier table.
      row_ids <- fam_sample(
        x = seq_len(nrow(id_table)),
        size = nrow(id_table),
        replace = TRUE,
        seed = seed
      )

      # Create subsample.
      id_table <- id_table[row_ids, ]

      # Merge the subsampled identifier table with the data (removing
      # duplicates).
      data <- merge(
        x = id_table,
        y = unique(data),
        by = id_columns,
        all = FALSE,
        allow.cartesian = TRUE
      )
    }

    return(data)
  }
)

setMethod(
  "get_bootstrap_sample",
  signature(data = "NULL"), 
  function(data, seed = NULL, ...) {
    return(NULL)
  }
)



# get_subsample-----------------------------------------------------------------
setMethod(
  "get_subsample",
  signature(data = "dataObject"),
  function(
    data, 
    seed = NULL, 
    size = NULL, 
    outcome_type = NULL,
    ...
  ) {
    # This function randomly selects up to "size" samples from the data.
    # Set seed for reproducible results.
    if (!is.null(seed)) {
      rstream_object <- .start_random_number_stream(seed = seed)
    }

    if (is.null(outcome_type)) outcome_type <- data@outcome_type

    if (.as_preprocessing_level(data) > "none") {
      # Indicating that some preprocessing has taken please.

      # Bootstrap the data element.
      data@data <- get_subsample(
        data = data@data,
        size = size,
        outcome_type = outcome_type,
        rstream_object = rstream_object
      )
      
    } else if (length(data@sample_set_on_load) > 0L) {
      # Subsample data.
      data@sample_set_on_load <- get_subsample(
        data = data@sample_set_on_load,
        size = size,
        outcome_type = outcome_type,
        rstream_object = rstream_object
      )
      
    } else {
      ..error_reached_unreachable_code(paste0(
        "get_boostrap_sample,dataObject: could not identify a suitable ",
        "method for bootstrapping."
      ))
    }

    return(data)
  }
)


setMethod(
  "get_subsample",
  signature(data = "data.table"),
  function(
    data, 
    seed = NULL, 
    size = NULL,
    outcome_type = NULL,
    rstream_object = NULL,
    ...
  ) {
    # Suppress NOTES due to non-standard evaluation in data.table
    .NATURAL <- NULL

    # If no size is set,
    if (is.null(size)) return(data)

    # Update size, if required.
    if (size > nrow(data)) size <- nrow(data)

    # If size is equal to the number of data elements, return the entire data.
    if (size == nrow(data)) return(data)

    # Generate a random stream object.
    if (!is.null(seed) && is.null(rstream_object)) {
      rstream_object <- .start_random_number_stream(seed = seed)
    }

    # Find identifier columns at the sample level, i.e. excluding
    # repetitions and series.
    id_columns <- intersect(
      get_id_columns(id_depth = "sample"),
      colnames(data)
    )

    if (length(id_columns) == 0L) {
      # Sample rows.
      row_ids <- fam_sample(
        x = seq_len(nrow(data)),
        size = size,
        replace = FALSE,
        rstream_object = rstream_object
      )

      # Create a subset.
      data <- data[row_ids, ]
      
    } else if (is.null(outcome_type)) {
      # Sample rows.
      row_ids <- fam_sample(
        x = seq_len(nrow(data)),
        size = size,
        replace = FALSE,
        rstream_object = rstream_object
      )

      # Create a subset.
      data <- data[row_ids, ]
      
    } else {
      # Get subsample.
      id_table <- .create_subsample(data,
        size = size,
        n_iter = 1L,
        outcome_type = outcome_type,
        rstream_object = rstream_object
      )

      # Isolate identifier table.
      id_table <- id_table$train_list[[1L]]

      # Select data.
      data <- data[id_table, on = .NATURAL]
    }

    return(data)
  }
)


setMethod(
  "get_subsample",
  signature(data = "NULL"), 
  function(data, seed = NULL, ...) {
    return(NULL)
  }
)



# fam_sample--------------------------------------------------------------------
setMethod(
  "fam_sample", signature(x = "ANY"),
  function(
    x,
    size = NULL,
    replace = FALSE, 
    prob = NULL,
    seed = NULL, 
    rstream_object = NULL,
    ...
  ) {
    # This function prevents the documented behaviour of the sample function,
    # where if x is positive, numeric and only has one element, it interprets x
    # as a series of x, i.e. x=seq_len(x). That's bad news if x is a sample
    # identifier.

    # Set size if it is unset.
    if (is.null(size)) size <- length(x)

    if (length(x) == 1L) {
      # Check that size is not greater than 1, if items are to be drawn
      # without replacement.
      if (!replace & size > 1L) {
        ..error("cannot take a sample larger than the population when 'replace = FALSE'")
      }

      return(rep_len(x = x, length.out = size))
      
    } else {
      # If x is a vector, array or list with multiple elements, then all
      # of the above is not an issue, and we can make use of sample.

      if (is.null(seed) & is.null(rstream_object)) {
        return(sample(
          x = x,
          size = size,
          replace = replace,
          prob = prob
        ))
        
      } else {
        return(..fam_sample(
          x = x,
          size = size,
          replace = replace,
          seed = seed,
          rstream_object = rstream_object
        ))
      }
    }
  }
)

setMethod(
  "fam_sample",
  signature(x = "data.table"),
  function(
    x, 
    size = NULL,
    replace = FALSE, 
    prob = NULL,
    seed = NULL,
    rstream_object = NULL, 
    ...
  ) {
    # This function prevents the documented behaviour of the sample function,
    # where if x is positive, numeric and only has one element, it interprets x
    # as a series of x, i.e. x=seq_len(x). That's bad news if x is a sample
    # identifier.

    # Suppress NOTES due to non-standard evaluation in data.table
    ..prob <- NULL

    # Make a local copy of x to prevent changing by reference.
    x <- data.table::copy(x)

    # Get id columns
    id_columns <- get_id_columns("sample")

    # Check if prob equals NULL, and set to FALSE if it is.
    if (is.null(prob)) prob <- FALSE

    # Check that batch_id and sample_id columns are present.
    if (!all(id_columns %in% colnames(x))) {
      ..error_reached_unreachable_code(
        "fam_sample,data.table: batch_id or sample_id columns are not present in x."
      )
    }

    # Check that the prob column is present.
    if (is.logical(prob)) {
      if (prob && !"prob" %in% colnames(x)) {
        ..error_reached_unreachable_code(
          "fam_sample,data.table: prob column is not present in x."
        )
      }

      # Select unique samples.
      x <- unique(x, by = id_columns)

      # Set probability to 1.0 for all samples.
      if (!prob) x[, "prob" := 1.0]
      
    } else {
      # Assume that the prob argument is provided as a separate vector
      if (length(x) == nrow(x)) {
        # In this case, assume that prob can be inserted as a new column.

        # Assign prob.
        x[, "prob" := ..prob]

        # Select unique samples.
        x <- unique(x, by = id_columns)
        
      } else {
        ..error_reached_unreachable_code(
          "fam_sample,data.table: the length of prob does not equal the number of instances in x."
        )
      }
    }

    # Set size if it is unset.
    if (is.null(size)) size <- nrow(x)

    # Sample x.
    if (nrow(x) == 1L) {
      # Check that size is not greater than 1, if items are to be drawn without
      # replacement.
      if (!replace && size > 1L) {
        ..error("cannot take a sample larger than the population when 'replace = FALSE'")
      }

      # Get row-ids
      row_id <- rep_len(x = 1L, length.out = size)

      # Create output table y.
      y <- x[row_id, mget(id_columns)]
      
    } else {
      # If x has a multiple instances, then all of the above is not an issue,
      # and we can use the standard implementation of sample.

      # Use default prob=NULL if all probabilities in x are 1.0.
      if (all(x$prob == 1.0)) {
        prob <- NULL
      } else {
        prob <- x$prob
      }

      if (is.null(seed) && is.null(rstream_object)) {
        # Get sampled row identifiers.
        row_id <- sample(
          x = seq_len(nrow(x)),
          size = size,
          replace = replace,
          prob = prob
        )
        
      } else {
        row_id <- ..fam_sample(
          x = seq_len(nrow(x)),
          size = size,
          replace = replace,
          seed = seed,
          rstream_object = rstream_object
        )
      }

      # Create output table y.
      y <- x[row_id, mget(id_columns)]
    }

    return(y)
  }
)


# has_optimised_hyperparameters ------------------------------------------------
setMethod("has_optimised_hyperparameters", signature(object = "familiarModel"), function(object) {
  return(.has_optimised_hyperparameters(object = object))
})

setMethod("has_optimised_hyperparameters", signature(object = "familiarVimpMethod"), function(object) {
  return(.has_optimised_hyperparameters(object = object))
})

setMethod("has_optimised_hyperparameters", signature(object = "familiarNoveltyDetector"), function(object) {
  return(.has_optimised_hyperparameters(object = object))
})


.has_optimised_hyperparameters <- function(object) {
  # Check if the object has any default parameters.
  default_parameters <- get_default_hyperparameters(object)

  # If there are no default hyperparameters, return TRUE.
  if (length(default_parameters) == 0L) return(TRUE)

  # Check if any hyperparameters are present in the object.
  if (is_empty(object@hyperparameters)) return(FALSE)

  # Check if any hyperparameters need to be optimised.
  unoptimised_hyperparameter <- sapply(object@hyperparameters, function(x) {
    # If element x is not a list itself, return false.
    if (!rlang::is_bare_list(x)) return(FALSE)

    # If element x is a list, but does not have a "randomise" element return
    # false.
    if (is.null(x$randomise)) return(FALSE)

    # Else, the hyperparameter requires optimisation.
    return(TRUE)
  })

  # If any hyperparameter was unoptimised return false.
  if (any(unoptimised_hyperparameter)) return(FALSE)

  # If any hyperparameter is missing, return false.
  if (!all(names(default_parameters) %in% names(object@hyperparameters))) {
    return(FALSE)
  }

  # If none of the above cases apply, the hyperparameters can be assumed to be
  # optimised.
  return(TRUE)
}

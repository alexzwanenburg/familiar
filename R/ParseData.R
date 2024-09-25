.load_data <- function(
    data, 
    sample_id_column = NULL,
    batch_id_column = NULL, 
    series_id_column = NULL,
    ...
) {
  # Parse the input sample_id_column
  if (!is.null(sample_id_column)) {
    sample_id_column <- .replace_illegal_column_name(sample_id_column)
  }

  # Parse the input batch_id_column
  if (!is.null(batch_id_column)) {
    batch_id_column <- .replace_illegal_column_name(batch_id_column)
  }

  # Parse the input series_id_column
  if (!is.null(series_id_column)) {
    series_id_column <- .replace_illegal_column_name(series_id_column)
  }

  if (data.table::is.data.table(data)) {
    # Keep data as is.
    data <- data.table::copy(data)

    # Update column names using a fixed routine
    data.table::setnames(data, .replace_illegal_column_name(colnames(data)))
  } else if (is.data.frame(data)) {
    
    # Convert to data.table
    data <- data.table::as.data.table(data)

    # Update column names using a fixed routine
    data.table::setnames(data, .replace_illegal_column_name(colnames(data)))
  } else if (is.character(data) && length(data) == 1L) {
    
    # Read from path
    if (!file.exists(data)) {
      ..error(paste0("The requested data file does not exist: ", data))
    }

    # Load data based on file extension
    file_extension <- tolower(.file_extension(data))

    if (file_extension == "csv") {
      data <- .load_csv(data)
    } else if (file_extension == "rdata") {
      data <- .load_rdata(data)
    } else if (file_extension == "rds") {
      data <- .load_rds(data)
    } else {
      ..error(paste(
        "File extension", file_extension, "was not recognised as a loadable data type. Please load the",
        "data manually."
      ))
    }

    # Update column names using a fixed routine
    data.table::setnames(data, .replace_illegal_column_name(colnames(data)))
    
  } else if ((is.atomic(data) && length(data) > 1L) || (is.list(data))) {
    
    # Read data to a list by calling the function recursively
    data_list <- lapply(data, .load_data)

    if (length(data_list) > 1L) {
      new_data_list <- list()
      joined_id <- integer()

      # Attempt to bind rows
      for (ii in seq_len(length(data_list))) {
        # Check if the current data list entry was already joined previously
        if (ii %in% joined_id) next

        # Get current data
        current_data <- data_list[[ii]]

        # Iterate over remaining datasets
        for (jj in seq(ii + 1L, length(data_list))) {
          # Check that index jj does not access non-existing data
          if (jj > length(data_list)) break

          # Find colnames in current data
          current_data_cols <- colnames(current_data)

          # Find column names of the new dataset
          new_data_cols <- colnames(data_list[[jj]])

          # Determine overlap in columns
          overlap_cols <- intersect(current_data_cols, new_data_cols)

          # Check if there is at least 90% overlap in column names
          if (length(overlap_cols) / min(c(length(current_data_cols), length(new_data_cols))) < 0.90) {
            next
          }

          # Check if types for each column are consistent
          matching_types <- sapply(
            overlap_cols,
            function(col, x, y) {
              if (is.logical(x[[col]]) && is.logical(y[[col]])) {
                return(TRUE)
              } else if (is.character(x[[col]]) && is.character(y[[col]])) {
                return(TRUE)
              } else if (is.factor(x[[col]]) && is.factor(y[[col]])) {
                return(TRUE)
              } else if (is.numeric(x[[col]]) && is.numeric(y[[col]])) {
                return(TRUE)
              } else {
                return(FALSE)
              }
            },
            x = current_data,
            y = data_list[[jj]]
          )
          
          # Throw an error if any class is not consistent.
          if (!all(matching_types)) {
            ..error(paste0(
              "Mismatching column classes were found between datasets ",
              ii, " and ", jj, ". Differences were found in columns:",
              paste_s(overlap_cols[!matching_types])
            ))
          }

          # Bind the datasets together
          current_data <- data.table::rbindlist(
            list(current_data, data_list[[jj]]),
            use.names = TRUE,
            fill = TRUE
          )

          # Mark the dataset as joined to prevent double usage.
          joined_id <- c(joined_id, jj)
        }

        # Append new data list with current_data
        new_data_list <- c(new_data_list, list(current_data))
      }

      # Replace data_list with new_data_list
      data_list <- new_data_list

      # Check if it makes sense to attempt to join datasets by index.
      if (length(data_list) > 1L) {
        # Get the left-hand dataset for merging
        data < data_list[[ii]]

        # Iterate over remaining datasets
        for (ii in seq(2L, length(data_list))) {
          if (is.null(sample_id_column)) {
            # Attempt cbind if both datasets have the same number of rows and
            # non-overlapping column names
            if (nrow(data) != nrow(data_list[[ii]])) {
              ..error(paste(
                "datasets could not be joined row-wise because the number of samples is different.",
                "Please provide a sample id column or merge the datasets yourself prior to input."
              ))
            }

            # Check for column overlap
            overlap_cols <- intersect(colnames(data), colnames(data_list[[ii]]))
            if (length(overlap_cols) > 0L) {
              ..error(paste(
                "datasets could not be joined row-wise as one or more columns with the same name",
                "appear in both the left and right-hand datasets:", paste_s(overlap_cols)
              ))
            }

            # Throw a warning, as row-wise binding is dangerous
            ..warning(paste(
              "datasets could be joined row-wise. Integrity of the data could not be ensured.",
              "Please ensure that each sample occupies same row for consistency. Alternatively,",
              "provide a sample id column or merge the datasets yourself prior to input."
            ))

            # Combine row-wise
            data <- cbind(data, data_list[[ii]])
            
          } else if (
            !is.null(sample_id_column) &&
            !is.null(batch_id_column) && 
            is.null(series_id_column)
          ) {
            # Attempt full join on sample_id_column and batch_id_column,
            # provided that other column names are not overlapping, and all
            # sample_ids for a batch are unique.

            # Check if the sample identifier columns is presents in the data
            if (
              !sample_id_column %in% colnames(data) ||
              !sample_id_column %in% colnames(data_list[[ii]])
            ) {
              ..error(paste(
                "The specified column of sample identifiers", sample_id_column,
                "was not found in one or more of the datasets."
              ))
            }

            # Check if the batch identifier column is present in the data
            if (!batch_id_column %in% colnames(data) ||
              !batch_id_column %in% colnames(data_list[[ii]])) {
              ..error(paste(
                "The specified column of batch identifiers", batch_id_column,
                "was not found in one or more of the datasets."
              ))
            }

            # Check uniqueness of identifiers
            if (
              anyDuplicated(data[, mget(sample_id_column, batch_id_column)]) > 0L ||
              anyDuplicated(data_list[[ii]][, mget(sample_id_column, batch_id_column)]) > 0L
            ) {
              ..error(paste(
                "Sample identifiers were not uniquely specified within each batch.",
                "In case this is intentional, i.e. for repeated measurements, please merge the",
                "datasets yourself prior to input."
              ))
            }

            # Check for column overlap
            overlap_cols <- setdiff(
              intersect(colnames(data), colnames(data_list[[ii]])),
              c(sample_id_column, batch_id_column)
            )

            if (length(overlap_cols) > 0L) {
              ..error(paste(
                "datasets could not be merged by sample and batch identifiers",
                "as one or more columns with the same name appear in both the left and",
                "right-hand datasets:", paste_s(overlap_cols)
              ))
            }

            # Perform full join
            data <- merge(
              x = data,
              y = data_list[[ii]],
              on = c(sample_id_column, batch_id_column),
              all = TRUE
            )
            
          } else if (
            !is.null(sample_id_column) &&
            !is.null(batch_id_column) &&
            !is.null(series_id_column)
          ) {
            # Attempt full join on sample_id_column, batch_id_column, and
            # series_id_column provided that other column names are not
            # overlapping, and all sample_ids and series_ids for a batch are
            # unique.

            # Check if the sample identifier columns is presents in the data.
            if (
              !sample_id_column %in% colnames(data) ||
              !sample_id_column %in% colnames(data_list[[ii]])
            ) {
              ..error(paste(
                "The specified column of sample identifiers", sample_id_column,
                "was not found in one or more of the datasets."
              ))
            }

            # Check if the batch identifier column is present in the data.
            if (
              !batch_id_column %in% colnames(data) ||
              !batch_id_column %in% colnames(data_list[[ii]])
            ) {
              ..error(paste(
                "The specified column of batch identifiers", batch_id_column,
                "was not found in one or more of the datasets."
              ))
            }

            # Check if the series identifier column is present in the data.
            if (
              !series_id_column %in% colnames(data) ||
              !series_id_column %in% colnames(data_list[[ii]])
            ) {
              ..error(paste(
                "The specified column of series identifiers", series_id_column,
                "was not found in one or more of the datasets."
              ))
            }

            # Check uniqueness of identifiers
            if (
              anyDuplicated(data[, mget(sample_id_column, batch_id_column, series_id_column)]) > 0L ||
              anyDuplicated(data_list[[ii]][, mget(sample_id_column, batch_id_column, series_id_column)]) > 0L
            ) {
              ..error(paste(
                "Sample identifiers were not uniquely specified within each batch.",
                "In case this is intentional, i.e. for repeated measurements, please merge the",
                "datasets yourself prior to input."
              ))
            }

            # Check for column overlap
            overlap_cols <- setdiff(
              intersect(colnames(data), colnames(data_list[[ii]])),
              c(sample_id_column, batch_id_column, series_id_column)
            )

            if (length(overlap_cols) > 0L) {
              ..error(paste(
                "datasets could not be merged by sample, batch and series identifiers",
                "as one or more columns with the same name appear in both the left and",
                "right-hand datasets:", paste_s(overlap_cols)
              ))
            }

            # Perform full join
            data <- merge(
              x = data,
              y = data_list[[ii]],
              on = c(sample_id_column, batch_id_column, series_id_column),
              all = TRUE
            )
            
          } else {
            # Attempt full join on sample_id_column, provided that other column
            # names do not overlap, and all sample ids are unique.

            # Check if the sample identifier columns is presents in the data
            if (
              !sample_id_column %in% colnames(data) ||
              !sample_id_column %in% colnames(data_list[[ii]])
            ) {
              ..error(paste(
                "The specified column of sample identifiers", sample_id_column,
                "was not found in one or more of the datasets."
              ))
            }

            # Determine if all sample identifiers are unique
            if (
              anyDuplicated(data[, mget(sample_id_column)]) > 0L ||
              anyDuplicated(data_list[[ii]][, mget(sample_id_column)]) > 0L
            ) {
              ..error(paste0(
                "Sample identifiers were not uniquely specified. In case this is intentional, ",
                "i.e. for repeated measurements, please merge the datasets yourself prior to input."
              ))
            }

            # Check for column overlap.
            overlap_cols <- setdiff(
              intersect(colnames(data), colnames(data_list[[ii]])),
              sample_id_column
            )

            if (length(overlap_cols) > 0L) {
              ..error(paste0(
                "Datasets could not be merged by sample identifiers as one or more ",
                "columns with the same name appear in both the left and ",
                "right-hand datasets: ", paste_s(overlap_cols)
              ))
            }

            # Perform full join.
            data <- merge(
              x = data,
              y = data_list[[ii]],
              on = sample_id_column,
              all = TRUE
            )
          }
        }
        
      } else {
        data <- data_list[[1L]]
      }
      
    } else {
      data <- data_list[[1L]]
    }
  } else {
    ..error(paste0(
      "Data is expected to be a data.table, data.frame, ",
      "a path towards a file or a vector or list of the above."
    ))
  }

  if (is_empty(data)) ..error_data_set_is_empty()

  return(data)
}



.load_rdata <- function(file_path) {
  # Loads an RData file, and returns its contents
  load(file_path)

  # Find data in the local environment
  data <- get(ls()[ls() != "file_path"])

  # Cast to data.table
  return(data.table::as.data.table(data))
}



.load_csv <- function(file_path) {
  return(data.table::fread(file = file_path))
}



.load_rds <- function(file_path) {
  return(data.table::as.data.table(readRDS(file_path)))
}



#' Internal function for finalising generic data processing
#'
#' @param data data.table with feature data
#' @inheritParams .parse_experiment_settings
#' @inheritParams as_data_object
#'
#' @details This function is used to update data.table provided by loading the
#'   data. When part of the main familiar workflow, this function is used after
#'   .parse_initial_settings --> .load_data --> .update_initial_settings.
#'
#'   When used to parse external data (e.g. in conjunction with familiarModel)
#'   it follows after .load_data. Hence the function contains several checks
#'   which are otherwise part of .update_initial_settings.
#'
#' @return data.table with expected column names.
#'
#' @md
#' @keywords internal
.finish_data_preparation <- function(
    data,
    sample_id_column,
    batch_id_column,
    series_id_column,
    outcome_column,
    outcome_type,
    include_features,
    class_levels,
    censoring_indicator,
    event_indicator,
    competing_risk_indicator,
    check_stringency = "strict",
    reference_method = "auto"
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  sample_id <- batch_id <- n <- NULL
  
  # Check if the input data has any samples
  if (is_empty(data)) ..error_data_set_is_empty()

  # Set sample identifier column
  if (!is.null(sample_id_column)) {
    # Check input -- note this may be double, but this function may be called
    # when parsing external data as well, e.g. as argument to a predict method
    # that is called using external data.
    .check_input_identifier_column(
      id_column = sample_id_column,
      data = data,
      include_features = include_features,
      col_type = "sample"
    )

    # Rename column
    data.table::setnames(
      x = data,
      old = sample_id_column,
      new = "sample_id"
    )
    
  } else {
    # Create new column with sample ids.
    data[, "sample_id" := .I]
  }

  # Set batch identifier column
  if (!is.null(batch_id_column)) {
    # Check input
    .check_input_identifier_column(
      id_column = batch_id_column,
      data = data,
      include_features = include_features,
      col_type = "batch"
    )

    # Rename column
    data.table::setnames(
      x = data,
      old = batch_id_column,
      new = "batch_id"
    )

    # Check data type of the batch_id column and change to character. Cohort
    # names are parsed as characters, not integers.
    if (!is.character(data$batch_id)) data$batch_id <- as.character(data$batch_id)
    
  } else {
    # Create a cohort id column with placeholder.
    data[, "batch_id" := "placeholder"]
  }

  # Set outcome column
  if (!is.null(outcome_column)) {
    # Check plausibility of outcome type given the data
    .check_outcome_type_plausibility(
      data = data,
      outcome_type = outcome_type,
      outcome_column = outcome_column,
      censoring_indicator = censoring_indicator,
      event_indicator = event_indicator,
      competing_risk_indicator = competing_risk_indicator,
      check_stringency = check_stringency
    )

    if (outcome_type %in% c("survival")) {
      # Add survival columns

      if (check_stringency == "strict") {
        # Identify survival status columns
        event_cols <- sapply(
          outcome_column, 
          .is_survival_status_col,
          data = data,
          censoring_indicator = censoring_indicator,
          event_indicator = event_indicator,
          competing_risk_indicator = competing_risk_indicator
        )

        # The plausibility already took care of consistency checking. This means
        # that there is one and only one event status column and the other
        # column contains survival time.
        time_column <- outcome_column[!event_cols]
        event_column <- outcome_column[event_cols]
        
      } else {
        # For other stringency levels, outcome columns are assumed to be
        # extracted from familiarModel or familiarEnsemble objects, which
        # already contain ordered values.
        time_column <- outcome_column[1L]
        event_column <- outcome_column[2L]
      }

      # Rename columns
      data.table::setnames(
        x = data,
        old = c(time_column, event_column),
        new = get_outcome_columns(x = outcome_type)
      )
      
    } else {
      # Rename outcome column
      data.table::setnames(
        x = data,
        old = outcome_column,
        new = get_outcome_columns(x = outcome_type)
      )
    }
    
  } else if (outcome_type %in% c("survival", "competing_risk")) {
    # Generate outcome columns with NA

    # Find outcome column names
    outcome_column <- get_outcome_columns(x = outcome_type)

    # Create new columns and set to NA
    for (current_outcome_col in outcome_column) {
      data[, (current_outcome_col) := NA]
    }
    
  } else if (outcome_type %in% c("binomial", "multinomial")) {
    # Find outcome column names
    outcome_column <- get_outcome_columns(x = outcome_type)

    # Generate outcome column with NA values
    data[, (outcome_column) := NA_character_]
    
  } else if (outcome_type %in% c("continuous")) {
    # Find outcome column names
    outcome_column <- get_outcome_columns(x = outcome_type)

    # Generate outcome column with NA values
    data[, (outcome_column) := NA_real_]
    
  } else if (outcome_type %in% c("unsupervised")) {
    # Outcome column is NULL, as unsupervised data do not have outcome.
    outcome_column <- NULL
    
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }

  # Set class levels
  if (outcome_type %in% c("binomial", "multinomial")) {
    if (!is.null(class_levels)) {
      # Perform checks on class levels
      .check_class_level_plausibility(
        data = data,
        outcome_type = outcome_type,
        outcome_column = "outcome",
        class_levels = class_levels,
        check_stringency = check_stringency
      )

      # Set class levels. This may involve reordering class levels in case the
      # outcome already was a factor.
      data$outcome <- factor(
        x = data$outcome,
        levels = class_levels,
        exclude = NA
      )
      
    } else if (!is.factor(data$outcome)) {
      # Convert to factors
      data$outcome <- factor(
        x = data$outcome,
        levels = unique(data$outcome),
        exclude = c(NA, "NA", "NAN", "na", "nan", "NaN")
      )
    }
  }

  # Check survival time for positivity.
  if (outcome_type %in% c("survival", "competing_risk")) {
    time_column <- get_outcome_columns(x = outcome_type)[1L]

    .check_survival_time_plausibility(
      data = data,
      outcome_column = time_column,
      outcome_type = outcome_type,
      check_stringency = check_stringency
    )

    # Convert outcome_event to 0s and 1s.
    replacement_outcome_event <- numeric(length(data$outcome_event))
    replacement_outcome_event[data$outcome_event %in% censoring_indicator] <- 0L
    replacement_outcome_event[data$outcome_event %in% event_indicator] <- 1L

    # Update competing risk indicators.
    if (length(competing_risk_indicator) > 0L) {
      for (ii in seq_along(competing_risk_indicator)) {
        replacement_outcome_event[data$outcome_event %in% competing_risk_indicator[ii]] <- ii + 1L
      }
    }

    # Update outcome event column.
    data$outcome_event <- replacement_outcome_event
  }

  # Find outcome columns.
  outcome_cols <- get_outcome_columns(x = outcome_type)

  # Set series identifier.
  if (!is.null(series_id_column)) {
    # Check input
    .check_input_identifier_column(
      id_column = series_id_column,
      data = data,
      include_features = include_features,
      col_type = "series"
    )

    # Rename column
    data.table::setnames(
      x = data,
      old = series_id_column,
      new = "series_id"
    )

    # Check data type of the series_id column and change to character. Series
    # identifiers are parsed as characters, not integers.
    if (!is.character(data$series_id)) data$series_id <- as.character(data$series_id)
    
  } else {
    # Assign series ID per unique outcome for each sample.
    temp_data <- unique(data[, mget(c("sample_id", "batch_id", outcome_cols))])
    temp_data[, "series_id" := seq_len(.N), by = c("sample_id", "batch_id")]

    # Merge the series_id column into the main dataset.
    data <- merge(
      x = data,
      y = temp_data,
      by = c("sample_id", "batch_id", outcome_cols)
    )
  }

  # Add repetition identifiers
  data[
    ,
    "repetition_id" := seq_len(.N),
    by = c("sample_id", "batch_id", "series_id", outcome_cols)
  ]

  # Check that there are all combinations of sample_id, batch_id and series_id
  # have the same outcome.
  single_outcome_samples <- unique(data, by = c("sample_id", "batch_id", "series_id", outcome_cols))
  single_outcome_samples <- single_outcome_samples[, list("n" = .N), by = c("sample_id", "batch_id", "series_id")]
  if (any(single_outcome_samples$n > 1L)) {
    single_outcome_samples <- single_outcome_samples[n > 1L]
    single_outcome_samples[, "descriptor" := paste0(sample_id, " (", batch_id, ")")]
    
    ..error(paste0(
      "One or more samples with the same identifier do not have the same outcome value: ",
      paste_s(single_outcome_samples$descriptor)
    ))
  }

  # Determine which columns to maintain
  if (!is.null(include_features)) {
    # Check presence of features in include_features in the data
    .check_feature_availability(
      data = data, 
      feature = include_features
    )

    # Select only features marked for inclusion, as well as identifier and outcome columns
    data <- data[, mget(c(get_non_feature_columns(x = outcome_type), include_features))]
  }

  # Check if the data actually contains any features at this point
  if (!has_feature_data(x = data, outcome_type = outcome_type)) {
    ..error_data_set_has_no_features()
  }

  # Convert data to categorical features
  data <- .parse_categorical_features(
    data = data,
    outcome_type = outcome_type,
    reference_method = reference_method
  )

  # Convert integer data to double. This prevents rare errors later e.g., 
  # when aggregating data by computing a median value (that is not guaranteed to
  # be an integer).
  data <- .parse_integer_features(
    data = data,
    outcome_type = outcome_type
  )
  
  return(data)
}



.check_data_plausibility <- function(
    data,
    settings
) {
  
  feature_cols <- get_feature_columns(data, outcome_type = settings$data$outcome_type)
  outcome_cols <- get_outcome_columns(settings$data$outcome_type)
  
  # Plausibility checks: duplicate rows.
  if (anyDuplicated(data[, mget(c(feature_cols, outcome_cols))]) > 0L) {
    ..warning(paste0(
      "Ignoring identifiers, one or more rows in the dataset may contain duplicated data. ",
      "This means the same combination of feature values and outcome appears multiple times."
    ))
  }
  
  # Plausibility checks: 
  
  # Plausibility checks: one-to-one predictors
  browser()
  # Convert to dataObject. We pretend that the data are preprocessed up to
  # clustering.
  data <- methods::new(
    "dataObject",
    data = data,
    preprocessing_level = "clustering",
    outcome_type = settings$data$outcome_type,
    outcome_info = create_outcome_info(settings = settings),
    data_column_info = create_data_column_info(settings = settings)
  )
  
  # Set up vimp object and promote to concordance.
  vimp_object <- promote_vimp_method(methods::new(
    "familiarVimpMethod",
    outcome_type = data@outcome_type,
    outcome_info = data@outcome_info,
    feature_info = .get_feature_info_data(
      data = data@data,
      file_paths = NULL,
      project_id = character(),
      outcome_type = settings$data$outcome_type
    )[["generic"]],
    vimp_method = "concordance"
  ))
  
  # Compute concordance.
  vimp_table <- suppressWarnings(get_vimp_table(.vimp(
    object = vimp_object,
    data = data
  )))
  browser()
  # Identify features with perfect concordance (1.0) or discordance (-1.0), and
  # warn.
  
  return(invisible(TRUE))
}



#' Internal function for converting integer features
#'
#' @param data data.table with feature data
#' @param outcome_type character, indicating the type of outcome
#'
#' @details This function parses columns containing integer feature data to
#'   features to double. This prevents, e.g., errors when the result of an
#'   operation on the feature data yields a non-integer (i.e. floating point)
#'   result.
#'
#' @return data.table with integer features converted to double.
#'
#' @md
#' @keywords internal
.parse_integer_features <- function(data, outcome_type) {
  # Replace columns types so that only numeric and categorical features remain
  
  # Check presence of feature columns
  if (!has_feature_data(x = data, outcome_type = outcome_type)) ..error_data_set_has_no_features()
  
  # Get feature columns
  feature_columns <- get_feature_columns(x = data, outcome_type = outcome_type)
  
  # Identify features that consist of integer values.
  integer_features <- sapply(
    feature_columns,
    function(feature, data) (is.integer(data[[feature]])),
    data = data
  )
  integer_features <- feature_columns[integer_features]
  
  # Do not update data if there are no columns with integer features.
  if (length(integer_features) == 0L) return(data)
  
  # Update to integer features to double.
  for (feature in integer_features) {
    data.table::set(data, j = feature, value = as.double(data[[feature]]))
  }
  
  return(data)
}



#' Internal function for setting categorical features
#'
#' @param data data.table with feature data
#' @param outcome_type character, indicating the type of outcome
#' @param reference_method character, indicating the type of method used to set
#'   the reference level.
#'
#' @details This function parses columns containing feature data to factors if
#'   the data contained therein have logical (TRUE, FALSE), character, or factor
#'   classes.  Unless passed as feature names with `reference`, numerical data,
#'   including integers, are not converted to factors.
#'
#' @return data.table with several features converted to factor.
#'
#' @md
#' @keywords internal
.parse_categorical_features <- function(
    data, 
    outcome_type,
    reference_method = "auto"
) {
  # Replace columns types so that only numeric and categorical features remain

  # Check presence of feature columns
  if (!has_feature_data(x = data, outcome_type = outcome_type)) ..error_data_set_has_no_features()

  # Get feature columns
  feature_columns <- get_feature_columns(x = data, outcome_type = outcome_type)

  # Find column classes
  column_class <- lapply(
    feature_columns,
    function(ii, data) (class(data[[ii]])),
    data = data
  )

  # Identify categorical columns
  categorical_columns <- sapply(
    column_class,
    function(selected_column_class) {
      any(selected_column_class %in% c("logical", "character", "factor"))
    }
  )
  categorical_columns <- feature_columns[categorical_columns]

  # Do not update data if there are no columns for categorical data
  if (length(categorical_columns) == 0L) return(data)

  # Generate a list of warnings.
  warning_list <- list()

  # Iterate over categorical columns
  for (ii in categorical_columns) {
    # The default option is to parse categorical features by setting the most
    # frequent level as reference.
    factor_fun <- ..parse_categorical_most_frequent

    # Check if the feature was externally set to be a factor.
    is_external <- is.factor(data[[ii]])

    if (reference_method == "auto") {
      # Under "auto" mode, the most frequent level is used for categorical
      # features that are not externally set, whereas features that are
      # externally set are not re-ordered.
      if (is_external) factor_fun <- ..parse_categorical_as_is
      
    } else if (reference_method == "never") {
      # Under "never" mode, the  categorical features that are not externally
      # set are simply ordered, whereas features that are externally set are not
      # re-ordered.
      factor_fun <- if (is_external) ..parse_categorical_as_is else ..parse_categorical_sorted
    }

    # Exclude any potential re-ordering of known ordinal features under all
    # conditions.
    if (is_external) {
      if (is.ordered(data[[ii]])) factor_fun <- ..parse_categorical_as_is
    }

    # Create a categorical variable for the current column.
    data.table::set(data, j = ii, value = factor_fun(data[[ii]]))
  }

  # Raise an error in case there are missing class levels for any feature.
  if (length(warning_list) > 0L) {
    ..error(paste(warning_list, collapse = "\n"))
  }

  return(data)
}



..parse_categorical_as_is <- function(x) {
  # Return as is.
  return(x)
}



..parse_categorical_sorted <- function(x) {
  # Find class levels.
  class_levels <- if (is.factor(x)) levels(x) else unique_na(x)

  # Sort class levels.
  class_levels <- sort(class_levels)

  return(factor(x = x, levels = class_levels))
}



..parse_categorical_most_frequent <- function(x) {
  # Suppress NOTES due to non-standard evaluation in data.table
  n <- NULL

  # Created sorted table
  class_level_data <- data.table::data.table("x" = x)
  class_level_data <- class_level_data[, list("n" = .N), by = "x"][order(-n, x)]

  # Keep only non-NA values.
  class_level_data <- class_level_data[!is.na(x)]

  # Convert to character.
  class_levels <- as.character(class_level_data$x)

  # Get the reference level.
  reference_level <- head(class_levels, n = 1L)

  # Order class levels.
  class_levels <- sort(class_levels)

  # Insert the reference level in the first position.
  class_levels <- setdiff(class_levels, reference_level)
  class_levels <- c(reference_level, class_levels)

  return(factor(x = x, levels = class_levels))
}



update_data_set <- function(data, object) {
  # Check if the classes of the input is correct.
  if (!data.table::is.data.table(data)) {
    ..error("update_data_set: data is not a data.table")
  }
  
  if (!(
    is(object, "familiarModel") ||
    is(object, "familiarEnsemble") ||
    is(object, "familiarNoveltyDetector")
  )) {
    ..error(paste0(
      "update_data_set: object is not a familiarModel, familiarNoveltyDetector ",
      "or a familiarEnsemble."
    ))
  }

  # Find the outcome type.
  if (is(object, "familiarNoveltyDetector")) {
    outcome_type <- "unsupervised"
  } else {
    outcome_type <- object@outcome_type
  }

  # Find the outcome column
  outcome_column <- get_outcome_columns(outcome_type)

  # Start warning list.
  warning_list <- NULL

  # Check outcome --------------------------------------------------------------

  # Checks for categorical / ordinal outcomes.
  if (outcome_type %in% c("binomial", "multinomial")) {
    if (is(object@outcome_info, "outcomeInfo")) {
      # Update the outcome column if the outcome data is ordinal.
      if (object@outcome_info@ordered && !is.ordered(data[[outcome_column]])) {
        data[[outcome_column]] <- ordered(
          x = data[[outcome_column]],
          levels = object@outcome_info@levels
        )
        
      } else if (!object@outcome_info@ordered && !is.factor(data[[outcome_column]])) {
        data[[outcome_column]] <- factor(
          x = data[[outcome_column]],
          levels = object@outcome_info@levels
        )
      }

      # Check that the data does not have extra levels.
      extra_levels <- setdiff(
        levels(data[[outcome_column]]),
        object@outcome_info@levels
      )

      if (length(extra_levels) > 0L) {
        warning_list <- c(
          warning_list,
          paste0(
            "The outcome column contains the following ",
            ifelse(length(extra_levels) > 1L, "levels", "level"),
            " that were not found in the original dataset: ",
            paste_s(extra_levels), "; original: ", paste_s(object@outcome_info@levels)
          )
        )
        
      } else {
        # Ensure that order is correct.
        data[[outcome_column]] <- factor(
          x = data[[outcome_column]],
          levels = object@outcome_info@levels,
          ordered = object@outcome_info@ordered
        )
      }
    }
  }

  # TODO: When we start supporting transformation and normalisation
  # parameters for outcome, process the data here.
  if (outcome_type %in% c("continuous")) {
    if (is(object@outcome_info, "outcomeInfo")) {
      if (
        !is.null(object@outcome_info@transformation_parameters) ||
        !is.null(object@outcome_info@normalisation_parameters)
      ) {
        browser()
      }
    }
  }

  # Check columns --------------------------------------------------------------

  # Get all column names.
  all_columns <- colnames(data)

  # Check that the non-feature columns are present.
  non_feature_columns <- get_non_feature_columns(outcome_type)
  missing_non_feature_columns <- setdiff(non_feature_columns, all_columns)

  if (length(missing_non_feature_columns) > 0L) {
    warning_list <- c(
      warning_list,
      paste0(
        "The following non-feature ",
        ifelse(length(missing_non_feature_columns) > 1L, "columns are", "column is"),
        " missing in the dataset: ",
        paste_s(missing_non_feature_columns)
      )
    )
  }

  # Remove non-feature columns from the check.
  all_columns <- setdiff(all_columns, non_feature_columns)

  # Check that the feature columns for required_features, and if not, for the
  # union of model_features and novelty_features are present.
  required_features <- object@required_features

  # Novelty detectors do not have separate novelty feature attributes.
  if (is(object, "familiarNoveltyDetector")) {
    model_and_novelty_features <- object@model_features
  } else {
    model_and_novelty_features <- union(object@model_features, object@novelty_features)
  }

  # Check presence of features.
  if (length(required_features) > 0L && length(model_and_novelty_features) > 0L) {
    if (all(required_features %in% all_columns)) {
      available_features <- required_features
      
    } else if (all(model_and_novelty_features %in% all_columns)) {
      available_features <- model_and_novelty_features
      
    } else {
      # At least one model / novelty feature is missing.
      missing_feature_columns <- setdiff(model_and_novelty_features, all_columns)
      
      warning_list <- c(
        warning_list,
        paste0(
          "The following feature ",
          ifelse(length(missing_feature_columns) > 1L, "columns are", "column is"),
          " missing in the dataset: ",
          paste_s(missing_feature_columns)
        )
      )

      # Select features that are available.
      available_features <- intersect(model_and_novelty_features, all_columns)

      if (length(available_features) == 0L) {
        warning_list <- c(
          warning_list,
          paste0(
            "No additional feature-specific details could be assessed because ",
            "none of the features appear in the dataset."
          )
        )
      }
    }
  } else {
    available_features <- NULL
  }

  # Check features -------------------------------------------------------------
  feature_info_list <- object@feature_info[available_features]

  # Iterate over features.
  for (feature in available_features) {
    # Select the feature info object for the current feature.
    feature_info <- feature_info_list[[feature]]

    if (feature_info@feature_type == "numeric") {
      # For numeric features determine whether the feature in the data is numeric.
      if (!is.numeric(data[[feature]])) {
        warning_list <- c(
          warning_list,
          paste0(
            "The ", feature, " column contain a numeric feature. Found: ",
            typeof(data[[feature]])
          )
        )
      }
      
    } else if (feature_info@feature_type == "factor") {
      # For categorical and ordinal features determine whether there are any
      # unknown levels in the data.
      if (is.factor(data[[feature]])) {
        levels_present <- levels(data[[feature]])
      } else {
        levels_present <- unique(data[[feature]])
      }

      # Check for extra levels. Note that fewer levels is fine.
      extra_levels <- setdiff(levels_present, feature_info@levels)

      if (length(extra_levels) > 0L) {
        warning_list <- c(
          warning_list,
          paste0(
            "The ", feature, " column contains the following ",
            ifelse(length(extra_levels) > 1L, "levels", "level"),
            " that were not found in the original dataset: ",
            paste_s(extra_levels), "; original: ", paste_s(feature_info@levels)
          )
        )
        
      } else {
        # Ensure that order of levels in the data is correct.
        data[[feature]] <- factor(data[[feature]],
          levels = feature_info@levels,
          ordered = feature_info@ordered
        )
      }
    } else {
      ..error_reached_unreachable_code(paste0(
        "update_data_set: unknown feature type encountered: ",
        feature_info@feature_type
      ))
    }
  }

  # Raise an error in case there was any error for any feature.
  if (length(warning_list) > 0L) ..error(paste(warning_list, collapse = "\n\n"))

  return(data)
}



.add_data_dummy_columns <- function(
    data,
    sample_id_column,
    batch_id_column,
    series_id_column,
    outcome_column) {
  # Add dummy sample identifier column if absent.
  if (!sample_id_column %in% colnames(data)) {
    data[, (sample_id_column) := .I]
  }

  # Add dummy batch identifier column if absent.
  if (!batch_id_column %in% colnames(data)) {
    data[, (batch_id_column) := "placeholder"]
  }

  # Add dummy series identifier column if absent.
  if (!series_id_column %in% colnames(data)) {
    data[, (series_id_column) := seq_len(.N), by = c(sample_id_column, batch_id_column)]
  }

  # Add dummy outcome columns, if absent.
  for (current_outcome_column in outcome_column) {
    if (!current_outcome_column %in% colnames(data)) {
      data[, (current_outcome_column) := NA]
    }
  }

  return(data)
}

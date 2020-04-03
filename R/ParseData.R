.load_data <- function(data, sample_id_column=NULL, batch_id_column=NULL, ...){

  # Parse the input sample_id_columns
  if(!is.null(sample_id_column)){
    sample_id_column <- check_column_name(sample_id_column)
  }
  
  # Parse the input batch_id_columns
  if(!is.null(batch_id_column)){
    batch_id_column <- check_column_name(batch_id_column)
  }
  
  if(data.table::is.data.table(data)){
    #####data.table---------------------------------------------
    # Keep data as is
    data <- data

    # Update column names using a fixed routine
    data.table::setnames(data, check_column_name(colnames(data)))
    
  } else if(is.data.frame(data)){
    #####data.frame---------------------------------------------
    # Convert to data.table
    data <- data.table::as.data.table(data)
    
    # Update column names using a fixed routine
    data.table::setnames(data, check_column_name(colnames(data)))
    
  } else if(is.character(data) & length(data) == 1) {
    #####character---------------------------------------------
    # Read from path
    if(!file.exists(data)){
      stop(paste0("The requested data file does not exist: ", data))
    }
    
    # Load data based on file extension
    file_extension <- tolower(tools::file_ext(data))
    
    if(file_extension == "csv"){
      data <- .load_csv(data)
    } else if(file_extension == "rdata"){
      data <- .load_rdata(data)
    } else if(file_extension == "rds"){
      data <- .load_rds(data)
    } else {
      stop(paste("File extension", file_extension, "was not recognised as a loadable data type. Please load the",
                 "data manually."))
    }
    
    # Update column names using a fixed routine
    data.table::setnames(data, check_column_name(colnames(data)))
    
  } else if((is.atomic(data) & length(data) > 1) | (is.list(data))){
    #####list or vector---------------------------------------------
    # Read data to a list by calling the function recursively
    data_list <- lapply(data, .load_data)
    
    if(length(data_list) > 1){
      
      new_data_list <- list()
      joined_id     <- integer(0)
      
      # Attempt to bind rows
      for(ii in seq_len(length(data_list))){
        
        # Check if the current data list entry was already joined previously
        if(ii %in% joined_id){ next() }
        
        # Get current data
        current_data <- data_list[[ii]]
        
        # Iterate over remaining data sets
        for(jj in seq(ii+1, length(data_list))){
          # Check that index jj does not access non-existing data
          if(jj > length(data_list)){ break() }
          
          # Find colnames in current data
          current_data_cols <- colnames(current_data)
          
          # Find column names of the new data set
          new_data_cols <- colnames(data_list[[jj]])
          
          # Determine overlap in columns
          overlap_cols <- intersect(current_data_cols, new_data_cols)
          
          # Check if there is at least 90% overlap in column names
          if(length(overlap_cols) / min(c(length(current_data_cols), length(new_data_cols))) < 0.90){
            next()
          }
          
          # Check if types for each column are consistent
          matching_types <- sapply(overlap_cols, function(col, x, y){
            if(is.logical(x[[col]]) & is.logical(y[[col]])){
              return(TRUE)
            } else if(is.character(x[[col]]) & is.character(y[[col]])){
              return(TRUE)
            } else if(is.factor(x[[col]]) & is.factor(y[[col]])){
              return(TRUE)
            } else if(is.numeric(x[[col]]) & is.numeric(y[[col]])){
              return(TRUE)
            } else {
              return(FALSE)
            }
            
          }, x=current_data, y=data_list[[jj]])
          
          # Throw an error if any class is not consistent.
          if(any(!matching_types)){
            stop(paste("Mismatching column classes were found between data sets", ii, "and",
            jj, ". Differences were found in columns:", paste0(overlap_cols[!matching_types], collapse=", ")))
          }
          
          # Bind the data sets together
          current_data <- data.table::rbindlist(list(current_data, data_list[[jj]]),
                                                use.names=TRUE, fill=TRUE)
          
          # Mark the dataset as joined to prevent double usage.
          joined_id <- append(joined_id, jj)
          
        }
        
        # Append new data list with current_data
        new_data_list <- append(new_data_list, list(current_data))
      }
      
      # Replace data_list with new_data_list
      data_list <- new_data_list
      
      # Check if it makes sense to attempt to join data sets by index.
      if(length(data_list) > 1){
        
        # Get the left-hand data set for merging
        data < data_list[[ii]]
        
        # Iterate over remaining data sets
        for(ii in seq(2, length(data_list))){
          
          if(is.null(sample_id_column)){
            # Attempt cbind if both data sets have the same number of rows and non-overlapping column names
            if(nrow(data) != nrow(data_list[[ii]])){
              stop(paste("Data sets could not be joined row-wise because the number of samples is different.",
                         "Please provide a sample id column or merge the data sets yourself prior to input."))
            }
            
            # Check for column overlap
            overlap_cols <- intersect(colnames(data), colnames(data_list[[ii]]))
            if(length(overlap_cols) > 0){
              stop(paste("Data sets could not be joined row-wise as one or more columns with the same name",
                         "appear in both the left and right-hand data sets:", paste0(overlap_cols, collapse=", ")))
            }
            
            # Throw a warning, as row-wise binding is dangerous
            warning(paste("Data sets could be joined row-wise. Integrity of the data could not be ensured.",
                          "Please ensure that each sample occupies same row for consistency. Alternatively,",
                          "provide a sample id column or merge the data sets yourself prior to input."))
            
            # Combine row-wise
            data <- cbind(data, data_list[[ii]])
            
          } else if(!is.null(sample_id_column) & !is.null(batch_id_column)){
            # Attempt full join on sample_id_column and batch_id_column, provided that
            # other column names are not overlapping, and all sample_ids for a cohort_id are unique.
            
            # Check if the sample identifier columns is presents in the data
            if(!sample_id_column %in% colnames(data) |
               !sample_id_column %in% colnames(data_list[[ii]])){
              stop(paste("The specified column of sample identifiers", sample_id_column,
                         "was not found in one or more of the data sets."))
            }
            
            # Check if the batch identifier column is present in the data
            if(!batch_id_column %in% colnames(data) |
               !batch_id_column %in% colnames(data_list[[ii]])){
              stop(paste("The specified column of batch identifiers", batch_id_column,
                         "was not found in one or more of the data sets."))
            }
            
            # Check uniqueness of identifiers
            if(anyDuplicated(data[, c(sample_id_column, batch_id_column), with=FALSE]) > 0 |
               anyDuplicated(data_list[[ii]][, c(sample_id_column, batch_id_column), with=FALSE]) > 0){
              stop(paste("Sample identifiers were not uniquely specified within each batch.",
                         "In case this is intentional, i.e. for repeated measurements, please merge the",
                         "data sets yourself prior to input."))
            }
            
            # Check for column overlap
            overlap_cols <- setdiff(intersect(colnames(data), colnames(data_list[[ii]])),
                                    c(sample_id_column, batch_id_column))
            
            if(length(overlap_cols) > 0){
              stop(paste("Data sets could not be merged by sample and batch identifiers",
                         "as one or more columns with the same name appear in both the left and",
                         "right-hand data sets:", paste0(overlap_cols, collapse=", ")))
            }
            
            # Perform full join
            data <- merge(x=data, y=data_list[[ii]], on=c(sample_id_column, batch_id_column), all=TRUE)
            
          } else {
            # Attempt full join on sample_id_column, provided that other column names do not overlap,
            # and all sample ids are unique.
            
            # Check if the sample identifier columns is presents in the data
            if(!sample_id_column %in% colnames(data) |
               !sample_id_column %in% colnames(data_list[[ii]])){
              stop(paste("The specified column of sample identifiers", sample_id_column,
                         "was not found in one or more of the data sets."))
            }
            
            # Determine if all sample identifiers are unique
            if(anyDuplicated(data[, sample_id_column, with=FALSE]) > 0 |
               anyDuplicated(data_list[[ii]][, sample_id_column, with=FALSE]) > 0){
              stop(paste("Sample identifiers were not uniquely specified.",
                         "In case this is intentional, i.e. for repeated measurements, please merge the",
                         "data sets yourself prior to input."))
            }
            
            # Check for column overlap
            overlap_cols <- setdiff(intersect(colnames(data), colnames(data_list[[ii]])),
                                    sample_id_column)
            
            if(length(overlap_cols) > 0){
              stop(paste("Data sets could not be merged by sample identifiers",
                         "as one or more columns with the same name appear in both the left and",
                         "right-hand data sets:", paste0(overlap_cols, collapse=", ")))
            }
            
            # Perform full join
            data <- merge(x=data, y=data_list[[ii]], on=sample_id_column, all=TRUE)
          }
          
        }
        
      } else {
        data <- data_list[[1]]
      }
      
    } else {
      data <- data_list[[1]]
      
    }
    
  } else {
    stop("Data is expected to be a data.table, data.frame, a path towards a file or a vector or list of the above.")
    
  }
  
  if(nrow(data) == 0){
    stop("An empty data set without samples was provided.")
  }
  
  return(data)
}



.load_rdata <- function(file_path){
  # Loads an RData file, and returns its contents
  load(file_path)
  
  # Find data in the local environment
  data <- get(ls()[ls() != "file_path"])
  
  # Cast to data.table
  return(data.table::as.data.table(data))
}


.load_csv <- function(file_path){
  return(data.table::fread(file=file_path))
}


.load_rds <- function(file_path){
  return(data.table::as.data.table(readRDS(file_path)))
}



#' Internal function for finalising generic data processing
#'
#' @param data data.table with feature data
#' @param reference list with class levels (`levels`) and ordering (`ordered`) per list entry. Each list entry
#'   should have the name of the corresponding feature. The intended use is that `featureInfo` objects
#'   are parsed to generate such a reference.
#' @inheritParams .parse_experiment_settings
#'
#' @details This function is used to update data.table provided by loading the data. When part
#' of the main familiar workflow, this function is used after .parse_initial_settings
#' --> .load_data --> .update_initial_settings.
#' 
#' When used to parse external data (e.g. in conjunction with familiarModel) it follows
#' after .load_data. Hence the function contains several checks which are otherwise part of
#' .update_initial_settings.
#'
#' @return data.table with expected column names.
#'
#' @md
#' @keywords internal
.finish_data_preparation <- function(data,
                                     sample_id_column,
                                     batch_id_column,
                                     outcome_column,
                                     outcome_type,
                                     include_features,
                                     class_levels,
                                     censoring_indicator,
                                     event_indicator,
                                     competing_risk_indicator,
                                     reference=NULL){

  # Suppress NOTES due to non-standard evaluation in data.table
  subject_id <- cohort_id <- NULL

  # Check if the input data has any samples
  if(is_empty(data)){ ..error_data_set_is_empty() }
  
  # Set sample identifier column
  if(!is.null(sample_id_column)){
    
    # Check input -- note this may be double, but this function may be called when parsing
    # external data as well, e.g. as argument to a predict method that is called using external
    # data.
    .check_input_identifier_column(id_column=sample_id_column,
                                   data=data,
                                   include_features=include_features,
                                   col_type="sample")
    
    # Rename column
    data.table::setnames(x=data, old=sample_id_column, new="subject_id")
    
  } else {
    # Create new column with sample ids.
    data[, "subject_id":=.I]
  }
  
  # Set batch identifier column
  if(!is.null(batch_id_column)){
    
    # Check input
    .check_input_identifier_column(id_column=batch_id_column,
                                   data=data,
                                   include_features=include_features,
                                   col_type="batch")
    
    # Rename column
    data.table::setnames(x=data, old=batch_id_column, new="cohort_id")
    
    # Check data type of the cohort_id column and change to character. Cohort names are parsed as characters, not integers.
    if(!is.character(data$cohort_id)) {
      data$cohort_id <- as.character(data$cohort_id)
    }
    
  } else {
    # Create a cohort id column with placeholder.
    data[, "cohort_id":="placeholder"]
  }
  
  # Set outcome column
  if(!is.null(outcome_column)){
    
    # Check plausibility of outcome type given the data
    .check_outcome_type_plausibility(data=data,
                                     outcome_type=outcome_type,
                                     outcome_column=outcome_column,
                                     censoring_indicator=censoring_indicator,
                                     event_indicator=event_indicator,
                                     competing_risk_indicator=competing_risk_indicator)
    
    if(outcome_type %in% c("survival")){
      # Add survival columns
      
      # Identify survival status columns
      event_cols <- sapply(outcome_column, .is_survival_status_col,
                           data=data,
                           censoring_indicator=censoring_indicator,
                           event_indicator=event_indicator,
                           competing_risk_indicator=competing_risk_indicator)
      
      # The plausibility already took care of consistency checking.
      # This means that there is one and only one event status column and the other
      # column contains survival time.
      time_column <- outcome_column[!event_cols]
      event_column <- outcome_column[event_cols]
      
      # Rename columns
      data.table::setnames(data, old=c(time_column, event_column),
                           new=get_outcome_columns(x=outcome_type))
      
    } else {
      # Rename outcome column
      data.table::setnames(data, old=outcome_column,
                           new=get_outcome_columns(x=outcome_type))
    }
    
  } else if(outcome_type == "survival"){
    # Generate outcome columns with NA
    
    # Find outcome column names
    outcome_column <- get_outcome_columns(x=outcome_type)
    
    # Create new columns and set to NA
    for(current_outcome_col in outcome_column){ data[, (current_outcome_col):=NA] }

  } else if(outcome_type %in% c("binomial", "multinomial", "count", "continuous")){
    
    # Find outcome column names
    outcome_column <- get_outcome_columns(x=outcome_type)
    
    # Generate outcome column with NA values
    data[, (outcome_column):=NA]
    
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }
  
  # Set class levels
  if(outcome_type %in% c("binomial", "multinomial")){
    if(!is.null(class_levels)){
      # Perform checks on class levels
      .check_class_level_plausibility(data=data, outcome_type=outcome_type, outcome_column="outcome", class_levels=class_levels)
      
      # Set class levels. This may involve reordering class levels in case the outcome already was a factor.
      data$outcome <- factor(data$outcome, levels=class_levels)
    
    } else if(!is.factor(data$outcome)){
      # Convert to factors
      data$outcome <- factor(data$outcome, levels=unique(data$outcome),
                             exclude=c(NA, "NA", "NAN", "na", "nan", "NaN"))
    
    }
  }
  
  # Add repetition identifiers
  outcome_cols <- get_outcome_columns(x=outcome_type)
  data[, "repetition_id":=seq_len(.N), by=list(subject_id, cohort_id, get(outcome_cols))]

  # Determine which columns to maintain
  if(!is.null(include_features)){
    # Check presence of features in include_features in the data
    .check_feature_availability(data=data, feature=include_features)
    
    # Select only features marked for inclusion, as well as identifier and outcome columns
    data <- data[, c(get_non_feature_columns(x=outcome_type), include_features), with=FALSE]
  }
  
  # Check if the data actually contains any features at this point
  if(!has_feature_data(x=data, outcome_type=outcome_type)){
    ..error_data_set_has_no_features()
  }
  
  # Convert data to categorical features
  data <- .parse_categorical_features(data=data, outcome_type=outcome_type, reference=reference)
  
  return(data)
}



#' Internal function for setting categorical features
#'
#' @param data data.table with feature data
#' @param outcome_type character, indicating the type of outcome
#' @param reference list with class levels (`levels`) and ordering (`ordered`) per list entry. Each list entry
#'   should have the name of the corresponding feature. The intended use is that `featureInfo` objects
#'   are parsed to generate such a reference.
#'
#' @details This function parses columns containing feature data to factors if the data contained therein have
#'   logical (TRUE, FALSE), character, or factor classes.  Unless passed as feature names with `reference`,
#'   numerical data, including integers, are not converted to factors.
#'
#' @return data.table with several features converted to factor.
#'
#' @md
#' @keywords internal
.parse_categorical_features <- function(data, outcome_type, reference=list()){
  # Replace columns types so that only numeric and categorical features remain

  # Check presence of feature columns
  if(!has_feature_data(x=data, outcome_type=outcome_type)){
    ..error_data_set_has_no_features()
  }

  # Get feature columns
  feature_columns <- get_feature_columns(x=data, outcome_type=outcome_type)
  
  # Find column classes
  column_class <- lapply(feature_columns, function(ii, data) (class(data[[ii]])), data=data)
  
  # Identify categorical columns
  categorical_columns <- sapply(column_class, function(selected_column_class) (any(selected_column_class %in% c("logical", "character", "factor"))))
  categorical_columns <- feature_columns[categorical_columns]
  
  # Add columns that both appear in the reference list and in data.
  categorical_columns <- union(categorical_columns, intersect(feature_columns, names(reference)))
  
  # Do not update data if there are no columns for categorical data
  if(length(categorical_columns) == 0){ return(data) }
  
  # Generate a list of warnings.
  warning_list <- list()

  # Iterate over categorical columns
  for(ii in categorical_columns){
    
    # Check if the data is a factor
    if(!is.factor(data[[ii]])){
      # Identify class levels in the current column
      class_levels <- sort(unique_na(data[[ii]]))
      is_ordered      <- FALSE
    } else {
      # Identify class levels from factor attributes.
      class_levels <- levels(data[[ii]])
      is_ordered      <- is.ordered(data[[ii]])
    }
    
    # Identify levels in the reference
    reference_levels <- reference[[ii]]$levels
     
    # Compare with reference
    if(!is.null(reference_levels)){
      browser()
      # Identify if there are any missing levels
      missing_levels <- setdiff(class_levels, reference_levels)
      
      # Generate an error if new class levels appear. However, we only generate this error at the end to
      # limit user frustration.
      if(length(missing_levels) > 0){
        warning_list <- append(warning_list, paste0("Missing class levels in feature", ii, ". Unmatched levels: ",
                                                    paste0(missing_levels, collapse=", "), ". Expected: ",
                                                    paste0(reference_levels, collapse=", "), "."))
        
        # Skip conversion for the current data to prevent errors.
        next()
      
      } else {
        # Use the reference levels for generating the factor, and let this data determine
        # ordering.
        class_levels <- reference_levels
        is_ordered   <- reference[[ii]]$ordered
      }
    }
    
    # Create a categorical variable for the current column.
    data.table::set(data, j=ii, value=factor(data[[ii]], levels=class_levels, ordered=is_ordered))
  }
  
  # Raise an error in case there are missing class levels for any feature.
  if(length(warning_list) > 0){
    stop(paste(warning_list, collapse="\n"))
  }
  
  return(data)
}

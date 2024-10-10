#' Parse experimental design
#'
#' @param message_indent Spacing inserted before messages.
#' @param verbose Sets verbosity.
#' @inheritParams .parse_experiment_settings
#'
#' @details This function converts the experimental_design string
#'
#' @return data.table with subsampler information at different levels of the
#' experimental design.
#'
#' @md
#' @keywords internal
extract_experimental_setup <- function(
    experimental_design,
    file_dir,
    message_indent = 0L,
    verbose = TRUE
) {
  
  if (.experimental_design_is_file(
    file_dir = file_dir,
    experimental_design = experimental_design
  )) {
    return(waiver())
  }
  
  # Remove all whitespace
  experimental_design <- gsub(
    pattern = " ",
    replacement = "",
    x = experimental_design
  )
  
  # Generate a section table
  section_table <- .get_experimental_design_section_table(
    experimental_design = experimental_design
  )
  
  # Identify the subsampler algorithms
  section_table <- .complete_experimental_design_section_table(
    section_table = section_table,
    experimental_design = experimental_design
  )
  
  # Check consistency of the table, e.g. feature selection should only appear
  # once, etc.
  .check_experimental_design_section_table(
    section_table = section_table
  )
  
  # Report experimental design to the user.
  .report_experimental_design(
    section_table = section_table,
    message_indent = message_indent,
    verbose = verbose
  )
  
  return(section_table)
}



.experimental_design_is_file <- function(
    file_dir,
    experimental_design
) {
  # Check if the experimental design argument is actually a path to a file.
  
  if (is.null(file_dir)) return(FALSE)
  
  # Check if the file exists at all.
  if (
    !file.exists(file.path(file_dir, experimental_design)) &&
    !file.exists(experimental_design)
  ) {
    return(FALSE)
  }
  
  # Check whether the file is an RDS file.
  return(tolower(.file_extension(basename(experimental_design))) == "rds")
}



.get_experimental_design_section_table <- function(experimental_design) {
  
  # Determine the number of experimental levels
  # First we locate the position of parentheses
  left_parenthesis <- gregexpr(
    pattern = "(",
    text = experimental_design,
    fixed = TRUE
  )[[1L]]
  right_parenthesis <- gregexpr(
    pattern = ")",
    text = experimental_design,
    fixed = TRUE
  )[[1L]]
  
  if (left_parenthesis[1L] == -1L) left_parenthesis <- integer(0L)
  if (right_parenthesis[1L] == -1L) right_parenthesis <- integer(0L)
  
  # Subsequently generate the corresponding experimental levels
  experiment_levels <- integer(nchar(experimental_design))
  for (ii in right_parenthesis) {
    experiment_levels[1L:ii] <- experiment_levels[1L:ii] + 1L
  }
  
  for (ii in left_parenthesis) {
    experiment_levels[1L:(ii - 1L)] <- experiment_levels[1L:(ii - 1L)] - 1L
  }
  
  # Generate setup sections
  sections <- rle(experiment_levels)
  sections$end <- cumsum(sections$lengths)
  sections$start <- sections$end - sections$lengths + 1L
  class(sections) <- NULL
  
  # Set up data table with experimental sections
  section_table <- data.table::as.data.table(sections)
  section_table[, "lengths" := NULL]
  data.table::setnames(
    x = section_table,
    old = c("values", "end", "start"),
    new = c("exp_level_id", "sect_end", "sect_start")
  )
  
  # Set up columns to be filled
  section_table[, ":="(
    "ref_data_id" = 0L,
    "main_data_id" = 0L,
    "feat_sel" = FALSE,
    "model_building" = FALSE,
    "external_validation" = FALSE,
    "perturb_method" = "none",
    "perturb_n_rep" = 0L,
    "perturb_n_folds" = 0L
  )]
  
  return(section_table)
}

.get_available_subsample_methods <- function() {
  return(c(
    "main",
    "limited_bootstrap",
    "full_bootstrap",
    "cross_val",
    "loocv",
    "imbalance_partition"
  ))
}


.complete_experimental_design_section_table <- function(
    section_table,
    experimental_design
) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  exp_level_id <- sect_start <- perturb_method <- NULL
  
  # Iterator
  main_data_id_iter <- 1L
  
  #Identify samplers------------------------------------------------------------
  # Iterate over sections to set main_data_id
  for (ii in seq_len(nrow(section_table))) {
    
    # Check if ip, bt, lv or cv preceeds the current section
    if (section_table$sect_start[ii] > 2L) {
      sampler_str <- substr(
        x = experimental_design,
        start = section_table$sect_start[ii] - 2L,
        stop = section_table$sect_start[ii] - 1L
      )
      
      # Check for imbalance partition (ip), limited bootstrap (bt), full
      # bootstrap (bs), cross-validation (cv) and leave-one-out-cross-validation
      # (lv)
      if (sampler_str == "bt") {
        section_table$perturb_method[ii] <- "limited_bootstrap"
        section_table$main_data_id[ii] <- main_data_id_iter
        main_data_id_iter <- main_data_id_iter + 1L
      
      } else if (sampler_str == "bs") {
        section_table$perturb_method[ii] <- "full_bootstrap"
        section_table$main_data_id[ii] <- main_data_id_iter
        main_data_id_iter <- main_data_id_iter + 1L
        
      } else if (sampler_str == "cv") {
        section_table$perturb_method[ii] <- "cross_val"
        section_table$main_data_id[ii] <- main_data_id_iter
        main_data_id_iter <- main_data_id_iter + 1L
        
      } else if (sampler_str == "lv") {
        section_table$perturb_method[ii] <- "loocv"
        section_table$main_data_id[ii] <- main_data_id_iter
        main_data_id_iter <- main_data_id_iter + 1L
        
      } else if (sampler_str == "ip") {
        section_table$perturb_method[ii] <- "imbalance_partition"
        section_table$main_data_id[ii] <- main_data_id_iter
        main_data_id_iter <- main_data_id_iter + 1L
      }
      
      rm(sampler_str)
    } else {
      section_table$perturb_method[ii] <- "main"
      section_table$main_data_id[ii] <- main_data_id_iter
      main_data_id_iter <- main_data_id_iter + 1L
    }
  }
  
  rm(main_data_id_iter)
  
  
  # Add main_data_id and ref_data_id -------------------------------------------
  # Iterate over sections to set main_data_id (where missing) and ref_data_id
  for (ii in seq_len(nrow(section_table))) {
    
    if (section_table$main_data_id[ii] == 0L) {
      # Make subselection of data at the same level and select only preceding sections
      dt_sub <- section_table[
        exp_level_id == section_table$exp_level_id[ii] &
          sect_start < section_table$sect_start[ii]
        ,
      ]
      
      # Set data id from nearest preceding lower level section
      section_table$main_data_id[ii] <- dt_sub$main_data_id[nrow(dt_sub)]
      
      rm(dt_sub)
    }
    
    # Set reference data id
    if (section_table$exp_level_id[ii] == 0L) {
      # At the lowest level (main) there is no reference
      section_table$ref_data_id[ii] <- 0L
      
    } else {
      # Make subselection of data one level higher and select only preceding sections
      dt_sub <- section_table[
        exp_level_id == section_table$exp_level_id[ii] - 1L &
          sect_start < section_table$sect_start[ii]
        ,
      ]
      
      # Set data id from nearest preceding lower level section
      section_table$ref_data_id[ii] <- dt_sub$main_data_id[nrow(dt_sub)]
      
      rm(dt_sub)
    }
  }
  
  # Complete details------------------------------------------------------------
  # Add details for perturbations and other sections
  for (ii in seq_len(nrow(section_table))) {
    
    if (section_table$perturb_method[ii] %in% .get_available_subsample_methods()) {
      
      # Create readable string for current data id
      curr_data_id_str <- NULL
      for (jj in which(section_table$main_data_id == section_table$main_data_id[ii])) {
        curr_data_id_str <- c(
          curr_data_id_str,
          substr(
            x = experimental_design,
            start = section_table$sect_start[jj],
            stop = section_table$sect_end[jj]
          )
        )
      }
      
      curr_data_id_str <- paste0(curr_data_id_str, collapse = "")
      
      # Drop parentheses and split string by comma
      curr_data_id_str <- gsub(
        pattern = "\\(|\\)",
        replacement = "",
        x = curr_data_id_str
      )
      curr_data_id_str <- strsplit(
        x = curr_data_id_str,
        split = ",",
        fixed = TRUE
      )[[1L]]

      # Check if feature selection is included in the current section
      if (grepl(pattern = "fs", x = curr_data_id_str[1L])) {
        section_table$feat_sel[ii] <- TRUE
      }
      
      # Check if model building is included in the current section
      if (grepl(pattern = "mb", x = curr_data_id_str[1L])) {
        section_table$model_building[ii] <- TRUE
      }
      
      # Check if external validation is included in the current section
      if (grepl(pattern = "ev", x = curr_data_id_str[1L])) {
        section_table$external_validation[ii] <- TRUE
      }
      
      # Read bootstrap data
      if (section_table$perturb_method[ii] %in% c("limited_bootstrap", "full_bootstrap")) {
        if (length(curr_data_id_str) < 2L) {
          ..error(
            paste0(
              "The number of bootstraps should be indicated when using the bt ",
              "(bootstrap) subsampler. None was found."
            ),
            error_class = "input_argument_error"
          )
        }
        
        # Determine the number of bootstraps
        n_reps <- .perform_type_conversion(
          x = curr_data_id_str[2L],
          to_type = "integer",
          var_name = "The number of bootstraps",
          req_length = 1L
        )
        
        # Check whether the number of bootstraps is at least 1
        .check_number_in_valid_range(
          x = n_reps,
          var_name = "The number of bootstraps",
          range = c(1L, Inf)
        )

        # Add the number of bootstraps to the section table
        section_table$perturb_n_rep[ii] <- n_reps
      }
      
      # Read cross-validation settings
      if (section_table$perturb_method[ii] == "cross_val") {
        if (length(curr_data_id_str) < 2L) {
          ..error(
            paste0(
              "The number of folds should be indicated when using the cv ",
              "(cross-validation) subsampler. None was found."
            ),
            error_class = "input_argument_error"
          )
        }
        
        # Determine the number of folds
        n_folds <- .perform_type_conversion(
          x = curr_data_id_str[2L],
          to_type = "integer",
          var_name = "The number of cross-validation folds",
          req_length = 1L
        )
        
        # Check whether the number of folds is at least 2
        .check_number_in_valid_range(
          x = n_folds,
          var_name = "The number of cross-validations folds",
          range = c(2L, Inf)
        )
        
        # Add number of folds to the section_table
        section_table$perturb_n_folds[ii] <- n_folds
        
        # Check the number of repetitions
        if (length(curr_data_id_str) >= 3L) {
          
          n_reps <- .perform_type_conversion(
            x = curr_data_id_str[3L],
            to_type = "integer",
            var_name = "The number of cross-validation repetitions",
            req_length = 1L
          )
          
          # Check whether the number of CV repetitions is at least 1
          .check_number_in_valid_range(
            x = n_folds,
            var_name = "The number of cross-validations repetitions",
            range = c(1L, Inf)
          )
          
          # Add number of repetitions to the section table
          section_table$perturb_n_rep[ii] <- n_reps
          
        } else {
          # Set the number of repetitions to one
          section_table$perturb_n_rep[ii] <- 1L
        }
      }
      
      # Read leave-one-out-cross-validation settings
      if (section_table$perturb_method[ii] == "loocv") {
        section_table$perturb_n_folds[ii] <- -1L
        section_table$perturb_n_rep[ii] <- 1L
      }
      
      rm(curr_data_id_str, jj)
    }
  }
  
  # Remove unnessary rows and columns
  section_table <- section_table[perturb_method != "none", ]
  section_table[, ":="(
    "exp_level_id" = NULL,
    "sect_end" = NULL,
    "sect_start" = NULL
  )]
  
  return(section_table)
}



.report_experimental_design <- function(
    section_table,
    message_indent = 0L,
    verbose = TRUE
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  feat_sel <- model_building <- main_data_id <- NULL
  
  # Report on validation data:
  if (any(section_table$external_validation)) {
    logger_message(
      "Setup report: Validation is external.",
      indent = message_indent,
      verbose = verbose
    )
    
  } else {
    logger_message(
      "Setup report: Validation is internal only.",
      indent = message_indent,
      verbose = verbose
    )
  }
  
  # Report on model building and feature selection
  if (any(section_table$feat_sel * section_table$model_building)) {
    main_message <- "Setup report: Feature selection and model building on"
    
    # Iteratively append message
    dt_sub <- section_table[feat_sel == TRUE & model_building == TRUE, ]
    curr_ref_data_id <- dt_sub$main_data_id[1L]
    while (curr_ref_data_id > 0L) {
      dt_sub <- section_table[main_data_id == curr_ref_data_id, ]
      
      if (dt_sub$perturb_method[1L] == "main") {
        main_message <- c(
          main_message,
          "the training data."
        )
        
      } else if (dt_sub$perturb_method[1L] %in% c("limited_bootstrap", "full_bootstrap")) {
        main_message <- c(
          main_message,
          paste0(dt_sub$perturb_n_rep[1L], " bootstraps of")
        )
        
      } else if (dt_sub$perturb_method[1L] == "cross_val") {
        main_message <- c(
          main_message,
          paste0(
            dt_sub$perturb_n_rep[1L], " repetitions of ",
            dt_sub$perturb_n_folds, "-fold cross validation of"
          )
        )
      
      } else if (dt_sub$perturb_method[1L] == "loocv") {
        main_message <- c(
          main_message,
          "folds of leave-one-out-cross-validation of"
        )
        
      } else if (dt_sub$perturb_method[1L] == "imbalance_partition") {
        main_message <- c(
          main_message,
          "class-balanced partitions of"
        )
      } 
      
      curr_ref_data_id <- dt_sub$ref_data_id[1L]
    }
    
    logger_message(
      paste0(main_message, collapse = " "),
      indent = message_indent,
      verbose = verbose
    )
    
  } else {
    # Feature selection first
    main_message <- "Setup report: Feature selection on"
    
    # Iteratively append message
    dt_sub <- section_table[feat_sel == TRUE, ]
    curr_ref_data_id <- dt_sub$main_data_id[1L]
    
    while (curr_ref_data_id > 0L) {
      
      dt_sub <- section_table[main_data_id == curr_ref_data_id, ]
      
      if (dt_sub$perturb_method[1L] == "main") {
        main_message <- c(
          main_message,
          "the training data."
        )
        
      } else if (dt_sub$perturb_method[1L] %in% c("limited_bootstrap", "full_bootstrap")) {
        main_message <- c(
          main_message,
          paste0(dt_sub$perturb_n_rep[1L], " bootstraps of")
        )
        
      } else if (dt_sub$perturb_method[1L] == "cross_val") {
        main_message <- c(
          main_message,
          paste0(
            dt_sub$perturb_n_rep[1L], " repetitions of ",
            dt_sub$perturb_n_folds, "-fold cross validation of"
          )
        )
        
      } else if (dt_sub$perturb_method[1L] == "loocv") {
        main_message <- c(
          main_message,
          "folds of leave-one-out-cross-validation of"
        )
        
      } else if (dt_sub$perturb_method[1L] == "imbalance_partition") {
        main_message <- c(
          main_message,
          "class-balanced partitions of"
        )
      }
      
      curr_ref_data_id <- dt_sub$ref_data_id[1L]
    }
    
    logger_message(
      paste0(main_message, collapse = " "),
      indent = message_indent,
      verbose = verbose
    )
    
    # Model building second
    main_message <- "Setup report: Model building on"
    
    # Iteratively append message
    dt_sub <- section_table[model_building == TRUE, ]
    curr_ref_data_id <- dt_sub$main_data_id[1L]
    
    while (curr_ref_data_id > 0L) {
      
      dt_sub <- section_table[main_data_id == curr_ref_data_id, ]
      
      if (dt_sub$perturb_method[1L] == "main") {
        main_message <- c(
          main_message,
          "the training data."
        )
        
      } else if (dt_sub$perturb_method[1L] %in% c("limited_bootstrap", "full_bootstrap")) {
        main_message <- c(
          main_message,
          paste0(dt_sub$perturb_n_rep[1L], " bootstraps of")
        )
        
      } else if (dt_sub$perturb_method[1L] == "cross_val") {
        main_message <- c(
          main_message,
          paste0(
            dt_sub$perturb_n_rep[1L], " repetitions of ",
            dt_sub$perturb_n_folds, "-fold cross validation of"
          )
        )
        
      } else if (dt_sub$perturb_method[1L] == "loocv") {
        main_message <- c(
          main_message,
          "folds of leave-one-out-cross-validation of"
        )
        
      } else if (dt_sub$perturb_method[1L] == "imbalance_partition") {
        main_message <- c(
          main_message,
          "class-balanced partitions of"
        )
      }
      
      curr_ref_data_id <- dt_sub$ref_data_id[1L]
    }
    
    logger_message(
      paste0(main_message, collapse = " "),
      indent = message_indent,
      verbose = verbose
    )
  }
}


.check_experimental_design_section_table <- function(section_table) {
  if (sum(section_table$feat_sel) > 1L) {
    ..error(
      paste0(
        "The fs component for feature selection may only be used once ",
        "in the experimental design."
      ),
      error_class = "input_argument_error"
    )
  }
  
  if (sum(section_table$feat_sel) == 0L) {
    ..error(
      paste0(
        "The fs component for feature selection must appear in the ",
        "experimental design. It was not found."
      ),
      error_class = "input_argument_error"
    )
  }
  
  if (sum(section_table$model_building) > 1L) {
    ..error(
      paste0(
        "The mb component for model building may only be used once ",
        "in the experimental design."
      ),
      error_class = "input_argument_error"
    )
  }
  
  if (sum(section_table$model_building) == 0L) {
    ..error(
      paste0(
        "The mb component for model building must appear in the ",
        "experimental design. It was not found."
      ),
      error_class = "input_argument_error"
    )
  }
  
  if (sum(section_table$external_validation) > 1L) {
    ..error(
      paste0(
        "The ev component for external validation can only appear once ",
        "in the experimental design."
      ),
      error_class = "input_argument_error"
    )
  }
  
  return(invisible(TRUE))
}

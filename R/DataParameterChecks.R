#' Internal check and update of settings related to data set parsing
#'
#' This function updates and checks parameters related to data set parsing based
#' on the available data set.
#'
#' @param formula User-provided formula, may be absent (`NULL`).
#' @param data Data set as loaded using the `.load_data` function.
#' @param settings List of parameter settings for data set parsing.
#' @inheritParams as_data_object
#'
#' @return A verified and updated list of parameter settings.
#' @md
#' @keywords internal
.update_initial_settings <- function(
    formula = NULL,
    data,
    settings,
    check_stringency = "strict") {
  # Outcome columns, outcome type, signature, included_features,
  # excluded_features, class_levels.

  if (!is.null(formula)) {
    
    # Parse information from the formula
    term_object <- stats::terms(formula, data=data)

    # Derive predictors
    predictor_vars <- check_column_name(attributes(term_object)$term.labels)
    all_vars <- check_column_name(all.vars(stats::terms(term_object)))
    outcome_col <- all_vars[!all_vars %in% predictor_vars]
    
    # Set outcome_col if it was previously unknown
    if (is.null(settings$data$outcome_col)) {
      settings$data$outcome_col <- outcome_col
    }
    
    # Ignore include_features and exclude_features when using the formula
    # interface
    if (!is.null(settings$data$include_features)) {
      warning("The include_features parameter is ignored when using the formula interface.")
      settings$data$include_features <- NULL
    }
    
    if (!is.null(settings$data$exclude_features)) {
      warning("The exclude_features parameter is ignored when using the formula interface.")
      settings$data$exclude_features <- NULL
    }
    
  } else {
    
    all_vars <- colnames(data)
    if (is.null(settings$data$outcome_col)) {
      predictor_vars <- all_vars
      
    } else {
      predictor_vars <- all_vars[!all_vars %in% settings$data$outcome_col]
    }
  }
  
  # Identifier columns----------------------------------------------------------
  if (!is.null(settings$data$sample_col) && check_stringency != "strict") {
    
    # Check if the internal default is used instead.
    if(!any(settings$data$sample_col %in% colnames(data)) &&
       get_id_columns(single_column = "sample") %in% colnames(data)) {
      settings$data$sample_col <- get_id_columns(single_column = "sample")
    }
  }
  
  if (!is.null(settings$data$batch_col) && check_stringency != "strict") {
    
    # Check if the internal default is used instead.
    if (!any(settings$data$batch_col %in% colnames(data)) &&
       get_id_columns(single_column = "batch") %in% colnames(data)) {
      settings$data$batch_col <- get_id_columns(single_column = "batch")
    }
  }
  
  if (!is.null(settings$data$series_col) & check_stringency != "strict") {
    
    # Check if the internal default is used instead.
    if (!any(settings$data$series_col %in% colnames(data)) &&
        get_id_columns(single_column = "series") %in% colnames(data)) {
      settings$data$series_col <- get_id_columns(single_column = "series")
    }
  }
  
  ## Sample column -------------------------------------------------------------
  if (!is.null(settings$data$sample_col)) {
    # Check input
    .check_input_identifier_column(
      id_column = settings$data$sample_col,
      data = data,
      signature = settings$data$signature,
      exclude_features = settings$data$exclude_features,
      include_features = settings$data$include_features,
      other_id_column = c(settings$data$batch_col, settings$data$series_col),
      outcome_column = settings$data$outcome_col,
      col_type = "sample",
      check_stringency = check_stringency)
    
    # Remove sample column identifier if it doesn't appear in the dataset, and
    # autopopulate the column. Only possible when checks are not strict.
    if (!settings$data$sample_col %in% colnames(data) &&
        check_stringency != "strict") {
      settings$data$sample_col <- NULL
    }
    
    # Remove the sample identifier column from the set of predictors
    predictor_vars <- setdiff(predictor_vars, settings$data$sample_col)
  }
  
  ## Batch column --------------------------------------------------------------
  if (!is.null(settings$data$batch_col)) {
    # Check input
    .check_input_identifier_column(
      id_column = settings$data$batch_col,
      data = data,
      signature = settings$data$signature,
      exclude_features = settings$data$exclude_features,
      include_features = settings$data$include_features,
      other_id_column = c(settings$data$sample_col, settings$data$series_col), 
      outcome_column = settings$data$outcome_col,
      col_type = "batch",
      check_stringency = check_stringency)
    
    # Remove batch column identifier if it doesn't appear in the dataset, and
    # autopopulate the column. Only possible when checks are not strict.
    if (!settings$data$batch_col %in% colnames(data) &&
        check_stringency != "strict") {
      settings$data$batch_col <- NULL
    }
    
    # Remove the batch identifier column from the set of predictors
    predictor_vars <- setdiff(predictor_vars, settings$data$batch_col)
  }
  
  
  ## Series column -------------------------------------------------------------
  if (!is.null(settings$data$series_col)) {
    # Check input
    .check_input_identifier_column(
      id_column = settings$data$series_col,
      data = data,
      signature = settings$data$signature,
      exclude_features = settings$data$exclude_features,
      include_features = settings$data$include_features,
      other_id_column = c(settings$data$batch_col, settings$data$sample_col),
      outcome_column = settings$data$outcome_col,
      col_type = "series",
      check_stringency = check_stringency)
    
    # Remove series column identifier if it doesn't appear in the dataset, and
    # autopopulate the column. Only possible when checks are not strict.
    if (!settings$data$series_col %in% colnames(data) &&
        check_stringency != "strict") {
      settings$data$series_col <- NULL
    }
    
    # Remove the series identifier column from the set of predictors
    predictor_vars <- setdiff(predictor_vars, settings$data$series_col)
  }
  
  # Outcome column -------------------------------------------------------------
  outcome_type <- "unset"
  if (!is.null(settings$data$outcome_type)) outcome_type <- settings$data$outcome_type
  
  if (outcome_type != "unset" && check_stringency != "strict") {
    # Check if outcome columns are present, or internal defaults are present.
    if (!any(settings$data$outcome_col %in% colnames(data)) &&
        all(get_outcome_columns(outcome_type) %in% colnames(data))) {
      settings$data$outcome_col <- get_outcome_columns(settings$data$outcome_type)
    }
    
    # Set outcome column to NULL for unsupervised data, and assign to
    # exclude_features.
    if (outcome_type == "unsupervised") {
      if (!is.null(settings$data$outcome_col)) {
        warning(paste0(
          paste_s(settings$data$outcome_col), " was selected as an outcome column, but ",
          "unsupervised learners and novelty detectors do not require ",
          "outcome columns."))
        
        settings$data$exclude_features <- c(
          settings$data$exclude_features,
          settings$data$outcome_col)
        settings$data$outcome_col <- NULL
      }
    }
  }
  
  # Check for presence of outcome column
  if (is.null(settings$data$outcome_col) &&
      outcome_type != "unsupervised" &&
      check_stringency == "strict") {
    
    # Attempt to determine the outcome_col from the set difference of all
    # features and the union of signature, include_features and
    # exclude_features.
    outcome_col <- setdiff(
      predictor_vars,
      c(
        settings$data$exclude_features,
        settings$data$include_features,
        settings$data$signature,
        settings$data$novelty_features))
    
    if (length(outcome_col) == 1) {
      # Only select outcome column with stringent checks.
      warning(paste0(
        paste_s(outcome_col), "was selected as an outcome column. It is recommended" ,
        "to provide the column name manually to avoid selecting the wrong column."))
      
      # Set the outcome column
      settings$data$outcome_col <- outcome_col
      
      # Update predictor_vars
      predictor_vars <- predictor_vars[!predictor_vars %in% outcome_col]
      
    } else {
      stop(paste0(
        "No column(s) that determine the outcome were provided and none could be imputed. ",
        " Please provide outcome columns."))
    }
  }
  
  if (!is.null(settings$data$outcome_col)) {
    # For strict checks outcome columns must be present. Check if outcome
    # columns are present in the data
    if (!all(settings$data$outcome_col %in% colnames(data))) {
      missing_col <- setdiff(settings$data$outcome_col, colnames(data))
      
      if (check_stringency %in% c("strict", "external_warn")) {
        stop_or_warn(paste0(
          "The outcome column ", paste_s(missing_col),
          " does not appear in the provided data set."),
          check_stringency == "strict")
      }
      
      # Set NULL and auto-generate outcome columns later.
      if (check_stringency != "strict") settings$data$outcome_col <- NULL
    }
  }
  
  if (length(settings$data$outcome_col) > 2) {
    stop("Only one or two (in case of survival endpoints), may be specified")
  }
  
  
  # Outcome type ---------------------------------------------------------------
  
  # Attempt to impute outcome settings
  if(outcome_type == "unset"){
    settings$data$outcome_type <- .impute_outcome_type(
      data = data,
      outcome_column = settings$data$outcome_col,
      class_levels = settings$data$class_levels,
      censoring_indicator = settings$data$censoring_indicator,
      event_indicator = settings$data$event_indicator,
      competing_risk_indicator = settings$data$competing_risk_indicator)
  }
  
  # Check whether the outcome type fits the data.
  .check_outcome_type_plausibility(
    data = data,
    outcome_type = settings$data$outcome_type,
    outcome_column = settings$data$outcome_col,
    censoring_indicator = settings$data$censoring_indicator,
    event_indicator = settings$data$event_indicator,
    competing_risk_indicator = settings$data$competing_risk_indicator,
    check_stringency = check_stringency)
  
  # Class levels ---------------------------------------------------------------
  .check_class_level_plausibility(
    data = data,
    outcome_type = settings$data$outcome_type,
    outcome_column = settings$data$outcome_col,
    class_levels = settings$data$class_levels,
    check_stringency = check_stringency)
  
  # Set class levels in settings, if appropriate.
  if (is.null(settings$data$class_levels) &&
      settings$data$outcome_type %in% c("binomial", "multinomial") &&
      !is.null(settings$data$outcome_col)) {
    
    if (is.factor(data[[settings$data$outcome_col]])) {
      settings$data$class_levels <- levels(data[[settings$data$outcome_col]])
      
    } else {
      settings$data$class_levels <- sort(unique_na(data[[settings$data$outcome_col]]))
    }
  }
  
  # Survival event indicator ---------------------------------------------------
  if(settings$data$outcome_type %in% c("survival", "competing_risk") &&
     !is.null(settings$data$outcome_col)){
    settings <- .impute_survival_indicators(
      data = data,
      outcome_type = settings$data$outcome_type,
      settings = settings,
      check_stringency = check_stringency)
  }
  
  # Features -------------------------------------------------------------------
  
  ## Signature features --------------------------------------------------------
  if (!is.null(settings$data$signature)) {
    # Check for overlap with exclude_features
    overlap_cols <- intersect(
      settings$data$signature,
      settings$data$exclude_features)
    
    if (length(overlap_cols) > 0) {
      stop(paste0(
        "One or more columns were provided that both appear in the signature and ",
        "among the features that should be removed. There can be no overlap. Found: ",
        paste_s(overlap_cols)))
    }
    
    # Check if all features in the signature appear in the data
    missing_cols <- settings$data$signature[!settings$data$signature %in% predictor_vars]
    if (length(missing_cols) > 0) {
      stop(paste0(
        "One or more features assigned to the signature were not found in the data set:",
        paste_s(missing_cols)))
    }
  }
  
  ## Novelty features ----------------------------------------------------------
  if(!is.null(settings$data$novelty_features)){
    # Check for overlap with exclude_features
    overlap_cols <- intersect(
      settings$data$novelty_features,
      settings$data$exclude_features)
    
    if (length(overlap_cols) > 0) {
      stop(paste0(
        "One or more columns were provided that both appear in novelty_features and",
        "among the features that should be removed. There can be no overlap. Found:",
        paste_s(overlap_cols)))
    }
    
    # Check if all features in novelty_features appear in the data
    missing_cols <- settings$data$novelty_features[!settings$data$novelty_features %in% predictor_vars]
    if (length(missing_cols) > 0) {
      stop(paste0(
        "One or more features assigned to novelty_features were not found in the data set:",
        paste_s(missing_cols)))
    }
  }
  
  ## Excluded features ---------------------------------------------------------
  if (!is.null(settings$data$exclude_features)) {
    
    # Check if all features marked for exclusion appear in the data
    missing_cols <- settings$data$exclude_features[!settings$data$exclude_features %in% predictor_vars]
    if (length(missing_cols) > 0) {
      stop(paste0(
        "One or more features marked for exclusion were not found in the data set:",
        paste_s(missing_cols)))
    }
    
    # Check if include_features also exists.
    overlap_cols <- intersect(
      settings$data$include_features,
      settings$data$exclude_features)
    
    if (length(overlap_cols) > 0) {
      stop(paste0(
        "One or more columns were provided that appear in both the set of features ",
        "marked for exclusion and for inclusion. There can be no overlap. Found:",
        paste_s(overlap_cols)))
    }
    
    # Notify the used that include_features parameter takes precedence.
    if (!is.null(settings$data$include_features)) {
      warning(paste0(
        "Features marked for inclusion take precedence over the features marked for inclusion, i.e. ",
        "the final data set will only contain those features marked for inclusion."))
      
      settings$data$exclude_features <- NULL
    }
  }
  
  ## Included features ---------------------------------------------------------
  if(!is.null(settings$data$include_features)){
    
    missing_cols <- settings$data$include_features[!settings$data$include_features %in% predictor_vars]
    
    if (length(missing_cols) > 0) {
      stop(paste0(
        "One or more features marked for inclusion were not found in the data set:",
        paste_s(missing_cols)))
    }
  }
  
  # Determine which features should go to include_features.
  if (!is.null(settings$data$exclude_features)) {
    # Select everything but the features marked for exclusion
    settings$data$include_features <- setdiff(
      predictor_vars,
      settings$data$exclude_features)
    
  } else if (!is.null(settings$data$include_features)) {
    # Select features marked for inclusion and signature, in so far as these do not overlap.
    settings$data$include_features <- unique(c(
      settings$data$include_features,
      settings$data$signature,
      settings$data$novelty_features))
    
  } else {
    # Select all available predictor variables.
    settings$data$include_features <- predictor_vars
  }
  
  # Outcome name ---------------------------------------------------------------
  if (is.null(settings$data$outcome_name) &&
      settings$data$outcome_type %in% c("survival", "competing_risk")) {
    if (!is.null(settings$data$outcome_col)) {
      # Use the name of the event column, because its usually more indicative.
      settings$data$outcome_name <- tolower(settings$data$outcome_col[2])
      
    } else {
      settings$data$outcome_name <- "survival"
    }
    
  } else if (is.null(settings$data$outcome_name)) {
    if (!is.null(settings$data$outcome_col)) {
      settings$data$outcome_name <- tolower(settings$data$outcome_col)
      
    } else {
      settings$data$outcome_name <- "outcome"
    }
  }
  
  return(settings)
}



#' Internal function to check batch assignment to development and validation
#'
#' This function checks which batches in the data set are assigned to model
#' development and external validation. Several errors may be raised if there
#' are inconsistencies such as an overlapping assignment, name mismatches etc.
#'
#' @param section_table data.table generated by the `extract_experimental_setup`
#'  function. Contains information regarding the experiment.
#' @param data Data set as loaded using the `.load_data` function.
#' @param settings List of parameter settings for data set parsing and setting
#'  up the experiment.
#'
#' @return A verified and updated list of parameter settings.
#' @md
#' @keywords internal
.update_experimental_design_settings <- function(
    section_table,
    data,
    settings){

  # Find out if any external validation is performed as part of the workflow
  if (is.waive(section_table)) {
    # If the section table is waived, this means that iterations come from a
    # user-provided different file.
    return(settings)
    
  } else {
    perform_external_validation <- any(section_table$external_validation)
  }
  
  # Determine the available batch identifiers
  available_batch_ids <- unique(data[[get_id_columns(single_column="batch")]])
  
  # Determine what happens if batch identifiers are not specified for both
  # development and validation.
  if (is.null(union(settings$data$train_cohorts, settings$data$valid_cohorts)) &&
      !perform_external_validation) {
    # Use all cohorts for training if nothing is specifically provided, and
    # external validation is not necessary.
    settings$data$train_cohorts <- available_batch_ids
    
  } else if (is.null(union(settings$data$train_cohorts, settings$data$valid_cohorts)) &&
             perform_external_validation) {
    # Validation cohort(s) should be provided, or be identifiable
    stop(paste0("The validation_batch_id variable should be set to perform external validation."))
  } 
  
  # Check if some cohorts are available for external validation, if required.
  if (all(available_batch_ids %in% settings$data$train_cohorts) &&
      perform_external_validation) {
    # All cohorts are found in the set used for development. Prompt the user
    # to assign some cohorts to validation.
    stop(paste0(
      "All batches/cohorts in the data set are assigned for model development. ",
      "External validation is not possible. ",
      "Please assign one or more batches/cohorts to external validation by ", 
      "passing their names as an argument to the validation_batch_id variable."))
  }
  
  # Check if one or more cohorts are available for development.
  if (all(available_batch_ids %in% settings$data$valid_cohorts)) {
    # All cohorts are found in the set used for external validation. Prompt the
    # user to assign some development cohorts.
    stop(paste0(
      "All batches/cohorts in the data set are assigned for external validation. ",
      "Model development is not possible. ",
      "Please assign one or more batches/cohorts to model development by ",
      "passing their names as an argument to the development_batch_id variable."))
  }
  
  # Check whether there is an overlap between development and validation group
  # identifiers.
  overlapping_batches <- intersect(
    settings$data$train_cohorts,
    settings$data$valid_cohorts)
    
  if (length(overlapping_batches) > 0) {
    stop(paste0(
      "One or more batch/cohort names occur in both the development_batch_id ",
      "and validation_batch_id variables: ",
      paste_s(overlapping_batches)))
  }
  
  # Check whether all specified development group identifiers actually appear in
  # the data.
  missing_batches <- setdiff(
    settings$data$train_cohorts,
    available_batch_ids)
    
  if (length(missing_batches) > 0) {
    stop(paste0(
      "One or more batch/cohort names specified in the development_batch_id ",
      "variable could not be found in the data: ",
      paste_s(missing_batches)))
  }
  
  # Determine if there are any missing batches for validation cohorts
  missing_batches <- setdiff(
    settings$data$valid_cohorts,
    available_batch_ids)
  
  if (length(missing_batches) > 0 && perform_external_validation) {
    # Raise an error if external validation is expected.
    stop(paste0(
      "One or more batch/cohort names specified in the validation_batch_id ",
      "variable could not be found in the data: ",
      paste_s(missing_batches)))
    
  } else if (length(missing_batches) > 0) {
    # Filter out the missing cohort names when external validation is not
    # performed.
    settings$data$valid_cohorts <- setdiff(
      settings$data$valid_cohorts,
      missing_batches)
    
    # If the remaining number of validation cohorts is zero, set valid_cohorts
    # to NULL
    if (length(settings$data$valid_cohorts) == 0) settings$data$valid_cohorts <- NULL
  }
  
  # Infer validation cohorts.
  if (!is.null(settings$data$train_cohorts) &&
      is.null(settings$data$valid_cohorts)) {
    # Identify batch ids that appear in the data set but are not included for
    # development. These are then assigned for validation.
    new_validation_batch_id <- setdiff(
      available_batch_ids,
      settings$data$train_cohorts)
    
    if (length(new_validation_batch_id) > 0 && perform_external_validation) {
      message(paste0(
        "One or more batches/cohorts were not used for development and are now ",
        "used for external validation: ",
        paste_s(new_validation_batch_id)))
      
      settings$data$valid_cohorts <- new_validation_batch_id
      
    } else if (length(new_validation_batch_id)) {
      message(paste0(
        "One or more batches/cohorts in the data are not used for development because they ",
        "were not provided in the development_batch_id variable."))
    }
  }
  
  # Infer training cohorts
  if (is.null(settings$data$train_cohorts) && 
      !is.null(settings$data$valid_cohorts)) {
    # Identify batch ids that appear in the data set but are not included for
    # validation. These are then assigned for development.
    new_development_batch_id <- setdiff(
      available_batch_ids,
      settings$data$valid_cohorts)
    
    if (length(new_development_batch_id) > 0) {
      message(paste0(
        "One or more batches/cohorts were not used for external validation and are now ",
        "used for development: ",
        paste_s(new_development_batch_id)))
      
      settings$data$train_cohorts <- new_development_batch_id
    }
  }
  
  # Determine if there are any unused groups
  unused_batch_id <- setdiff(
    available_batch_ids,
    union(
      settings$data$train_cohorts,
      settings$data$valid_cohorts))
  
  if (length(unused_batch_id) > 0) {
    message(paste0(
      "One or more batches/cohorts are not used for development or external validation: ",
      paste_s(unused_batch_id)))
  }
  
  return(settings)
}



#' Internal imputation function for the outcome type.
#'
#' This function allows for imputation of the most plausible outcome type.
#' This imputation is only done for trivial cases, where there is little doubt.
#' As a consequence `count` and `continuous` outcome types are never imputed.
#'
#' @param data Data set as loaded using the `.load_data` function.
#' @param outcome_column Name of the outcome column in the data set. 
#' @param class_levels User-provided class levels for the outcome.
#' @param censoring_indicator Name of censoring indicator.
#' @param event_indicator Name of event indicator.
#' @param competing_risk_indicator Name of competing risk indicator.
#' 
#' @note It is highly recommended that the user provides the outcome type.
#'
#' @return The imputed outcome type.
#' @md
#' @keywords internal
.impute_outcome_type <- function(
    data,
    outcome_column,
    class_levels,
    censoring_indicator,
    event_indicator,
    competing_risk_indicator) {

  if (length(outcome_column) > 2) {
    stop(paste0(
      "Only one or two (in case of survival endpoints) outcome columns are expected.",
      "However ", length(outcome_column), " were found."))
  }
  
  # Test for survival
  if (length(outcome_column) == 2) {
    # One column should contain continuous data >= 0.0 (time column). One column
    # should contain numerical or logical data with 0, 1 (event status column)
    
    # Check for presence of an event column
    event_survival_cols <- sapply(
      outcome_column,
      .is_survival_status_col, 
      data = data,
      censoring_indicator = censoring_indicator,
      event_indicator = event_indicator,
      competing_risk_indicator = NULL)
    
    event_competing_risk_cols <- sapply(
      outcome_column,
      .is_survival_status_col,
      data = data,
      censoring_indicator = censoring_indicator,
      event_indicator = event_indicator,
      competing_risk_indicator = competing_risk_indicator)
    
    # Check for presence of a time column
    time_cols <- sapply(
      outcome_column,
      .is_survival_time_col,
      data = data)
    
    if (!any(time_cols) ||
        (!any(event_survival_cols) && !any(event_competing_risk_cols))) {
      # Check if there is at least one event_col and one time_col, and in case
      # there is only one event_col and one time_col, that these are not the
      # same.
      stop(paste0(
        "Survival or competing risk outcomes were expected as two outcome columns were provided. ",
        "However, data in the columns did not strictly correspond to these outcomes. ",
        "Check if a survival outcome was intended, and provide a single outcome column otherwise. ",
        "Columns for survival or competing risk outcomes should contain time (numeric values >= 0) and event status ",
        "(0 and 1 or FALSE and TRUE, or values according to censoring_indicator, ",
        "event_indicator and competing_risk_indicator arguments) information."))
      
    } else if (sum(time_cols) == 1 &&
               sum(event_survival_cols) == 1 &&
               sum(event_competing_risk_cols) == 1) {
      # Only one column of either type. The test below yields 0 if these are
      # different columns, and 1 if they are the same column, in which case an
      # error is raised.
      
      if (sum(time_cols * event_survival_cols) &&
          sum(time_cols * event_competing_risk_cols)) {
        stop(paste0(
          "Survival or competing risk outcome was expected as two outcome columns were provided. ",
          "However, data in the columns did not strictly correspond to these outcomes. ",
          "Check if a survival outcome was intended, and provide a single outcome column otherwise. ",
          "Columns for survival or competing risk outcomes should contain time (numeric values >= 0) and event status ",
          "(0 and 1 or FALSE and TRUE, censoring_indicator, event_indicator ",
          "and competing_risk_indicator arguments) information."))
      }
    }
    
    if (is.null(competing_risk_indicator)) {
      # Set outcome_type to survival as it is plausible
      message(paste0(
        "A survival outcome was imputed based on the data. If this is an incorrect type, ",
        "please provide an outcome_type manually."))
      
      return("survival")
      
    } else {
      # Set outcome_type to competing_risk as it is plausible
      message(paste0(
        "A competing_risk outcome was imputed based on the data. If this is an incorrect type,",
        "please provide an outcome_type manually."))
      
      return("competing_risk")
    }
  }
  
  if (length(outcome_column) == 1) {
    # Extract data
    x <- data[[outcome_column]]
    
    # Test for binomial and multinomial outcomes based on class_levels
    if (!is.null(class_levels)) {
      if (all(x %in% class_levels, na.rm = TRUE)) {
        if (length(class_levels) == 2) {
          message(paste0(
            "A binomial outcome was imputed based on the data. If this is an incorrect type, ",
            "please provide an outcome_type manually."))
          
          return("binomial")
          
        } else if (length(class_levels) > 2) {
          message(paste0(
            "A multinomial outcome was imputed based on the data. If this is an incorrect type, ",
            "please provide an outcome_type manually."))
          
          return("multinomial")
        }
      }
    }
    
    # Test for categorical outcomes based on class of the data in the outcome
    # column, in this case factor.
    if (is.factor(x)) {
      
      if (nlevels(x) == 2) {
        message(paste0(
          "A binomial outcome was imputed based on the data. If this is an incorrect type, ",
          "please provide an outcome_type manually."))
        
        return("binomial")
        
      } else if (nlevels(x) > 2) {
        message(paste0(
          "A multinomial outcome was imputed based on the data. If this is an incorrect type,",
          "please provide an outcome_type manually."))
        
        return("multinomial")
      }
    }
    
    # Test for categorical outcomes based on class of the data in the outcome
    # column, in this case logical.
    if (is.logical(x)) {
      message(paste0(
        "A binomial outcome was imputed based on the data. If this is an incorrect type,",
        "please provide an outcome_type manually."))
      
      return("binomial")
    }
    
    # Test for categorical outcomes based on class of the data in the outcome
    # column, in this case character.
    if (is.character(x)) {
      if (data.table::uniqueN(x, na.rm=TRUE) == 2) {
        message(paste0(
          "A binomial outcome was imputed based on the data. If this is an incorrect type, ",
          "please provide an outcome_type manually."))
        
        return("binomial")
        
      } else if (data.table::uniqueN(x, na.rm=TRUE) > 2) {
        message(paste0(
          "A multinomial outcome was imputed based on the data. If this is an incorrect type, ",
          "please provide an outcome_type manually."))
        
        return("multinomial")
      }
    }
    
    # Tests for continuous and count type data are not really possible. One
    # could test on is.numeric, and minimum value, but these data could still
    # represent binomial (e.g. 0s and 1s) or multinomial data. One may devise
    # some tests by counting the number of samples with a unique value, but this
    # is dangerous for smaller data sets.
    stop("Imputation of the outcome type was not possible. Please provide an outcome type manually.")
  }
}



.impute_survival_indicators <- function(
    data,
    outcome_type,
    settings,
    check_stringency = "strict") {
  
  # Skip checks if check stringency is external: no checks on outcome columns
  # are required.
  if (check_stringency == "external") return(settings)
  
  # Define standard indicators for censoring, event and competing risk.
  standard_censoring_indicator <- c("0", "false", "f", "n", "no")
  if (!is.null(settings$data$censoring_indicator)) {
    standard_censoring_indicator <- settings$data$censoring_indicator
  }
  
  standard_event_indicator <- c("1", "true", "t", "y", "yes")
  if (!is.null(settings$data$event_indicator)) {
    standard_event_indicator <- settings$data$event_indicator
  }
  
  standard_competing_risk_indicator <- NULL
  if (!is.null(settings$data$competing_risk_indicator)) {
    standard_competing_risk_indicator <- settings$data$competing_risk_indicator
  }
  
  # Combine indicators
  all_indicators <- c(
    standard_censoring_indicator,
    standard_event_indicator,
    standard_competing_risk_indicator)
  
  # Find the number of matches with each of the two outcome columns.
  n_matches <- sapply(
    settings$data$outcome_col,
    function(column, data, all_indicators) {
      x <- data[[column]]
      x <- tolower(as.character(x[!is.na(x)]))
      
    return(sum(x %in% tolower(all_indicators)))
  },
  data = data,
  all_indicators = all_indicators)
  
  # For non-strict test, we assume that that outcome columns are already
  # organised as time, event, whereas for strict tests we need to identify it de
  # novo.
  if (check_stringency == "strict") {
    # Identify the column that is most likely to be the event column.
    event_column <- settings$data$outcome_col[which.max(n_matches)]
    
  } else {
    event_column <- settings$data$outcome_col[2]
  }
  
  # Select unique values in the event column.
  event_values <- data[[event_column]]
  event_values <- unique_na(event_values)
  
  # Try to identify indicators present in the dataset.
  present_censoring_indicator <- event_values[
    tolower(as.character(event_values)) %in% tolower(standard_censoring_indicator)]
  present_event_indicator <- event_values[
    tolower(as.character(event_values)) %in% tolower(standard_event_indicator)]
  present_competing_risk_indicator <- event_values[
    tolower(as.character(event_values)) %in% tolower(standard_competing_risk_indicator)]
  
  # Identify values in the event column which are not yet assigned.
  event_values <- setdiff(
    event_values, c(
      present_censoring_indicator,
      present_event_indicator,
      present_competing_risk_indicator))
  
  # Check the presence of user-provided indicators.
  if (!is.null(settings$data$event_indicator) && length(event_values) > 0) {
    
    # An event indicator should always be present.
    if (!all(tolower(as.character(settings$data$event_indicator)) %in% tolower(as.character(event_values))) &&
       check_stringency == "strict") {
      stop(paste0(
        "The following provided event indicator(s) were not found in the data: ",
        paste_s(setdiff(
          tolower(as.character(settings$data$event_indicator)),
          tolower(as.character(event_values)))),
        " . Please check for spelling errors."))
    }
  }
  
  if (!is.null(settings$data$censoring_indicator) && length(event_values) > 0) {
    # An censoring indicator is not required to be present, but if the user
    # provides one, and it isn't found in the dataset, raise an error.
    if (!all(tolower(as.character(settings$data$censoring_indicator)) %in% tolower(as.character(event_values))) &&
       check_stringency == "strict") {
      stop(paste0(
        "The following provided censoring indicator(s) were not found in the data: ",
        paste_s(setdiff(
          tolower(as.character(settings$data$censoring_indicator)),
          tolower(as.character(event_values))))))
    }
  }
  
  if (!is.null(settings$data$competing_risk_indicator) && length(event_values) > 0) {
    # Competing risk indicators are only present in competing_risk outcomes.
    if(outcome_type == "survival" && check_stringency == "strict"){
      stop(paste0(
        "One or indicators for competing risks were specified. ",
        "However, the outcome type was set to survival, which does not check competing risks. ",
        "Please change outcome_type to competing_risk, or remove the competing risk indicators."))
    }
    
    if(!all(tolower(as.character(settings$data$competing_risk_indicator)) %in% tolower(as.character(event_values))) &&
       check_stringency == "strict") {
      stop(paste0(
        "The following provided competing risk indicator(s) were not found in the data: ",
        paste_s(setdiff(
          tolower(as.character(settings$data$competing_risk_indicator)),
          tolower(as.character(event_values))))))
    }
  }

  if (length(event_values) > 0) {
    # Event indicator can only be determined if there is one value left over.
    if (length(present_event_indicator) == 0) {
      
      if (length(event_values) == 1) {
        
        # Assign the remaining value to the event indicator.
        present_event_indicator <- event_values
        
        # Make event_values empty.
        event_values <- setdiff(event_values, event_values)
        
        if (check_stringency %in% c("strict", "external_warn")) {
          # Throw a warning, because the assumption may be false.
          warning(paste0(
            "The event indicator for survival/competing risk outcomes was neither ",
            "a standard value, nor provided. Based on the values found in ", event_column,
            ", ", present_event_indicator, " was selected as an event indicator."))
        }
        
      } else if (check_stringency %in% c("strict", "external_warn")) {
        stop_or_warn(
          paste0(
            "The event indicator for survival/competing risk outcomes was neither ",
            "a standard value, nor provided. More than one unassigned value was found in ",
            event_column, ", preventing automatic assignment: ",
            paste_s(event_values)),
          as_error=check_stringency == "strict")
      }
    }
  }
  
  if (length(event_values) > 0 && outcome_type == "survival") {
    if (length(present_censoring_indicator) == 0) {
      
      # Assign all remaining values.
      present_censoring_indicator <- event_values
      
      # Make event_values empty.
      event_values <- setdiff(event_values, event_values)
      
      if (check_stringency %in% c("strict", "external_warn")) {
        # Throw a warning, because the assumption may be false.
        warning(paste0(
          "The censoring indicator for the survival outcome was neither a standard value, nor provided. ",
          "Based on the values found in ", event_column, ", ",
          paste_s(present_censoring_indicator),
          ifelse(length(present_censoring_indicator) == 1, " was", " were"),
          " selected as censoring indicator."))
      }
      
    } else if (check_stringency %in% c("strict", "external_warn")) {
      stop_or_warn(
        paste0(
          "One or more unassigned values for the survival outcome were found in ", event_column, ": ",
          paste_s(event_values),
          ". Please assign manually."),
        as_error=check_stringency == "strict")
    }
  }
  
  if (length(event_values) > 0 && outcome_type == "competing_risk") {
    if (length(present_censoring_indicator) == 0 &&
        length(present_competing_risk_indicator) == 0) {
      if (check_stringency %in% c("strict", "external_warn")) {
        stop_or_warn(
          paste0(
            "One or more unassigned values for the survival outcome were found in ", event_column, ": ",
            paste_s(event_values), ". These could not be assigned automatically as indicators for ",
            "censoring and competing risks were both missing. Please assign manually."),
          as_error=check_stringency == "strict")
      }
      
    } else if (length(present_censoring_indicator) == 0) {
      
      # Assign all remaining values.
      present_censoring_indicator <- event_values
      
      # Make event_values empty.
      event_values <- setdiff(event_values, event_values)
      
      if (check_stringency %in% c("strict", "external_warn")) {
        # Throw a warning, because the assumption may be false.
        warning(paste0(
          "The censoring indicator for the competing risk outcome was neither a standard value, nor provided. ",
          "Based on the values found in ", event_column, ", ",
          paste_s(present_censoring_indicator),
          ifelse(length(present_censoring_indicator) == 1, " was", " were"),
          " selected as censoring indicator."))
      }
      
    } else if (length(present_competing_risk_indicator) == 0) {
      
      # Assign all remaining values.
      present_competing_risk_indicator <- event_values
      
      # Make event_values empty.
      event_values <- setdiff(event_values, event_values)
      
      if (check_stringency %in% c("strict", "external_warn")) {
        # Throw a warning, because the assumption may be false.
        warning(paste0(
          "The competing risk indicator for the competing risk outcome was not provided. ",
          "Based on the values found in ", event_column, ", ",
          paste_s(present_competing_risk_indicator),
          ifelse(length(present_competing_risk_indicator) == 1, " was", " were"),
          " selected as competing risk indicator."))
      }
      
    } else {
      if (check_stringency %in% c("strict", "external_warn")) {
        stop_or_warn(
          paste0(
            "One or more unassigned values for the competing risk outcome were found in ",
            event_column, ": ", paste_s(event_values), 
            ". Please assign manually to the indicators."),
          as_error=check_stringency == "strict")
      }
    }
  }
  
  # Update censoring indicator in settings.
  if (length(present_censoring_indicator) > 0) {
    settings$data$censoring_indicator <- present_censoring_indicator
    
  } else {
    settings$data$censoring_indicator <- NULL
  } 
  
  # Update event indicator in settings.
  if (length(present_event_indicator) > 0) {
    settings$data$event_indicator <- present_event_indicator 
    
  } else {
    settings$data$event_indicator <- NULL
  } 
  
  if (length(present_competing_risk_indicator) > 0) {
    settings$data$competing_risk_indicator <- present_competing_risk_indicator
    
  } else {
    settings$data$competing_risk_indicator <- NULL
  }
  
  # Make sure that columns are organised as time, event. Only do this for strict
  # tests.
  if (check_stringency == "strict") {
    time_column <- setdiff(settings$data$outcome_col, event_column)
    settings$data$outcome_col <- c(time_column, event_column)
  }
  
  return(settings)
}



#' Internal function for checking if the outcome type fits well to the data
#'
#' This function may help identify if the outcome type is plausible
#' given the outcome data. In practice it also tests whether the outcome column
#' is actually correct given the outcome type.
#'
#' @param data Data set as loaded using the `.load_data` function.
#' @param outcome_type Character string indicating the type of outcome being
#'  assessed.
#' @param outcome_column Name of the outcome column in the data set.
#' @param censoring_indicator Name of censoring indicator.
#' @param event_indicator Name of event indicator.
#' @param competing_risk_indicator Name of competing risk indicator.
#' @inheritParams as_data_object
#' 
#' @return NULL
#' @md
#' @keywords internal
.check_outcome_type_plausibility <- function(
    data,
    outcome_type,
    outcome_column,
    censoring_indicator,
    event_indicator,
    competing_risk_indicator,
    check_stringency = "strict"){
  
  # Checks plausibility of the outcome type and identifies any errors 
  
  # Check if only a single outcome_type is specified.
  if (length(outcome_type) != 1) {
    stop(paste0("A single outcome type should be provided. Found: ", length(outcome_type)))
  }
  
  # Check if the provided outcome_type is a valid choice.
  if (!outcome_type %in% c(
    "binomial", "multinomial", "count", "continuous",
    "survival", "competing_risk", "unsupervised")) {
    ..error_no_known_outcome_type(outcome_type)
  }
  
  # Check if two columns are provided for survival data
  if (outcome_type %in% c("survival", "competing_risk")) {
    
    if (check_stringency != "strict" && !is.null(outcome_column)) {
      # For non-strict checks only check in case any outcome columns are
      # present.
      if (length(outcome_column) != 2) {
        stop(paste0(
          "Two outcome columns are expected for data with survival outcomes. Found: ",
          length(outcome_column)))
      }
      
    } else if (check_stringency == "strict") {
      # For strict checks always check.
      if (length(outcome_column) != 2) {
        stop(paste0(
          "Two outcome columns are expected for data with survival outcomes. Found: ",
          length(outcome_column)))
      }
    }
  }
    
  # Check if one column is provided for binomial, multinomial, count and
  # continuous outcome types.
  if (outcome_type %in% c("binomial", "multinomial", "count", "continuous")) {
    
    if (check_stringency != "strict" && !is.null(outcome_column)) {
      # For non-strict checks only check in case any outcome columns are
      # present.
      if (length(outcome_column) != 1) {
        stop(paste0(
          "One outcome column is expected for data with ", outcome_type, " outcomes. ",
          "Found: ", length(outcome_column)))
      }
      
    } else if(check_stringency == "strict"){
      # For strict checks always check.
      if(length(outcome_column) != 1){
        stop(paste0(
          "One outcome column is expected for data with ", outcome_type, " outcomes. ",
          "Found: ", length(outcome_column)))
      }
    }
  }
  
  # Check that no outcome columns are provided for unsupervised outcome types.
  if (outcome_type %in% "unsupervised") {
    
    if (check_stringency == "strict") {
      # For strict checks always check if outcome columns are present.
      if (length(outcome_column) > 0) {
        stop(paste0(
          "No outcome columns are expected for unsupervised analysis. Found: ",
          paste_s(outcome_column)))
      }
    }
  }
  
  # Check for outcome columns with NA values.
  outcome_na <- TRUE
  if(!is.null(outcome_column)){
    outcome_na <- any(sapply(
      outcome_column,
      function(ii, data) (!any(is_valid_data(data[[ii]]))),
      data = data))
  }
  
  if (check_stringency == "strict" &&
      outcome_na &&
      outcome_type != "unsupervised") {
    # Under strict conditions, outcome data should be present.
    stop(paste0("Outcome column(s) do not contain any data."))
    
  } else if (check_stringency == "external_warn" &&
             outcome_na &&
             outcome_type != "unsupervised") {
    # Under less strict conditions, outcome data may be absent, but the user
    # should be warned.
    warning(paste0(
      "Outcome column(s) do not contain any data. Some evaluation steps may fail to produce results."))
  }
  
  # Plausibility checks for binomial outcome type
  if (outcome_type == "binomial" && !outcome_na) {
    
    # Check which classes are present.
    classes_present <- unique_na(data[[outcome_column]])
    
    if (check_stringency == "strict") {
      if (length(classes_present) > 2) {
        stop(paste0(
          "More than two classes (", paste_s(classes_present) ,
          ") were found in the outcome column: ", outcome_column, ". ",
          "Exactly two classes are expected for the binomial outcome type. ",
          "Specify outcome_type=\"multinomial\" if this is intentional."))
        
      } else if (length(classes_present) < 2) {
        stop(paste0(
          "Fewer than two classes were found in the outcome column: ", outcome_column,
          ". Exactly two classes are expected for the binomial outcome type."))
      }
      
    } else if (check_stringency == "external_warn") {
      if (length(classes_present) > 2) {
        stop(paste0(
          "More than two classes (", paste_s(classes_present) 
          ,") were found in the outcome column: ", outcome_column,
          ". Exactly two classes are expected for the binomial outcome type."))
      }
    }
  } 
  
  # Plausibility check for the multinomial outcome type
  if (outcome_type == "multinomial" && !outcome_na) {
    # Check which classes are present.
    classes_present <- unique_na(data[[outcome_column]])
    
    if (check_stringency == "strict") {
      if (length(classes_present) < 2) {
        stop(paste0(
          "Fewer than two classes were found in the outcome column:", outcome_column,
          ". Two or more classes are expected for the multinomial outcome type."))
      }
    }
  }
  
  # Plausibility check for the count outcome type
  if (outcome_type == "count" && !outcome_na) {
    
    .Deprecated(msg = "The \"count\" outcome type will be deprecated in version 2.0.0.")
    
    if (check_stringency %in% c("strict", "external_warn")) {
      if (!is.numeric(data[[outcome_column]])) {
        stop(paste0(
          "The outcome column (", outcome_column, ") does not contain numeric data. ",
          "Numeric data are expected for the count outcome type."))
      }
      
      if (min(data[[outcome_column]], na.rm=TRUE) < 0.0) {
        stop_or_warn(
          paste0(
            "The outcome column (", outcome_column, ") contains values smaller than 0. ",
            "The count outcome type expects that all values are 0 or greater."),
          check_stringency == "strict")
      }
    }
  }
  
  # Plausibility check for the continuous outcome type
  if (outcome_type == "continuous" && !outcome_na) {
    
    if (check_stringency %in% c("strict", "external_warn")) {
      if (!is.numeric(data[[outcome_column]])) {
        stop(paste0(
          "The outcome column (", outcome_column, ") does not contain numeric data. ",
          "Numeric data are expected for the continuous outcome type."))
      }
    }
  }
  
  # Plausibility check for the survival outcome type
  if (outcome_type %in% c("survival", "competing_risk") & !outcome_na) {
    # Check for presence of an event column and time columns
    
    if (check_stringency == "strict") {
      if (outcome_type == "survival") {
        # The competing risk indicator should be absent for survival outcomes.
        event_cols <- sapply(
          outcome_column,
          .is_survival_status_col,
          data = data,
          censoring_indicator = censoring_indicator,
          event_indicator = event_indicator,
          competing_risk_indicator = NULL)
        
      } else {
        event_cols <- sapply(
          outcome_column,
          .is_survival_status_col,
          data = data,
          censoring_indicator = censoring_indicator,
          event_indicator = event_indicator,
          competing_risk_indicator = competing_risk_indicator)
      }
      
      time_cols <- sapply(
        outcome_column,
        .is_survival_time_col,
        data = data)
      
      # Check if there is at least one event_col and one time_col, and in case
      # there is only one event_col and one time_col, that these are not the
      # same.
      if (!any(event_cols)) {
        stop(paste0(
          "None of the outcome columns (", paste_s(outcome_column),
          ") contain event status information. This column may only contain values 0 and 1 or ",
          "FALSE and TRUE, or the value indicated by ",
          ifelse(
            outcome_type=="survival",
            "censoring_indicator and event_indicator",
            "censoring_indicator, event_indicator and competing_risk_indicator"),
          " arguments."))
        
      } else if (all(event_cols)) {
        stop(paste0(
          "Both outcome columns (", paste_s(outcome_column),
          ") seem to contain event status information. One column with survival times should ",
          "be provided together with one column with survival event status information."))
        
      } else if (!any(time_cols)) {
        stop(paste0(
          "None of the outcome columns (", paste_s(outcome_column),
          ") contain survival time information. This column may only contain numeric values ",
          "greater or equal to 0."))
        
      } else if (sum(time_cols) == 1 && sum(event_cols) == 1) {
        # Only one column of either type. The test below yields 0 if these are
        # different columns, and 1 if they are the same column, in which case an
        # error is raised.
        if (sum(time_cols * event_cols)) {
          stop(paste0(
            "None of the outcome columns (", paste_s(outcome_column),
            ") contain survival time information. This column may only contain numeric values ",
            "greater or equal to 0, and may not contain only 0s and 1s to avoid misinterpration ",
            "as event status information."))
        }
      }
    }
  }
  
  return(invisible(TRUE))
}



#' Internal function for checking consistency of the identifier columns
#' 
#' This function checks whether an identifier column is consistent, i.e. appears
#' it exists, there is only one, and there is no overlap with any user-provided
#' feature columns, identifiers, or 
#'
#' @param id_column Character string indicating the currently inspected
#'  identifier column.
#' @param data Data set as loaded using the `.load_data` function.
#' @param col_type Character string indicating the type of column, i.e. `sample`
#'  or `batch`.
#' @param other_id_column Character string indicating another identifier column.
#' @param outcome_column Character string indicating the outcome column(s).
#' @inheritParams .parse_experiment_settings
#' @inheritParams as_data_object
#' 
#' @return NULL
#' @md
#' @keywords internal
.check_input_identifier_column <- function(
    id_column,
    data,
    signature = NULL,
    exclude_features = NULL,
    include_features = NULL,
    other_id_column = NULL,
    outcome_column = NULL,
    col_type,
    check_stringency = "strict") {
  
  # Check number of provided columns
  if (length(id_column) > 1) {
    stop(paste0(
      "Only one column is expected to be contain ", col_type, " identifiers. ",
      length(id_column), " columns were provided."))
  }
  
  # End of checks for external.
  if (check_stringency == "external") return(invisible(TRUE))
  
  # Check whether the column exists in the data set. This may be turned into a
  # warning.
  if (!id_column %in% colnames(data)) {
    stop_or_warn(
      paste0(
        "The ", col_type, " identifier column ", id_column,
        " does not appear in the provided data set."),
      check_stringency == "strict")
  }

  # Check whether the id column is the same as another id_column
  if (length(intersect(id_column, other_id_column)) > 0) {
    stop(paste0(
      "The ", col_type, " identifier column ", id_column,
      " is also used as a different identifier column."))
  }
  
  # Check whether the id column overlaps with the outcome column
  if (length(intersect(id_column, outcome_column)) > 0) {
    stop(paste0(
      "The ", col_type, " identifier column ", id_column,
      " is also used as an outcome column."))
  }
  
  # End of checks for external_warn. Only strict remains.
  if (check_stringency == "external_warn") return()
  
  # Check whether the identifier column is erroneously included in the
  # signature.
  if (length(intersect(id_column, signature)) > 0) {
    stop(paste0(
      "The ", col_type, " identifier column ", id_column,
      " also appears in the signature."))
  }
  
  # Check whether the identifier column is erroneously included in the set of
  # features marked for exclusion.
  if (length(intersect(id_column, exclude_features)) > 0) {
    stop(paste0(
      "The ", col_type, " identifier column ", id_column, 
      " also appears among the features marked for exclusion."))
  }
  
  # Check whether the identifier column is erroneously included in the set of
  # features marked for inclusion.
  if (length(intersect(id_column, include_features)) > 0) {
    stop(paste0(
      "The ", col_type, " identifier column ", id_column,
      " also appears among the features marked for inclusion."))
  }
  
  return(invisible(TRUE))
}



#' Internal function to test plausibility of provided survival times.
#'
#' This function checks whether non-positive outcome time is present in the
#' data. This may produce unexpected results for some packages. For example,
#' glmnet will not train if an instance has a survival time of 0 or lower.
#'
#' @param data Data set as loaded using the `.load_data` function.
#' @inheritParams .parse_experiment_settings
#'
#' @return NULL
#' @md
#' @keywords internal
.check_survival_time_plausibility <- function(
    data,
    outcome_type,
    outcome_column,
    check_stringency = "strict"){
  
  if (outcome_type %in% c("survival", "competing_risk")) {
    
    # Find the levels in the data
    outcome_time <- unique_na(data[[outcome_column]])
    
    if (length(outcome_time) > 0) {
      if (any(outcome_time <= 0.0) &&
          check_stringency %in% c("strict", "external_warn")) {
        warning(paste0(
          "Survival data contain instances with non-positive (zero or negative) time. ",
          "Some packages that could be used during the analysis, such as glmnet, ",
          "will not be able to produce useful models."))
      }
    }
  }
  
  return(invisible(TRUE))
}



#' Internal function to test plausibility of provided class levels
#'
#' This function checks whether categorical levels are present in the data that
#' are not found in the user-provided class levels.
#'
#' @param data Data set as loaded using the `.load_data` function.
#' @inheritParams .parse_experiment_settings
#' @inheritParams as_data_object
#'
#' @return NULL
#' @md
#' @keywords internal
.check_class_level_plausibility <- function(
    data,
    outcome_type, 
    outcome_column,
    class_levels,
    check_stringency = "strict"){
  
  if (outcome_type %in% c("binomial", "multinomial") &&
      !is.null(class_levels)) {
    
    # Find the levels in the data
    unique_levels <- unique_na(data[[outcome_column]])
    
    # Find levels that are not provided
    missing_levels <- setdiff(unique_levels, class_levels)
    
    if (check_stringency %in% c("strict", "external_warn")) {
      if (length(missing_levels) > 0) {
        stop(paste0(
          "The outcome data contains levels that are not found among the provided class levels: ",
          paste_s(missing_levels)))
      }
    }
  }
  
  return(invisible(TRUE))
}



#' Internal function to check whether feature columns are found in the data
#' 
#' This function checks whether feature columns can be found in the data set.
#' It will raise an error if any feature columns are missing from the data set.
#'
#' @param data Data set as loaded using the `.load_data` function.
#' @param feature Character string(s) indicating one or more features.
#'
#' @return NULL
#' @md
#' @keywords internal
.check_feature_availability <- function(data, feature){
  # Check whether features are available in the data
  
  # Missing features are features that are not available in the provided data.
  missing_feature <- feature[!feature %in% colnames(data)]
  
  if (length(missing_feature) > 0) {
    stop(paste0(
      "One or more features could not be found in the data set: ",
      paste_s(missing_feature)))
  }
  
  return(invisible(TRUE))
}



.is_survival_status_col <- function(
    column_name,
    data,
    censoring_indicator = NULL,
    event_indicator = NULL,
    competing_risk_indicator = NULL){
  # Identify if the column could contain survival status information.
  
  # Find all indicators
  present_indicators <- c(
    censoring_indicator,
    event_indicator,
    competing_risk_indicator)
  
  if (!is.null(present_indicators)) {
    x <- data[[column_name]]
    x <- x[!is.na(x)]
    
    # Check if all data belong to the indicators.
    return(all(x %in% present_indicators))
    
  } else if (is.logical(data[[column_name]])) {
    return(TRUE)
    
  } else if (is.numeric(data[[column_name]])) {
    x <- data[[column_name]]
    x <- x[is.finite(x)]
    
    # Check if all finite values are either 0 or 1
    return((sum(x == 0) + sum(x == 1)) == length(x))
    
  }
  
  return(FALSE)
}



.is_survival_time_col <- function(
    column_name,
    data){
  # Identify if the column could contain survival time information.
  
  if (is.numeric(data[[column_name]])) {
    x <- data[[column_name]]
    x <- x[is.finite(x)]
    
    # Check if all finite values are 0 or positive
    return(all(x >= 0.0))
  }
  
  return(FALSE)
}

#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# test_train (generic) ---------------------------------------------------------
setGeneric("test_train", function(data, ...) standardGeneric("test_train"))



# test_train (data.table) ------------------------------------------------------
setMethod(
  "test_train",
  signature(data = "data.table"),
  function(
    data,
    data_bypass = NULL,
    learner,
    hyperparameter_list = list(),
    create_bootstrap = FALSE,
    ...) {
    if (!is.null(data_bypass)) {
      # Convert data_bypass to dataObject.
      data_bypass <- do.call(
        as_data_object,
        args = c(
          list("data" = data_bypass),
          list(...)))
    }

    # Convert data to dataObject.
    data <- do.call(
      as_data_object,
      args = c(
      list("data" = data),
      list(...)))

    return(do.call(
      test_train, 
      args = c(
        list(
          "data" = data,
          "data_bypass" = data_bypass,
          "learner" = learner,
          "hyperparameter_list" = hyperparameter_list,
          "create_bootstrap" = create_bootstrap),
        list(...))))
  }
)



# test_train (dataObject) ------------------------------------------------------
setMethod(
  "test_train",
  signature(data = "dataObject"),
  function(
    data,
    data_bypass = NULL,
    learner,
    hyperparameter_list = list(),
    create_bootstrap = FALSE,
    create_novelty_detector = FALSE,
    create_naive = FALSE,
    cl = NULL,
    trim_model = FALSE,
    ...) {
    # The bypass data allows for bypassing important aspects of the
    # pre-processing pipeline, e.g. the preprocessing checks. This enables
    # testing of very rare cases where preprocessing may run fine, but the
    # subsample does not allow for training.
    if (is.null(data_bypass)) data_bypass <- data

    # Prepare settings ---------------------------------------------------------

    # Reconstitute settings from the data.
    settings <- extract_settings_from_data(data_bypass)

    # Update some missing settings that can be fixed within this method.
    settings$data$train_cohorts <- unique(data_bypass@data[[get_id_columns(single_column = "batch")]])

    # Parse the remaining settings that are important. Remove outcome_type from
    # ... This prevents an error caused by multiple matching arguments.
    dots <- list(...)
    dots$parallel <- NULL
    dots$fs_method <- NULL
    dots$hyperparameter <- NULL

    # Create setting_hyperparam so that it can be parsed correctly.
    if (!learner %in% names(hyperparameter_list) && length(hyperparameter_list) > 0) {
      setting_hyperparam <- list()
      setting_hyperparam[[learner]] <- hyperparameter_list
    } else {
      setting_hyperparam <- hyperparameter_list
    }

    # Determine if a naive model should be forced.
    fs_method <- ifelse(create_naive, "no_features", "none")
    
    settings <- do.call(
      .parse_general_settings,
      args = c(
        list(
          "settings" = settings,
          "data" = data_bypass@data,
          "parallel" = FALSE,
          "fs_method" = fs_method,
          "learner" = learner,
          "hyperparameter" = setting_hyperparam),
        dots))
    
    # Push settings to the backend.
    .assign_settings_to_global(settings = settings)

    # Prepare outcome_info -----------------------------------------------------

    # Create a generic outcome object
    outcome_info <- data_bypass@outcome_info

    # Prepare featureInfo objects ----------------------------------------------

    # Create a list of featureInfo objects.
    feature_info_list <- .get_feature_info_data(
      data = data_bypass@data,
      file_paths = NULL,
      project_id = character(),
      outcome_type = settings$data$outcome_type)

    # Extract the generic data.
    feature_info_list <- feature_info_list[["generic"]]

    # Perform some pre-processing (i.e. remove singular features)
    feature_info_list <- .determine_preprocessing_parameters(
      cl = cl,
      data = data_bypass,
      feature_info_list = feature_info_list,
      settings = settings,
      verbose = FALSE)

    # Remove invariant features from the data
    data <- filter_features(
      data = data,
      available_features = get_available_features(
        feature_info_list = feature_info_list))

    # Find features that are required for processing the data
    required_features <- get_required_features(
      x = data,
      feature_info_list = feature_info_list)

    # Find important features, i.e. those that constitute the signature either
    # individually or as part of a cluster.
    model_features <- get_model_features(
      x = data,
      feature_info_list = feature_info_list)

    # Naive models do not require features.
    if (create_naive) required_features <- model_features <- NULL
    
    # Prepare hyperparameters --------------------------------------------------

    # Get default hyperparameters.
    param_list <- .get_preset_hyperparameters(
      data = data,
      learner = learner,
      names_only = FALSE)

    # Update with user-provided settings.
    param_list <- .update_hyperparameters(
      parameter_list = param_list,
      user_list = settings$mb$hyper_param[[learner]])

    # Determine which hyperparameters still need to be specified.
    unset_parameters <- sapply(
      param_list,
      function(hyperparameter_entry) hyperparameter_entry$randomise)

    # Raise an error if any hyperparameters were not set.
    if (any(unset_parameters)) {
      stop(paste0(
        "The following hyperparameters need to be specified: ",
        paste_s(names(unset_parameters)[unset_parameters])))
    }

    # Obtain the final list of hyperparameters.
    param_list <- lapply(
      param_list, 
      function(hyperparameter_entry) hyperparameter_entry$init_config)

    # Prepare model and data ---------------------------------------------------

    # Create familiar model
    object <- methods::new(
      "familiarModel",
      outcome_type = settings$data$outcome_type,
      learner = learner,
      fs_method = fs_method,
      hyperparameters = param_list,
      required_features = required_features,
      model_features = model_features,
      novelty_features = model_features,
      run_table = get_placeholder_run_table(),
      feature_info = feature_info_list,
      outcome_info = outcome_info,
      settings = settings$eval,
      project_id = 0)

    # Add package version/
    object <- add_package_version(object = object)

    # Process data.
    data <- process_input_data(
      object = object,
      data = data,
      stop_at = "clustering")

    # Create bootstraps.
    if (create_bootstrap) {
      data <- select_data_from_samples(
        data = data,
        samples = fam_sample(
          x = data@data,
          replace = TRUE))
    }

    # Train model.
    object <- .train(
      object = object,
      data = data,
      get_additional_info = TRUE,
      trim_model = trim_model,
      timeout = Inf)

    # Train novelty detector.
    object <- .train_novelty_detector(
      object = object,
      data = data,
      detector = ifelse(create_novelty_detector, "isolation_forest", "none"))

    # Generate a placeholder name for the familiarModel object
    object <- set_object_name(x = object)

    return(object)
  }
)



# test_train_novelty_detector (generic) ----------------------------------------
setGeneric("test_train_novelty_detector", function(data, ...) standardGeneric("test_train_novelty_detector"))



# test_train_novelty_detector (data.table) -------------------------------------
setMethod(
  "test_train_novelty_detector",
  signature(data = "data.table"),
  function(
    data, 
    data_bypass = NULL,
    detector,
    hyperparameter_list = list(),
    create_bootstrap = FALSE,
    ...) {
    if (!is.null(data_bypass)) {
      # Convert data_bypass to dataObject.
      data_bypass <- do.call(
        as_data_object,
        args = c(
          list("data" = data_bypass),
          list(...)))
    }

    # Convert data to dataObject.
    data <- do.call(
      as_data_object, 
      args = c(
        list("data" = data),
        list(...)))
    
    return(do.call(
      test_train_novelty_detector,
      args = c(
        list(
          "data" = data,
          "data_bypass" = data_bypass,
          "detector" = detector,
          "hyperparameter_list" = hyperparameter_list,
          "create_bootstrap" = create_bootstrap),
        list(...))))
  }
)


# test_train_novelty_detector (dataObject) -------------------------------------
setMethod(
  "test_train_novelty_detector",
  signature(data = "dataObject"),
  function(
    data,
    data_bypass = NULL,
    detector,
    hyperparameter_list = list(),
    create_bootstrap = FALSE,
    cl = NULL,
    trim_model = FALSE,
    ...) {
    # The bypass data allows for bypassing important aspects of the
    # pre-processing pipeline, e.g. the preprocessing checks. This enables
    # testing of very rare cases where preprocessing may run fine, but the
    # subsample does not allow for training.
    if (is.null(data_bypass)) data_bypass <- data

    # Prepare settings ---------------------------------------------------------

    # Reconstitute settings from the data.
    settings <- extract_settings_from_data(data_bypass)

    # Update some missing settings that can be fixed within this method.
    settings$data$train_cohorts <- unique(data_bypass@data[[get_id_columns(single_column = "batch")]])

    # Parse the remaining settings that are important. Remove outcome_type from
    # ... This prevents an error caused by multiple matching arguments.
    dots <- list(...)
    dots$parallel <- NULL
    dots$fs_method <- NULL
    dots$hyperparameter <- NULL

    # Create setting_hyperparam so that it can be parsed correctly.
    if (!detector %in% names(hyperparameter_list) && length(hyperparameter_list) > 0) {
      setting_hyperparam <- list()
      setting_hyperparam[[detector]] <- hyperparameter_list
    } else {
      setting_hyperparam <- hyperparameter_list
    }

    settings <- do.call(
      .parse_general_settings,
      args = c(
        list(
          "settings" = settings,
          "data" = data_bypass@data,
          "parallel" = FALSE,
          "fs_method" = "none",
          "learner" = "glm",
          "novelty_detector" = detector,
          "detector_parameters" = setting_hyperparam),
        dots))

    # Push settings to the backend.
    .assign_settings_to_global(settings = settings)

    # Prepare featureInfo objects ----------------------------------------------

    # Create a list of featureInfo objects.
    feature_info_list <- .get_feature_info_data(
      data = data_bypass@data,
      file_paths = NULL,
      project_id = character(),
      outcome_type = settings$data$outcome_type)

    # Extract the generic data.
    feature_info_list <- feature_info_list[["generic"]]

    # Perform some pre-processing (i.e. remove singular features)
    feature_info_list <- .determine_preprocessing_parameters(
      cl = cl,
      data = data_bypass,
      feature_info_list = feature_info_list,
      settings = settings,
      verbose = FALSE)

    # Remove invariant features from the data
    data <- filter_features(
      data = data,
      available_features = get_available_features(
        feature_info_list = feature_info_list)
    )

    # Find features that are required for processing the data.
    required_features <- get_required_features(
      x = data,
      feature_info_list = feature_info_list
    )

    # Find important features, i.e. those that constitute the signature either
    # individually or as part of a cluster.
    model_features <- get_model_features(
      x = data,
      feature_info_list = feature_info_list)

    # Prepare hyperparameters --------------------------------------------------

    # Get default hyperparameters.
    param_list <- .get_preset_hyperparameters(
      data = data,
      detector = detector,
      names_only = FALSE)

    # Update with user-provided settings.
    param_list <- .update_hyperparameters(
      parameter_list = param_list,
      user_list = settings$mb$detector_parameters[[detector]])

    # Determine which hyperparameters still need to be specified.
    unset_parameters <- sapply(
      param_list, 
      function(hyperparameter_entry) hyperparameter_entry$randomise)

    # Raise an error if any hyperparameters were not set.
    if (any(unset_parameters)) {
      stop(paste0(
        "The following hyperparameters need to be specified: ",
        paste_s(names(unset_parameters)[unset_parameters])))
    }

    # Obtain the final list of hyperparameters.
    param_list <- lapply(
      param_list, 
      function(hyperparameter_entry) hyperparameter_entry$init_config)

    # Prepare model and data ---------------------------------------------------

    # Create familiar model
    object <- methods::new(
      "familiarNoveltyDetector",
      learner = detector,
      hyperparameters = param_list,
      required_features = required_features,
      model_features = model_features,
      feature_info = feature_info_list,
      run_table = get_placeholder_run_table(),
      project_id = 0)

    # Add package version/
    object <- add_package_version(object = object)

    # Process data.
    data <- process_input_data(
      object = object,
      data = data,
      stop_at = "clustering")

    # Create bootstraps.
    if (create_bootstrap) {
      data <- select_data_from_samples(
        data = data,
        samples = fam_sample(
          x = data@data,
          replace = TRUE))
    }

    # Train model.
    object <- .train(
      object = object,
      data = data,
      get_additional_info = TRUE,
      trim_model = trim_model,
      timeout = Inf)

    return(object)
  }
)



get_placeholder_run_table <- function() {
  return(data.table::data.table(
    "run_id" = 1L,
    "data_id" = 1L, 
    "can_pre_process" = TRUE,
    "perturbation" = "main",
    "perturb_level" = 1))
}

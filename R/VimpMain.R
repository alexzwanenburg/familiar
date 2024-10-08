#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


# promote_vimp_method ----------------------------------------------------------
setMethod(
  "promote_vimp_method",
  signature(object = "familiarVimpMethod"),
  function(object) {
    # Extract vimp_method.
    method <- object@vimp_method

    if (method %in% .get_available_concordance_vimp_method()) {
      # Concordance-based methods.
      object <- methods::new("familiarConcordanceVimp", object)
    } else if (method %in% .get_available_univariate_mutual_information_vimp_method()) {
      # Mutual information maximisation.
      object <- methods::new("familiarUnivariateMutualInfoVimp", object)
    } else if (method %in% .get_available_multivariate_mutual_information_vimp_method()) {
      # Multivariate information methods.
      object <- methods::new("familiarMultivariateMutualInfoVimp", object)
    } else if (method %in% .get_available_correlation_vimp_methods()) {
      # Correlation-based methods.
      object <- methods::new("familiarCorrelationVimp", object)
    } else if (method %in% .get_available_corelearn_gini_vimp_method()) {
      # Gini measure.
      object <- methods::new("familiarCoreLearnGiniVimp", object)
    } else if (method %in% .get_available_corelearn_mdl_vimp_method()) {
      # MDL variable importance method.
      object <- methods::new("familiarCoreLearnMDLVimp", object)
    } else if (method %in% .get_available_corelearn_relieff_exp_rank_vimp_method()) {
      # ReliefF with exponentially decreasing rank.
      object <- methods::new("familiarCoreLearnRelieffExpRankVimp", object)
    } else if (method %in% .get_available_corelearn_gain_ratio_vimp_method()) {
      # Gain ratio measure.
      object <- methods::new("familiarCoreLearnGainRatioVimp", object)
    } else if (method %in% .get_available_univariate_regression_vimp_methods()) {
      # Univariate regression-based methods.
      object <- methods::new("familiarUnivariateRegressionVimp", object)
    } else if (method %in% .get_available_multivariate_regression_vimp_methods()) {
      # Multivariate regression-based methods.
      object <- methods::new("familiarMultivariateRegressionVimp", object)
    } else if (method %in% .get_available_glmnet_ridge_vimp_methods()) {
      # Ridge penalised regression model-based methods.

      # Create a familiarModel and promote to the right class.
      object <- methods::new(
        "familiarModel",
        fs_method = "none",
        learner = method,
        outcome_type = object@outcome_type,
        hyperparameters = object@hyperparameters,
        outcome_info = object@outcome_info,
        feature_info = object@feature_info,
        required_features = object@required_features,
        run_table = object@run_table,
        project_id = object@project_id)

      # Promote to the correct subclass.
      object <- promote_learner(object)
      
    } else if (method %in% .get_available_glmnet_lasso_vimp_methods()) {
      # Lasso penalised regression model-based methods.

      # Create a familiarModel and promote to the right class.
      object <- methods::new(
        "familiarModel",
        fs_method = "none",
        learner = method,
        outcome_type = object@outcome_type,
        hyperparameters = object@hyperparameters,
        outcome_info = object@outcome_info,
        feature_info = object@feature_info,
        required_features = object@required_features,
        run_table = object@run_table,
        project_id = object@project_id)

      # Promote to the correct subclass.
      object <- promote_learner(object)
      
    } else if (method %in% .get_available_glmnet_elastic_net_vimp_methods()) {
      # Elastic net penalised regression model-based methods.

      # Create a familiarModel and promote to the right class.
      object <- methods::new("familiarModel",
        fs_method = "none",
        learner = method,
        outcome_type = object@outcome_type,
        hyperparameters = object@hyperparameters,
        outcome_info = object@outcome_info,
        feature_info = object@feature_info,
        required_features = object@required_features,
        run_table = object@run_table,
        project_id = object@project_id)

      # Promote to the correct subclass.
      object <- promote_learner(object)
      
    } else if (method %in% .get_available_rfsrc_vimp_methods() ||
      method %in% .get_available_rfsrc_default_vimp_methods()) {
      # Random forest variable importance methods.

      # Create a familiarModel and promote to the right class.
      object <- methods::new("familiarModel",
        fs_method = "none",
        learner = ifelse(method %in% .get_available_rfsrc_vimp_methods(),
          "random_forest_rfsrc",
          "random_forest_rfsrc_default"
        ),
        outcome_type = object@outcome_type,
        hyperparameters = object@hyperparameters,
        outcome_info = object@outcome_info,
        feature_info = object@feature_info,
        required_features = object@required_features,
        run_table = object@run_table,
        project_id = object@project_id)

      # Promote to the correct subclass.
      object <- promote_learner(object)

      # Set the variable importance parameters for the method.
      object <- ..set_vimp_parameters(object, method = method)
      
    } else if (method %in% .get_available_ranger_vimp_methods() ||
      method %in% .get_available_ranger_default_vimp_methods()) {
      # Ranger random forest variable importance methods.

      # Create a familiarModel and promote to the right class.
      object <- methods::new("familiarModel",
        fs_method = "none",
        learner = ifelse(
          method %in% .get_available_ranger_vimp_methods(),
          "random_forest_ranger",
          "random_forest_ranger_default"),
        outcome_type = object@outcome_type,
        hyperparameters = object@hyperparameters,
        outcome_info = object@outcome_info,
        feature_info = object@feature_info,
        required_features = object@required_features,
        run_table = object@run_table,
        project_id = object@project_id)

      # Promote to the correct subclass.
      object <- promote_learner(object)

      # Set the variable importance parameters for the method.
      object <- ..set_vimp_parameters(object, method = method)
    } else if (method %in% .get_available_random_vimp_methods()) {
      # Random feature selection..
      object <- methods::new("familiarRandomVimp", object)
    } else if (method %in% .get_available_none_vimp_methods()) {
      # None feature selection methods - all features are equally
      # important.
      object <- methods::new("familiarNoneVimp", object)
    } else if (method %in% .get_available_signature_only_vimp_methods()) {
      # Signature only methods.
      object <- methods::new("familiarSignatureVimp", object)
    } else if (method %in% .get_available_no_features_vimp_methods()) {
      # No feature selection methods - no features are selected, leading
      # to naive models.
      object <- methods::new("familiarNoFeaturesVimp", object)
    }

    return(object)
  }
)



.get_vimp_hyperparameters <- function(
    data, 
    method, 
    outcome_type, 
    names_only = FALSE) {
  # Get the outcome type from the data object, if available
  if (!is.null(data)) outcome_type <- data@outcome_type

  # Create familiarModel
  vimp_method_object <- methods::new(
    "familiarVimpMethod",
    vimp_method = method,
    outcome_type = outcome_type)

  # Set up the specific model
  vimp_method_object <- promote_vimp_method(vimp_method_object)

  # Variable importance hyperparameters
  model_hyperparameters <- get_default_hyperparameters(vimp_method_object, data = data)

  # Extract names from parameter list
  if (names_only == TRUE) {
    model_hyperparameters <- names(model_hyperparameters)
  }

  # Return hyperparameter list, or names of parameters
  return(model_hyperparameters)
}



.check_vimp_outcome_type <- function(
    method, 
    outcome_type, 
    as_flag = FALSE) {
  # Create familiarModel
  vimp_method_object <- methods::new(
    "familiarVimpMethod",
    vimp_method = method,
    outcome_type = outcome_type)

  # Set up the specific model
  vimp_method_object <- promote_vimp_method(vimp_method_object)

  # Check validity.
  vimp_method_available <- is_available(vimp_method_object)

  if (as_flag) return(vimp_method_available)

  # Check if the vimp method or familiar model has been successfully promoted.
  if (!is_subclass(class(vimp_method_object)[1], "familiarVimpMethod") &&
      !is_subclass(class(vimp_method_object)[1], "familiarModel")) {
    stop(paste0(
      method, " is not a valid variable importance method. ",
      "Please check the vignette for available methods."))
  }

  if (!vimp_method_available) {
    stop(paste0(method, " is not available for \"", outcome_type, "\" outcomes."))
  }

  # Check that the required package can be loaded.
  require_package(
    x = vimp_method_object,
    purpose = paste0("to assess variable importance using the ", method, " method"),
    message_type = "backend_error")
}



# prepare_vimp_object (data.table) ---------------------------------------------
setMethod(
  "prepare_vimp_object",
  signature(data = "data.table"),
  function(
    data, 
    vimp_method, 
    vimp_method_parameter_list = list(),
    ...) {
    # Convert data to dataObject.
    data <- do.call(
      as_data_object, 
      args = c(
        list("data" = data),
        list(...)))
    
    return(do.call(
      prepare_vimp_object, 
      args = c(
        list(
          "data" = data,
          "vimp_method" = vimp_method,
          "vimp_method_parameter_list" = vimp_method_parameter_list),
        list(...))))
  }
)



# prepare_vimp_object (dataObject) ---------------------------------------------
setMethod(
  "prepare_vimp_object", signature(data = "dataObject"),
  function(data, vimp_method, vimp_method_parameter_list = list(), ...) {
    # This method is used within unit tests, but not by the main
    # familiar workflow.

    # Prepare setting ----------------------------------------------------------

    # Reconstitute settings from the data.
    settings <- extract_settings_from_data(data)

    # Update some missing settings that can be fixed within this method.
    settings$data$train_cohorts <- unique(data@data[[get_id_columns(single_column = "batch")]])

    # Parse the remaining settings that are important. Remove outcome_type from
    # ... This prevents an error caused by multiple matching arguments.
    dots <- list(...)
    dots$parallel <- NULL
    dots$fs_method <- NULL
    dots$fs_method_parameter <- NULL
    dots$learner <- NULL

    if (!is.null(dots$signature)) settings$data$signature <- dots$signature

    # Create setting_hyperparam so that it can be parsed correctly.
    if (!vimp_method %in% names(vimp_method_parameter_list) &&
        length(vimp_method_parameter_list) > 0) {
      setting_hyperparam <- list()
      setting_hyperparam[[vimp_method]] <- vimp_method_parameter_list
    } else {
      setting_hyperparam <- vimp_method_parameter_list
    }

    settings <- do.call(
      .parse_general_settings,
      args = c(
        list(
          "settings" = settings,
          "data" = data@data,
          "parallel" = FALSE,
          "fs_method" = vimp_method,
          "learner" = "glm",
          "fs_method_parameter" = setting_hyperparam),
        dots))

    # Push settings to the backend.
    .assign_settings_to_global(settings = settings)

    # Prepare featureInfo objects ----------------------------------------------

    # Create a list of featureInfo objects.
    feature_info_list <- .get_feature_info_data(
      data = data@data,
      file_paths = NULL,
      project_id = character(),
      outcome_type = settings$data$outcome_type)

    # Extract the generic data.
    feature_info_list <- feature_info_list[["generic"]]

    # Add signature info.
    feature_info_list <- add_signature_info(
      feature_info_list = feature_info_list,
      signature = settings$data$signature)

    # Perform some pre-processing (i.e. remove singular features)
    feature_info_list <- .determine_preprocessing_parameters(
      cl = NULL,
      data = data,
      feature_info_list = feature_info_list,
      settings = settings,
      verbose = FALSE)

    # Prepare hyperparameters --------------------------------------------------

    # Get default hyperparameters.
    param_list <- .get_preset_hyperparameters(
      data = data,
      fs_method = vimp_method,
      names_only = FALSE)

    # Update with user-provided settings.
    param_list <- .update_hyperparameters(
      parameter_list = param_list,
      user_list = settings$fs$param[[vimp_method]])

    # Determine which hyperparameters still need to be specified.
    unset_parameters <- sapply(
      param_list,
      function(hyperparameter_entry) hyperparameter_entry$randomise)

    # Mark sign-size as set, as it is not used for variable importance.
    if ("sign_size" %in% names(unset_parameters)) {
      unset_parameters["sign_size"] <- FALSE
    }

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

    # Prepare vimp object ------------------------------------------------------

    # Get required features.
    required_features <- get_required_features(
      x = data,
      feature_info_list = feature_info_list)

    # Create a familiar variable importance method.
    object <- methods::new(
      "familiarVimpMethod",
      outcome_type = settings$data$outcome_type,
      vimp_method = vimp_method,
      hyperparameters = param_list,
      outcome_info = data@outcome_info,
      feature_info = feature_info_list[required_features],
      required_features = required_features)

    # Promote object to correct subclass.
    object <- promote_vimp_method(object)

    # Return in list.
    return(object)
  }
)

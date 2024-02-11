#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



.is_available_prediction_type <- function(type) {
  return(type %in% c(.get_available_prediction_type_arguments()))
}



.get_available_prediction_type_arguments <- function() {
  return(c(
    .get_available_novelty_prediction_type_arguments(),
    "default", "survival_probability", "risk_stratification"
  ))
}



.get_available_novelty_prediction_type_arguments <- function() {
  return(c("novelty"))
}



# predict (generic) ------------------------------------------------------------

#' @title Model predictions for familiar models and model ensembles
#'
#' @description Fits the model or ensemble of models to the data and shows the
#'   result.
#'
#' @param object A familiar model or ensemble of models that should be used for
#'   prediction. This can also be a path to the ensemble model, one or more
#'   paths to models, or a list of models.
#' @param newdata Data to which the models are fitted. `familiar` performs
#'   checks on the data to ensure that all features required for fitting the
#'   model are present, and no additional levels are present in categorical
#'   features. Unlike other `predict` methods, `newdata` cannot be missing in
#'   `familiar`, as training data are not stored with the models.
#' @param type Type of prediction made. The following values are directly
#'   supported:
#'
#'  * `default`: Default prediction, i.e. value estimates for `continuous`
#'   outcomes, predicted class probabilities and class for `binomial` and
#'   `multinomial` and the model response for `survival` outcomes.
#'
#'  * `survival_probability`: Predicts survival probabilities at the time
#'   specified by `time`. Only applicable to `survival` outcomes. Some models
#'   may not allow for predicting survival probabilities based on their
#'   response.
#'
#'  * `novelty`: Predicts novelty of each sample, which can be used for
#'   out-of-distribution detection.
#'
#'  * `risk_stratification`: Predicts the strata to which the data belongs. Only
#'   for `survival` outcomes.
#'
#'   Other values for type are passed to the fitting method of the actual
#'   underlying model. For example for generalised linear models (`glm`) `type`
#'   can be `link`, `response` or `terms` as well. Some of these model-specific
#'   prediction types may fail to return results if the model has been trimmed.
#'
#' @param time Time at which the response (`default`) or survival probability
#'   (`survival_probability`) should be predicted for `survival` outcomes. Some
#'   models have a response that does not depend on `time`, e.g. `cox`, whereas
#'   others do, e.g. `random_forest`.
#' @param dir_path Path to the folder containing the models. Ensemble objects
#'   are stored with the models detached. In case the models were moved since
#'   creation, `dir_path` can be used to specify the current folder.
#'   Alternatively the `update_model_dir_path` method can be used to update the
#'   path.
#' @param stratification_method Selects the stratification method from which the
#'   threshold values should be selected. If the model or ensemble of models
#'   does not contain thresholds for the indicated method, an error is returned.
#'   In addition this argument is ignored if a `stratification_threshold` is
#'   set.
#' @param stratification_threshold Threshold value(s) used for stratifying
#'   instances into risk groups. If this parameter is specified,
#'   `stratification_method` and any threshold values that come with the model
#'   are ignored, and `stratification_threshold` is used instead.
#' @param percentiles Currently unused.
#' @param .as_prediction_table Selects whether a data.table or prediction table
#'   object should be returned.
#' @param ... to be documented.
#'
#' @inheritParams .extract_data
#'
#' @details This method is used to predict values for instances specified by the
#'   `newdata` using the model or ensemble of models specified by the `object`
#'   argument.
#'
#' @return A `data.table` with predicted values.
#' @exportMethod predict
#' @md
#' @rdname predict-methods
setGeneric("predict")



# predict (model) --------------------------------------------------------------

#' @rdname predict-methods
setMethod(
  "predict",
  signature(object = "familiarModel"),
  function(
    object,
    newdata,
    type = "default",
    time = NULL,
    dir_path = NULL,
    ensemble_method = "median",
    stratification_threshold = NULL,
    stratification_method = NULL,
    percentiles = NULL,
    .as_prediction_table = FALSE,
    ...
  ) {
    # Create ensemble.
    object <- as_familiar_ensemble(object = object)

    # Create predictions.
    predictions <- predict(
      object = object,
      newdata = newdata,
      type = type,
      time = time,
      dir_path = dir_path,
      ensemble_method = ensemble_method,
      stratification_threshold = stratification_threshold,
      stratification_method = stratification_method,
      percentiles = percentiles,
      .as_prediction_table = .as_prediction_table,
      ...
    )

    return(predictions)
  }
)



# predict (ensemble) -----------------------------------------------------------

#' @rdname predict-methods
setMethod(
  "predict",
  signature(object = "familiarEnsemble"),
  function(
    object,
    newdata,
    type = "default",
    time = NULL,
    dir_path = NULL,
    ensemble_method = "median",
    stratification_threshold = NULL,
    stratification_method = NULL,
    percentiles = NULL,
    .as_prediction_table = FALSE,
    ...
  ) {
    if (missing(newdata)) rlang::abort("newdata must be provided.")
    if (is_empty(newdata)) ..error_data_set_is_empty()

    # Make sure the ensemble model object is updated.
    object <- update_object(object = object)

    # Parse newdata to data object
    data <- as_data_object(
      data = newdata,
      object = object,
      check_stringency = "external"
    )

    # Propagate to .predict
    predictions <- .predict(
      object = object,
      data = data,
      type = type,
      time = time,
      dir_path = dir_path,
      ensemble_method = ensemble_method,
      stratification_threshold = stratification_threshold,
      stratification_method = stratification_method,
      percentiles = percentiles
    )
    
    if (is(predictions, "familiarDataElementPredictionTable")) {
      # Only return prediction columns for external predict calls.
      
      # Complete the object by adding any missing information, such as the
      # predicted class.
      predictions <- .complete(predictions)
      
      # Ensure that values are ordered the same as in the input data.
      predictions@data <- merge(
        x = data@data[, mget(get_id_columns())],
        y = predictions@data,
        on = c(familiar:::get_id_columns()),
        all.x = TRUE,
        sort = FALSE
      )
      
      # Isolate predicted values.
      if (!.as_prediction_table) {
        predictions <- .as_data_table(predictions)[, mget(predictions@value_column)]
      }
    }

    return(predictions)
  }
)


# predict (novelty detector) ---------------------------------------------------

#' @rdname predict-methods
setMethod(
  "predict",
  signature(object = "familiarNoveltyDetector"),
  function(
    object,
    newdata,
    type = "novelty",
    ensemble_method = "median",
    .as_prediction_table = FALSE,
    ...
  ) {
    # Create ensemble.
    object <- as_familiar_ensemble(object = object)
    
    # Create predictions.
    predictions <- predict(
      object = object,
      newdata = newdata,
      type = type,
      time = time,
      ensemble_method = ensemble_method,
      .as_prediction_table = .as_prediction_table,
      ...
    )

    return(predictions)
  }
)



# predict (list) ---------------------------------------------------------------
#' @rdname predict-methods
setMethod(
  "predict",
  signature(object = "list"),
  function(
    object,
    newdata,
    type = "default",
    time = NULL,
    dir_path = NULL,
    ensemble_method = "median",
    stratification_threshold = NULL,
    stratification_method = NULL,
    percentiles = NULL,
    .as_prediction_table = FALSE,
    ...
  ) {
    # Create ensemble.
    object <- as_familiar_ensemble(object = object)

    # Create predictions.
    predictions <- predict(
      object = object,
      newdata = newdata,
      type = type,
      time = time,
      dir_path = dir_path,
      ensemble_method = ensemble_method,
      stratification_threshold = stratification_threshold,
      stratification_method = stratification_method,
      percentiles = percentiles,
      .as_prediction_table = .as_prediction_table,
      ...
    )
    
    return(predictions)
  }
)



# predict (character) ----------------------------------------------------------

#' @rdname predict-methods
setMethod(
  "predict",
  signature(object = "character"),
  function(
    object,
    newdata,
    type = "default",
    time = NULL,
    dir_path = NULL,
    ensemble_method = "median",
    stratification_threshold = NULL,
    stratification_method = NULL,
    percentiles = NULL,
    .as_prediction_table = FALSE,
    ...
  ) {
    # Create ensemble.
    object <- as_familiar_ensemble(object = object)
    
    # Create predictions.
    predictions <- predict(
      object = object,
      newdata = newdata,
      type = type,
      time = time,
      dir_path = dir_path,
      ensemble_method = ensemble_method,
      stratification_threshold = stratification_threshold,
      stratification_method = stratification_method,
      percentiles = percentiles,
      .as_prediction_table = .as_prediction_table,
      ...
    )
    
    return(predictions)
  }
)


# .predict (ensemble) ----------------------------------------------------------
setMethod(
  ".predict",
  signature(object = "familiarEnsemble"),
  function(
    object,
    data,
    allow_recalibration = TRUE,
    is_pre_processed = FALSE,
    time = NULL,
    type = "default",
    dir_path = NULL,
    ensemble_method = "median",
    stratification_threshold = NULL,
    stratification_method = NULL,
    percentiles = NULL,
    ...,
    aggregate_results = TRUE
  ) {
    # Predict function for a model ensemble. This will always return ensemble
    # information, and not details corresponding to the individual models.

    # Test if the models are loaded, and load the models if they are not.
    object <- load_models(object, dir_path = dir_path)

    # Extract predictions for each individual model
    if (length(object@model_list) > 0) {
      predict_list <- lapply(
        object@model_list,
        .predict,
        data = data,
        allow_recalibration = allow_recalibration,
        is_pre_processed = is_pre_processed,
        time = time,
        type = type,
        ensemble_method = "median",
        stratification_threshold = stratification_threshold,
        stratification_method = stratification_method,
        percentiles = percentiles,
        ...
      )
      
    } else {
      # Generate a placeholder table.
      predict_list <- NULL
    }

    # Generate ensemble predictions
    if (all(type %in% .get_available_prediction_type_arguments())) {
      if (aggregate_results) {
        
        # Merge prediction data etc. slots into the data slot.
        predict_list <- lapply(predict_list, .merge_slots_into_data)
        
        # Compute ensemble values.
        predict_list <- .compute_data_element_estimates(
          x = predict_list,
          object = object
        )
        
        # Extract the merged object.
        predict_list <- predict_list[[1]]
      }
    }
    
    return(predict_list)
  }
)


# .predict (model) -------------------------------------------------------------
setMethod(
  ".predict",
  signature(object = "familiarModel"),
  function(
    object,
    data,
    allow_recalibration = TRUE,
    is_pre_processed = FALSE,
    time = NULL,
    type = "default",
    ensemble_method = "median",
    stratification_threshold = NULL,
    stratification_method = NULL,
    percentiles = NULL,
    ...
  ) {

    if (length(type) != 1) {
      ..error_reached_unreachable_code(paste0(
        "Only one type of prediction is expected as argument to .predict. ",
        "Found: ", paste_s(type)
      ))
    }
    
    # Prepare input data
    data <- process_input_data(
      object = object,
      data = data,
      is_pre_processed = is_pre_processed,
      stop_at = "clustering",
      keep_novelty = "novelty" %in% type
    )
    
    # user specified type ------------------------------------------------------
    if (any(!type %in% .get_available_prediction_type_arguments())) {
      # Select the first non-standard type.
      type <- setdiff(type, .get_available_prediction_type_arguments())[1]

      # Select only features used by the model.
      data <- select_features(
        data = data,
        features = features_after_clustering(
          features = object@model_features,
          feature_info_list = object@feature_info
        )
      )
      
      # Call predict from ..predict method with the given type and the
      # unspecified extra arguments in ... .
      return(..predict(
        object = object,
        data = data,
        time = time,
        type = type,
        ...
      ))
    }

    # novelty ------------------------------------------------------------------
    if (type == "novelty") {
      # Predict instance novelty.
      return(.predict_novelty(
          object = object,
          data = data
        ))
    }
    
    # Keep only model features in data for the remaining predictions.
    data <- select_features(
      data = data,
      features = features_after_clustering(
        features = object@model_features,
        feature_info_list = object@feature_info
      )
    )
    
    if (type == "default") {
      # default ----------------------------------------------------------------
      
      # Predict using the model and the standard type.
      prediction_table <- ..predict(
        object = object,
        data = data,
        type = type,
        time = time
      )
      
      # Recalibrate the default predictions.
      if (allow_recalibration) {
        prediction_table <- .apply_recalibration(
          object = object,
          prediction_table = prediction_table,
          data = data
        )
      }
      
    } else if (type == "survival_probability") {
      # survival probability ---------------------------------------------------
      
      # Predict using the model and the standard type.
      prediction_table <- ..predict(
        object = object,
        data = data,
        type = type,
        time = time
      )
      
    } else if (type == "risk_stratification") {
      # risk stratification ----------------------------------------------------
      
      # Prediction survival probabilities,
      prediction_table <- .predict_risk_stratification(
        object = object,
        data = data,
        time = time,
        stratification_threshold = stratification_threshold,
        stratification_method = stratification_method,
        ...
      )
      
    } else {
      ..error_reached_unreachable_code(paste0(
        "Type of prediction was not recognised: ", type
      ))
    }
  
    if (is(prediction_table, "familiarDataElementPredictionTable")) {
      prediction_table@detail_level <- "ensemble"
      prediction_table@percentiles <- percentiles
      prediction_table@ensemble_method <- ensemble_method
      prediction_table@estimation_type = ifelse(is.null(percentiles), "point", "bootstrap_confidence_interval")
    }
    
    return(prediction_table)
  }
)



# .predict (novelty detector) --------------------------------------------------
setMethod(
  ".predict",
  signature(object = "familiarNoveltyDetector"),
  function(
    object,
    data, 
    type = "novelty",
    is_pre_processed = FALSE,
    ...
  ) {
    # Prepare input data.
    data <- process_input_data(
      object = object,
      data = data,
      is_pre_processed = is_pre_processed,
      stop_at = "clustering"
    )
    
    # Predict novelty.
    prediction_table <- ..predict(
      object = object,
      data = data,
      type = type
    )

    return(prediction_table)
  }
)



# .predict (character) ---------------------------------------------------------
setMethod(
  ".predict",
  signature(object = "character"),
  function(object, data, ...) {
    # Load object.
    object <- load_familiar_object(object)

    return(do.call(
      .predict,
      args = c(
        list(
          "object" = object,
          "data" = data),
        list(...)
      )
    ))
  }
)



# .predict (list) --------------------------------------------------------------
setMethod(
  ".predict",
  signature(object = "list"),
  function(object, data, ...) {
    if (all(sapply(object, is, class2 = "familiarModel"))) {
      object <- as_familiar_ensemble(object = object)
      
      return(do.call(
        .predict,
        args = c(
          list(
            "object" = object,
            "data" = data
          ),
          list(...)
        )
      ))
      
    } else {
      ..error_reached_unreachable_code(".predict for list expects a list of familiarModel objects")
    }
  }
)



# .predict_novelty (model) -----------------------------------------------------
setMethod(
  ".predict_novelty",
  signature(object = "familiarModel"),
  function(
    object,
    data, 
    type = "novelty",
    is_pre_processed = FALSE,
    ...
  ) {
    return(.predict(
      object = object@novelty_detector,
      data = data,
      type = type,
      is_pre_processed = is_pre_processed
    ))
  }
)



# .predict_novelty (character) -------------------------------------------------
setMethod(
  ".predict_novelty",
  signature(object = "character"),
  function(object, ...) {
    # Load object.
    object <- load_familiar_object(object)

    return(do.call(
      .predict_novelty,
      args = c(
        list("object" = object),
        list(...)
      )
    ))
  }
)



# .predict_risk_stratification (model) -----------------------------------------
setMethod(
  ".predict_risk_stratification",
  signature(object = "familiarModel"),
  function(
    object,
    data,
    time, 
    stratification_threshold = NULL,
    stratification_method = NULL, 
    ...
  ) {
    # Only assess stratification for survival outcomes.
    if (!object@outcome_type %in% c("survival")) return(NULL)
    
    # Allow for settings the stratification threshold explicitly.
    if (is.null(stratification_threshold)) {
      if (!is.null(stratification_method)) {
        # Select threshold values for the given method.

        # First check if the method exists.
        if (!stratification_method %in% object@km_info$stratification_method) {
          stop(paste0(
            "Data for stratification method ", stratification_method,
            " was not found with the object."))
        }

        # Select stratification threshold for the given method.
        stratification_threshold <- object@km_info$parameters[[stratification_method]]$cutoff
      } else {
        # Select stratification threshold for the first method.
        stratification_threshold <- object@km_info$parameters[[1]]$cutoff
      }
    }

    # Check again if a stratification threshold is now set.
    if (is.null(stratification_threshold)) return(NULL)

    # Predict for the instances in data.
    predictions <- .predict(
      object = object,
      data = data,
      time = time
    )

    # Check for valid predictions,
    if (is_empty(predictions)) return(NULL)
    if (!any_predictions_valid(predictions)) return(NULL)

    # Find risk groups for each instance.
    assigned_risk_group <- .apply_risk_threshold(
      x = predictions,
      cutoff = stratification_threshold
    )

    # Convert to prediction table
    prediction_table <- as_prediction_table(
      x = assigned_risk_group,
      type = "risk_stratification",
      data = data,
      time = time
    )

    return(prediction_table)
  }
)


# .predict_risk_stratification (naive model) -----------------------------------
setMethod(
  ".predict_risk_stratification",
  signature(object = "familiarNaiveModel"),
  function(
    object,
    data,
    time,
    stratification_threshold = NULL,
    stratification_method = NULL,
    ...) {
    return(NULL)
  }
)



# .predict_risk_stratification (character) -------------------------------------
setMethod(
  ".predict_risk_stratification",
  signature(object = "character"),
  function(object, ...) {
    # Load object.
    object <- load_familiar_object(object)

    return(do.call(
      .predict_risk_stratification, 
      args = c(
        list("object" = object),
        list(...)
      )
    ))
  }
)



.get_available_ensemble_prediction_methods <- function() {
  return(c("median", "mean"))
}




# remove_missing_outcomes (ANY) ------------------------------------------------
setMethod(
  "remove_missing_outcomes",
  signature(data = "ANY"),
  function(data, outcome_type) {
    stop("remove_missing_outcomes is replaced by filter_missing_outcome")
  }
)

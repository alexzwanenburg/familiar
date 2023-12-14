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
#'  outcomes, predicted class probabilities and class for `binomial` and
#'  `multinomial` and the model response for `survival` outcomes.
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
#' @param ... to be documented.
#'
#' @inheritParams extract_data
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
    ...) {
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
      ...)

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
    ...
  ) {
    if (missing(newdata)) stop("newdata must be provided.")
    if (is_empty(newdata)) ..error_data_set_is_empty()

    # Make sure the ensemble model object is updated.
    object <- update_object(object = object)

    # Parse newdata to data object
    data <- as_data_object(
      data = newdata,
      object = object,
      check_stringency = "external")

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
    browser()
    if (is(predictions, "familiarDataElementPredictionTable")) {
      # Only return prediction columns for external predict calls.
      
      # Merge slots into data (this should have already happened).
      predictions <- .merge_slots_into_data(predictions)
      
      # Complete the prediction table (e.g., by predicting the majority class).
      # TODO: only affects classification tables at the moment.
      
      # Ensure that values are ordered the same as in the input data.
      # TODO: Use identifiers from data to order the predictions.
      
      # Isolate predicted values.
      predictions <- .as_data_table(predictions)[, mget(predictions@value_column)]
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
    ...) {
    if (missing(newdata)) stop("newdata must be provided.")
    if (is_empty(newdata)) ..error_data_set_is_empty()

    # Make sure the ensemble model object is updated.
    object <- update_object(object = object)

    # Parse newdata to data object
    data <- as_data_object(
      data = newdata,
      object = object,
      check_stringency = "external")

    # Propagate to .predict
    predictions <- .predict(
      object = object,
      data = data,
      type = type)

    if (type %in% .get_available_novelty_prediction_type_arguments()) {
      # Find non-feature columns.
      non_feature_columns <- get_non_feature_columns(object)
      prediction_columns <- setdiff(colnames(predictions), non_feature_columns)

      # Update the table with predictions by removing the non-feature columns.
      predictions <- data.table::copy(predictions[, mget(prediction_columns)])
    }

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

    # ensemble predictions -----------------------------------------------------
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
    # Suppress NOTES due to non-standard evaluation in data.table
    .NATURAL <- NULL
    
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
    
    if (type %in% c("default", "survival_probability")) {
      # default, survival_probability ------------------------------------------
      
      # Predict using the model and the standard type.
      prediction_table <- ..predict(
        object = object,
        data = data,
        type = type,
        time = time
      )

      # Recalibrate the predictions.
      if (allow_recalibration) {
        prediction_table <- .apply_recalibration(
          object = object,
          predictions = prediction_table
        )
      }
      
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
    ...) {
    # Prepare input data.
    data <- process_input_data(
      object = object,
      data = data,
      is_pre_processed = is_pre_processed,
      stop_at = "clustering")

    # Predict novelty.
    prediction_table <- ..predict(
      object = object,
      data = data,
      type = type)

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
      list(...))))
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
        list(...))))
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
    browser()
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

    # Generate an empty prediction table.
    prediction_table <- get_placeholder_prediction_table(
      object = object,
      data = data,
      type = "risk_stratification")

    # Check again if a stratification threshold is now set.
    if (is.null(stratification_threshold)) return(prediction_table)

    # Predict for the instances in data.
    predictions <- .predict(
      object = object,
      data = data,
      time = time)

    # Check for valid predictions,
    if (is_empty(predictions)) {
      return(prediction_table)
    }
    if (!any_predictions_valid(
      prediction_table = predictions,
      outcome_type = object@outcome_type)) {
      return(prediction_table)
    }

    # Find risk groups for each instance.
    assigned_risk_group <- .apply_risk_threshold(
      object = object,
      predicted_values = predictions$predicted_outcome,
      cutoff = stratification_threshold)

    # Set to prediction table.
    prediction_table[, "risk_group" := assigned_risk_group]

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
    # Only assess stratification for survival outcomes.
    if (!object@outcome_type %in% c("survival")) return(NULL)

    # Generate an empty prediction table.
    prediction_table <- get_placeholder_prediction_table(
      object = object,
      data = data,
      type = "risk_stratification")

    return(prediction_table)
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
        list(...))))
  }
)



.get_available_ensemble_prediction_methods <- function() {
  return(c("median", "mean"))
}



remove_nonvalid_predictions <- function(prediction_table, outcome_type) {
  # TODO: REMOVE THIS FUNCTION: it is replaced by remove_invalid_predictions.
  stop("This function has been replaced by remove_invalid_predictions.")
  
  # Suppress NOTES due to non-standard evaluation in data.table
  predicted_outcome <- predicted_class <- survival_probability <- risk_group <- NULL

  if (is_empty(prediction_table)) return(prediction_table)

  # Check predicted outcome columns.
  if (outcome_type %in% c("survival", "competing_risk")) {
    if ("predicted_outcome" %in% colnames(prediction_table)) {
      prediction_table <- prediction_table[is.finite(predicted_outcome), ]
    }
    if ("survival_probability" %in% colnames(prediction_table)) {
      prediction_table <- prediction_table[is.finite(survival_probability), ]
    }
    if ("risk_group" %in% colnames(prediction_table)) {
      prediction_table <- prediction_table[!is.na(risk_group), ]
    }
    
  } else if (outcome_type %in% c("continuous")) {
    prediction_table <- prediction_table[is.finite(predicted_outcome), ]
    
  } else if (outcome_type %in% c("binomial", "multinomial")) {
    # predicted_class may be absent (e.g. for ICE-tables), so we need check if
    # it exists.
    if ("predicted_class" %in% colnames(prediction_table)) {
      # Mask based on predicted_class.
      prediction_table <- prediction_table[!is.na(predicted_class), ]
      
    } else {
      # Mask probability columns.
      class_probability_columns <- grepl(
        pattern = "predicted_class_probability_",
        x = colnames(prediction_table))

      # Get the names of the probability columns.
      class_probability_columns <- colnames(prediction_table)[class_probability_columns]

      # Create mask for valid instances.
      instance_mask <- rep.int(TRUE, nrow(prediction_table))
      for (ii in class_probability_columns) {
        instance_mask <- instance_mask & is.finite(prediction_table[[ii]])
      }

      # Apply mask to the prediction table.
      prediction_table <- prediction_table[instance_mask, ]
    }
    
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }

  return(prediction_table)
}



# remove_missing_outcomes (data.table) -----------------------------------------
setMethod(
  "remove_missing_outcomes",
  signature(data = "data.table"),
  function(data, outcome_type) {
    stop("remove_missing_outcomes is replaced by filter_missing_outcome")
    
    # Suppress NOTES due to non-standard evaluation in data.table
    outcome <- outcome_time <- outcome_event <- NULL

    if (is_empty(data)) return(data)

    # Check predicted outcome columns.
    if (outcome_type %in% c("survival", "competing_risk")) {
      data <- data[is.finite(outcome_time) & !is.na(outcome_event), ]
    } else if (outcome_type %in% c("continuous")) {
      data <- data[is.finite(outcome), ]
    } else if (outcome_type %in% c("binomial", "multinomial")) {
      data <- data[!is.na(outcome), ]
    } else {
      ..error_no_known_outcome_type(outcome_type)
    }

    return(data)
  }
)


# remove_missing_outcomes (dataObject) -----------------------------------------
setMethod(
  "remove_missing_outcomes", 
  signature(data = "dataObject"),
  function(data, outcome_type = NULL) {
    if (is_empty(data)) return(data)

    if (is.null(outcome_type)) outcome_type <- data@outcome_type

    # Update data attribute
    data@data <- remove_missing_outcomes(
      data = data@data,
      outcome_type = outcome_type)

    return(data)
  }
)


# remove_missing_outcomes (ANY) ------------------------------------------------
setMethod(
  "remove_missing_outcomes",
  signature(data = "ANY"),
  function(data, outcome_type = NULL) {
    if (is_empty(data)) return(data)

    ..error_reached_unreachable_code(
      "remove_missing_outcomes,ANY: data does not have a known object class, nor is empty.")
  }
)

.set_recalibration <- function(
    object, 
    data, 
    time = NULL, 
    ...
) {
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- predicted_outcome <- NULL
  
  # Initial empty calibration list
  calibration_model_list <- list()

  # Predict the outcome using the current model.
  model_predictions <- .predict(
    object = object,
    data = data,
    allow_recalibration = FALSE,
    time = time
  )
  
  if (!all_predictions_valid(model_predictions)) return(NULL)
  
  # Convert to data.table.
  model_predictions <- .as_data_table(model_predictions)

  if (object@outcome_type %in% c("binomial", "multinomial")) {
    # Get class levels and class probability column names
    class_levels <- get_outcome_class_levels(x = object)

    # Build a logistic model on top of the predicted class probabilities for
    # each predicted class probability
    for (ii in seq_along(class_levels)) {
      # Set positive class flag
      model_predictions[, "positive_class" := outcome == class_levels[ii]]

      # Parse formula
      model_formula <- stats::reformulate(
        termlabels = class_levels[ii],
        response = "positive_class"
      )

      # Remove NA from the data.table
      model_predictions <- model_predictions[is.finite(get(class_levels[ii])), ]

      # If the prediction data table returns no or just one (valid) entry,
      # calibration is not possible.
      if (nrow(model_predictions) <= 1) return(NULL)

      # Generate the calibration model.
      calibration_model <- tryCatch(
        stats::glm(
          model_formula,
          data = model_predictions,
          family = stats::binomial(link = "logit")
        ),
        error = identity
      )

      # Check if the calibration model was created.
      if (inherits(calibration_model, "error")) calibration_model <- NULL

      # Create calibration model
      calibration_model_list[[class_levels[ii]]] <- calibration_model
    }
    
  } else if (object@outcome_type == "survival") {
    # Parse formula
    formula <- stats::reformulate(
      termlabels = "predicted_outcome",
      response = quote(survival::Surv(outcome_time, outcome_event))
    )
    
    # Remove NA from the table
    model_predictions <- model_predictions[is.finite(predicted_outcome)]

    # If the prediction data table returns no or just one (valid) entry,
    # calibration is not possible.
    if (nrow(model_predictions) <= 1) return(NULL)

    # Generate model
    model_control <- survival::coxph.control(iter.max = 100)
    calibration_model <- tryCatch(
      survival::coxph(
        formula,
        data = model_predictions,
        control = model_control,
        y = FALSE
      ),
      error = identity
    )
    
    # Check if the model trained at all.
    if (inherits(calibration_model, "error")) return(NULL)

    # Check if the model fitter converged in time.
    if (calibration_model$iter >= 100) return(NULL)

    # Store calibration model.
    calibration_model_list[[1]] <- calibration_model
  }

  # Return list of calibration models
  return(calibration_model_list)
}



.apply_recalibration <- function(object, prediction_table, data) {
  # Suppress NOTES due to non-standard evaluation in data.table
  prob_sum <- NULL

  # Return predictions if calibration models are missing
  if (is_empty(object@calibration_model)) return(prediction_table)

  # Return predictions if it is empty
  if (is_empty(prediction_table)) return(prediction_table)

  # Convert to data.table.
  predictions <- .as_data_table(prediction_table)
  
  if (object@outcome_type %in% c("binomial", "multinomial")) {
    class_levels <- get_outcome_class_levels(x = object)
    prediction_list <- list()
    
    # Iterate over the calibration model for each class and obtain the class
    # probabilities.
    for (ii in seq_along(class_levels)) {

      # Skip if no calibration model is provided.
      if (is.null(object@calibration_model[[class_levels[ii]]])) {
        prediction_list[[class_levels[ii]]] <- predictions[[class_levels[ii]]]
        next
      }

      # Predict calibrated probabilities using the calibration model for the
      # current column.
      prediction_list[[class_levels[ii]]] <- stats::predict.glm(
        object = object@calibration_model[[class_levels[ii]]],
        newdata = predictions,
        type = "response"
      )
    }
    
    # Convert list of class probabilities to a data.table.
    predictions <- data.table::as.data.table(prediction_list)
    
    # Normalise predicted probabilities to 1.0.
    predictions[, "prob_sum" := rowSums(.SD, na.rm = TRUE), .SDcols = class_levels]
    predictions[, (class_levels) := lapply(.SD, "/", prob_sum), .SDcols = class_levels]
    predictions[, "prob_sum" := NULL]

    # Create new prediction table object.
    prediction_table <- as_prediction_table(
      x = predictions,
      type = "classification",
      data = data,
      model_object = object
    )
    
  } else if (object@outcome_type == "survival") {
    # Predict cox PH relative risk
    predicted_outcome_value <- predict(
      object = object@calibration_model[[1]],
      newdata = predictions,
      type = "risk"
    )

    # Create new prediction table object.
    prediction_table <- as_prediction_table(
      x = predicted_outcome_value,
      type = "hazard_ratio",
      data = data,
      model_object = object
    )
  }

  return(prediction_table)
}

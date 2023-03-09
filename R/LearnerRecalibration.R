.set_recalibration <- function(object, data, time = NULL) {
  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- predicted_outcome <- NULL

  # Initial empty calibration list
  calibration_model_list <- list()

  # Predict the outcome using the current model.
  model_predictions <- .predict(
    object = object,
    data = data,
    allow_recalibration = FALSE,
    time = time)

  if (object@outcome_type %in% c("binomial", "multinomial")) {
    # Get class levels and class probability column names
    class_levels <- get_outcome_class_levels(x = object)
    class_probability_columns <- get_class_probability_name(x = class_levels)

    # Build a logistic model on top of the predicted class probabilities for
    # each predicted class probability
    for (ii in seq_along(class_probability_columns)) {
      # Select current probability column
      current_class_probability_column <- class_probability_columns[ii]

      # Set positive class flag
      model_predictions[, "positive_class" := outcome == class_levels[ii]]

      # Parse formula
      model_formula <- stats::reformulate(
        termlabels = current_class_probability_column,
        response = "positive_class")

      # Remove NA from the data.table
      model_predictions <- model_predictions[is.finite(get(current_class_probability_column)), ]

      # If the prediction data table returns no or just one (valid) entry,
      # calibration is not possible.
      if (nrow(model_predictions) <= 1) return(NULL)

      # Generate the calibration model.
      calibration_model <- tryCatch(
        stats::glm(model_formula,
          data = model_predictions,
          family = stats::binomial(link = "logit")),
        error = identity)

      # Check if the calibration model was created.
      if (inherits(calibration_model, "error")) calibration_model <- NULL

      # Create calibration model
      calibration_model_list[[class_levels[ii]]] <- calibration_model
    }
  } else if (object@outcome_type == "survival") {
    # Parse formula
    formula <- stats::reformulate(
      termlabels = "predicted_outcome",
      response = quote(survival::Surv(outcome_time, outcome_event)))

    # Remove NA from the table
    model_predictions <- model_predictions[is.finite(predicted_outcome)]

    # If the prediction data table returns no or just one (valid) entry,
    # calibration is not possible.
    if (nrow(model_predictions) <= 1) return(NULL)

    # Generate model
    model_control <- survival::coxph.control(iter.max = 100)
    calibration_model <- tryCatch(
      survival::coxph(formula,
        data = model_predictions,
        control = model_control,
        y = FALSE),
      error = identity)

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



learner.apply_calibration <- function(object, predictions){

  # Suppress NOTES due to non-standard evaluation in data.table
  prob_sum <- NULL
  

  # Return predictions if calibration models are missing
  if (is_empty(object@calibration_model)) return(predictions)

  # Return predictions if it is empty
  if (is_empty(predictions)) return(predictions)

  if (object@outcome_type %in% c("binomial", "multinomial")) {
    # Determine probability columns
    class_probability_columns <- get_class_probability_name(x = object)
    class_levels <- get_outcome_class_levels(x = object)

    # Iterate over calibration models and reconstruct the outcome data table
    for (ii in seq_along(class_probability_columns)) {
      # Get name of current probability column
      current_class_probability_column <- class_probability_columns[ii]

      # Skip if no calibration model is provided.
      if (is.null(object@calibration_model[[class_levels[ii]]])) next

      # Predict calibrated probabilities using the calibration model for the
      # current column.
      predicted_probability <- stats::predict.glm(
        object = object@calibration_model[[class_levels[ii]]],
        newdata = predictions,
        type = "response")

      # Replace column contents with predicted probabilities.
      predictions[, (current_class_probability_column) := predicted_probability]
    }

    # Normalise predicted probabilities to 1
    predictions[, "prob_sum" := rowSums(.SD, na.rm = TRUE), .SDcols = class_probability_columns]
    predictions[, (class_probability_columns) := lapply(.SD, "/", prob_sum), .SDcols = class_probability_columns]

    # Drop sum of probabilities
    predictions[, "prob_sum" := NULL]

    # Update predicted outcome with class with maximum predicted probability
    max_prob_class <- factor(
      x = class_levels[predictions[, max.col(.SD), .SDcols = class_probability_columns]],
      levels = class_levels)
    
    predictions[, "predicted_class" := max_prob_class]
    
  } else if (object@outcome_type == "survival") {
    # Predict cox PH relative risk
    predicted_outcome_value <- predict(
      object = object@calibration_model[[1]],
      newdata = predictions,
      type = "risk")

    # Replace in table
    predictions[, "predicted_outcome" := predicted_outcome_value]
  }

  return(predictions)
}

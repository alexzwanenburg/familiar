#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

# familiarNaiveModel objects ---------------------------------------------------
setClass(
  "familiarNaiveModel",
  contains = "familiarModel")

setClass(
  "familiarNaiveCoxModel",
  contains = "familiarNaiveModel")

setClass(
  "familiarNaiveSurvivalTimeModel",
  contains = "familiarNaiveModel")

setClass(
  "familiarNaiveCumulativeIncidenceModel",
  contains = "familiarNaiveModel")



# ..train (familiarNaiveModel) -------------------------------------------------
setMethod(
  "..train",
  signature(
    object = "familiarNaiveModel",
    data = "dataObject"),
  function(object, data, ...) {
    # Check if training data is ok.
    if (reason <- has_bad_training_data(
      object = object,
      data = data, 
      allow_no_features = TRUE)) {
      
      return(callNextMethod(object = .why_bad_training_data(
        object = object,
        reason = reason)))
    }

    # Check if hyperparameters are set.
    if (is.null(object@hyperparameters)) {
      return(callNextMethod(object = ..update_errors(
        object = object, 
        ..error_message_no_optimised_hyperparameters_available())))
    }

    if (object@outcome_type %in% c("continuous")) {
      model <- object@outcome_info@distribution
    } else if (object@outcome_type %in% c("binomial", "multinomial")) {
      model <- object@outcome_info@distribution
    } else if (object@outcome_type %in% c("survival")) {
      model <- object@outcome_info@distribution
    } else {
      ..error_outcome_type_not_implemented(object@outcome_type)
    }

    # Add model... well "model" really.
    object@model <- model

    # Set learner version
    object <- set_package_version(object)

    return(object)
  }
)



# ..predict (familiarNaiveModel) -----------------------------------------------
setMethod(
  "..predict",
  signature(
    object = "familiarNaiveModel",
    data = "dataObject"),
  function(object, data, ...) {
    # Get an empty prediction table.
    prediction_table <- get_placeholder_prediction_table(
      object = object,
      data = data,
      type = "default")

    if (object@outcome_type %in% c("binomial", "multinomial")) {
      # Get classes and their probabilities from the stored model data. In the
      # naive model, the probability of each class is its occurrence in the
      # development data.
      class_levels <- object@model$frequency$outcome
      class_probabilities <- object@model$frequency$count / object@model$n

      # Fill class probabilities.
      class_probability_columns <- get_class_probability_name(class_levels)
      for (ii in seq_along(class_probability_columns)) {
        prediction_table[, (class_probability_columns[ii]) := class_probabilities[ii]]
      }

      # Update predicted class based on provided probabilities.
      class_predictions <- class_levels[apply(
        prediction_table[, mget(class_probability_columns)], 1, which.max)]
      
      class_predictions <- factor(
        x = class_predictions,
        levels = get_outcome_class_levels(object))
      
      prediction_table[, "predicted_class" := class_predictions]
      
    } else if (object@outcome_type %in% c("continuous")) {
      # In the naive model, return the median value for regression problems.
      prediction_table[, "predicted_outcome" := object@model$median]
      
    } else {
      ..error_outcome_type_not_implemented(object@outcome_type)
    }

    return(prediction_table)
  }
)



# ..predict (familiarNaiveCoxModel) --------------------------------------------
setMethod(
  "..predict",
  signature(
    object = "familiarNaiveCoxModel",
    data = "dataObject"),
  function(object, data, ...) {
    # Get an empty prediction table.
    prediction_table <- get_placeholder_prediction_table(
      object = object,
      data = data,
      type = "default")

    if (object@outcome_type %in% c("survival")) {
      # For survival outcomes based on relative risks, predict the average risk,
      # i.e. 1.0.
      prediction_table[, "predicted_outcome" := 1.0]
      
    } else {
      ..error_outcome_type_not_implemented(object@outcome_type)
    }

    return(prediction_table)
  }
)



# ..predict (familiarNaiveSurvivalTimeModel) -----------------------------------
setMethod(
  "..predict", 
  signature(
    object = "familiarNaiveSurvivalTimeModel",
    data = "dataObject"),
  function(object, data, ...) {
    # Get an empty prediction table.
    prediction_table <- get_placeholder_prediction_table(
      object = object,
      data = data,
      type = "default")

    if (object@outcome_type %in% c("survival")) {
      # For survival outcomes based on survival times, predict the average
      # survival time.

      # Check if the median survival time was measured.
      if (any(object@model$survival_probability$survival_probability <= 0.5)) {
        # Interpolate the survival function to find the median survival time.
        survival_time <- suppressWarnings(
          stats::approx(
            x = object@model$survival_probability$survival_probability,
            y = object@model$survival_probability$time,
            xout = 0.5,
            method = "linear",
            rule = 2
          )$y)
        
      } else {
        # Use the last known time point.
        survival_time <- max(object@model$survival_probability$time)
      }

      # Set the survival time.
      prediction_table[, "predicted_outcome" := survival_time]
      
    } else {
      ..error_outcome_type_not_implemented(object@outcome_type)
    }

    return(prediction_table)
  }
)



# ..predict (familiarNaiveCumulativeIncidenceModel) ----------------------------
setMethod(
  "..predict",
  signature(
    object = "familiarNaiveCumulativeIncidenceModel",
    data = "dataObject"),
  function(object, data, time = NULL, ...) {
    # Get an empty prediction table.
    prediction_table <- get_placeholder_prediction_table(
      object = object,
      data = data,
      type = "default")

    if (object@outcome_type %in% c("survival")) {
      # For survival outcomes based on survival times, predict the average
      # survival time.

      # If time is not provided, set the time at the last observed event.
      if (is.null(time)) time <- object@model$event_fivenum$max

      # Approximate cumulative incidence.
      cumulative_incidence <- suppressWarnings(
        stats::approx(
          x = object@model$cumulative_incidence$time,
          y = object@model$cumulative_incidence$cumulative_incidence,
          xout = time,
          method = "linear",
          rule = 2
        )$y)

      # Compute the cumulative hazard at the indicated time point.
      prediction_table[, "predicted_outcome" := cumulative_incidence]
      
    } else {
      ..error_outcome_type_not_implemented(object@outcome_type)
    }

    return(prediction_table)
  }
)


# ..predict_survival_probability -----------------------------------------------
setMethod(
  "..predict_survival_probability",
  signature(
    object = "familiarNaiveModel",
    data = "dataObject"),
  function(object, data, time) {
    if (!object@outcome_type %in% c("survival")) {
      return(callNextMethod())
    }

    # If time is unset, read the max time stored by the model.
    if (is.null(time)) time <- object@settings$time_max

    # Prepare an empty table in case things go wrong.
    prediction_table <- get_placeholder_prediction_table(
      object = object,
      data = data,
      type = "survival_probability")

    # Check for several issues that prevent survival probabilities from being
    # predicted.
    if (!has_calibration_info(object)) return(prediction_table)

    # For naive models, survival probability is defined by the Kaplan-Meier
    # estimate of survival at the time point time.
    prediction_table[, "survival_probability" := ..survival_probability_relative_risk(
      object = object,
      relative_risk = rep_len(1.0, nrow(prediction_table)),
      time = time)]

    return(prediction_table)
  }
)



# get_prediction_type (familiarNaiveCoxModel) ----------------------------------
setMethod(
  "get_prediction_type",
  signature(object = "familiarNaiveCoxModel"),
  function(object, type = "default") {
    # Cox proportional hazards models predict relative risks.
    if (type == "default") {
      return("hazard_ratio")
    } else if (type == "survival_probability") {
      return("survival_probability")
    } else {
      ..error_reached_unreachable_code(
        "get_prediction_type,familiarNaiveCoxModel: unknown type")
    }
  }
)



# get_prediction_type (familiarNaiveSurvivalTimeModel) -------------------------
setMethod(
  "get_prediction_type", 
  signature(object = "familiarNaiveSurvivalTimeModel"),
  function(object, type = "default") {
    # These models predict an expected survival time by
    # default.
    if (type == "default") {
      return("expected_survival_time")
    } else if (type == "survival_probability") {
      return("survival_probability")
    } else {
      ..error_reached_unreachable_code(
        "get_prediction_type,familiarNaiveSurvivalTimeModel: unknown type")
    }
  }
)



# get_prediction_type (familiarNaiveCumulativeIncidenceModel) ------------------
setMethod(
  "get_prediction_type",
  signature(object = "familiarNaiveCumulativeIncidenceModel"),
  function(object, type = "default") {
    # These models predict an cumulative hazard by default.
    if (type == "default") {
      return("cumulative_hazard")
    } else if (type %in% c("survival_probability")) {
      return("survival_probability")
    } else {
      ..error_reached_unreachable_code(
        "get_prediction_type,familiarNaiveCumulativeIncidenceModel: unknown type")
    }
  }
)


# ..set_calibration_info -------------------------------------------------------
setMethod(
  "..set_calibration_info",
  signature(object = "familiarNaiveModel"),
  function(object, data) {
    # Check if calibration info already.
    if (has_calibration_info(object)) {
      return(object)
    }

    if (object@outcome_type == "survival") {
      # Determine baseline survival.
      object@calibration_info <- get_baseline_survival(data = data)
    } else {
      return(callNextMethod())
    }

    return(object)
  }
)



# show (familiarNaiveModel) ----------------------------------------------------
setMethod(
  "show",
  signature(object = "familiarNaiveModel"),
  function(object) {
    # Make sure the model object is updated.
    object <- update_object(object = object)

    if (!model_is_trained(object)) {
      cat(paste0(
        "A ", object@learner, " model (class: ", class(object)[1],
        ") that was not successfully trained (", .familiar_version_string(object),
        ").\n"))

      if (length(object@messages$warning) > 0) {
        condition_messages <- condition_summary(object@messages$warning)
        cat(paste0(
          "\nThe following ",
          ifelse(length(condition_messages) == 1, "warning was", "warnings were"),
          " generated while trying to train the model:\n",
          paste0(condition_messages, collapse = "\n"),
          "\n"))
      }

      if (length(object@messages$error) > 0) {
        condition_messages <- condition_summary(object@messages$error)
        cat(paste0(
          "\nThe following ",
          ifelse(length(condition_messages) == 1, "error was", "errors were"),
          " encountered while trying to train the model:\n",
          paste0(condition_messages, collapse = "\n"),
          "\n"))
      }
    }
    
    # Describe the learner and the version of familiar.
    message_str <- paste0(
      "A naive ", object@learner, " model (class: ", class(object)[1],
      "; ", .familiar_version_string(object), ")")
    
    # Describe the package(s), if any
    if (!is.null(object@package)) {
      message_str <- c(
        message_str,
        paste0(" trained using "),
        paste_s(mapply(
          ..message_package_version,
          x = object@package, 
          version = object@package_version)),
        ifelse(length(object@package) > 1, " packages", " package"))
    }
    
    # Complete message and write.
    message_str <- paste0(c(message_str, ".\n"), collapse = "")
    cat(message_str)
    
    cat(paste0("\n--------------- Model details ---------------\n"))
    
    # Model details
    if (object@is_trimmed) {
      cat(object@trimmed_function$show, sep = "\n")
    } else {
      show(object@model)
    }
    
    cat(paste0("\n---------------------------------------------\n"))
    
    # Outcome details
    cat("\nThe following outcome was modelled:\n")
    show(object@outcome_info)
    
    # Details concerning hyperparameters.
    cat("\nThe model was trained using the following hyperparameters:\n")
    invisible(lapply(
      names(object@hyperparameters),
      function(x, object) {
        cat(paste0("  ", x, ": ", object@hyperparameters[[x]], "\n"))
      },
      object = object))
    
    # Add note that naive models don't directly use hyperparameters.
    if (model_is_trained(object)) {
      cat("Note that the above hyperparameters are not directly used by the naive model.\n")
    }
    
    if (length(object@messages$warning) > 0 || length(object@messages$error) > 0) {
      cat(paste0("\n------------ Warnings and errors ------------\n"))
      
      if (length(object@messages$warning) > 0) {
        condition_messages <- condition_summary(object@messages$warning)
        cat(paste0(
          "\nThe following ",
          ifelse(length(condition_messages) == 1, "warning was", "warnings were"),
          " generated while training the model:\n",
          paste0(condition_messages, collapse = "\n")))
      }
      
      if (length(object@messages$error) > 0) {
        condition_messages <- condition_summary(object@messages$error)
        cat(paste0(
          "\nThe following ",
          ifelse(length(condition_messages) == 1, "error was", "errors were"),
          " encountered while training the model:\n",
          paste0(condition_messages, collapse = "\n")))
      }
    }
    
    # Check package version.
    check_package_version(object)
  }
)

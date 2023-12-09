#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

# familiarCoxPH object ---------------------------------------------------------
setClass("familiarCoxPH",
  contains = "familiarModel",
  slots = list("encoding_reference_table" = "ANY"),
  prototype = list("encoding_reference_table" = NULL)
)



# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "familiarCoxPH"),
  function(.Object, ...) {
    # Update with parent class first.
    .Object <- callNextMethod()

    # Set the required package
    .Object@package <- "survival"

    return(.Object)
  }
)



# is_available -----------------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarCoxPH"),
  function(object, ...) {
    # CoxPH is only available if for survival outcomes.
    return(object@outcome_type == "survival")
  }
)



# get_default_hyperparameters --------------------------------------------------
setMethod(
  "get_default_hyperparameters",
  signature(object = "familiarCoxPH"),
  function(object, data = NULL, ...) {
    # Initialise list and declare hyperparameter entries
    param <- list()
    param$sign_size <- list()

    # If data is explicitly NULL, return the list with hyperparameter names only
    if (is.null(data)) return(param)

    ## signature size ----------------------------------------------------------
    param$sign_size <- .get_default_sign_size(data = data, restrict_samples = TRUE)

    return(param)
  }
)



# get_prediction_type ----------------------------------------------------------
setMethod(
  "get_prediction_type",
  signature(object = "familiarCoxPH"),
  function(object, type = "default") {
    # Cox proportional hazards models predict relative risks
    if (type == "default") {
      return("hazard_ratio")
      
    } else if (type == "survival_probability") {
      return("survival_probability")
      
    } else {
      ..error_reached_unreachable_code(
        "get_prediction_type,familiarCoxPH: unknown type")
    }
  }
)



# ..train ----------------------------------------------------------------------
setMethod(
  "..train",
  signature(
    object = "familiarCoxPH",
    data = "dataObject"),
  function(object, data, ...) {
    # Check if training data is ok.
    if (reason <- has_bad_training_data(object = object, data = data)) {
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

    # Check that required packages are loaded and installed.
    require_package(object, "train")

    # Use effect coding to convert categorical data into encoded data - this is
    # required to deal with factors with missing/new levels between training and
    # test data sets.
    encoded_data <- encode_categorical_variables(
      data = data,
      object = object,
      encoding_method = "dummy",
      drop_levels = FALSE)

    # Find feature columns in the data.
    feature_columns <- get_feature_columns(x = encoded_data$encoded_data)

    # Parse formula
    formula <- stats::reformulate(
      termlabels = feature_columns,
      response = quote(survival::Surv(outcome_time, outcome_event)))

    # Generate model.
    model_control <- survival::coxph.control(iter.max = 100)

    # Train the model
    model <- do.call_with_handlers(
      survival::coxph,
      args = list(
        formula,
        "data" = encoded_data$encoded_data@data,
        "control" = model_control,
        "y" = FALSE))

    # Extract values.
    object <- ..update_warnings(object = object, model$warning)
    object <- ..update_errors(object = object, model$error)
    model <- model$value

    # Check if the model trained at all.
    if (!is.null(object@messages$error)) {
      return(callNextMethod(object = object))
    }

    # Check if the model fitter converged in time.
    if (model$iter >= 100) {
      return(callNextMethod(object = ..update_errors(
        object = object,
        "Model fitter ran out of iterations and did not converge.")))
    }

    # Check if all coefficients could not be estimated. Sometimes models could
    # be trained with a large number of features whose coefficients fail to
    # converge. This would sometimes lead to overly large signature sizes being
    # selected during hyperparameter optimisation, especially in situations
    # where there is not a lot of signal. Checking for non-finite coefficients
    # is an easy way to figure out if the model is not properly trained.
    if (any(!sapply(stats::coef(model), is.finite))) {
      return(callNextMethod(object = ..update_errors(
        object = object,
        ..error_message_failed_model_coefficient_estimation())))
    }

    # Add model
    object@model <- model

    # Add the contrast references to model_list
    object@encoding_reference_table <- encoded_data$reference_table

    # Set learner version
    object <- set_package_version(object)

    return(object)
  }
)



# ..train_naive ----------------------------------------------------------------
setMethod(
  "..train_naive",
  signature(
    object = "familiarCoxPH",
    data = "dataObject"),
  function(object, data, ...) {
    # Turn into a Naive model.
    object <- methods::new("familiarNaiveCoxModel", object)

    return(..train(
      object = object,
      data = data,
      ...))
  }
)



# ..predict --------------------------------------------------------------------
setMethod(
  "..predict",
  signature(
    object = "familiarCoxPH",
    data = "dataObject"),
  function(object, data, type = "default", ...) {
    # Check that required packages are loaded and installed.
    require_package(object, "predict")

    if (type == "default") {
      # Default method --------------------------------------------------------

      # Check if the model was trained.
      if (!model_is_trained(object)) {
        return(callNextMethod())
      }

      # Check if the data is empty.
      if (is_empty(data)) {
        return(callNextMethod())
      }

      # Encode data so that the features are the same as in the training.
      encoded_data <- encode_categorical_variables(
        data = data,
        object = object,
        encoding_method = "dummy",
        drop_levels = FALSE)

      # Use the model for prediction.
      model_predictions <- predict(
        object = object@model,
        newdata = encoded_data$encoded_data@data,
        type = "risk")
      
      # Store as prediction table.
      prediction_table <- as_prediction_table(
        x = model_predictions,
        type = "hazard_ratio",
        data = data
      )
      
      return(prediction_table)
      
    } else {
      # User-specified method --------------------------------------------------

      # Check if the model was trained.
      if (!model_is_trained(object)) {
        return(NULL)
      }

      # Check if the data is empty.
      if (is_empty(data)) {
        return(NULL)
      }

      # Encode data so that the features are the same as in the training.
      encoded_data <- encode_categorical_variables(
        data = data,
        object = object,
        encoding_method = "dummy",
        drop_levels = FALSE)

      return(predict(
        object = object@model,
        newdata = encoded_data$encoded_data@data,
        type = type,
        ...))
    }
  }
)



# ..predict_survival_probability -----------------------------------------------
setMethod(
  "..predict_survival_probability",
  signature(
    object = "familiarCoxPH",
    data = "dataObject"),
  function(object, data, time, ...) {
    if (object@outcome_type != "survival") {
      return(callNextMethod())
    }

    # If time is unset, read the max time stored by the model.
    if (is.null(time)) time <- object@settings$time_max

    # Check that required packages are loaded and installed.
    require_package(object, "predict")

    return(.survival_probability_relative_risk(
      object = object,
      data = data,
      time = time))
  }
)



#  ..vimp ----------------------------------------------------------------------
setMethod(
  "..vimp",
  signature(object = "familiarCoxPH"),
  function(object, ...) {
    if (!model_is_trained(object)) {
      return(callNextMethod())
    }

    # Check that required packages are loaded and installed.
    require_package(object, "vimp")

    # Define p-values
    coefficient_z_values <- tryCatch(
      .compute_z_statistic(object),
      error = identity)

    if (inherits(coefficient_z_values, "error")) {
      return(callNextMethod())
    }

    # Remove any coefficients for the intercept.
    coefficient_z_values <- coefficient_z_values[names(coefficient_z_values) != "(Intercept)"]

    if (length(coefficient_z_values) == 0) {
      return(callNextMethod())
    }

    # Create variable importance object.
    vimp_object <- methods::new("vimpTable",
      vimp_table = data.table::data.table(
        "score" = abs(coefficient_z_values),
        "name" = names(coefficient_z_values)),
      encoding_table = object@encoding_reference_table,
      score_aggregation = "max",
      invert = TRUE)

    return(vimp_object)
  }
)



# ..set_calibration_info -------------------------------------------------------
setMethod(
  "..set_calibration_info",
  signature(object = "familiarCoxPH"),
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



# .trim_model ------------------------------------------------------------------
setMethod(
  ".trim_model",
  signature(object = "familiarCoxPH"),
  function(object, ...) {
    # Update model by removing the call.
    object@model$call <- call("trimmed")

    # Add show.
    object <- .capture_show(object)

    # Remove .Environment.
    object@model$terms <- .replace_environment(object@model$terms)
    object@model$formula <- .replace_environment(object@model$formula)

    # Remove elements that contain sample-specific values.
    object@model$linear.predictors <- NULL
    object@model$residuals <- NULL

    # Set is_trimmed to TRUE.
    object@is_trimmed <- TRUE

    # Default method for models that lack a more specific method.
    return(object)
  }
)



.get_available_cox_learners <- function(show_general = TRUE) {
  # Learners
  learners <- c("cox")
  
  return(learners)
}

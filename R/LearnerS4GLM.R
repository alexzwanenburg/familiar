#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


# familiarGLM object -----------------------------------------------------------
setClass("familiarGLM",
  contains = "familiarModel",
  slots = list(
    "encoding_reference_table" = "ANY",
    "feature_order" = "character"),
  prototype = list(
    "encoding_reference_table" = NULL,
    "feature_order" = character())
)

# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "familiarGLM"),
  function(.Object, ...) {
    # Update with parent class first.
    .Object <- callNextMethod()

    if (.Object@outcome_type == "multinomial") {
      .Object@package <- "nnet"
    } else if (.Object@outcome_type == "survival") {
      .Object@package <- "survival"
    } else {
      # The default option is to use "fastglm::fastglm" for speed, but if this
      # package is not available, switch to the "stats::glm" backup option.
      .Object@package <- if (is_package_installed("fastglm")) c("stats", "fastglm") else "stats"
    }

    return(.Object)
  }
)



# is_available -----------------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarGLM"),
  function(object, ...) {
    # Extract outcome type and learner from the model object.
    outcome_type <- object@outcome_type
    learner <- object@learner

    # Check outcome type and learner.
    if (
      outcome_type == "binomial" &&
      learner %in% c("glm", "glm_logistic", "glm_probit", "glm_cauchy", "glm_loglog")) {
      return(TRUE)
      
    } else if (
      outcome_type == "multinomial" &&
      learner %in% c("glm", "glm_multinomial")) {
      return(TRUE)
      
    } else if (
      outcome_type == "continuous" &&
      learner %in% c(
        "glm", "glm_log", "glm_gaussian", "glm_log_gaussian",
        "glm_inv_gaussian", "glm_poisson", "glm_log_poisson")) {
      return(TRUE)
      
    } else if (
      outcome_type == "survival" &&
      learner %in% c("glm")) {
      return(TRUE)
      
    } else if (
      outcome_type == "count" &&
      learner %in% c("glm", "glm_poisson", "glm_log_poisson")) {
      ..deprecation_count()
      return(TRUE)
    }
    
    return(FALSE)
  }
)



# get_default_hyperparameters --------------------------------------------------
setMethod(
  "get_default_hyperparameters",
  signature(object = "familiarGLM"),
  function(object, data = NULL, user_list = NULL, ...) {
    # Initialise list and declare hyperparameter entries
    param <- list()
    param$sign_size <- list()
    param$family <- list()
    param$sample_weighting <- list()
    param$sample_weighting_beta <- list()

    # If no data object is not provided, return the list with hyperparameter
    # names only
    if (is.null(data)) return(param)

    # Get the outcome type
    outcome_type <- data@outcome_type

    # signature size -----------------------------------------------------------
    param$sign_size <- .get_default_sign_size(
      data = data, 
      restrict_samples = TRUE)

    # model family -------------------------------------------------------------

    # Read family string by parsing the learner.
    fam <- sub(
      x = object@learner,
      pattern = "glm",
      replacement = "",
      fixed = TRUE)
    if (fam != "") {
      fam <- sub(
        x = fam,
        pattern = "_",
        replacement = "",
        fixed = TRUE)
    }

    # Determine the family or families.
    if (fam == "") {
      # If no family is specified, the default behaviour is to identify the
      # family through optimisation.
      if (outcome_type == "binomial") {
        family_default <- c("logistic", "probit", "loglog", "cauchy")
      } else if (outcome_type == "continuous") {
        family_default <- c("gaussian", "log_gaussian", "inv_gaussian", "poisson", "log_poisson")
      } else if (outcome_type == "count") {
        family_default <- c("poisson", "log_poisson")
      } else if (outcome_type == "multinomial") {
        family_default <- "multinomial"
      } else if (outcome_type == "survival") {
        family_default <- "cox"
      } else {
        ..error_outcome_type_not_implemented(outcome_type)
      }
      
    } else if (fam == "log") {
      # "log" is a collection of different families, that should be specified
      # according to the outcome type.
      if (outcome_type == "continuous") {
        family_default <- c("log_gaussian", "log_poisson")
      } else if (outcome_type == "count") {
        family_default <- "log_poisson"
      }
      
    } else {
      # A family was unambiguously specified.
      family_default <- fam
    }

    # Set family parameter.
    param$family <- .set_hyperparameter(
      default = family_default,
      type = "factor",
      range = family_default,
      randomise = ifelse(length(family_default) > 1, TRUE, FALSE))

    # sample weighting method --------------------------------------------------
    # Class imbalances may lead to learning majority classes. This can be
    # partially mitigated by increasing weight of minority classes.
    param$sample_weighting <- .get_default_sample_weighting_method(
      outcome_type = outcome_type)

    # effective number of samples beta -----------------------------------------
    # Specifies the beta parameter for effective number sample weighting method.
    # See Cui et al. (2019).
    param$sample_weighting_beta <- .get_default_sample_weighting_beta(
      method = c(
        param$sample_weighting$init_config,
        user_list$sample_weighting),
      outcome_type = outcome_type)

    return(param)
  }
)



# get_prediction_type ----------------------------------------------------------
setMethod(
  "get_prediction_type",
  signature(object = "familiarGLM"),
  function(object, type = "default") {
    if (object@outcome_type != "survival") {
      return(callNextMethod())
    }

    # This is a backup in case glm is used to refer to CoxPH methods.
    if (type == "default") {
      return("hazard_ratio")
    } else if (type == "survival_probability") {
      return("survival_probability")
    } else {
      ..error_reached_unreachable_code("get_prediction_type,familiarGLM: unknown type")
    }
  }
)



# ..train ----------------------------------------------------------------------
setMethod(
  "..train",
  signature(
    object = "familiarGLM",
    data = "dataObject"),
  function(object, data, approximate = FALSE, ...) {
    # For survival outcomes, switch to familiarCoxPH.
    if (object@outcome_type == "survival") {
      # Create a familiarCoxPH object.
      object <- methods::new("familiarCoxPH", object)

      return(..train(
        object = object,
        data = data,
        ...))
    }

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
      response = quote(outcome))

    # Get family for glm, which determines how the response and predictors are
    # linked.
    family <- ..get_distribution_family(object)

    # Set weights
    weights <- create_instance_weights(
      data = encoded_data$encoded_data,
      method = object@hyperparameters$sample_weighting,
      beta = ..compute_effective_number_of_samples_beta(
        object@hyperparameters$sample_weighting_beta),
      normalisation = "average_one")
    
    if (object@outcome_type %in% c("binomial", "continuous", "count")) {
      # Faster implementation using fastglm. We keep the stats::glm
      # implementation in case fastglm disappears from CRAN at some point.

      version <- ifelse(
        require_package("fastglm", message_type = "silent"),
        "fastglm",
        "stats")

      if (version == "fastglm") {
        outcome_data <- encoded_data$encoded_data@data$outcome
        if (object@outcome_type == "binomial") {
          # Convert levels to [0, 1] range.
          outcome_data <- as.numeric(outcome_data) - 1
        }

        # Add intercept.
        encoded_data$encoded_data@data[, "intercept__" := 1.0]

        # Fit using fastglm using the LDLT Cholesky method.
        model <- do.call_with_handlers(
          fastglm::fastglm,
          args = list(
            "x" = as.matrix(encoded_data$encoded_data@data[, mget(c(feature_columns, "intercept__"))]),
            "y" = matrix(outcome_data, ncol = 1L),
            "weights" = weights,
            "family" = family,
            "method" = 3L))
        
      } else {
        model <- do.call_with_handlers(
          stats::glm,
          args = list(formula,
            "data" = encoded_data$encoded_data@data,
            "weights" = weights,
            "family" = family,
            "model" = FALSE,
            "x" = FALSE,
            "y" = FALSE))
      }
    } else if (object@outcome_type == "multinomial") {
      max_iterations <- ifelse(approximate, 100L, 500L)
      absolute_tolerance <- ifelse(approximate, 1.0e-2, 1.0e-4)
      relative_tolerance <- ifelse(approximate, 1.0e-2, 1.0e-8)

      # Fit multinomial logistic models using nnet::multinom.
      model <- do.call_with_handlers(
        nnet::multinom,
        args = list(formula,
          "data" = encoded_data$encoded_data@data,
          "weights" = weights,
          "maxit" = max_iterations,
          "abstol" = absolute_tolerance,
          "reltol" = relative_tolerance,
          "MaxNWts" = Inf))
      
    } else {
      ..error_reached_unreachable_code(paste0(
        "..train,familiarGLM: unknown outcome type: ",
        object@outcome_type))
    }

    # Extract values.
    object <- ..update_warnings(object = object, model$warning)
    object <- ..update_errors(object = object, model$error)
    model <- model$value

    # Check if the model trained at all.
    if (!is.null(object@messages$error)) {
      return(callNextMethod(object = object))
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

    # Add feature order
    object@feature_order <- feature_columns

    # Set learner version
    object <- set_package_version(object)

    return(object)
  }
)



# ..train_naive ----------------------------------------------------------------
setMethod(
  "..train_naive",
  signature(
    object = "familiarGLM",
    data = "dataObject"),
  function(object, data, ...) {
    # For survival outcomes, switch to familiarCoxPH.
    if (object@outcome_type == "survival") {
      # Create a familiarCoxPH object.
      object <- methods::new("familiarCoxPH", object)

      return(..train_naive(
        object = object,
        data = data,
        ...))
    }

    # Turn into a Naive model.
    object <- methods::new("familiarNaiveModel", object)

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
    object = "familiarGLM",
    data = "dataObject"),
  function(object, data, type = "default", ...) {
    # Check that required packages are loaded and installed.
    require_package(object, "predict")

    if (type == "default") {
      # Default method ---------------------------------------------------------

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

      # Add intercept variable, because by default, fastglm does not fit
      # an intercept.
      if (inherits(object@model, "fastglm")) {
        encoded_data$encoded_data@data[, "intercept__" := 1.0]
      }

      # Get an empty prediction table.
      prediction_table <- get_placeholder_prediction_table(
        object = object,
        data = encoded_data$encoded_data,
        type = type)

      if (object@outcome_type == "binomial") {
        ## Binomial outcomes ---------------------------------------------------

        if (inherits(object@model, "fastglm")) {
          # For fastglm::fastglm models.
          model_predictions <- suppressWarnings(predict(
            object = object@model,
            newdata = as.matrix(
              encoded_data$encoded_data@data[, mget(c(object@feature_order, "intercept__"))]),
            type = "response"))
          
        } else {
          # For stats::glm models.
          model_predictions <- suppressWarnings(predict(
            object = object@model,
            newdata = encoded_data$encoded_data@data,
            type = "response"))
        }
        
        # Obtain class levels.
        class_levels <- get_outcome_class_levels(x = object)

        # Add class probabilities (glm always gives probability for the
        # second class).
        class_probability_columns <- get_class_probability_name(x = object)
        prediction_table[, (class_probability_columns[1]) := 1.0 - model_predictions]
        prediction_table[, (class_probability_columns[2]) := model_predictions]

        # Update predicted class based on provided probabilities.
        class_predictions <- class_levels[apply(prediction_table[, mget(class_probability_columns)], 1, which.max)]
        class_predictions <- factor(class_predictions, levels = class_levels)
        prediction_table[, "predicted_class" := class_predictions]
        
      } else if (object@outcome_type == "multinomial") {
        ## Multinomial outcomes ------------------------------------------------

        if (inherits(object@model, "nnet")) {
          # For nnet::multinomial models.
          model_predictions <- predict(
            object@model,
            newdata = encoded_data$encoded_data@data[, mget(object@feature_order)],
            type = "probs"
          )
        } else {
          # For VGAM::vglm
          ..deprecation_vgam()

          model_predictions <- suppressWarnings(
            VGAM::predictvglm(
              object = object@model,
              newdata = encoded_data$encoded_data@data,
              type = "response"
            )
          )
        }

        # Obtain class levels.
        class_levels <- get_outcome_class_levels(x = object)

        # Add class probabilities.
        class_probability_columns <- get_class_probability_name(x = object)
        for (ii in seq_along(class_probability_columns)) {
          if (is.matrix(model_predictions)) {
            prediction_table[, (class_probability_columns[ii]) := model_predictions[, ii]]
          } else {
            prediction_table[, (class_probability_columns[ii]) := model_predictions[ii]]
          }
        }

        # Update predicted class based on provided probabilities.
        class_predictions <- class_levels[apply(prediction_table[, mget(class_probability_columns)], 1, which.max)]
        class_predictions <- factor(class_predictions, levels = class_levels)
        prediction_table[, "predicted_class" := class_predictions]
        
      } else if (object@outcome_type %in% c("continuous", "count")) {
        ## Count and continuous outcomes ---------------------------------------

        if (inherits(object@model, "fastglm")) {
          # For fastglm::fastglm models.
          model_predictions <- suppressWarnings(
            predict(
              object = object@model,
              newdata = as.matrix(
                encoded_data$encoded_data@data[, mget(c(object@feature_order, "intercept__"))]),
              type = "response"))
          
        } else {
          # For stats::glm models.
          model_predictions <- suppressWarnings(
            predict(
              object = object@model,
              newdata = encoded_data$encoded_data@data,
              type = "response"))
        }

        # Add regression.
        prediction_table[, "predicted_outcome" := model_predictions]
      } else {
        ..error_outcome_type_not_implemented(object@outcome_type)
      }

      return(prediction_table)
      
    } else {
      # User-specified method --------------------------------------------------

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

      if (inherits(object@model, "fastglm")) {
        encoded_data$encoded_data@data[, "intercept__" := 1.0]
      }

      if (object@outcome_type %in% c("continuous", "count", "binomial")) {
        ## Binomial, count and continuous outcomes -----------------------------

        if (inherits(object@model, "fastglm")) {
          return(predict(
            object = object@model,
            newdata = as.matrix(
              encoded_data$encoded_data@data[, mget(c(object@feature_order, "intercept__"))]),
            type = type,
            ...))
          
        } else {
          # Use the model for prediction.
          return(predict(
            object = object@model,
            newdata = encoded_data$encoded_data@data,
            type = type,
            ...))
        }
        
      } else if (object@outcome_type == "multinomial") {
        ## Multinomial outcomes ------------------------------------------------

        if (inherits(object@model, "nnet")) {
          # For nnet::multinomial models.
          return(predict(object@model,
            newdata = encoded_data$encoded_data@data[, mget(c(object@feature_order, "intercept__"))],
            type = type,
            ...))
          
        } else {
          # For VGAM::vglm
          ..deprecation_vgam()

          # Use the model for prediction.
          return(VGAM::predictvglm(
            object = object@model,
            newdata = encoded_data$encoded_data@data,
            type = type,
            ...))
        }
        
      } else {
        ..error_outcome_type_not_implemented(object@outcome_type)
      }
    }
  }
)



# ..vimp -----------------------------------------------------------------------
setMethod(
  "..vimp",
  signature(object = "familiarGLM"),
  function(object, ...) {
    # Suppress NOTES due to non-standard evaluation in data.table
    score <- NULL

    if (!model_is_trained(object)) {
      return(callNextMethod())
    }

    # Check that required packages are loaded and installed.
    require_package(object, "vimp")

    # Compute z-values
    coefficient_z_values <- tryCatch(
      .compute_z_statistic(object, fix_all_missing = TRUE),
      error = identity)

    if (inherits(coefficient_z_values, "error")) {
      return(callNextMethod())
    }

    if (is(object@model, "vglm")) {
      ..deprecation_vgam()

      # Parse coefficient names. vglm adds :1 and :2 (and so on) to
      # coefficient names.
      coefficient_names <- strsplit(x = names(coefficient_z_values), split = ":", fixed = TRUE)
      coefficient_names <- sapply(coefficient_names, function(coefficient_name) coefficient_name[1])
      names(coefficient_z_values) <- coefficient_names
      
    } else if (inherits(object@model, "nnet")) {
      coefficient_names <- colnames(coefficient_z_values)
      coefficient_names <- rep(coefficient_names, each = nrow(coefficient_z_values))
      coefficient_z_values <- as.vector(coefficient_z_values)
      names(coefficient_z_values) <- coefficient_names
    }

    # Remove intercept from the coefficients.
    coefficient_z_values <- coefficient_z_values[
      !names(coefficient_z_values) %in% c("(Intercept)", "intercept__")]
    
    if (length(coefficient_z_values) == 0) {
      return(callNextMethod())
    }

    # Assign to variable importance table.
    vimp_table <- data.table::data.table(
      "score" = abs(coefficient_z_values),
      "name" = names(coefficient_z_values))

    # Merge by name (vglm coefficients can occur multiple times for the same
    # feature).
    vimp_table <- vimp_table[, list("score" = max(score)), by = "name"]

    # Create variable importance object.
    vimp_object <- methods::new("vimpTable",
      vimp_table = vimp_table,
      encoding_table = object@encoding_reference_table,
      score_aggregation = "max",
      invert = TRUE)

    return(vimp_object)
  }
)



# ..get_distribution_family ----------------------------------------------------
setMethod(
  "..get_distribution_family",
  signature(object = "familiarGLM"),
  function(object) {
    # Obtain family from the hyperparameters.
    family <- object@hyperparameters$family

    # Check that required packages are loaded and installed.
    require_package(object, "distribution")

    # Check that the family hyperparameter exists.
    if (!is.character(family) && !is.factor(family)) {
      ..error_reached_unreachable_code(
        "..get_distribution_family,familiarGLM: family hyperparameter was not set.")
    }

    # Load families for linear regression
    if (family %in% c("logistic", "binomial")) {
      family_fun <- stats::binomial(link = "logit")
    } else if (family == "probit") {
      family_fun <- stats::binomial(link = "probit")
    } else if (family == "cauchy") {
      family_fun <- stats::binomial(link = "cauchit")
    } else if (family == "loglog") {
      family_fun <- stats::binomial(link = "cloglog")
    } else if (family == "gaussian") {
      family_fun <- stats::gaussian(link = "identity")
    } else if (family == "log_gaussian") {
      family_fun <- stats::gaussian(link = "log")
    } else if (family == "inv_gaussian") {
      family_fun <- stats::gaussian(link = "inverse")
    } else if (family == "poisson") {
      family_fun <- stats::poisson(link = "identity")
    } else if (family == "log_poisson") {
      family_fun <- stats::poisson(link = "log")
    } else if (family == "multinomial") {
      family_fun <- "_placeholder_"
    } else {
      ..error_reached_unreachable_code(paste0(
        "..get_distribution_family,familiarGLM: unknown family.", family))
    }

    return(family_fun)
  }
)



# ..set_vimp_parameters --------------------------------------------------------
setMethod(
  "..set_vimp_parameters",
  signature(object = "familiarGLM"),
  function(object, method, ...) {
    # Read family string by parsing the learner.
    family_str <- sub(
      x = method,
      pattern = "glm",
      replacement = "",
      fixed = TRUE)
    
    if (family_str != "") {
      family_str <- sub(
        x = family_str, 
        pattern = "_",
        replacement = "",
        fixed = TRUE)
    }

    # Determine the family or families.
    if (family_str == "") {
      # If no family is specified, the default behaviour is to identify the
      # family through optimisation.
      if (object@outcome_type == "binomial") {
        family_default <- c("logistic")
      } else if (object@outcome_type == "continuous") {
        family_default <- c("gaussian")
      } else if (object@outcome_type == "count") {
        family_default <- c("poisson")
      } else if (object@outcome_type == "multinomial") {
        family_default <- "multinomial"
      } else if (object@outcome_type == "survival") {
        family_default <- "cox"
      } else {
        ..error_outcome_type_not_implemented(object@outcome_type)
      }
      
    } else if (family_str == "log") {
      # "log" is a collection of different families, that should be specified
      # according to the outcome type.
      if (object@outcome_type == "continuous") {
        family_default <- c("log_gaussian")
      } else if (object@outcome_type == "count") {
        family_default <- "log_poisson"
      }
      
    } else {
      # A family was unambiguously specified.
      family_default <- family_str
    }

    # Update family hyperparameter.
    object@hyperparameters$family <- family_default

    return(object)
  }
)



# .trim_model ------------------------------------------------------------------
setMethod(
  ".trim_model",
  signature(object = "familiarGLM"),
  function(object, ...) {
    if (inherits(object@model, "fastglm")) {
      # Update model by removing the call.
      object@model$call <- call("trimmed")

      # Add show.
      object <- .capture_show(object)

      object@model$fitted.values <- NULL
      object@model$linear.predictors <- NULL
      object@model$weights <- NULL
      object@model$prior.weights <- NULL
      object@model$y <- NULL
      object@model$n <- NULL
      object@model$residuals <- NULL
      
    } else if (inherits(object@model, "glm")) {
      # Update model by removing the call.
      object@model$call <- call("trimmed")

      # Add show.
      object <- .capture_show(object)

      # Remove .Environment.
      object@model$terms <- .replace_environment(object@model$terms)
      object@model$formula <- .replace_environment(object@model$formula)

      # Remove elements that contain sample-specific values.
      object@model$fitted.values <- NULL
      object@model$data <- NULL
      object@model$linear.predictors <- NULL
      object@model$prior.weights <- NULL
      object@model$weights <- NULL
      object@model$qr$qr <- NULL
      object@model$residuals <- NULL
      object@model$effects <- NULL
      
    } else if (inherits(object@model, "nnet")) {
      # Update model by removing the call.
      object@model$call <- call("trimmed")

      # Add show.
      object <- .capture_show(object)

      # Remove .Environment.
      object@model$terms <- .replace_environment(object@model$terms)

      # Remove elements that contain sample-specific values.
      object@model$fitted.values <- NULL
      object@model$weights <- NULL
      object@model$residuals <- NULL
      
    } else if (inherits(object@model, "vglm")) {
      ..deprecation_vgam()
      
      # Update model by removing the call.
      object@model@call <- call("trimmed")

      # Add show.
      object <- .capture_show(object)

      # Remove .Environment.
      object@model@terms$terms <- .replace_environment(object@model@terms$terms)
      object@model@misc$formula <- .replace_environment(object@model@misc$formula)

      # Remove elements that contain sample-specific values.
      object@model@predictors <- matrix(0)
      object@model@effects <- numeric(0)
      object@model@qr$qr <- NULL
      object@model@fitted.values <- matrix(0)
      object@model@residuals <- matrix(0)
      object@model@weights <- matrix(0)
      object@model@x <- matrix(0)
      object@model@y <- matrix(0)
    }

    # Set is_trimmed to TRUE.
    object@is_trimmed <- TRUE

    # Default method for models that lack a more specific method.
    return(object)
  }
)



.get_available_glm_learners <- function(show_general = TRUE) {
  # Learners
  learners <- c(
    "glm", "glm_logistic", "glm_probit", "glm_cauchy",
    "glm_log", "glm_loglog", "glm_multinomial", "glm_gaussian",
    "glm_log_gaussian", "glm_inv_gaussian", "glm_poisson",
    "glm_log_poisson"
  )
  
  if (!show_general) {
    learners <- setdiff(learners, c("glm", "glm_log"))
  }
  
  return(learners)
}

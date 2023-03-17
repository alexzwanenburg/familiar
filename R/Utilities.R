#' @include FamiliarS4Generics.R


stop_or_warn <- function(message, as_error = TRUE) {
  # Find the name of the calling environment.
  calling_function <- environmentName(parent.env)

  if (length(calling_function) > 0) {
    if (calling_function != "") {
      message <- paste0(calling_function, ": ", message)
    }
  }

  if (as_error) {
    stop(message, call. = FALSE)
  } else {
    warning(message, call. = FALSE)
  }
}



compute_univariable_p_values <- function(cl = NULL, data_obj, feature_columns) {
  outcome_type <- data_obj@outcome_type
  outcome_columns <- get_outcome_columns(data_obj)

  if (outcome_type == "survival") {
    univariate_fun <- .univariate_cox_regression_test
  } else if (outcome_type == "continuous") {
    univariate_fun <- .univariate_linear_regression_test
  } else if (outcome_type == "binomial") {
    univariate_fun <- .univariate_binomial_logistic_regression_test
  } else if (outcome_type == "multinomial") {
    univariate_fun <- .univariate_multinomial_logistic_regression_test
  } else if (outcome_type == "count") {
    univariate_fun <- .univariate_poisson_regression_test
  } else if (outcome_type == "competing_risk") {
    ..error_outcome_type_not_implemented(outcome_type)
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }

  # Obtain p-values
  coefficient_p_values <- fam_sapply(
    cl = cl,
    assign = NULL,
    X = data_obj@data[, mget(feature_columns)],
    FUN = univariate_fun,
    progress_bar = FALSE,
    outcome_data = data_obj@data[, mget(outcome_columns)],
    chopchop = TRUE)

  return(coefficient_p_values)
}



..univariate_test_variable_encoding <- function(x, insert_intercept = FALSE) {
  n <- length(x)

  if (is.factor(x)) {
    # Categorical variables are encoded as numeric levels (ordinal), or using
    # one-hot-encoding (nominal).
    if (is.ordered(x)) {
      x <- list(as.numeric(x))
    } else {
      # Drop unused levels.
      x <- droplevels(x)

      # Dummy encoding of categorical variable.
      level_names <- levels(x)
      level_count <- nlevels(x)

      x <- lapply(level_names[2:level_count], function(ii, x) (as.numeric(x == ii)), x = x)
    }
  } else {
    # Numeric variables are only stored as a list.
    x <- list(x)
  }

  # Set names
  names(x) <- paste0("name_", seq_along(x))

  if (insert_intercept) {
    x <- c(x, list("intercept__" = numeric(n) + 1.0))
  }

  return(data.table::as.data.table(x))
}



.univariate_cox_regression_test <- function(x, outcome_data) {
  # Cox regression model for univariate analysis

  # Check if any data was provided.
  if (length(x) == 0) {
    return(NA_real_)
  }

  # Extract response
  y_time <- outcome_data$outcome_time
  y_event <- outcome_data$outcome_event

  # Remove missing elements.
  valid_elements <- is.finite(x) & is.finite(y_time) & is.finite(y_event)
  if (sum(valid_elements) <= 1) return(NA_real_)

  # Keep only valid elements.
  x <- x[valid_elements]
  y_time <- y_time[valid_elements]
  y_event <- y_event[valid_elements]

  # Check if the feature column is singular.
  if (is_singular_data(x)) return(NA_real_)

  # Bind data.
  data <- cbind(
    ..univariate_test_variable_encoding(x, insert_intercept = FALSE),
    data.table::data.table(
      "time" = y_time,
      "event" = y_event))

  # Construct model
  model <- tryCatch(
    coxph(
      Surv(time, event) ~ .,
      data = data),
    error = identity)

  # Check if the model did not converge.
  if (inherits(model, "error")) return(NA_real_)

  # Compute z-statistic.
  z <- .compute_z_statistic(model)

  # Compute the p-value from a t-distribution
  p_value <- 2 * (1.0 - stats::pt(abs(z), df = length(x)))

  # Check if the value is finite.
  if (all(!is.finite(p_value))) return(NA_real_)

  # Return p-value.
  return(unname(min(p_value, na.rm = TRUE)))
}



.univariate_linear_regression_test <- function(x, outcome_data) {
  # Gaussian regression for univariable analysis

  # Check if any data was provided.
  if (length(x) == 0) return(NA_real_)

  # Extract response
  y <- outcome_data$outcome

  # Remove missing elements.
  valid_elements <- is.finite(x) & is.finite(y)
  if (sum(valid_elements) <= 1) return(NA_real_)

  # Keep only valid elements.
  x <- x[valid_elements]
  y <- y[valid_elements]

  # Check if the feature column is singular.
  if (is_singular_data(x)) return(NA_real_)

  # Bind data.
  data <- cbind(
    ..univariate_test_variable_encoding(x, insert_intercept = TRUE),
    data.table::data.table("response" = y))

  if (require_package("fastglm", message_type = "silent")) {
    # Using fastglm.

    predictors <- setdiff(colnames(data), "response")

    # Construct model.
    model <- do.call_with_handlers(
      fastglm::fastglm,
      args = list(
        "x" = as.matrix(data[, mget(predictors)]),
        "y" = data$response,
        "family" = stats::gaussian(link = "identity"),
        "method" = 3L))

    # Check for errors.
    if (!is.null(model$error)) return(NA_real_)

    # Extract the model itself.
    model <- model$value
    
  } else {
    # Construct model
    model <- tryCatch(
      stats::glm(response ~ . - 1,
        data = data,
        family = stats::gaussian(link = "identity")),
      error = identity)

    # Check if the model did not converge.
    if (inherits(model, "error")) return(NA_real_)
  }

  # Compute z-statistic.
  z <- .compute_z_statistic(model)

  # Remove intercept (if present).
  z <- z[!names(z) %in% "intercept__"]

  # Compute the p-value from a t-distribution
  p_value <- 2 * (1.0 - stats::pt(abs(z), df = length(x)))

  # Check if the value is finite.
  if (all(!is.finite(p_value))) return(NA_real_)

  # Return p-value.
  return(unname(min(p_value, na.rm = TRUE)))
}



.univariate_poisson_regression_test <- function(x, outcome_data) {
  # Poisson regression for univariable analysis with count-type outcomes

  # Check if any data was provided.
  if (length(x) == 0) return(NA_real_)

  # Extract response
  y <- outcome_data$outcome

  # Remove missing elements.
  valid_elements <- is.finite(x) & is.finite(y)
  if (sum(valid_elements) <= 1) return(NA_real_)

  # Keep only valid elements.
  x <- x[valid_elements]
  y <- y[valid_elements]

  # Check if the feature column is singular.
  if (is_singular_data(x)) return(NA_real_)

  # Bind data.
  data <- cbind(
    ..univariate_test_variable_encoding(x, insert_intercept = TRUE),
    data.table::data.table("response" = y))

  if (require_package("fastglm", message_type = "silent")) {
    # Using fastglm.
    predictors <- setdiff(colnames(data), "response")

    # Construct model.
    model <- do.call_with_handlers(
      fastglm::fastglm,
      args = list(
        "x" = as.matrix(data[, mget(predictors)]),
        "y" = data$response,
        "family" = stats::poisson(),
        "method" = 3L))

    # Check for errors.
    if (!is.null(model$error)) return(NA_real_)

    # Extract the model itself.
    model <- model$value
    
  } else {
    # Using glm as backup option.
    model <- suppressWarnings(tryCatch(
      stats::glm(
        response ~ . - 1,
        data = data,
        family = stats::poisson(link = "log")),
      error = identity))

    # Check if the model did not converge.
    if (inherits(model, "error")) return(NA_real_)
  }

  # Compute z-statistic.
  z <- .compute_z_statistic(model)

  # Remove intercept (if present).
  z <- z[names(z) != "intercept__"]

  # Compute the p-value from a t-distribution
  p_value <- 2 * (1.0 - stats::pt(abs(z), df = length(x)))

  # Check if the value is finite.
  if (all(!is.finite(p_value))) return(NA_real_)

  # Return p-value.
  return(unname(min(p_value, na.rm = TRUE)))
}



.univariate_binomial_logistic_regression_test <- function(x, outcome_data) {
  # Binomial model for univariable analysis using logistic regression

  # Check if any data was provided.
  if (length(x) == 0) return(NA_real_)

  # Extract response
  y <- outcome_data$outcome

  # Remove missing elements.
  valid_elements <- is.finite(x) & is.finite(y)
  if (sum(valid_elements) <= 1) return(NA_real_)

  # Keep only valid elements.
  x <- x[valid_elements]
  y <- y[valid_elements]

  # Check if the feature column is singular.
  if (is_singular_data(x)) return(NA_real_)

  # Bind data.
  data <- cbind(
    ..univariate_test_variable_encoding(x, insert_intercept = TRUE),
    data.table::data.table("response" = y))

  if (require_package("fastglm", message_type = "silent")) {
    # Using fastglm.
    predictors <- setdiff(colnames(data), "response")

    # Construct model.
    model <- do.call_with_handlers(
      fastglm::fastglm,
      args = list(
        "x" = as.matrix(data[, mget(predictors)]),
        "y" = as.numeric(data$response) - 1,
        "family" = stats::binomial(link = "logit"),
        "method" = 3L))

    # Check for errors.
    if (!is.null(model$error)) return(NA_real_)

    # Extract the model itself.
    model <- model$value

    # Check for Hauck-Donner effect.
    if (approximately(model$deviance, 0.0)) {
      # Create a model without an intercept.
      model_2 <- do.call_with_handlers(
        fastglm::fastglm,
        args = list(
          "x" = as.matrix(data[, mget(setdiff(predictors, "intercept__"))]),
          "y" = as.numeric(data$response) - 1,
          "family" = stats::binomial(link = "logit"),
          "method" = 3L))

      # Check for errors, and only attempt to replace model by model_2 if no
      # errors are present.
      if (is.null(model_2$error)) {
        # Extract the model itself.
        model_2 <- model_2$value

        # Replace by intercept-less model in case the deviance is higher.
        if (model_2$deviance > model$deviance) model <- model_2
      }
    }
    
  } else {
    # Construct model
    model <- suppressWarnings(tryCatch(
      stats::glm(
        response ~ . - 1,
        data = data,
        family = stats::binomial(link = "logit")),
      error = identity))

    # Check if the model did not converge
    if (inherits(model, "error")) return(NA_real_)
  }

  # Compute z-statistic
  z <- .compute_z_statistic(model)

  # Remove intercept (if present).
  z <- z[names(z) != "intercept__"]

  # Compute the p-value from a t-distribution
  p_value <- 2 * (1.0 - stats::pt(abs(z), df = length(x)))

  # Check if the value is finite.
  if (all(!is.finite(p_value))) return(NA_real_)

  # Return p-value.
  return(unname(min(p_value, na.rm = TRUE)))
}



.univariate_multinomial_logistic_regression_test <- function(x, outcome_data) {
  # Multinomial model for univariable analysis using logistic regression

  # Check if any data was provided.
  if (length(x) == 0) return(NA_real_)

  # Extract response
  y <- outcome_data$outcome

  # Remove missing elements.
  valid_elements <- is.finite(x) & is.finite(y)
  if (sum(valid_elements) <= 1) return(NA_real_)

  # Keep only valid elements.
  x <- x[valid_elements]
  y <- y[valid_elements]

  # Check if the feature column is singular.
  if (is_singular_data(x)) return(NA_real_)

  # Bind data.
  data <- cbind(
    ..univariate_test_variable_encoding(x, insert_intercept = TRUE),
    data.table::data.table("response" = y))

  # Check if the package is installed and attached.
  require_package(
    x = "nnet",
    purpose = "to determine univariate p-values for multinomial outcomes")

  # Construct model
  model <- do.call_with_handlers(
    nnet::multinom,
    args = list(
      response ~ . - 1,
      "data" = data,
      "maxit" = 500))

  # Check for errors.
  if (!is.null(model$error)) return(NA_real_)

  # Extract the model itself.
  model <- model$value

  # Check if the model converged. NNET might fail to converge if the
  # Hauck-Donner effect is present.
  if (model$convergence == 1) {
    model_2 <- do.call_with_handlers(
      nnet::multinom,
      args = list(
        response ~ . - 1 - intercept__,
        "data" = data,
        "maxit" = 500))

    # Check for errors, and only attempt to replace model by model_2 if no
    # errors are present.
    if (is.null(model_2$error)) {
      # Extract the model itself.
      model_2 <- model_2$value

      # If the Hauck-Donner effect is indeed present, the new model without the
      # intercept should converge quickly.
      if (model_2$convergence == 0) model <- model_2
    }
  }

  # Compute z-statistic
  z <- .compute_z_statistic(model)

  # Remove intercept (if present).
  predictors <- setdiff(colnames(data), c("response", "intercept__"))
  z <- if (is.matrix(z)) as.vector(z[, predictors]) else z[predictors]

  # Compute the p-value from a t-distribution
  p_value <- 2 * (1.0 - stats::pt(abs(z), df = length(x) * (nlevels(y) - 1L)))

  # Check if the value is finite.
  if (all(!is.finite(p_value))) return(NA_real_)

  # Return p-value.
  return(unname(min(p_value, na.rm = TRUE)))
}



.get_available_icc_types <- function() {
  return(c("1", "2", "3"))
}



compute_icc <- function(x, feature, id_data, type = "1") {
  # We start from the following equation: xij = mu + a_i + b_j + e_ij, with mu
  # the population mean, a_i the rater-dependent change, b_j the
  # subject-dependent change and eij an error with mean 0.
  #
  # * type = "1": ICC 1: We assume that each rated value is cased by a random
  # effect of the rater: i.e. the actual rotation is not associated with an
  # systematic change in value, and there is no interaction between rater and
  # subject. [Shrout 1979]. This means that a_i has mean 0, so that wij = ai +
  # eij. Put differently, raters are randomly chosen from a larger population
  # for each sample.
  #
  # * type = "2": ICC 2: There is a panel of raters, and the entire panel
  # evaluates each sample. However, the panel is assumed to be part of a larger
  # population of raters. Raters are assumed to be systematically biased.

  # * type = "3": ICC 3: There is a panel of raters and the entire panel evaluates each
  # subject. The panel is the entire population and raters are assumed to be
  # systematically biased.

  # Suppress NOTES due to non-standard evaluation in data.table
  value <- mu <- bj <- ai <- NULL

  # Determine identifier columns.
  sample_id_columns <- get_id_columns(id_depth = "series")
  repetition_id_column <- get_id_columns(single_column = "repetition_id")

  # Create data table from x and combine with id_data
  data <- data.table::data.table("value" = x)
  data <- cbind(id_data, data)

  # Calculate each parameter in the equation
  data[, "mu" := mean(value, na.rm = TRUE)]
  data[, "bj" := mean(value, na.rm = TRUE) - mu, by = sample_id_columns]
  data[, "ai" := mean(value, na.rm = TRUE) - mu, by = repetition_id_column]
  data[, "eij" := value - mu - bj - ai]

  # Calculate
  n_samples <- data.table::uniqueN(data, by = sample_id_columns)
  n_raters <- data.table::uniqueN(data, by = repetition_id_column)

  # Calculate mean squared errors: msb between subjects (bj), msj between raters
  # (ai), mse of error (eij) and msw of error with rater (ai + eij).
  if (n_samples > 1) {
    msb <- sum(data$bj^2, na.rm = TRUE) / (n_samples - 1)
  }

  if (type == "1") {
    # Calculate mean squared of error with rater
    msw <- (sum(data$eij^2, na.rm = TRUE) + sum(data$ai^2, na.rm = TRUE)) /
      (n_samples * (n_raters - 1))

    # Calculate icc for individual rater and rater panel
    if (msb == 0 && msw == 0) {
      icc <- 1
      icc_panel <- 1
      icc_ci_low <- 1
      icc_ci_up <- 1
      icc_panel_ci_low <- 1
      icc_panel_ci_up <- 1
      
    } else {
      icc <- (msb - msw) / (msb + (n_raters - 1) * msw)
      icc_panel <- (msb - msw) / msb

      # Fisher score
      s_fisher <- msb / msw
      s_fisher_low <- s_fisher / stats::qf(0.975, n_samples - 1, n_samples * (n_raters - 1))
      s_fisher_up <- s_fisher / stats::qf(0.025, n_samples - 1, n_samples * (n_raters - 1))

      # Calcuate confidence intervals from fisher score
      icc_ci_low <- (s_fisher_low - 1) / (s_fisher_low + n_raters - 1)
      icc_ci_up <- (s_fisher_up - 1) / (s_fisher_up + n_raters - 1)
      icc_panel_ci_low <- 1 - 1 / s_fisher_low
      icc_panel_ci_up <- 1 - 1 / s_fisher_up
    }
  }

  if (type == "2") {
    # Calculate mean squared error (mse) and mean squared rater error (msj)
    msj <- sum(data$ai^2, na.rm = TRUE) / (n_raters - 1)
    mse <- sum(data$eij^2, na.rm = TRUE) / ((n_samples - 1) * (n_raters - 1))

    # Calculate icc for individual rater and rater panel
    if (msb == 0 && mse == 0) {
      icc <- 1
      icc_panel <- 1
      icc_ci_low <- 1
      icc_ci_up <- 1
      icc_panel_ci_low <- 1
      icc_panel_ci_up <- 1
      
    } else {
      icc <- (msb - mse) / (msb + (n_raters - 1) * mse + (n_raters / n_samples) * (msj - mse))
      icc_panel <- (msb - mse) / (msb + (msj - mse) / n_samples)

      # Determine confidence intervals
      vn <- (n_raters - 1) * (n_samples - 1) * 
        (n_raters * icc * msj / mse + n_samples * (1 + (n_raters - 1) * icc) - n_raters * icc)^2
      vd <- (n_samples - 1) * n_raters^2 * icc^2 * (msj / mse)^2 + 
        (n_samples * (1 + (n_raters - 1) * icc) - n_raters * icc)^2
      v <- vn / vd
      thresh_low <- stats::qf(0.975, n_samples - 1, v)
      thresh_up <- stats::qf(0.025, n_samples - 1, v)

      # Calcuate confidence intervals from fisher score
      icc_ci_low <- n_samples * (msb - thresh_low * mse) / 
        (thresh_low * (n_raters * msj + (n_raters * n_samples - n_raters - n_samples) * mse) + n_samples * msb)
      icc_ci_up <- n_samples * (msb - thresh_up * mse) / 
        (thresh_up * (n_raters * msj + (n_raters * n_samples - n_raters - n_samples) * mse) + n_samples * msb)
      icc_panel_ci_low <- icc_ci_low * n_raters / (1 + icc_ci_low * (n_raters - 1))
      icc_panel_ci_up <- icc_ci_up * n_raters / (1 + icc_ci_up * (n_raters - 1))
    }
  }

  if (type == "3") {
    # Calculate mean squared error (mse)
    mse <- sum(data$eij^2, na.rm = TRUE) / ((n_samples - 1) * (n_raters - 1))

    # Calculate icc for individual rater and rater panel
    if (msb == 0 && mse == 0) {
      icc <- 1
      icc_panel <- 1
      icc_ci_low <- 1
      icc_ci_up <- 1
      icc_panel_ci_low <- 1
      icc_panel_ci_up <- 1
      
    } else {
      icc <- (msb - mse) / (msb + (n_raters - 1) * mse)
      icc_panel <- (msb - mse) / msb

      # Fisher score
      s_fisher <- msb / mse
      s_fisher_low <- s_fisher / stats::qf(0.975, n_samples - 1, (n_samples - 1) * (n_raters - 1))
      s_fisher_up <- s_fisher / stats::qf(0.025, n_samples - 1, (n_samples - 1) * (n_raters - 1))

      # Calcuate confidence intervals from fisher score
      icc_ci_low <- (s_fisher_low - 1) / (s_fisher_low + n_raters - 1)
      icc_ci_up <- (s_fisher_up - 1) / (s_fisher_up + n_raters - 1)
      icc_panel_ci_low <- 1 - 1 / s_fisher_low
      icc_panel_ci_up <- 1 - 1 / s_fisher_up
    }
  }

  if (icc == 1.0) {
    if (!is.finite(icc_ci_low)) icc_ci_low <- 1.0
    if (!is.finite(icc_ci_up)) icc_ci_up <- 1.0
  }

  if (icc_panel == 1.0) {
    if (!is.finite(icc_ci_low)) icc_panel_ci_low <- 1.0
    if (!is.finite(icc_ci_up)) icc_panel_ci_up <- 1.0
  }

  return(data.table::data.table(
    "feature" = feature,
    "icc" = icc,
    "icc_low" = icc_ci_low,
    "icc_up" = icc_ci_up,
    "icc_panel" = icc_panel,
    "icc_panel_low" = icc_panel_ci_low,
    "icc_panel_up" = icc_panel_ci_up))
}



.file_extension <- function(x) {
  # Find the file extension by extracting the substring to the right of the
  # final period.

  # Find the position of the period preceding the file extension.
  indicator <- tail(
    gregexpr(pattern = ".", text = x, fixed = TRUE)[[1]],
    n = 1L)

  # Check when period (.) is not found in x.
  if (indicator == -1) return("")

  # Find the extension
  extension <- substr(x, start = indicator + 1L, stop = nchar(x))

  return(extension)
}



harmonic_p_value <- function(x) {
  if (!is_package_installed("harmonicmeanp")) return(NA_real_)

  if (data.table::is.data.table(x)) {
    x <- x$p_value

    # Fix numeric issues due to very small p-values.
    x[x < 1E-15] <- 1E-15

    return(list("p_value" = harmonicmeanp::p.hmp(x, L = length(x))))
    
  } else {
    # Fix numeric issues due to very small p-values.
    x[x < 1E-15] <- 1E-15

    return(harmonicmeanp::p.hmp(x, L = length(x)))
  }
}



get_mode <- function(x) {
  # Ken Williams:
  # https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
  ux <- unique(x)
  ux <- ux[which.max(tabulate(match(x, ux)))]

  return(ux)
}



get_estimate <- function(x, na.rm = TRUE) {
  # Remove NA values.
  if (na.rm) x <- x[!is.na(x)]

  if (is.numeric(x)) {
    # Determine the estimate.
    if (length(x) > 0) {
      y <- mean(x)
    } else {
      y <- NA_real_
    }
    
  } else {
    # Determine the estimate.
    if (length(x) > 0) {
      y <- get_mode(x)
    } else {
      y <- NA
    }
  }

  return(y)
}


.sanitise_dots <- function(class, ...) {
  dots <- list(...)

  if (length(dots) == 0) return(dots)

  slot_names <- names(methods::getSlots(class))

  return(dots[intersect(names(dots), slot_names)])
}



get_placeholder_vimp_table <- function(
    vimp_method,
    run_table = NULL) {
  # Set vimp method.
  vimp_object <- methods::new(
    "vimpTable",
    vimp_method = vimp_method)

  # Set run table.
  vimp_object@run_table <- run_table

  # Set package version.
  vimp_object <- add_package_version(vimp_object)

  return(vimp_object)
}



get_id_columns <- function(id_depth = "repetition", single_column = NULL) {
  # Check that until_depth is correctly specified.
  if (!id_depth %in% c("batch", "sample", "series", "repetition")) {
    ..error_value_not_allowed(
      x = id_depth, 
      var_name = "id_depth", 
      values = c("batch", "sample", "series", "repetition"))
  }

  if (is.null(single_column)) {
    # Generate the names of the non-feature columns
    id_columns <- switch(
      id_depth,
      "batch" = "batch_id",
      "sample" = c("batch_id", "sample_id"),
      "series" = c("batch_id", "sample_id", "series_id"),
      "repetition" = c("batch_id", "sample_id", "series_id", "repetition_id"))
    
  } else {
    id_columns <- switch(
      single_column,
      "batch" = "batch_id",
      "sample" = "sample_id",
      "series" = "series_id",
      "repetition" = "repetition_id")
  }

  return(id_columns)
}



get_object_file_name <- function(
    learner,
    fs_method,
    project_id,
    data_id,
    run_id,
    pool_data_id = NULL,
    pool_run_id = NULL,
    object_type,
    is_ensemble = NULL,
    is_validation = NULL,
    with_extension = TRUE,
    dir_path = NULL) {
  # Generate file name for an object

  if (!object_type %in% c("familiarModel", "familiarEnsemble", "familiarData")) {
    stop("The object type was not recognised.")
  }

  # Generate the basic string
  base_str <- paste0(
    project_id, "_", 
    learner, "_", 
    fs_method, "_", 
    data_id, "_", 
    run_id)

  if (object_type == "familiarModel") {
    # For familiarModel objects

    output_str <- paste0(base_str, "_model")
    
  } else if (object_type == "familiarEnsemble") {
    # For familiarEnsemble objects

    if (is.null(is_ensemble)) {
      stop("The \"is_ensemble\" parameter is not set to TRUE or FALSE.")
    }

    output_str <- paste0(
      base_str, "_", 
      ifelse(is_ensemble, "ensemble", "pool"))
    
  } else if (object_type == "familiarData") {
    # For familiarData objects

    if (is.null(is_ensemble)) {
      stop("The \"is_ensemble\" parameter is not set to TRUE or FALSE.")
    }

    if (is.null(is_validation)) {
      stop("The \"is_validation\" parameter is not set to TRUE or FALSE.")
    }

    if (is.null(pool_data_id) || is.null(pool_run_id)) {
      stop()
    }

    output_str <- paste0(
      base_str, "_",
      ifelse(is_ensemble, "ensemble", "pool"), "_",
      pool_data_id, "_",
      pool_run_id, "_",
      ifelse(is_validation, "validation", "development"), "_data"
    )
  }

  # Add extension
  if (with_extension) {
    output_str <- paste0(output_str, ".RDS")
  }

  # Join with dir_path
  if (!is.null(dir_path)) {
    output_str <- normalizePath(file.path(dir_path, output_str), mustWork = FALSE)
  }

  return(output_str)
}



get_object_dir_path <- function(
    dir_path, 
    object_type, 
    learner = NULL, 
    fs_method = NULL) {
  # Generate the directory path to an object

  if (!object_type %in% c(
    "familiarModel", "familiarEnsemble", "familiarData", "familiarCollection")) {
    stop("The object type was not recognised.")
  }

  if (object_type %in% c("familiarModel", "familiarEnsemble")) {
    if (is.null(learner) && is.null(fs_method)) {
      return(normalizePath(file.path(dir_path), mustWork = FALSE))
    } else {
      return(normalizePath(file.path(dir_path, learner, fs_method), mustWork = FALSE))
    }
    
  } else if (object_type %in% c("familiarData", "familiarCollection")) {
    return(normalizePath(file.path(dir_path)))
  }
}


extract_from_slot <- function(
    object_list, 
    slot_name, 
    slot_element = NULL, 
    na.rm = FALSE) {
  # Extracts values from a slot in an object. Providing slot_element allows
  # extraction from a list in a slot

  if (is.null(slot_element)) {
    slot_values <- sapply(object_list, slot, name = slot_name)
    
  } else {
    slot_values <- sapply(
      object_list, 
      function(object, slot_name, slot_element) {
        list_element <- slot(object = object, name = slot_name)
        if (!is.list(list_element)) {
          return(NULL)
        } else {
          return(list_element[[slot_element]])
        }
      }, 
      slot_name = slot_name,
      slot_element = slot_element)
  }

  if (na.rm) {
    # First remove NULL
    slot_values <- slot_values[!sapply(slot_values, is.null)]

    # Then remove NA
    if (is.numeric(slot_values) ||
        is.logical(slot_values) ||
        is.character(slot_values)) {
      slot_values <- slot_values[!sapply(slot_values, is.na)]
    }

    # Check if the slot values are numeric, and remove infinite values if so
    if (is.numeric(slot_values)) {
      slot_values <- slot_values[!sapply(slot_values, is.infinite)]
    }
  }

  return(slot_values)
}



all_empty_slot <- function(object_list, slot_name, slot_element = NULL) {
  # Determine if all slots of objects in the object list are empty

  # Extract slot values
  slot_values <- extract_from_slot(
    object_list = object_list,
    slot_name = slot_name,
    slot_element = slot_element,
    na.rm = TRUE)

  # Determine if there are any slot values that have a value.
  return(length(slot_values) == 0)
}


process_random_forest_survival_predictions <- function(
    event_matrix, 
    event_times, 
    prediction_table, 
    time, 
    type) {
  # Suppress NOTES due to non-standard evaluation in data.table
  event_time <- NULL

  # Set id columns
  id_columns <- get_id_columns()

  # Make a local copy of the prediction_table
  prediction_table <- data.table::copy(prediction_table)

  # Convert event_matrix to a matrix.
  if (!is.matrix(event_matrix)) {
    event_matrix <- matrix(
      data = event_matrix, 
      ncol = length(event_matrix))
  }

  # Combine with identifiers and cast to table.
  event_table <- cbind(
    prediction_table[, mget(id_columns)],
    data.table::as.data.table(event_matrix))

  # Remove duplicate entries
  event_table <- unique(event_table, by = id_columns)

  # Melt to a long format.
  event_table <- data.table::melt(
    event_table,
    id.vars = id_columns,
    variable.name = "time_variable",
    value.name = "value")

  # Create conversion table to convert temporary variables into the event times.
  conversion_table <- data.table::data.table(
    "time_variable" = paste0("V", seq_along(event_times)),
    "event_time" = event_times)

  # Add in event times
  event_table <- merge(
    x = event_table, 
    y = conversion_table, 
    on = "time_variable")

  # Drop the time_variable column
  event_table[, "time_variable" := NULL]

  if (time %in% event_times) {
    # Get the event time directly.
    event_table <- event_table[event_time == time, ]

    # Remove event_time column and rename the value column to predicted_outcome.
    event_table[, "event_time" := NULL]
    data.table::setnames(
      x = event_table,
      old = "value",
      new = "predicted_outcome")
    
  } else {
    # Add starting values.
    if (!0 %in% event_times) {
      # Create initial table
      initial_event_table <- data.table::copy(event_table[event_time == event_times[1]])

      # Update values
      if (type == "cumulative_hazard") {
        initial_event_table[, ":="("value" = 0.0, "event_time" = 0)]
      } else if (type == "survival") {
        initial_event_table[, ":="("value" = 1.0, "event_time" = 0)]
      } else {
        ..error_reached_unreachable_code(paste0(
          "process_random_forest_survival_predictions: type was not recognised: ",
          type))
      }

      # Combine with the event table.
      event_table <- rbind(initial_event_table, event_table)
    }

    # Now, interpolate at the given time point.
    event_table <- lapply(
      split(event_table, by = id_columns), 
      function(sample_table, time, id_columns) {
        # Interpolate values at the given time.
        value <- stats::approx(
          x = sample_table$event_time,
          y = sample_table$value,
          xout = time,
          rule = 2
        )$y
        
        # Create an output table
        output_table <- data.table::copy(sample_table[1, mget(id_columns)])
        output_table[, "predicted_outcome" := value]
        
        return(output_table)
      },
      time = time, 
      id_columns = id_columns)

    # Concatenate to single table.
    event_table <- data.table::rbindlist(event_table)
  }

  if (type == "cumulative_hazard") {
    # Remove predicted_outcome from the prediction table.
    prediction_table[, "predicted_outcome" := NULL]
    
  } else {
    # Remove survival_probability from the prediction table.
    prediction_table[, "survival_probability" := NULL]

    # Update response column.
    data.table::setnames(
      event_table,
      old = "predicted_outcome",
      new = "survival_probability")
  }

  # Merge the event table into the prediction table.
  prediction_table <- merge(
    x = prediction_table, 
    y = event_table, 
    by = id_columns)

  return(prediction_table)
}



is_singular_data <- function(x) {
  # Checks if the input data is singular (i.e. only has one value)
  class_x <- class(x)

  # Drop any NA data.
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(TRUE)

  if (any(class_x %in% "factor")) {
    return(length(levels(droplevels(x))) == 1)
    
  } else if (any(class_x %in% c("numeric", "integer", "logical"))) {
    # Drop any infinite data
    x <- x[is.finite(x)]
    if (length(x) <= 1) return(TRUE)

    return(stats::var(x, na.rm = TRUE) == 0)
    
  } else if (any(class_x %in% c("character"))) {
    return(length(unique(x)) == 1)
    
  } else {
    return(TRUE)
  }
}



is_valid_data <- function(x) {
  # Checks validity of input data x

  class_x <- class(x)

  if (any(class_x %in% c("numeric", "integer", "logical"))) {
    return(is.finite(x))
    
  } else if (any(class_x %in% c("factor"))) {
    return(!is.na(x))
    
  } else if (any(class_x %in% "character")) {
    return(!(is.na(x) | tolower(x) %in% c("na", "nan", "inf", "-inf")))
    
  } else {
    return(FALSE)
  }
}



.get_class_methods <- function(object) {
  # In R, methods are not really inherently associated with a class, but they
  # are functions that are dispatched to a specific class using magic. This
  # function identifies which methods are associated with an object.

  associated_methods <- NULL
  for (object_class in class(object)) {
    # The utils::methods function can return all known (i.e. package in
    # namespace) methods for an object.
    associated_methods <- c(
      associated_methods,
      attr(utils::methods(class = object_class), "info")$generic)
  }

  # Keep only unique methods.
  associated_methods <- unique(associated_methods)

  return(associated_methods)
}



.compute_z_statistic <- function(model, fix_all_missing = FALSE) {
  mu <- cov_matrix <- stdevs <- NULL

  if (is(model, "familiarModel")) {
    # Attempt to extract the estimates and the covariance matrix from the
    # familiarModel object.
    if (!is.null(model@trimmed_function$coef)) {
      mu <- model@trimmed_function$coef
    }

    if (!is.null(model@trimmed_function$vcov)) {
      cov_matrix <- model@trimmed_function$vcov
    }

    # Extract the model itself.
    model <- model@model
  }

  if (is(model, "vglm")) {
    # Check if the package is installed and attached.
    require_package(
      x = "VGAM",
      purpose = "to determine z-scores for multinomial regression models")

    if (is.null(mu)) mu <- VGAM::coefvlm(model)
    if (is.null(cov_matrix)) cov_matrix <- VGAM::vcovvlm(model)
    
  } else if (inherits(model, "fastglm")) {
    require_package(
      x = "fastglm",
      purpose = "to determine z-scores for generalised linear regression models")

    if (is.null(mu)) mu <- stats::coef(model)

    stdevs <- model$se
    names(stdevs) <- names(mu)
    
  } else if (inherits(model, "nnet")) {
    if (is.null(mu)) mu <- stats::coef(model)
    if (is.null(cov_matrix)) cov_matrix <- stats::vcov(model)
    
  } else {
    if (is.null(mu)) mu <- stats::coef(model)
    if (is.null(cov_matrix)) cov_matrix <- stats::vcov(model)
  }

  if (is.null(stdevs)) {
    if (is.matrix(mu)) {
      stdevs <- matrix(sqrt(diag(cov_matrix)), ncol = ncol(mu), byrow = TRUE)
    } else {
      stdevs <- sqrt(diag(cov_matrix))

      # Order standard deviations according to the estimates.
      if (is.null(names(stdevs))) {
        stdevs <- stdevs[seq_along(mu)]
      } else {
        stdevs <- stdevs[names(stdevs) %in% names(mu)][names(mu)]
      }
    }
  }

  # Compute z-score
  z <- mu / stdevs

  if (fix_all_missing && all(!is.finite(z))) z <- mu

  # Return z-score, as p-values can become very small.
  return(abs(z))
}



is_any <- function(object, class2) {
  # Tests whether the object is any of the classes in class2.
  return(any(sapply(
    class2,
    function(class, object) is(object, class2 = class),
    object = object)))
}



fivenum_summary <- function(x, na.rm = FALSE) {
  # Compute fivenumber summary
  y <- stats::fivenum(x = x, na.rm = na.rm)

  # Return as data.table
  return(data.table::data.table(
    "min" = y[1],
    "Q1" = y[2],
    "median" = y[3],
    "Q3" = y[4],
    "max" = y[5]))
}



trim <- function(x, fraction = 0.1) {
  if (fraction < 0.0 || fraction > 0.5) {
    stop("Trimming fraction should be between 0.0 and 0.5.")
  }

  # Determine thresholds based on fraction.
  threshold <- stats::quantile(
    x = x, 
    probs = c(fraction, 1 - fraction), 
    na.rm = TRUE)

  # Set mask
  x_mask <- x >= threshold[1] & x <= threshold[2]

  # Return trimmed array
  return(x[x_mask])
}



winsor <- function(x, fraction = 0.1) {
  if (fraction < 0.0 || fraction > 0.5) {
    stop("Winsoring fraction should be between 0.0 and 0.5.")
  }

  # Determine thresholds based on fraction.
  threshold <- stats::quantile(
    x = x, 
    probs = c(fraction, 1 - fraction),
    na.rm = TRUE)

  # Values outside valid range are assigned edge values
  x[x < threshold[1]] <- threshold[1]
  x[x > threshold[2]] <- threshold[2]

  return(x)
}



unique_na <- function(...) {
  values <- do.call(unique, args = list(...))

  return(values[!is.na(values)])
}



#' Create a waiver object
#'
#' This function is functionally identical to `ggplot2::waiver()` function and
#' creates a waiver object. A waiver object is an otherwise empty object that
#' serves the same purpose as `NULL`, i.e. as placeholder for a default value.
#' Because `NULL` can sometimes be a valid input argument, it can therefore not
#' be used to switch to an internal default value.
#'
#' @return waiver object
#' @export
#' @md
waiver <- function() {
  structure(list(), class = "waiver")
}



#' Internal test to see if an object is a waiver
#'
#' This function tests if the object was created by the `waiver` function. This
#' function is functionally identical to `ggplot2:::is.waive()`.
#'
#' @param x Object to be tested.
#'
#' @return `TRUE` for objects that are waivers, `FALSE` otherwise.
#' @md
#' @keywords internal
is.waive <- function(x) {
  return(inherits(x, "waiver"))
}



#' Encapsulate path
#'
#' This function is used to encapsulate paths to allow for behaviour switches.
#' One use is for example when plotting. The plot_all method will encapsulate a
#' path so that plots may be saved to a directory structure. Other plot methods,
#' e.g. plot_model_performance do not encapsulate a path, and if the user calls
#' these functions directly, the plot may be written to the provided path
#' instead of a directory structure.
#'
#' @return encapsulated_path object
#' @md
#' @keywords internal
encapsulate_path <- function(path) {
  structure(path, class = "encapsulated_path")
}



#' Internal test for encapsulated_path
#'
#' This function tests if the object is an `encapsulated_path` object.
#'
#' @param x Object to be tested.
#'
#' @return `TRUE` for objects that are `encapsulated_path`, `FALSE` otherwise.
#' @md
#' @keywords internal
is.encapsulated_path <- function(x) {
  return(inherits(x, "encapsulated_path"))
}



is_subclass <- function(class_1, class_2) {
  # Find out if class_1 is a subclass of class_2
  extending_classes <- methods::extends(class_1)

  # If class 2 is not in the classes from which class 1 inherits, class 1 cannot
  # be a subclass of class 2.
  if (!class_2 %in% extending_classes) return(FALSE)

  # If the classes are the same, class 1 cannot be a subclass of class 2.
  if (class_1 == class_2) return(FALSE)

  # The classes are ordered by distance. Therefore class 2 cannot be a subclass
  # of class 1 if it is less distant.
  if (extending_classes[1] == class_2) return(FALSE)

  return(TRUE)
}



.as_preprocessing_level <- function(x) {
  if (is(x, "dataObject")) x <- x@preprocessing_level

  # Available pre-processing levels
  preprocessing_levels <- c(
    "none",
    "signature",
    "transformation",
    "normalisation",
    "batch_normalisation",
    "imputation",
    "clustering")

  if (!all(x %in% preprocessing_levels)) {
    ..error_reached_unreachable_code(paste0(
      ".as_preprocessing_level: one or more of x could not be matched to ",
      "preprocessing levels."))
  }

  return(factor(
    x = x,
    levels = preprocessing_levels,
    ordered = TRUE))
}



.flatten_nested_list <- function(x, flatten = FALSE) {
  # Identify names of elements.
  element_names <- unique(unlist(lapply(x, names)))
  
  flattened_list <- lapply(
    element_names, 
    function(element_name, x, flatten) {
      
      element_content <- lapply(x, function(x) (x[[element_name]]))
      
      # Set names of elements
      if (!is.null(names(x))) {
        if (length(names(x)) == length(element_content)) {
          names(element_content) <- names(x)
        }
      }
      
      if (flatten) {
        element_content <- unlist(element_content, recursive = FALSE)
      }
      
      return(element_content)
    },
    x = x,
    flatten = flatten)
  
  names(flattened_list) <- element_names
  
  return(flattened_list)
}



dmapply <- function(FUN, ..., MoreArgs = NULL) {
  # mapply for use within data.table. This parses the result of FUN to a flat
  # list. data.table then adds the contents of the list as columns.

  # Apply function.
  x <- mapply(FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = FALSE)

  # Combine to data.table.
  x <- data.table::rbindlist(x, use.names = TRUE)

  # Return as list.
  return(as.list(x))
}



near <- function(x, y, df = 2.0, tol = .Machine$double.eps) {
  # Determines whether floating points values in x are very close to another
  # value in y.
  .near <- function(x, tol) (x <= tol)

  if (length(x) != length(y) && length(y) != 1) {
    stop("x and y should have the same length, or y should be a single value.")
  }

  if (!is.numeric(x)) {
    stop("x should be a numeric value.")
  }

  if (!is.numeric(y)) {
    stop("y should be a numeric value.")
  }

  return(sapply(abs(x - y), .near, tol = df * tol))
}



approximately <- function(x, y, tol = sqrt(.Machine$double.eps)) {
  # Determines whether numeric values in x are approximately y. This check is
  # the same as all.equal but without the all additional and checking that
  # all.equal does.
  #
  # approximately wraps near.
  return(near(
    x = x,
    y = y,
    df = 1.0,
    tol = tol))
}



huber_estimate <- function(x, k = 1.28, tol = 1E-4) {
  # k=1.28 is based on Wilcox RR. Introduction to Robust Estimation and
  # Hypothesis Testing. Academic Press; 2011.

  # Filter missing values
  x <- x[is.finite(x)]

  if (length(x) == 0) {
    return(list(
      "mu" = NA_real_,
      "sigma" = NA_real_))
  }

  # Initial estimates for the estimate mu and scale sigma
  mu_0 <- stats::median(x)
  sigma_0 <- stats::mad(x)

  # Check that there is an initial estimate for scale.
  if (sigma_0 == 0.0) {
    return(list(
      "mu" = mu_0,
      "sigma" = 0.0))
  }

  # Determine theta, i.e. the probability corresponding to k, according to a
  # Gaussian distribution.
  theta <- 2.0 * stats::pnorm(k) - 1.0
  beta <- theta + k^2 * (1 - theta) - 2.0 * k * stats::dnorm(k)

  for (ii in seq_len(50)) {
    # Apply Huber's rho, that basically winsorizes values |x| >= k * sigma_0
    xx <- pmin(pmax(mu_0 - k * sigma_0, x), mu_0 + k * sigma_0)

    # Compute updated mean and scale estimate.
    mu_1 <- sum(xx) / length(xx)
    sigma_1 <- sqrt(sum((xx - mu_1)^2) / (beta * (length(xx) - 1)))

    # Check convergence.
    if (abs(mu_0 - mu_1) < tol * mu_0 && 
        abs(sigma_0 - sigma_1) < tol * sigma_0) break

    # Values for next iteration
    mu_0 <- mu_1
    sigma_0 <- sigma_1
  }

  return(list(
    "mu" = mu_0, 
    "sigma" = sigma_0))
}

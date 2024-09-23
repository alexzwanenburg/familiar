#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


# familiarKNN ------------------------------------------------------------------
setClass(
  "familiarKNN",
  contains = "familiarModel")



# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "familiarKNN"),
  function(.Object, ...) {
    # Update with parent class first.
    .Object <- callNextMethod()

    # Set required package
    .Object@package <- c("e1071", "proxy")

    return(.Object)
  }
)



# is_available -----------------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarKNN"),
  function(object, ...) {
    # k-nearest neighbours is only available for categorical outcomes.
    if (object@outcome_type %in% c("binomial", "multinomial", "continuous")) {
      return(TRUE)
      
    } else if (object@outcome_type == "count") {
      ..deprecation_count()
      return(TRUE)
      
    }
    
    return(FALSE)
  }
)



# get_default_hyperparameters --------------------------------------------------
setMethod(
  "get_default_hyperparameters",
  signature(object = "familiarKNN"),
  function(object, data = NULL, ...) {
    # Initialise list and declare hyperparameter entries.
    param <- list()
    param$sign_size <- list()
    param$k <- list()
    param$distance_metric <- list()

    # If data is explicitly NULL, return the list with hyperparameter names
    # only.
    if (is.null(data)) return(param)

    # Get the number of unique series.
    n_samples <- data.table::uniqueN(
      data@data,
      by = get_id_columns(id_depth = "series"))

    # signature size -----------------------------------------------------------
    param$sign_size <- .get_default_sign_size(data = data)

    # number of nearest neighbours k -------------------------------------------

    # Define the range for the number of nearest neighbour clusters.
    k_range <- c(1, max(c(1, ceiling(2 * n_samples^(1 / 3)))))

    # Define the default value.
    k_default <- sort(unique(c(1, 2, 5, 10, 20, k_range)))
    k_default <- k_default[k_default >= k_range[1] & k_default <= k_range[2]]

    param$k <- .set_hyperparameter(
      default = k_default, type = "integer", range = k_range,
      valid_range = c(1L, Inf), randomise = TRUE)

    # distance metric ----------------------------------------------------------

    # Read distance_metric string by parsing the learner.
    distance_metric <- sub_all_patterns(
      x = object@learner,
      pattern = c("knn", "k_nearest_neighbours"), 
      replacement = "", 
      fixed = TRUE)
    
    if (distance_metric != "") {
      distance_metric <- sub(
        x = distance_metric, 
        pattern = "_", 
        replacement = "", 
        fixed = TRUE)
    }

    if (distance_metric == "") {
      # Detect feature columns present in the data.
      feature_columns <- get_feature_columns(x = data)

      # Set default values based on the data. For fully numeric data, use
      # euclidean by default, and gower otherwise.
      if (all(sapply(
        feature_columns, 
        function(ii, data) (is.numeric(data@data[[ii]])),
        data = data))) {
        distance_metric_default <- "euclidean"
        
      } else {
        distance_metric_default <- "gower"
      }
      
    } else {
      # A distance metric was unambiguously defined.
      distance_metric_default <- distance_metric
    }

    # Set default and valid ranges.
    distance_metric_default_range <- c("euclidean", "manhattan", "gower")
    # Do not generate additional distance metrics if proxy is not available.
    if (requireNamespace("proxy", quietly = TRUE)) {
      distance_metric_valid_range <- tolower(unname(unlist(lapply(
        proxy::pr_DB$get_entries(), 
        function(list_elem) (list_elem$names)))))
      
    } else {
      distance_metric_valid_range <- distance_metric_default_range
    }

    # Set distance metric.
    param$distance_metric <- .set_hyperparameter(
      default = distance_metric_default,
      type = "factor",
      range = distance_metric_default_range,
      valid_range = distance_metric_valid_range,
      randomise = distance_metric == "")

    return(param)
  }
)



# ..train ----------------------------------------------------------------------
setMethod(
  "..train",
  signature(
    object = "familiarKNN",
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

    # Find feature columns in the data.
    feature_columns <- get_feature_columns(x = data)

    # Parse formula
    formula <- stats::reformulate(
      termlabels = feature_columns,
      response = quote(outcome))

    # Train model. Disable scaling because of bad interaction with
    # predicting single instances.
    model <- do.call_with_handlers(
      e1071::gknn,
      args = list(formula,
        "data" = data@data,
        "k" = object@hyperparameters$k,
        "method" = as.character(object@hyperparameters$distance_metric),
        "scale" = FALSE))

    # Extract values.
    object <- ..update_warnings(object = object, model$warning)
    object <- ..update_errors(object = object, model$error)
    model <- model$value

    # Check if the model trained at all.
    if (!is.null(object@messages$error)) {
      return(callNextMethod(object = object))
    }

    # Add model to the familiarModel object.
    object@model <- model

    # Set learner version
    object <- set_package_version(object)

    return(object)
  }
)



# ..train_naive ----------------------------------------------------------------
setMethod(
  "..train_naive",
  signature(
    object = "familiarKNN",
    data = "dataObject"),
  function(object, data, ...) {
    if (object@outcome_type %in% c("count", "continuous", "binomial", "multinomial")) {
      # Turn into a naive model.
      object <- methods::new("familiarNaiveModel", object)
    }

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
    object = "familiarKNN",
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

      # Get an empty prediction table.
      prediction_table <- get_placeholder_prediction_table(
        object = object,
        data = data,
        type = type)

      if (object@outcome_type %in% c("binomial", "multinomial")) {
        # Binomial and multinomial outcomes-------------------------------------

        # Use the model to predict class probabilities.
        model_predictions <- predict(
          object = object@model,
          newdata = data@data,
          type = "prob")

        # Obtain class levels.
        class_levels <- get_outcome_class_levels(x = object)

        # Add class probabilities.
        class_probability_columns <- get_class_probability_name(x = object)
        for (ii in seq_along(class_probability_columns)) {
          prediction_table[, (class_probability_columns[ii]) := model_predictions[, class_levels[ii]]]
        }

        # Update predicted class based on provided probabilities.
        class_predictions <- class_levels[apply(
          prediction_table[, mget(class_probability_columns)], 1, which.max)]
        
        class_predictions <- factor(class_predictions, levels = class_levels)
        prediction_table[, "predicted_class" := class_predictions]
        
      } else if (object@outcome_type %in% c("continuous", "count")) {
        # Count and continuous outcomes ----------------------------------------

        # Use the model for prediction.
        model_predictions <- predict(
          object = object@model,
          newdata = data@data)

        # Set outcome.
        prediction_table[, "predicted_outcome" := model_predictions]
        
      } else {
        ..error_outcome_type_not_implemented(object@outcome_type)
      }

      return(prediction_table)
      
    } else {
      # User-specified method --------------------------------------------------
      # Note: the predict method for e1071::sknn specifies class, prob and
      # votes.

      # Check if the model was trained.
      if (!model_is_trained(object)) {
        return(NULL)
      }

      # Check if the data is empty.
      if (is_empty(data)) {
        return(NULL)
      }

      # Use the model for prediction.
      return(predict(
        object = object@model,
        newdata = data@data,
        type = type,
        ...))
    }
  }
)



# ..vimp -----------------------------------------------------------------------
# KNN does not have an associated variable importance method.


# .trim_model ------------------------------------------------------------------
setMethod(
  ".trim_model",
  signature(object = "familiarKNN"),
  function(object, ...) {
    # Note that knn by design always contains a copy of the dataset: it cannot
    # identify nearest neighbours otherwise.

    # Update model by removing the call.
    object@model$call <- call("trimmed")

    # Add show.
    object <- .capture_show(object)

    # Remove .Environment.
    object@model$terms <- .replace_environment(object@model$terms)

    # Set is_trimmed to TRUE.
    object@is_trimmed <- TRUE

    # Default method for models that lack a more specific method.
    return(object)
  }
)




.get_available_knn_learners <- function(
    show_general = TRUE,
    show_default = FALSE) {
  # General names without pre-specified distance metrics.
  general_learners <- c("k_nearest_neighbours", "knn")
  
  # Do not generate additional learners if proxy is not available.
  if (!requireNamespace("proxy", quietly = TRUE)) {
    return(general_learners)
  }
  
  # Define all distance metrics and the smaller default set.
  distance_metrics <- tolower(unname(unlist(
    lapply(
      proxy::pr_DB$get_entries(),
      function(list_elem) (list_elem$names)))))
  
  distance_metrics_default <- c("euclidean", "manhattan", "gower")
  
  # Append distance metrics.
  all_learners <- c(sapply(general_learners, paste, distance_metrics, sep = "_"))
  default_learners <- c(sapply(general_learners, paste, distance_metrics_default, sep = "_"))
  
  # Add general learners, if needed.
  if (show_general) {
    all_learners <- c(general_learners, all_learners)
    default_learners <- c(general_learners, default_learners)
  }
  
  # Return only default learners -- this is for unit testing.
  if (show_default) {
    return(default_learners)
  }
  
  return(all_learners)
}

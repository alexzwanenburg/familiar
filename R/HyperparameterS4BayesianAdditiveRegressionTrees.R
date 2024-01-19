#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R

setClass("familiarHyperparameterLearnerBART",
  contains = "familiarHyperparameterLearner",
  slots = list(
    "encoding_reference_table" = "ANY",
    "hyperparameter_order" = "character"),
  prototype = list(
    "encoding_reference_table" = NULL,
    "hyperparameter_order" = character()))




# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "familiarHyperparameterLearnerBART"),
  function(.Object, ...) {
    # Update with parent class first.
    .Object <- callNextMethod()

    # Set required package
    .Object@package <- "BART"

    # Set name
    .Object@name <- "Bayesian additive regression trees (BART) hyperparameter optimisation model"

    return(.Object)
  }
)



.get_available_bart_hyperparameter_learners <- function() {
  return(c("bayesian_additive_regression_trees", "bart"))
}



# get_prediction_type ----------------------------------------------------------
setMethod(
  "get_prediction_type",
  signature(object = "familiarHyperparameterLearnerBART"),
  function(object, ...) {
    return(c("default", "sd", "percentile", "raw"))
  }
)



# ..train ----------------------------------------------------------------------
setMethod(
  "..train",
  signature(
    object = "familiarHyperparameterLearnerBART",
    data = "data.table"),
  function(object, data, ...) {
    # Check if the training data is ok.
    if (has_bad_training_data(object = object, data = data)) {
      return(callNextMethod())
    }

    # Check that required packages are loaded and installed.
    require_package(object, "train")

    # Encode categorical variables.
    encoded_data <- encode_categorical_variables(
      data = data[, mget(object@target_hyperparameters)],
      object = NULL,
      encoding_method = "dummy",
      drop_levels = FALSE)

    # Get the hyperparameter names from the encoded data.table.
    hyperparameter_names <- colnames(encoded_data$encoded_data)

    # Identify invariant columns in the encoded data. These will be
    # dropped prior to training.
    invariant_columns <- sapply(encoded_data$encoded_data, is_singular_data)
    hyperparameter_names <- hyperparameter_names[!invariant_columns]

    # Check that are any non-invariant hyperparameters.
    if (length(hyperparameter_names) == 0) {
      return(callNextMethod())
    }

    # Get optimisation score as response.
    y <- data$optimisation_score

    # Create model. Note that prediction by BART is expensive for large
    # ndpost.
    quiet(model <- BART::wbart(
      x.train = data.frame(encoded_data$encoded_data[, mget(hyperparameter_names)]),
      y.train = y,
      ntree = 100,
      ndpost = 50))

    # Add model
    object@model <- model

    # Add the contrast references to the object.
    object@encoding_reference_table <- encoded_data$reference_table

    # Add hyperparameters that were used in the end.
    object@hyperparameter_order <- hyperparameter_names

    # Set learner version
    object <- set_package_version(object)

    return(object)
  }
)



# ..predict --------------------------------------------------------------------
setMethod(
  "..predict",
  signature(
    object = "familiarHyperparameterLearnerBART",
    data = "data.table"),
  function(
    object,
    data, 
    type = "default",
    percentile = NULL,
    ...) {
    # Check that required packages are loaded and installed.
    require_package(object, "predict")

    # Check if the model was trained.
    if (!model_is_trained(object)) {
      return(callNextMethod())
    }

    # Check if the data is empty.
    if (is_empty(data)) {
      return(callNextMethod())
    }

    # Get an empty prediction table.
    prediction_table <- .get_placeholder_hyperparameter_prediction_table(
      object = object,
      data = data,
      type = type)

    # Encode categorical variables.
    x_encoded <- encode_categorical_variables(
      data = data,
      object = NULL,
      encoding_method = "dummy",
      drop_levels = FALSE
    )$encoded_data

    # Get predicted values. object@hyperparameter_order is used to drop
    # any columns that were invariant during training.
    quiet(predicted_scores <- predict(
      object = object@model,
      data.frame(x_encoded[, mget(object@hyperparameter_order)])))

    # Compute mean and standard deviation.
    score_mean <- apply(predicted_scores, MARGIN = 2, mean)
    score_sd <- apply(predicted_scores, MARGIN = 2, stats::sd)

    # Separate by type
    if (type == "default") {
      prediction_table[, "mu" := score_mean]
      
    } else if (type == "sd") {
      prediction_table[, ":="("mu" = score_mean,
        "sigma" = score_sd)]
      
    } else if (type == "percentile") {
      # Compute the requested percentile.
      score_percentile <- apply(
        predicted_scores,
        MARGIN = 2,
        stats::quantile,
        probs = percentile,
        names = FALSE)

      prediction_table[, "percentile" := score_percentile]
      
    } else if (type == "raw") {
      # Drop raw_1 from the placeholder prediction table.
      prediction_table[, "raw_1" := NULL]

      # Parse raw data to a data.table with the expected output. Note
      # that we transpose the data to have row instances instead of
      # column instances.
      raw_data <- data.table::data.table(t(predicted_scores))

      # Set colnames.
      data.table::setnames(
        x = raw_data,
        new = paste0("raw_", seq_len(ncol(raw_data))))

      # Combine with the placeholder prediction table.
      prediction_table <- cbind(prediction_table, raw_data)
      
    } else {
      ..error_reached_unreachable_code(paste0(
        "..predict,familiarHyperparameterLearnerBART,data.table: ",
        "Encountered an unknown prediction type: ", type))
    }

    return(prediction_table)
  }
)

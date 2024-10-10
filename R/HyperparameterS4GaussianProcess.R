#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R


setClass("familiarHyperparameterLearnerLAGP",
  contains = "familiarHyperparameterLearner",
  slots = list("encoding_reference_table" = "ANY"),
  prototype = list("encoding_reference_table" = NULL)
)



# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "familiarHyperparameterLearnerLAGP"),
  function(.Object, ...) {
    # Update with parent class first.
    .Object <- callNextMethod()

    # Set required package
    .Object@package <- "laGP"

    # Set name
    .Object@name <- "Localised approximate Gaussian process (laGP) hyperparameter optimisation model"

    return(.Object)
  }
)



.get_available_lagp_hyperparameter_learners <- function() {
  return("gaussian_process")
}



# get_prediction_type ----------------------------------------------------------
setMethod(
  "get_prediction_type",
  signature(object = "familiarHyperparameterLearnerLAGP"),
  function(object, ...) {
    return(c("default", "sd"))
  }
)



# ..train ----------------------------------------------------------------------
setMethod(
  "..train",
  signature(
    object = "familiarHyperparameterLearnerLAGP",
    data = "data.table"
  ),
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
      drop_levels = FALSE
    )

    # Get optimisation score as response.
    y <- data$optimisation_score

    # Add model. Note that laGP basically builds and evaluates the
    # Gaussian process when predicting. We are just passing the
    # information at this point.
    object@model <- list(
      "X" = as.matrix(encoded_data$encoded_data),
      "Z" = y
    )

    # Set reference table for encoding.
    object@encoding_reference_table <- encoded_data$reference_table

    # Set learner version
    object <- set_package_version(object)

    return(object)
  }
)



# ..predict --------------------------------------------------------------------
setMethod(
  "..predict",
  signature(
    object = "familiarHyperparameterLearnerLAGP",
    data = "data.table"
  ),
  function(object, data, type = "default", ...) {
    # Check that required packages are loaded and installed.
    require_package(object, "predict")

    # Check if the model was trained.
    if (!model_is_trained(object)) return(callNextMethod())

    # Check if the data is empty.
    if (is_empty(data)) return(callNextMethod())

    # Get an empty prediction table.
    prediction_table <- .get_placeholder_hyperparameter_prediction_table(
      object = object,
      data = data,
      type = type
    )

    # Encode categorical variables.
    x_encoded <- encode_categorical_variables(
      data = data[, mget(object@target_hyperparameters)],
      object = NULL,
      encoding_method = "dummy",
      drop_levels = FALSE
    )$encoded_data

    # Update the end-size parameter, if the number of instances is
    # smaller than the default of 51. Note that we already capture
    # end_size of 7 or smaller using has_bad_training_data.
    end_size <- ifelse(nrow(object@model$X) < 51L, nrow(object@model$X) - 1L, 50L)

    # Infer scores for the hyperparameters.
    quiet(predicted_scores <- laGP::aGP(
      X = object@model$X,
      Z = object@model$Z,
      XX = as.matrix(x_encoded),
      end = end_size
    ))

    # Compute mean and standard deviation.
    score_mean <- predicted_scores$mean
    score_sd <- sqrt(predicted_scores$var)

    # Separate by type
    if (type == "default") {
      prediction_table[, "mu" := score_mean]
    } else if (type == "sd") {
      prediction_table[, ":="(
        "mu" = score_mean,
        "sigma" = score_sd
      )]
    } else if (type %in% c("percentile", "raw")) {
      ..error_reached_unreachable_code(paste0(
        "..predict,familiarHyperparameterLearnerLAGP,data.table: ",
        "Encountered a prediction type that is not supported by the model: ", type
      ))
    } else {
      ..error_reached_unreachable_code(paste0(
        "..predict,familiarHyperparameterLearnerLAGP,data.table: ",
        "Encountered an unknown prediction type: ", type
      ))
    }

    return(prediction_table)
  }
)



# has_bad_training_data --------------------------------------------------------
setMethod(
  "has_bad_training_data",
  signature(
    object = "familiarHyperparameterLearnerLAGP",
    data = "data.table"
  ),
  function(object, data, ...) {
    # Perform the general check first.
    if (callNextMethod()) return(TRUE)

    # For Gaussian process at least eight instances should be present.
    if (nrow(data) < 8L) return(TRUE)

    return(FALSE)
  }
)

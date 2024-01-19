#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R



# promote_learner (hyperparameter learner) -------------------------------------
setMethod(
  "promote_learner",
  signature(object = "familiarHyperparameterLearner"),
  function(object) {
    learner <- object@learner

    if (learner %in% .get_available_ranger_hyperparameter_learners()) {
      # Ranger random forest
      object <- methods::new("familiarHyperparameterLearnerRanger", object)
      
    } else if (learner %in% .get_available_lagp_hyperparameter_learners()) {
      # Localised approximate Gaussian process.
      object <- methods::new("familiarHyperparameterLearnerLAGP", object)
      
    } else if (learner %in% .get_available_bart_hyperparameter_learners()) {
      # Bayesian additive regression trees
      object <- methods::new("familiarHyperparameterLearnerBART", object)
      
    } else if (learner %in% .get_available_random_search_hyperparameter_learners()) {
      # Random search
      object <- methods::new("familiarHyperparameterRandomSearch", object)
      
    } else {
      ..error_reached_unreachable_code(paste0(
        "promote_learner,familiarHyperparameterLearner: encountered ",
        "unknown hyperparameter learner: ", learner))
    }

    # Add package version.
    object <- add_package_version(object = object)

    # Return promoted object.
    return(object)
  }
)



# .train (hyperparameter learner) ----------------------------------------------
setMethod(
  ".train",
  signature(
    object = "familiarHyperparameterLearner",
    data = "data.table"),
  function(
    object,
    data,
    parameter_data = NULL,
    ...) {
    # Train method for training hyperparameter models

    # Suppress NOTES due to non-standard evaluation in data.table
    optimisation_score <- NULL

    # Check if the class of object is a subclass of
    # familiarHyperparameterLearner.
    if (!is_subclass(class(object)[1], "familiarHyperparameterLearner")) {
      object <- promote_learner(object)
    }

    # Check whether data contains the optimisation_score column.
    if (!"optimisation_score" %in% colnames(data)) {
      data <- .compute_hyperparameter_optimisation_score(
        score_table = data,
        optimisation_function = object@optimisation_function)
    }

    # Merge data and parameter_data on param_id.
    if (!is.null(parameter_data)) {
      data <- merge(
        x = data,
        y = parameter_data,
        all.x = TRUE,
        all.y = FALSE,
        by = "param_id")
    }

    # Replace NA entries with the minimum optimisation score.
    data[is.na(optimisation_score), optimisation_score := -1.0]

    # Get hyperparameter names from the target learner.
    default_hyperparameters <- .get_learner_hyperparameters(
      data = NULL,
      learner = object@target_learner,
      outcome_type = object@target_outcome_type,
      names_only = TRUE)

    # Set hyperparameters in data.
    target_hyperparameters <- intersect(colnames(data), default_hyperparameters)
    if (length(target_hyperparameters) == 0) {
      stop(paste0(
        "The provided hyperparameter data does not contain any of the ",
        "hyperparameters expected by the learner. The following hyperparameters ",
        "are used: ", paste_s(default_hyperparameters)))
    }
    object@target_hyperparameters <- target_hyperparameters

    if (has_bad_training_data(object = object, data = data)) {
      # Create a random search object in case training data are bad in
      # some way or form.
      object@learner <- "random_search"
      object <- methods::new("familiarHyperparameterRandomSearch", object)
    }

    # Train a new model based on data.
    object <- ..train(
      object = object,
      data = data,
      ...)

    # Add familiar version.
    object <- add_package_version(object)

    return(object)
  }
)


# .predict (hyperparameter learner) --------------------------------------------
setMethod(
  ".predict",
  signature(
    object = "familiarHyperparameterLearner",
    data = "data.table"),
  function(
    object,
    data,
    type = "default",
    percentile = NULL,
    ...) {
    # Predict method for hyperparameter models. The type argument can be default
    # (reports the model estimate), sd (reports the model estimate + standard
    # deviation), percentile, in which case the model estimate and the requested
    # percentile are returned, and raw, which returns the data in a wide format.
    # Not all learners support the same types. Use the get_prediction_type
    # method to figure out which type of output can be generated.

    # Dispatch to the ..predict methods of the child classes.
    prediction_table <- ..predict(
      object = object,
      data = data,
      type = type,
      percentile = percentile
    )

    return(prediction_table)
  }
)



# get_prediction_type ----------------------------------------------------------
setMethod(
  "get_prediction_type",
  signature(object = "familiarHyperparameterLearner"),
  function(object, ...) {
    # Placeholder for hyperparameter learners.
    return(NULL)
  }
)



# show (hyperparameter learner) ------------------------------------------------
setMethod(
  "show",
  signature(object = "familiarHyperparameterLearner"),
  function(object) {
    if (!model_is_trained(object)) {
      # Describe the learner and the version of familiar.
      cat(paste0(
        "A ", object@learner, " model (class: ", class(object)[1],
        ") for inferring hyperparameters of the ", object@target_learner, ". ",
        "This hyperparameter model could not successfully be trained (",
        .familiar_version_string(object), ").\n"))
      
      return(invisible(NULL))
    }
    
    # Describe the learner and the version of familiar.
    message_str <- paste0(
      "A ", object@learner, " model (class: ", class(object)[1],
      "; ", .familiar_version_string(object), ") ",
      "for inferring hyperparameters of the ", object@target_learner,
      " learner. ")
    
    # Describe the package(s), if any.
    if (!is.null(object@package)) {
      message_str <- c(
        message_str,
        paste0("This model was trained using "),
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
    show(object@model)
    
    cat(paste0("---------------------------------------------\n"))
    
    # Details concerning hyperparameters.
    cat(paste0(
      "\nThe model was trained to infer the optimisation ",
      "score of the following hyperparameter set:\n",
      paste_s(object@target_hyperparameters), "\n"))
    
    cat(paste0(
      "\nOptimisation scores were determined using the ",
      object@optimisation_function, " method, ",
      "based on assessment of model performance using the ",
      paste_s(object@optimisation_metric),
      ifelse(length(object@optimisation_metric) > 1, " metrics", " metric"),
      ".\n"))
    
    # Check package version.
    check_package_version(object)
  }
)



# require_package (hyperparameter learner) -------------------------------------
setMethod(
  "require_package",
  signature(x = "familiarHyperparameterLearner"),
  function(
    x,
    purpose = NULL, 
    message_type = "error",
    ...) {
    # Skip if no package is required.
    if (is_empty(x@package)) return(invisible(TRUE))

    # Set standard purposes for common uses.
    if (!is.null(purpose)) {
      if (purpose %in% c("train", "predict", "show")) {
        purpose <- switch(purpose,
          "train" = "to train a model for inferring hyperparameter scores",
          "predict" = "to infer hyperparameter scores",
          "show" = "to capture output of the hyperparameter model"
        )
      }
    }

    return(invisible(.require_package(
      x = x@package,
      purpose = purpose,
      message_type = message_type)))
  }
)



# set_package_version (hyperparameter learner) ---------------------------------
setMethod(
  "set_package_version",
  signature(object = "familiarHyperparameterLearner"),
  function(object) {
    # Do not add package versions if there are no packages.
    if (is_empty(object@package)) return(object)

    # Obtain package versions.
    object@package_version <- sapply(
      object@package, 
      function(x) (as.character(utils::packageVersion(x))))

    return(object)
  }
)



# check_package_version (hyperparameter learner) -------------------------------
setMethod(
  "check_package_version",
  signature(object = "familiarHyperparameterLearner"),
  function(object) {
    .check_package_version(
      name = object@package,
      version = object@package_version,
      when = "at model creation")
  }
)



# model_is_trained (hyperparameter learner) ------------------------------------
setMethod(
  "model_is_trained",
  signature(object = "familiarHyperparameterLearner"),
  function(object) {
    # Check if a model was trained.
    if (is.null(object@model)) {
      # If no model is attached to the object, assume that no model was trained.
      return(FALSE)
      
    } else {
      # If the the model is not NULL, assume that it is present.
      return(TRUE)
    }
  }
)



# add_package_version (hyperparameter learner) ---------------------------------
setMethod(
  "add_package_version",
  signature(object = "familiarHyperparameterLearner"),
  function(object) {
    # Set version of familiar
    return(.add_package_version(object = object))
  }
)


# ..train (hyperparameter learner, data) ---------------------------------------
setMethod(
  "..train",
  signature(
    object = "familiarHyperparameterLearner",
    data = "ANY"),
  function(object, data, ...) {
    # This method is basically a capture-all for when model training does not
    # succeed. Normally ..train calls refer to child classes. This method is
    # then only called when something goes wrong there.

    # Set a NULL model
    object@model <- NULL

    return(object)
  }
)


# ..predict (hyperparameter learner, data) -------------------------------------
setMethod(
  "..predict",
  signature(
    object = "familiarHyperparameterLearner",
    data = "data.table"),
  function(object, data, type = "default", ...) {
    # Like ..train, this method is a capture-all for when predictions fail.
    return(.get_placeholder_hyperparameter_prediction_table(
      object = object,
      data = data,
      type = type
    ))
  }
)


# has_bad_training_data --------------------------------------------------------
setMethod(
  "has_bad_training_data",
  signature(
    object = "familiarHyperparameterLearner",
    data = "data.table"),
  function(object, data, ...) {
    # One cannot train without data or on a single sample.
    if (is_empty(data)) return(TRUE)

    # Get identifier columns in the data.
    id_columns <- intersect(c("param_id", "run_id"), colnames(data))
    if (length(id_columns) > 0) {
      # Check that at least two unique entries are present.
      if (data.table::uniqueN(data, by = c(id_columns)) < 2) return(TRUE)
      
    } else {
      # Check that at least two rows are present.
      if (nrow(data) < 2) return(TRUE)
    }

    # Check if all data are non-singular.
    if (all(sapply(data[, mget(object@target_hyperparameters)], is_singular_data))) {
      return(TRUE)
    }

    return(FALSE)
  }
)



.check_hyperparameter_learner_available <- function(
    hyperparameter_learner,
    as_flag = FALSE) {
  # Create familiarHyperparameterLearner object.
  fam_hyperparameter_model <- methods::new(
    "familiarHyperparameterLearner",
    learner = hyperparameter_learner)

  # Set up the specific novelty detector
  fam_hyperparameter_model <- promote_learner(fam_hyperparameter_model)

  # Check if the familiar model has been successfully promoted.
  if (!is_subclass(
    class(fam_hyperparameter_model)[1],
    "familiarHyperparameterLearner")) {
    
    stop(paste0(
      hyperparameter_learner, " is not a valid hyperparameter learner. ",
      "Please check the vignette for available hyperparameter learners."))
  }

  # Check that the required package can be loaded.
  require_package(
    x = fam_hyperparameter_model,
    purpose = "train",
    message_type = "backend_error")
}



.get_placeholder_hyperparameter_prediction_table <- function(object, data, type = "default") {
  # Find the id columns.
  id_columns <- intersect(c("param_id", "run_id"), colnames(data))
  
  if (length(id_columns) > 0) {
    # Create a placeholder by only keeping the identifier columns.
    prediction_table <- data.table::copy(data[, mget(id_columns)])
    
  } else {
    # Add a placeholder parameter identifier as scaffolding.
    prediction_table <- data.table::data.table(param_id = rep_len(NA_integer_, nrow(data)))
  }
  
  # Add placeholder columns.
  if (type == "default") {
    prediction_table[, "mu" := as.double(NA)]
    
  } else if (type == "sd") {
    prediction_table[, ":="(
      "mu" = as.double(NA),
      "sigma" = as.double(NA))]
    
  } else if (type == "percentile") {
    prediction_table[, "percentile" := as.double(NA)]
    
  } else if (type == "raw") {
    prediction_table[, "raw_1" := as.double(NA)]
    
  } else {
    ..error_reached_unreachable_code(paste0(
      ".get_placeholder_hyperparameter_prediction_table: ",
      "Encountered an unknown prediction type: ", type
    ))
  }
  
  return(prediction_table)
}

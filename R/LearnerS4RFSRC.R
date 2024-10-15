#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


# familiarRFSRC ----------------------------------------------------------------
setClass("familiarRFSRC",
  contains = "familiarModel",
  slots = list("seed" = "ANY"),
  prototype = list("seed" = NULL)
)



# familiarRFSRCDefault ---------------------------------------------------------
setClass("familiarRFSRCDefault",
  contains = "familiarRFSRC"
)



# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "familiarRFSRC"),
  function(.Object, ...) {
    # Update with parent class first.
    .Object <- callNextMethod()

    # Set package
    .Object@package <- "randomForestSRC"

    return(.Object)
  }
)



# is_available -----------------------------------------------------------------
setMethod(
  "is_available", signature(object = "familiarRFSRC"),
  function(object, ...) {
    # Random forests exists for all outcome types and variable importance
    # methods.
    if (object@outcome_type == "count") {
      ..deprecation_count()
      return(FALSE)
    }
    
    return(TRUE)
  }
)



# get_default_hyperparameters (familiarRFSRC) ----------------------------------
setMethod(
  "get_default_hyperparameters",
  signature(object = "familiarRFSRC"),
  function(object, data = NULL, user_list = NULL, ...) {
    # Initialise list and declare hyperparameter entries
    param <- list()
    param$sign_size <- list()
    param$n_tree <- list()
    param$sample_size <- list()
    param$m_try <- list()
    param$node_size <- list()
    param$tree_depth <- list()
    param$n_split <- list()
    param$split_rule <- list()
    param$sample_weighting <- list()
    param$sample_weighting_beta <- list()

    # Variable importance only parameters (are not optimised by hyperparameter
    # optimisation)
    param$fs_vimp_method <- list()

    # If datat is not provided, return the list with hyperparameter names only
    if (is.null(data)) return(param)

    # Get the number of samples
    n_samples <- data.table::uniqueN(data@data, by = get_id_columns(id_depth = "series"))

    # signature size -----------------------------------------------------------
    param$sign_size <- .get_default_sign_size(data = data)

    # number of trees ----------------------------------------------------------

    # Note that the number of trees is defined in powers of 2, based on Oshiro,
    # T. M., Perez, P. S., & Baranauskas, J. A. (2012, July). How many trees in
    # a random forest?. In MLDM (pp. 154-168).
    param$n_tree <- .set_hyperparameter(
      default = c(4L, 8L, 10L),
      type = "integer",
      range = c(4L, 10L),
      valid_range = c(0L, Inf),
      randomise = TRUE
    )

    # sample size --------------------------------------------------------------

    # Note that the sample size is here noted as a fraction, which corresponds
    # to the usage in random forest SRC
    param$sample_size <- .set_hyperparameter(
      default = c(0.30, 0.50, 0.70, 0.90),
      type = "numeric",
      range = c(2.0 / n_samples, 1.0),
      valid_range = c(0.0, 1.0),
      randomise = TRUE
    )

    # number of candidate features selected at node ----------------------------

    # Note that the number of features is here noted as a fraction, but is used
    # in randomforestSRC as an integer. Familiar ensures that always at least 1
    # feature is available as a candidate.
    param$m_try <- .set_hyperparameter(
      default = c(0.1, 0.3, 0.5, 1.0),
      type = "numeric",
      range = c(0.0, 1.0),
      randomise = TRUE
    )
    
    # terminal node size -------------------------------------------------------

    # Number of instances in the terminal node. Larger terminal node sizes limit
    # tree depth and overfitting.

    # Define the default range.
    node_size_range <- c(5L, as.integer(max(c(5L, floor(n_samples / 3L)))))

    # Define the default values.
    node_size_default <- c(5L, 10L, 20L, 50L)
    node_size_default <- node_size_default[
      node_size_default >= node_size_range[1L] &
        node_size_default <= node_size_range[2L]
    ]

    # Set the node_size parameter.
    param$node_size <- .set_hyperparameter(
      default = node_size_default,
      type = "integer",
      range = node_size_range,
      valid_range = c(1L, Inf),
      randomise = TRUE
    )

    # maximum tree depth -------------------------------------------------------

    # Determines the depth trees are allowed to grow to. Larger depths increase
    # the risk of overfitting.
    param$tree_depth <- .set_hyperparameter(
      default = c(1L, 2L, 3L, 7L),
      type = "integer",
      range = c(1L, 10L),
      valid_range = c(1L, Inf),
      randomise = TRUE
    )

    # number of split points ---------------------------------------------------

    # The number of split points for each candidate variable is not randomised
    # by default, and deterministic splitting is used.
    param$n_split <- .set_hyperparameter(
      default = 0L,
      type = "integer",
      range = c(0L, 10L),
      valid_range = c(0L, Inf),
      randomise = FALSE
    )

    # splitting rule -----------------------------------------------------------

    # Splitting rule is dependent on the outcome
    if (data@outcome_type %in% c("binomial", "multinomial")) {
      split_rule_range <- c("gini", "auc", "entropy")
      split_rule_default <- "gini"
    } else if (data@outcome_type %in% c("continuous")) {
      split_rule_range <- c("mse", "quantile.regr", "la.quantile.regr")
      split_rule_default <- "mse"
    } else if (data@outcome_type == "survival") {
      split_rule_range <- c("logrank", "logrankscore", "bs.gradient")
      split_rule_default <- "logrank"
    } else if (data@outcome_type == "competing_risk") {
      split_rule_range <- c("logrankCR", "logrank")
      split_rule_default <- "logrankCR"
    } else {
      ..error_no_known_outcome_type(data@outcome_type)
    }

    # Set the split_rule parameter.
    param$split_rule <- .set_hyperparameter(
      default = split_rule_default,
      type = "factor",
      range = split_rule_range,
      randomise = FALSE
    )

    # sample weighting method --------------------------------------------------
    
    # Class imbalances may lead to learning majority classes. This can be
    # partially mitigated by increasing weight of minority classes.
    param$sample_weighting <- .get_default_sample_weighting_method(
      outcome_type = object@outcome_type
    )


    # effective number of samples beta -----------------------------------------
    
    # Specifies the beta parameter for effective number sample weighting method.
    # See Cui et al. (2019).
    param$sample_weighting_beta <- .get_default_sample_weighting_beta(
      method = c(
        param$sample_weighting$init_config,
        user_list$sample_weighting
      ),
      outcome_type = object@outcome_type
    )

    # variable importance method -----------------------------------------------
    param$fs_vimp_method <- .set_hyperparameter(
      default = "permutation",
      type = "factor",
      range = c("permutation", "minimum_depth", "holdout"),
      randomise = FALSE
    )

    return(param)
  }
)



# get_default_hyperparameters (familiarRFSRCDefault) ---------------------------
setMethod(
  "get_default_hyperparameters",
  signature(object = "familiarRFSRCDefault"),
  function(
    object, 
    data = NULL, 
    user_list = NULL, 
    ...
  ) {
    # Initialise list and declare hyperparameter entries
    param <- list()
    param$sign_size <- list()
    param$sample_weighting <- list()
    param$sample_weighting_beta <- list()

    # Variable importance only parameters (are not optimised by hyperparameter
    # optimisation)
    param$fs_vimp_method <- list()

    # If data is not provided, return the list with hyperparameter names only.
    if (is.null(data)) return(param)

    # signature size -----------------------------------------------------------
    param$sign_size <- .get_default_sign_size(data = data)

    # sample weighting method --------------------------------------------------
    
    # Class imbalances may lead to learning majority classes. This can be
    # partially mitigated by increasing weight of minority classes.
    param$sample_weighting <- .get_default_sample_weighting_method(
      outcome_type = object@outcome_type
    )

    # effective number of samples beta -----------------------------------------
    
    # Specifies the beta parameter for effective number sample weighting method.
    # See Cui et al. (2019).
    param$sample_weighting_beta <- .get_default_sample_weighting_beta(
      method = c(
        param$sample_weighting$init_config,
        user_list$sample_weighting
      ),
      outcome_type = object@outcome_type
    )

    # variable importance method -----------------------------------------------
    param$fs_vimp_method <- .set_hyperparameter(
      default = "permutation",
      type = "factor",
      range = c("permutation", "minimum_depth", "holdout"),
      randomise = FALSE
    )

    return(param)
  }
)



# get_prediction_type ----------------------------------------------------------
setMethod(
  "get_prediction_type",
  signature(object = "familiarRFSRC"),
  function(object, type = "default") {
    if (object@outcome_type == "survival") {
      if (type == "default") {
        # Standard predictions.
        return("cumulative_hazard")
      } else if (type %in% c("survival", "survival_probability")) {
        return("survival_probability")
      } else if (type %in% c("response", "cumulative_hazard")) {
        return("cumulative_hazard")
      } else {
        ..error(paste0("Prediction type is not implemented: ", type))
      }
      
    } else {
      return(callNextMethod())
    }
  }
)



# ..train ----------------------------------------------------------------------
setMethod(
  "..train", 
  signature(
    object = "familiarRFSRC",
    data = "dataObject"
  ),
  function(
    object, 
    data, 
    anonymous = TRUE, 
    ...
  ) {
    # Aggregate repeated measurement data - randomForestSRC does not facilitate
    # repeated measurements.
    data <- aggregate_data(data = data)

    # Check if training data is ok.
    if (reason <- has_bad_training_data(object = object, data = data)) {
      return(callNextMethod(object = .why_bad_training_data(
        object = object, 
        reason = reason
      )))
    }

    # Check if hyperparameters are set.
    if (is.null(object@hyperparameters)) {
      return(callNextMethod(object = ..update_errors(
        object = object,
        ..error_message_no_optimised_hyperparameters_available()
      )))
    }

    # Check that required packages are loaded and installed.
    require_package(object, "train")

    # Find feature columns in data table
    feature_columns <- get_feature_columns(x = data)

    # Parse formula.
    if (object@outcome_type == "survival") {
      Surv <- survival::Surv
      formula <- stats::reformulate(
        termlabels = feature_columns,
        response = quote(Surv(outcome_time, outcome_event))
      )
      
    } else if (object@outcome_type %in% c("binomial", "multinomial", "continuous")) {
      formula <- stats::reformulate(
        termlabels = feature_columns,
        response = quote(outcome)
      )
      
    } else {
      ..error_outcome_type_not_implemented(object@outcome_type)
    }

    # Extract hyperparameters from the model object.
    param <- object@hyperparameters

    # Determine the sample size
    sample_size <- ceiling(param$sample_size * nrow(data@data))
    sample_type <- ifelse(sample_size == nrow(data@data), "swr", "swor")

    # Set weights.
    weights <- create_instance_weights(
      data = data,
      method = object@hyperparameters$sample_weighting,
      beta = ..compute_effective_number_of_samples_beta(
        object@hyperparameters$sample_weighting_beta
      ),
      normalisation = "average_one"
    )

    # Set forest seed. If object comes with a defined seed use the seed.
    forest_seed <- ifelse(
      is.null(object@seed),
      as.integer(stats::runif(1L, -100000L, -1L)),
      object@seed
    )

    # Use anonymised version for the forest, if available.
    if (utils::packageVersion("randomForestSRC") >= "2.11.0" && anonymous) {
      forest_function <- randomForestSRC::rfsrc.anonymous
    } else {
      forest_function <- randomForestSRC::rfsrc
    }

    # Set learner arguments.
    learner_arguments <- list(formula,
      "data" = data@data,
      "case.wt" = weights,
      "seed" = forest_seed
    )

    # Add additional hyperparameters for models that use them.
    if (!is(object, "familiarRFSRCDefault")) {
      learner_arguments <- c(
        learner_arguments,
        list(
          "ntree" = 2L^param$n_tree,
          "samptype" = sample_type,
          "sampsize" = sample_size,
          "mtry" = max(c(1L, ceiling(param$m_try * length(feature_columns)))),
          "nodesize" = param$node_size,
          "nodedepth" = param$tree_depth,
          "nsplit" = param$n_split,
          "splitrule" = as.character(param$split_rule)
        )
      )
    }
    
    # Train the model.
    model <- do.call_with_handlers(
      forest_function,
      args = learner_arguments
    )

    # Extract values.
    object <- ..update_warnings(object = object, model$warning)
    object <- ..update_errors(object = object, model$error)
    model <- model$value

    # Check if the model trained at all.
    if (!is.null(object@messages$error)) return(callNextMethod(object = object))

    # Add model to the object.
    object@model <- model

    # Store the seed used to create the object. This helps recreate the object
    # in case the model is an anonymous forest.
    object@seed <- forest_seed

    # Set learner version
    object <- set_package_version(object)

    return(object)
  }
)



# ..train_naive ----------------------------------------------------------------
setMethod(
  "..train_naive",
  signature(
    object = "familiarRFSRC",
    data = "dataObject"
  ),
  function(object, data, ...) {
    if (object@outcome_type %in% c("continuous", "binomial", "multinomial")) {
      # Turn into a Naive model.
      object <- methods::new("familiarNaiveModel", object)
      
    } else if (object@outcome_type %in% c("survival")) {
      # Turn into a Naive cumulative incidence model.
      object <- methods::new("familiarNaiveCumulativeIncidenceModel", object)
    }

    return(..train(
      object = object,
      data = data,
      ...
    ))
  }
)



# ..predict --------------------------------------------------------------------
setMethod(
  "..predict",
  signature(
    object = "familiarRFSRC",
    data = "dataObject"
  ),
  function(
    object,
    data, 
    type = "default",
    time = NULL, 
    ...
  ) {
    # Check that required packages are loaded and installed.
    require_package(object, "predict")

    # Check if the model was trained.
    if (!model_is_trained(object)) {
      return(NULL)
    }
    
    # Check if the data is empty.
    if (is_empty(data)) {
      return(NULL)
    }
    
    if (type %in% c("default", "survival_probability")) {
      # Default method ---------------------------------------------------------

      # Make predictions using the model.
      model_predictions <- predict(
        object = object@model,
        newdata = data@data
      )

      if (object@outcome_type %in% c("binomial", "multinomial")) {
        # categorical outcomes -------------------------------------------------

        # Obtain class levels.
        class_levels <- colnames(model_predictions$predicted)
        
        # Add class probabilities.
        prediction_list <- list()
        for (ii in seq_along(class_levels)) {
          prediction_list[[class_levels[ii]]] <- model_predictions$predicted[, ii]
        }
        
        # Store as prediction table.
        prediction_table <- as_prediction_table(
          x = prediction_list,
          type = "classification",
          data = data,
          model_object = object
        )
        
      } else if (object@outcome_type %in% c("continuous")) {
        # numerical outcomes ---------------------------------------------------

        # Store as prediction table.
        prediction_table <- as_prediction_table(
          x = as.numeric(model_predictions$predicted),
          type = "regression",
          data = data,
          model_object = object
        )
        
      } else if (object@outcome_type %in% c("survival")) {
        # survival outcomes ----------------------------------------------------

        # Get the unique event times
        event_times <- model_predictions$time.interest

        # Set default time, if not provided.
        time <- ifelse(is.null(time), max(event_times), time)

        if (type == "default") {
          # Cumulative hazard.

          # Get the cumulative hazards at the given time point.
          prediction_table <- .random_forest_survival_predictions(
            object = object,
            event_matrix = model_predictions$chf,
            event_times = event_times,
            data = data,
            time = time,
            type = "cumulative_hazard"
          )
          
        } else if (type == "survival_probability") {
          # Survival probability.

          # Get the survival probability at the given time point.
          prediction_table <- .random_forest_survival_predictions(
            object = object,
            event_matrix = model_predictions$survival,
            event_times = event_times,
            data = data,
            time = time,
            type = "survival"
          )
          
        } else {
          ..error_outcome_type_not_implemented(object@outcome_type)
        }
      }

      return(prediction_table)
      
    } else if (!.is_available_prediction_type(type)) {
      # User-specified method --------------------------------------------------

      # Make predictions using the model.
      return(predict(
        object = object@model,
        newdata = data@data,
        ...
      ))
      
    } else {
      ..error_no_predictions_possible(object, type)
    }
  }
)



# ..get_prediction_table_type --------------------------------------------------
setMethod(
  "..get_prediction_table_type",
  signature(object = "familiarRFSRC"),
  function(object, type, ...) {
    prediction_table_type <- NULL
    if (object@outcome_type %in% c("survival") && type == "default") {
      prediction_table_type <- "cumulative_hazard"
    } else {
      prediction_table_type <- callNextMethod()
    }
    
    return(prediction_table_type)
  }
)



# ..vimp -----------------------------------------------------------------------
setMethod(
  "..vimp",
  signature(object = "familiarRFSRC"),
  function(object, data = NULL, ...) {
    
    # Attempt to train the model if it has not been trained yet.
    if (!model_is_trained(object)) {
      object <- .train(
        object = object,
        data = data,
        get_additional_info = FALSE,
        trim_model = FALSE,
        anonymous = FALSE
      )
    }

    # Check if the model has been trained upon retry.
    if (!model_is_trained(object)) return(callNextMethod())

    # Check that required packages are loaded and installed.
    require_package(object, "vimp")

    # Find the vimp_method specified.
    vimp_method <- object@hyperparameters$fs_vimp_method
    if (is.null(vimp_method)) {
      ..error_reached_unreachable_code(paste0(
        "..vimp,familiarRFSRC: vimp method was not specified as a ",
        "hyperparameter (fs_vimp_method)"
      ))
    }
    
    vimp_method <- as.character(vimp_method)

    # Extract the variable importance score
    if (vimp_method == "permutation") {
      # Check if the model is anonymous, and rebuild if it is. VIMP does not
      # work otherwise.
      if (inherits(object@model, "anonymous")) {
        object <- .train(
          object = object,
          data = data,
          get_additional_info = FALSE,
          trim_model = FALSE,
          anonymous = FALSE
        )

        # Check that the non-anonymous model is trained.
        if (!model_is_trained(object)) return(callNextMethod())
      }

      # Determine permutation variable importance
      vimp_score <- randomForestSRC::vimp(
        object = object@model,
        importance = "permute"
      )$importance

      # Check that the variable importance score is not empty.
      if (is_empty(vimp_score)) return(callNextMethod())

      # The variable importance score for binomial and multinomial
      # outcomes is per class
      if (is.matrix(vimp_score)) {
        vimp_score_names <- rownames(vimp_score)
        vimp_score <- vimp_score[, 1L]
        
      } else {
        vimp_score_names <- names(vimp_score)
      }

      # Create variable importance object.
      vimp_object <- methods::new(
        "vimpTable",
        vimp_table = data.table::data.table(
          "score" = vimp_score, 
          "name" = vimp_score_names
        ),
        score_aggregation = "max",
        invert = TRUE
      )

      return(vimp_object)
      
    } else if (vimp_method == "minimum_depth") {
      # Check if the model is anonymous, and rebuild if it is. VIMP does not
      # work otherwise.
      if (inherits(object@model, "anonymous")) {
        object <- .train(
          object = object,
          data = data,
          get_additional_info = FALSE,
          trim_model = FALSE,
          anonymous = FALSE
        )

        # Check that the non-anonymous model is trained.
        if (!model_is_trained(object)) return(callNextMethod())
      }

      # Determine minimum depth variable importance
      vimp_score <- randomForestSRC::var.select(
        object = object@model,
        method = "md",
        verbose = FALSE
      )$md.obj$order

      # Check that the variable importance score is not empty.
      if (is_empty(vimp_score)) return(callNextMethod())

      # Select the "min depth" column, which is the first column
      if (is.matrix(vimp_score)) {
        vimp_score_names <- rownames(vimp_score)
        vimp_score <- vimp_score[, 1L]
        
      } else {
        vimp_score_names <- names(vimp_score)
      }

      # Create variable importance object.
      vimp_object <- methods::new(
        "vimpTable",
        vimp_table = data.table::data.table(
          "score" = vimp_score, 
          "name" = vimp_score_names
        ),
        score_aggregation = "max",
        invert = FALSE
      )

      return(vimp_object)
      
    } else if (vimp_method == "variable_hunting") {
      ..deprecation_rfsrc_variable_hunting()
      return(callNextMethod())
      
    } else if (vimp_method == "holdout") {
      # Check that data is present.
      if (is_empty(data)) {
        warning(paste0(
          "Data is required to assess variable importance using ",
          "randomForestRFSRC::holdout.vimp"
        ))
        return(callNextMethod())
      }

      if (get_n_features(data) <= 1L) {
        warning(paste0(
          "Variable importance using randomForestRFSRC::holdout.vimp ",
          "requires more than 1 feature."
        ))

        # Create variable importance object.
        vimp_object <- methods::new(
          "vimpTable",
          vimp_table = data.table::data.table(
            "score" = 0.0, 
            "name" = get_feature_columns(x = data)
          ),
          score_aggregation = "max",
          invert = TRUE
        )

        return(vimp_object)
      }

      # Find feature columns in the data.
      feature_columns <- get_feature_columns(x = data)

      # Parse formula.
      if (object@outcome_type == "survival") {
        # Creating a local instance of survival::Surv() prevents an
        # error.
        Surv <- survival::Surv
        formula <- stats::reformulate(
          termlabels = feature_columns,
          response = quote(Surv(outcome_time, outcome_event))
        )
        
      } else if (object@outcome_type %in% c("binomial", "multinomial", "continuous")) {
        formula <- stats::reformulate(
          termlabels = feature_columns,
          response = quote(outcome)
        )
        
      } else {
        ..error_outcome_type_not_implemented(object@outcome_type)
      }

      # Get arguments for the holdout.vimp learner.
      learner_arguments <- list(
        formula,
        "data" = data@data,
        "verbose" = FALSE
      )

      # Get additional variables for the optimised learner.
      if (!is(object, "familiarRFSRCDefault")) {
        # Determine the sample size
        sample_size <- ceiling(object@hyperparameters$sample_size * nrow(data@data))
        sample_type <- ifelse(sample_size == nrow(data@data), "swr", "swor")

        learner_arguments <- c(
          learner_arguments,
          list(
            "mtry" = max(c(1L, ceiling(object@hyperparameters$m_try * get_n_features(data)))),
            "samptype" = sample_type,
            "sampsize" = sample_size,
            "nodesize" = object@hyperparameters$node_size,
            "nodedepth" = object@hyperparameters$tree_depth,
            "nsplit" = object@hyperparameters$n_split,
            "splitrule" = as.character(object@hyperparameters$split_rule)
          )
        )
      }

      # Perform holdout variable importance.
      vimp_score <- do.call(
        randomForestSRC::holdout.vimp,
        args = learner_arguments
      )$importance

      # Check that the variable importance score is not empty.
      if (is_empty(vimp_score)) return(callNextMethod())

      # Select the "all" column, which is the first column
      if (is.matrix(vimp_score)) {
        vimp_score_names <- rownames(vimp_score)
        vimp_score <- vimp_score[, 1L]
        
      } else {
        vimp_score_names <- names(vimp_score)
      }

      # Create variable importance object.
      vimp_object <- methods::new(
        "vimpTable",
        vimp_table = data.table::data.table(
          "score" = vimp_score,
          "name" = vimp_score_names
        ),
        score_aggregation = "max",
        invert = TRUE
      )

      return(vimp_object)
      
    } else {
      ..error_reached_unreachable_code(paste0(
        "..vimp,familiarRFSRC: unknown vimp method was specified: ", vimp_method
      ))
    }

    return(NULL)
  }
)



# ..set_calibration_info -------------------------------------------------------
setMethod(
  "..set_calibration_info",
  signature(object = "familiarRFSRC"),
  function(object, data) {
    # Check if calibration info already.
    if (has_calibration_info(object)) return(object)

    if (object@outcome_type == "survival") {
      # Determine baseline survival.
      object@calibration_info <- get_baseline_survival(data = data)
      
    } else {
      return(callNextMethod())
    }

    return(object)
  }
)



# ..set_vimp_parameters --------------------------------------------------------
setMethod(
  "..set_vimp_parameters",
  signature(object = "familiarRFSRC"),
  function(
    object,
    method,
    ...
  ) {
    # Determine variable importance method
    if (startswith_any(
      method,
      prefix = c(
        "random_forest_permutation",
        "random_forest_rfsrc_permutation"
      )
    )) {
      vimp_method <- "permutation"
      
    } else if (startswith_any(
      method,
      prefix = c(
        "random_forest_minimum_depth",
        "random_forest_rfsrc_minimum_depth"
      )
    )) {
      vimp_method <- "minimum_depth"
      
    } else if (startswith_any(
      method,
      prefix = c(
        "random_forest_variable_hunting",
        "random_forest_rfsrc_variable_hunting"
      )
    )) {
      ..deprecation_rfsrc_variable_hunting(as_error = TRUE)
      
    } else if (startswith_any(
      method,
      prefix = c(
        "random_forest_holdout",
        "random_forest_rfsrc_holdout"
      )
    )) {
      vimp_method <- "holdout"
      
    } else {
      ..error_reached_unreachable_code(paste0(
        "..set_vimp_parameters,familiarRFSRC: unknown feature selection ",
        "method was specified."
      ))
    }

    # Check if a list of hyperparameters is already present.
    if (is.null(object@hyperparameters)) {
      hyperparameters <- list()
    } else {
      hyperparameters <- object@hyperparameters
    }

    # Update the fs_vimp_method hyperparameters.
    hyperparameters$fs_vimp_method <- vimp_method

    # Store in the object
    object@hyperparameters <- hyperparameters

    return(object)
  }
)



# .trim_model-------------------------------------------------------------------
setMethod(
  ".trim_model",
  signature(object = "familiarRFSRC"),
  function(object, ...) {
    # Update model by removing the call.
    object@model$call <- call("trimmed")

    # Add show.
    object <- .capture_show(object)

    # Remove the predictions.
    object@model$predicted <- NULL
    object@model$predicted.oob <- NULL

    # Remove yvar due to redundancy.
    object@model$yvar <- NULL

    # Set is_trimmed to TRUE.
    object@is_trimmed <- TRUE

    return(object)
  }
)



.get_available_rfsrc_learners <- function(show_general = TRUE) {
  return(c("random_forest", "random_forest_rfsrc"))
}



.get_available_rfsrc_default_learners <- function(show_general = TRUE) {
  return(paste0(.get_available_rfsrc_learners(show_general = show_general), "_default"))
}



.get_available_rfsrc_vimp_methods <- function(show_general = TRUE) {
  return(c(
    "random_forest_permutation", "random_forest_minimum_depth", 
    "random_forest_rfsrc_permutation", "random_forest_rfsrc_minimum_depth",
    "random_forest_holdout", "random_forest_rfsrc_holdout"
  ))
}



.get_available_rfsrc_default_vimp_methods <- function(show_general = TRUE) {
  return(paste0(.get_available_rfsrc_vimp_methods(show_general = show_general), "_default"))
}

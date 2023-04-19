#' @include FamiliarS4Generics.R
NULL



# familiarSimilarityMetric -----------------------------------------------------
setClass(
  "familiarSimilarityMetric",
  slots = list(
    "similarity_metric" = "character",
    "range" = "numeric",
    "distance" = "logical",
    "require_normalisation" = "logical",
    "highly_similar_threshold" = "numeric"),
  prototype = list(
    "similarity_metric" = character(),
    "range" = NA_real_,
    "distance" = logical(),
    "require_normalisation" = logical(),
    "highly_similar_threshold" = NA_real_))



# familiarSimilarityMetricPseudoR2 ---------------------------------------------
setClass(
  "familiarSimilarityMetricPseudoR2",
  contains = "familiarSimilarityMetric",
  prototype = methods::prototype(
    "range" = c(0.0, 1.0),
    "distance" = FALSE,
    "require_normalisation" = FALSE,
    "highly_similar_threshold" = 0.80))



# familiarSimilarityMetricCorrelation ------------------------------------------
setClass(
  "familiarSimilarityMetricCorrelation",
  contains = "familiarSimilarityMetric",
  prototype = methods::prototype(
    "range" = c(-1.0, 0.0, 1.0),
    "distance" = FALSE,
    "require_normalisation" = FALSE,
    "highly_similar_threshold" = 0.90))



# familiarSimilarityMetricMutualInformation ------------------------------------
setClass(
  "familiarSimilarityMetricMutualInformation",
  contains = "familiarSimilarityMetric",
  prototype = methods::prototype(
    "range" = c(0.0, 1.0),
    "distance" = FALSE,
    "require_normalisation" = FALSE,
    "highly_similar_threshold" = 0.45))



# familiarSimilarityMetricDistance ---------------------------------------------
setClass(
  "familiarSimilarityMetricDistance",
  contains = "familiarSimilarityMetric",
  prototype = methods::prototype(
    "range" = c(0.0, Inf),
    "distance" = TRUE,
    "require_normalisation" = TRUE,
    "highly_similar_threshold" = 0.01))



# is_available (general) -------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarSimilarityMetric"),
  function(object, ...) {
    return(TRUE)
  }
)



# is_available (distance) ------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarSimilarityMetricDistance"),
  function(object, any_categorical, ...) {
    return(!any_categorical)
  }
)



.create_similarity_metric_object <- function(similarity_metric) {
  # Strip some information from similarity_metric
  similarity_metric <- gsub(
    x = similarity_metric, 
    pattern = "_trim", 
    replacement = "",
    fixed = TRUE)
  similarity_metric <- gsub(
    x = similarity_metric,
    pattern = "_winsor",
    replacement = "", 
    fixed = TRUE)
  
  if (similarity_metric %in% c("cox_snell_r2", "nagelkerke_r2", "mcfadden_r2")) {
    object <- methods::new(
      "familiarSimilarityMetricPseudoR2",
      similarity_metric = similarity_metric)
    
  } else if (similarity_metric %in% c("pearson", "kendall", "spearman")) {
    object <- methods::new(
      "familiarSimilarityMetricCorrelation",
      similarity_metric = similarity_metric)
    
  } else if (similarity_metric %in% c("mutual_information")) {
    object <- methods::new(
      "familiarSimilarityMetricMutualInformation",
      similarity_metric = similarity_metric)

  } else if (similarity_metric %in% c(
    "gower", "euclidean", "manhattan", "chebyshev", "cosine", "canberra", "bray_curtis")) {
    object <- methods::new(
      "familiarSimilarityMetricDistance",
      similarity_metric = similarity_metric)
    
  } else {
    ..error_reached_unreachable_code(
      "compute_similarity_metric: encountered unknown similarity metric")
  }
  
  return(object)
}



# encode_categorical_variables methods -----------------------------------------

## encode_categorical_variables (general) --------------------------------------
setMethod(
  "encode_categorical_variables",
  signature(
    object = "familiarSimilarityMetric",
    data = "ANY"),
  function(object, data, ...) {
    
    # Extract data table with contrasts.
    data <- encode_categorical_variables(
      data = data,
      object = NULL,
      encoding_method = "dummy",
      drop_levels = FALSE)
    
    return(data$encoded_data)
  }
)



## encode_categorical_variables (distance) -------------------------------------
setMethod(
  "encode_categorical_variables",
  signature(
    object = "familiarSimilarityMetricDistance",
    data = "ANY"),
  function(object, data, ...) {
    
    # Leave data as is.
    return(data)
  }
)



# .compute_feature_similarity methods ------------------------------------------

compute_feature_similarity_metric <- function(
    data,
    similarity_metric,
    feature_info_list,
    cl = NULL,
    verbose = FALSE) {
  
  if (is_empty(data)) return(NULL)
  if (!is(data, "dataObject")) ..error_reached_unreachable_code("data should be dataObject.")
  
  # Strip some information from similarity_metric
  similarity_metric_type <- gsub(
    x = similarity_metric, 
    pattern = "_trim", 
    replacement = "",
    fixed = TRUE)
  similarity_metric_type <- gsub(
    x = similarity_metric_type,
    pattern = "_winsor",
    replacement = "", 
    fixed = TRUE)
  
  # Find similarity object.
  object <- .create_similarity_metric_object(similarity_metric = similarity_metric_type)
  
  # Set feature columns.
  feature_columns <- get_feature_columns(data)
  
  # Check that the number of features is at least two. This is more of technical
  # requirement than anything else.
  if (length(feature_columns) < 2) return(NULL)
  
  # Set the categorical mask.
  categorical_mask <- sapply(
    feature_info_list[feature_columns],
    function(x) (x@feature_type == "factor"))

  if (!is_available(object = object, any_categorical = any(categorical_mask))) {
    rlang::warn(
      message = paste0(
        "The ", similarity_metric, " metric can not be used to assess similarity between ",
        "categorical variables. We will use mutual_information instead."),
      class = "parameter_replacement_warning")
    
    # Replace metric by mutual_information.
    object <- .create_similarity_metric_object(similarity_metric = "mutual_information")
  }
  
  # Normalise data, if required.
  if (object@require_normalisation) {
    # Identify numerical features
    numerical_features <- feature_columns[!categorical_mask]
    
    # Create a local copy of data.
    data@data <- data.table::copy(data@data)
    
    # Find the normalisation method.
    norm_method <- "normalisation"
    if (grepl(pattern = "_trim", x = similarity_metric, fixed = TRUE)) {
      norm_method <- "normalisation_trim"
    } else if (grepl(pattern = "_winsor", x = similarity_metric, fixed = TRUE)) {
      norm_method <- "normalisation_winsor"
    }
    
    # Perform normalisation.
    for (ii in numerical_features) {
      data.table::set(data@data, j = ii, value = .normalise(
        x = data@data[[ii]],
        normalisation_method = norm_method,
        range = c(0, 1)))
    }
  }
  
  # Generate all combinations of features
  combinations <- utils::combn(sort(feature_columns), 2)
  
  # Determine similarity measures for each feature pair.
  similarity <- fam_sapply(
    cl = cl,
    assign = NULL,
    X = seq_len(ncol(combinations)),
    function(ii, combinations, data, object, categorical_mask) {
      
      # Identify features that are being compared.
      feature_1 <- combinations[1, ii]
      feature_2 <- combinations[2, ii]
      
      # Compute pairwise similarity
      similarity <- .compute_feature_similarity(
        object = object,
        x = data[[feature_1]],
        y = data[[feature_2]],
        x_categorical = categorical_mask[feature_1],
        y_categorical = categorical_mask[feature_2])
      
      return(similarity)
    },
    progress_bar = verbose,
    combinations = combinations,
    data = droplevels(data@data),
    object = object,
    categorical_mask = categorical_mask,
    chopchop = TRUE)
  
  # Transform similarity scores into a data.table.
  similarity_data  <- data.table::data.table(
    "feature_name_1" = combinations[1, ],
    "feature_name_2" = combinations[2, ],
    "value" = similarity)
  
  return(similarity_data)
}





## .compute_feature_similarity (generic) ---------------------------------------
setGeneric(".compute_feature_similarity", function(object, ...) standardGeneric(".compute_feature_similarity"))



## .compute_feature_similarity (general) ---------------------------------------
setMethod(
  ".compute_feature_similarity",
  signature(object = "familiarSimilarityMetric"),
  function(object, ...) {
    # Fall-back method.
    return(NA_real_)
  }
)


## .compute_feature_similarity (pseudo r2) -------------------------------------
setMethod(
  ".compute_feature_similarity",
  signature(object = "familiarSimilarityMetricPseudoR2"),
  function(
    object, 
    x, 
    y, 
    x_categorical, 
    y_categorical, 
    type = "approximate", 
    ...) {
    
    .check_parameter_value_is_valid(
      x = type,
      var_name = "type",
      values = c("exact", "approximate"))
    
    ..encode_variable_to_list <- function(x, is_categorical, insert_intercept = FALSE) {
      if (is_categorical) {
        # Categorical variables are encoded as numeric levels (ordinal), or
        # using one-hot-encoding (nominal).
        if (is.ordered(x)) {
          x <- list(as.numeric(x))
          
        } else {
          # Dummy encoding of categorical variable.
          level_names <- levels(x)
          level_count <- nlevels(x)
          
          x <- lapply(
            level_names[2:level_count],
            function(ii, x) (as.numeric(x == ii)),
            x = x)
        }
      } else {
        # Numeric variables are only stored as a list.
        x <- list(x)
      }
      
      # Set names
      names(x) <- paste0("name_", seq_along(x))
      
      if (insert_intercept) {
        x <- c(x, list("intercept__" = numeric(length(x)) + 1.0))
      }
      
      return(data.table::as.data.table(x))
    }
    
    # Remove missing elements.
    valid_elements <- is.finite(x) & is.finite(y)
    if (sum(valid_elements) <= 1) return(callNextMethod())
    
    # Keep only valid elements.
    x <- x[valid_elements]
    y <- y[valid_elements]
    
    # Check if there are more than one unique values in x and or y.
    if (length(unique(x)) == 1 && length(unique(y)) == 1) return(1.0)
    
    if (type == "approximate") {
      # Select the number of samples for the computing approximate distances.
      n_samples <- min(c(length(x), ceiling(1000^0.3 * length(x)^0.7)))
      
      # Set sample indices - avoid selecting samples randomly, as that is a
      # somewhat costly operation.
      sample_index <- as.integer(round(seq_len(n_samples) / n_samples * length(x)))
      
      # Select sample subset.
      x <- x[sample_index]
      if (x_categorical) x <- droplevels(x)
      y <- y[sample_index]
      if (y_categorical) y <- droplevels(y)
    }
    
    # Check if there are more than one unique values in x and or y.
    if (length(unique(x)) == 1 && length(unique(y)) == 1) return(1.0)
    
    # Find analysis type and whether x and y should be swapped in the models,
    # based on information content.
    analysis_info <- .get_analysis_type_pairwise_similarity(
      x = x,
      y = y,
      x_categorical = x_categorical,
      y_categorical = y_categorical)
    
    x <- analysis_info$x
    y <- analysis_info$y
    x_categorical <- analysis_info$x_categorical
    y_categorical <- analysis_info$y_categorical
    
    if (analysis_info$type %in% c("gaussian", "binomial")) {
      
      if (analysis_info$type == "gaussian") {
        fitting_family <- stats::gaussian()
      } else {
        fitting_family <- stats::binomial()
      }
      
      if (require_package("fastglm", message_type = "silent")) {
        predictors <- ..encode_variable_to_list(
          x = x,
          is_categorical = x_categorical,
          insert_intercept = TRUE)
        
        # Fit informative model.
        model <- fastglm::fastglm(
          x = as.matrix(predictors),
          y = y,
          family = fitting_family,
          method = 3L)
        
        predictor_names <- setdiff(names(model$coefficients), "intercept__")
        
        if (any(!is.finite(model$coefficients[predictor_names]))) return(0.0)
        if (all(approximately(model$coefficients[predictor_names], 0.0))) return(0.0)
        if (approximately(model$deviance, 0.0)) return(1.0)
        
        # Fit uninformative model.
        null_model <- fastglm::fastglm(
          x = as.matrix(numeric(length(y)), ncol = 1L),
          y = y,
          family = fitting_family,
          method = 3L)
        
      } else {
        # Fit informative model.
        model <- stats::glm(
          "y ~ x",
          data = data.table::data.table("x" = x, "y" = y),
          family = fitting_family)
        
        predictor_names <- setdiff(names(coef(model)), "(Intercept)")
        
        if (any(!is.finite(model$coefficients[predictor_names]))) return(0.0)
        if (all(near(model$coefficients[predictor_names], 0.0, df = 2 * length(x)))) return(0.0)
        if (approximately(model$deviance, 0.0)) return(1.0)
        
        # Fit uninformative model.
        null_model <- stats::glm(
          "y ~ 1",
          data = data.table::data.table("x" = x, "y" = y),
          family = fitting_family)
      }
      
      # Compute log-likelihoods
      model_loglik <- stats::logLik(model)[1]
      null_loglik <- stats::logLik(null_model)[1]
      
    } else if (analysis_info$type == "multinomial") {
      
      require_package(
        x = "nnet",
        purpose = paste0(
          "to compute log-likelihood pseudo R2 similarity using the ",
          object@similarity_metric, " metric"))
      
      # Set predictors and response.
      predictors <- ..encode_variable_to_list(
        x = x,
        is_categorical = x_categorical)
      response <- data.table::data.table("response" = y)
      
      # Set model formula.
      model_formula <- stats::reformulate(
        termlabels = names(predictors),
        response = quote(response))
      null_formula <- stats::as.formula("response ~ 1")
      
      quiet(model <- nnet::multinom(
        model_formula,
        data = cbind(predictors, response),
        maxit = ifelse(type == "approximate", 100L, 500L),
        MaxNWts = Inf))
      
      model_coefficients <- stats::coef(model)
      predictor_names <- setdiff(colnames(model_coefficients), "(Intercept)")
      
      if (any(!is.finite(model_coefficients[, predictor_names]))) return(0.0)
      if (all(near(as.vector(model_coefficients[, predictor_names]), 0.0, df = 2 * length(x)))) return(0.0)
      if (approximately(model$deviance, 0.0)) return(1.0)
      
      quiet(null_model <- nnet::multinom(
        null_formula,
        data = cbind(predictors, response),
        maxit = ifelse(type == "approximate", 100L, 500L)))
      
      # Compute log-likelihoods
      model_loglik <- stats::logLik(model)[1]
      null_loglik <- stats::logLik(null_model)[1]
      
    } else {
      ..error_reached_unreachable_code(paste0(
        ".compute_similarity,familiarSimilarityMetricPseudoR2: ",
        "encountered unknown analysis type"))
    }
    
    # Compute pseudo R-squared values from log-likelihoods
    if (object@similarity_metric == "mcfadden_r2") {
      # Compute McFadden's R-squared
      similarity <- 1.0 - model_loglik / null_loglik
      
    } else if (object@similarity_metric == "cox_snell_r2") {
      # Compute Cox and Snell's R-squared
      similarity <- 1.0 - (exp(null_loglik) / exp(model_loglik))^(2.0 / length(x))
      
    } else if (object@similarity_metric == "nagelkerke_r2") {
      # Compute Nagelkerke's R-squared
      similarity <- (1.0 - (exp(null_loglik) / exp(model_loglik))^(2.0 / length(x))) /
        (1.0 - exp(null_loglik)^(2.0 / length(x)))
      
    } else {
      ..error_reached_unreachable_code(paste0(
      ".compute_similarity,familiarSimilarityMetricPseudoR2: ",
      "encountered unknown similarity metric"))
    }
    
    # Check if similarity is NaN or infinite.
    if (!is.finite(similarity)) similarity <- 0.0
    
    # Limit to range [0,1]
    if (similarity < 0.0) {
      similarity <- 0.0
    } else if (similarity > 1.0) {
      # Greater than 1.0 should be set to 1.0.
      similarity <- 1.0
    } else if (approximately(similarity, 1.0)) {
      # Check if values are very close to 1.0.
      similarity <- 1.0
    }
    
    return(similarity)
  }
)



## .compute_feature_similarity (correlation) -----------------------------------
setMethod(
  ".compute_feature_similarity",
  signature(object = "familiarSimilarityMetricCorrelation"),
  function(
    object, 
    x, 
    y, 
    x_categorical, 
    y_categorical, 
    ...) {
   
    ..dummy_encode <- function(x) {
      # Dummy encoding of categorical variable.
      level_names <- levels(x)
      level_count <- nlevels(x)
      
      return(lapply(
        level_names[2:level_count],
        function(ii, x) (as.numeric(x == ii)),
        x = x))
    }
    
    ..encode_variable_to_list <- function(x, is_categorical) {
      if (is_categorical) {
        # Categorical variables are encoded as numeric levels (ordinal), or using
        # one-hot-encoding (nominal).
        if (is.ordered(x)) {
          x <- list(as.numeric(x))
        } else {
          x <- ..dummy_encode(x)
        }
      } else {
        # Numeric variables are only stored as a list.
        x <- list(x)
      }
      
      return(x)
    }
    
    # Cast x and y to lists. Categorical variables are encoded.
    x <- ..encode_variable_to_list(x = x, is_categorical = x_categorical)
    y <- ..encode_variable_to_list(x = y, is_categorical = y_categorical)
    
    # Get all list combinations; for numeric x and y this is always 1, but can
    # be more if x and/or y are factors
    combinations <- expand.grid(
      ii = seq_along(x), 
      jj = seq_along(y),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE)
    
    # Calculate correlation coefficients
    correlation_coef <- sapply(
      seq_len(nrow(combinations)),
      function(kk, combinations, x, y, method) {
        stats::cor(
          x = x[[combinations$ii[kk]]],
          y = y[[combinations$jj[kk]]],
          use = "na.or.complete",
          method = method)
      },
      combinations = combinations,
      x = x,
      y = y,
      method = object@similarity_metric)
    
    if (x_categorical || y_categorical) {
      correlation_coef <- abs(correlation_coef)
    }
    
    # Compute similarity as the mean correlation coefficient.
    similarity <- mean(correlation_coef, na.rm = TRUE)
    
    # Check for NaN or infinite values.
    if (!is.finite(similarity)) similarity <- 0.0
    
    # Return similarity
    return(similarity)
  }
)



## .compute_feature_similarity (mutual information) ----------------------------
setMethod(
  ".compute_feature_similarity",
  signature(object = "familiarSimilarityMetricMutualInformation"),
  function(
    object, 
    x, 
    y, 
    x_categorical, 
    y_categorical, 
    ...) {
    
    # Remove missing elements.
    valid_elements <- is.finite(x) & is.finite(y)
    if (sum(valid_elements) <= 1) return(callNextMethod())
    
    # Keep only valid elements.
    x <- x[valid_elements]
    y <- y[valid_elements]
    
    # Check if there are more than one unique values in x and or y.
    if (length(unique(x)) == 1 && length(unique(y)) == 1) return(1.0)
    
    # Ensure that numeric values are actually encoded as numeric, because
    # praznik handles integers as categorical variables. This is not the
    # convention used by familiar.
    if (!x_categorical && is.integer(x)) x <- as.numeric(x)
    if (!y_categorical && is.integer(y)) y <- as.numeric(y)
    
    require_package(
      x = "praznik",
      purpose = paste0(
        "to compute similarity using the ",
        object@similarity_metric, " metric"))
    
    # Compute normalised mutual information.
    return(praznik::miScores(x, y, threads = 1L) / praznik::jhScores(x, y, threads = 1L))
  }
)



## .compute_feature_similarity (distance) --------------------------------------
setMethod(
  ".compute_feature_similarity",
  signature(object = "familiarSimilarityMetricDistance"),
  function(
    object, 
    x, 
    y, 
    x_categorical, 
    y_categorical, 
    ...) {
    
    # For feature-wise comparison, the presence of categorical features is
    # problematic, because they don't have a "distance".
    if (x_categorical || y_categorical) return(callNextMethod())
    
    # Pass to sample similarity.
    return(.compute_sample_similarity(
      object = object,
      x = x,
      y = y,
      categorical = logical(length(x))),
      ...)
  }
)



compute_sample_similarity_metric <- function(
    data,
    similarity_metric,
    feature_info_list,
    cl = NULL,
    verbose = FALSE) {
  
  if (is_empty(data)) return(NULL)
  if (!is(data, "dataObject")) ..error_reached_unreachable_code("data should be dataObject.")
  
  # Strip some information from similarity_metric
  similarity_metric_type <- gsub(
    x = similarity_metric, 
    pattern = "_trim", 
    replacement = "",
    fixed = TRUE)
  similarity_metric_type <- gsub(
    x = similarity_metric_type,
    pattern = "_winsor",
    replacement = "", 
    fixed = TRUE)
  
  # Find similarity object.
  object <- .create_similarity_metric_object(similarity_metric = similarity_metric_type)
  
  # Set feature columns.
  feature_columns <- get_feature_columns(data)
  
  # Set the categorical mask.
  categorical_mask <- sapply(
    feature_info_list[feature_columns],
    function(x) (x@feature_type == "factor"))
  
  # Normalise data, if required.
  if (object@require_normalisation) {
    # Identify numerical features
    numerical_features <- feature_columns[!categorical_mask]
    
    # Create a local copy of data.
    data@data <- data.table::copy(data@data)
    
    # Find the normalisation method.
    norm_method <- "normalisation"
    if (grepl(pattern = "_trim", x = similarity_metric, fixed = TRUE)) {
      norm_method <- "normalisation_trim"
    } else if (grepl(pattern = "_winsor", x = similarity_metric, fixed = TRUE)) {
      norm_method <- "normalisation_winsor"
    }
    
    # Perform normalisation.
    for (ii in numerical_features) {
      data.table::set(data@data, j = ii, value = .normalise(
        x = data@data[[ii]],
        normalisation_method = norm_method,
        range = c(0, 1)))
    }
  }
  
  # Check that the number of rows is at least two. This is more
  # of technical requirement than anything else.
  if (nrow(data@data) < 2) return(NULL)
  
  # Generate all combinations of samples
  combinations <- utils::combn(seq_len(nrow(data@data)), 2)
  
  # # Encode data -- this has no effect for distance-based metrics.
  # data <- encode_categorical_variables(object = object, data = data)
  # 
  # # Find feature columns and categorical mask from encoded data.
  # feature_columns <- get_feature_columns(data)
  # categorical_mask <- sapply(
  #   feature_columns,
  #   function(feature, x) (is.factor(x[[feature]])),
  #   )
  
  # Determine similarity measures for each sample pair.
  similarity <- fam_sapply(
    cl = cl,
    assign = NULL,
    X = seq_len(ncol(combinations)),
    function(ii, combinations, data, object, categorical_mask) {
      
      # Identify features that are being compared.
      row_1 <- combinations[1, ii]
      row_2 <- combinations[2, ii]
      
      # Compute pairwise similarity
      similarity <- .compute_sample_similarity(
        object = object,
        x = as.numeric(data[row_1, ]),
        y = as.numeric(data[row_2, ]),
        categorical = categorical_mask)
      
      return(similarity)
    },
    progress_bar = verbose,
    combinations = combinations,
    data = data@data[, mget(feature_columns)],
    object = object,
    categorical_mask = categorical_mask,
    chopchop = TRUE)
  
  # Create unique row names.
  row_names <- get_unique_row_names(x = data)
  
  # Transform similarity scores into a data.table.
  similarity_data  <- data.table::data.table(
    "sample_1" = row_names[combinations[1, ]],
    "sample_2" = row_names[combinations[2, ]],
    "value" = similarity)
  
  return(similarity_data)
}



# .compute_sample_similarity (generic) -----------------------------------------
setGeneric(".compute_sample_similarity", function(object, ...) standardGeneric(".compute_sample_similarity"))



# .compute_sample_similarity (general) -----------------------------------------
setMethod(
  ".compute_sample_similarity",
  signature(object = "familiarSimilarityMetric"),
  function(object, ...) {
    # Fall-back method.
    return(NA_real_)
  }
)


# .compute_sample_similarity (pseudo r2) --------------------------------------
setMethod(
  ".compute_sample_similarity",
  signature(object = "familiarSimilarityMetricPseudoR2"),
  function(
    object, 
    x, 
    y, 
    categorical, 
    ...) {
    
    # The presence of categorical variables is problematic for computing
    # pseudo R2, because we can only correlation
    # between vectors that have a consistent data type, in this case numeric.
    if (any(categorical)) return(callNextMethod())
    
    # Pass to feature similarity routine.
    return(.compute_feature_similarity(
      object = object,
      x = x,
      y = y,
      categorical_x = FALSE,
      categorical_y = FALSE,
      ...))
  }
)



# .compute_sample_similarity (correlation) -------------------------------------
setMethod(
  ".compute_sample_similarity",
  signature(object = "familiarSimilarityMetricCorrelation"),
  function(
    object, 
    x, 
    y, 
    categorical, 
    ...) {
    
    # The presence of categorical variables is problematic for computing
    # sample-wise similarity, because we can only correlation
    # between vectors that have a consistent data type, in this case numeric.
    if (any(categorical)) return(callNextMethod())
    
    # Pass to feature similarity routine.
    return(.compute_feature_similarity(
      object = object,
      x = x,
      y = y,
      categorical_x = FALSE,
      categorical_y = FALSE,
      ...))
  }
)



# .compute_sample_similarity (mutual information) ------------------------------
setMethod(
  ".compute_sample_similarity",
  signature(object = "familiarSimilarityMetricMutualInformation"),
  function(
    object, 
    x, 
    y, 
    categorical, 
    ...) {
    
    # The presence of categorical variables is problematic for computing
    # sample-wise similarity, because we can only compute mutual information
    # between vectors that have a consistent data type, in this case numeric.
    if (any(categorical)) return(callNextMethod())
    
    # Pass to feature similarity routine.
    return(.compute_feature_similarity(
      object = object,
      x = x,
      y = y,
      categorical_x = FALSE,
      categorical_y = FALSE,
      ...))
  }
)



# .compute_sample_similarity (distance) ----------------------------------------
setMethod(
  ".compute_sample_similarity",
  signature(object = "familiarSimilarityMetricDistance"),
  function(
    object, 
    x, 
    y, 
    categorical,
    ...) {
    
    # Ensure that data presented as numeric values.
    if (!is.numeric(x)) x <- as.numeric(x)
    if (!is.numeric(y)) y <- as.numeric(y)
    
    # Remove missing elements.
    valid_elements <- is.finite(x) & is.finite(y)
    if (sum(valid_elements) < 1) return(callNextMethod())
    
    # Keep only valid elements.
    x <- x[valid_elements]
    y <- y[valid_elements]
    
    # Handle categorical variables so that distance = 0 if they are the same,
    # and distance = 1 if they are not.
    if (any(categorical)) {
      # Determine where values match.
      matching_values <- x[categorical] == y[categorical]
      
      # Assign the same value in x and y in case of matching categorical values.
      x[categorical][matching_values] <- 1.0
      y[categorical][matching_values] <- 1.0
      
      # Assign different values in x and y in case of mismatching categorical
      # values.
      x[categorical][!matching_values] <- 0.0
      y[categorical][!matching_values] <- 1.0
    }
    
    if (object@similarity_metric == "gower") {
      distance <- sum(abs(x - y)) / length(x)
    } else if (object@similarity_metric == "euclidean") {
      distance <- sqrt(sum((x - y)^2))
    } else if (object@similarity_metric == "manhattan") {
      distance <- sum(abs(x - y))
    } else if (object@similarity_metric == "chebyshev") {
      distance <- max(abs(x - y))
    } else if (object@similarity_metric == "cosine") {
      distance <- 1 - sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
    } else if (object@similarity_metric == "canberra") {
      distance <- sum(abs(x - y) / (abs(x) + abs(y)))
    } else if (object@similarity_metric == "bray_curtis") {
      distance <- sum(abs(x - y)) / sum(abs(x + y))
    }
    
    return(distance)
  }
)



.get_analysis_type_pairwise_similarity <- function(x, y, x_categorical, y_categorical) {
  # Determine analysis types: note that we expect the outcome to be y: hence it
  # is recommended that predictor x contains the widest range of variables, i.e.
  # continuous or multinomial variables.
  #
  # This function checks the analysis type and whether x and y should be swapped
  # in the analysis.

  # Determine number of levels of categorical features.
  n_x <- ifelse(x_categorical, nlevels(x), data.table::uniqueN(x))
  n_y <- ifelse(y_categorical, nlevels(y), data.table::uniqueN(y))

  if (x_categorical && y_categorical) {
    # Case 1: Both x and y are categorical variables.

    # Determine analysis type.
    analysis_type <- ifelse(min(c(n_x, n_y)) <= 2, "binomial", "multinomial")

    # Determine if swapping is required.
    requires_swap <- n_x < n_y
    
  } else if (x_categorical) {
    # Case 2: x is a categorical variable, and y numerical.

    # Determine analysis type.
    analysis_type <- ifelse(n_x <= 2, "binomial", "multinomial")

    # Swapping is required.
    requires_swap <- TRUE
    
  } else if (y_categorical) {
    # Case 3: x is a numerical variable, and y categorical.

    # Determine analysis type.
    analysis_type <- ifelse(n_y <= 2, "binomial", "multinomial")

    # Swapping is not required.
    requires_swap <- FALSE
    
  } else {
    # Case 4: both x and y are numerical variables.
    analysis_type <- "gaussian"
    requires_swap <- n_x < n_y
  }

  if (requires_swap) {
    x_out <- y
    y_out <- x
    x_categorical_out <- y_categorical
    y_categorical_out <- x_categorical
    
  } else {
    x_out <- x
    y_out <- y
    x_categorical_out <- x_categorical
    y_categorical_out <- y_categorical
  }
  
  return(list(
    "type" = analysis_type,
    "swap" = requires_swap,
    "x" = x_out,
    "y" = y_out,
    "x_categorical" = x_categorical_out,
    "y_categorical" = y_categorical_out))
}


# .similarity_to_distance ------------------------------------------------------

convert_similarity_to_distance <- function(x, similarity_metric) {
  # Convert to distance.
  
  # Find similarity object.
  object <- .create_similarity_metric_object(similarity_metric = similarity_metric)
  
  return(.similarity_to_distance(
    object = object,
    x = x))
}



## .similarity_to_distance (generic) -------------------------------------------
setGeneric(".similarity_to_distance", function(object, ...) standardGeneric(".similarity_to_distance"))



## .similarity_to_distance (pseudo r2) -----------------------------------------
setMethod(
  ".similarity_to_distance",
  signature(object = "familiarSimilarityMetricPseudoR2"),
  function(object, x, ...) {
    return(1 - sqrt(x))
  }
)



## .similarity_to_distance (correlation) ---------------------------------------
setMethod(
  ".similarity_to_distance",
  signature(object = "familiarSimilarityMetricCorrelation"),
  function(object, x, ...) {
    return(1 - abs(x))
  }
)



## .similarity_to_distance (mutual information) --------------------------------
setMethod(
  ".similarity_to_distance",
  signature(object = "familiarSimilarityMetricMutualInformation"),
  function(object, x, ...) {
    return(1 - x)
  }
)



## .similarity_to_distance (distance) ------------------------------------------
setMethod(
  ".similarity_to_distance",
  signature(object = "familiarSimilarityMetricDistance"),
  function(object, x, ...) {
    # Distance-based metrics don't require conversion to distance.
    return(x)
  }
)


# .distance_to_similarity ------------------------------------------------------

convert_distance_to_similarity <- function(x, similarity_metric) {
  # Convert distance to similarity.

  # Find similarity object.
  object <- .create_similarity_metric_object(similarity_metric = similarity_metric)
  
  return(.distance_to_similarity(
    object = object,
    x = x))
}



## .distance_to_similarity (generic) -------------------------------------------
setGeneric(".distance_to_similarity", function(object, ...) standardGeneric(".distance_to_similarity"))



## .distance_to_similarity (pseudo r2) -----------------------------------------
setMethod(
  ".distance_to_similarity",
  signature(object = "familiarSimilarityMetricPseudoR2"),
  function(object, x, ...) {
    return((1 - x)^2)
  }
)



## .distance_to_similarity (correlation) ---------------------------------------
setMethod(
  ".distance_to_similarity",
  signature(object = "familiarSimilarityMetricCorrelation"),
  function(object, x, ...) {
    # Note that the sign is lost at conversion back to similarity.
    return(1 - abs(x))
  }
)



## .distance_to_similarity (mutual information) --------------------------------
setMethod(
  ".distance_to_similarity",
  signature(object = "familiarSimilarityMetricMutualInformation"),
  function(object, x, ...) {
    return(1 - x)
  }
)



## .distance_to_similarity (distance) ------------------------------------------
setMethod(
  ".distance_to_similarity",
  signature(object = "familiarSimilarityMetricDistance"),
  function(object, x, ...) {
    # Return normalised similarity.
    return(1.0 - x / max(x, na.rm = TRUE))
  }
)



get_high_similarity_threshold <- function(similarity_metric) {

  # Find similarity object.
  object <- .create_similarity_metric_object(similarity_metric = similarity_metric)
  
  return(object@highly_similar_threshold)
}



get_similarity_range <- function(similarity_metric, as_distance = FALSE) {
  
  # Find similarity object.
  object <- .create_similarity_metric_object(similarity_metric = similarity_metric)
  
  value_range <- object@range
  
  if (as_distance) {
    value_range <- .distance_to_similarity(
      object = object,
      x = value_range)
    
    value_range <- sort(unique(value_range))
  }
  
  return(object@range)
}



is_default_distance <- function(similarity_metric) {
  # Find similarity object.
  object <- .create_similarity_metric_object(similarity_metric = similarity_metric)
  
  return(object@distance)
}



.get_available_similarity_metrics <- function(data_type = "feature") {
  if (data_type %in% c("feature", "cluster")) {
    # Pair-wise comparison between features.
    return(c(
      "mcfadden_r2", "cox_snell_r2", "nagelkerke_r2", "spearman", 
      "kendall", "pearson", "mutual_information"))
    
  } else {
    # Pair-wise comparison between samples.
    return(c(
      "gower", "gower_trim", "gower_winsor",
      "euclidean", "euclidean_trim", "euclidean_winsor"
    ))
  }
}

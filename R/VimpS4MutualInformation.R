#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass(
  "familiarMutualInformationVimp",
  contains = "familiarVimpMethod")

setClass(
  "familiarUnivariateMutualInfoVimp",
  contains = "familiarMutualInformationVimp")

setClass(
  "familiarMultivariateMutualInfoVimp",
  contains = "familiarMutualInformationVimp",
  prototype = methods::prototype(multivariate = TRUE))

# initialize -------------------------------------------------------------------
setMethod(
  "initialize",
  signature(.Object = "familiarMultivariateMutualInfoVimp"),
  function(.Object, ...) {
    # Update with parent class first.
    .Object <- callNextMethod()

    # Set the required package
    .Object@multivariate <- TRUE

    return(.Object)
  }
)



.get_available_univariate_mutual_information_vimp_method <- function(show_general = TRUE) {
  return("mim")
}

.get_available_multivariate_mutual_information_vimp_method <- function(show_general = TRUE) {
  return(c("mifs", "mrmr"))
}



# is_available -----------------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarMutualInformationVimp"),
  function(object, ...) {
    return(TRUE)
  }
)



# get_default_hyperparameters --------------------------------------------------
setMethod(
  "get_default_hyperparameters",
  signature(object = "familiarMutualInformationVimp"),
  function(object, data = NULL, ...) {
    return(list())
  }
)



# ..vimp (univariate) ----------------------------------------------------------
setMethod(
  "..vimp", 
  signature(object = "familiarUnivariateMutualInfoVimp"),
  function(object, data, ...) {

    if (is_empty(data)) return(callNextMethod())

    # Identify feature columns.
    feature_columns <- get_feature_columns(data)

    # Calculate mutual information
    mutual_information <- .compute_mutual_information(
      data = data, 
      features = feature_columns)

    # Create variable importance object.
    vimp_object <- methods::new(
      "vimpTable",
      vimp_table = data.table::data.table(
        "score" = mutual_information,
        "name" = feature_columns),
      score_aggregation = "max",
      invert = TRUE)

    return(vimp_object)
  }
)


# ..vimp (multivariate) --------------------------------------------------------
setMethod(
  "..vimp", signature(object = "familiarMultivariateMutualInfoVimp"),
  function(object, data, ...) {
    # - mifs: Mutual information feature selection using greedy search (Battiti
    # 1994)
    # - mrmr: Minimum Redundancy Maximum Relevance feature selection using
    # greedy search (Peng 2005)

    # Suppress NOTES due to non-standard evaluation in data.table
    objective_score <- available <- name <- mi_redundancy <- mi <- selected <- NULL

    if (is_empty(data)) return(callNextMethod())

    # Find feature columns in data table.
    feature_columns <- get_feature_columns(data)

    # Find signature features.
    signature_feature <- names(object@feature_info)[
      sapply(object@feature_info, is_in_signature)]

    # Calculate mutual information.
    mutual_information <- .compute_mutual_information(data = data)

    # Setup mutual information table.
    mi_data <- data.table::data.table(
      "name" = feature_columns,
      "mi" = mutual_information,
      "mi_redundancy" = 0,
      "objective_score" = mutual_information,
      "selected" = FALSE,
      "available" = TRUE,
      "selection_step" = 0)

    if (length(signature_feature) > 0) {
      # Iteration counter.
      ii <- 1

      for (current_feature in signature_feature) {
        # Mark the current signature feature. Note that we iterate over the
        # features, and update mutual information to ensure that we get to the
        # correct starting point for the non-signature variables.
        best_feature <- current_feature
        mi_data[name %in% best_feature, ":="(
          "selected" = TRUE,
          "available" = FALSE,
          "selection_step" = ii)]

        # Update Iteration counter.
        ii <- ii + 1

        # Determine the features still available.
        available_features <- mi_data[available == TRUE, ]$name
        if (length(available_features) == 0) break()

        # Also skip if this is the last signature feature -- this is to prevent
        # overlap with the main while-cycle below.
        if (current_feature == tail(signature_feature, n = 1L)) break()

        # Calculate mutual information of available features and the
        # current signature feature.
        current_redundancy_mi <- .compute_mutual_information(
          data, 
          features = available_features, 
          with_feature = best_feature)

        # Update redundancy
        mi_data[
          available == TRUE,
          "mi_redundancy" := mi_redundancy + current_redundancy_mi]
        
        if (object@vimp_method == "mifs") {
          # Update the objective score by subtracting the redundancy score. MIFS
          # uses beta=1 (Battiti 1994)
          mi_data[
            available == TRUE,
            "objective_score" := mi - mi_redundancy]
          
          # Only consider features with a valid, non-negative optimisation
          # score, as redundancy is strictly increasing. Note that signature
          # features are never removed until they are selected.
          mi_data[
            objective_score < 0.0 & selected == FALSE & !name %in% signature_feature,
            "available" := FALSE]
          
        } else if (object@vimp_method == "mrmr") {
          # Calculate optimisation score. MRMR uses beta=1/(ii-1), with ii-1 the
          # size of the selected feature set.
          mi_data[
            available == TRUE,
            "objective_score" := mi - mi_redundancy / (ii - 1)]

          # Only consider features with a valid, non-negative optimisation
          # score, as redundancy is strictly increasing. Note that signature
          # features are never removed until they are selected.
          mi_data[
            objective_score < 0.0 & selected == FALSE & !name %in% signature_feature,
            "available" := FALSE]
          
        } else {
          ..error_reached_unreachable_code(paste0(
            "vimp,familiarMultivariateMutualInfoVimp: encountered unknown vimp_method: ",
            object@vimp_method))
        }
      }
    } else {
      # Select initial feature and update mi_data.
      max_objective_score <- max(mi_data$objective_score)
      best_feature <- mi_data[objective_score == max_objective_score, ]$name[1]
      mi_data[name %in% best_feature, ":="(
        "selected" = TRUE,
        "available" = FALSE,
        "selection_step" = 1)]

      # Determine the features that were not selected.
      available_features <- mi_data[available == TRUE, ]$name

      # Iteration counter.
      ii <- 2
    }


    while (length(available_features) > 0) {
      # Calculate mutual information of available features and the selected feature
      current_redundancy_mi <- .compute_mutual_information(
        data, 
        features = available_features, 
        with_feature = best_feature)

      # Update redundancy
      mi_data[
        available == TRUE, 
        "mi_redundancy" := mi_redundancy + current_redundancy_mi]

      if (object@vimp_method == "mifs") {
        # Update the objective score by subtracting the redundancy score. MIFS
        # uses beta=1 (Battiti 1994)
        mi_data[
          available == TRUE,
          "objective_score" := mi - mi_redundancy]

        # Only consider features with a valid, non-negative optimisation score,
        # as redundancy is strictly increasing.
        mi_data[
          objective_score < 0.0 & selected == FALSE,
          "available" := FALSE]
        
      } else if (object@vimp_method == "mrmr") {
        # Calculate optimisation score. MRMR uses beta=1/(ii-1), with ii-1 the
        # size of the selected feature set
        mi_data[
          available == TRUE,
          "objective_score" := mi - mi_redundancy / (ii - 1)]

        # Only consider features with a valid, non-negative optimisation score,
        # as redundancy is strictly increasing.
        mi_data[
          objective_score < 0.0 & selected == FALSE,
          "available" := FALSE]
        
      } else {
        ..error_reached_unreachable_code(paste0(
          "vimp,familiarMultivariateMutualInfoVimp: encountered unknown vimp_method: ",
          object@vimp_method))
      }

      # If there are no more available features (i.e. due to negative objective
      # score), break from the loop.
      if (nrow(mi_data[available == TRUE, ]) == 0) break

      # Select best feature and update mi_data.
      max_objective_score <- max(mi_data[available == TRUE]$objective_score)
      best_feature <- mi_data[available == TRUE & objective_score == max_objective_score, ]$name[1]
      mi_data[name %in% best_feature, ":="(
        "selected" = TRUE, 
        "available" = FALSE,
        "selection_step" = ii)]

      # Determine the features still available.
      available_features <- mi_data[available == TRUE, ]$name

      # Increment iteration counter.
      ii <- ii + 1
    }

    # Create variable importance table from the score table.
    vimp_table <- mi_data[selected == TRUE, c("name", "objective_score", "selection_step")]

    # Create variable importance object.
    vimp_object <- methods::new(
      "vimpTable",
      vimp_table = data.table::data.table(
        "score" = vimp_table$selection_step,
        "name" = vimp_table$name),
      score_aggregation = "max",
      invert = FALSE)

    return(vimp_object)
  }
)



.compute_mutual_information <- function(
    data, 
    features = NULL, 
    with_feature = NULL) {
  # If the comparison feature is NULL, compare with outcome.
  if (is.null(with_feature)) {
    # Set with_feature.
    with_feature <- get_outcome_columns(data)

    # Set the type of y.
    y_type <- switch(
      data@outcome_type,
      survival = "survival",
      binomial = "discrete",
      multinomial = "discrete",
      continuous = "continuous",
      count = "continuous")

    if (data@outcome_type %in% c("survival")) {
      y <- survival::Surv(
        time = data@data[["outcome_time"]],
        event = data@data[["outcome_event"]])
    } else {
      y <- data@data[["outcome"]]
    }
    
  } else {
    # Isolate y-feature
    y <- data@data[[with_feature]]

    # Set the type of y.
    y_type <- ..determine_mi_variable_type(y)
  }

  # Get features.
  if (is.null(features)) {
    features <- get_feature_columns(data)
  }

  # Compute mutual information.
  mutual_information <- sapply(
    features,
    function(feature, data, y, y_type) {
      return(..compute_mutual_information(
        x = data@data[[feature]],
        y = y,
        y_type = y_type))
    },
    data = data,
    y = y, 
    y_type = y_type)
  
  return(mutual_information)
}



..compute_mutual_information <- function(x, y, x_type = NULL, y_type = NULL) {
  # Determine variable type.
  if (is.null(x_type)) x_type <- ..determine_mi_variable_type(x)
  if (is.null(y_type)) y_type <- ..determine_mi_variable_type(y)

  # Convert discrete variables to factors.
  if (x_type == "discrete" && !is.factor(x)) x <- as.factor(x)
  if (y_type == "discrete" && !is.factor(y)) y <- as.factor(y)

  # Drop unused levels from factors.
  if (x_type == "discrete") x <- droplevels(x)
  if (y_type == "discrete") y <- droplevels(y)

  if (x_type == "continuous" && is.integer(x)) x <- as.numeric(x)
  if (y_type == "continuous" && is.integer(y)) y <- as.numeric(y)

  if (require_package("praznik", message_type = "silent")) {
    # Praznik-based computation.

    if (y_type == "survival") {
      # Mutual information for survival.
      mutual_information <- ..compute_mutual_information_any_survival(
        x = x,
        y = y)
      
    } else {
      # Use praznik package.
      mutual_information <- praznik::miScores(
        X = x,
        Y = y,
        threads = 1L)
    }
    
  } else {
    # Computation using internal methods.

    if (y_type == "survival") {
      # Mutual information for survival.
      mutual_information <- ..compute_mutual_information_any_survival(
        x = x, 
        y = y)
      
    } else if (y_type == "discrete") {
      if (x_type == "continuous") {
        # Mutual information for continuous and discrete variables.
        mutual_information <- ..compute_mutual_information_continuous_discrete(
          x = x, 
          y = y)
        
      } else if (x_type == "discrete") {
        # Mutual information for two discrete variables.
        mutual_information <- ..compute_mutual_information_discrete_discrete(
          x = x, 
          y = y)
      } else {
        ..error_reached_unreachable_code(
          "..compute_mutual_information: unknown variable type encountered for variable x.")
      }
      
    } else if (y_type == "continuous") {
      if (x_type == "continuous") {
        # Mutual information for two continuous variables.
        mutual_information <- ..compute_mutual_information_continuous_continuous(
          x = x, 
          y = y)
        
      } else if (x_type == "discrete") {
        # Mutual information for continuous and discrete variables. Note that
        # the order is reversed so that y is the discrete variable.
        mutual_information <- ..compute_mutual_information_continuous_discrete(
          x = y, 
          y = x)
        
      } else {
        ..error_reached_unreachable_code(
          "..compute_mutual_information: unknown variable type encountered for variable x.")
      }
    } else {
      ..error_reached_unreachable_code(
        "..compute_mutual_information: unknown variable type encountered for variable y.")
    }
  }

  # Check for invalid values.
  if (!is.finite(mutual_information)) mutual_information <- 0.0

  # Correct negative values.
  if (mutual_information < 0.0) mutual_information <- 0.0

  return(mutual_information)
}



..determine_mi_variable_type <- function(x) {
  # Determine the variable type.
  if (is.factor(x) || is.logical(x) || is.character(x)) {
    return("discrete")
  } else {
    return("continuous")
  }
}



..compute_mutual_information_any_survival <- function(x, y) {
  if (is.factor(x)) {
    # Expand x by using dummy coding.
    x <- stats::model.matrix(~ . - 1, data.frame(x))

    # Calculate concordance indices
    ci <- apply(
      x, 2, 
      function(x_data, y_data) {
        return(..compute_concordance_index(
          x = x_data,
          time = y_data[, 1],
          event = y_data[, 2]))
      },
      y_data = y)

    # Calculate mutual information from the concordance indices
    mi <- -0.5 * log(1.0 - (2.0 * (ci - 0.5))^2 + 1E-10)

    # Find the maximum mutual information.
    mi <- max(mi)
    
  } else {
    # Compute the concordance index.
    ci <- ..compute_concordance_index(
      x = x, 
      time = y[, 1],
      event = y[, 2])

    # Compute mutual information from the concordance index.
    mi <- -0.5 * log(1.0 - (2.0 * (ci - 0.5))^2 + 1E-10)
  }

  return(mi)
}



..compute_mutual_information_continuous_discrete <- function(x, y) {
  # MI is calculated based on linear approximation by De Jay et al. 2013;
  # doi:10.1093/bioinformatics/btt383 and Gel'fand, I.M.; Yaglom, A.M. (1957).
  # "Calculation of amount of information about a random function contained in
  # another such function". American Mathematical Society Translations: Series
  # 2. 12: 199-246

  # Expand y by using dummy coding. x is the continuous variable.
  y <- stats::model.matrix(~ . - 1, data.frame(y))

  # Calculate mutual information using correlation.
  mi <- apply(
    y, 2,
    function(y, x) {
      return(-0.5 * log(1.0 - stats::cor(
        x = x, 
        y = y, 
        method = "spearman")^2 + 1E-10))
    },
    x = x)
  
  # Select the maximum mutual information.
  mi <- max(mi)

  return(mi)
}



..compute_mutual_information_discrete_discrete <- function(x, y) {
  # Direct, exact, calculation of mutual information for binomial and
  # multinomial outcomes Mutual information is defined as: I(X,Y) = H(X) -
  # H(X|Y) = H(Y) - H(Y|X) = H(X) + H(Y) - H(X,Y), and we calculate entropies

  # Calculate probability distributions
  p <- ..compute_mutual_information_conditional_probability(x = x, y = y)

  # Calculate entropies for marginal and joint distributions
  h_j <- -sum(p$prob_j$prob_j * log(p$prob_j$prob_j + 1E-10))
  h_k <- -sum(p$prob_k$prob_k * log(p$prob_k$prob_k + 1E-10))
  h_jk <- -sum(p$prob_kj$prob_kj * log(p$prob_kj$prob_kj + 1E-10))

  # Compute mutual information
  mi <- h_j + h_k - h_jk

  return(mi)
}



..compute_mutual_information_continuous_continuous <- function(x, y) {
  # MI is calculated based on linear approximation by De Jay et al. 2013;
  # doi:10.1093/bioinformatics/btt383 and Gel'fand, I.M.; Yaglom, A.M. (1957).
  # "Calculation of amount of information about a random function contained in
  # another such function". American Mathematical Society Translations: Series
  # 2. 12: 199-246

  return(-0.5 * log(1 - stats::cor(x = x, y = y, method = "spearman")^2 + 1E-10))
}



..compute_mutual_information_conditional_probability <- function(x, y) {
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- NULL

  # Total number of training instances
  n <- length(x)

  # Create data table from data
  data <- data.table::data.table(
    "value" = x, 
    "class" = y)

  # p(k,j): joint probability for the combination of class k and value j
  p_kj <- data[, list(prob_kj = .N / n), by = list(value, class)]

  # p(k): marginal probability for class k
  p_k <- data[, list(prob_k = .N / n), by = class]

  # p(j): marginal probability for value j
  p_j <- data[, list(prob_j = .N / n), by = value]

  return(list(
    "prob_kj" = p_kj,
    "prob_k" = p_k, 
    "prob_j" = p_j))
}

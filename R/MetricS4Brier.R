#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



# familiarMetricBrier ----------------------------------------------------------
setClass(
  "familiarMetricBrier",
  contains = "familiarMetric",
  prototype = methods::prototype(
    metric = "brier",
    outcome_type = NA_character_,
    name = "Brier Score",
    value_range = c(0.0, 1.0),
    higher_better = FALSE
  )
)



# is_available -----------------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarMetricBrier"),
  function(object, ...) {
    return(object@outcome_type %in% c("binomial", "multinomial"))
  }
)



# compute_metric_score ---------------------------------------------------------
setMethod(
  "compute_metric_score",
  signature(metric = "familiarMetricBrier"),
  function(metric, data, ...) {
    
    if (is_empty(data)) return(callNextMethod())
    
    if (!is(data, "predictionTableClassification")) {
      ..error_data_not_prediction_table(data, "predictionTableClassification")
    }
    
    # Get the classes and number of classes in data.
    outcome_classes <- get_outcome_class_levels(data)
    n_classes <- length(outcome_classes)

    # Skip calculation if there are no other classes.
    if (n_classes <= 1) return(callNextMethod())

    # Remove any entries that lack valid predictions.
    data <- remove_invalid_predictions(data)

    # Remove any entries that lack observed values.
    data <- filter_missing_outcome(data)
    if (is_empty(data)) return(callNextMethod())
    
    # Identify the outcome column.
    outcome_column <- get_outcome_columns(x = data@outcome_type)
    data <- .as_data_table(data)

    # Create empty brier score vector.
    brier_score <- vector(mode = "numeric", length = n_classes)

    # Iterate over classes.
    for (ii in seq_along(outcome_classes)) {
      # Find the current positive class
      positive_class <- outcome_classes[ii]

      # Prepare data to compute a Brier score.
      brier_data <- data.table::copy(data[, mget(c(outcome_column, positive_class))])

      # Mark positive class as 1, and the rest as 0.
      brier_data[, "positive_outcome" := 0]
      brier_data[get(outcome_column) == positive_class, "positive_outcome" := 1]

      # Calculate uncorrected brier score for the current positive class.
      brier_score[ii] <- sum((brier_data[[positive_class]] - brier_data$positive_outcome)^2)
    }

    # Calculate overall brier score
    brier_score <- sum(brier_score) / nrow(data)

    # Correct score for binomial and multinomial outcomes so that it falls in
    # the (0, 1) range
    brier_score <- brier_score / 2.0

    return(brier_score)
  }
)



.get_available_brier_metrics <- function() {
  return(c("brier"))
}

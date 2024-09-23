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
    higher_better = FALSE))



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
    # Get the classes and number of classes in data.
    outcome_classes <- get_outcome_class_levels(
      data,
      outcome_type = metric@outcome_type)
    n_classes <- length(outcome_classes)

    # Skip calculation if there are no other classes.
    if (n_classes <= 1) return(callNextMethod())

    # Remove any entries that lack valid predictions.
    data <- remove_nonvalid_predictions(
      prediction_table = data,
      outcome_type = metric@outcome_type)

    # Remove any entries that lack observed values.
    data <- remove_missing_outcomes(
      data = data,
      outcome_type = metric@outcome_type)

    # Check that there is any data left.
    if (is_empty(data)) return(callNextMethod())

    # Identify the outcome column.
    outcome_column <- get_outcome_columns(x = metric@outcome_type)

    # Create empty brier score vector.
    brier_score <- vector(mode = "numeric", length = n_classes)

    # Iterate over classes.
    for (ii in seq_along(outcome_classes)) {
      # Find the current positive class
      positive_class <- outcome_classes[ii]

      # Get the probability column name for the positive class.
      class_probability_column <- get_class_probability_name(x = positive_class)

      # Prepare data to compute a Brier score.
      brier_data <- data.table::copy(data[, mget(c(outcome_column, class_probability_column))])

      # Mark positive class as 1, and the rest as 0.
      brier_data[, "positive_outcome" := 0]
      brier_data[get(outcome_column) == positive_class, "positive_outcome" := 1]

      # Calculate uncorrected brier score for the current positive class.
      brier_score[ii] <- sum((brier_data[[class_probability_column]] - brier_data$positive_outcome)^2)
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

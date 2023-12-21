#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

# familiarMetricAUCROC
setClass(
  "familiarMetricAUCROC",
  contains = "familiarMetric",
  prototype = methods::prototype(
    metric = "auc_roc",
    outcome_type = NA_character_,
    name = "Area under Receiver Operating Characteristic Curve",
    baseline_value = 0.5,
    value_range = c(0.0, 1.0),
    higher_better = TRUE
  )
)



# is_available -----------------------------------------------------------------
setMethod(
  "is_available",
  signature(object = "familiarMetricAUCROC"),
  function(object, ...) {
    return(object@outcome_type %in% c("binomial", "multinomial"))
  }
)



# compute_metric_score ---------------------------------------------------------
setMethod(
  "compute_metric_score",
  signature(metric = "familiarMetricAUCROC"),
  function(metric, data, ...) {
    # Suppress NOTES due to non-standard evaluation in data.table
    outcome <- NULL

    if (is_empty(data)) return(callNextMethod())

    if (!is(data, "predictionTableClassification")) {
      ..error_data_not_prediction_table(data, "predictionTableClassification")
    }

    # Get the classes and number of classes in data.
    outcome_classes <- get_outcome_class_levels(data)
    n_classes <- length(outcome_classes)

    # Skip calculation if an AUC cannot be computed.
    if (n_classes <= 1) return(callNextMethod())

    # Remove any entries that lack valid predictions.
    data <- remove_invalid_predictions(data)
    
    # Remove any entries that lack observed values.
    data <- filter_missing_outcome(data)
    if (is_empty(data)) return(callNextMethod())
    
    data <- .as_data_table(data)
    if (nrow(data) <= 1) return(callNextMethod())

    # Define class combinations (>1 in case of multinomial outcomes)
    class_combinations <- utils::combn(outcome_classes, 2)
    n_class_combinations <- ncol(class_combinations)

    # Generate empty auc vector AUC of the ROC is calculated according to Hand,
    # D.J, and Till, R.J. A simple generalisation of the area under the ROC
    # curve for multiple class classification problems, Machine Learning 45
    # 171-186 (2001)
    auc_score <- vector(mode = "numeric", length = n_class_combinations)

    # Iterate over combinations
    for (ii in seq_len(n_class_combinations)) {
      # Find the current positive and negative classes
      positive_class <- class_combinations[1, ii]
      negative_class <- class_combinations[2, ii]

      # Get the probabilities that correspond to the positive and
      # negative class in outcome (g and f in Hand et al.).
      class_probability_positive <- data[outcome == positive_class, ][[positive_class]]
      class_probability_negative <- data[outcome == negative_class, ][[positive_class]]

      # Get number of positive and negative class entries (n0 and n1 in Hand et
      # al.).
      n_positive <- length(class_probability_positive)
      n_negative <- length(class_probability_negative)

      # Calculate AUC only when positive and negative classes are present.
      if (n_positive > 0 & n_negative > 0) {
        # Determine probability ranks
        sample_rank <- data.table::frank(
          x = c(class_probability_positive, class_probability_negative),
          ties.method = "average"
        )

        # Calculate AUC
        auc_score[ii] <- (sum(sample_rank[seq_len(n_positive)]) - n_positive * (n_positive + 1) / 2) /
          (n_positive * n_negative)
        
      } else {
        auc_score[ii] <- NA_real_
      }
    }

    # Calculate mean AUC (eq. 7 from Hand et al.). This has no effect for
    # binomial AUC.
    if (!any(is.finite(auc_score))) return(callNextMethod())
    auc_score <- mean(auc_score, na.rm = TRUE)

    return(auc_score)
  }
)



.get_available_auc_roc_metrics <- function() {
  return(c("auc", "auc_roc"))
}

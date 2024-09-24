familiar:::test_all_metrics_available(
  metrics = familiar:::.get_available_concordance_index_metrics()
)

# Skip remainder on CRAN due to runtimes.
testthat::skip_on_cran()

familiar:::test_all_metrics(
  metrics = familiar:::.get_available_concordance_index_metrics(),
  not_available_single_sample = TRUE,
  not_available_all_samples_identical = TRUE
)



.as_risk <- function(x) {
  familiar::as_prediction_table(
    x = x$predicted_outcome,
    y = list("outcome_time" = x$outcome_time, "outcome_event" = x$outcome_event),
    type = "hazard_ratio"
  )
}

.as_lifetime <- function(x) {
  familiar::as_prediction_table(
    x = x$predicted_outcome,
    y = list("outcome_time" = x$outcome_time, "outcome_event" = x$outcome_event),
    type = "expected_survival_time"
  )
}

data_good_no_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(1, 1, 1, 1, 1),
  "predicted_outcome" = c(10, 8, 6, 4, 2)
)

data_inv_no_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(1, 1, 1, 1, 1),
  "predicted_outcome" = c(2, 4, 6, 8, 10)
)

data_bad_no_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(1, 1, 1, 1, 1),
  "predicted_outcome" = c(5, 5, 5, 5, 5)
)

data_moderate_no_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(1, 1, 1, 1, 1),
  "predicted_outcome" = c(10, 6, 8, 2, 4)
)

data_good_init_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(0, 1, 1, 1, 1),
  "predicted_outcome" = c(10, 8, 6, 4, 2)
)

data_inv_init_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(0, 1, 1, 1, 1),
  "predicted_outcome" = c(2, 4, 6, 8, 10)
)

data_bad_init_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(0, 1, 1, 1, 1),
  "predicted_outcome" = c(5, 5, 5, 5, 5)
)

data_moderate_init_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(0, 1, 1, 1, 1),
  "predicted_outcome" = c(10, 6, 8, 2, 4)
)

data_good_end_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(1, 1, 1, 1, 0),
  "predicted_outcome" = c(10, 8, 6, 4, 2)
)

data_inv_end_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(1, 1, 1, 1, 0),
  "predicted_outcome" = c(2, 4, 6, 8, 10)
)

data_bad_end_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(1, 1, 1, 1, 0),
  "predicted_outcome" = c(5, 5, 5, 5, 5)
)

data_moderate_end_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(1, 1, 1, 1, 0),
  "predicted_outcome" = c(10, 6, 8, 2, 4)
)

data_good_mid_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(1, 1, 0, 0, 1),
  "predicted_outcome" = c(10, 8, 6, 4, 2)
)

data_inv_mid_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(1, 1, 0, 0, 1),
  "predicted_outcome" = c(2, 4, 6, 8, 10)
)

data_bad_mid_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(1, 1, 0, 0, 1),
  "predicted_outcome" = c(5, 5, 5, 5, 5)
)

data_moderate_mid_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(1, 1, 0, 0, 1),
  "predicted_outcome" = c(10, 6, 8, 2, 4)
)

data_all_censoring_risk <- data.table::data.table(
  "outcome_time" = c(1, 2, 3, 4, 5),
  "outcome_event" = c(0, 0, 0, 0, 0),
  "predicted_outcome" = c(10, 8, 6, 4, 2)
)

data_list <- list(
  "good_no_censoring_risk" = data_good_no_censoring_risk,
  "inv_no_censoring_risk" = data_inv_no_censoring_risk,
  "bad_no_censoring_risk" = data_bad_no_censoring_risk,
  "moderate_no_censoring_risk" = data_moderate_no_censoring_risk,
  "good_init_censoring_risk" = data_good_init_censoring_risk,
  "inv_init_censoring_risk" = data_inv_init_censoring_risk,
  "bad_init_censoring_risk" = data_bad_init_censoring_risk,
  "moderate_init_censoring_risk" = data_moderate_init_censoring_risk,
  "good_end_censoring_risk" = data_good_end_censoring_risk,
  "inv_end_censoring_risk" = data_inv_end_censoring_risk,
  "bad_end_censoring_risk" = data_bad_end_censoring_risk,
  "moderate_end_censoring_risk" = data_moderate_end_censoring_risk,
  "good_mid_censoring_risk" = data_good_mid_censoring_risk,
  "inv_mid_censoring_risk" = data_inv_mid_censoring_risk,
  "bad_mid_censoring_risk" = data_bad_mid_censoring_risk,
  "moderate_mid_censoring_risk" = data_moderate_mid_censoring_risk,
  "all_censoring_risk" = data_all_censoring_risk
)



# Test for risk-like predictions -----------------------------------------------
testthat::test_that("Concordance index for risk-like predictions is correct", {
  expected_score <- c(
    1.0, 0.0, 0.5, 0.8, 1.0, 0.0, 
    0.5, 2 / 3, 1.0, 0.0, 0.5, 0.8,
    1.0, 0.0, 0.5, 6 / 7, NA)
  expected_objective <- c(
    1.0, -1.0, 0.0, 0.6, 1.0, -1.0,
    0.0, 1 / 3, 1.0, -1.0, 0.0, 0.6,
    1.0, -1.0, 0.0, 5 / 7, NA)

  # Create metric object.
  metric_object <- familiar:::as_metric(
    metric = "concordance_index",
    outcome_type = "survival"
  )

  # Iterate over the data sets.
  for (ii in seq_along(data_list)) {
    # Check that the metric is available
    testthat::expect_true(familiar:::is_available(metric_object))

    # Compute the metric value.
    score <- familiar:::compute_metric_score(
      metric = metric_object,
      data = .as_risk(data_list[[ii]])
    )

    # Compute the objective score.
    objective_score <- familiar:::compute_objective_score(
      metric = metric_object,
      data = .as_risk(data_list[[ii]])
    )

    # Test the values.
    testthat::expect_equal(score, expected_score[ii])
    testthat::expect_equal(objective_score, expected_objective[ii])
  }
})

# Test for t-imelike predictions ----------------------------------------------
testthat::test_that("Concordance index for time-like predictions is correct", {
  expected_score <- c(
    0.0, 1.0, 0.5, 0.2, 0.0, 1.0,
    0.5, 1 / 3, 0.0, 1.0, 0.5, 0.2,
    0.0, 1.0, 0.5, 1 / 7, NA)
  expected_objective <- c(
    -1.0, 1.0, 0.0, -0.6, -1.0, 1.0, 
    0.0, -1 / 3, -1.0, 1.0, 0.0, -0.6,
    -1.0, 1.0, 0.0, -5 / 7, NA)

  # Create metric object.
  metric_object <- familiar:::as_metric(
    metric = "concordance_index",
    outcome_type = "survival"
  )

  # Iterate over the data sets.
  for (ii in seq_along(data_list)) {
    # Check that the metric is available
    testthat::expect_true(familiar:::is_available(metric_object))

    # Compute the metric value.
    score <- familiar:::compute_metric_score(
      metric = metric_object,
      data = .as_lifetime(data_list[[ii]])
    )

    # Compute the objective score.
    objective_score <- familiar:::compute_objective_score(
      metric = metric_object,
      data = .as_lifetime(data_list[[ii]])
    )

    # Test the values.
    testthat::expect_equal(score, expected_score[ii])
    testthat::expect_equal(objective_score, expected_objective[ii])
  }
})

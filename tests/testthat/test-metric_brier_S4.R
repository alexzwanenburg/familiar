data_good_binomial <- familiar::as_prediction_table(
  x = data.table::data.table(
    "a" = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
    "b" = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
  ),
  y = list("outcome" = factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels = c("a", "b"))),
  type = "classification"
)

data_bad_binomial <- familiar::as_prediction_table(
  x = data.table::data.table(
    "a" = c(.5, .5, .5, .5, .5, .5, .5, .5, .5, .5),
    "b" = c(.5, .5, .5, .5, .5, .5, .5, .5, .5, .5)
  ),
  y = list("outcome" = factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels = c("a", "b"))),
  type = "classification"
)

data_ok_binomial <- familiar::as_prediction_table(
  x = data.table::data.table(
    "a" = c(1.0, 0.9, 0.8, 0.4, 0.3, 0.7, 0.6, 0.2, 0.1, 0.0),
    "b" = c(0.0, 0.1, 0.2, 0.6, 0.7, 0.3, 0.4, 0.8, 0.9, 1.0)
  ),
  y = list("outcome" = factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels = c("a", "b"))),
  type = "classification"
)

data_inv_binomial <- familiar::as_prediction_table(
  x = data.table::data.table(
    "a" = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
    "b" = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
  ),
  y = list("outcome" = factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels = c("a", "b"))),
  type = "classification"
)

data_good_multinomial <- familiar::as_prediction_table(
  x = data.table::data.table(
    "a" = c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0),
    "b" = c(0, 0, 0, 1, 1, 1, 0, 0, 0, 0),
    "c" = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
  ),
  y = list("outcome" = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels = c("a", "b", "c"))),
  type = "classification"
)

data_bad_multinomial <- familiar::as_prediction_table(
  x = data.table::data.table(
    "a" = c(1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3),
    "b" = c(1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3),
    "c" = c(1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3)
  ),
  y = list("outcome" = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels = c("a", "b", "c"))),
  type = "classification"
)

data_ok_multinomial <- familiar::as_prediction_table(
  x = data.table::data.table(
    "a" = c(1.0, 0.2, 0.4, 0.0, 0.4, 0.1, 0.0, 0.0, 0.0, 0.1),
    "b" = c(0.0, 0.5, 0.3, 1.0, 0.3, 0.5, 0.0, 0.6, 0.4, 0.3),
    "c" = c(0.0, 0.3, 0.3, 0.0, 0.3, 0.4, 1.0, 0.4, 0.6, 0.7)
  ),
  y = list("outcome" = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels = c("a", "b", "c"))),
  type = "classification"
)

data_inv_multinomial <- familiar::as_prediction_table(
  x = data.table::data.table(
    "a" = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1),
    "b" = c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0),
    "c" = c(0, 0, 0, 1, 1, 1, 0, 0, 0, 0)
  ),
  y = list("outcome" = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels = c("a", "b", "c"))),
  type = "classification"
)

# Package data in a list for easier iterative tests
data_list <- list(
  "good_binomial" = data_good_binomial,
  "bad_binomial" = data_bad_binomial,
  "ok_binomial" = data_ok_binomial,
  "inv_binomial" = data_inv_binomial,
  "good_multinomial" = data_good_multinomial,
  "bad_multinomial" = data_bad_multinomial,
  "ok_multinomial" = data_ok_multinomial,
  "inv_multinomial" = data_inv_multinomial
)

familiar:::test_all_metrics_available(
  metrics = familiar:::.get_available_brier_metrics()
)
familiar:::test_all_metrics(
  metrics = familiar:::.get_available_brier_metrics()
)

# Brier score ------------------------------------------------------------------
testthat::test_that("Brier score is correct", {
  expected_score <- c(0.0, 1 / 4, 0.18, 1.0, 0.0, 1 / 3, 391 / 2000, 1.0)
  expected_objective <- c(1.0, 3 / 4, 0.82, 0.0, 1.0, 2 / 3, 1609 / 2000, 0.0)

  # Iterate over the data sets.
  for (ii in seq_along(data_list)) {
    # Create metric object.
    metric_object <- familiar:::as_metric(
      metric = "brier",
      outcome_type = data_list[[ii]]@outcome_type
    )
    
    # Set baseline-value explicitly.
    metric_object@baseline_value <- 1.0

    # Check that the metric is available
    testthat::expect_true(familiar:::is_available(metric_object))

    # Compute the metric score.
    score <- familiar:::compute_metric_score(
      metric = metric_object,
      data = data_list[[ii]]
    )

    objective_score <- familiar:::compute_objective_score(
      metric = metric_object,
      data = data_list[[ii]]
    )
    
    # Test the values.
    testthat::expect_equal(score, expected_score[ii])
    testthat::expect_equal(objective_score, expected_objective[ii])
  }
})

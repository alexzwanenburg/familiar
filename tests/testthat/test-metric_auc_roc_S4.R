familiar:::test_all_metrics_available(metrics = familiar:::.get_available_auc_roc_metrics())

# Skip remainder on CRAN due to runtimes.
testthat::skip_on_cran()

familiar:::test_all_metrics(
  metrics = familiar:::.get_available_auc_roc_metrics(),
  not_available_single_sample = TRUE,
  not_available_all_samples_identical = TRUE
)

data_good_binomial <- data.table::data.table(
  "outcome" = factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels = c("a", "b")),
  "predicted_class" = factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels = c("a", "b")),
  "predicted_class_probability_a" = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
  "predicted_class_probability_b" = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1)
)

data_bad_binomial <- data.table::data.table(
  "outcome" = factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels = c("a", "b")),
  "predicted_class" = factor(c("a", "b", "a", "b", "a", "b", "a", "b", "a", "b"), levels = c("a", "b")),
  "predicted_class_probability_a" = c(.5, .5, .5, .5, .5, .5, .5, .5, .5, .5),
  "predicted_class_probability_b" = c(.5, .5, .5, .5, .5, .5, .5, .5, .5, .5)
)

data_ok_binomial <- data.table::data.table(
  "outcome" = factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels = c("a", "b")),
  "predicted_class" = factor(c("a", "a", "a", "b", "b", "a", "a", "b", "b", "b"), levels = c("a", "b")),
  "predicted_class_probability_a" = c(1.0, 0.9, 0.8, 0.4, 0.3, 0.7, 0.6, 0.2, 0.1, 0.0),
  "predicted_class_probability_b" = c(0.0, 0.1, 0.2, 0.6, 0.7, 0.3, 0.4, 0.8, 0.9, 1.0)
)

data_inv_binomial <- data.table::data.table(
  "outcome" = factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels = c("a", "b")),
  "predicted_class" = factor(c("b", "b", "b", "b", "b", "a", "a", "a", "b", "b"), levels = c("a", "b")),
  "predicted_class_probability_a" = c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
  "predicted_class_probability_b" = c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0)
)

data_good_multinomial <- data.table::data.table(
  "outcome" = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels = c("a", "b", "c")),
  "predicted_class" = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels = c("a", "b", "c")),
  "predicted_class_probability_a" = c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0),
  "predicted_class_probability_b" = c(0, 0, 0, 1, 1, 1, 0, 0, 0, 0),
  "predicted_class_probability_c" = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1)
)

data_bad_multinomial <- data.table::data.table(
  "outcome" = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels = c("a", "b", "c")),
  "predicted_class" = factor(c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a"), levels = c("a", "b", "c")),
  "predicted_class_probability_a" = c(1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3),
  "predicted_class_probability_b" = c(1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3),
  "predicted_class_probability_c" = c(1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3, 1 / 3)
)

data_ok_multinomial <- data.table::data.table(
  "outcome" = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels = c("a", "b", "c")),
  "predicted_class" = factor(c("a", "b", "a", "b", "a", "b", "c", "b", "c", "c"), levels = c("a", "b", "c")),
  "predicted_class_probability_a" = c(1.0, 0.2, 0.4, 0.0, 0.4, 0.1, 0.0, 0.0, 0.0, 0.1),
  "predicted_class_probability_b" = c(0.0, 0.5, 0.3, 1.0, 0.3, 0.5, 0.0, 0.6, 0.4, 0.3),
  "predicted_class_probability_c" = c(0.0, 0.3, 0.3, 0.0, 0.3, 0.4, 1.0, 0.4, 0.6, 0.7)
)

data_inv_multinomial <- data.table::data.table(
  "outcome" = factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels = c("a", "b", "c")),
  "predicted_class" = factor(c("b", "b", "b", "c", "c", "c", "a", "a", "a", "a"), levels = c("a", "b", "c")),
  "predicted_class_probability_a" = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1),
  "predicted_class_probability_b" = c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0),
  "predicted_class_probability_c" = c(0, 0, 0, 1, 1, 1, 0, 0, 0, 0)
)

# Package data in a list for easier iterative tests
data_list <- list(
  "good_binomial" = list("data" = data_good_binomial, "outcome_type" = "binomial"),
  "bad_binomial" = list("data" = data_bad_binomial, "outcome_type" = "binomial"),
  "ok_binomial" = list("data" = data_ok_binomial, "outcome_type" = "binomial"),
  "inv_binomial" = list("data" = data_inv_binomial, "outcome_type" = "binomial"),
  "good_multinomial" = list("data" = data_good_multinomial, "outcome_type" = "multinomial"),
  "bad_multinomial" = list("data" = data_bad_multinomial, "outcome_type" = "multinomial"),
  "ok_multinomial" = list("data" = data_ok_multinomial, "outcome_type" = "multinomial"),
  "inv_multinomial" = list("data" = data_inv_multinomial, "outcome_type" = "multinomial")
)

# Area under the curve ---------------------------------------------------------
testthat::test_that("AUC-ROC is correct", {
  expected_score <- c(1.0, 0.5, 21 / 25, 0.0, 1.0, 0.5, 61 / 72, 1 / 3)
  expected_objective <- c(1.0, 0.0, 17 / 25, -1.0, 1.0, 0.0, 25 / 36, -1 / 3)

  # Iterate over the data sets.
  for (ii in seq_along(data_list)) {
    # Create metric object.
    metric_object <- familiar:::as_metric(
      metric = "auc_roc",
      outcome_type = data_list[[ii]]$outcome_type)

    # Set baseline-value explicitly.
    metric_object@baseline_value <- 0.5

    # Check that the metric is available
    testthat::expect_equal(familiar:::is_available(metric_object), TRUE)

    # Compute the metric value.
    score <- familiar:::compute_metric_score(
      metric = metric_object,
      data = data_list[[ii]]$data)

    # Compute the objective score.
    objective_score <- familiar:::compute_objective_score(
      metric = metric_object,
      data = data_list[[ii]]$data)

    # Test the values.
    testthat::expect_equal(score, expected_score[ii])
    testthat::expect_equal(objective_score, expected_objective[ii])
  }
})

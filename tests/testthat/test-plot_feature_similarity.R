# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Generic test
# Feature similarity can be computed for failing survival predictions.
familiar:::test_plots(
  plot_function = familiar:::plot_feature_similarity,
  not_available_single_feature = TRUE,
  not_available_single_sample = TRUE,
  not_available_all_predictions_fail = FALSE,
  not_available_some_predictions_fail = FALSE,
  outcome_type_available = c("count", "continuous", "binomial", "multinomial", "survival"),
  data_element = "feature_similarity",
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_feature_similarity,
  data_element = "feature_similarity",
  outcome_type_available = c("count", "continuous", "binomial", "multinomial", "survival"),
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_feature_similarity,
  data_element = "feature_similarity",
  outcome_type_available = c("count", "continuous", "binomial", "multinomial", "survival"),
  plot_args = list("facet_by" = c("learner", "fs_method", "data_set")),
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_feature_similarity,
  data_element = "feature_similarity",
  outcome_type_available = c("count", "continuous", "binomial", "multinomial", "survival"),
  plot_args = list("show_dendrogram" = c("left", "bottom")),
  debug = debug_flag
)

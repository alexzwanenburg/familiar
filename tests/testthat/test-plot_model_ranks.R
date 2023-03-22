# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

debug_flag <- FALSE

# Generic test.
familiar:::test_plots(
  plot_function = familiar:::plot_model_signature_occurrence,
  not_available_no_samples = FALSE,
  not_available_all_predictions_fail = FALSE,
  not_available_some_predictions_fail = FALSE,
  outcome_type_available = c("count", "continuous", "binomial", "multinomial", "survival"),
  data_element = "model_vimp",
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_signature_occurrence,
  data_element = "model_vimp",
  outcome_type_available = c("count", "continuous", "binomial", "multinomial", "survival"),
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_signature_occurrence,
  data_element = "model_vimp",
  outcome_type_available = c("count", "continuous", "binomial", "multinomial", "survival"),
  plot_args = list(
    "facet_by" = c("learner"),
    "color_by" = c("fs_method")),
  debug = debug_flag
)

# Generic test.
familiar:::test_plots(
  plot_function = familiar:::plot_model_signature_variable_importance,
  not_available_no_samples = FALSE,
  not_available_all_predictions_fail = FALSE,
  not_available_some_predictions_fail = FALSE,
  outcome_type_available = c("count", "continuous", "binomial", "multinomial", "survival"),
  data_element = "model_vimp",
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_signature_variable_importance,
  data_element = "model_vimp",
  outcome_type_available = c("count", "continuous", "binomial", "multinomial", "survival"),
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_signature_variable_importance,
  data_element = "model_vimp",
  outcome_type_available = c("count", "continuous", "binomial", "multinomial", "survival"),
  plot_args = list(
    "facet_by" = c("learner"),
    "color_by" = c("fs_method")),
  debug = debug_flag
)

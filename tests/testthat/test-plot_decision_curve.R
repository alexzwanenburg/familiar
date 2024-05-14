# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Generic test.
familiar:::test_plots(
  plot_function = familiar:::plot_decision_curve,
  outcome_type_available = c("binomial", "multinomial", "survival"),
  data_element = "decision_curve_analyis",
  not_available_all_prospective = TRUE,
  not_available_any_prospective = TRUE,
  not_available_single_sample = TRUE,
  not_available_extreme_probability = TRUE,
  debug = debug_flag
)

# Test with step-wise confidence interval
familiar:::test_plots(
  plot_function = familiar:::plot_decision_curve,
  outcome_type_available = c("binomial", "multinomial", "survival"),
  data_element = "decision_curve_analyis",
  test_specific_config = TRUE,
  plot_args = list("conf_int_style" = "step"),
  debug = debug_flag
)

# Test without confidence interval shown
familiar:::test_plots(
  plot_function = familiar:::plot_decision_curve,
  outcome_type_available = c("binomial", "multinomial", "survival"),
  data_element = "decision_curve_analyis",
  test_specific_config = TRUE,
  plot_args = list("conf_int_style" = "none"),
  debug = debug_flag
)

# Test without confidence interval
familiar:::test_plots(
  plot_function = familiar:::plot_decision_curve,
  outcome_type_available = c("binomial", "multinomial", "survival"),
  data_element = "decision_curve_analyis",
  estimation_type = "point",
  test_specific_config = TRUE,
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_decision_curve,
  data_element = "decision_curve_analyis",
  estimation_type = "point",
  outcome_type_available = c("binomial", "multinomial", "survival"),
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_decision_curve,
  data_element = "decision_curve_analyis",
  outcome_type_available = c("binomial"),
  plot_args = list(
    "facet_by" = c("learner", "fs_method"),
    "color_by" = "data_set"),
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_decision_curve,
  data_element = "decision_curve_analyis",
  outcome_type_available = c("multinomial"),
  plot_args = list(
    "facet_by" = c("learner", "fs_method"),
    "color_by" = c("data_set", "positive_class")),
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_decision_curve,
  data_element = "decision_curve_analyis",
  outcome_type_available = c("survival"),
  plot_args = list(
    "facet_by" = c("learner", "fs_method"),
    "color_by" = c("data_set")),
  debug = debug_flag
)

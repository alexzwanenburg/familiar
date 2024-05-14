# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

familiar:::test_plots(
  plot_function = familiar:::plot_calibration_data,
  data_element = "calibration_data",
  not_available_all_prospective = TRUE,
  not_available_any_prospective = TRUE,
  not_available_single_sample = TRUE,
  debug = debug_flag
)

# With the ensemble detail level.
familiar:::test_plots(
  plot_function = familiar:::plot_calibration_data,
  detail_level = "ensemble",
  data_element = "calibration_data",
  not_available_all_prospective = TRUE,
  not_available_any_prospective = TRUE,
  not_available_single_sample = TRUE,
  debug = debug_flag
)

# For bias-corrected data
familiar:::test_plots(
  plot_function = familiar:::plot_calibration_data,
  estimation_type = "bias_correction",
  data_element = "calibration_data",
  not_available_all_prospective = TRUE,
  not_available_any_prospective = TRUE,
  not_available_single_sample = TRUE,
  debug = debug_flag
)

# For point estimates
familiar:::test_plots(
  plot_function = familiar:::plot_calibration_data,
  estimation_type = "point",
  data_element = "calibration_data",
  not_available_all_prospective = TRUE,
  not_available_any_prospective = TRUE,
  not_available_single_sample = TRUE,
  debug = debug_flag
)

# Test without density plot
familiar:::test_plots(
  plot_function = familiar:::plot_calibration_data,
  data_element = "calibration_data",
  test_specific_config = TRUE,
  plot_args = list("show_density" = FALSE),
  debug = debug_flag
)

# Test without the goodness of fit data
familiar:::test_plots(
  plot_function = familiar:::plot_calibration_data,
  data_element = "calibration_data",
  test_specific_config = TRUE,
  plot_args = list("show_goodness_of_fit" = FALSE),
  debug = debug_flag
)

# Test without the calibration fit data
familiar:::test_plots(
  plot_function = familiar:::plot_calibration_data,
  data_element = "calibration_data",
  test_specific_config = TRUE,
  plot_args = list("show_calibration_fit" = FALSE),
  debug = debug_flag
)

# Test without either fit data
familiar:::test_plots(
  plot_function = familiar:::plot_calibration_data,
  data_element = "calibration_data",
  test_specific_config = TRUE,
  plot_args = list(
    "show_calibration_fit" = FALSE,
    "show_goodness_of_fit" = FALSE),
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_calibration_data,
  data_element = "calibration_data",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  debug = debug_flag
)

# Test alignment of different plots from prediction tables.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_calibration_data,
  data_element = "calibration_data",
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  prediction_type = list("survival" = "survival_probability"),
  use_prediction_table = TRUE,
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_calibration_data,
  data_element = "calibration_data",
  outcome_type_available = c("continuous"),
  plot_args = list(
    "facet_by" = c("fs_method", "learner"),
    "color_by" = c("data_set")),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_calibration_data,
  data_element = "calibration_data",
  outcome_type_available = c("binomial"),
  plot_args = list(
    "facet_by" = c("fs_method", "learner"),
    "color_by" = c("data_set")),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_calibration_data,
  data_element = "calibration_data",
  outcome_type_available = c("multinomial"),
  plot_args = list(
    "facet_by" = c("fs_method", "learner"),
    "color_by" = c("data_set", "positive_class")),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_calibration_data,
  data_element = "calibration_data",
  outcome_type_available = c("survival"),
  plot_args = list(
    "facet_by" = c("fs_method", "learner"),
    "color_by" = c("data_set")),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_calibration_data,
  data_element = "calibration_data",
  outcome_type_available = c("continuous", "binomial", "survival"),
  plot_args = list("facet_by" = c("learner", "fs_method", "data_set")),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_calibration_data,
  data_element = "calibration_data",
  outcome_type_available = c("multinomial"),
  plot_args = list(
    "facet_by" = c("learner", "fs_method", "data_set"),
    "color_by" = "positive_class"),
  debug = debug_flag
)

# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Generic test
# Note that one-sample kaplan-meier curves can be created.
familiar:::test_plots(
  plot_function = familiar:::plot_kaplan_meier,
  not_available_all_prospective = TRUE,
  outcome_type_available = c("survival"),
  data_element = "risk_stratification_data",
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_kaplan_meier,
  data_element = "risk_stratification_data",
  outcome_type_available = c("survival"),
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_kaplan_meier,
  data_element = "risk_stratification_data",
  outcome_type_available = c("survival"),
  plot_args = list(
    "facet_by" = c("learner", "fs_method", "data_set"),
    "color_by" = "risk_group"),
  debug = debug_flag
)

# Fixed stratification with 3 groups works correctly.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_kaplan_meier,
  data_element = "risk_stratification_data",
  outcome_type_available = c("survival"),
  experiment_args = list(
    stratification_method = "fixed"),
  plot_args = list(
    "facet_by" = c("learner", "fs_method", "data_set"),
    "color_by" = "risk_group"),
  debug = debug_flag
)

# Fixed stratification with 5 groups works correctly.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_kaplan_meier,
  data_element = "risk_stratification_data",
  outcome_type_available = c("survival"),
  experiment_args = list(
    stratification_method = "fixed",
    stratification_threshold = c(0.20, 0.40, 0.60, 0.80)),
  plot_args = list(
    "facet_by" = c("learner", "fs_method", "data_set"),
    "color_by" = "risk_group"),
  debug = debug_flag
)

# Fixed stratification with 5 groups works correctly.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_kaplan_meier,
  data_element = "risk_stratification_data",
  outcome_type_available = c("survival"),
  experiment_args = list(
    stratification_method = c("median", "fixed"),
    stratification_threshold = c(0.20, 0.40, 0.60, 0.80)),
  plot_args = list(
    "facet_by" = c("learner", "fs_method", "data_set"),
    "color_by" = "risk_group",
    "split_by" = "stratification_method"),
  debug = debug_flag
)

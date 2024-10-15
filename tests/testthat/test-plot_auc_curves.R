# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# AUC-ROC plots ----------------------------------------------------------------

familiar:::test_plots(
  plot_function = familiar::plot_auc_roc_curve,
  data_element = "auc_data",
  outcome_type_available = c("binomial", "multinomial"),
  not_available_all_prospective = TRUE,
  not_available_any_prospective = TRUE,
  not_available_single_sample = TRUE,
  debug = debug_flag
)

# Test with step-wise confidence interval
familiar:::test_plots(
  plot_function = familiar::plot_auc_roc_curve,
  data_element = "auc_data",
  outcome_type_available = c("binomial", "multinomial"),
  test_specific_config = TRUE,
  plot_args = list("conf_int_style" = "step"),
  debug = debug_flag
)

# Test without confidence interval shown
familiar:::test_plots(
  plot_function = familiar::plot_auc_roc_curve,
  data_element = "auc_data",
  outcome_type_available = c("binomial", "multinomial"),
  test_specific_config = TRUE,
  plot_args = list("conf_int_style" = "none"),
  debug = debug_flag
)

# Test without confidence interval
familiar:::test_plots(
  plot_function = familiar::plot_auc_roc_curve,
  data_element = "auc_data",
  outcome_type_available = c("binomial", "multinomial"),
  estimation_type = "point",
  test_specific_config = TRUE,
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar::plot_auc_roc_curve,
  data_element = "auc_data",
  outcome_type_available = c("binomial", "multinomial"),
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar::plot_auc_roc_curve,
  data_element = "auc_data",
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list(
    "facet_by" = c("vimp_method", "learner"),
    "color_by" = c("data_set", "positive_class")),
  debug = debug_flag
)

# AUC precision-recall plots ---------------------------------------------------

familiar:::test_plots(
  plot_function = familiar::plot_auc_precision_recall_curve,
  data_element = "auc_data",
  outcome_type_available = c("binomial", "multinomial"),
  not_available_all_prospective = TRUE,
  not_available_any_prospective = TRUE,
  not_available_single_sample = TRUE,
  debug = debug_flag
)

# Test with step-wise confidence interval
familiar:::test_plots(
  plot_function = familiar::plot_auc_precision_recall_curve,
  data_element = "auc_data",
  outcome_type_available = c("binomial", "multinomial"),
  test_specific_config = TRUE,
  plot_args = list("conf_int_style" = "step"),
  debug = debug_flag
)

# Test without confidence interval shown
familiar:::test_plots(
  plot_function = familiar::plot_auc_precision_recall_curve,
  data_element = "auc_data",
  outcome_type_available = c("binomial", "multinomial"),
  test_specific_config = TRUE,
  plot_args = list("conf_int_style" = "none"),
  debug = debug_flag
)

# Test without confidence interval
familiar:::test_plots(
  plot_function = familiar::plot_auc_precision_recall_curve,
  data_element = "auc_data",
  outcome_type_available = c("binomial", "multinomial"),
  estimation_type = "point",
  test_specific_config = TRUE,
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar::plot_auc_precision_recall_curve,
  data_element = "auc_data",
  outcome_type_available = c("binomial", "multinomial"),
  debug = debug_flag
)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar::plot_auc_roc_curve,
  data_element = "auc_data",
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list(
    "facet_by" = c("vimp_method", "learner"),
    "color_by" = c("data_set", "positive_class")),
  debug = debug_flag
)

# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Generic test.
familiar:::test_plots(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  not_available_all_prospective = TRUE,
  not_available_any_prospective = c("binomial", "multinomial", "survival"),
  not_available_single_sample = c("binomial", "multinomial", "survival"),
  outcome_type_available = c("count", "continuous", "binomial", "multinomial", "survival"),
  debug = debug_flag
)

# Violin-plot (categorical) ----------------------------------------------------
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list("facet_by" = c("data_set", "learner", "fs_method")),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("auc_roc", "accuracy"),
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list(
    "facet_by" = c("data_set", "learner", "fs_method"),
    "x_axis_by" = "metric"),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("auc_roc", "accuracy"),
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list(
    "facet_by" = c("learner", "fs_method"),
    "x_axis_by" = "data_set",
    "color_by" = "metric"),
  debug = debug_flag
)

# Violin-plot (numeric) --------------------------------------------------------
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  outcome_type_available = c("count", "continuous"),
  plot_args = list("facet_by" = c("data_set", "learner", "fs_method")),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("rmse", "mae"),
  outcome_type_available = c("count", "continuous"),
  plot_args = list(
    "facet_by" = c("data_set", "learner", "fs_method"),
    "x_axis_by" = "metric"),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("rmse", "mae"),
  outcome_type_available = c("count", "continuous"),
  plot_args = list(
    "facet_by" = c("learner", "fs_method"),
    "x_axis_by" = "data_set",
    "color_by" = "metric"),
  debug = debug_flag
)

# Violin-plot (survival) -------------------------------------------------------
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  outcome_type_available = c("survival"),
  plot_args = list("facet_by" = c("data_set", "learner", "fs_method")),
  debug = debug_flag
)

# Bar plot (categorical) -------------------------------------------------------
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list(
    "plot_type" = "barplot",
    "facet_by" = c("data_set", "learner", "fs_method")),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("auc_roc", "accuracy"),
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list(
    "plot_type" = "barplot",
    "facet_by" = c("data_set", "learner", "fs_method"),
    "x_axis_by" = "metric"),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("auc_roc", "accuracy"),
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list(
    "plot_type" = "barplot",
    "facet_by" = c("learner", "fs_method"),
    "x_axis_by" = "data_set",
    "color_by" = "metric"),
  debug = debug_flag
)

# Bar plot (numeric) -----------------------------------------------------------
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  outcome_type_available = c("count", "continuous"),
  plot_args = list(
    "plot_type" = "barplot",
    "facet_by" = c("data_set", "learner", "fs_method")),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("rmse", "mae"),
  outcome_type_available = c("count", "continuous"),
  plot_args = list(
    "plot_type" = "barplot",
    "facet_by" = c("data_set", "learner", "fs_method"),
    "x_axis_by" = "metric"),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("rmse", "mae"),
  outcome_type_available = c("count", "continuous"),
  plot_args = list(
    "plot_type" = "barplot",
    "facet_by" = c("learner", "fs_method"),
    "x_axis_by" = "data_set",
    "color_by" = "metric"),
  debug = debug_flag
)

# Bar plot (survival) ----------------------------------------------------------
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  outcome_type_available = c("survival"),
  plot_args = list(
    "plot_type" = "barplot",
    "facet_by" = c("data_set", "learner", "fs_method")),
  debug = debug_flag
)

# Box plot (categorical) -------------------------------------------------------
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list(
    "plot_type" = "boxplot",
    "facet_by" = c("data_set", "learner", "fs_method")),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("auc_roc", "accuracy"),
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list(
    "plot_type" = "boxplot",
    "facet_by" = c("data_set", "learner", "fs_method"),
    "x_axis_by" = "metric"),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("auc_roc", "accuracy"),
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list(
    "plot_type" = "boxplot",
    "facet_by" = c("learner", "fs_method"),
    "x_axis_by" = "data_set",
    "color_by" = "metric"),
  debug = debug_flag
)

# Box plot (numeric) -----------------------------------------------------------
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  outcome_type_available = c("count", "continuous"),
  plot_args = list(
    "plot_type" = "boxplot",
    "facet_by" = c("data_set", "learner", "fs_method")),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("rmse", "mae"),
  outcome_type_available = c("count", "continuous"),
  plot_args = list(
    "plot_type" = "boxplot",
    "facet_by" = c("data_set", "learner", "fs_method"),
    "x_axis_by" = "metric"),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("rmse", "mae"),
  outcome_type_available = c("count", "continuous"),
  plot_args = list(
    "plot_type" = "boxplot",
    "facet_by" = c("learner", "fs_method"),
    "x_axis_by" = "data_set",
    "color_by" = "metric"),
  debug = debug_flag
)

# Box plot (survival) ----------------------------------------------------------
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  outcome_type_available = c("survival"),
  plot_args = list(
    "plot_type" = "boxplot",
    "facet_by" = c("data_set", "learner", "fs_method")),
  debug = debug_flag
)

# Heatmap (categorical) --------------------------------------------------------
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list("plot_type" = "heatmap"),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("auc_roc", "accuracy"),
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list("plot_type" = "heatmap"),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("auc_roc", "accuracy"),
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list(
    "plot_type" = "heatmap",
    "facet_by" = c("data_set", "metric"),
    "x_axis_by" = "learner",
    "y_axis_by" = "fs_method"),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("auc_roc", "accuracy"),
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list(
    "plot_type" = "heatmap",
    "annotate_performance" = "value_ci",
    "facet_by" = c("data_set", "metric"),
    "x_axis_by" = "learner",
    "y_axis_by" = "fs_method"),
  debug = debug_flag
)

# Heatmap (numeric) ------------------------------------------------------------
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  outcome_type_available = c("binomial", "multinomial"),
  plot_args = list("plot_type" = "heatmap"),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("rmse", "mae"),
  outcome_type_available = c("count", "continuous"),
  plot_args = list("plot_type" = "heatmap"),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("rmse", "mae"),
  outcome_type_available = c("count", "continuous"),
  plot_args = list(
    "plot_type" = "heatmap",
    "facet_by" = c("data_set", "metric"),
    "x_axis_by" = "learner",
    "y_axis_by" = "fs_method"),
  debug = debug_flag
)

familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  metric = c("rmse", "mae"),
  outcome_type_available = c("count", "continuous"),
  plot_args = list(
    "plot_type" = "heatmap",
    "annotate_performance" = "value_ci",
    "facet_by" = c("data_set", "metric"),
    "x_axis_by" = "learner",
    "y_axis_by" = "fs_method"),
  debug = debug_flag
)

# Heatmap (survival) -----------------------------------------------------------
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_model_performance,
  data_element = "model_performance",
  outcome_type_available = c("survival"),
  plot_args = list("plot_type" = "heatmap"),
  debug = debug_flag
)

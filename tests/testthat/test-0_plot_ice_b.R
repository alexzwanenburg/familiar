# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_ice,
  data_element = "ice_data",
  outcome_type_available = "survival",
  features = c("feature_4"),
  sample_limit = 20L,
  n_sample_points = 10L,
  debug = debug_flag
)

# 1D plot without novelty, and with anchored values.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_ice,
  data_element = "ice_data",
  outcome_type_available = "survival",
  features = c("feature_1"),
  plot_args = list(
    "anchor_values" = list("feature_1" = 0.4),
    "value_scales" = "figure"),
  sample_limit = 20L,
  n_sample_points = 10L,
  debug = debug_flag
)

# 1D plot with novelty, and with anchored values.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_ice,
  data_element = "ice_data",
  outcome_type_available = "survival",
  features = c("feature_1"),
  plot_args = list(
    "anchor_values" = list("feature_1" = 0.4),
    "value_scales" = "figure"),
  sample_limit = 20L,
  n_sample_points = 10L,
  create_novelty_detector = TRUE,
  debug = debug_flag
)

# 1D plot with novelty, but no ICE
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_ice,
  data_element = "ice_data",
  outcome_type_available = "survival",
  features = c("feature_1"),
  plot_args = list("show_ice" = FALSE),
  sample_limit = 20L,
  n_sample_points = 10L,
  create_novelty_detector = TRUE,
  debug = debug_flag
)

# 1D plot with novelty, but no PD
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_ice,
  data_element = "ice_data",
  outcome_type_available = "survival",
  features = c("feature_1"),
  plot_args = list("show_pd" = FALSE),
  sample_limit = 20L,
  n_sample_points = 10L,
  create_novelty_detector = TRUE,
  debug = debug_flag
)


# 1D plot with novelty and colouring, and with anchored values.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_ice,
  data_element = "ice_data",
  outcome_type_available = "multinomial",
  features = c("feature_1"),
  plot_args = list(
    "anchor_values" = list("feature_1" = 0.4),
    "value_scales" = "figure",
    "color_by" = "positive_class",
    "facet_by" = c("data_set", "vimp_method", "learner")),
  sample_limit = 20L,
  n_sample_points = 10L,
  create_novelty_detector = TRUE,
  debug = debug_flag
)


# 1D plot with novelty and colouring, with anchored values and scaling per
# facet.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_ice,
  data_element = "ice_data",
  outcome_type_available = "multinomial",
  features = c("feature_1"),
  plot_args = list(
    "anchor_values" = list("feature_1" = 0.40),
    "value_scales" = "facet",
    "color_by" = "positive_class",
    "facet_by" = c("data_set", "vimp_method", "learner")),
  sample_limit = 20L,
  n_sample_points = 10L,
  create_novelty_detector = TRUE,
  debug = debug_flag
)


# 1D plot with novelty and colouring, and with anchored values.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_ice,
  data_element = "ice_data",
  outcome_type_available = "multinomial",
  features = c("feature_1"),
  plot_args = list(
    "anchor_values" = list("feature_1" = 0.40),
    "value_scales" = "figure",
    "split_by" = c("vimp_method", "learner"),
    "color_by" = "positive_class",
    "facet_by" = "data_set"),
  sample_limit = 20L,
  n_sample_points = 10L,
  create_novelty_detector = TRUE,
  debug = debug_flag
)

# 2D plot without novelty, and with anchored values.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_ice,
  data_element = "ice_data",
  outcome_type_available = "survival",
  features = c("feature_1", "feature_4"),
  plot_args = list(
    "anchor_values" = list(
      "feature_1" = 0.4,
      "feature_4" = "better"),
    "value_scales" = "figure"),
  sample_limit = 20L,
  n_sample_points = 10L,
  debug = debug_flag
)

# 2D plot with novelty, and with anchored values.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_ice,
  data_element = "ice_data",
  outcome_type_available = "survival",
  features = c("feature_1", "feature_4"),
  plot_args = list(
    "anchor_values" = list(
      "feature_1" = 0.4,
      "feature_4" = "better"),
    "value_scales" = "figure"),
  sample_limit = 20L,
  n_sample_points = 10L,
  create_novelty_detector = TRUE,
  debug = debug_flag
)

# 2D plot with novelty, and with anchored values.
familiar:::test_plot_ordering(
  plot_function = familiar:::plot_ice,
  data_element = "ice_data",
  outcome_type_available = "multinomial",
  features = c("feature_1", "feature_2a"),
  plot_args = list(
    "anchor_values" = list(
      "feature_1" = 0.4,
      "feature_2a" = 0.4),
    "value_scales" = "figure"),
  sample_limit = 20L,
  n_sample_points = 10L,
  debug = debug_flag
)

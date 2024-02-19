# Don't perform any further tests on CRAN due to running time.
testthat::skip_on_cran()

debug_flag <- FALSE

# Default
results <- familiar:::test_export_specific(
  export_function = familiar:::export_ice_data,
  data_element = "ice_data",
  sample_limit = 20L,
  create_novelty_detector = FALSE,
  debug = debug_flag
)

testthat::test_that("Sample limit is correctly propagated", {
  for (data_set in results) {
    # Get sample names.
    sample_names <- unique(c(data_set[[1]]@data$sample))

    testthat::expect_equal(length(sample_names), 20)
  }
})

# Test that test_export_specific works when specifying a single feature.
results <- familiar:::test_export_specific(
  export_function = familiar:::export_ice_data,
  outcome_type_available = "survival",
  data_element = "ice_data",
  features = c("feature_1"),
  sample_limit = 20L,
  create_novelty_detector = FALSE,
  debug = debug_flag
)

testthat::test_that("A single feature can be specified.", {
  testthat::expect_equal(results$survival[[1]]@identifiers$feature_x, "feature_1")
  testthat::expect_equal(length(results$survival), 1L)
})

# Test that test_export_specific works when specifying two features.
results <- familiar:::test_export_specific(
  export_function = familiar:::export_ice_data,
  outcome_type_available = "survival",
  data_element = "ice_data",
  features = c("feature_4", "feature_1"),
  sample_limit = 20L,
  create_novelty_detector = FALSE,
  debug = debug_flag
)

testthat::test_that("Two features can be specified.", {
  testthat::expect_equal(results$survival[[1]]@identifiers$feature_x, "feature_4")
  testthat::expect_equal(results$survival[[1]]@identifiers$feature_y, "feature_1")
  testthat::expect_equal(length(results$survival), 1L)
})

# Test that test_export_specific works when specifying two features and their
# ranges.
results <- familiar:::test_export_specific(
  export_function = familiar:::export_ice_data,
  outcome_type_available = "survival",
  data_element = "ice_data",
  features = c("feature_4", "feature_1"),
  feature_x_range = c("better", "best"),
  feature_y_range = c(0.25, 0.75),
  n_sample_points = 5L,
  sample_limit = 20L,
  create_novelty_detector = FALSE,
  debug = debug_flag
)

testthat::test_that("Ranges and features can specified.", {
  testthat::expect_setequal(unique(results$survival[[1]]@data$feature_x_value), c("better", "best"))
  testthat::expect_true(all(results$survival[[1]]@data$feature_y_value <= 0.75))
  testthat::expect_true(all(results$survival[[1]]@data$feature_y_value >= 0.25))
})

# Test that test_export_specific works when specifying two features and their
# specific ranges.
results <- familiar:::test_export_specific(
  export_function = familiar:::export_ice_data,
  outcome_type_available = "survival",
  data_element = "ice_data",
  features = c("feature_4", "feature_3a"),
  feature_x_range = c("good", "better"),
  feature_y_range = c("round", "square"),
  sample_limit = 20L,
  create_novelty_detector = FALSE,
  debug = debug_flag
)

testthat::test_that("Ranges and features can specified.", {
  testthat::expect_setequal(unique(results$survival[[1]]@data$feature_x_value), c("good", "better"))
  testthat::expect_setequal(unique(results$survival[[1]]@data$feature_y_value), c("round", "square"))
})

# Don't perform any further tests on CRAN due to running time.
testthat::skip_on_cran()

# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "binomial",
  to_data_object = FALSE)

# Create data assignment object.
experiment_data_assignment <- familiar::precompute_data_assignment(
  data = data,
  experimental_design = "bs(fs+mb,3)",
  outcome_type = "binomial",
  outcome_column = "cell_malignancy",
  sample_id_column = "id",
  class_levels = c("benign", "malignant"),
  verbose = FALSE)

testthat::test_that("Assert that data assignment performs correctly", {
  testthat::expect_equal(is.null(experiment_data_assignment@feature_info), TRUE)
  testthat::expect_equal(is.null(experiment_data_assignment@vimp_table_list), TRUE)
})

# Create feature info object.
experiment_feature_info <- familiar::precompute_feature_info(
  data = data,
  experiment_data = experiment_data_assignment,
  outcome_type = "binomial",
  outcome_column = "cell_malignancy",
  sample_id_column = "id",
  class_levels = c("benign", "malignant"),
  verbose = FALSE)

testthat::test_that("Assert that feature info computation performs correctly", {
  # Check that 6 subsets are present, namely generic + complete
  testthat::expect_equal(length(experiment_feature_info@feature_info), 4L)

  for (feature_info_set in names(experiment_feature_info@feature_info)) {
    for (feature_info in experiment_feature_info@feature_info[[feature_info_set]]) {
      # Assert that feature info has the correct class.
      testthat::expect_s4_class(feature_info, "featureInfo")

      # Assert that the feature information is complete.
      testthat::expect_equal(
        familiar:::feature_info_complete(feature_info), 
        feature_info_set != "generic")
    }
  }

  # Test contents.
  testthat::expect_equal(is.null(experiment_feature_info@vimp_table_list), TRUE)
  testthat::expect_equal(
    experiment_data_assignment@experiment_setup,
    experiment_feature_info@experiment_setup,
    ignore_attr = TRUE)
  testthat::expect_equal(
    experiment_data_assignment@iteration_list,
    experiment_feature_info@iteration_list,
    ignore_attr = TRUE)
  testthat::expect_equal(
    experiment_data_assignment@project_id,
    experiment_feature_info@project_id)
})

# Create variable importance
experiment_vimp <- familiar::precompute_vimp(
  data = data,
  experiment_data = experiment_feature_info,
  fs_method = "univariate_regression",
  outcome_type = "binomial",
  outcome_column = "cell_malignancy",
  sample_id_column = "id",
  class_levels = c("benign", "malignant"),
  verbose = FALSE)

testthat::test_that("Assert that variable importance computation performs correctly", {
  # Assert that variable importance data are correct.
  testthat::expect_equal(
    length(experiment_vimp@vimp_table_list$univariate_regression),
    3L)
  for (vimp_table in experiment_vimp@vimp_table_list$univariate_regression) {
    testthat::expect_s4_class(vimp_table, "vimpTable")
  }

  # Test contents.
  testthat::expect_equal(
    experiment_data_assignment@experiment_setup,
    experiment_vimp@experiment_setup,
    ignore_attr = TRUE)
  testthat::expect_equal(
    experiment_data_assignment@iteration_list,
    experiment_vimp@iteration_list,
    ignore_attr = TRUE)
  testthat::expect_equal(
    experiment_feature_info@feature_info,
    experiment_vimp@feature_info,
    ignore_attr = TRUE)
  testthat::expect_equal(
    experiment_data_assignment@project_id,
    experiment_vimp@project_id)

  # Test that get_vimp_table produces three tables.
  vimp_table <- familiar::get_vimp_table(experiment_vimp)
  testthat::expect_equal(length(vimp_table$univariate_regression), 3L)

  # Test that aggregate_vimp_table produces one table.
  vimp_table <- familiar::aggregate_vimp_table(
    experiment_vimp, 
    aggregation_method = "borda")
  testthat::expect_s4_class(
    vimp_table$univariate_regression, "vimpTable")
  vimp_table <- familiar::get_vimp_table(vimp_table)
  testthat::expect_s3_class(
    vimp_table$univariate_regression, "data.table")
})

# Add additional variable importance methods.
experiment_vimp_3 <- familiar::precompute_vimp(
  data = data,
  experiment_data = experiment_vimp,
  fs_method = c("univariate_regression", "mim", "mrmr"),
  outcome_type = "binomial",
  outcome_column = "cell_malignancy",
  sample_id_column = "id",
  class_levels = c("benign", "malignant"),
  verbose = FALSE)


testthat::test_that("Addition variable importance computation performs correctly", {
  # Assert that the vimp tables for the univariate_regression method are not
  # altered.
  for (ii in seq_along(experiment_vimp_3@vimp_table_list$univariate_regression)) {
    testthat::expect_equal(
      experiment_vimp_3@vimp_table_list$univariate_regression[[ii]],
      experiment_vimp@vimp_table_list$univariate_regression[[ii]],
      ignore_attr = TRUE)
  }

  # Test that 9 variable importance tables were created.
  testthat::expect_equal(
    length(unlist(familiar::get_vimp_table(experiment_vimp_3), recursive = FALSE)),
    9L)

  # Test that 3 aggregated variable importance tables are created.
  testthat::expect_equal(
    length(familiar::get_vimp_table(
      familiar::aggregate_vimp_table(experiment_vimp_3, "borda"))),
    3L)

  testthat::expect_equal(
    experiment_vimp@project_id,
    experiment_vimp_3@project_id)
})

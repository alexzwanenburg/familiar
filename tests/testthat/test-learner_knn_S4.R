# First test if all selectable learners are also available
familiar:::test_all_learners_available(
  learners = familiar:::.get_available_knn_learners(show_general = TRUE))

# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

familiar:::test_all_learners_train_predict_vimp(
  learners = familiar:::.get_available_knn_learners(
    show_general = FALSE, show_default = TRUE),
  hyperparameter_list = list(
    "continuous" = list("k" = 3),
    "binomial" = list("k" = 3),
    "multinomial" = list("k" = 3)),
  has_vimp = FALSE)

familiar:::test_all_learners_parallel_train_predict_vimp(
  learners = familiar:::.get_available_knn_learners(
    show_general = FALSE, show_default = TRUE),
  hyperparameter_list = list(
    "continuous" = list("k" = 3),
    "binomial" = list("k" = 3),
    "multinomial" = list("k" = 3)),
  has_vimp = FALSE)

# Continuous outcome tests------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test_create_good_data("continuous")

# Train the model using the good dataset.
good_model <- familiar:::test_train(
  data = good_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list(
    "sign_size" = familiar:::get_n_features(good_data),
    "k" = 3),
  learner = "k_nearest_neighbours_gower")


testthat::test_that("k-nearest neighbour model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("k-nearest neighbour model has no variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table is empty.
  testthat::expect_equal(familiar:::is_empty(vimp_table), TRUE)
})


# Binomial tests----------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test_create_good_data("binomial")

# Train the model using the good dataset.
good_model <- familiar:::test_train(
  data = good_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list(
    "sign_size" = familiar:::get_n_features(good_data),
    "k" = 3),
  learner = "k_nearest_neighbours_gower")

testthat::test_that("k-nearest neighbour model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("k-nearest neighbour model does not have variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table has no rows.
  testthat::expect_equal(familiar:::is_empty(vimp_table), TRUE)
})


# Multinomial tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test_create_good_data("multinomial")

# Train the model using the good dataset.
good_model <- suppressWarnings(familiar:::test_train(
  data = good_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list(
    "sign_size" = familiar:::get_n_features(good_data),
    "k" = 3),
  learner = "k_nearest_neighbours_gower"))

testthat::test_that("k-nearest neighbour model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("k-nearest neighbour model has no variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table is empty.
  testthat::expect_equal(familiar:::is_empty(vimp_table), TRUE)
})


testthat::skip("Skip hyperparameter optimisation, unless manual.")

familiar:::test_hyperparameter_optimisation(
  learners = familiar:::.get_available_knn_learners(
    show_general = TRUE, show_default = TRUE),
  debug = FALSE,
  parallel = FALSE)

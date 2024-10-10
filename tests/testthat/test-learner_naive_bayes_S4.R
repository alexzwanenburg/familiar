# First test if all selectable learners are also available
familiar:::test_all_learners_available(
  learners = familiar:::.get_available_naive_bayes_learners(show_general = TRUE)
)

# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()
testthat::skip_on_ci()

familiar:::test_all_learners_train_predict_vimp(
  learners = familiar:::.get_available_naive_bayes_learners(show_general = FALSE),
  hyperparameter_list = list(
    "binomial" = list("laplace" = 0.0),
    "multinomial" = list("laplace" = 0.0)
  ),
  has_vimp = FALSE
)

familiar:::test_all_learners_parallel_train_predict_vimp(
  learners = familiar:::.get_available_naive_bayes_learners(show_general = FALSE),
  hyperparameter_list = list(
    "binomial" = list("laplace" = 0.0),
    "multinomial" = list("laplace" = 0.0)
  ),
  has_vimp = FALSE
)


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
    "laplace" = 0.0
  ),
  learner = "naive_bayes"
)

testthat::test_that("Naive Bayes model trained correctly", {
  # Model trained
  testthat::expect_true(familiar:::model_is_trained(good_model))

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Naive Bayes model has no variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table is empty.
  testthat::expect_true(familiar:::is_empty(vimp_table))
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
    "laplace" = 0.0
  ),
  learner = "naive_bayes"
))

testthat::test_that("Naive Bayes model trained correctly", {
  # Model trained
  testthat::expect_true(familiar:::model_is_trained(good_model))

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Naive Bayes model has no variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table is empty.
  testthat::expect_true(familiar:::is_empty(vimp_table))
})


familiar:::test_hyperparameter_optimisation(
  learners = "naive_bayes",
  debug = FALSE,
  parallel = FALSE,
  test_specific_config = TRUE
)


testthat::skip("Skip hyperparameter optimisation, unless manual.")

familiar:::test_hyperparameter_optimisation(
  learners = familiar:::.get_available_naive_bayes_learners(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE
)

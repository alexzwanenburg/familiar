# First test if all selectable learners are also available
familiar:::test_all_learners_available(
  learners = familiar:::.get_available_svm_c_learners(show_general = TRUE))
familiar:::test_all_learners_available(
  learners = familiar:::.get_available_svm_nu_learners(show_general = TRUE))
familiar:::test_all_learners_available(
  learners = familiar:::.get_available_svm_eps_learners(show_general = TRUE))

# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()

familiar:::test_all_learners_train_predict_vimp(
  learners = familiar:::.get_available_svm_c_learners(show_general = FALSE),
  hyperparameter_list = list(
    "binomial" = list(
      "c" = -1.0,
      "gamma" = 0.1,
      "degree" = 2.0,
      "offset" = 0.0
    ),
    "multinomial" = list(
      "c" = -1.0,
      "gamma" = 0.1,
      "degree" = 2.0,
      "offset" = 0.0
    )),
  has_vimp = FALSE
)


familiar:::test_all_learners_train_predict_vimp(
  learners = familiar:::.get_available_svm_nu_learners(show_general = FALSE),
  hyperparameter_list = list(
    "continuous" = list(
      "c" = -1.0,
      "epsilon" = 0.0,
      "nu" = -4.0,
      "gamma" = 0.1,
      "degree" = 2.0,
      "offset" = 0.0
    ),
    "binomial" = list(
      "c" = -1.0,
      "epsilon" = 0.0,
      "nu" = -4.0,
      "gamma" = 0.1,
      "degree" = 2.0,
      "offset" = 0.0
    ),
    "multinomial" = list(
      "c" = -1.0,
      "epsilon" = 0.0,
      "nu" = -4.0,
      "gamma" = 0.1,
      "degree" = 2.0,
      "offset" = 0.0
    )),
  has_vimp = FALSE
)


familiar:::test_all_learners_train_predict_vimp(
  learners = familiar:::.get_available_svm_eps_learners(show_general = FALSE),
  hyperparameter_list = list(
    "continuous" = list(
      "c" = -1.0,
      "epsilon" = 0.0,
      "gamma" = 0.1,
      "degree" = 2.0,
      "offset" = 0.0
    )),
  has_vimp = FALSE
)

# Parallel tests.
familiar:::test_all_learners_parallel_train_predict_vimp(
  learners = familiar:::.get_available_svm_c_learners(show_general = FALSE),
  hyperparameter_list = list(
    "binomial" = list(
      "c" = -1.0,
      "gamma" = 0.1,
      "degree" = 2.0,
      "offset" = 0.0
    ),
    "multinomial" = list(
      "c" = -1.0,
      "gamma" = 0.1,
      "degree" = 2.0,
      "offset" = 0.0
    )),
  has_vimp = FALSE
)


familiar:::test_all_learners_parallel_train_predict_vimp(
  learners = familiar:::.get_available_svm_nu_learners(show_general = FALSE),
  hyperparameter_list = list(
    "continuous" = list(
      "c" = -1.0,
      "epsilon" = 0.0,
      "nu" = -4.0,
      "gamma" = 0.1,
      "degree" = 2.0,
      "offset" = 0.0
    ),
    "binomial" = list(
      "c" = -1.0,
      "epsilon" = 0.0,
      "nu" = -4.0,
      "gamma" = 0.1,
      "degree" = 2.0,
      "offset" = 0.0
    ),
    "multinomial" = list(
      "c" = -1.0,
      "epsilon" = 0.0,
      "nu" = -4.0,
      "gamma" = 0.1,
      "degree" = 2.0,
      "offset" = 0.0
    )),
  has_vimp = FALSE
)


familiar:::test_all_learners_parallel_train_predict_vimp(
  learners = familiar:::.get_available_svm_eps_learners(show_general = FALSE),
  hyperparameter_list = list(
    "continuous" = list(
      "c" = -1.0,
      "epsilon" = 0.0,
      "gamma" = 0.1,
      "degree" = 2.0,
      "offset" = 0.0
    )),
  has_vimp = FALSE
)


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
    "c" = -1.0,
    "epsilon" = 0.0,
    "gamma" = 1.0),
  learner = "svm_eps_radial")

testthat::test_that("SVM model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("SVM model has no variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table is empty.
  testthat::expect_equal(is_empty(vimp_table), TRUE)
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
    "c" = -1.0,
    "gamma" = 1.0),
  learner = "svm_c_radial")

testthat::test_that("SVM model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("SVM model has no variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table is empty.
  testthat::expect_equal(is_empty(vimp_table), TRUE)
})


# Multinomial tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test_create_good_data("multinomial")

# Train the model using the good dataset.
good_model <- familiar:::test_train(
  data = good_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list(
    "sign_size" = familiar:::get_n_features(good_data),
    "c" = -1.0,
    "gamma" = 1.0),
  learner = "svm_c_radial")

testthat::test_that("SVM model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("SVM model has no variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table is empty.
  testthat::expect_equal(is_empty(vimp_table), TRUE)
})


testthat::skip("Skip hyperparameter optimisation, unless manual.")

familiar:::test_hyperparameter_optimisation(
  learners = familiar:::.get_available_svm_c_learners(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE
)

familiar:::test_hyperparameter_optimisation(
  learners = familiar:::.get_available_svm_nu_learners(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE
)

familiar:::test_hyperparameter_optimisation(
  learners = familiar:::.get_available_svm_eps_learners(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE
)

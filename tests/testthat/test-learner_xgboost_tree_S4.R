# First test if all selectable learners are also available
familiar:::test_all_learners_available(
  learners = familiar:::.get_available_xgboost_tree_learners(show_general = TRUE))

# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()

familiar:::test_all_learners_train_predict_vimp(
  learners = familiar:::.get_available_xgboost_tree_learners(show_general = FALSE),
  hyperparameter_list = list(
    "continuous" = list(
      "n_boost" = 2,
      "learning_rate" = -1,
      "lambda" = 0.0,
      "alpha" = -6.0,
      "min_child_weight" = 1.04,
      "tree_depth" = 3,
      "sample_size" = 1.0,
      "gamma" = -6.0
    ),
    "binomial" = list(
      "n_boost" = 2,
      "learning_rate" = -1,
      "lambda" = 0.0,
      "alpha" = -6.0,
      "min_child_weight" = 1.04,
      "tree_depth" = 3,
      "sample_size" = 1.0,
      "gamma" = -6.0
    ),
    "multinomial" = list(
      "n_boost" = 2,
      "learning_rate" = -1,
      "lambda" = 0.0,
      "alpha" = -6.0,
      "min_child_weight" = 1.04,
      "tree_depth" = 3,
      "sample_size" = 1.0,
      "gamma" = -6.0
    ),
    "survival" = list(
      "n_boost" = 2,
      "learning_rate" = -1,
      "lambda" = 0.0,
      "alpha" = -6.0,
      "min_child_weight" = 1.04,
      "tree_depth" = 3,
      "sample_size" = 1.0,
      "gamma" = -6.0
    )
  )
)

familiar:::test_all_learners_parallel_train_predict_vimp(
  learners = familiar:::.get_available_xgboost_tree_learners(show_general = FALSE),
  hyperparameter_list = list(
    "continuous" = list(
      "n_boost" = 2,
      "learning_rate" = -1,
      "lambda" = 0.0,
      "alpha" = -6.0,
      "min_child_weight" = 1.04,
      "tree_depth" = 3,
      "sample_size" = 1.0,
      "gamma" = -6.0
    ),
    "binomial" = list(
      "n_boost" = 2,
      "learning_rate" = -1,
      "lambda" = 0.0,
      "alpha" = -6.0,
      "min_child_weight" = 1.04,
      "tree_depth" = 3,
      "sample_size" = 1.0,
      "gamma" = -6.0
    ),
    "multinomial" = list(
      "n_boost" = 2,
      "learning_rate" = -1,
      "lambda" = 0.0,
      "alpha" = -6.0,
      "min_child_weight" = 1.04,
      "tree_depth" = 3,
      "sample_size" = 1.0,
      "gamma" = -6.0
    ),
    "survival" = list(
      "n_boost" = 2,
      "learning_rate" = -1,
      "lambda" = 0.0,
      "alpha" = -6.0,
      "min_child_weight" = 1.04,
      "tree_depth" = 3,
      "sample_size" = 1.0,
      "gamma" = -6.0
    )
  )
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
    "n_boost" = 2,
    "learning_rate" = -1,
    "lambda" = 0.0,
    "alpha" = -6.0,
    "min_child_weight" = 1.04,
    "tree_depth" = 3,
    "sample_size" = 1.0,
    "gamma" = -6.0
  ),
  learner = "xgboost_tree_gaussian"
)

testthat::test_that("Extreme gradient boosting tree model trained correctly", {
  # Model trained
  testthat::expect_true(familiar:::model_is_trained(good_model))

  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Extreme gradient boosting tree model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)
  
  # Expect that the vimp table has six rows.
  testthat::expect_equal(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(familiar:::get_feature_columns(good_data) %in% vimp_table$name)
  )
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
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
    "n_boost" = 2,
    "learning_rate" = -1,
    "lambda" = 0.0,
    "alpha" = -6.0,
    "min_child_weight" = 1.04,
    "tree_depth" = 3,
    "sample_size" = 1.0,
    "gamma" = -6.0
  ),
  learner = "xgboost_tree_logistic"
)

testthat::test_that("Extreme gradient boosting tree model trained correctly", {
  # Model trained
  testthat::expect_true(familiar:::model_is_trained(good_model))

  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Extreme gradient boosting tree model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)
  
  # Expect that the vimp table has up to six rows.
  testthat::expect_lte(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(vimp_table$name %in% familiar:::get_feature_columns(good_data))
  )
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
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
    "n_boost" = 2,
    "learning_rate" = -1,
    "lambda" = 0.0,
    "alpha" = -6.0,
    "min_child_weight" = 1.04,
    "tree_depth" = 3,
    "sample_size" = 1.0,
    "gamma" = -6.0
  ),
  learner = "xgboost_tree_logistic"
)

testthat::test_that("Extreme gradient boosting tree model trained correctly", {
  # Model trained
  testthat::expect_true(familiar:::model_is_trained(good_model))

  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Extreme gradient boosting tree model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)
  
  # Expect that the vimp table has six rows.
  testthat::expect_equal(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(familiar:::get_feature_columns(good_data) %in% vimp_table$name)
  )
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})


# Survival tests----------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test_create_good_data("survival")

# Train the model using the good dataset.
good_model <- familiar:::test_train(
  data = good_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list(
    "sign_size" = familiar:::get_n_features(good_data),
    "n_boost" = 2,
    "learning_rate" = -1,
    "lambda" = 0.0,
    "alpha" = -6.0,
    "min_child_weight" = 1.04,
    "tree_depth" = 3,
    "sample_size" = 1.0,
    "gamma" = -6.0
  ),
  time_max = 3.5,
  learner = "xgboost_tree_cox"
)

testthat::test_that("Extreme gradient boosting tree model trained correctly", {
  # Model trained
  testthat::expect_true(familiar:::model_is_trained(good_model))

  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Extreme gradient boosting tree model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)
  
  # Expect that the vimp table has six rows.
  testthat::expect_equal(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(familiar:::get_feature_columns(good_data) %in% vimp_table$name)
  )
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})


testthat::skip("Skip hyperparameter optimisation, unless manual.")

familiar:::test_hyperparameter_optimisation(
  learners = familiar:::.get_available_xgboost_tree_learners(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE
)

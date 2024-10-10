# First test if all selectable learners are also available
familiar:::test_all_learners_available(
  learners = familiar:::.get_available_ranger_learners(show_general = TRUE)
)

# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

familiar:::test_all_learners_train_predict_vimp(
  learners = familiar:::.get_available_ranger_learners(show_general = FALSE),
  hyperparameter_list = list(
    "continuous" = list(
      "n_tree" = 4,
      "sample_size" = 1.00,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "alpha" = 0.1
    ),
    "binomial" = list(
      "n_tree" = 4,
      "sample_size" = 1.00,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "alpha" = 0.1
    ),
    "multinomial" = list(
      "n_tree" = 4,
      "sample_size" = 1.00,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "alpha" = 0.1
    ),
    "survival" = list(
      "n_tree" = 4,
      "sample_size" = 1.00,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "alpha" = 0.1
    )
  )
)

familiar:::test_all_learners_parallel_train_predict_vimp(
  learners = familiar:::.get_available_ranger_learners(show_general = FALSE),
  hyperparameter_list = list(
    "continuous" = list(
      "n_tree" = 4,
      "sample_size" = 1.00,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "alpha" = 0.1
    ),
    "binomial" = list(
      "n_tree" = 4,
      "sample_size" = 1.00,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "alpha" = 0.1
    ),
    "multinomial" = list(
      "n_tree" = 4,
      "sample_size" = 1.00,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "alpha" = 0.1
    ),
    "survival" = list(
      "n_tree" = 4,
      "sample_size" = 1.00,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "alpha" = 0.1
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
    "n_tree" = 8,
    "sample_size" = 1.00,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5,
    "alpha" = 0.1
  ),
  learner = "random_forest_ranger"
)

testthat::test_that("Ranger random forest model trained correctly", {
  # Model trained
  testthat::expect_true(familiar:::model_is_trained(good_model))

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Ranger random forest model has variable importance", {
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
    "n_tree" = 8,
    "sample_size" = 1.00,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5,
    "alpha" = 0.1
  ),
  learner = "random_forest_ranger"
)

testthat::test_that("Ranger random forest model trained correctly", {
  # Model trained
  testthat::expect_true(familiar:::model_is_trained(good_model))

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Ranger random forest model has variable importance", {
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
    "n_tree" = 8,
    "sample_size" = 1.00,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5,
    "alpha" = 0.1
  ),
  learner = "random_forest_ranger"
)

testthat::test_that("Ranger random forest model trained correctly", {
  # Model trained
  testthat::expect_true(familiar:::model_is_trained(good_model))

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Ranger random forest model has variable importance", {
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
    "n_tree" = 8,
    "sample_size" = 1.00,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5,
    "alpha" = 0.1
  ),
  learner = "random_forest_ranger"
)

testthat::test_that("Ranger random forest model trained correctly", {
  # Model trained
  testthat::expect_true(familiar:::model_is_trained(good_model))

  # Calibration info is present
  testthat::expect_true(familiar:::has_calibration_info(good_model))

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Ranger random forest model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)
  
  # Expect that the vimp table has six rows.
  testthat::expect_equal(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(familiar:::get_feature_columns(good_data) %in% vimp_table$name))
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})


familiar:::test_hyperparameter_optimisation(
  learners = "random_forest_ranger",
  debug = FALSE,
  parallel = FALSE,
  test_specific_config = TRUE
)


testthat::skip("Skip hyperparameter optimisation, unless manual.")

familiar:::test_hyperparameter_optimisation(
  learners = familiar:::.get_available_ranger_learners(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE
)

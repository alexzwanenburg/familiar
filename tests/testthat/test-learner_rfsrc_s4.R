# First test if all selectable learners are also available
familiar:::test_all_learners_available(
  learners = familiar:::.get_available_rfsrc_learners(show_general = TRUE))

# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

familiar:::test_all_learners_train_predict_vimp(
  learners = familiar:::.get_available_rfsrc_learners(show_general = FALSE),
  hyperparameter_list = list(
    "count" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5
    ),
    "continuous" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5
    ),
    "binomial" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5
    ),
    "multinomial" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5
    ),
    "survival" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5
    )
  )
)

familiar:::test_all_learners_parallel_train_predict_vimp(
  learners = familiar:::.get_available_rfsrc_learners(show_general = FALSE),
  hyperparameter_list = list(
    "count" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5
    ),
    "continuous" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5
    ),
    "binomial" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5
    ),
    "multinomial" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5
    ),
    "survival" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5
    )
  )
)

# Count outcome tests-----------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test_create_good_data("count")
wide_data <- familiar:::test_create_wide_data("count")

# Train the model using the good dataset.
good_model <- familiar:::test_train(
  data = good_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list(
    "sign_size" = familiar:::get_n_features(good_data),
    "n_tree" = 4,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  learner = "random_forest_rfsrc")

# Train the model using wide data.
wide_model <- familiar:::test_train(
  data = wide_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list(
    "sign_size" = familiar:::get_n_features(wide_data),
    "n_tree" = 4,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  learner = "random_forest_rfsrc")

testthat::test_that("Random forest SRC model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Random forest SRC model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model, data = good_data)

  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 13)

  # Expect that the names are the same as that of the features.
  testthat::expect_true(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name))

  # Expect that avg_rooms has rank 1 and per_capita_crime has rank 2. Disabled
  # test because vimp by the random forest is unstable.
})

testthat::test_that("Random forest SRC model can train on wide data", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)

  # Variable importance table is present.
  testthat::expect_false(familiar:::is_empty(
    familiar:::get_vimp_table(wide_model, data = wide_data)))

  # Valid predictions.
  testthat::expect_true(familiar:::any_predictions_valid(
    familiar:::.predict(wide_model, wide_data),
    outcome_type = wide_data@outcome_type))

  # Test that no deprecation warnings are given.
  familiar:::test_not_deprecated(wide_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(wide_model@messages$error, NULL)
})

# Continuous outcome tests------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test_create_good_data("continuous")
wide_data <- familiar:::test_create_wide_data("continuous")

# Train the model using the good dataset.
good_model <- familiar:::test_train(
  data = good_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list(
    "sign_size" = familiar:::get_n_features(good_data),
    "n_tree" = 4,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  learner = "random_forest_rfsrc")

# Train the model using wide data.
wide_model <- familiar:::test_train(
  data = wide_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list(
    "sign_size" = familiar:::get_n_features(wide_data),
    "n_tree" = 4,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  learner = "random_forest_rfsrc")

testthat::test_that("Random forest SRC model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("Random forest SRC model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model, data = good_data)

  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 10)

  # Expect that the names are the same as that of the features.
  testthat::expect_true(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name))

  # Expect that avginc has rank 1 and mealpct has rank 2.
  testthat::expect_true(any(vimp_table[rank <= 2]$name %in% c("enrltot", "avginc", "calwpct")))
})

testthat::test_that("Random forest SRC model can train on wide data", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)

  # Variable importance table is present.
  testthat::expect_false(
    familiar:::is_empty(familiar:::get_vimp_table(wide_model, data = wide_data)))

  # Valid predictions.
  testthat::expect_true(familiar:::any_predictions_valid(
    familiar:::.predict(wide_model, wide_data),
    outcome_type = wide_data@outcome_type))

  # Test that no deprecation warnings are given.
  familiar:::test_not_deprecated(wide_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(wide_model@messages$error, NULL)
})

# Binomial tests----------------------------------------------------------------s

# Create test data sets.
good_data <- familiar:::test_create_good_data("binomial")
wide_data <- familiar:::test_create_wide_data("binomial")

# Train the model using the good dataset.
good_model <- familiar:::test_train(
  data = good_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list(
    "sign_size" = familiar:::get_n_features(good_data),
    "n_tree" = 4,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  learner = "random_forest_rfsrc")

# Train the model using wide data.
wide_model <- familiar:::test_train(
  data = wide_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list(
    "sign_size" = familiar:::get_n_features(wide_data),
    "n_tree" = 4,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  learner = "random_forest_rfsrc")

testthat::test_that("Random forest SRC model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Random forest SRC model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model, data = good_data)

  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 8)

  # Expect that the names are the same as that of the features.
  testthat::expect_true(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name))

  # Disabled test ranking test because vimp by the random forest is unstable.
})

testthat::test_that("Random forest SRC model can train on wide data", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)

  # Variable importance table is present.
  testthat::expect_false(
    familiar:::is_empty(familiar:::get_vimp_table(wide_model, data = wide_data)))

  # Valid predictions.
  testthat::expect_true(familiar:::any_predictions_valid(
    familiar:::.predict(wide_model, wide_data),
    outcome_type = wide_data@outcome_type))

  # Test that no deprecation warnings are given.
  familiar:::test_not_deprecated(wide_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(wide_model@messages$error, NULL)
})

# Multinomial tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test_create_good_data("multinomial")
wide_data <- familiar:::test_create_wide_data("multinomial")

# Train the model using the good dataset.
good_model <- familiar:::test_train(
  data = good_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list(
    "sign_size" = familiar:::get_n_features(good_data),
    "n_tree" = 4,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  learner = "random_forest_rfsrc")

# Train the model using wide data.
wide_model <- familiar:::test_train(
  data = wide_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list(
    "sign_size" = familiar:::get_n_features(wide_data),
    "n_tree" = 4,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  learner = "random_forest_rfsrc")

testthat::test_that("Random forest SRC model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Random forest SRC model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model, data = good_data)

  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 4)

  # Expect that the names are the same as that of the features.
  testthat::expect_true(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name))

  # Expect that Petal length has rank 1 and petal width has rank 2. Disabled
  # test because vimp by the random forest is unstable.
})

testthat::test_that("Random forest SRC model can train on wide data", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)

  # Variable importance table is present.
  testthat::expect_false(
    familiar:::is_empty(familiar:::get_vimp_table(wide_model, data = wide_data)))

  # Valid predictions.
  testthat::expect_true(familiar:::any_predictions_valid(
    familiar:::.predict(wide_model, wide_data),
    outcome_type = wide_data@outcome_type))

  # Test that no deprecation warnings are given.
  familiar:::test_not_deprecated(wide_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(wide_model@messages$error, NULL)
})

# Survival tests----------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test_create_good_data("survival")
wide_data <- familiar:::test_create_wide_data("survival")

# Train the model using the good dataset.
good_model <- familiar:::test_train(
  data = good_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list(
    "sign_size" = familiar:::get_n_features(good_data),
    "n_tree" = 4,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  learner = "random_forest_rfsrc")

# Train the model using wide data.
wide_model <- familiar:::test_train(
  data = wide_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list(
    "sign_size" = familiar:::get_n_features(wide_data),
    "n_tree" = 4,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  learner = "random_forest_rfsrc")

testthat::test_that("Random forest SRC model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # Calibration info is present
  testthat::expect_equal(familiar:::has_calibration_info(good_model), TRUE)

  # Test that the model predicts cumulative hazard
  testthat::expect_equal(
    familiar:::get_prediction_type(good_model),
    "cumulative_hazard")

  # Test that the model predicts hazard ratios
  testthat::expect_equal(
    familiar:::get_prediction_type(good_model, type = "survival_probability"),
    "survival_probability")

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Random forest SRC model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model, data = good_data)

  # Expect that the vimp table has three rows.
  testthat::expect_equal(nrow(vimp_table), 3)

  # Expect that the names are the same as that of the features.
  testthat::expect_true(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name))

  # Expect that nodes and rx are the best features. The ordering is
  # non-deterministic.
  testthat::expect_equal(vimp_table[rank == 1, ]$name %in% c("nodes", "rx"), TRUE)
  testthat::expect_equal(vimp_table[rank == 2, ]$name %in% c("rx", "adhere"), TRUE)
})


testthat::test_that("Random forest SRC model can train on wide data", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)

  # Variable importance table is present
  testthat::expect_false(
    familiar:::is_empty(familiar:::get_vimp_table(wide_model, data = wide_data)))

  # Valid predictions are possible.
  testthat::expect_true(familiar:::any_predictions_valid(
    familiar:::.predict(wide_model, wide_data),
    outcome_type = wide_data@outcome_type))

  # Valid survival probability predictions can be made.
  testthat::expect_true(familiar:::any_predictions_valid(
    familiar:::.predict(wide_model, wide_data, type = "survival_probability", time = 1000),
    outcome_type = wide_data@outcome_type))

  # Test that no deprecation warnings are given.
  familiar:::test_not_deprecated(wide_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(wide_model@messages$error, NULL)
})

testthat::skip("Skip hyperparameter optimisation, unless manual.")

familiar:::test_hyperparameter_optimisation(
  learners = familiar:::.get_available_rfsrc_learners(show_general = TRUE),
  debug = FALSE,
  parallel = TRUE
)

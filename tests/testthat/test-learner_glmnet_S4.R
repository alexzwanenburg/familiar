# First test if all selectable learners are also available
familiar:::test_all_learners_available(
  learners = familiar:::.get_available_glmnet_ridge_learners(show_general = TRUE))
familiar:::test_all_learners_available(
  learners = familiar:::.get_available_glmnet_lasso_learners(show_general = TRUE))
familiar:::test_all_learners_available(
  learners = familiar:::.get_available_glmnet_elastic_net_learners(show_general = TRUE))

# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

familiar:::test_all_learners_train_predict_vimp(
  learners = familiar:::.get_available_glmnet_ridge_learners(show_general = FALSE))
familiar:::test_all_learners_train_predict_vimp(
  learners = familiar:::.get_available_glmnet_lasso_learners(show_general = FALSE))
familiar:::test_all_learners_train_predict_vimp(
  learners = familiar:::.get_available_glmnet_elastic_net_learners(show_general = FALSE),
  hyperparameter_list = list(
    "count" = list("alpha" = 0.50),
    "continuous" = list("alpha" = 0.50),
    "binomial" = list("alpha" = 0.50),
    "multinomial" = list("alpha" = 0.50),
    "survival" = list("alpha" = 0.50)))

familiar:::test_all_learners_parallel_train_predict_vimp(
  learners = familiar:::.get_available_glmnet_ridge_learners(show_general = FALSE))
familiar:::test_all_learners_parallel_train_predict_vimp(
  learners = familiar:::.get_available_glmnet_lasso_learners(show_general = FALSE))
familiar:::test_all_learners_parallel_train_predict_vimp(
  learners = familiar:::.get_available_glmnet_elastic_net_learners(show_general = FALSE),
  hyperparameter_list = list(
    "count" = list("alpha" = 0.50),
    "continuous" = list("alpha" = 0.50),
    "binomial" = list("alpha" = 0.50),
    "multinomial" = list("alpha" = 0.50),
    "survival" = list("alpha" = 0.50)))

# Count outcome tests-----------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test_create_good_data("count")
wide_data <- familiar:::test_create_wide_data("count")

# Train the model using the good dataset.
good_model <- familiar:::test_train(
  data = good_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(good_data)),
  learner = "lasso")

# Train the model using wide data.
wide_model <- familiar:::test_train(
  data = wide_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(wide_data)),
  learner = "lasso")

testthat::test_that("Regularised regression model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Regularised regression model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table has entries up to the number of features.
  testthat::expect_equal(nrow(vimp_table) <= familiar:::get_n_features(good_data), TRUE)

  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(vimp_table$name %in% familiar:::get_feature_columns(good_data)), TRUE)

  # Expect specific features to be highly ranked.
  testthat::expect_true(
    any(vimp_table[rank <= 2]$name %in% c(
      "avg_rooms", "per_capita_crime", "lower_status_percentage", "industry", "large_residence_proportion")))
})

testthat::test_that("Regularised regression model can train on wide data", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)

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
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(good_data)),
  learner = "lasso_gaussian")

# Train the model using wide data.
wide_model <- familiar:::test_train(
  data = wide_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(wide_data)),
  learner = "lasso_gaussian")

testthat::test_that("Regularised regression model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Regularised regression model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table) <= 10, TRUE)

  # Expect that the names are the same as that of the features.
  testthat::expect_true(all(vimp_table$name %in% familiar:::get_feature_columns(good_data)))

  # Expect that avginc has rank 1 and calwpct has rank 2.
  testthat::expect_true(vimp_table[rank == 1, ]$name %in% c(
    "avginc", "calwpct", "teachers", "enrltot"))
  testthat::expect_true(vimp_table[rank == 2, ]$name %in% c(
    "avginc", "calwpct", "teachers", "enrltot"))
})

testthat::test_that("Regularised regression model can train on wide data", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)

  # Valid predictions.
  testthat::expect_true(familiar:::any_predictions_valid(
    familiar:::.predict(wide_model, wide_data),
    outcome_type = wide_data@outcome_type))

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(wide_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(wide_model@messages$error, NULL)
})

# Binomial tests----------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test_create_good_data("binomial")
wide_data <- familiar:::test_create_wide_data("binomial")

# Train the model using the good dataset.
good_model <- familiar:::test_train(
  data = good_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(good_data)),
  learner = "lasso_binomial")

# Train the model using wide data.
wide_model <- suppressWarnings(familiar:::test_train(
  data = wide_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(wide_data)),
  learner = "lasso_binomial"))

testthat::test_that("Regularised regression model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("Regularised regression model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table has two rows.
  testthat::expect_lte(nrow(vimp_table), 8)

  # Expect that the names are the same as that of the features.
  testthat::expect_true(any(familiar:::get_feature_columns(good_data) %in% vimp_table$name))

  testthat::expect_true(all(vimp_table[rank <= 2, ]$name %in% c("cell_shape_uniformity", "mitoses", "bare_nuclei")))
})

testthat::test_that("Regularised regression model can train on wide data", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)

  # Variable importance table is present.
  testthat::expect_equal(familiar:::is_empty(familiar:::get_vimp_table(wide_model)), FALSE)

  # Valid predictions.
  testthat::expect_true(familiar:::any_predictions_valid(
    familiar:::.predict(wide_model, wide_data),
    outcome_type = wide_data@outcome_type))

  # That no deprecation warnings are given.
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
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(good_data)),
  learner = "lasso_multinomial")

# Train the model using wide data.
wide_model <- suppressWarnings(familiar:::test_train(
  data = wide_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(wide_data)),
  learner = "lasso_multinomial"))

testthat::test_that("Regularised regression model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("Regularised regression model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 4)

  # Expect that the names are the same as that of the features.
  testthat::expect_true(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name))

  # Expect that Petal length has rank 1 and petal width has rank 2.
  testthat::expect_true(all(vimp_table[rank <= 2, ]$name %in% c("Petal_Length", "Petal_Width")))
})


# This model may occasionally be able to train.
testthat::test_that("Regularised regression model can not train on wide data", {
  if (familiar:::model_is_trained(wide_model)) {
    # Variable importance table is not empty.
    testthat::expect_false(familiar:::is_empty(familiar:::get_vimp_table(wide_model)))

    # Valid predictions can be made.
    testthat::expect_true(familiar:::any_predictions_valid(
      familiar:::.predict(wide_model, wide_data),
      outcome_type = wide_data@outcome_type))

    # That no deprecation warnings are given.
    familiar:::test_not_deprecated(wide_model@messages$warning)

    # Check that the expected error appears.
    testthat::expect_equal(wide_model@messages$error, NULL)
    
  } else {
    # Variable importance table is empty.
    testthat::expect_true(familiar:::is_empty(familiar:::get_vimp_table(wide_model)))

    # Valid predictions cannot be made.
    testthat::expect_false(familiar:::any_predictions_valid(
      familiar:::.predict(wide_model, wide_data),
      outcome_type = wide_data@outcome_type))

    # That no deprecation warnings are given.
    familiar:::test_not_deprecated(wide_model@messages$warning)

    # Check that the expected error appears.
    testthat::expect_equal(length(wide_model@messages$error), 1L)
    testthat::expect_true(grepl(
      x = wide_model@messages$error,
      pattern = "lognet: one multinomial or binomial class has 1 or 0 observations; not allowed",
      fixed = TRUE))
  }
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
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(good_data)),
  time_max = 1832,
  learner = "lasso_cox")

# Train the model using wide data.
wide_model <- familiar:::test_train(
  data = wide_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(wide_data)),
  time_max = 1832,
  learner = "lasso_cox")

testthat::test_that("Regularised regression model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # Calibration info is present
  testthat::expect_equal(familiar:::has_calibration_info(good_model), TRUE)

  # Test that the model predicts hazard ratios.
  testthat::expect_equal(
    familiar:::get_prediction_type(good_model),
    "hazard_ratio")

  # Test that the model predicts hazard ratios
  testthat::expect_equal(
    familiar:::get_prediction_type(good_model, type = "survival_probability"),
    "survival_probability")

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("Regularised regression model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 2)

  # Expect that the names are the same as that of the features.
  testthat::expect_true(any(familiar:::get_feature_columns(good_data) %in% vimp_table$name))

  # Expect that rx has rank 1 and nodes has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "rx")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "nodes")
})

testthat::test_that("Regularised regression model can train on wide data", {
  # Model was trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)

  # Valid predictions are present.
  testthat::expect_true(familiar:::any_predictions_valid(
    familiar:::.predict(wide_model, wide_data),
    outcome_type = wide_data@outcome_type))

  # Valid survival probability predictions can be made.
  testthat::expect_true(familiar:::any_predictions_valid(
    familiar:::.predict(wide_model, wide_data, type = "survival_probability", time = 1000),
    outcome_type = wide_data@outcome_type))
  
  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(wide_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(wide_model@messages$error, NULL)
})


testthat::skip("Skip hyperparameter optimisation, unless manual.")

familiar:::test_hyperparameter_optimisation(
  learners = familiar:::.get_available_glmnet_ridge_learners(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE)

familiar:::test_hyperparameter_optimisation(
  learners = familiar:::.get_available_glmnet_lasso_learners(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE)

familiar:::test_hyperparameter_optimisation(
  learners = familiar:::.get_available_glmnet_elastic_net_learners(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE)

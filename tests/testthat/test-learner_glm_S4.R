# First test if all selectable learners are also available
familiar:::test_all_learners_available(
  learners = familiar:::.get_available_glm_learners(show_general = TRUE))

# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

familiar:::test_all_learners_train_predict_vimp(
  learners = familiar:::.get_available_glm_learners(show_general = FALSE))

familiar:::test_all_learners_parallel_train_predict_vimp(
  learners = familiar:::.get_available_glm_learners(show_general = FALSE))

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
  learner = "glm_poisson")

# Train the model using wide data.
wide_model <- familiar:::test_train(
  data = wide_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(wide_data)),
  learner = "glm_poisson")

testthat::test_that("Generalised linear model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # Test that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("Generalised linear model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 13)

  # Expect that the names are the same as that of the features.
  testthat::expect_equal(
    all(familiar:::get_feature_columns(good_data) %in% vimp_table$name),
    TRUE)

  testthat::expect_true(
    all(vimp_table[rank <= 2, ]$name %in% c("avg_rooms", "lower_status_percentage", "industry")))
})


testthat::test_that("Generalised linear model can train on wide data", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)

  # Variable importance table is present.
  suppressWarnings(testthat::expect_equal(
    familiar:::is_empty(familiar:::get_vimp_table(wide_model)), FALSE))

  # Valid predictions.
  suppressWarnings(testthat::expect_equal(
    familiar:::any_predictions_valid(
      familiar:::.predict(wide_model, wide_data),
      outcome_type = wide_data@outcome_type),
    TRUE))

  # That no deprecation warnings are given.
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
  learner = "glm_gaussian")

# Train the model using wide data.
wide_model <- familiar:::test_train(
  data = wide_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(wide_data)),
  learner = "glm_gaussian")

testthat::test_that("Generalised linear model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("Generalised linear model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 10)

  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(familiar:::get_feature_columns(good_data) %in% vimp_table$name))

  # Expect that calwpct has rank 1 and elpct has rank 2.
  testthat::expect_true(all(vimp_table[rank <= 2, ]$name %in% c("calwpct", "avginc", "elpct")))
})


testthat::test_that("Generalised linear model can train on wide data", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)

  # Variable importance table is present.
  suppressWarnings(testthat::expect_false(familiar:::is_empty(familiar:::get_vimp_table(wide_model))))

  # Valid predictions.
  suppressWarnings(testthat::expect_true(
    familiar:::any_predictions_valid(
      familiar:::.predict(wide_model, wide_data),
      outcome_type = wide_data@outcome_type)))

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
  learner = "glm_logistic")

# Train the model using wide data.
wide_model <- familiar:::test_train(
  data = wide_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(wide_data)),
  learner = "glm_logistic")

testthat::test_that("Generalised linear model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("Generalised linear model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 8)

  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(familiar:::get_feature_columns(good_data) %in% vimp_table$name))

  # Expect that bare_nuclei has rank 1 and clump_thickness has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "bare_nuclei")
  testthat::expect_equal(vimp_table[rank == 2, ]$name %in% c("clump_thickness", "cell_shape_uniformity"), TRUE)
})


testthat::test_that("Generalised linear model can train on wide data", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)

  # Variable importance table is present.
  suppressWarnings(testthat::expect_false(familiar:::is_empty(familiar:::get_vimp_table(wide_model))))

  # Valid predictions.
  suppressWarnings(testthat::expect_true(
    familiar:::any_predictions_valid(
      familiar:::.predict(wide_model, wide_data),
      outcome_type = wide_data@outcome_type)))

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
good_model <- suppressWarnings(familiar:::test_train(
  data = good_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(good_data)),
  learner = "glm_multinomial"))

# Train the model using wide data.
wide_model <- familiar:::test_train(
  data = wide_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(wide_data)),
  learner = "glm_multinomial")

testthat::test_that("Generalised linear model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning, "deprec")

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})

testthat::test_that("Generalised linear model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 4)

  # Expect that the names are the same as that of the features.
  testthat::expect_true(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name))

  # Expect that Petal length has rank 1 and petal width has rank 2.
  testthat::expect_true(all(vimp_table[rank <= 2, ]$name %in% c("Petal_Length", "Petal_Width")))
})


testthat::test_that("Generalised linear model can train on wide data", {
  # Model can be trained -- even if the neural network does not converge
  # completely, it still functions.
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)

  # Variable importance table is empty.
  testthat::expect_false(familiar:::is_empty(familiar:::get_vimp_table(wide_model)))

  # Valid predictions can be made.
  testthat::expect_true(familiar:::any_predictions_valid(
    familiar:::.predict(wide_model, wide_data),
    outcome_type = wide_data@outcome_type))

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(wide_model@messages$warning)
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
  learner = "glm")

# Train the model using wide data.
wide_model <- suppressWarnings(familiar:::test_train(
  data = wide_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(wide_data)),
  time_max = 1832,
  learner = "glm"))

testthat::test_that("Generalised linear model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # Test that the model predicts hazard ratios.
  testthat::expect_equal(familiar:::get_prediction_type(good_model), "hazard_ratio")

  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("Generalised linear model has variable importance", {
  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)

  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 3)

  # Expect that the names are the same as that of the features.
  testthat::expect_true(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name))

  # Expect that nodes has rank 1 and rx has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "nodes")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "rx")
})


testthat::test_that("Generalised linear model cannot train on wide data", {
  # Model was not trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), FALSE)

  # Variable importance table is empty.
  testthat::expect_true(familiar:::is_empty(familiar:::get_vimp_table(wide_model)))

  # Valid predictions are not possible.
  testthat::expect_false(familiar:::any_predictions_valid(
    familiar:::.predict(wide_model, wide_data),
    outcome_type = wide_data@outcome_type))

  # Valid survival probability predictions can not be made.
  testthat::expect_false(familiar:::any_predictions_valid(
    familiar:::.predict(wide_model, wide_data, type = "survival_probability", time = 1000),
    outcome_type = wide_data@outcome_type))

  # Test that specific warnings and errors appear.
  testthat::expect_equal(length(wide_model@messages$warning), 1L)
  testthat::expect_equal(
    grepl(x = wide_model@messages$warning, pattern = "did not converge", fixed = TRUE),
    TRUE)

  testthat::expect_equal(length(wide_model@messages$error), 1L)
  testthat::expect_equal(
    grepl(x = wide_model@messages$error, pattern = "did not converge", fixed = TRUE),
    TRUE)
})


testthat::skip("Skip hyperparameter optimisation, unless manual.")

# Test hyperparameters
familiar:::test_hyperparameter_optimisation(
  learners = familiar:::.get_available_glm_learners(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE)

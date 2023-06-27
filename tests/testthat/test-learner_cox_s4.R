# First test if all selectable learners are also available
familiar:::test_all_learners_available(
  learners = familiar:::.get_available_cox_learners(show_general = TRUE))

# Don't perform any further tests on CRAN due to time of running the test.
testthat::skip_on_cran()

# Generic test
familiar:::test_all_learners_train_predict_vimp(
  learners = familiar:::.get_available_cox_learners(show_general = TRUE))

familiar:::test_all_learners_parallel_train_predict_vimp(
  learners = familiar:::.get_available_cox_learners(show_general = TRUE))

# Create test data sets.
good_data <- familiar:::test_create_good_data("survival")

# Train the model using the good dataset.
good_model <- familiar:::test_train(
  data = good_data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(good_data)),
  learner = "cox")


testthat::test_that("Cox model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)

  # Test the prediction type
  testthat::expect_equal(
    familiar:::get_prediction_type(good_model),
    "hazard_ratio")

  # Test that the model predicts hazard ratios
  testthat::expect_equal(
    familiar:::get_prediction_type(good_model, type = "survival_probability"),
    "survival_probability")

  # Checkt that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)

  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("Cox model has variable importance", {
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


testthat::skip("Skip hyperparameter optimisation, unless manual.")

# Test hyperparameters
familiar:::test_hyperparameter_optimisation(
  learners = familiar:::.get_available_cox_learners(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE)

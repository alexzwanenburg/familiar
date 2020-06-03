# Create test data sets.
good_data <- familiar:::test.create_good_data_set("survival")
empty_data <- familiar:::test.create_empty_data_set("survival")
one_sample_data <- familiar:::test.create_one_sample_data_set("survival")
wide_data <- familiar:::test.create_wide_data_set("survival")
bad_data <- familiar:::test.create_bad_data_set("survival")

# Train the model using the good dataset.
good_model <- familiar:::train(data=good_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data)),
                               learner="cox")

# Train the model using wide data.
wide_model <- familiar:::train(data=wide_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data)),
                               learner="cox")

# Train the model using the bad data (all censored).
bad_model <- familiar:::train(data=bad_data,
                              cluster_method="none",
                              imputation_method="simple",
                              hyperparameter_list=list("sign_size"=familiar:::get_n_features(bad_data)),
                              time_max=1832,
                              learner="cox")


testthat::test_that("Cox model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
  
  # Calibration info is present
  testthat::expect_equal(familiar:::has_calibration_info(good_model), TRUE)
})


testthat::test_that("Cox model has variable importance", {

  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 2)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name), TRUE)
  
  # Expect that nodes has rank 1 and rx has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "nodes")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "rx")
})


testthat::test_that("Cox model can predict risk", {

  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=good_data)
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, good_data@outcome_type), TRUE)
  
  # Test that the model predicts hazard ratios
  testthat::expect_equal(familiar:::get_prediction_type(good_model), "hazard_ratio")
})


testthat::test_that("Cox model can predict survival probabilities", {

  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=good_data, type="survival_probability", time=1000)
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, good_data@outcome_type), TRUE)
  
  # Test that the model predicts hazard ratios
  testthat::expect_equal(familiar:::get_prediction_type(good_model, type="survival_probability"), "survival_probability")
})


testthat::test_that("Cox model cannot predict if data is empty", {

  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=empty_data)
  
  # The prediction does not contain any valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, empty_data@outcome_type), FALSE)

  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=empty_data, type="survival_probability", time=1000)
  
  # The prediction does not contain any valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, empty_data@outcome_type), FALSE)
})


testthat::test_that("Cox model can predict single samples", {

  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=one_sample_data)
  
  # The prediction is valid.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, one_sample_data@outcome_type), TRUE)
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=one_sample_data, type="survival_probability", time=1000)
  
  # The prediction is valid.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, one_sample_data@outcome_type), TRUE)
})


testthat::test_that("The Cox model does not train for wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), FALSE)
  
  # No variable importance table.
  testthat::expect_equal(nrow(familiar:::..vimp(wide_model)), 0)
  
  # No valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), FALSE)
  
  # No valid survival probability predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data, type="survival_probability", time=1000), outcome_type=wide_data@outcome_type), FALSE)
})


testthat::test_that("The Cox model does not train for bad data with all censoring", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(bad_model), FALSE)
  
  # No variable importance table.
  testthat::expect_equal(nrow(familiar:::..vimp(bad_model)), 0)
  
  # No valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(bad_model, good_data), outcome_type=good_data@outcome_type), FALSE)
  
  # No valid survival probability predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(bad_model, good_data, type="survival_probability", time=1000), outcome_type=good_data@outcome_type), FALSE)
})

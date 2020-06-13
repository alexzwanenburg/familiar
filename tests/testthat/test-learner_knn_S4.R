#####Binomial tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("binomial")
empty_data <- familiar:::test.create_empty_data_set("binomial")
one_sample_data <- familiar:::test.create_one_sample_data_set("binomial")
one_feature_data <- familiar:::test.create_one_feature_data_set("binomial")
wide_data <- familiar:::test.create_wide_data_set("binomial")
bad_data <- familiar:::test.create_bad_data_set("binomial")

# Train the model using the good dataset.
good_model <- familiar:::train(data=good_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                        "k"=3),
                               learner="k_nearest_neighbours")

# Train the one-feature model.
one_feature_model <- familiar:::train(data=one_feature_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=list("sign_size"=familiar:::get_n_features(one_feature_data),
                                                               "k"=3),
                                      learner="k_nearest_neighbours")

# Train the model using wide data.
wide_model <- familiar:::train(data=wide_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data),
                                                        "k"=3),
                               learner="k_nearest_neighbours")

# Train the model using the bad data (all censored).
bad_model <- familiar:::train(data=bad_data,
                              cluster_method="none",
                              imputation_method="simple",
                              hyperparameter_list=list("sign_size"=familiar:::get_n_features(bad_data),
                                                       "k"=3),
                              learner="k_nearest_neighbours")

testthat::test_that("k-nearest neighbour model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("k-nearest neighbour model does not have variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has no rows.
  testthat::expect_equal(familiar:::is_empty(vimp_table), TRUE)
})


testthat::test_that("k-nearest neighbour model can predict class probabilities", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=good_data)
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, good_data@outcome_type), TRUE)
  
  # Expect that the predicted_class column is a factor.
  testthat::expect_s3_class(prediction_table$predicted_class, "factor")
})


testthat::test_that("k-nearest neighbour model cannot predict if data is empty", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=empty_data)
  
  # The prediction does not contain any valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, empty_data@outcome_type), FALSE)
})


testthat::test_that("k-nearest neighbour model can predict single samples", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=one_sample_data)
  
  # The prediction is valid.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, one_sample_data@outcome_type), TRUE)
})


testthat::test_that("k-nearest neighbour model can train on single features", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(one_feature_model), TRUE)
  
  # Variable importance table should not be present
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(one_feature_model)), TRUE)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(one_feature_model, one_feature_data), outcome_type=one_feature_model@outcome_type), TRUE)
})


testthat::test_that("k-nearest neighbour model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is absent.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), TRUE)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
})


testthat::test_that("k-nearest neighbour model does not train for bad data with one class only", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(bad_model), FALSE)
  
  # No variable importance table.
  testthat::expect_equal(nrow(familiar:::..vimp(bad_model)), 0)
  
  # No valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(bad_model, good_data), outcome_type=good_data@outcome_type), FALSE)
})


#####Multinomial tests----------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("multinomial")
empty_data <- familiar:::test.create_empty_data_set("multinomial")
one_sample_data <- familiar:::test.create_one_sample_data_set("multinomial")
one_feature_data <- familiar:::test.create_one_feature_data_set("multinomial")
wide_data <- familiar:::test.create_wide_data_set("multinomial")
bad_data <- familiar:::test.create_bad_data_set("multinomial")

# Train the model using the good dataset.
good_model <- suppressWarnings(familiar:::train(data=good_data,
                                                cluster_method="none",
                                                imputation_method="simple",
                                                hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                                         "k"=3),
                                                learner="k_nearest_neighbours"))

# Train the one-feature model.
one_feature_model <- suppressWarnings(familiar:::train(data=one_feature_data,
                                                       cluster_method="none",
                                                       imputation_method="simple",
                                                       hyperparameter_list=list("sign_size"=familiar:::get_n_features(one_feature_data),
                                                                                "k"=3),
                                                       learner="k_nearest_neighbours"))

# Train the model using wide data.
wide_model <- familiar:::train(data=wide_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data),
                                                        "k"=3),
                               learner="k_nearest_neighbours")

# Train the model using the bad data (all censored).
bad_model <- familiar:::train(data=bad_data,
                              cluster_method="none",
                              imputation_method="simple",
                              hyperparameter_list=list("sign_size"=familiar:::get_n_features(bad_data),
                                                       "k"=3),
                              learner="k_nearest_neighbours")


testthat::test_that("k-nearest neighbour model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("k-nearest neighbour model has no variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table is empty.
  testthat::expect_equal(familiar:::is_empty(vimp_table), TRUE)
})


testthat::test_that("k-nearest neighbour model can predict class probabilities", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=good_data)
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, good_data@outcome_type), TRUE)
  
  # Expect that the predicted_class column is a factor.
  testthat::expect_s3_class(prediction_table$predicted_class, "factor")
})


testthat::test_that("k-nearest neighbour model cannot predict if data is empty", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=empty_data)
  
  # The prediction does not contain any valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, empty_data@outcome_type), FALSE)
})


testthat::test_that("k-nearest neighbour model can predict single samples", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=one_sample_data)
  
  # The prediction is valid.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, one_sample_data@outcome_type), TRUE)
})


testthat::test_that("k-nearest neighbour model can train on single features", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(one_feature_model), TRUE)
  
  # Variable importance table is absent.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(one_feature_model)), TRUE)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(one_feature_model, one_feature_data), outcome_type=one_feature_model@outcome_type), TRUE)
})


testthat::test_that("k-nearest neighbour model can train on wide data", {
  
  # Model cannot be trained.
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is empty.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), TRUE)
  
  # Valid predictions cannot be made.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
})


testthat::test_that("k-nearest neighbour model does not train for bad data with missing classes", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(bad_model), FALSE)
  
  # No variable importance table.
  testthat::expect_equal(nrow(familiar:::..vimp(bad_model)), 0)
  
  # No valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(bad_model, good_data), outcome_type=good_data@outcome_type), FALSE)
})

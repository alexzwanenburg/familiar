#####Count outcome tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("count")
empty_data <- familiar:::test.create_empty_data_set("count")
one_sample_data <- familiar:::test.create_one_sample_data_set("count")
one_feature_data <- familiar:::test.create_one_feature_data_set("count")
wide_data <- familiar:::test.create_wide_data_set("count")
bad_data <- familiar:::test.create_bad_data_set("count")

# Train the model using the good dataset.
good_model <- familiar:::train(data=good_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data)),
                               learner="glm_poisson")

# Train the one-feature model.
one_feature_model <- familiar:::train(data=one_feature_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=list("sign_size"=familiar:::get_n_features(one_feature_data)),
                                      learner="glm_poisson")

# Train the model using wide data.
wide_model <- familiar:::train(data=wide_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data)),
                               learner="glm_poisson")

# Train the model using the bad data (all censored).
bad_model <- familiar:::train(data=bad_data,
                              cluster_method="none",
                              imputation_method="simple",
                              hyperparameter_list=list("sign_size"=familiar:::get_n_features(bad_data)),
                              learner="glm_poisson")

testthat::test_that("Generalised linear model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Generalised linear model has variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 13)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name), TRUE)
  
  # Expect that avg_rooms has rank 1 and residence_before_1940_proportion has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "avg_rooms")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "residence_before_1940_proportion")
})


testthat::test_that("Generalised linear model can predict values", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=good_data)
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, good_data@outcome_type), TRUE)
})


testthat::test_that("Generalised linear model cannot predict if data is empty", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=empty_data)
  
  # The prediction does not contain any valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, empty_data@outcome_type), FALSE)
})


testthat::test_that("Generalised linear model can predict single samples", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=one_sample_data)
  
  # The prediction is valid.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, one_sample_data@outcome_type), TRUE)
})


testthat::test_that("Generalised linear model can train on single features", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(one_feature_model), TRUE)
  
  # Variable importance table is present
  testthat::expect_equal(nrow(familiar:::..vimp(one_feature_model)), 1)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(one_feature_model, one_feature_data), outcome_type=one_feature_model@outcome_type), TRUE)
})


testthat::test_that("Generalised linear model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  suppressWarnings(testthat::expect_equal(is_empty(familiar:::..vimp(wide_model)), FALSE))
  
  # Valid predictions.
  suppressWarnings(testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE))
})


testthat::test_that("Generalised linear model does not train for bad data with one class only", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(bad_model), FALSE)
  
  # No variable importance table.
  testthat::expect_equal(nrow(familiar:::..vimp(bad_model)), 0)
  
  # No valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(bad_model, good_data), outcome_type=good_data@outcome_type), FALSE)
})


#####Continuous outcome tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("continuous")
empty_data <- familiar:::test.create_empty_data_set("continuous")
one_sample_data <- familiar:::test.create_one_sample_data_set("continuous")
one_feature_data <- familiar:::test.create_one_feature_data_set("continuous")
wide_data <- familiar:::test.create_wide_data_set("continuous")
bad_data <- familiar:::test.create_bad_data_set("continuous")

# Train the model using the good dataset.
good_model <- familiar:::train(data=good_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data)),
                               learner="glm_gaussian")

# Train the one-feature model.
one_feature_model <- familiar:::train(data=one_feature_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=list("sign_size"=familiar:::get_n_features(one_feature_data)),
                                      learner="glm_gaussian")

# Train the model using wide data.
wide_model <- familiar:::train(data=wide_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data)),
                               learner="glm_gaussian")

# Train the model using the bad data (all censored).
bad_model <- familiar:::train(data=bad_data,
                              cluster_method="none",
                              imputation_method="simple",
                              hyperparameter_list=list("sign_size"=familiar:::get_n_features(bad_data)),
                              learner="glm_gaussian")

testthat::test_that("Generalised linear model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Generalised linear model has variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 10)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name), TRUE)
  
  # Expect that calwpct has rank 1 and elpct has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "calwpct")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "elpct")
})


testthat::test_that("Generalised linear model can predict values", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=good_data)
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, good_data@outcome_type), TRUE)
})


testthat::test_that("Generalised linear model cannot predict if data is empty", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=empty_data)
  
  # The prediction does not contain any valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, empty_data@outcome_type), FALSE)
})


testthat::test_that("Generalised linear model can predict single samples", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=one_sample_data)
  
  # The prediction is valid.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, one_sample_data@outcome_type), TRUE)
})


testthat::test_that("Generalised linear model can train on single features", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(one_feature_model), TRUE)
  
  # Variable importance table is present
  testthat::expect_equal(nrow(familiar:::..vimp(one_feature_model)), 1)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(one_feature_model, one_feature_data), outcome_type=one_feature_model@outcome_type), TRUE)
})


testthat::test_that("Generalised linear model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  suppressWarnings(testthat::expect_equal(is_empty(familiar:::..vimp(wide_model)), FALSE))
  
  # Valid predictions.
  suppressWarnings(testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE))
})


testthat::test_that("Generalised linear model does not train for bad data with one class only", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(bad_model), FALSE)
  
  # No variable importance table.
  testthat::expect_equal(nrow(familiar:::..vimp(bad_model)), 0)
  
  # No valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(bad_model, good_data), outcome_type=good_data@outcome_type), FALSE)
})


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
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data)),
                               learner="glm_logistic")

# Train the one-feature model.
one_feature_model <- familiar:::train(data=one_feature_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=list("sign_size"=familiar:::get_n_features(one_feature_data)),
                                      learner="glm_logistic")

# Train the model using wide data.
wide_model <- familiar:::train(data=wide_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data)),
                               learner="glm_logistic")

# Train the model using the bad data (all censored).
bad_model <- familiar:::train(data=bad_data,
                              cluster_method="none",
                              imputation_method="simple",
                              hyperparameter_list=list("sign_size"=familiar:::get_n_features(bad_data)),
                              learner="glm_logistic")

testthat::test_that("Generalised linear model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Generalised linear model has variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 8)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name), TRUE)
  
  # Expect that bare_nuclei has rank 1 and clump_thickness has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "bare_nuclei")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "clump_thickness")
})


testthat::test_that("Generalised linear model can predict class probabilities", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=good_data)
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, good_data@outcome_type), TRUE)
  
  # Expect that the predicted_class column is a factor.
  testthat::expect_s3_class(prediction_table$predicted_class, "factor")
})


testthat::test_that("Generalised linear model cannot predict if data is empty", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=empty_data)
  
  # The prediction does not contain any valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, empty_data@outcome_type), FALSE)
})


testthat::test_that("Generalised linear model can predict single samples", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=one_sample_data)
  
  # The prediction is valid.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, one_sample_data@outcome_type), TRUE)
})


testthat::test_that("Generalised linear model can train on single features", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(one_feature_model), TRUE)
  
  # Variable importance table is present
  testthat::expect_equal(nrow(familiar:::..vimp(one_feature_model)), 1)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(one_feature_model, one_feature_data), outcome_type=one_feature_model@outcome_type), TRUE)
})


testthat::test_that("Generalised linear model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  suppressWarnings(testthat::expect_equal(is_empty(familiar:::..vimp(wide_model)), FALSE))
                   
  # Valid predictions.
  suppressWarnings(testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE))
})


testthat::test_that("Generalised linear model does not train for bad data with one class only", {
  
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
                                                hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data)),
                                                learner="glm_multinomial"))

# Train the one-feature model.
one_feature_model <- suppressWarnings(familiar:::train(data=one_feature_data,
                                                       cluster_method="none",
                                                       imputation_method="simple",
                                                       hyperparameter_list=list("sign_size"=familiar:::get_n_features(one_feature_data)),
                                                       learner="glm_multinomial"))

# Train the model using wide data.
wide_model <- familiar:::train(data=wide_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data)),
                               learner="glm_multinomial")

# Train the model using the bad data (all censored).
bad_model <- familiar:::train(data=bad_data,
                              cluster_method="none",
                              imputation_method="simple",
                              hyperparameter_list=list("sign_size"=familiar:::get_n_features(bad_data)),
                              learner="glm_multinomial")


testthat::test_that("Generalised linear model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Generalised linear model has variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 4)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name), TRUE)
  
  # Expect that Petal length has rank 1 and petal width has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "Petal_Length")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "Petal_Width")
})


testthat::test_that("Generalised linear model can predict class probabilities", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=good_data)
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, good_data@outcome_type), TRUE)
  
  # Expect that the predicted_class column is a factor.
  testthat::expect_s3_class(prediction_table$predicted_class, "factor")
})


testthat::test_that("Generalised linear model cannot predict if data is empty", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=empty_data)
  
  # The prediction does not contain any valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, empty_data@outcome_type), FALSE)
})


testthat::test_that("Generalised linear model can predict single samples", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=one_sample_data)
  
  # The prediction is valid.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, one_sample_data@outcome_type), TRUE)
})


testthat::test_that("Generalised linear model can train on single features", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(one_feature_model), TRUE)
  
  # Variable importance table is present
  testthat::expect_equal(nrow(familiar:::..vimp(one_feature_model)), 1)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(one_feature_model, one_feature_data), outcome_type=one_feature_model@outcome_type), TRUE)
})


testthat::test_that("Generalised linear model can not train on wide data", {
  
  # Model cannot be trained.
  testthat::expect_equal(familiar:::model_is_trained(wide_model), FALSE)
  
  # Variable importance table is empty.
  testthat::expect_equal(is_empty(familiar:::..vimp(wide_model)), TRUE)
  
  # Valid predictions cannot be made.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), FALSE)
})


testthat::test_that("Generalised linear model does not train for bad data with missing classes", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(bad_model), FALSE)
  
  # No variable importance table.
  testthat::expect_equal(nrow(familiar:::..vimp(bad_model)), 0)
  
  # No valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(bad_model, good_data), outcome_type=good_data@outcome_type), FALSE)
})


#####Survival tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("survival")
empty_data <- familiar:::test.create_empty_data_set("survival")
one_sample_data <- familiar:::test.create_one_sample_data_set("survival")
one_feature_data <- familiar:::test.create_one_feature_data_set("survival")
wide_data <- familiar:::test.create_wide_data_set("survival")
bad_data <- familiar:::test.create_bad_data_set("survival")

# Train the model using the good dataset.
good_model <- familiar:::train(data=good_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data)),
                               time_max=1832,
                               learner="glm")

# Train the one-feature model.
one_feature_model <- familiar:::train(data=one_feature_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=list("sign_size"=familiar:::get_n_features(one_feature_data)),
                                      time_max=1832,
                                      learner="glm")

# Train the model using wide data.
wide_model <- suppressWarnings(familiar:::train(data=wide_data,
                                                cluster_method="none",
                                                imputation_method="simple",
                                                hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data)),
                                                time_max=1832,
                                                learner="glm"))
                               
# Train the model using the bad data (all censored).
bad_model <- familiar:::train(data=bad_data,
                              cluster_method="none",
                              imputation_method="simple",
                              hyperparameter_list=list("sign_size"=familiar:::get_n_features(bad_data)),
                              time_max=1832,
                              learner="glm")


testthat::test_that("Generalised linear model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
  
  # Calibration info is present
  testthat::expect_equal(familiar:::has_calibration_info(good_model), TRUE)
})


testthat::test_that("Generalised linear model has variable importance", {
  
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


testthat::test_that("Generalised linear model can predict risk", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=good_data)
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, good_data@outcome_type), TRUE)
  
  # Test that the model predicts cumulative hazard
  testthat::expect_equal(familiar:::get_prediction_type(good_model), "hazard_ratio")
})


testthat::test_that("Generalised linear model can predict survival probabilities", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=good_data, type="survival_probability", time=1000)
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, good_data@outcome_type), TRUE)
  
  # Test that the model predicts hazard ratios
  testthat::expect_equal(familiar:::get_prediction_type(good_model, type="survival_probability"), "survival_probability")
})


testthat::test_that("Generalised linear model cannot predict if data is empty", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=empty_data)
  
  # The prediction does not contain any valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, empty_data@outcome_type), FALSE)
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=empty_data, type="survival_probability", time=1000)
  
  # The prediction does not contain any valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, empty_data@outcome_type), FALSE)
})


testthat::test_that("Generalised linear model can predict single samples", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=one_sample_data)
  
  # The prediction is valid.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, one_sample_data@outcome_type), TRUE)
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=one_sample_data, type="survival_probability", time=1000)
  
  # The prediction is valid.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, one_sample_data@outcome_type), TRUE)
})


testthat::test_that("Generalised linear model can train on single features", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(one_feature_model), TRUE)
  
  # Variable importance table is present
  testthat::expect_equal(nrow(familiar:::..vimp(one_feature_model)), 1)
  
  # Valid predictions are possible.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(one_feature_model, one_feature_data), outcome_type=one_feature_model@outcome_type), TRUE)
  
  # Valid survival probability predictions can be made.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(one_feature_model, one_feature_data, type="survival_probability", time=1000), outcome_type=one_feature_model@outcome_type), TRUE)
})


testthat::test_that("Generalised linear model cannot train on wide data", {
  
  # Model was not trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), FALSE)
  
  # Variable importance table is empty.
  testthat::expect_equal(is_empty(familiar:::..vimp(wide_model)), TRUE)
  
  # Valid predictions are not possible.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), FALSE)
  
  # Valid survival probability predictions can not be made.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data, type="survival_probability", time=1000), outcome_type=wide_data@outcome_type), FALSE)
})


testthat::test_that("Generalised linear model does not train for bad data with all censoring", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(bad_model), FALSE)
  
  # No variable importance table.
  testthat::expect_equal(nrow(familiar:::..vimp(bad_model)), 0)
  
  # No valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(bad_model, good_data), outcome_type=good_data@outcome_type), FALSE)
  
  # No valid survival probability predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(bad_model, good_data, type="survival_probability", time=1000), outcome_type=good_data@outcome_type), FALSE)
})

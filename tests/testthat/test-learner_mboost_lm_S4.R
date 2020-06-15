# First test if all selectable learners are also available
familiar:::test_all_learners_available(learners=familiar:::.get_available_mboost_lm_learners(show_general=TRUE))

familiar:::test_all_learners_train_and_predict(learners=familiar:::.get_available_mboost_lm_learners(show_general=FALSE),
                                               hyperparameter_list=list("count"=list("n_boost"=2,
                                                                                     "learning_rate"=-5),
                                                                        "continuous"=list("n_boost"=2,
                                                                                          "learning_rate"=-3),
                                                                        "binomial"=list("n_boost"=2,
                                                                                        "learning_rate"=-3),
                                                                        "survival"=list("n_boost"=2,
                                                                                        "learning_rate"=-3)),
                                               except_predict_survival=c("boosted_glm_loglog",
                                                                         "boosted_glm_lognormal",
                                                                         "boosted_glm_weibull"))

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
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                        "n_boost"=2,
                                                        "learning_rate"=-5),
                               learner="boosted_glm_poisson")

# Train the one-feature model.
one_feature_model <- familiar:::train(data=one_feature_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=list("sign_size"=familiar:::get_n_features(one_feature_data),
                                                               "n_boost"=2,
                                                               "learning_rate"=-5),
                                      learner="boosted_glm_poisson")

# Train the model using wide data.
wide_model <- familiar:::train(data=wide_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data),
                                                        "n_boost"=2,
                                                        "learning_rate"=-5),
                               learner="boosted_glm_poisson")

# Train the model using the bad data (all censored).
bad_model <- familiar:::train(data=bad_data,
                              cluster_method="none",
                              imputation_method="simple",
                              hyperparameter_list=list("sign_size"=familiar:::get_n_features(bad_data),
                                                       "n_boost"=2,
                                                       "learning_rate"=-5),
                              learner="boosted_glm_poisson")

testthat::test_that("Gradient boosting regression model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Gradient boosting regression model has variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 13)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(vimp_table$name %in% familiar:::get_feature_columns(good_data)), TRUE)
  
  # Expect that lower_status_percentage has rank 1 and avg_rooms has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "lower_status_percentage")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "avg_rooms")
})


testthat::test_that("Gradient boosting regression model can predict values", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=good_data)
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, good_data@outcome_type), TRUE)
})


testthat::test_that("Gradient boosting regression model cannot predict if data is empty", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=empty_data)
  
  # The prediction does not contain any valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, empty_data@outcome_type), FALSE)
})


testthat::test_that("Gradient boosting regression model can predict single samples", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=one_sample_data)
  
  # The prediction is valid.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, one_sample_data@outcome_type), TRUE)
})


testthat::test_that("Gradient boosting regression model can train on single features", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(one_feature_model), TRUE)
  
  # Variable importance table is present
  testthat::expect_equal(nrow(familiar:::..vimp(one_feature_model)), 1)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(one_feature_model, one_feature_data), outcome_type=one_feature_model@outcome_type), TRUE)
})


testthat::test_that("Gradient boosting regression model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), FALSE)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
})


testthat::test_that("Gradient boosting regression model does not train for bad data with one class only", {
  
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
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                        "n_boost"=2,
                                                        "learning_rate"=-1),
                               learner="boosted_glm_gaussian")

# Train the one-feature model.
one_feature_model <- familiar:::train(data=one_feature_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=list("sign_size"=familiar:::get_n_features(one_feature_data),
                                                               "n_boost"=2,
                                                               "learning_rate"=-1),
                                      learner="boosted_glm_gaussian")

# Train the model using wide data.
wide_model <- familiar:::train(data=wide_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data),
                                                        "n_boost"=2,
                                                        "learning_rate"=-1),
                               learner="boosted_glm_gaussian")

# Train the model using the bad data (all censored).
bad_model <- familiar:::train(data=bad_data,
                              cluster_method="none",
                              imputation_method="simple",
                              hyperparameter_list=list("sign_size"=familiar:::get_n_features(bad_data),
                                                       "n_boost"=2,
                                                       "learning_rate"=-1),
                              learner="boosted_glm_gaussian")

testthat::test_that("Gradient boosting regression model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Gradient boosting regression model has variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 10)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(vimp_table$name %in% familiar:::get_feature_columns(good_data)), TRUE)
  
  # Expect that avginc has rank 1 and calwpct has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "avginc")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "calwpct")
})


testthat::test_that("Gradient boosting regression model can predict values", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=good_data)
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, good_data@outcome_type), TRUE)
})


testthat::test_that("Gradient boosting regression model cannot predict if data is empty", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=empty_data)
  
  # The prediction does not contain any valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, empty_data@outcome_type), FALSE)
})


testthat::test_that("Gradient boosting regression model can predict single samples", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=one_sample_data)
  
  # The prediction is valid.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, one_sample_data@outcome_type), TRUE)
})


testthat::test_that("Gradient boosting regression model can train on single features", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(one_feature_model), TRUE)
  
  # Variable importance table is present
  testthat::expect_equal(nrow(familiar:::..vimp(one_feature_model)), 1)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(one_feature_model, one_feature_data), outcome_type=one_feature_model@outcome_type), TRUE)
})


testthat::test_that("Gradient boosting regression model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), FALSE)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
})


testthat::test_that("Gradient boosting regression model does not train for bad data with one class only", {
  
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
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                        "n_boost"=2,
                                                        "learning_rate"=-1),
                               learner="boosted_glm_logistic")

# Train the one-feature model.
one_feature_model <- familiar:::train(data=one_feature_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=list("sign_size"=familiar:::get_n_features(one_feature_data),
                                                               "n_boost"=2,
                                                               "learning_rate"=-1),
                                      learner="boosted_glm_logistic")

# Train the model using wide data.
wide_model <- suppressWarnings(familiar:::train(data=wide_data,
                                                cluster_method="none",
                                                imputation_method="simple",
                                                hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data),
                                                                         "n_boost"=2,
                                                                         "learning_rate"=-1),
                                                learner="boosted_glm_logistic"))

# Train the model using the bad data (all censored).
bad_model <- familiar:::train(data=bad_data,
                              cluster_method="none",
                              imputation_method="simple",
                              hyperparameter_list=list("sign_size"=familiar:::get_n_features(bad_data),
                                                       "n_boost"=2,
                                                       "learning_rate"=-1),
                              learner="boosted_glm_logistic")

testthat::test_that("Gradient boosting regression model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Gradient boosting regression model has variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 8)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name), TRUE)
  
  # Expect that cell_shape_uniformity has rank 1 and bare_nuclei has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "cell_shape_uniformity")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "bare_nuclei")
})


testthat::test_that("Gradient boosting regression model can predict class probabilities", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=good_data)
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, good_data@outcome_type), TRUE)
  
  # Expect that the predicted_class column is a factor.
  testthat::expect_s3_class(prediction_table$predicted_class, "factor")
})


testthat::test_that("Gradient boosting regression model cannot predict if data is empty", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=empty_data)
  
  # The prediction does not contain any valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, empty_data@outcome_type), FALSE)
})


testthat::test_that("Gradient boosting regression model can predict single samples", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=one_sample_data)
  
  # The prediction is valid.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, one_sample_data@outcome_type), TRUE)
})


testthat::test_that("Gradient boosting regression model can train on single features", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(one_feature_model), TRUE)
  
  # Variable importance table is present
  testthat::expect_equal(nrow(familiar:::..vimp(one_feature_model)), 1)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(one_feature_model, one_feature_data), outcome_type=one_feature_model@outcome_type), TRUE)
})


testthat::test_that("Gradient boosting regression model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), FALSE)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
})


testthat::test_that("Gradient boosting regression model does not train for bad data with one class only", {
  
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
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                        "n_boost"=2,
                                                        "learning_rate"=-1),
                               time_max=1832,
                               learner="boosted_glm_cox")

# Train the one-feature model.
one_feature_model <- familiar:::train(data=one_feature_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=list("sign_size"=familiar:::get_n_features(one_feature_data),
                                                               "n_boost"=2,
                                                               "learning_rate"=-1),
                                      time_max=1832,
                                      learner="boosted_glm_cox")

# Train the model using wide data.
wide_model <- familiar:::train(data=wide_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data),
                                                        "n_boost"=2,
                                                        "learning_rate"=-1),
                               time_max=1832,
                               learner="boosted_glm_cox")

# Train the model using the bad data (all censored).
bad_model <- familiar:::train(data=bad_data,
                              cluster_method="none",
                              imputation_method="simple",
                              hyperparameter_list=list("sign_size"=familiar:::get_n_features(bad_data),
                                                       "n_boost"=2,
                                                       "learning_rate"=-1),
                              time_max=1832,
                              learner="boosted_glm_cox")


testthat::test_that("Gradient boosting regression model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
  
  # Calibration info is present
  testthat::expect_equal(familiar:::has_calibration_info(good_model), TRUE)
})


testthat::test_that("Gradient boosting regression model has variable importance", {
  
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


testthat::test_that("Gradient boosting regression model can predict risk", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=good_data)
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, good_data@outcome_type), TRUE)
  
  # Test that the model predicts cumulative hazard
  testthat::expect_equal(familiar:::get_prediction_type(good_model), "hazard_ratio")
})


testthat::test_that("Gradient boosting regression model can predict survival probabilities", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=good_data, type="survival_probability", time=1000)
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, good_data@outcome_type), TRUE)
  
  # Test that the model predicts hazard ratios
  testthat::expect_equal(familiar:::get_prediction_type(good_model, type="survival_probability"), "survival_probability")
})


testthat::test_that("Gradient boosting regression model cannot predict if data is empty", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=empty_data)
  
  # The prediction does not contain any valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, empty_data@outcome_type), FALSE)
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=empty_data, type="survival_probability", time=1000)
  
  # The prediction does not contain any valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, empty_data@outcome_type), FALSE)
})


testthat::test_that("Gradient boosting regression model can predict single samples", {
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=one_sample_data)
  
  # The prediction is valid.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, one_sample_data@outcome_type), TRUE)
  
  # Extract the prediction table.
  prediction_table <- familiar:::.predict(good_model, data=one_sample_data, type="survival_probability", time=1000)
  
  # The prediction is valid.
  testthat::expect_equal(familiar:::any_predictions_valid(prediction_table, one_sample_data@outcome_type), TRUE)
})


testthat::test_that("Gradient boosting regression model can train on single features", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(one_feature_model), TRUE)
  
  # Variable importance table is present
  testthat::expect_equal(nrow(familiar:::..vimp(one_feature_model)), 1)
  
  # Valid predictions are possible.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(one_feature_model, one_feature_data), outcome_type=one_feature_model@outcome_type), TRUE)
  
  # Valid survival probability predictions can be made.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(one_feature_model, one_feature_data, type="survival_probability", time=1000), outcome_type=one_feature_model@outcome_type), TRUE)
})


testthat::test_that("Gradient boosting regression model can train on wide data", {
  
  # Model was trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), FALSE)
  
  # Valid predictions are present.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
  
  # Valid survival probability predictions can be made.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data, type="survival_probability", time=1000), outcome_type=wide_data@outcome_type), TRUE)
})


testthat::test_that("Gradient boosting regression model does not train for bad data with all censoring", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(bad_model), FALSE)
  
  # No variable importance table.
  testthat::expect_equal(nrow(familiar:::..vimp(bad_model)), 0)
  
  # No valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(bad_model, good_data), outcome_type=good_data@outcome_type), FALSE)
  
  # No valid survival probability predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(bad_model, good_data, type="survival_probability", time=1000), outcome_type=good_data@outcome_type), FALSE)
})

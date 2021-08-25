# First test if all selectable learners are also available
familiar:::test_all_learners_available(learners=familiar:::.get_available_xgboost_lm_learners(show_general=TRUE))

# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

familiar:::test_all_learners_train_predict_vimp(learners=familiar:::.get_available_xgboost_lm_learners(show_general=FALSE),
                                                hyperparameter_list=list("count"=list("n_boost" = 2,
                                                                                      "learning_rate" = -1,
                                                                                      "lambda" = 0.0,
                                                                                      "alpha" =-6.0),
                                                                         "continuous"=list("n_boost" = 2,
                                                                                           "learning_rate" = -1,
                                                                                           "lambda" = 0.0,
                                                                                           "alpha" =-6.0),
                                                                         "binomial"=list("n_boost" = 2,
                                                                                         "learning_rate" = -1,
                                                                                         "lambda" = 0.0,
                                                                                         "alpha" =-6.0),
                                                                         "multinomial"=list("n_boost" = 2,
                                                                                            "learning_rate" = -1,
                                                                                            "lambda" = 0.0,
                                                                                            "alpha" =-6.0),
                                                                         "survival"=list("n_boost" = 2,
                                                                                         "learning_rate" = -1,
                                                                                         "lambda" = 0.0,
                                                                                         "alpha" =-6.0)))


#####Count outcome tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("count")
wide_data <- familiar:::test.create_wide_data_set("count")

# Train the model using the good dataset.
good_model <- familiar:::test_train(data=good_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                             "n_boost" = 2,
                                                             "learning_rate" = -1,
                                                             "lambda" = 0.0,
                                                             "alpha" =-6.0),
                                    learner="xgboost_lm_poisson")

# Train the model using wide data.
wide_model <- familiar:::test_train(data=wide_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data),
                                                             "n_boost" = 2,
                                                             "learning_rate" = -1,
                                                             "lambda" = 0.0,
                                                             "alpha" =-6.0),
                                    learner="xgboost_lm_poisson")


testthat::test_that("Extreme gradient boosting regression model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Extreme gradient boosting regression model has variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 13)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(vimp_table$name %in% familiar:::get_feature_columns(good_data)), TRUE)
  
  # Expect that avg_rooms has rank 1 and lower_status_percentage has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "avg_rooms")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "lower_status_percentage")
})



testthat::test_that("Extreme gradient boosting regression model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), FALSE)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
})


#####Continuous outcome tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("continuous")
wide_data <- familiar:::test.create_wide_data_set("continuous")

# Train the model using the good dataset.
good_model <- familiar:::test_train(data=good_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                             "n_boost" = 2,
                                                             "learning_rate" = -1,
                                                             "lambda" = 0.0,
                                                             "alpha" =-6.0),
                                    learner="xgboost_lm_gaussian")

# Train the model using wide data.
wide_model <- familiar:::test_train(data=wide_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data),
                                                             "n_boost" = 2,
                                                             "learning_rate" = -1,
                                                             "lambda" = 0.0,
                                                             "alpha" =-6.0),
                                    learner="xgboost_lm_gaussian")


testthat::test_that("Extreme gradient boosting regression model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Extreme gradient boosting regression model has variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 10)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(vimp_table$name %in% familiar:::get_feature_columns(good_data)), TRUE)
  
  # Expect that avginc and calwpct have rank 1 and 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name %in% c("calwpct", "avginc"), TRUE)
  testthat::expect_equal(vimp_table[rank == 2, ]$name %in% c("calwpct", "avginc"), TRUE)
})


testthat::test_that("Extreme gradient boosting regression model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), FALSE)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
})


#####Binomial tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("binomial")
wide_data <- familiar:::test.create_wide_data_set("binomial")

# Train the model using the good dataset.
good_model <- familiar:::test_train(data=good_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                             "n_boost" = 2,
                                                             "learning_rate" = -1,
                                                             "lambda" = 0.0,
                                                             "alpha" =-6.0),
                                    learner="xgboost_lm_logistic")

# Train the model using wide data.
wide_model <- familiar:::test_train(data=wide_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data),
                                                             "n_boost" = 2,
                                                             "learning_rate" = -1,
                                                             "lambda" = 0.0,
                                                             "alpha" =-6.0),
                                    learner="xgboost_lm_logistic")

testthat::test_that("Extreme gradient boosting regression model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Extreme gradient boosting regression model has variable importance", {
  
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


testthat::test_that("Extreme gradient boosting regression model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), FALSE)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
})



#####Multinomial tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("multinomial")
wide_data <- familiar:::test.create_wide_data_set("multinomial")

# Train the model using the good dataset.
good_model <- familiar:::test_train(data=good_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                             "n_boost" = 2,
                                                             "learning_rate" = -1,
                                                             "lambda" = 0.0,
                                                             "alpha" =-6.0),
                                    learner="xgboost_lm_logistic")

# Train the model using wide data.
wide_model <- familiar:::test_train(data=wide_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data),
                                                             "n_boost" = 2,
                                                             "learning_rate" = -1,
                                                             "lambda" = 0.0,
                                                             "alpha" =-6.0),
                                    learner="xgboost_lm_logistic")

testthat::test_that("Extreme gradient boosting regression model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Extreme gradient boosting regression model has variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 4)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name), TRUE)
  
  # Expect that Petal_Length has rank 1 and Petal_Width has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "Petal_Length")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "Petal_Width")
})


testthat::test_that("Extreme gradient boosting regression model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), FALSE)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
})



#####Survival tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("survival")
wide_data <- familiar:::test.create_wide_data_set("survival")

# Train the model using the good dataset.
good_model <- familiar:::test_train(data=good_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                             "n_boost" = 2,
                                                             "learning_rate" = -1,
                                                             "lambda" = 0.0,
                                                             "alpha" =-6.0),
                                    time_max=1832,
                                    learner="xgboost_lm_cox")

# Train the model using wide data.
wide_model <- suppressWarnings(familiar:::test_train(data=wide_data,
                                                     cluster_method="none",
                                                     imputation_method="simple",
                                                     hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data),
                                                                              "n_boost" = 2,
                                                                              "learning_rate" = -1,
                                                                              "lambda" = 0.0,
                                                                              "alpha" =-6.0),
                                                     time_max=1832,
                                                     learner="xgboost_lm_cox"))


testthat::test_that("Extreme gradient boosting regression model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
  
  # Test the prediction type
  testthat::expect_equal(familiar:::get_prediction_type(good_model), "hazard_ratio")
  
  # Test that the model predicts hazard ratios
  testthat::expect_equal(familiar:::get_prediction_type(good_model, type="survival_probability"), "survival_probability")
})


testthat::test_that("Extreme gradient boosting regression model has variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has three rows.
  testthat::expect_equal(nrow(vimp_table), 3)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name), TRUE)
  
  # Expect that nodes has rank 1 and rx has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "nodes")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "rx")
})


testthat::test_that("Extreme gradient boosting regression model can train and predict on wide data", {
  
  # Model was trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), FALSE)
  
  # Valid predictions are present.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
  
  # Valid survival probability predictions can be made.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data, type="survival_probability", time=1000), outcome_type=wide_data@outcome_type), TRUE)
})


testthat::skip("Skip hyperparameter optimisation, unless manual.")

familiar:::test_hyperparameter_optimisation(learners=familiar:::.get_available_xgboost_lm_learners(show_general=TRUE),
                                            n_random_sets=100L,
                                            debug=TRUE,
                                            parallel=FALSE)

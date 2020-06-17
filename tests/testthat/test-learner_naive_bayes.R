# First test if all selectable learners are also available
familiar:::test_all_learners_available(learners=familiar:::.get_available_naive_bayes_learners(show_general=TRUE))

familiar:::test_all_learners_train_predict_vimp(learners=familiar:::.get_available_naive_bayes_learners(show_general=FALSE),
                                                has_vimp=FALSE)

#####Binomial tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("binomial")
wide_data <- familiar:::test.create_wide_data_set("binomial")

# Train the model using the good dataset.
good_model <- familiar:::train(data=good_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data)),
                               learner="naive_bayes")

# Train the model using wide data.
wide_model <- familiar:::train(data=wide_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data)),
                               learner="naive_bayes")

testthat::test_that("Naive Bayes model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Naive Bayes model has no variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table is empty.
  testthat::expect_equal(familiar:::is_empty(vimp_table), TRUE)
})


testthat::test_that("Naive Bayes model can not train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), FALSE)
  
  # Variable importance table is absent.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), TRUE)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), FALSE)
})


#####Multinomial tests----------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("multinomial")
wide_data <- familiar:::test.create_wide_data_set("multinomial")

# Train the model using the good dataset.
good_model <- suppressWarnings(familiar:::train(data=good_data,
                                                cluster_method="none",
                                                imputation_method="simple",
                                                hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data)),
                                                learner="naive_bayes"))

# Train the model using wide data.
wide_model <- familiar:::train(data=wide_data,
                               cluster_method="none",
                               imputation_method="simple",
                               hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data)),
                               learner="naive_bayes")

testthat::test_that("Naive Bayes model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Naive Bayes model has no variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table is empty.
  testthat::expect_equal(familiar:::is_empty(vimp_table), TRUE)
})


testthat::test_that("Naive Bayes model can not train on wide data", {
  
  # Model cannot be trained.
  testthat::expect_equal(familiar:::model_is_trained(wide_model), FALSE)
  
  # Variable importance table is empty.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), TRUE)
  
  # Valid predictions cannot be made.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), FALSE)
})

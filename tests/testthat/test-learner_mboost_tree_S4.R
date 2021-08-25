# First test if all selectable learners are also available
familiar:::test_all_learners_available(learners=familiar:::.get_available_mboost_lm_learners(show_general=TRUE))

# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

familiar:::test_all_learners_train_predict_vimp(learners=familiar:::.get_available_mboost_tree_learners(show_general=FALSE),
                                                hyperparameter_list=list("count"=list("n_boost"=2,
                                                                                      "learning_rate"=-5,
                                                                                      "tree_depth"=3,
                                                                                      "min_child_weight"=0.5,
                                                                                      "alpha"=0.10),
                                                                         "continuous"=list("n_boost"=2,
                                                                                           "learning_rate"=-3,
                                                                                           "tree_depth"=3,
                                                                                           "min_child_weight"=0.5,
                                                                                           "alpha"=0.10),
                                                                         "binomial"=list("n_boost"=2,
                                                                                         "learning_rate"=-3,
                                                                                         "tree_depth"=3,
                                                                                         "min_child_weight"=0.5,
                                                                                         "alpha"=0.10),
                                                                         "survival"=list("n_boost"=2,
                                                                                         "learning_rate"=-3,
                                                                                         "tree_depth"=3,
                                                                                         "min_child_weight"=0.5,
                                                                                         "alpha"=0.10)),
                                                except_predict_survival=c("boosted_tree_loglog",
                                                                          "boosted_tree_lognormal",
                                                                          "boosted_tree_weibull"),
                                                has_vimp=FALSE)

#####Count outcome tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("count")
wide_data <- familiar:::test.create_wide_data_set("count")

# Train the model using the good dataset.
good_model <- familiar:::test_train(data=good_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                             "n_boost"=2,
                                                             "learning_rate"=-5,
                                                             "tree_depth"=3,
                                                             "min_child_weight"=0.5,
                                                             "alpha"=0.10),
                                    learner="boosted_tree_poisson")

# Train the model using wide data.
wide_model <- familiar:::test_train(data=wide_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data),
                                                             "n_boost"=2,
                                                             "learning_rate"=-5,
                                                             "tree_depth"=3,
                                                             "min_child_weight"=0.5,
                                                             "alpha"=0.10),
                                    learner="boosted_tree_poisson")


testthat::test_that("Gradient boosting tree model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Gradient boosting tree model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), TRUE)
  
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
                                                             "n_boost"=2,
                                                             "learning_rate"=-1,
                                                             "tree_depth"=3,
                                                             "min_child_weight"=0.5,
                                                             "alpha"=0.10),
                                    learner="boosted_tree_gaussian")

# Train the model using wide data.
wide_model <- familiar:::test_train(data=wide_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data),
                                                             "n_boost"=2,
                                                             "learning_rate"=-1,
                                                             "tree_depth"=3,
                                                             "min_child_weight"=0.5,
                                                             "alpha"=0.10),
                                    learner="boosted_tree_gaussian")


testthat::test_that("Gradient boosting tree model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Gradient boosting tree model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), TRUE)
  
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
                                                             "n_boost"=2,
                                                             "learning_rate"=-1,
                                                             "tree_depth"=3,
                                                             "min_child_weight"=0.5,
                                                             "alpha"=0.10),
                                    learner="boosted_tree_logistic")

# Train the model using wide data.
wide_model <- suppressWarnings(familiar:::test_train(data=wide_data,
                                                     cluster_method="none",
                                                     imputation_method="simple",
                                                     hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data),
                                                                              "n_boost"=2,
                                                                              "learning_rate"=-1,
                                                                              "tree_depth"=3,
                                                                              "min_child_weight"=0.5,
                                                                              "alpha"=0.10),
                                                     learner="boosted_tree_logistic"))

testthat::test_that("Gradient boosting tree model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
})


testthat::test_that("Gradient boosting tree model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), TRUE)
  
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
                                                             "n_boost"=2,
                                                             "learning_rate"=-1,
                                                             "tree_depth"=3,
                                                             "min_child_weight"=0.5,
                                                             "alpha"=0.10),
                                    time_max=1832,
                                    learner="boosted_tree_cox")

# Train the model using wide data.
wide_model <- familiar:::test_train(data=wide_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data),
                                                             "n_boost"=2,
                                                             "learning_rate"=-1,
                                                             "tree_depth"=3,
                                                             "min_child_weight"=0.5,
                                                             "alpha"=0.10),
                                    time_max=1832,
                                    learner="boosted_tree_cox")


testthat::test_that("Gradient boosting tree model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
  
  # Test the prediction type
  testthat::expect_equal(familiar:::get_prediction_type(good_model), "hazard_ratio")
  
  # Test that the model predicts hazard ratios
  testthat::expect_equal(familiar:::get_prediction_type(good_model, type="survival_probability"), "survival_probability")
})


testthat::test_that("Gradient boosting tree model can train and predict on wide data", {
  
  # Model was trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  testthat::expect_equal(familiar:::is_empty(familiar:::..vimp(wide_model)), TRUE)
  
  # Valid predictions are present.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
  
  # Valid survival probability predictions can be made.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data, type="survival_probability", time=1000), outcome_type=wide_data@outcome_type), TRUE)
})


testthat::skip("Skip hyperparameter optimisation, unless manual.")

familiar:::test_hyperparameter_optimisation(learners=familiar:::.get_available_mboost_tree_learners(show_general=TRUE),
                                            debug=FALSE,
                                            parallel=FALSE)

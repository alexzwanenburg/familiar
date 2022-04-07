# First test if all selectable learners are also available
familiar:::test_all_learners_available(learners=familiar:::.get_available_svm_c_learners(show_general=TRUE))
familiar:::test_all_learners_available(learners=familiar:::.get_available_svm_nu_learners(show_general=TRUE))
familiar:::test_all_learners_available(learners=familiar:::.get_available_svm_eps_learners(show_general=TRUE))

# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

familiar:::test_all_learners_train_predict_vimp(learners=familiar:::.get_available_svm_c_learners(show_general=FALSE),
                                                hyperparameter_list=list("binomial"=list("c"=-1.0,
                                                                                         "gamma"=0.1,
                                                                                         "degree"=2.0,
                                                                                         "offset"=0.0),
                                                                         "multinomial"=list("c"=-1.0,
                                                                                            "gamma"=0.1,
                                                                                            "degree"=2.0,
                                                                                            "offset"=0.0)),
                                                has_vimp=FALSE)


familiar:::test_all_learners_train_predict_vimp(learners=familiar:::.get_available_svm_nu_learners(show_general=FALSE),
                                                hyperparameter_list=list("count"=list("c"=-1.0,
                                                                                      "epsilon"=0.0,
                                                                                      "nu"=-4.0,
                                                                                      "gamma"=0.1,
                                                                                      "degree"=2.0,
                                                                                      "offset"=0.0),
                                                                         "continuous"=list("c"=-1.0,
                                                                                           "epsilon"=0.0,
                                                                                           "nu"=-4.0,
                                                                                           "gamma"=0.1,
                                                                                           "degree"=2.0,
                                                                                           "offset"=0.0),
                                                                         "binomial"=list("c"=-1.0,
                                                                                         "epsilon"=0.0,
                                                                                         "nu"=-4.0,
                                                                                         "gamma"=0.1,
                                                                                         "degree"=2.0,
                                                                                         "offset"=0.0),
                                                                         "multinomial"=list("c"=-1.0,
                                                                                            "epsilon"=0.0,
                                                                                            "nu"=-4.0,
                                                                                            "gamma"=0.1,
                                                                                            "degree"=2.0,
                                                                                            "offset"=0.0)),
                                                has_vimp=FALSE)


familiar:::test_all_learners_train_predict_vimp(learners=familiar:::.get_available_svm_eps_learners(show_general=FALSE),
                                                hyperparameter_list=list("count"=list("c"=-1.0,
                                                                                      "epsilon"=0.0,
                                                                                      "gamma"=0.1,
                                                                                      "degree"=2.0,
                                                                                      "offset"=0.0),
                                                                         "continuous"=list("c"=-1.0,
                                                                                           "epsilon"=0.0,
                                                                                           "gamma"=0.1,
                                                                                           "degree"=2.0,
                                                                                           "offset"=0.0)),
                                                has_vimp=FALSE)

# Parallel tests.
familiar:::test_all_learners_parallel_train_predict_vimp(learners=familiar:::.get_available_svm_c_learners(show_general=FALSE),
                                                         hyperparameter_list=list("binomial"=list("c"=-1.0,
                                                                                                  "gamma"=0.1,
                                                                                                  "degree"=2.0,
                                                                                                  "offset"=0.0),
                                                                                  "multinomial"=list("c"=-1.0,
                                                                                                     "gamma"=0.1,
                                                                                                     "degree"=2.0,
                                                                                                     "offset"=0.0)),
                                                         has_vimp=FALSE)


familiar:::test_all_learners_parallel_train_predict_vimp(learners=familiar:::.get_available_svm_nu_learners(show_general=FALSE),
                                                         hyperparameter_list=list("count"=list("c"=-1.0,
                                                                                               "epsilon"=0.0,
                                                                                               "nu"=-4.0,
                                                                                               "gamma"=0.1,
                                                                                               "degree"=2.0,
                                                                                               "offset"=0.0),
                                                                                  "continuous"=list("c"=-1.0,
                                                                                                    "epsilon"=0.0,
                                                                                                    "nu"=-4.0,
                                                                                                    "gamma"=0.1,
                                                                                                    "degree"=2.0,
                                                                                                    "offset"=0.0),
                                                                                  "binomial"=list("c"=-1.0,
                                                                                                  "epsilon"=0.0,
                                                                                                  "nu"=-4.0,
                                                                                                  "gamma"=0.1,
                                                                                                  "degree"=2.0,
                                                                                                  "offset"=0.0),
                                                                                  "multinomial"=list("c"=-1.0,
                                                                                                     "epsilon"=0.0,
                                                                                                     "nu"=-4.0,
                                                                                                     "gamma"=0.1,
                                                                                                     "degree"=2.0,
                                                                                                     "offset"=0.0)),
                                                         has_vimp=FALSE)


familiar:::test_all_learners_parallel_train_predict_vimp(learners=familiar:::.get_available_svm_eps_learners(show_general=FALSE),
                                                         hyperparameter_list=list("count"=list("c"=-1.0,
                                                                                               "epsilon"=0.0,
                                                                                               "gamma"=0.1,
                                                                                               "degree"=2.0,
                                                                                               "offset"=0.0),
                                                                                  "continuous"=list("c"=-1.0,
                                                                                                    "epsilon"=0.0,
                                                                                                    "gamma"=0.1,
                                                                                                    "degree"=2.0,
                                                                                                    "offset"=0.0)),
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
                                                             "c"=-1.0,
                                                             "epsilon"=0.0,
                                                             "gamma"=0.0),
                                    learner="svm_eps_radial")

# Train the model using wide data.
wide_model <- familiar:::test_train(data=wide_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                             "c"=-1.0,
                                                             "epsilon"=0.0,
                                                             "gamma"=0.0),
                                    learner="svm_eps_radial")

testthat::test_that("SVM model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
  
  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)
  
  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("SVM model has no variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table is empty.
  testthat::expect_equal(is_empty(vimp_table), TRUE)
})


testthat::test_that("SVM model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is absent
  testthat::expect_equal(is_empty(familiar:::..vimp(wide_model)), TRUE)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
  
  # Test that no deprecation warnings are given.
  familiar:::test_not_deprecated(wide_model@messages$warning)
  
  # Test that no errors appear.
  testthat::expect_equal(wide_model@messages$error, NULL)
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
                                                             "c"=-1.0,
                                                             "epsilon"=0.0,
                                                             "gamma"=1.0),
                                    learner="svm_eps_radial")

# Train the model using wide data.
wide_model <- familiar:::test_train(data=wide_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                             "c"=-1.0,
                                                             "epsilon"=0.0,
                                                             "gamma"=1.0),
                                    learner="svm_eps_radial")

testthat::test_that("SVM model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
  
  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)
  
  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("SVM model has no variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table is empty.
  testthat::expect_equal(is_empty(vimp_table), TRUE)
})


testthat::test_that("SVM model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is absent.
  testthat::expect_equal(is_empty(familiar:::..vimp(wide_model)), TRUE)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
  
  # Test that no deprecation warnings are given.
  familiar:::test_not_deprecated(wide_model@messages$warning)
  
  # Test that no errors appear.
  testthat::expect_equal(wide_model@messages$error, NULL)
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
                                                             "c"=-1.0,
                                                             "gamma"=1.0),
                                    learner="svm_c_radial")

# Train the model using wide data.
wide_model <- familiar:::test_train(data=good_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                             "c"=-1.0,
                                                             "gamma"=1.0),
                                    learner="svm_c_radial")

testthat::test_that("SVM model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
  
  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)
  
  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("SVM model has no variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table is empty.
  testthat::expect_equal(is_empty(vimp_table), TRUE)
})


testthat::test_that("SVM model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is absent.
  testthat::expect_equal(is_empty(familiar:::..vimp(wide_model)), TRUE)
  
  # Valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
  
  # Test that no deprecation warnings are given.
  familiar:::test_not_deprecated(wide_model@messages$warning)
  
  # Test that no errors appear.
  testthat::expect_equal(wide_model@messages$error, NULL)
})


#####Multinomial tests----------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("multinomial")
wide_data <- familiar:::test.create_wide_data_set("multinomial")

# Train the model using the good dataset.
good_model <- familiar:::test_train(data=good_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                             "c"=-1.0,
                                                             "gamma"=1.0),
                                    learner="svm_c_radial")

# Train the model using wide data.
wide_model <- familiar:::test_train(data=good_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data),
                                                             "c"=-1.0,
                                                             "gamma"=1.0),
                                    learner="svm_c_radial")

testthat::test_that("SVM model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
  
  # Check that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)
  
  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("SVM model has no variable importance", {
  
  # Extract the variable importance table.
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table is empty.
  testthat::expect_equal(is_empty(vimp_table), TRUE)
})


testthat::test_that("SVM model can train on wide data", {
  
  # Model cannot be trained.
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is empty.
  testthat::expect_equal(is_empty(familiar:::..vimp(wide_model)), TRUE)
  
  # Valid predictions can be made.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE)
  
  # Test that no deprecation warnings are given.
  familiar:::test_not_deprecated(wide_model@messages$warning)
  
  # Test that no errors appear.
  testthat::expect_equal(wide_model@messages$error, NULL)
})


testthat::skip("Skip hyperparameter optimisation, unless manual.")

familiar:::test_hyperparameter_optimisation(learners=familiar:::.get_available_svm_c_learners(show_general=TRUE),
                                            debug=FALSE,
                                            parallel=FALSE)

familiar:::test_hyperparameter_optimisation(learners=familiar:::.get_available_svm_nu_learners(show_general=TRUE),
                                            debug=FALSE,
                                            parallel=FALSE)

familiar:::test_hyperparameter_optimisation(learners=familiar:::.get_available_svm_eps_learners(show_general=TRUE),
                                            debug=FALSE,
                                            parallel=FALSE)


# First test if all selectable learners are also available
familiar:::test_all_learners_available(learners=familiar:::.get_available_glm_learners(show_general=TRUE))

# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

familiar:::test_all_learners_train_predict_vimp(learners=familiar:::.get_available_glm_learners(show_general=FALSE))

familiar:::test_all_learners_parallel_train_predict_vimp(learners=familiar:::.get_available_glm_learners(show_general=FALSE))

#####Count outcome tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("count")
wide_data <- familiar:::test.create_wide_data_set("count")

# Train the model using the good dataset.
good_model <- familiar:::test_train(data=good_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data)),
                                    learner="glm_poisson")

# Train the model using wide data.
wide_model <- familiar:::test_train(data=wide_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data)),
                                    learner="glm_poisson")

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
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 13)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name), TRUE)
  
  # Expect that avg_rooms has rank 1 and residence_before_1940_proportion has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "avg_rooms")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "residence_before_1940_proportion")
})


testthat::test_that("Generalised linear model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  suppressWarnings(testthat::expect_equal(is_empty(familiar:::..vimp(wide_model)), FALSE))
  
  # Valid predictions.
  suppressWarnings(testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE))
  
  # That no deprecation warnings are given.
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
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data)),
                                    learner="glm_gaussian")

# Train the model using wide data.
wide_model <- familiar:::test_train(data=wide_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data)),
                                    learner="glm_gaussian")

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
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 10)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name), TRUE)
  
  # Expect that calwpct has rank 1 and elpct has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "calwpct")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "elpct")
})


testthat::test_that("Generalised linear model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  suppressWarnings(testthat::expect_equal(is_empty(familiar:::..vimp(wide_model)), FALSE))
  
  # Valid predictions.
  suppressWarnings(testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE))
  
  # That no deprecation warnings are given.
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
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data)),
                                    learner="glm_logistic")

# Train the model using wide data.
wide_model <- familiar:::test_train(data=wide_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data)),
                                    learner="glm_logistic")

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
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 8)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name), TRUE)
  
  # Expect that bare_nuclei has rank 1 and clump_thickness has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "bare_nuclei")
  testthat::expect_equal(vimp_table[rank == 2, ]$name %in% c("clump_thickness", "cell_shape_uniformity"), TRUE)
})


testthat::test_that("Generalised linear model can train on wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), TRUE)
  
  # Variable importance table is present.
  suppressWarnings(testthat::expect_equal(is_empty(familiar:::..vimp(wide_model)), FALSE))
  
  # Valid predictions.
  suppressWarnings(testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), TRUE))
  
  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(wide_model@messages$warning)
  
  # Test that no errors appear.
  testthat::expect_equal(wide_model@messages$error, NULL)
})


#####Multinomial tests----------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("multinomial")
wide_data <- familiar:::test.create_wide_data_set("multinomial")

# Train the model using the good dataset.
good_model <- suppressWarnings(familiar:::test_train(data=good_data,
                                                     cluster_method="none",
                                                     imputation_method="simple",
                                                     hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data)),
                                                     learner="glm_multinomial"))

# Train the model using wide data.
wide_model <- familiar:::test_train(data=wide_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data)),
                                    learner="glm_multinomial")

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
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 4)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name), TRUE)
  
  # Expect that Petal length has rank 1 and petal width has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "Petal_Length")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "Petal_Width")
})


testthat::test_that("Generalised linear model can not train on wide data", {
  
  # Model cannot be trained.
  testthat::expect_equal(familiar:::model_is_trained(wide_model), FALSE)
  
  # Variable importance table is empty.
  testthat::expect_equal(is_empty(familiar:::..vimp(wide_model)), TRUE)
  
  # Valid predictions cannot be made.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data), outcome_type=wide_data@outcome_type), FALSE)
  
  # That no deprecation warnings are given.
  familiar:::test_not_deprecated(wide_model@messages$warning)
  
  # Test that specific warnings and errors appear.
  testthat::expect_equal(length(wide_model@messages$error), 1L)
  testthat::expect_equal(grepl(x=wide_model@messages$error, pattern="vglm.fitter: There are", fixed=TRUE) &&
                           grepl(x=wide_model@messages$error, pattern="parameters but only", fixed=TRUE) &&
                           grepl(x=wide_model@messages$error, pattern="observations", fixed=TRUE),
                         TRUE)
})


#####Survival tests-------------------------------------------------------------

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("survival")
wide_data <- familiar:::test.create_wide_data_set("survival")

# Train the model using the good dataset.
good_model <- familiar:::test_train(data=good_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data)),
                                    time_max=1832,
                                    learner="glm")

# Train the model using wide data.
wide_model <- suppressWarnings(familiar:::test_train(data=wide_data,
                                                     cluster_method="none",
                                                     imputation_method="simple",
                                                     hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data)),
                                                     time_max=1832,
                                                     learner="glm"))

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
  vimp_table <- familiar:::..vimp(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 3)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name), TRUE)
  
  # Expect that nodes has rank 1 and rx has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "nodes")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "rx")
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
  
  # Test that specific warnings and errors appear.
  testthat::expect_equal(length(wide_model@messages$warning), 1L)
  testthat::expect_equal(grepl(x=wide_model@messages$warning, pattern="did not converge", fixed=TRUE),
                         TRUE)
  
  testthat::expect_equal(length(wide_model@messages$error), 1L)
  testthat::expect_equal(grepl(x=wide_model@messages$error, pattern="did not converge", fixed=TRUE),
                         TRUE)
})


testthat::skip("Skip hyperparameter optimisation, unless manual.")

# Test hyperparameters
familiar:::test_hyperparameter_optimisation(learners=familiar:::.get_available_glm_learners(show_general=TRUE),
                                            debug=FALSE,
                                            parallel=FALSE)

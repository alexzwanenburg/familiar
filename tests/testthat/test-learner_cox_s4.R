# First test if all selectable learners are also available
familiar:::test_all_learners_available(learners=familiar:::.get_available_cox_learners(show_general=TRUE))

# Don't perform any further tests on CRAN due to time of running the test.
testthat::skip_on_cran()
testthat::skip_on_ci()

# Generic test
familiar:::test_all_learners_train_predict_vimp(learners=familiar:::.get_available_cox_learners(show_general=TRUE))

familiar:::test_all_learners_parallel_train_predict_vimp(learners=familiar:::.get_available_cox_learners(show_general=TRUE))

# Create test data sets.
good_data <- familiar:::test.create_good_data_set("survival")
wide_data <- familiar:::test.create_wide_data_set("survival")

# Train the model using the good dataset.
good_model <- familiar:::test_train(data=good_data,
                                    cluster_method="none",
                                    imputation_method="simple",
                                    hyperparameter_list=list("sign_size"=familiar:::get_n_features(good_data)),
                                    learner="cox")

# Train the model using wide data.
wide_model <- suppressWarnings(familiar:::test_train(data=wide_data,
                                                     cluster_method="none",
                                                     imputation_method="simple",
                                                     hyperparameter_list=list("sign_size"=familiar:::get_n_features(wide_data)),
                                                     learner="cox"))


testthat::test_that("Cox model trained correctly", {
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(good_model), TRUE)
  
  # Test the prediction type
  testthat::expect_equal(familiar:::get_prediction_type(good_model), "hazard_ratio")
  
  # Test that the model predicts hazard ratios
  testthat::expect_equal(familiar:::get_prediction_type(good_model, type="survival_probability"), "survival_probability")
  
  # Checkt that no deprecation warnings are given.
  familiar:::test_not_deprecated(good_model@messages$warning)
  
  # Test that no errors appear.
  testthat::expect_equal(good_model@messages$error, NULL)
})


testthat::test_that("Cox model has variable importance", {

  # Extract the variable importance table.
  vimp_table <- familiar:::get_vimp_table(good_model)
  
  # Expect that the vimp table has two rows.
  testthat::expect_equal(nrow(vimp_table), 3)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_equal(all(familiar:::get_feature_columns(good_data) %in% vimp_table$name), TRUE)
  
  # Expect that nodes has rank 1 and rx has rank 2.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "nodes")
  testthat::expect_equal(vimp_table[rank == 2, ]$name, "rx")
})


testthat::test_that("Cox model does not train for wide data", {
  
  # Model trained
  testthat::expect_equal(familiar:::model_is_trained(wide_model), FALSE)
  
  # No variable importance table.
  testthat::expect_equal(familiar:::is_empty(familiar:::get_vimp_table(wide_model)), TRUE)
  
  # No valid predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data),
                                                          outcome_type=wide_data@outcome_type),
                         FALSE)
  
  # No valid survival probability predictions.
  testthat::expect_equal(familiar:::any_predictions_valid(familiar:::.predict(wide_model, wide_data, type="survival_probability", time=1000),
                                                          outcome_type=wide_data@outcome_type),
                         FALSE)
  
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
familiar:::test_hyperparameter_optimisation(
  learners=familiar:::.get_available_cox_learners(show_general=TRUE),
  debug=FALSE,
  parallel=FALSE
)

# Find available stratification methods.
stratification_methods <- familiar:::.get_available_stratification_methods()

# Test for good dataset.
for(stratification_method in stratification_methods){
  # Create a dataset using the good dataset.
  data <- familiar:::test.create_good_data_set("survival")
  
  # Train a simple linear GLM using the good dataset.
  fam_model <- familiar:::test_train(data=data,
                                     cluster_method="none",
                                     imputation_method="simple",
                                     hyperparameter_list=list("sign_size"=familiar:::get_n_features(data)),
                                     learner="cox",
                                     stratification_method=stratification_method,
                                     create_novelty_detector=FALSE)
  
  # Risk stratification.
  predictions_risk <- familiar::predict(object=fam_model,
                                        newdata=data,
                                        type="risk_stratification")
  
  testthat::test_that("Risk groups are formed.", {
    testthat::expect_equal(fam_model@km_info$stratification_method, stratification_method)
    testthat::expect_equal(all(is.finite(predictions_risk$risk_group)), TRUE)
  })
}

# Test for single-feature dataset.
for(stratification_method in stratification_methods){
  # Create a dataset using the single-feature dataset.
  data <- familiar:::test.create_one_feature_data_set("survival")
  
  # Train a simple linear GLM using the good dataset.
  fam_model <- familiar:::test_train(data=data,
                                     cluster_method="none",
                                     imputation_method="simple",
                                     hyperparameter_list=list("sign_size"=familiar:::get_n_features(data)),
                                     learner="cox",
                                     stratification_method=stratification_method,
                                     create_novelty_detector=FALSE)
  
  # Risk stratification.
  predictions_risk <- familiar::predict(object=fam_model,
                                        newdata=data,
                                        type="risk_stratification")
  
  testthat::test_that("Risk groups are formed.", {
    testthat::expect_equal(fam_model@km_info$stratification_method, stratification_method)
    testthat::expect_equal(all(is.finite(predictions_risk$risk_group)), TRUE)
  })
}

# Test for dataset with only one value for a feature.
for(stratification_method in stratification_methods){
  # Create a dataset with a feature that only has two values.
  data <- familiar:::test.create_one_feature_two_values_data_set("survival")
  
  # Train a simple linear GLM using the feature set that only has two values.
  fam_model <- familiar:::test_train(data=data,
                                     cluster_method="none",
                                     imputation_method="simple",
                                     hyperparameter_list=list("sign_size"=familiar:::get_n_features(data)),
                                     learner="cox",
                                     stratification_method=stratification_method,
                                     create_novelty_detector=FALSE)
  
  # Risk stratification.
  predictions_risk <- familiar::predict(object=fam_model,
                                        newdata=data,
                                        type="risk_stratification")
  
  testthat::test_that("Risk groups are formed.", {
    testthat::expect_equal(fam_model@km_info$stratification_method, stratification_method)
    testthat::expect_equal(all(is.finite(predictions_risk$risk_group)), TRUE)
  })
}


# Test that multiple risk stratifications methods can be applied simultaneously.

# Create a dataset using the good dataset.
data <- familiar:::test.create_good_data_set("survival")

# Train a simple linear GLM using the good dataset.
fam_model <- familiar:::test_train(data=data,
                                   cluster_method="none",
                                   imputation_method="simple",
                                   hyperparameter_list=list("sign_size"=familiar:::get_n_features(data)),
                                   learner="cox",
                                   stratification_method=stratification_methods,
                                   create_novelty_detector=FALSE)

for(stratification_method in stratification_methods){
  # Risk stratification.
  predictions_risk <- familiar::predict(object=fam_model,
                                        newdata=data,
                                        type="risk_stratification",
                                        stratification_method=stratification_method)
  
  testthat::test_that("Risk groups are formed.", {
    testthat::expect_equal(all(is.finite(predictions_risk$risk_group)), TRUE)
  })
}

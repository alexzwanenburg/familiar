# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()
testthat::skip_on_ci()

familiar_data_creation_unit_test <- function(outcome_type){
  
  if(outcome_type == "count"){
    learner <- "glm_poisson"
    
  } else if(outcome_type == "continuous"){
    learner <- "glm_gaussian"
    
  } else if(outcome_type == "binomial"){
    learner <- "glm_logistic"
    
  } else if(outcome_type == "multinomial"){
    learner <- "glm_multinomial"
    
  } else if(outcome_type == "survival"){
    learner <- "cox"
    
  } else {
    ..error_outcome_type_not_implemented(outcome_type)
  }
  
  
  # Create a dataset using the good dataset.
  data <- familiar:::test.create_good_data_set(outcome_type)
  
  # Train a simple linear GLM using the good dataset.
  fam_model <- suppressWarnings(familiar:::test_train(data=data,
                                                      cluster_method="none",
                                                      imputation_method="simple",
                                                      hyperparameter_list=list("sign_size"=familiar:::get_n_features(data)),
                                                      learner=learner,
                                                      time_max=1832))
  
  testthat::test_that(paste0("1. familiarData for the ", outcome_type, " outcome can be created from good data and the model created using the complete data set."), {
    # Test with small, but good dataset.
    fam_data <- suppressWarnings(familiar::as_familiar_data(object=fam_model,
                                                            data=familiar:::test.create_small_good_data_set(outcome_type=outcome_type),
                                                            estimation_type="point",
                                                            verbose=FALSE))
    
    # Test that an familiarDataObject is created.
    testthat::expect_s4_class(fam_data, "familiarData")
  })
  
  
  testthat::test_that(paste0("2. familiarData for the ", outcome_type, " outcome can be created from single-sample data and the model created using the complete data set."), {
    # Test with one-sample dataset.
    fam_data <- familiar::as_familiar_data(object=fam_model,
                                           data=familiar:::test.create_one_sample_data_set(outcome_type=outcome_type),
                                           estimation_type="point",
                                           verbose=FALSE)
    
    # Test that an familiarDataObject is created.
    testthat::expect_s4_class(fam_data, "familiarData")
  })
  
  
  testthat::test_that(paste0("3. familiarData for the ", outcome_type, " outcome can be created from empty data and the model created using the complete data set."), {
    # Test with empty dataset
    fam_data <- familiar::as_familiar_data(object=fam_model,
                                           data=familiar:::test.create_empty_data_set(outcome_type=outcome_type),
                                           estimation_type="point",
                                           verbose=FALSE)
    
    # Test that an familiarDataObject is created.
    testthat::expect_s4_class(fam_data, "familiarData")
  })
  
  
  testthat::test_that(paste0("4. familiarData for the ", outcome_type, " outcome can be created from the bad dataset and the model created using the complete data set."), {
    # Test with bad dataset
    fam_data <- suppressWarnings(familiar::as_familiar_data(object=fam_model,
                                                            data=familiar:::test.create_small_bad_data_set(outcome_type=outcome_type),
                                                            estimation_type="point",
                                                            verbose=FALSE))
    
    # Test that an familiarDataObject is created.
    testthat::expect_s4_class(fam_data, "familiarData")
  })
  
  
  # Create a dataset using the one-feature dataset.
  data <- familiar:::test.create_one_feature_data_set(outcome_type=outcome_type)
  
  # Train a simple linear GLM using the one feature dataset.
  fam_model <- suppressWarnings(familiar:::test_train(data=data,
                                                      cluster_method="none",
                                                      imputation_method="simple",
                                                      hyperparameter_list=list("sign_size"=familiar:::get_n_features(data)),
                                                      learner=learner,
                                                      time_max=1832))
  
  testthat::test_that(paste0("5. familiarData for the ", outcome_type, " outcome can be created from the one-feature dataset and the model created using the same data set."), {
    # Test with one-feature dataset.
    fam_data <- suppressWarnings(familiar::as_familiar_data(object=fam_model,
                                                            data=data,
                                                            estimation_type="point",
                                                            verbose=FALSE))
    
    # Test that an familiarDataObject is created.
    testthat::expect_s4_class(fam_data, "familiarData")
  })
  
  
  testthat::test_that(paste0("6. familiarData for the ", outcome_type, " outcome can be created from the one-feature, one-sample dataset and the model created using the one-feature data set."), {
    # Test with one-sample dataset
    fam_data <- familiar::as_familiar_data(object=fam_model,
                                           data=familiar:::test.create_one_feature_one_sample_data_set(outcome_type=outcome_type),
                                           estimation_type="point",
                                           verbose=FALSE)
    
    # Test that an familiarDataObject is created.
    testthat::expect_s4_class(fam_data, "familiarData")
  })
  
  
  # Create a dataset using the bad dataset.
  data <- familiar:::test.create_bad_data_set(outcome_type=outcome_type)
  
  # Train a simple linear GLM using the bad dataset. The model will fail to build
  fam_model <- suppressWarnings(familiar:::test_train(data=data,
                                                      cluster_method="none",
                                                      imputation_method="simple",
                                                      hyperparameter_list=list("sign_size"=familiar:::get_n_features(data)),
                                                      learner=learner,
                                                      time_max=1832))
  
  testthat::test_that(paste0("7. familiarData for the ", outcome_type, " outcome can be created from good data and the NULL model created using the bad data set."), {
    # Test with small, but good dataset.
    fam_data <- suppressWarnings(familiar::as_familiar_data(object=fam_model,
                                                            data=familiar:::test.create_small_good_data_set(outcome_type=outcome_type),
                                                            estimation_type="point",
                                                            verbose=FALSE))
    
    # Test that an familiarDataObject is created.
    testthat::expect_s4_class(fam_data, "familiarData")
  })
  
  
  testthat::test_that(paste0("8. familiarData for the ", outcome_type, " outcome can be created from single-sample data and the NULL model created using the bad data set."), {
    # Test with one-sample dataset.
    fam_data <- familiar::as_familiar_data(object=fam_model,
                                           data=familiar:::test.create_one_sample_data_set(outcome_type=outcome_type),
                                           estimation_type="point",
                                           verbose=FALSE)
    
    # Test that an familiarDataObject is created.
    testthat::expect_s4_class(fam_data, "familiarData")
  })
  
  
  testthat::test_that(paste0("9. familiarData for the ", outcome_type, " outcome can be created from empty data and the NULL model created using the bad data set."), {
    # Test with empty dataset
    fam_data <- familiar::as_familiar_data(object=fam_model,
                                           data=familiar:::test.create_empty_data_set(outcome_type=outcome_type),
                                           estimation_type="point",
                                           verbose=FALSE)
    
    # Test that an familiarDataObject is created.
    testthat::expect_s4_class(fam_data, "familiarData")
  })
}

familiar_data_creation_unit_test("count")
familiar_data_creation_unit_test("continuous")
familiar_data_creation_unit_test("binomial")
familiar_data_creation_unit_test("multinomial")
familiar_data_creation_unit_test("survival")

# Skip on CRAN as this test takes some time.
testthat::skip_on_cran()

outcome_type <- "survival"
n_numeric_features <- 1
imputation_method <- "lasso"

# Generic test -----------------------------------------------------------------
for(n_numeric_features in c(4, 3, 2, 1, 0)){
  
  data <- familiar:::test_create_synthetic_series_data(
    outcome_type=outcome_type,
    n_numeric=n_numeric_features)
  
  for(imputation_method in familiar:::.get_available_imputation_methods()){
    
    testthat::test_that(
      paste0(
        "Imputation is correctly performed using the ",
        imputation_method, " method and ",
        n_numeric_features, " numeric features."),
      {
        # Make a copy of the data.
        data_copy <- data.table::copy(data)
        
        # Create a list of featureInfo objects.
        feature_info_list <- familiar:::.get_feature_info_data(
          data=data_copy@data,
          file_paths=NULL,
          project_id=character(),
          outcome_type=outcome_type)[[1]]
        
        # Create imputation skeleton.
        feature_info_list <- familiar:::create_imputation_parameter_skeleton(
          feature_info_list=feature_info_list,
          imputation_method=imputation_method)
        
        # Add imputer parameters.
        feature_info_list <- familiar:::add_imputation_info(
          feature_info_list=feature_info_list,
          data=data_copy)
        
        # Assume that the data is pre-processed.
        data_copy@preprocessing_level <- "batch_normalisation"
        
        # Attempt to impute the data.
        data_copy <- familiar:::impute_features(
          data=data_copy,
          feature_info_list=feature_info_list)
        
        # Check that the data is not altered.
        testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)
        
        # Iterate over features.
        for(feature in familiar:::get_feature_columns(data_copy)){
          
          # Check that imputer parameters were set.
          testthat::expect_equal(
            familiar:::feature_info_complete(feature_info_list[[feature]]@imputation_parameters),
            TRUE)
          
          # Check that all feature data are valid.
          testthat::expect_equal(
            all(familiar:::is_valid_data(data_copy@data[[feature]])),
            TRUE)
        }
      }
    )
  }
}



# NA-instance test -------------------------------------------------------------
for(n_numeric_features in c(4, 3, 2, 1, 0)){
  
  data <- familiar:::test_create_synthetic_series_na_data(
    outcome_type=outcome_type,
    n_numeric=n_numeric_features)
  
  for(imputation_method in familiar:::.get_available_imputation_methods()){
    
    testthat::test_that(
      paste0(
        "Imputation is correctly performed using the ",
        imputation_method, " method and ",
        n_numeric_features, " numeric features for a dataset with some NA instances"),
      {
        # Make a copy of the data.
        data_copy <- data.table::copy(data)
        
        # Create a list of featureInfo objects.
        feature_info_list <- familiar:::.get_feature_info_data(
          data=data_copy@data,
          file_paths=NULL,
          project_id=character(),
          outcome_type=outcome_type)[[1]]
        
        # Create imputation skeleton.
        feature_info_list <- familiar:::create_imputation_parameter_skeleton(
          feature_info_list=feature_info_list,
          imputation_method=imputation_method)
        
        # Add imputer parameters.
        feature_info_list <- familiar:::add_imputation_info(
          feature_info_list=feature_info_list,
          data=data_copy)
        
        # Assume that the data is pre-processed.
        data_copy@preprocessing_level <- "batch_normalisation"
        
        # Attempt to impute the data.
        data_copy <- familiar:::impute_features(
          data=data_copy,
          feature_info_list=feature_info_list)
        
        # Iterate over features.
        for(feature in familiar:::get_feature_columns(data_copy)){
          
          # Check that imputer parameters were set.
          testthat::expect_equal(
            familiar:::feature_info_complete(feature_info_list[[feature]]@imputation_parameters),
            TRUE)
          
          # Check that all feature data are valid.
          testthat::expect_equal(
            all(familiar:::is_valid_data(data_copy@data[[feature]])),
            TRUE)
        }
      }
    )
  }
}



# NA random-value test ---------------------------------------------------------
for(n_numeric_features in c(4, 3, 2, 1, 0)){
  
  data <- familiar:::test_create_synthetic_series_random_na_data(
    outcome_type=outcome_type,
    n_numeric=n_numeric_features)
  
  for(imputation_method in familiar:::.get_available_imputation_methods()){
    
    testthat::test_that(
      paste0(
        "Imputation is correctly performed using the ",
        imputation_method, " method and ",
        n_numeric_features, " numeric features for a dataset with some NA datapoints."),
      {
        # Make a copy of the data.
        data_copy <- data.table::copy(data)
        
        # Create a list of featureInfo objects.
        feature_info_list <- familiar:::.get_feature_info_data(
          data=data_copy@data,
          file_paths=NULL,
          project_id=character(),
          outcome_type=outcome_type)[[1]]
        
        # Create imputation skeleton.
        feature_info_list <- familiar:::create_imputation_parameter_skeleton(
          feature_info_list=feature_info_list,
          imputation_method=imputation_method)
        
        # Add imputer parameters.
        feature_info_list <- familiar:::add_imputation_info(
          feature_info_list=feature_info_list,
          data=data_copy)
        
        # Assume that the data is pre-processed.
        data_copy@preprocessing_level <- "batch_normalisation"
        
        # Attempt to impute the data.
        data_copy <- familiar:::impute_features(
          data=data_copy,
          feature_info_list=feature_info_list)
        
        # Iterate over features.
        for(feature in familiar:::get_feature_columns(data_copy)){
          
          # Check that imputer parameters were set.
          testthat::expect_equal(
            familiar:::feature_info_complete(feature_info_list[[feature]]@imputation_parameters),
            TRUE)
          
          # Check that all feature data are valid.
          testthat::expect_equal(
            all(familiar:::is_valid_data(data_copy@data[[feature]])),
            TRUE)
        }
      }
    )
  }
}



# One feature NA test ----------------------------------------------------------
for(n_numeric_features in c(4, 3, 2, 1, 0)){
  
  data <- familiar:::test_create_synthetic_series_one_feature_all_na_data(
    outcome_type=outcome_type,
    n_numeric=n_numeric_features)
  
  for(imputation_method in familiar:::.get_available_imputation_methods()){
    
    testthat::test_that(
      paste0(
        "Imputation is correctly performed using the ",
        imputation_method, " method and ",
        n_numeric_features,  " numeric features for a dataset with one feature entirely NA."),
      {
        # Make a copy of the data.
        data_copy <- data.table::copy(data)
        
        # Create a list of featureInfo objects.
        feature_info_list <- familiar:::.get_feature_info_data(
          data=data_copy@data,
          file_paths=NULL,
          project_id=character(),
          outcome_type=outcome_type)[[1]]
        
        # Create imputation skeleton.
        feature_info_list <- familiar:::create_imputation_parameter_skeleton(
          feature_info_list=feature_info_list,
          imputation_method=imputation_method)
        
        # Add imputer parameters.
        feature_info_list <- familiar:::add_imputation_info(
          feature_info_list=feature_info_list,
          data=data_copy)
        
        # Assume that the data is pre-processed.
        data_copy@preprocessing_level <- "batch_normalisation"
        
        # Attempt to impute the data.
        data_copy <- familiar:::impute_features(
          data=data_copy,
          feature_info_list=feature_info_list)
        
        # Iterate over features.
        for(feature in familiar:::get_feature_columns(data_copy)){
          
          # Check that imputer parameters were set.
          testthat::expect_equal(familiar:::feature_info_complete(
            feature_info_list[[feature]]@imputation_parameters),
            TRUE)
          if(feature == "feature_2"){
            # Feature 2 is completely missing data.
            testthat::expect_equal(
              any(familiar:::is_valid_data(data_copy@data[[feature]])),
              FALSE)
            
            # Check that a None-class imputer is trained.
            testthat::expect_s4_class(
              feature_info_list[[feature]]@imputation_parameters,
              "featureInfoParametersImputationNone")
            
          } else {
            # Check that all feature data are valid.
            testthat::expect_equal(
              all(familiar:::is_valid_data(data_copy@data[[feature]])),
              TRUE)
          }
        }  
      }
    )
  }
}




# Invariant feature test -------------------------------------------------------
for(n_numeric_features in c(4, 3, 2, 1, 0)){
  
  data <- familiar:::test_create_synthetic_series_invariant_feature_data(
    outcome_type=outcome_type,
    n_numeric=n_numeric_features)
  
  for(imputation_method in familiar:::.get_available_imputation_methods()){
    
    testthat::test_that(
      paste0("Imputation is correctly performed using the ",
             imputation_method, " method and ",
             n_numeric_features,  " numeric features for a dataset with invariant features."),
      {
        # Make a copy of the data.
        data_copy <- data.table::copy(data)
        
        # Create a list of featureInfo objects.
        feature_info_list <- familiar:::.get_feature_info_data(
          data=data_copy@data,
          file_paths=NULL,
          project_id=character(),
          outcome_type=outcome_type)[[1]]
        
        # Create imputation skeleton.
        feature_info_list <- familiar:::create_imputation_parameter_skeleton(
          feature_info_list=feature_info_list,
          imputation_method=imputation_method)
        
        # Add imputer parameters.
        feature_info_list <- familiar:::add_imputation_info(
          feature_info_list=feature_info_list,
          data=data_copy)
        
        # Assume that the data is pre-processed.
        data_copy@preprocessing_level <- "batch_normalisation"
        
        # Attempt to impute the data.
        data_copy <- familiar:::impute_features(
          data=data_copy,
          feature_info_list=feature_info_list)
        
        # Iterate over features.
        for(feature in familiar:::get_feature_columns(data_copy)){
          
          # Check that imputer parameters were set.
          testthat::expect_equal(
            familiar:::feature_info_complete(feature_info_list[[feature]]@imputation_parameters),
            TRUE)
          
          # Check that all feature data are valid.
          testthat::expect_equal(
            all(familiar:::is_valid_data(data_copy@data[[feature]])),
            TRUE)
        } 
      })
  }
}



# One feature invariant test ---------------------------------------------------
for(n_numeric_features in c(4, 3, 2, 1, 0)){
  
  data <- familiar:::test_create_synthetic_series_one_feature_invariant_data(
    outcome_type=outcome_type,
    n_numeric=n_numeric_features)
  
  for(imputation_method in familiar:::.get_available_imputation_methods()){
    
    testthat::test_that(
      paste0(
        "Imputation is correctly performed using the ",
        imputation_method, " method and ",
        n_numeric_features,  " numeric features for a dataset with one invariant feature."),
      {
        # Make a copy of the data.
        data_copy <- data.table::copy(data)
        
        # Create a list of featureInfo objects.
        feature_info_list <- familiar:::.get_feature_info_data(
          data=data_copy@data,
          file_paths=NULL,
          project_id=character(),
          outcome_type=outcome_type)[[1]]
        
        # Create imputation skeleton.
        feature_info_list <- familiar:::create_imputation_parameter_skeleton(
          feature_info_list=feature_info_list,
          imputation_method=imputation_method)
        
        # Add imputer parameters.
        feature_info_list <- familiar:::add_imputation_info(
          feature_info_list=feature_info_list,
          data=data_copy)
        
        # Assume that the data is pre-processed.
        data_copy@preprocessing_level <- "batch_normalisation"
        
        # Attempt to impute the data.
        data_copy <- familiar:::impute_features(
          data=data_copy,
          feature_info_list=feature_info_list)
        
        # Iterate over features.
        for(feature in familiar:::get_feature_columns(data_copy)){
          
          # Check that imputer parameters were set.
          testthat::expect_equal(
            familiar:::feature_info_complete(feature_info_list[[feature]]@imputation_parameters),
            TRUE)
          
          # Check that all feature data are valid.
          testthat::expect_equal(
            all(familiar:::is_valid_data(data_copy@data[[feature]])),
            TRUE)
        } 
      }
    )
  }
}


# One-sample test --------------------------------------------------------------
for(n_numeric_features in c(4, 3, 2, 1, 0)){
  
  data <- familiar:::test_create_synthetic_series_one_sample_data(
    outcome_type=outcome_type,
    n_numeric=n_numeric_features)
  
  for(imputation_method in familiar:::.get_available_imputation_methods()){
    
    testthat::test_that(
      paste0(
        "Imputation is correctly performed using the ",
        imputation_method, " method and ",
        n_numeric_features,  " numeric features for a dataset with invariant features."),
      {
        # Make a copy of the data.
        data_copy <- data.table::copy(data)
        
        # Create a list of featureInfo objects.
        feature_info_list <- familiar:::.get_feature_info_data(
          data=data_copy@data,
          file_paths=NULL,
          project_id=character(),
          outcome_type=outcome_type)[[1]]
        
        # Create imputation skeleton.
        feature_info_list <- familiar:::create_imputation_parameter_skeleton(
          feature_info_list=feature_info_list,
          imputation_method=imputation_method)
        
        # Add imputer parameters.
        feature_info_list <- familiar:::add_imputation_info(
          feature_info_list=feature_info_list,
          data=data_copy)
        
        # Assume that the data is pre-processed.
        data_copy@preprocessing_level <- "batch_normalisation"
        
        # Attempt to impute the data.
        data_copy <- familiar:::impute_features(
          data=data_copy,
          feature_info_list=feature_info_list)
        
        # Iterate over features.
        for(feature in familiar:::get_feature_columns(data_copy)){
          
          # Check that imputer parameters were set.
          testthat::expect_equal(
            familiar:::feature_info_complete(feature_info_list[[feature]]@imputation_parameters),
            TRUE)
          
          # Check that all feature data are valid.
          testthat::expect_equal(
            all(familiar:::is_valid_data(data_copy@data[[feature]])),
            TRUE)
        } 
      })
  }
}


# One-feature test -------------------------------------------------------------
for(n_numeric_features in c(1, 0)){
  
  data <- familiar:::test_create_synthetic_series_random_na_data(
    outcome_type=outcome_type,
    n_numeric=n_numeric_features)
  
  # Select only the first feature.
  data@data <- data@data[, mget(c(familiar:::get_non_feature_columns(data),
                                  "feature_1"))]
  
  for(imputation_method in familiar:::.get_available_imputation_methods()){
    
    testthat::test_that(
      paste0(
        "Imputation is correctly performed using the ",
        imputation_method, " method and ",
        n_numeric_features, " numeric features for a dataset with a single feature and some NA datapoints."),
      {
        # Make a copy of the data.
        data_copy <- data.table::copy(data)
        
        # Create a list of featureInfo objects.
        feature_info_list <- familiar:::.get_feature_info_data(
          data=data_copy@data,
          file_paths=NULL,
          project_id=character(),
          outcome_type=outcome_type)[[1]]
        
        # Create imputation skeleton.
        feature_info_list <- familiar:::create_imputation_parameter_skeleton(
          feature_info_list=feature_info_list,
          imputation_method=imputation_method)
        
        # Add imputer parameters.
        feature_info_list <- familiar:::add_imputation_info(
          feature_info_list=feature_info_list,
          data=data_copy)
        
        # Assume that the data is pre-processed.
        data_copy@preprocessing_level <- "batch_normalisation"
        
        # Attempt to impute the data.
        data_copy <- familiar:::impute_features(
          data=data_copy,
          feature_info_list=feature_info_list)
        
        # Iterate over features.
        for(feature in familiar:::get_feature_columns(data_copy)){
          
          # Check that imputer parameters were set.
          testthat::expect_equal(
            familiar:::feature_info_complete(feature_info_list[[feature]]@imputation_parameters),
            TRUE)
          
          # Check that all feature data are valid.
          testthat::expect_equal(
            all(familiar:::is_valid_data(data_copy@data[[feature]])),
            TRUE)
        }
      }
    )
  }
}


# Collection test --------------------------------------------------------------
for(n_numeric_features in c(4, 3, 2, 1, 0)){
  
  data <- familiar:::test_create_synthetic_series_random_na_data(
    outcome_type=outcome_type,
    n_numeric=n_numeric_features)
  
  for(imputation_method in familiar:::.get_available_imputation_methods()){
    
    testthat::test_that(
      paste0(
        "Imputation is correctly performed using the ",
        imputation_method, " method and ",
        n_numeric_features, " numeric features for a dataset with some NA datapoints and a collection of imputers."),
      {
        # Make a copy of the data.
        data_copy <- data.table::copy(data)
        
        # Create a list of featureInfo objects.
        feature_info_list <- familiar:::.get_feature_info_data(
          data=data_copy@data,
          file_paths=NULL,
          project_id=character(),
          outcome_type=outcome_type)[[1]]
        
        # Create imputation skeleton.
        feature_info_list_1 <- familiar:::create_imputation_parameter_skeleton(
          feature_info_list=feature_info_list,
          imputation_method=imputation_method)
        
        # Add imputer parameters.
        feature_info_list_1 <- familiar:::add_imputation_info(
          feature_info_list=feature_info_list_1,
          data=data_copy)
        
        # Create imputation skeleton.
        feature_info_list_2 <- familiar:::create_imputation_parameter_skeleton(
          feature_info_list=feature_info_list,
          imputation_method=imputation_method)
        
        # Add imputer parameters.
        feature_info_list_2 <- familiar:::add_imputation_info(
          feature_info_list=feature_info_list_2,
          data=data_copy)
        
        # Create imputation skeleton.
        feature_info_list_3 <- familiar:::create_imputation_parameter_skeleton(
          feature_info_list=feature_info_list,
          imputation_method=imputation_method)
        
        # Add imputer parameters.
        feature_info_list_3 <- familiar:::add_imputation_info(
          feature_info_list=feature_info_list_3,
          data=data_copy)
        
        # Collect features.
        for(feature in familiar:::get_feature_columns(data_copy)){
          feature_info_list[[feature]]@imputation_parameters <- familiar:::..collect_and_aggregate_imputation_info(
            feature_info_list=list(
              feature_info_list_1[[feature]],
              feature_info_list_2[[feature]],
              feature_info_list_3[[feature]]),
            feature_type=feature_info_list[[feature]]@feature_type,
            feature_name=feature_info_list[[feature]]@name)$parameters
        }
        
        # Assume that the data is pre-processed.
        data_copy@preprocessing_level <- "batch_normalisation"
        
        # Attempt to impute the data.
        data_copy <- familiar:::impute_features(
          data=data_copy,
          feature_info_list=feature_info_list)
        
        # Iterate over features.
        for(feature in familiar:::get_feature_columns(data_copy)){
          
          # Check that imputer parameters were set.
          testthat::expect_equal(
            familiar:::feature_info_complete(feature_info_list[[feature]]@imputation_parameters),
            TRUE)
          
          # Check that all feature data are valid.
          testthat::expect_equal(
            all(familiar:::is_valid_data(data_copy@data[[feature]])),
            TRUE)
        }
      }
    )
  }
}

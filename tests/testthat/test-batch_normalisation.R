
outcome_type <- "survival"


for(n_numeric_features in c(3, 2, 1, 0)){
  
  data <- familiar:::test_create_synthetic_series_data(outcome_type=outcome_type, n_numeric=n_numeric_features)
  
  for(batch_normalisation_method in familiar:::.get_available_batch_normalisation_methods()){
    
    testthat::test_that(paste0("Batch normalisation is correctly performed using the ", batch_normalisation_method, " method and ", n_numeric_features, " numeric features."), {
      # Make a copy of the data.
      data_copy <- data.table::copy(data)
      
      # Create a list of featureInfo objects.
      feature_info_list <- familiar:::.get_feature_info_data(data=data_copy@data,
                                                             file_paths=NULL,
                                                             project_id=character(),
                                                             outcome_type=outcome_type)[[1]]
      
      # Combat requires global standardisation
      if(batch_normalisation_method %in% familiar:::.get_available_batch_normalisation_methods("combat")){
        feature_info_list <- familiar:::add_normalisation_parameters(feature_info_list=feature_info_list,
                                                                     data_obj=data_copy,
                                                                     normalisation_method="standardisation")
        
        # Act as if the data has been transformed.
        data_copy@preprocessing_level <- "transformation"
        
        # Perform a global normalisation.
        data_copy <- familiar:::normalise_features(data=data_copy,
                                                   feature_info_list=feature_info_list)
      }
      
      # Determine batch parameters.
      feature_info_list <- familiar:::add_batch_normalisation_parameters(feature_info_list=feature_info_list,
                                                                         data_obj=data_copy,
                                                                         batch_normalisation_method=batch_normalisation_method)
      
      # Assume that the data is pre-processed.
      data_copy@preprocessing_level <- "normalisation"
      
      # Attempt to batch normalise the data.
      data_copy <- familiar:::batch_normalise_features(data=data_copy, feature_info_list=feature_info_list)
      
      # Test whether the features are normalised (unless none).
      if(batch_normalisation_method == "none"){
        # Check that the data is not altered.
        testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)
        
      } else {
        for(feature in familiar:::get_feature_columns(data_copy)){
          
          # Determine if the feature is numeric.
          if(feature_info_list[[feature]]@feature_type == "numeric"){
            
            # Compute mean.
            x <- mean(data_copy@data[[feature]], na.rm=TRUE)
            
            if(batch_normalisation_method %in% c("normalisation", "normalisation_trim", "normalisation_winsor")){
              # Check that the feature is correctly centred around 0.
              testthat::expect_equal(x < 0.7 & x > 0.3, TRUE)
              
            } else {
              # Check that the feature is correctly centred around 0.
              testthat::expect_equal(x < 0.2 & x > -0.2, TRUE)
            }
          } else {
            # For categorical features test that the none batch normalisation
            # method is present.
            x <- all(sapply(feature_info_list[[feature]]@batch_normalisation_parameters, function(batch_param) (batch_param$norm_method)) == "none")
            
            testthat::expect_equal(x, TRUE)
          }
        }
      }
    })
  }
}

# Test what happens when all data for feature 1 in batch 1 are missing.

outcome_type <- "survival"

data <- familiar:::test_create_synthetic_series_data(outcome_type=outcome_type)
data@data[batch_id == "1", "feature_1":=NA_real_]

for(batch_normalisation_method in familiar:::.get_available_batch_normalisation_methods()){
  
  testthat::test_that(paste0("Batch normalisation is correctly performed using the ", batch_normalisation_method, " method, when data for feature_1 in batch 1 is missing."), {
    # Make a copy of the data.
    data_copy <- data.table::copy(data)
    
    # Create a list of featureInfo objects.
    feature_info_list <- familiar:::.get_feature_info_data(data=data_copy@data,
                                                           file_paths=NULL,
                                                           project_id=character(),
                                                           outcome_type=outcome_type)[[1]]
    
    # Combat requires global standardisation
    if(batch_normalisation_method %in% familiar:::.get_available_batch_normalisation_methods("combat")){
      feature_info_list <- familiar:::add_normalisation_parameters(feature_info_list=feature_info_list,
                                                                   data_obj=data_copy,
                                                                   normalisation_method="standardisation")
      
      # Act as if the data has been transformed.
      data_copy@preprocessing_level <- "transformation"
      
      # Perform a global normalisation.
      data_copy <- familiar:::normalise_features(data=data_copy,
                                                 feature_info_list=feature_info_list)
    }
    
    # Determine batch parameters.
    feature_info_list <- familiar:::add_batch_normalisation_parameters(feature_info_list=feature_info_list,
                                                                       data_obj=data_copy,
                                                                       batch_normalisation_method=batch_normalisation_method)
    
    
    # Assume that the data is pre-processed.
    data_copy@preprocessing_level <- "normalisation"
    
    # Attempt to batch normalise the data.
    data_copy <- familiar:::batch_normalise_features(data=data_copy, feature_info_list=feature_info_list)
    
    # Test whether the features are normalised (unless none).
    if(batch_normalisation_method == "none"){
      # Check that the data is not altered.
      testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)
      
    } else {
      for(feature in familiar:::get_feature_columns(data_copy)){
        
        # Compute mean.
        x <- mean(data_copy@data[[feature]], na.rm=TRUE)
        
        if(batch_normalisation_method %in% c("normalisation", "normalisation_trim", "normalisation_winsor")){
          # Check that the feature is correctly centred around 0.
          testthat::expect_equal(x < 0.7 & x > 0.3, TRUE)
          
        } else if(batch_normalisation_method %in% c("combat", "combat_np", "combat_non_parametric")){
          # Non-parametric ComBat has some issues here due to way it is
          # formulated. Here we simply test that x is finite.
          testthat::expect_equal(is.finite(x), TRUE)
           
        } else {
          # Check that the feature is correctly centred around 0.
          testthat::expect_equal(x < 0.2 & x > -0.2, TRUE)
        }
      }
    }
  })
}


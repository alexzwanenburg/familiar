
outcome_type <- "survival"

data <- familiar:::test_create_synthetic_series_data(outcome_type=outcome_type)

for(batch_normalisation_method in familiar:::.get_available_batch_normalisation_methods()){
  
  testthat::test_that(paste0("Batch normalisation is correctly performed using the ", batch_normalisation_method, " method."), {
    # Create a list of featureInfo objects.
    feature_info_list <- familiar:::.get_feature_info_data(data=data@data,
                                                           file_paths=NULL,
                                                           project_id=character(),
                                                           outcome_type=outcome_type)[[1]]
    
    
    feature_info_list <- familiar:::add_batch_normalisation_parameters(feature_info_list=feature_info_list,
                                                                       data_obj=data,
                                                                       batch_normalisation_method=batch_normalisation_method)
    
    # Make a copy of the data.
    data_copy <- data.table::copy(data)
    
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
        
        # Check that the feature is correctly centred around 0.
        testthat::expect_equal(x < 0.2 & x > -0.2, TRUE)
      }
    }
  })
}

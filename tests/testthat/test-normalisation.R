normalisation_test <- function(data_list, norm_method, expected_shift, expected_scale, expected_min, expected_max){
  
  # Iterate over the data sets.
  for(ii in seq_along(data_list)){
    
    # Get values
    x <- data_list[[ii]]
    
    # Acquire normalisation parameters
    norm_parameters <- familiar:::normalise.get_normalisation_parameters(x=x,
                                                                         norm_method=norm_method)
    
    # Test the shift and scale values.
    testthat::expect_equal(norm_parameters$norm_shift, expected_shift[ii])
    testthat::expect_equal(norm_parameters$norm_scale, expected_scale[ii])
    
    # Apply normalisation
    y <- familiar:::normalise.apply_normalisation(x=x, norm_param=norm_parameters)
    
    # Test that length is the same as input
    testthat::expect_equal(length(y), length(x))
    
    # Test that non-finite values are present and correctly positioned.
    testthat::expect_equal(is.finite(y), is.finite(x))
    
    # Remove non-finite values
    y <- y[is.finite(y)]
    
    # Check minimum value of normalised range, and its position (first).
    if(length(expected_min[[ii]]) > 0){
      testthat::expect_equal(min(y), expected_min[[ii]])
      testthat::expect_equal(min(y), head(y, n=1))
    } else {
      testthat::expect_equal(length(y), 0)
    }
    
    # Check maximum value of normalised range, and its position (last).
    if(length(expected_max[[ii]]) > 0){
      testthat::expect_equal(max(y), expected_max[[ii]])
      testthat::expect_equal(max(y), tail(y, n=1))
    } else {
      testthat::expect_equal(length(y), 0)
    }
  }
}


# Create data
good_data <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
bad_data <- c(-Inf, Inf, Inf, NA, NA, NA, -Inf, Inf, Inf, NA)
no_data <- numeric(0L)
realistic_data <- c(0, 1, 2, NA, 3, 4, 5, 6, 7, Inf, 8, 9)

# Combine to list
data_list <- list("good" = good_data,
                  "bad" = bad_data,
                  "empty" = no_data,
                  "realistic" = realistic_data)

#### No normalisation ###############################
testthat::test_that("None normalisation is correctly performed", {
  
  expected_shift <- c(0.0, 0.0, 0.0, 0.0)
  expected_scale <- c(1.0, 1.0, 1.0, 1.0)
  expected_min <- list(0.0, numeric(0), numeric(0), 0.0)
  expected_max <- list(9.0, numeric(0), numeric(0), 9.0)
  
  normalisation_test(data_list=data_list,
                     norm_method="none",
                     expected_shift=expected_shift,
                     expected_scale=expected_scale,
                     expected_min=expected_min,
                     expected_max=expected_max)
  
})

#### Normalisation ##################################
testthat::test_that("Normalisation is correctly performed", {
  
  expected_shift <- c(0.0, 0.0, 0.0, 0.0)
  expected_scale <- c(9.0, 1.0, 1.0, 9.0)
  expected_min <- list(0.0, numeric(0), numeric(0), 0.0)
  expected_max <- list(1.0, numeric(0), numeric(0), 1.0)
  
  normalisation_test(data_list=data_list,
                     norm_method="normalisation",
                     expected_shift=expected_shift,
                     expected_scale=expected_scale,
                     expected_min=expected_min,
                     expected_max=expected_max)
  
})


#### Normalisation (trimmed) ##################################
testthat::test_that("Normalisation (trimmed) is correctly performed", {
  
  expected_shift <- c(1.0, 0.0, 0.0, 1.0)
  expected_scale <- c(7.0, 1.0, 1.0, 7.0)
  expected_min <- list(-1/7, numeric(0), numeric(0), -1/7)
  expected_max <- list(8/7, numeric(0), numeric(0), 8/7)
  
  normalisation_test(data_list=data_list,
                     norm_method="normalisation_trim",
                     expected_shift=expected_shift,
                     expected_scale=expected_scale,
                     expected_min=expected_min,
                     expected_max=expected_max)
  
})

#### Normalisation (winsorised) ##############################
testthat::test_that("Normalisation (winsorised) is correctly performed", {
  
  expected_shift <- c(0.45, 0.0, 0.0, 0.45)
  expected_scale <- c(8.1, 1.0, 1.0, 8.1)
  expected_min <- list(-0.45/8.1, numeric(0), numeric(0), -0.45/8.1)
  expected_max <- list(8.55/8.1, numeric(0), numeric(0), 8.55/8.1)
  
  normalisation_test(data_list=data_list,
                     norm_method="normalisation_winsor",
                     expected_shift=expected_shift,
                     expected_scale=expected_scale,
                     expected_min=expected_min,
                     expected_max=expected_max)
  
})


#### Standardisation ##############################
testthat::test_that("Standardisation is correctly performed", {
  
  # Set x.
  x <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  
  # Mean
  mu <- mean(x)
  
  # Standard deviation (without Bessel correction)
  sigma <- sqrt(sum((x-mu)^2) / length(x))
  
  expected_shift <- c(mu, 0.0, 0.0, mu)
  expected_scale <- c(sigma, 1.0, 1.0, sigma)
  expected_min <- list(-mu/sigma, numeric(0), numeric(0), -mu/sigma)
  expected_max <- list((9-mu)/sigma, numeric(0), numeric(0), (9-mu)/sigma)
  
  normalisation_test(data_list=data_list,
                     norm_method="standardisation",
                     expected_shift=expected_shift,
                     expected_scale=expected_scale,
                     expected_min=expected_min,
                     expected_max=expected_max)
  
})


#### Standardisation (trimmed) ##############################
testthat::test_that("Standardisation (trimmed) is correctly performed", {
  
  # Set x.
  x <- c(1, 2, 3, 4, 5, 6, 7, 8)
  
  # Mean
  mu <- mean(x)
  
  # Standard deviation (without Bessel correction)
  sigma <- sqrt(sum((x-mu)^2) / length(x))
  
  expected_shift <- c(mu, 0.0, 0.0, mu)
  expected_scale <- c(sigma, 1.0, 1.0, sigma)
  expected_min <- list(-mu/sigma, numeric(0), numeric(0), -mu/sigma)
  expected_max <- list((9-mu)/sigma, numeric(0), numeric(0), (9-mu)/sigma)
  
  normalisation_test(data_list=data_list,
                     norm_method="standardisation_trim",
                     expected_shift=expected_shift,
                     expected_scale=expected_scale,
                     expected_min=expected_min,
                     expected_max=expected_max)
})


#### Standardisation (winsorised) ##############################
testthat::test_that("Standardisation (winsorised) is correctly performed", {
  
  # Set x.
  x <- c(0.45, 1, 2, 3, 4, 5, 6, 7, 8, 8.55)
  
  # Mean
  mu <- mean(x)
  
  # Standard deviation (without Bessel correction)
  sigma <- sqrt(sum((x-mu)^2) / length(x))
  
  expected_shift <- c(mu, 0.0, 0.0, mu)
  expected_scale <- c(sigma, 1.0, 1.0, sigma)
  expected_min <- list(-mu/sigma, numeric(0), numeric(0), -mu/sigma)
  expected_max <- list((9-mu)/sigma, numeric(0), numeric(0), (9-mu)/sigma)
  
  normalisation_test(data_list=data_list,
                     norm_method="standardisation_winsor",
                     expected_shift=expected_shift,
                     expected_scale=expected_scale,
                     expected_min=expected_min,
                     expected_max=expected_max)
})


#### Mean centering #########################################
testthat::test_that("Mean centering is correctly performed", {
  
  expected_shift <- c(4.5, 0.0, 0.0, 4.5)
  expected_scale <- c(1.0, 1.0, 1.0, 1.0)
  expected_min <- list(-4.5, numeric(0), numeric(0), -4.5)
  expected_max <- list(4.5, numeric(0), numeric(0), 4.5)
  
  normalisation_test(data_list=data_list,
                     norm_method="mean_centering",
                     expected_shift=expected_shift,
                     expected_scale=expected_scale,
                     expected_min=expected_min,
                     expected_max=expected_max)
})


#### Quantile #########################################
testthat::test_that("Quantile normalisation is correctly performed", {
  
  expected_shift <- c(4.5, 0.0, 0.0, 4.5)  # Median will be between 4.0 and 5.0: 4.5
  expected_scale <- c(4.5, 1.0, 1.0, 4.5)  # IQR
  expected_min <- list(-1.0, numeric(0), numeric(0), -1.0)
  expected_max <- list(1.0, numeric(0), numeric(0), 1.0)
  
  normalisation_test(data_list=data_list,
                     norm_method="quantile",
                     expected_shift=expected_shift,
                     expected_scale=expected_scale,
                     expected_min=expected_min,
                     expected_max=expected_max)
})


##### Generic test #############################################################
outcome_type <- "survival"

for(n_numeric_features in c(4, 3, 2, 1, 0)){
  
  data <- familiar:::test_create_synthetic_series_data(outcome_type=outcome_type, n_numeric=n_numeric_features)
  
  for(normalisation_method in familiar:::.get_available_normalisation_methods()){
    
    testthat::test_that(paste0("Normalisation is correctly performed using the ", normalisation_method, " method and ", n_numeric_features, " numeric features."), {
      # Make a copy of the data.
      data_copy <- data.table::copy(data)
      
      # Create a list of featureInfo objects.
      feature_info_list <- familiar:::.get_feature_info_data(data=data_copy@data,
                                                             file_paths=NULL,
                                                             project_id=character(),
                                                             outcome_type=outcome_type)[[1]]
      
      # Update the feature info list.
      feature_info_list <- familiar:::add_normalisation_parameters(feature_info_list=feature_info_list,
                                                                   data_obj=data_copy,
                                                                   normalisation_method=normalisation_method)
        
      # Act as if the data has been transformed.
      data_copy@preprocessing_level <- "transformation"
        
      # Perform a normalisation.
      data_copy <- familiar:::normalise_features(data=data_copy,
                                                 feature_info_list=feature_info_list)
      
      
      # Test whether the features are normalised (unless none).
      if(normalisation_method == "none"){
        # Check that the data is not altered.
        testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)
        
      } else {
        for(feature in familiar:::get_feature_columns(data_copy)){
          
          # Determine if the feature is numeric.
          if(feature_info_list[[feature]]@feature_type == "numeric"){
            
            # Compute mean.
            x <- mean(data_copy@data[[feature]], na.rm=TRUE)
            
            if(normalisation_method %in% c("normalisation", "normalisation_trim", "normalisation_winsor")){
              # Check that the feature is correctly centred around 0.
              testthat::expect_equal(x < 0.7 & x > 0.3, TRUE)
              
            } else {
              # Check that the feature is correctly centred around 0.
              testthat::expect_equal(x < 0.2 & x > -0.2, TRUE)
            }
          } else {
            # For categorical features test that the none batch normalisation
            # method is present.
            x <- feature_info_list[[feature]]@normalisation_parameters$norm_method == "none"
            
            testthat::expect_equal(x, TRUE)
          }
        }
      }
    })
  }
}


#### NA-value test #############################################################
for(n_numeric_features in c(4, 3, 2, 1, 0)){
  
  data <- familiar:::test_create_synthetic_series_na_data(outcome_type=outcome_type, n_numeric=n_numeric_features)
  
  for(normalisation_method in familiar:::.get_available_normalisation_methods()){
    
    testthat::test_that(paste0("Normalisation is correctly performed using the ",
                               normalisation_method, " method and ",
                               n_numeric_features, " numeric features for a dataset with some NA data."), {
      # Make a copy of the data.
      data_copy <- data.table::copy(data)
      
      # Create a list of featureInfo objects.
      feature_info_list <- familiar:::.get_feature_info_data(data=data_copy@data,
                                                             file_paths=NULL,
                                                             project_id=character(),
                                                             outcome_type=outcome_type)[[1]]
      
      # Update the feature info list.
      feature_info_list <- familiar:::add_normalisation_parameters(feature_info_list=feature_info_list,
                                                                   data_obj=data_copy,
                                                                   normalisation_method=normalisation_method)
      
      # Act as if the data has been transformed.
      data_copy@preprocessing_level <- "transformation"
      
      # Perform a normalisation.
      data_copy <- familiar:::normalise_features(data=data_copy,
                                                 feature_info_list=feature_info_list)
      
      
      # Test whether the features are normalised (unless none).
      if(normalisation_method == "none"){
        # Check that the data is not altered.
        testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)
        
      } else {
        for(feature in familiar:::get_feature_columns(data_copy)){
          
          # Determine if the feature is numeric.
          if(feature_info_list[[feature]]@feature_type == "numeric"){
            
            # Compute mean.
            x <- mean(data_copy@data[[feature]], na.rm=TRUE)
            
            if(normalisation_method %in% c("normalisation", "normalisation_trim", "normalisation_winsor")){
              # Check that the feature is correctly centred around 0.
              testthat::expect_equal(x < 0.7 & x > 0.3, TRUE)
              
            } else {
              # Check that the feature is correctly centred around 0.
              testthat::expect_equal(x < 0.2 & x > -0.2, TRUE)
            }
          } else {
            # For categorical features test that the none batch normalisation
            # method is present.
            x <- feature_info_list[[feature]]@normalisation_parameters$norm_method == "none"
            
            testthat::expect_equal(x, TRUE)
          }
        }
      }
    })
  }
}


#### One feature NA test ###########################################################
for(n_numeric_features in c(4, 3, 2, 1, 0)){
  
  data <- familiar:::test_create_synthetic_series_one_feature_all_na_data(outcome_type=outcome_type, n_numeric=n_numeric_features)
  
  for(normalisation_method in familiar:::.get_available_normalisation_methods()){
    
    testthat::test_that(paste0("Normalisation is correctly performed using the ",
                               normalisation_method, " method and ",
                               n_numeric_features, " numeric features for a dataset with one feature completely NA."), {
                                 # Make a copy of the data.
                                 data_copy <- data.table::copy(data)
                                 
                                 # Create a list of featureInfo objects.
                                 feature_info_list <- familiar:::.get_feature_info_data(data=data_copy@data,
                                                                                        file_paths=NULL,
                                                                                        project_id=character(),
                                                                                        outcome_type=outcome_type)[[1]]
                                 
                                 # Update the feature info list.
                                 feature_info_list <- familiar:::add_normalisation_parameters(feature_info_list=feature_info_list,
                                                                                              data_obj=data_copy,
                                                                                              normalisation_method=normalisation_method)
                                 
                                 # Act as if the data has been transformed.
                                 data_copy@preprocessing_level <- "transformation"
                                 
                                 # Perform a normalisation.
                                 data_copy <- familiar:::normalise_features(data=data_copy,
                                                                            feature_info_list=feature_info_list)
                                 
                                 
                                 # Test whether the features are normalised (unless none).
                                 if(normalisation_method == "none"){
                                   # Check that the data is not altered.
                                   testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)
                                   
                                 } else {
                                   for(feature in familiar:::get_feature_columns(data_copy)){
                                     
                                     # Determine if the feature is numeric.
                                     if(feature_info_list[[feature]]@feature_type == "numeric"){
                                       
                                       if(feature == "feature_2"){
                                         # Expect that shift and scale parameters are 0 and 1, respectively.
                                         x_shift <- feature_info_list[[feature]]@normalisation_parameters$norm_shift
                                         x_scale <- feature_info_list[[feature]]@normalisation_parameters$norm_scale
                                         
                                         testthat::expect_equal(x_shift, 0.0)
                                         testthat::expect_equal(x_scale, 1.0)
                                         
                                       } else {
                                         # Compute mean.
                                         x <- mean(data_copy@data[[feature]], na.rm=TRUE)
                                         
                                         if(normalisation_method %in% c("normalisation", "normalisation_trim", "normalisation_winsor")){
                                           # Check that the feature is correctly centred around 0.
                                           testthat::expect_equal(x < 0.7 & x > 0.3, TRUE)
                                           
                                         } else {
                                           # Check that the feature is correctly centred around 0.
                                           testthat::expect_equal(x < 0.2 & x > -0.2, TRUE)
                                         }
                                       }
                                       
                                     } else {
                                       # For categorical features test that the none normalisation
                                       # method is present.
                                       x <- feature_info_list[[feature]]@normalisation_parameters$norm_method == "none"
                                       
                                       testthat::expect_equal(x, TRUE)
                                     }
                                   }
                                 }
                               })
  }
}



#### Invariant feature test ###########################################################
for(n_numeric_features in c(4, 3, 2, 1, 0)){
  
  data <- familiar:::test_create_synthetic_series_invariant_feature_data(outcome_type=outcome_type, n_numeric=n_numeric_features)
  
  for(normalisation_method in familiar:::.get_available_normalisation_methods()){
    
    testthat::test_that(paste0("Normalisation is correctly performed using the ",
                               normalisation_method, " method and ",
                               n_numeric_features, " numeric features for a dataset with invariant features."), {
                                 # Make a copy of the data.
                                 data_copy <- data.table::copy(data)
                                 
                                 # Create a list of featureInfo objects.
                                 feature_info_list <- familiar:::.get_feature_info_data(data=data_copy@data,
                                                                                        file_paths=NULL,
                                                                                        project_id=character(),
                                                                                        outcome_type=outcome_type)[[1]]
                                 
                                 # Update the feature info list.
                                 feature_info_list <- familiar:::add_normalisation_parameters(feature_info_list=feature_info_list,
                                                                                              data_obj=data_copy,
                                                                                              normalisation_method=normalisation_method)
                                 
                                 # Act as if the data has been transformed.
                                 data_copy@preprocessing_level <- "transformation"
                                 
                                 # Perform a normalisation.
                                 data_copy <- familiar:::normalise_features(data=data_copy,
                                                                            feature_info_list=feature_info_list)
                                 
                                 
                                 # Test whether the features are normalised (unless none).
                                 if(normalisation_method == "none"){
                                   # Check that the data is not altered.
                                   testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)
                                   
                                 } else {
                                   for(feature in familiar:::get_feature_columns(data_copy)){
                                     
                                     # Determine if the feature is numeric.
                                     if(feature_info_list[[feature]]@feature_type == "numeric"){
                                       
                                       # Expect that shift and scale parameters are 0 and 1, respectively.
                                       x_shift <- feature_info_list[[feature]]@normalisation_parameters$norm_shift
                                       x_scale <- feature_info_list[[feature]]@normalisation_parameters$norm_scale
                                       
                                       testthat::expect_equal(x_shift, 0.0)
                                       testthat::expect_equal(x_scale, 1.0)
                                       
                                     } else {
                                       # For categorical features test that the none batch normalisation
                                       # method is present.
                                       x <- feature_info_list[[feature]]@normalisation_parameters$norm_method == "none"
                                       
                                       testthat::expect_equal(x, TRUE)
                                     }
                                   }
                                 }
                               })
  }
}



#### One feature invariant test ###########################################################
for(n_numeric_features in c(4, 3, 2, 1, 0)){
  
  data <- familiar:::test_create_synthetic_series_one_feature_invariant_data(outcome_type=outcome_type, n_numeric=n_numeric_features)
  
  for(normalisation_method in familiar:::.get_available_normalisation_methods()){
    
    testthat::test_that(paste0("Normalisation is correctly performed using the ",
                               normalisation_method, " method and ",
                               n_numeric_features, " numeric features for a dataset with one invariant feature."), {
                                 # Make a copy of the data.
                                 data_copy <- data.table::copy(data)
                                 
                                 # Create a list of featureInfo objects.
                                 feature_info_list <- familiar:::.get_feature_info_data(data=data_copy@data,
                                                                                        file_paths=NULL,
                                                                                        project_id=character(),
                                                                                        outcome_type=outcome_type)[[1]]
                                 
                                 # Update the feature info list.
                                 feature_info_list <- familiar:::add_normalisation_parameters(feature_info_list=feature_info_list,
                                                                                              data_obj=data_copy,
                                                                                              normalisation_method=normalisation_method)
                                 
                                 # Act as if the data has been transformed.
                                 data_copy@preprocessing_level <- "transformation"
                                 
                                 # Perform a normalisation.
                                 data_copy <- familiar:::normalise_features(data=data_copy,
                                                                            feature_info_list=feature_info_list)
                                 
                                 
                                 # Test whether the features are normalised (unless none).
                                 if(normalisation_method == "none"){
                                   # Check that the data is not altered.
                                   testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)
                                   
                                 } else {
                                   for(feature in familiar:::get_feature_columns(data_copy)){
                                     
                                     # Determine if the feature is numeric.
                                     if(feature_info_list[[feature]]@feature_type == "numeric"){
                                       
                                       if(feature == "feature_2"){
                                         # Expect that shift and scale parameters are 0 and 1, respectively.
                                         x_shift <- feature_info_list[[feature]]@normalisation_parameters$norm_shift
                                         x_scale <- feature_info_list[[feature]]@normalisation_parameters$norm_scale
                                         
                                         testthat::expect_equal(x_shift, 0.0)
                                         testthat::expect_equal(x_scale, 1.0)
                                         
                                       } else {
                                         # Compute mean.
                                         x <- mean(data_copy@data[[feature]], na.rm=TRUE)
                                         
                                         if(normalisation_method %in% c("normalisation", "normalisation_trim", "normalisation_winsor")){
                                           # Check that the feature is correctly centred around 0.
                                           testthat::expect_equal(x < 0.7 & x > 0.3, TRUE)
                                           
                                         } else {
                                           # Check that the feature is correctly centred around 0.
                                           testthat::expect_equal(x < 0.2 & x > -0.2, TRUE)
                                         }
                                       }
                                       
                                     } else {
                                       # For categorical features test that the none normalisation
                                       # method is present.
                                       x <- feature_info_list[[feature]]@normalisation_parameters$norm_method == "none"
                                       
                                       testthat::expect_equal(x, TRUE)
                                     }
                                   }
                                 }
                               })
  }
}



#### One-sample test ###########################################################
for(n_numeric_features in c(4, 3, 2, 1, 0)){
  
  data <- familiar:::test_create_synthetic_series_one_sample_data(outcome_type=outcome_type, n_numeric=n_numeric_features)
  
  for(normalisation_method in familiar:::.get_available_normalisation_methods()){
    
    testthat::test_that(paste0("Normalisation is correctly performed using the ",
                               normalisation_method, " method and ",
                               n_numeric_features, " numeric features for a dataset with one sample."), {
                                 # Make a copy of the data.
                                 data_copy <- data.table::copy(data)
                                 
                                 # Create a list of featureInfo objects.
                                 feature_info_list <- familiar:::.get_feature_info_data(data=data_copy@data,
                                                                                        file_paths=NULL,
                                                                                        project_id=character(),
                                                                                        outcome_type=outcome_type)[[1]]
                                 
                                 # Update the feature info list.
                                 feature_info_list <- familiar:::add_normalisation_parameters(feature_info_list=feature_info_list,
                                                                                              data_obj=data_copy,
                                                                                              normalisation_method=normalisation_method)
                                 
                                 # Act as if the data has been transformed.
                                 data_copy@preprocessing_level <- "transformation"
                                 
                                 # Perform a normalisation.
                                 data_copy <- familiar:::normalise_features(data=data_copy,
                                                                            feature_info_list=feature_info_list)
                                 
                                 
                                 # Test whether the features are normalised (unless none).
                                 if(normalisation_method == "none"){
                                   # Check that the data is not altered.
                                   testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)
                                   
                                 } else {
                                   for(feature in familiar:::get_feature_columns(data_copy)){
                                     
                                     # Determine if the feature is numeric.
                                     if(feature_info_list[[feature]]@feature_type == "numeric"){
                                       
                                       # Expect that shift and scale parameters are 0 and 1, respectively.
                                       x_shift <- feature_info_list[[feature]]@normalisation_parameters$norm_shift
                                       x_scale <- feature_info_list[[feature]]@normalisation_parameters$norm_scale
                                       
                                       testthat::expect_equal(x_shift, 0.0)
                                       testthat::expect_equal(x_scale, 1.0)
                                       
                                     } else {
                                       # For categorical features test that the none batch normalisation
                                       # method is present.
                                       x <- feature_info_list[[feature]]@normalisation_parameters$norm_method == "none"
                                       
                                       testthat::expect_equal(x, TRUE)
                                     }
                                   }
                                 }
                               })
  }
}

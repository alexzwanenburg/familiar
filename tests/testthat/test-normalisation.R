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

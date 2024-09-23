testthat::skip_on_cran()

normalisation_test <- function(
    data_list,
    norm_method,
    expected_shift = NULL,
    expected_scale = NULL,
    expected_min,
    expected_max,
    expected_none) {
  # Iterate over the data sets.
  for (ii in seq_along(data_list)) {
    # Get values
    x <- data_list[[ii]]

    # Create skeleton
    object <- familiar:::..create_normalisation_parameter_skeleton(
      feature_name = "test",
      method = norm_method)

    # Acquire normalisation parameters.
    object <- familiar:::add_feature_info_parameters(object, data = x)

    if (expected_none[ii]) {
      testthat::expect_s4_class(object, "featureInfoParametersNormalisationNone")
      
    } else {
      # Test the shift and scale values.
      if (!is.null(expected_scale)) {
        # Scale may be absent for mean centering and similar methods.
        testthat::expect_equal(object@scale, expected_scale[ii])
      }

      # Test shift parameter
      testthat::expect_equal(object@shift, expected_shift[ii])
    }

    # Apply normalisation
    y <- familiar:::apply_feature_info_parameters(
      object = object,
      data = x)

    # Test that length is the same as input
    testthat::expect_equal(length(y), length(x))

    # Test that non-finite values are present and correctly positioned.
    testthat::expect_equal(is.finite(y), is.finite(x))

    # Remove non-finite values
    y <- y[is.finite(y)]

    # Check minimum value of normalised range, and its position (first).
    if (length(expected_min[[ii]]) > 0) {
      testthat::expect_equal(min(y), expected_min[[ii]])
      testthat::expect_equal(min(y), head(y, n = 1))
    } else {
      testthat::expect_equal(length(y), 0)
    }

    # Check maximum value of normalised range, and its position (last).
    if (length(expected_max[[ii]]) > 0) {
      testthat::expect_equal(max(y), expected_max[[ii]])
      testthat::expect_equal(max(y), tail(y, n = 1))
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
data_list <- list(
  "good" = good_data,
  "bad" = bad_data,
  "empty" = no_data,
  "realistic" = realistic_data)

# No normalisation -------------------------------------------------------------
testthat::test_that("None normalisation is correctly performed", {
  expected_min <- list(0.0, numeric(0), numeric(0), 0.0)
  expected_max <- list(9.0, numeric(0), numeric(0), 9.0)
  expected_none <- c(TRUE, TRUE, TRUE, TRUE)

  normalisation_test(
    data_list = data_list,
    norm_method = "none",
    expected_min = expected_min,
    expected_max = expected_max,
    expected_none = expected_none)
})

# Normalisation ----------------------------------------------------------------
testthat::test_that("Normalisation is correctly performed", {
  expected_shift <- c(0.0, NA_real_, NA_real_, 0.0)
  expected_scale <- c(9.0, NA_real_, NA_real_, 9.0)
  expected_min <- list(0.0, numeric(0), numeric(0), 0.0)
  expected_max <- list(1.0, numeric(0), numeric(0), 1.0)
  expected_none <- c(FALSE, TRUE, TRUE, FALSE)

  normalisation_test(
    data_list = data_list,
    norm_method = "normalisation",
    expected_shift = expected_shift,
    expected_scale = expected_scale,
    expected_min = expected_min,
    expected_max = expected_max,
    expected_none = expected_none)
})

# Normalisation (trimmed) ------------------------------------------------------
testthat::test_that("Normalisation (trimmed) is correctly performed", {
  expected_shift <- c(1.0, NA_real_, NA_real_, 1.0)
  expected_scale <- c(7.0, NA_real_, NA_real_, 7.0)
  expected_min <- list(-1 / 7, numeric(0), numeric(0), -1 / 7)
  expected_max <- list(8 / 7, numeric(0), numeric(0), 8 / 7)
  expected_none <- c(FALSE, TRUE, TRUE, FALSE)

  normalisation_test(
    data_list = data_list,
    norm_method = "normalisation_trim",
    expected_shift = expected_shift,
    expected_scale = expected_scale,
    expected_min = expected_min,
    expected_max = expected_max,
    expected_none = expected_none)
})

# Normalisation (winsorised) ---------------------------------------------------
testthat::test_that("Normalisation (winsorised) is correctly performed", {
  expected_shift <- c(0.45, NA_real_, NA_real_, 0.45)
  expected_scale <- c(8.1, NA_real_, NA_real_, 8.1)
  expected_min <- list(-0.45 / 8.1, numeric(0), numeric(0), -0.45 / 8.1)
  expected_max <- list(8.55 / 8.1, numeric(0), numeric(0), 8.55 / 8.1)
  expected_none <- c(FALSE, TRUE, TRUE, FALSE)

  normalisation_test(
    data_list = data_list,
    norm_method = "normalisation_winsor",
    expected_shift = expected_shift,
    expected_scale = expected_scale,
    expected_min = expected_min,
    expected_max = expected_max,
    expected_none = expected_none)
})


# Standardisation --------------------------------------------------------------
testthat::test_that("Standardisation is correctly performed", {
  # Set x.
  x <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

  # Mean
  mu <- mean(x)

  # Standard deviation (without Bessel correction)
  sigma <- sqrt(sum((x - mu)^2) / length(x))

  expected_shift <- c(mu, NA_real_, NA_real_, mu)
  expected_scale <- c(sigma, NA_real_, NA_real_, sigma)
  expected_min <- list(-mu / sigma, numeric(0), numeric(0), -mu / sigma)
  expected_max <- list((9 - mu) / sigma, numeric(0), numeric(0), (9 - mu) / sigma)
  expected_none <- c(FALSE, TRUE, TRUE, FALSE)

  normalisation_test(
    data_list = data_list,
    norm_method = "standardisation",
    expected_shift = expected_shift,
    expected_scale = expected_scale,
    expected_min = expected_min,
    expected_max = expected_max,
    expected_none = expected_none)
})


# Standardisation (trimmed) ----------------------------------------------------
testthat::test_that("Standardisation (trimmed) is correctly performed", {
  # Set x.
  x <- c(1, 2, 3, 4, 5, 6, 7, 8)

  # Mean
  mu <- mean(x)

  # Standard deviation (without Bessel correction)
  sigma <- sqrt(sum((x - mu)^2) / length(x))

  expected_shift <- c(mu, NA_real_, NA_real_, mu)
  expected_scale <- c(sigma, NA_real_, NA_real_, sigma)
  expected_min <- list(-mu / sigma, numeric(0), numeric(0), -mu / sigma)
  expected_max <- list((9 - mu) / sigma, numeric(0), numeric(0), (9 - mu) / sigma)
  expected_none <- c(FALSE, TRUE, TRUE, FALSE)

  normalisation_test(
    data_list = data_list,
    norm_method = "standardisation_trim",
    expected_shift = expected_shift,
    expected_scale = expected_scale,
    expected_min = expected_min,
    expected_max = expected_max,
    expected_none = expected_none)
})


# Standardisation (winsorised) -------------------------------------------------
testthat::test_that("Standardisation (winsorised) is correctly performed", {
  # Set x.
  x <- c(0.45, 1, 2, 3, 4, 5, 6, 7, 8, 8.55)

  # Mean
  mu <- mean(x)

  # Standard deviation (without Bessel correction)
  sigma <- sqrt(sum((x - mu)^2) / length(x))

  expected_shift <- c(mu, NA_real_, NA_real_, mu)
  expected_scale <- c(sigma, NA_real_, NA_real_, sigma)
  expected_min <- list(-mu / sigma, numeric(0), numeric(0), -mu / sigma)
  expected_max <- list((9 - mu) / sigma, numeric(0), numeric(0), (9 - mu) / sigma)
  expected_none <- c(FALSE, TRUE, TRUE, FALSE)

  normalisation_test(
    data_list = data_list,
    norm_method = "standardisation_winsor",
    expected_shift = expected_shift,
    expected_scale = expected_scale,
    expected_min = expected_min,
    expected_max = expected_max,
    expected_none = expected_none)
})


# Mean centering ---------------------------------------------------------------
testthat::test_that("Mean centering is correctly performed", {
  expected_shift <- c(4.5, NA_real_, NA_real_, 4.5)
  expected_min <- list(-4.5, numeric(0), numeric(0), -4.5)
  expected_max <- list(4.5, numeric(0), numeric(0), 4.5)
  expected_none <- c(FALSE, TRUE, TRUE, FALSE)

  normalisation_test(
    data_list = data_list,
    norm_method = "mean_centering",
    expected_shift = expected_shift,
    expected_min = expected_min,
    expected_max = expected_max,
    expected_none = expected_none)
})


# Quantile ---------------------------------------------------------------------
testthat::test_that("Quantile normalisation is correctly performed", {
  expected_shift <- c(4.5, NA_real_, NA_real_, 4.5) # Median will be between 4.0 and 5.0: 4.5
  expected_scale <- c(4.5, NA_real_, NA_real_, 4.5) # IQR
  expected_min <- list(-1.0, numeric(0), numeric(0), -1.0)
  expected_max <- list(1.0, numeric(0), numeric(0), 1.0)
  expected_none <- c(FALSE, TRUE, TRUE, FALSE)

  normalisation_test(
    data_list = data_list,
    norm_method = "quantile",
    expected_shift = expected_shift,
    expected_scale = expected_scale,
    expected_min = expected_min,
    expected_max = expected_max,
    expected_none = expected_none)
})



# Generic test -----------------------------------------------------------------
outcome_type <- "survival"

for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_data(
    outcome_type = outcome_type, 
    n_numeric = n_numeric_features)

  for (normalisation_method in familiar:::.get_available_normalisation_methods()) {
    testthat::test_that(paste0(
      "Normalisation is correctly performed using the ",
      normalisation_method, " method and ", n_numeric_features,
      " numeric features."
    ), {
      # Make a copy of the data.
      data_copy <- data.table::copy(data)

      # Create a list of featureInfo objects.
      feature_info_list <- familiar:::.get_feature_info_data(
        data = data_copy@data,
        file_paths = NULL,
        project_id = character(),
        outcome_type = outcome_type
      )[[1]]

      # Add skeletons.
      feature_info_list <- familiar:::create_normalisation_parameter_skeleton(
        feature_info_list = feature_info_list,
        normalisation_method = normalisation_method)

      # Update the feature info list.
      feature_info_list <- familiar:::add_normalisation_parameters(
        feature_info_list = feature_info_list,
        data = data_copy)

      # Act as if the data has been transformed.
      data_copy@preprocessing_level <- "transformation"

      # Perform a normalisation.
      data_copy <- familiar:::normalise_features(
        data = data_copy,
        feature_info_list = feature_info_list)

      # Test whether the features are normalised (unless none).
      if (normalisation_method == "none") {
        # Check that the data is not altered.
        testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)
        
      } else {
        for (feature in familiar:::get_feature_columns(data_copy)) {
          # Determine if the feature is numeric.
          if (feature_info_list[[feature]]@feature_type == "numeric") {
            # Compute mean.
            x <- mean(data_copy@data[[feature]], na.rm = TRUE)

            if (normalisation_method %in% 
                c("normalisation", "normalisation_trim", "normalisation_winsor")) {
              # Check that the feature is correctly centred around 0.
              testthat::expect_equal(x < 0.7 & x > 0.3, TRUE)
              
            } else {
              # Check that the feature is correctly centred around 0.
              testthat::expect_equal(x < 0.2 & x > -0.2, TRUE)
            }
          } else {
            # For categorical features test that the object class is
            # featureInfoParametersNormalisationNone.
            testthat::expect_s4_class(
              feature_info_list[[feature]]@normalisation_parameters,
              "featureInfoParametersNormalisationNone")
          }
        }
      }

      # Assert that inverting the transformation produces the original dataset.
      data_restored <- familiar:::normalise_features(
        data = data_copy,
        feature_info_list = feature_info_list,
        invert = TRUE)

      # Iterate over features and compare. They should be equal.
      for (feature in familiar:::get_feature_columns(data_restored)) {
        # Expect that values are similar to a tolerance.
        testthat::expect_equal(data_restored@data[[feature]], data@data[[feature]])
      }

      # Assert that aggregating the normalisation parameters works as expected.
      aggr_normalisation_parameters <- familiar:::..collect_and_aggregate_normalisation_info(
        feature_info_list = feature_info_list,
        instance_mask = rep_len(TRUE, length(feature_info_list)),
        feature_name = "test"
      )

      if (n_numeric_features > 0 && normalisation_method != "none") {
        # Expect that the selected normalisation method matches the selected
        # method.
        testthat::expect_equal(
          aggr_normalisation_parameters$parameters@method, 
          normalisation_method, 
          ignore_attr = TRUE)

        # Expect that the shift and scale parameters are finite.
        testthat::expect_equal(is.finite(aggr_normalisation_parameters$parameters@shift), TRUE)
        if (is(aggr_normalisation_parameters$parameters,
               "featureInfoParametersNormalisationShiftScale")) {
          testthat::expect_equal(is.finite(aggr_normalisation_parameters$parameters@scale), TRUE)
        }

        # Expect that instance mask is equal to the number of numeric features.
        testthat::expect_equal(sum(aggr_normalisation_parameters$instance_mask) > 0, TRUE)
        testthat::expect_lte(
          sum(aggr_normalisation_parameters$instance_mask),
          n_numeric_features)
        
      } else {
        # Expect that the selected normalisation method matches the selected
        # method.
        testthat::expect_equal(aggr_normalisation_parameters$parameters@method, "none")

        # Expect that instance mask is equal to the number of numeric features.
        testthat::expect_equal(
          sum(aggr_normalisation_parameters$instance_mask),
          familiar:::get_n_features(data_copy))
      }
    })
  }
}


# NA-value test ----------------------------------------------------------------
for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_na_data(
    outcome_type = outcome_type,
    n_numeric = n_numeric_features)

  for (normalisation_method in familiar:::.get_available_normalisation_methods()) {
    testthat::test_that(paste0(
      "Normalisation is correctly performed using the ",
      normalisation_method, " method and ",
      n_numeric_features, " numeric features for a dataset with some NA data."
    ), {
      # Make a copy of the data.
      data_copy <- data.table::copy(data)

      # Create a list of featureInfo objects.
      feature_info_list <- familiar:::.get_feature_info_data(
        data = data_copy@data,
        file_paths = NULL,
        project_id = character(),
        outcome_type = outcome_type
      )[[1]]

      # Add skeletons.
      feature_info_list <- familiar:::create_normalisation_parameter_skeleton(
        feature_info_list = feature_info_list,
        normalisation_method = normalisation_method)

      # Update the feature info list.
      feature_info_list <- familiar:::add_normalisation_parameters(
        feature_info_list = feature_info_list,
        data = data_copy)

      # Act as if the data has been transformed.
      data_copy@preprocessing_level <- "transformation"

      # Perform a normalisation.
      data_copy <- familiar:::normalise_features(
        data = data_copy,
        feature_info_list = feature_info_list)

      # Test whether the features are normalised (unless none).
      if (normalisation_method == "none") {
        # Check that the data is not altered.
        testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)
        
      } else {
        for (feature in familiar:::get_feature_columns(data_copy)) {
          # Determine if the feature is numeric.
          if (feature_info_list[[feature]]@feature_type == "numeric") {
            # Compute mean.
            x <- mean(data_copy@data[[feature]], na.rm = TRUE)

            if (normalisation_method %in% 
                c("normalisation", "normalisation_trim", "normalisation_winsor")) {
              # Check that the feature is correctly centred around 0.
              testthat::expect_equal(x < 0.7 & x > 0.3, TRUE)
            } else {
              # Check that the feature is correctly centred around 0.
              testthat::expect_equal(x < 0.2 & x > -0.2, TRUE)
            }
          } else {
            # Assert that for the categorical features, the none method is used.
            testthat::expect_s4_class(
              feature_info_list[[feature]]@normalisation_parameters,
              "featureInfoParametersNormalisationNone")
          }
        }
      }
    })
  }
}


# One feature NA test ----------------------------------------------------------
for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_one_feature_all_na_data(
    outcome_type = outcome_type, 
    n_numeric = n_numeric_features)

  for (normalisation_method in familiar:::.get_available_normalisation_methods()) {
    testthat::test_that(paste0(
      "Normalisation is correctly performed using the ",
      normalisation_method, " method and ",
      n_numeric_features, " numeric features for a dataset with one feature completely NA."
    ), {
      # Make a copy of the data.
      data_copy <- data.table::copy(data)

      # Create a list of featureInfo objects.
      feature_info_list <- familiar:::.get_feature_info_data(
        data = data_copy@data,
        file_paths = NULL,
        project_id = character(),
        outcome_type = outcome_type
      )[[1]]

      # Add skeletons.
      feature_info_list <- familiar:::create_normalisation_parameter_skeleton(
        feature_info_list = feature_info_list,
        normalisation_method = normalisation_method)

      # Update the feature info list.
      feature_info_list <- familiar:::add_normalisation_parameters(
        feature_info_list = feature_info_list,
        data = data_copy)

      # Act as if the data has been transformed.
      data_copy@preprocessing_level <- "transformation"

      # Perform a normalisation.
      data_copy <- familiar:::normalise_features(
        data = data_copy,
        feature_info_list = feature_info_list)

      # Test whether the features are normalised (unless none).
      if (normalisation_method == "none") {
        # Check that the data is not altered.
        testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)
        
      } else {
        for (feature in familiar:::get_feature_columns(data_copy)) {
          # Determine if the feature is numeric.
          if (feature_info_list[[feature]]@feature_type == "numeric") {
            if (feature == "feature_2") {
              # Assert that for the NA feature, the none method is used.
              testthat::expect_s4_class(
                feature_info_list[[feature]]@normalisation_parameters,
                "featureInfoParametersNormalisationNone")
              
            } else {
              # Compute mean.
              x <- mean(data_copy@data[[feature]], na.rm = TRUE)

              if (normalisation_method %in% 
                  c("normalisation", "normalisation_trim", "normalisation_winsor")) {
                # Check that the feature is correctly centred around 0.
                testthat::expect_equal(x < 0.7 & x > 0.3, TRUE)
              } else {
                # Check that the feature is correctly centred around 0.
                testthat::expect_equal(x < 0.2 & x > -0.2, TRUE)
              }
            }
          } else {
            # Assert that for the categorical features, the none method is used.
            testthat::expect_s4_class(
              feature_info_list[[feature]]@normalisation_parameters,
              "featureInfoParametersNormalisationNone")
          }
        }
      }
    })
  }
}



# Invariant feature test -------------------------------------------------------
for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_invariant_feature_data(
    outcome_type = outcome_type, 
    n_numeric = n_numeric_features)

  for (normalisation_method in familiar:::.get_available_normalisation_methods()) {
    testthat::test_that(paste0(
      "Normalisation is correctly performed using the ",
      normalisation_method, " method and ",
      n_numeric_features, " numeric features for a dataset with invariant features."
    ), {
      # Make a copy of the data.
      data_copy <- data.table::copy(data)

      # Create a list of featureInfo objects.
      feature_info_list <- familiar:::.get_feature_info_data(
        data = data_copy@data,
        file_paths = NULL,
        project_id = character(),
        outcome_type = outcome_type
      )[[1]]

      # Add skeletons.
      feature_info_list <- familiar:::create_normalisation_parameter_skeleton(
        feature_info_list = feature_info_list,
        normalisation_method = normalisation_method)

      # Update the feature info list.
      feature_info_list <- familiar:::add_normalisation_parameters(
        feature_info_list = feature_info_list,
        data = data_copy)

      # Act as if the data has been transformed.
      data_copy@preprocessing_level <- "transformation"

      # Perform a normalisation.
      data_copy <- familiar:::normalise_features(
        data = data_copy,
        feature_info_list = feature_info_list)

      # Check that the data is not altered.
      testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)

      for (feature in familiar:::get_feature_columns(data_copy)) {
        # Assert that the none method is used.
        testthat::expect_s4_class(
          feature_info_list[[feature]]@normalisation_parameters,
          "featureInfoParametersNormalisationNone")
      }
    })
  }
}



# One feature invariant test ---------------------------------------------------
for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_one_feature_invariant_data(
    outcome_type = outcome_type, 
    n_numeric = n_numeric_features)

  for (normalisation_method in familiar:::.get_available_normalisation_methods()) {
    testthat::test_that(paste0(
      "Normalisation is correctly performed using the ",
      normalisation_method, " method and ",
      n_numeric_features, " numeric features for a dataset with one invariant feature."
    ), {
      # Make a copy of the data.
      data_copy <- data.table::copy(data)

      # Create a list of featureInfo objects.
      feature_info_list <- familiar:::.get_feature_info_data(
        data = data_copy@data,
        file_paths = NULL,
        project_id = character(),
        outcome_type = outcome_type
      )[[1]]

      # Add skeletons.
      feature_info_list <- familiar:::create_normalisation_parameter_skeleton(
        feature_info_list = feature_info_list,
        normalisation_method = normalisation_method)

      # Update the feature info list.
      feature_info_list <- familiar:::add_normalisation_parameters(
        feature_info_list = feature_info_list,
        data = data_copy)

      # Act as if the data has been transformed.
      data_copy@preprocessing_level <- "transformation"

      # Perform a normalisation.
      data_copy <- familiar:::normalise_features(
        data = data_copy,
        feature_info_list = feature_info_list)

      # Test whether the features are normalised (unless none).
      if (normalisation_method == "none") {
        # Check that the data is not altered.
        testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)

      } else {
        for (feature in familiar:::get_feature_columns(data_copy)) {
          # Determine if the feature is numeric.
          if (feature_info_list[[feature]]@feature_type == "numeric") {
            if (feature == "feature_2") {
              # Assert that for the NA feature, the none method is used.
              testthat::expect_s4_class(
                feature_info_list[[feature]]@normalisation_parameters,
                  "featureInfoParametersNormalisationNone")

            } else {
              # Compute mean.
              x <- mean(data_copy@data[[feature]], na.rm = TRUE)

              if (normalisation_method %in% 
                  c("normalisation", "normalisation_trim", "normalisation_winsor")) {
                # Check that the feature is correctly centred around 0.
                testthat::expect_equal(x < 0.7 & x > 0.3, TRUE)
              } else {
                # Check that the feature is correctly centred around 0.
                testthat::expect_equal(x < 0.2 & x > -0.2, TRUE)
              }
            }
          } else {
            # Assert that for the categorical features, the none method is used.
            testthat::expect_s4_class(
              feature_info_list[[feature]]@normalisation_parameters,
              "featureInfoParametersNormalisationNone")
          }
        }
      }
    })
  }
}



# One-sample test --------------------------------------------------------------
for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_one_sample_data(
    outcome_type = outcome_type, 
    n_numeric = n_numeric_features)

  for (normalisation_method in familiar:::.get_available_normalisation_methods()) {
    testthat::test_that(paste0(
      "Normalisation is correctly performed using the ",
      normalisation_method, " method and ",
      n_numeric_features, " numeric features for a dataset with one sample."
    ), {
      # Make a copy of the data.
      data_copy <- data.table::copy(data)

      # Create a list of featureInfo objects.
      feature_info_list <- familiar:::.get_feature_info_data(
        data = data_copy@data,
        file_paths = NULL,
        project_id = character(),
        outcome_type = outcome_type
      )[[1]]

      # Add skeletons.
      feature_info_list <- familiar:::create_normalisation_parameter_skeleton(
        feature_info_list = feature_info_list,
        normalisation_method = normalisation_method)

      # Update the feature info list.
      feature_info_list <- familiar:::add_normalisation_parameters(
        feature_info_list = feature_info_list,
        data = data_copy)

      # Act as if the data has been transformed.
      data_copy@preprocessing_level <- "transformation"

      # Perform a normalisation.
      data_copy <- familiar:::normalise_features(
        data = data_copy,
        feature_info_list = feature_info_list)

      # Check that the data is not altered.
      testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)

      for (feature in familiar:::get_feature_columns(data_copy)) {
        # Assert that the none method is used.
        testthat::expect_s4_class(
          feature_info_list[[feature]]@normalisation_parameters,
          "featureInfoParametersNormalisationNone")
      }
    })
  }
}

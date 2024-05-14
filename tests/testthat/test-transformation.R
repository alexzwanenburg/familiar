# Generic test -----------------------------------------------------------------
outcome_type <- "survival"

for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_data(
    outcome_type = outcome_type,
    n_numeric = n_numeric_features)

  for (transformation_method in familiar:::.get_available_transformation_methods()) {
    testthat::test_that(
      paste0(
        "Transformation is correctly performed using the ", transformation_method,
        " method and ", n_numeric_features, " numeric features."
      ),
      {
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
        feature_info_list <- familiar:::create_transformation_parameter_skeleton(
          feature_info_list = feature_info_list,
          transformation_method = transformation_method)

        # Update the feature info list.
        feature_info_list <- familiar:::add_transformation_parameters(
          feature_info_list = feature_info_list,
          data = data_copy)

        # Perform the transformation.
        data_copy <- familiar:::transform_features(
          data = data_copy,
          feature_info_list = feature_info_list)

        # Test whether the features are transformed (unless none).
        if (transformation_method == "none") {
          # Check that the data is not altered.
          testthat::expect_true(data.table::fsetequal(data_copy@data, data@data))
          
        } else {
          for (feature in familiar:::get_feature_columns(data_copy)) {
            # Determine if the feature is numeric.
            if (feature_info_list[[feature]]@feature_type == "numeric") {
              # Expect that values are finite.
              testthat::expect_true(is.finite(power.transform::get_lambda(
                feature_info_list[[feature]]@transformation_parameters@transformer)))
              
            } else {
              # For categorical features test that the object method is "none".
              testthat::expect_equal(
                feature_info_list[[feature]]@transformation_parameters@method,
                "none")
            }
          }
        }

        # Assert that inverting the transformation produces the original
        # dataset.
        data_restored <- familiar:::transform_features(
          data = data_copy,
          feature_info_list = feature_info_list,
          invert = TRUE)

        # Iterate over features and compare. They should be equal
        for (feature in familiar:::get_feature_columns(data_restored)) {
          # Expect that values are similar to a tolerance.
          testthat::expect_equal(data_restored@data[[feature]], data@data[[feature]])
        }

        # Assert that aggregating the transformation parameters works as expected.
        aggr_transform_parameters <- familiar:::..collect_and_aggregate_transformation_info(
          feature_info_list = feature_info_list,
          instance_mask = rep_len(TRUE, length(feature_info_list)),
          feature_name = "test")

        if (n_numeric_features > 0 && transformation_method != "none") {
          # Expect that the selected transformation method matches the selected
          # method -- Note that the method attribute is only the transformation
          # method.
          testthat::expect_true(startsWith(
            x = transformation_method,
            prefix = aggr_transform_parameters$parameters@method))

          # Expect that the lambda is finite.
          testthat::expect_true(is.finite(power.transform::get_lambda(
            aggr_transform_parameters$parameters@transformer)))

          # Expect that instance mask is equal to one.
          testthat::expect_equal(sum(aggr_transform_parameters$instance_mask), 1)
          
        } else {
          # Expect that the selected transformation method matches the selected
          # method.
          testthat::expect_equal(aggr_transform_parameters$parameters@method, "none")

          # Expect that instance mask is equal to the number of numeric
          # features.
          testthat::expect_equal(
            sum(aggr_transform_parameters$instance_mask),
            familiar:::get_n_features(data_copy))
        }
      }
    )
  }
}

# Don't perform any further tests on CRAN due to running time.
testthat::skip_on_cran()
testthat::skip_on_ci()

# NA-value test ----------------------------------------------------------------
for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_na_data(
    outcome_type = outcome_type,
    n_numeric = n_numeric_features)

  for (transformation_method in familiar:::.get_available_transformation_methods()) {
    testthat::test_that(
      paste0(
        "Transformation is correctly performed using the ",
        transformation_method, " method and ",
        n_numeric_features, " numeric features for a dataset with some NA data."
      ),
      {
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
        feature_info_list <- familiar:::create_transformation_parameter_skeleton(
          feature_info_list = feature_info_list,
          transformation_method = transformation_method)

        # Update the feature info list.
        feature_info_list <- familiar:::add_transformation_parameters(
          feature_info_list = feature_info_list,
          data = data_copy)

        # Perform a transformation.
        data_copy <- familiar:::transform_features(
          data = data_copy,
          feature_info_list = feature_info_list)

        # Test whether the features are transformed (unless none).
        if (transformation_method == "none") {
          # Check that the data is not altered.
          testthat::expect_true(data.table::fsetequal(data_copy@data, data@data))
        } else {
          for (feature in familiar:::get_feature_columns(data_copy)) {
            # Determine if the feature is numeric.
            if (feature_info_list[[feature]]@feature_type == "numeric") {
              # Expect that values are finite.
              testthat::expect_true(is.finite(power.transform::get_lambda(
                feature_info_list[[feature]]@transformation_parameters@transformer)))
              
            } else {
              # For categorical features test that the none method is used.
              testthat::expect_equal(
                feature_info_list[[feature]]@transformation_parameters@method,
                "none")
            }
          }
        }
      }
    )
  }
}

# One feature NA test ----------------------------------------------------------
for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_one_feature_all_na_data(
    outcome_type = outcome_type,
    n_numeric = n_numeric_features)

  for (transformation_method in familiar:::.get_available_transformation_methods()) {
    testthat::test_that(
      paste0(
        "Transformation is correctly performed using the ",
        transformation_method, " method and ",
        n_numeric_features, " numeric features for a dataset with one feature completely NA."
      ),
      {
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
        feature_info_list <- familiar:::create_transformation_parameter_skeleton(
          feature_info_list = feature_info_list,
          transformation_method = transformation_method)

        # Update the feature info list.
        feature_info_list <- familiar:::add_transformation_parameters(
          feature_info_list = feature_info_list,
          data = data_copy)

        # Perform a normalisation.
        data_copy <- familiar:::transform_features(
          data = data_copy,
          feature_info_list = feature_info_list)

        # Test whether the features are transformed (unless none).
        if (transformation_method == "none") {
          # Check that the data is not altered.
          testthat::expect_true(data.table::fsetequal(data_copy@data, data@data))
        } else {
          for (feature in familiar:::get_feature_columns(data_copy)) {
            # Determine if the feature is numeric.
            if (feature_info_list[[feature]]@feature_type == "numeric") {
              if (feature == "feature_2") {
                # Expect that for the NA feature, the none method is used.
                testthat::expect_equal(
                  feature_info_list[[feature]]@transformation_parameters@method,
                  "none")
                
              } else {
                # Check that the lambda is finite.
                testthat::expect_true(is.finite(power.transform::get_lambda(
                  feature_info_list[[feature]]@transformation_parameters@transformer)))
              }
            } else {
              # For categorical features test that the none method is used.
              testthat::expect_equal(
                feature_info_list[[feature]]@transformation_parameters@method,
                "none")
            }
          }
        }
      }
    )
  }
}

# Invariant feature test -------------------------------------------------------
for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_invariant_feature_data(
    outcome_type = outcome_type,
    n_numeric = n_numeric_features)

  for (transformation_method in familiar:::.get_available_transformation_methods()) {
    testthat::test_that(
      paste0(
        "Transformation is correctly performed using the ",
        transformation_method, " method and ",
        n_numeric_features, " numeric features for a dataset with invariant features."
      ),
      {
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
        feature_info_list <- familiar:::create_transformation_parameter_skeleton(
          feature_info_list = feature_info_list,
          transformation_method = transformation_method)

        # Update the feature info list.
        feature_info_list <- familiar:::add_transformation_parameters(
          feature_info_list = feature_info_list,
          data = data_copy)

        # Perform the transformation.
        data_copy <- familiar:::transform_features(
          data = data_copy,
          feature_info_list = feature_info_list)

        # Test whether the features are transformed (unless none).
        if (transformation_method == "none") {
          # Check that the data is not altered.
          testthat::expect_true(data.table::fsetequal(data_copy@data, data@data))
          
        } else {
          for (feature in familiar:::get_feature_columns(data_copy)) {
            # Determine if the feature is numeric.
            if (feature_info_list[[feature]]@feature_type == "numeric") {
              # Check the none method is used for invariant features.
              testthat::expect_equal(
                feature_info_list[[feature]]@transformation_parameters@method,
                "none")
            } else {
              # For categorical features test that the none method is used.
              testthat::expect_equal(
                feature_info_list[[feature]]@transformation_parameters@method,
                "none")
            }
          }
        }
      }
    )
  }
}

# One feature invariant test ---------------------------------------------------
for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_one_feature_invariant_data(
    outcome_type = outcome_type,
    n_numeric = n_numeric_features)

  for (transformation_method in familiar:::.get_available_transformation_methods()) {
    testthat::test_that(
      paste0(
        "Transformation is correctly performed using the ",
        transformation_method, " method and ",
        n_numeric_features, " numeric features for a dataset with one invariant feature."
      ),
      {
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
        feature_info_list <- familiar:::create_transformation_parameter_skeleton(
          feature_info_list = feature_info_list,
          transformation_method = transformation_method)

        # Update the feature info list.
        feature_info_list <- familiar:::add_transformation_parameters(
          feature_info_list = feature_info_list,
          data = data_copy)

        # Perform the transformation.
        data_copy <- familiar:::transform_features(
          data = data_copy,
          feature_info_list = feature_info_list)

        # Test whether the features are transformed (unless none).
        if (transformation_method == "none") {
          # Check that the data is not altered.
          testthat::expect_true(data.table::fsetequal(data_copy@data, data@data))
          
        } else {
          for (feature in familiar:::get_feature_columns(data_copy)) {
            # Determine if the feature is numeric.
            if (feature_info_list[[feature]]@feature_type == "numeric") {
              if (feature == "feature_2") {
                # Expect that the invariant feature uses the none method.
                testthat::expect_equal(
                  feature_info_list[[feature]]@transformation_parameters@method,
                  "none")
              } else {
                # Check that the lambda is finite.
                testthat::expect_true(is.finite(power.transform::get_lambda(
                  feature_info_list[[feature]]@transformation_parameters@transformer)))
              }
            } else {
              # For categorical features test that the none method is used.
              testthat::expect_equal(
                feature_info_list[[feature]]@transformation_parameters@method,
                "none")
            }
          }
        }
      }
    )
  }
}

# One-sample test --------------------------------------------------------------
for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_one_sample_data(
    outcome_type = outcome_type,
    n_numeric = n_numeric_features)

  for (transformation_method in familiar:::.get_available_transformation_methods()) {
    testthat::test_that(
      paste0(
        "Transformation is correctly performed using the ",
        transformation_method, " method and ",
        n_numeric_features, " numeric features for a dataset with one sample."
      ),
      {
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
        feature_info_list <- familiar:::create_transformation_parameter_skeleton(
          feature_info_list = feature_info_list,
          transformation_method = transformation_method)

        # Update the feature info list.
        feature_info_list <- familiar:::add_transformation_parameters(
          feature_info_list = feature_info_list,
          data = data_copy)

        # Perform the transformation.
        data_copy <- familiar:::transform_features(
          data = data_copy,
          feature_info_list = feature_info_list)

        # Test whether the features are transformed (unless none).
        if (transformation_method == "none") {
          # Check that the data is not altered.
          testthat::expect_true(data.table::fsetequal(data_copy@data, data@data))
          
        } else {
          for (feature in familiar:::get_feature_columns(data_copy)) {
            # Determine if the feature is numeric.
            if (feature_info_list[[feature]]@feature_type == "numeric") {
              # Expect that the none-method is used for one-sample data.
              testthat::expect_equal(
                feature_info_list[[feature]]@transformation_parameters@method,
                "none")
            } else {
              # For categorical features test that the none method is used.
              testthat::expect_equal(
                feature_info_list[[feature]]@transformation_parameters@method,
                "none")
            }
          }
        }
      }
    )
  }
}

# Mix of methods ---------------------------------------------------------------

data <- familiar:::test_create_synthetic_series_data(
  outcome_type = outcome_type,
  n_numeric = 4)

testthat::test_that(
  paste0("Transformation is correctly performed when mixing transformation methods."),
  {
    # Make a copy of the data.
    data_copy <- data.table::copy(data)

    # Create a list of featureInfo objects.
    feature_info_list <- familiar:::.get_feature_info_data(
      data = data_copy@data,
      file_paths = NULL,
      project_id = character(),
      outcome_type = outcome_type
    )[[1]]

    # Add skeletons for features 1 and 2.
    feature_info_list <- familiar:::create_transformation_parameter_skeleton(
      feature_info_list = feature_info_list,
      transformation_method = "box_cox",
      feature_names = c("feature_1", "feature_2"))

    # Check that this only updates features 1 and 2.
    testthat::expect_equal(
      feature_info_list$feature_1@transformation_parameters@fitting_parameters$method,
      "box_cox")
    testthat::expect_equal(
      feature_info_list$feature_2@transformation_parameters@fitting_parameters$method,
      "box_cox")
    testthat::expect_equal(feature_info_list$feature_3@transformation_parameters, NULL)
    testthat::expect_equal(feature_info_list$feature_4@transformation_parameters, NULL)

    # Add skeleton for feature 3.
    feature_info_list <- familiar:::create_transformation_parameter_skeleton(
      feature_info_list = feature_info_list,
      transformation_method = "yeo_johnson",
      feature_names = c("feature_3"))

    # Check that this only updates feature 3.
    testthat::expect_equal(
      feature_info_list$feature_1@transformation_parameters@fitting_parameters$method,
      "box_cox")
    testthat::expect_equal(
      feature_info_list$feature_2@transformation_parameters@fitting_parameters$method,
      "box_cox")
    testthat::expect_equal(
      feature_info_list$feature_3@transformation_parameters@fitting_parameters$method,
      "yeo_johnson")
    testthat::expect_equal(
      feature_info_list$feature_4@transformation_parameters,
      NULL)

    # Add skeletons for feature 4.
    feature_info_list <- familiar:::create_transformation_parameter_skeleton(
      feature_info_list = feature_info_list,
      transformation_method = "none",
      feature_names = c("feature_4"))
    
    # Check that this only updates feature 4.
    testthat::expect_equal(
      feature_info_list$feature_1@transformation_parameters@fitting_parameters$method,
      "box_cox")
    testthat::expect_equal(
      feature_info_list$feature_2@transformation_parameters@fitting_parameters$method,
      "box_cox")
    testthat::expect_equal(
      feature_info_list$feature_3@transformation_parameters@fitting_parameters$method,
      "yeo_johnson")
    testthat::expect_equal(
      feature_info_list$feature_4@transformation_parameters@fitting_parameters$method,
      "none")

    # Update the feature info list.
    feature_info_list <- familiar:::add_transformation_parameters(
      feature_info_list = feature_info_list,
      data = data_copy)

    # Perform the transformation.
    data_copy <- familiar:::transform_features(
      data = data_copy,
      feature_info_list = feature_info_list)

    # Assert that inverting the transformation produces the original dataset.
    data_restored <- familiar:::transform_features(
      data = data_copy,
      feature_info_list = feature_info_list,
      invert = TRUE)

    # Iterate over features and compare. They should be equal
    for (feature in familiar:::get_feature_columns(data_restored)) {
      # Expect that values are similar to a tolerance.
      testthat::expect_equal(data_restored@data[[feature]], data@data[[feature]])
    }
  }
)

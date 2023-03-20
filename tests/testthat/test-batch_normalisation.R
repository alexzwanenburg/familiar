# Skip on CRAN as this test takes about a 30 seconds.
testthat::skip_on_cran()

outcome_type <- "survival"

# Generic test -----------------------------------------------------------------
for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_data(
    outcome_type = outcome_type,
    n_numeric = n_numeric_features)

  for (batch_normalisation_method in familiar:::.get_available_batch_normalisation_methods()) {
    testthat::test_that(paste0(
      "Batch normalisation is correctly performed using the ",
      batch_normalisation_method, " method and ",
      n_numeric_features, " numeric features."
    ), {
      # Make a copy of the data.
      data_copy <- data.table::copy(data)
      
      # Find batch names.
      batch_ids <- unique(data_copy@data[[familiar:::get_id_columns("batch")]])
      
      # Create a list of featureInfo objects.
      feature_info_list <- familiar:::.get_feature_info_data(
        data = data_copy@data,
        file_paths = NULL,
        project_id = character(),
        outcome_type = outcome_type
      )[[1]]

      # Combat requires global standardisation
      if (batch_normalisation_method %in% familiar:::.get_available_batch_normalisation_methods("combat")) {
        # Create normalisation skeletons.
        feature_info_list <- familiar:::create_normalisation_parameter_skeleton(
          feature_info_list = feature_info_list,
          normalisation_method = "standardisation")

        # Update the feature info list with global standardisation parameters.
        feature_info_list <- familiar:::add_normalisation_parameters(
          feature_info_list = feature_info_list,
          data = data_copy)

        # Act as if the data has been transformed.
        data_copy@preprocessing_level <- "transformation"

        # Perform a global normalisation.
        data_copy <- familiar:::normalise_features(
          data = data_copy,
          feature_info_list = feature_info_list)
      }

      # Create batch normalisation container skeleton.
      feature_info_list <- familiar:::create_batch_normalisation_parameter_skeleton(
        feature_info_list = feature_info_list,
        normalisation_method = batch_normalisation_method)

      # Add batch normalisation parameters.
      feature_info_list <- familiar:::add_batch_normalisation_parameters(
        feature_info_list = feature_info_list,
        data = data_copy)

      # Assume that the data is pre-processed.
      data_copy@preprocessing_level <- "normalisation"

      # Attempt to batch normalise the data.
      data_copy <- familiar:::batch_normalise_features(
        data = data_copy,
        feature_info_list = feature_info_list)

      # Test whether the features are normalised (unless none).
      if (batch_normalisation_method == "none") {
        # Check that the data is not altered.
        testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)

        # Iterate over features.
        for (feature in familiar:::get_feature_columns(data_copy)) {
          # Test that the container method matches the batch normalisation
          # method.
          testthat::expect_equal(
            feature_info_list[[feature]]@batch_normalisation_parameters@method,
            batch_normalisation_method)

          # Expect that all batches are named.
          testthat::expect_setequal(
            names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
            batch_ids)

          # Expect that all batches parameter objects have the none class.
          for (batch_parameter_object in 
               feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
            testthat::expect_s4_class(
              batch_parameter_object,
              "featureInfoParametersNormalisationNone")
            testthat::expect_equal(batch_parameter_object@name, feature)
            testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
          }
        }
      } else {
        # Iterate over the different batches to determine if batch corrections
        # were performed correctly.
        for (x in split(data_copy@data, by = "batch_id")) {
          # Find the current batch identifier.
          current_batch_id <- x[["batch_id"]][1]

          # Iterate over features.
          for (feature in familiar:::get_feature_columns(data_copy)) {
            # Determine if the feature is numeric.
            if (feature_info_list[[feature]]@feature_type == "numeric") {
              # Compute mean.
              mu <- mean(x[[feature]], na.rm = TRUE)

              if (batch_normalisation_method %in% 
                  familiar:::.get_available_batch_normalisation_methods("normalisation")) {
                # Check that the feature is correctly centred around 0.5.
                
                testthat::expect_equal(mu < 0.7 & mu > 0.3, TRUE)
                
              } else if (batch_normalisation_method %in% 
                         familiar:::.get_available_batch_normalisation_methods("combat")) {
                # Check that the feature is correctly centred around 0, but with
                # wider margins for the non-parametric method Check also that
                # the overall mean is around 0.
                if (batch_normalisation_method %in% c("combat", "combat_np", "combat_non_parametric")) {
                  testthat::expect_equal(mu < 0.5 & mu > -0.5, TRUE)
                } else {
                  testthat::expect_equal(mu < 0.2 & mu > -0.2, TRUE)
                }

                # Check overall mean.
                mu_t <- mean(data_copy@data[[feature]], na.rm = TRUE)
                testthat::expect_equal(mu_t < 0.2 & mu_t > -0.2, TRUE)
                
              } else {
                # Check that the feature is correctly centred around 0.
                testthat::expect_equal(mu < 0.2 & mu > -0.2, TRUE)
              }

              # Test that the container method matches the batch normalisation
              # method.
              testthat::expect_equal(
                feature_info_list[[feature]]@batch_normalisation_parameters@method,
                batch_normalisation_method)

              # Expect that all batches are named.
              testthat::expect_setequal(
                names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
                batch_ids)
              
            } else {
              # For categorical features test that the none batch normalisation
              # method is present.
              testthat::expect_equal(
                feature_info_list[[feature]]@batch_normalisation_parameters@method,
                "none")

              # Expect that all batches are named.
              testthat::expect_setequal(
                names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
                batch_ids)

              # Expect that all batches parameter objects have
              # the none class.
              for (batch_parameter_object in 
                   feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
                testthat::expect_s4_class(batch_parameter_object, "featureInfoParametersNormalisationNone")
                testthat::expect_equal(batch_parameter_object@name, feature)
                testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
              }
            }
          }
        }
      }

      # Assert that inverting the transformation produces the original dataset.
      data_restored <- familiar:::batch_normalise_features(
        data = data_copy,
        feature_info_list = feature_info_list,
        invert = TRUE)

      # Inverse the global normalisation.
      if (batch_normalisation_method %in% 
          familiar:::.get_available_batch_normalisation_methods("combat")) {
        # Perform a global normalisation.
        data_restored <- familiar:::normalise_features(
          data = data_restored,
          feature_info_list = feature_info_list,
          invert = TRUE)
      }

      # Iterate over features and compare. They should be equal.
      for (feature in familiar:::get_feature_columns(data_restored)) {
        # Expect that values are similar to a tolerance.
        testthat::expect_equal(data_restored@data[[feature]], data@data[[feature]])
      }

      # Assert that aggregating the batch normalisation parameters works as expected.
      aggr_batch_normalisation_parameters <- familiar:::..collect_and_aggregate_batch_normalisation_info(
        feature_info_list = feature_info_list,
        instance_mask = rep_len(TRUE, length(feature_info_list)),
        feature_name = "test"
      )$parameters

      for (batch_norm_parameters in aggr_batch_normalisation_parameters@batch_parameters) {
        if (n_numeric_features > 0 && batch_normalisation_method != "none") {
          # Expect that the selected transformation method matches the selected
          # method, except for combat methods.
          if (n_numeric_features <= 2 && 
              batch_normalisation_method %in% familiar:::.get_available_batch_normalisation_methods("combat")) {
            testthat::expect_s4_class(
              batch_norm_parameters,
              "featureInfoParametersNormalisationStandardisation")
            
          } else {
            testthat::expect_equal(
              batch_norm_parameters@method, 
              batch_normalisation_method)
          }

          # Expect that the shift and scale parameters are not NA.
          testthat::expect_equal(is.finite(batch_norm_parameters@shift), TRUE)

          if (is(batch_norm_parameters, "featureInfoParametersNormalisationShiftScale")) {
            testthat::expect_equal(is.finite(batch_norm_parameters@scale), TRUE)
          }
        } else {
          # Assert that for the categorical features, the none method is used.
          testthat::expect_s4_class(
            batch_norm_parameters,
            "featureInfoParametersNormalisationNone")
        }
      }
    })
  }
}



# NA-value test ----------------------------------------------------------------
for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_na_data(
    outcome_type = outcome_type,
    n_numeric = n_numeric_features)

  for (batch_normalisation_method in familiar:::.get_available_batch_normalisation_methods()) {
    testthat::test_that(paste0(
      "Batch normalisation is correctly performed using the ",
      batch_normalisation_method, " method and ",
      n_numeric_features, " numeric features for a dataset with some NA data."
    ), {
      # Make a copy of the data.
      data_copy <- data.table::copy(data)

      # Find batch names.
      batch_ids <- unique(data_copy@data[[familiar:::get_id_columns("batch")]])

      # Create a list of featureInfo objects.
      feature_info_list <- familiar:::.get_feature_info_data(
        data = data_copy@data,
        file_paths = NULL,
        project_id = character(),
        outcome_type = outcome_type
      )[[1]]

      # Combat requires global standardisation
      if (batch_normalisation_method %in% 
          familiar:::.get_available_batch_normalisation_methods("combat")) {
        # Create normalisation skeletons.
        feature_info_list <- familiar:::create_normalisation_parameter_skeleton(
          feature_info_list = feature_info_list,
          normalisation_method = "standardisation")

        # Update the feature info list with global standardisation parameters.
        feature_info_list <- familiar:::add_normalisation_parameters(
          feature_info_list = feature_info_list,
          data = data_copy)

        # Act as if the data has been transformed.
        data_copy@preprocessing_level <- "transformation"

        # Perform a global normalisation.
        data_copy <- familiar:::normalise_features(
          data = data_copy,
          feature_info_list = feature_info_list)
      }

      # Create batch normalisation container skeleton.
      feature_info_list <- familiar:::create_batch_normalisation_parameter_skeleton(
        feature_info_list = feature_info_list,
        normalisation_method = batch_normalisation_method)

      # Add batch normalisation parameters.
      feature_info_list <- familiar:::add_batch_normalisation_parameters(
        feature_info_list = feature_info_list,
        data = data_copy)

      # Assume that the data is pre-processed.
      data_copy@preprocessing_level <- "normalisation"

      # Attempt to batch normalise the data.
      data_copy <- familiar:::batch_normalise_features(
        data = data_copy,
        feature_info_list = feature_info_list)

      # Test whether the features are normalised (unless none).
      if (batch_normalisation_method == "none") {
        # Check that the data is not altered.
        testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)

        # Iterate over features.
        for (feature in familiar:::get_feature_columns(data_copy)) {
          # Test that the container method matches the batch normalisation
          # method.
          testthat::expect_equal(
            feature_info_list[[feature]]@batch_normalisation_parameters@method,
            batch_normalisation_method)

          # Expect that all batches are named.
          testthat::expect_setequal(
            names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
            batch_ids)

          # Expect that all batches parameter objects have the none class.
          for (batch_parameter_object in
               feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
            testthat::expect_s4_class(
              batch_parameter_object,
              "featureInfoParametersNormalisationNone")
            testthat::expect_equal(batch_parameter_object@name, feature)
            testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
          }
        }
        
      } else {
        # Iterate over the different batches to determine if batch corrections
        # were performed correctly.
        for (x in split(data_copy@data, by = "batch_id")) {
          # Find the current batch identifier.
          current_batch_id <- x[["batch_id"]][1]

          # Iterate over features.
          for (feature in familiar:::get_feature_columns(data_copy)) {
            # Determine if the feature is numeric.
            if (feature_info_list[[feature]]@feature_type == "numeric") {
              # Compute mean.
              mu <- mean(x[[feature]], na.rm = TRUE)

              if (batch_normalisation_method %in% 
                  familiar:::.get_available_batch_normalisation_methods("normalisation")) {
                # Check that the feature is correctly centred around 0.5.
                testthat::expect_equal(mu < 0.7 & mu > 0.3, TRUE)
                
              } else if (batch_normalisation_method %in% 
                         familiar:::.get_available_batch_normalisation_methods("combat")) {
                # Check that the feature is correctly centred around 0, but with
                # wider margins for the non-parametric method Check also that
                # the overall mean is around 0.
                if (batch_normalisation_method %in% c("combat", "combat_np", "combat_non_parametric")) {
                  testthat::expect_equal(mu < 0.5 & mu > -0.5, TRUE)
                } else {
                  testthat::expect_equal(mu < 0.2 & mu > -0.2, TRUE)
                }

                # Check overall mean.
                mu_t <- mean(data_copy@data[[feature]], na.rm = TRUE)
                testthat::expect_equal(mu_t < 0.2 & mu_t > -0.2, TRUE)
                
              } else {
                # Check that the feature is correctly centred around 0.
                testthat::expect_equal(mu < 0.2 & mu > -0.2, TRUE)
              }

              # Test that the container method matches the batch normalisation
              # method.
              testthat::expect_equal(
                feature_info_list[[feature]]@batch_normalisation_parameters@method,
                batch_normalisation_method)

              # Expect that all batches are named.
              testthat::expect_setequal(
                names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
                batch_ids)
              
            } else {
              # For categorical features test that the none batch normalisation
              # method is present.
              testthat::expect_equal(
                feature_info_list[[feature]]@batch_normalisation_parameters@method,
                "none")

              # Expect that all batches are named.
              testthat::expect_setequal(
                names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
                batch_ids)

              # Expect that all batches parameter objects have the none class.
              for (batch_parameter_object in 
                   feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
                testthat::expect_s4_class(
                  batch_parameter_object,
                  "featureInfoParametersNormalisationNone")
                testthat::expect_equal(batch_parameter_object@name, feature)
                testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
              }
            }
          }
        }
      }
    })
  }
}



#### One feature NA test ##############################################################
for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_one_feature_all_na_data(
    outcome_type = outcome_type, 
    n_numeric = n_numeric_features)

  for (batch_normalisation_method in familiar:::.get_available_batch_normalisation_methods()) {
    testthat::test_that(paste0(
      "Batch normalisation is correctly performed using the ",
      batch_normalisation_method, " method and ",
      n_numeric_features, " numeric features for a dataset with one feature entirely NA."
    ), {
      # Make a copy of the data.
      data_copy <- data.table::copy(data)

      # Find batch names.
      batch_ids <- unique(data_copy@data[[familiar:::get_id_columns("batch")]])

      # Create a list of featureInfo objects.
      feature_info_list <- familiar:::.get_feature_info_data(
        data = data_copy@data,
        file_paths = NULL,
        project_id = character(),
        outcome_type = outcome_type
      )[[1]]

      # Combat requires global standardisation
      if (batch_normalisation_method %in% familiar:::.get_available_batch_normalisation_methods("combat")) {
        # Create normalisation skeletons.
        feature_info_list <- familiar:::create_normalisation_parameter_skeleton(
          feature_info_list = feature_info_list,
          normalisation_method = "standardisation")

        # Update the feature info list with global standardisation parameters.
        feature_info_list <- familiar:::add_normalisation_parameters(
          feature_info_list = feature_info_list,
          data = data_copy)

        # Act as if the data has been transformed.
        data_copy@preprocessing_level <- "transformation"

        # Perform a global normalisation.
        data_copy <- familiar:::normalise_features(
          data = data_copy,
          feature_info_list = feature_info_list)
      }

      # Create batch normalisation container skeleton.
      feature_info_list <- familiar:::create_batch_normalisation_parameter_skeleton(
        feature_info_list = feature_info_list,
        normalisation_method = batch_normalisation_method)

      # Add batch normalisation parameters.
      feature_info_list <- familiar:::add_batch_normalisation_parameters(
        feature_info_list = feature_info_list,
        data = data_copy)

      # Assume that the data is pre-processed.
      data_copy@preprocessing_level <- "normalisation"

      # Attempt to batch normalise the data.
      data_copy <- familiar:::batch_normalise_features(
        data = data_copy,
        feature_info_list = feature_info_list)

      # Test whether the features are normalised (unless none).
      if (batch_normalisation_method == "none") {
        # Check that the data is not altered.
        testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)

        # Iterate over features.
        for (feature in familiar:::get_feature_columns(data_copy)) {
          # Test that the container method matches the batch normalisation
          # method.
          testthat::expect_equal(
            feature_info_list[[feature]]@batch_normalisation_parameters@method,
            batch_normalisation_method)

          # Expect that all batches are named.
          testthat::expect_setequal(
            names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
            batch_ids)

          # Expect that all batches parameter objects have the none class.
          for (batch_parameter_object in 
               feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
            testthat::expect_s4_class(
              batch_parameter_object,
              "featureInfoParametersNormalisationNone")
            testthat::expect_equal(batch_parameter_object@name, feature)
            testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
          }
        }
      } else {
        # Iterate over the different batches to determine if batch corrections
        # were performed correctly.
        for (x in split(data_copy@data, by = "batch_id")) {
          # Find the current batch identifier.
          current_batch_id <- x[["batch_id"]][1]

          # Iterate over features.
          for (feature in familiar:::get_feature_columns(data_copy)) {
            # Determine if the feature is numeric.
            if (feature_info_list[[feature]]@feature_type == "numeric") {
              if (feature == "feature_2") {
                # Expect that all batches are named.
                testthat::expect_setequal(
                  names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
                  batch_ids)

                # Expect that all batch parameter objects have the none class.
                for (batch_parameter_object in 
                     feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
                  testthat::expect_s4_class(
                    batch_parameter_object,
                    "featureInfoParametersNormalisationNone")
                  testthat::expect_equal(batch_parameter_object@name, feature)
                  testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
                }
                
              } else {
                # Compute mean.
                mu <- mean(x[[feature]], na.rm = TRUE)

                if (batch_normalisation_method %in% 
                    familiar:::.get_available_batch_normalisation_methods("normalisation")) {
                  # Check that the feature is correctly centred around 0.5.
                  testthat::expect_equal(mu < 0.7 & mu > 0.3, TRUE)
                  
                } else if (batch_normalisation_method %in% 
                           familiar:::.get_available_batch_normalisation_methods("combat")) {
                  # Check that the feature is correctly centred around 0, but
                  # with wider margins for the non-parametric method Check also
                  # that the overall mean is around 0.
                  if (batch_normalisation_method %in% c("combat", "combat_np", "combat_non_parametric")) {
                    testthat::expect_equal(mu < 0.5 & mu > -0.5, TRUE)
                  } else {
                    testthat::expect_equal(mu < 0.2 & mu > -0.2, TRUE)
                  }

                  # Check overall mean.
                  mu_t <- mean(data_copy@data[[feature]], na.rm = TRUE)
                  testthat::expect_equal(mu_t < 0.2 & mu_t > -0.2, TRUE)
                  
                } else {
                  # Check that the feature is correctly
                  # centred around 0.
                  testthat::expect_equal(mu < 0.2 & mu > -0.2, TRUE)
                }

                # Test that the container method matches the batch normalisation
                # method.
                testthat::expect_equal(
                  feature_info_list[[feature]]@batch_normalisation_parameters@method,
                  batch_normalisation_method)

                # Expect that all batches are named.
                testthat::expect_setequal(
                  names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
                  batch_ids)
              }
              
            } else {
              # For categorical features test that the none batch normalisation
              # method is present.
              testthat::expect_equal(
                feature_info_list[[feature]]@batch_normalisation_parameters@method,
                "none")

              # Expect that all batches are named.
              testthat::expect_setequal(
                names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
                batch_ids)

              # Expect that all batch parameter objects have
              # the none class.
              for (batch_parameter_object in 
                   feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
                testthat::expect_s4_class(
                  batch_parameter_object,
                  "featureInfoParametersNormalisationNone")
                testthat::expect_equal(batch_parameter_object@name, feature)
                testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
              }
            }
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

  for (batch_normalisation_method in familiar:::.get_available_batch_normalisation_methods()) {
    testthat::test_that(paste0(
      "Batch normalisation is correctly performed using the ",
      batch_normalisation_method, " method and ",
      n_numeric_features, " numeric features for a dataset with invariant features."
    ), {
      # Make a copy of the data.
      data_copy <- data.table::copy(data)

      # Find batch names.
      batch_ids <- unique(data_copy@data[[familiar:::get_id_columns("batch")]])

      # Create a list of featureInfo objects.
      feature_info_list <- familiar:::.get_feature_info_data(
        data = data_copy@data,
        file_paths = NULL,
        project_id = character(),
        outcome_type = outcome_type
      )[[1]]

      # Combat requires global standardisation
      if (batch_normalisation_method %in% familiar:::.get_available_batch_normalisation_methods("combat")) {
        # Create normalisation skeletons.
        feature_info_list <- familiar:::create_normalisation_parameter_skeleton(
          feature_info_list = feature_info_list,
          normalisation_method = "standardisation")

        # Update the feature info list with global standardisation parameters.
        feature_info_list <- familiar:::add_normalisation_parameters(
          feature_info_list = feature_info_list,
          data = data_copy)

        # Act as if the data has been transformed.
        data_copy@preprocessing_level <- "transformation"

        # Perform a global normalisation.
        data_copy <- familiar:::normalise_features(
          data = data_copy,
          feature_info_list = feature_info_list)
      }

      # Create batch normalisation container skeleton.
      feature_info_list <- familiar:::create_batch_normalisation_parameter_skeleton(
        feature_info_list = feature_info_list,
        normalisation_method = batch_normalisation_method)

      # Add batch normalisation parameters.
      feature_info_list <- familiar:::add_batch_normalisation_parameters(
        feature_info_list = feature_info_list,
        data = data_copy)

      # Assume that the data is pre-processed.
      data_copy@preprocessing_level <- "normalisation"

      # Attempt to batch normalise the data.
      data_copy <- familiar:::batch_normalise_features(
        data = data_copy,
        feature_info_list = feature_info_list)

      # Test whether the features are normalised (unless none).
      if (batch_normalisation_method == "none") {
        # Check that the data is not altered.
        testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)

        # Iterate over features.
        for (feature in familiar:::get_feature_columns(data_copy)) {
          # Test that the container method matches the batch normalisation
          # method.
          testthat::expect_equal(
            feature_info_list[[feature]]@batch_normalisation_parameters@method,
            batch_normalisation_method)

          # Expect that all batches are named.
          testthat::expect_setequal(
            names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
            batch_ids)

          # Expect that all batches parameter objects have
          # the none class.
          for (batch_parameter_object in feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
            testthat::expect_s4_class(batch_parameter_object, "featureInfoParametersNormalisationNone")
            testthat::expect_equal(batch_parameter_object@name, feature)
            testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
          }
        }
      } else {
        # Iterate over the different batches to determine if batch corrections
        # were performed correctly.
        for (feature in familiar:::get_feature_columns(data_copy)) {
          for (x in split(data_copy@data, by = "batch_id")) {
            # Find the current batch identifier.
            current_batch_id <- x[["batch_id"]][1]

            # Iterate over features.
            for (feature in familiar:::get_feature_columns(data_copy)) {
              # Determine if the feature is numeric.
              if (feature_info_list[[feature]]@feature_type == "numeric") {
                # Expect that all batches are named.
                testthat::expect_setequal(
                  names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
                  batch_ids)

                # Expect that all batch parameter objects have the none class.
                for (batch_parameter_object in 
                     feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
                  testthat::expect_s4_class(
                    batch_parameter_object,
                    "featureInfoParametersNormalisationNone")
                  testthat::expect_equal(batch_parameter_object@name, feature)
                  testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
                }
                
              } else {
                # For categorical features test that the none batch
                # normalisation method is present.
                testthat::expect_equal(
                  feature_info_list[[feature]]@batch_normalisation_parameters@method,
                  "none")

                # Expect that all batches are named.
                testthat::expect_setequal(
                  names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
                  batch_ids)

                # Expect that all batches parameter objects have the none class.
                for (batch_parameter_object in 
                     feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
                  testthat::expect_s4_class(
                    batch_parameter_object,
                    "featureInfoParametersNormalisationNone")
                  testthat::expect_equal(batch_parameter_object@name, feature)
                  testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
                }
              }
            }
          }
        }
      }
    })
  }
}



# One feature invariant test ---------------------------------------------------
for (n_numeric_features in c(4, 3, 2, 1, 0)) {
  data <- familiar:::test_create_synthetic_series_one_feature_invariant_data(
    outcome_type = outcome_type,
    n_numeric = n_numeric_features)

  for (batch_normalisation_method in 
       familiar:::.get_available_batch_normalisation_methods()) {
    testthat::test_that(paste0(
      "Batch normalisation is correctly performed using the ",
      batch_normalisation_method, " method and ",
      n_numeric_features, " numeric features for a dataset with one invariant feature."
    ), {
      # Make a copy of the data.
      data_copy <- data.table::copy(data)

      # Find batch names.
      batch_ids <- unique(data_copy@data[[familiar:::get_id_columns("batch")]])

      # Create a list of featureInfo objects.
      feature_info_list <- familiar:::.get_feature_info_data(
        data = data_copy@data,
        file_paths = NULL,
        project_id = character(),
        outcome_type = outcome_type
      )[[1]]

      # Combat requires global standardisation
      if (batch_normalisation_method %in% 
          familiar:::.get_available_batch_normalisation_methods("combat")) {
        # Create normalisation skeletons.
        feature_info_list <- familiar:::create_normalisation_parameter_skeleton(
          feature_info_list = feature_info_list,
          normalisation_method = "standardisation")

        # Update the feature info list with global standardisation parameters.
        feature_info_list <- familiar:::add_normalisation_parameters(
          feature_info_list = feature_info_list,
          data = data_copy)

        # Act as if the data has been transformed.
        data_copy@preprocessing_level <- "transformation"

        # Perform a global normalisation.
        data_copy <- familiar:::normalise_features(
          data = data_copy,
          feature_info_list = feature_info_list)
      }

      # Create batch normalisation container skeleton.
      feature_info_list <- familiar:::create_batch_normalisation_parameter_skeleton(
        feature_info_list = feature_info_list,
        normalisation_method = batch_normalisation_method)

      # Add batch normalisation parameters.
      feature_info_list <- familiar:::add_batch_normalisation_parameters(
        feature_info_list = feature_info_list,
        data = data_copy)

      # Assume that the data is pre-processed.
      data_copy@preprocessing_level <- "normalisation"

      # Attempt to batch normalise the data.
      data_copy <- familiar:::batch_normalise_features(
        data = data_copy,
        feature_info_list = feature_info_list)

      # Test whether the features are normalised (unless none).
      if (batch_normalisation_method == "none") {
        # Check that the data is not altered.
        testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)

        # Iterate over features.
        for (feature in familiar:::get_feature_columns(data_copy)) {
          # Test that the container method matches the batch normalisation
          # method.
          testthat::expect_equal(
            feature_info_list[[feature]]@batch_normalisation_parameters@method,
            batch_normalisation_method)

          # Expect that all batches are named.
          testthat::expect_setequal(
            names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
            batch_ids)

          # Expect that all batches parameter objects have the none class.
          for (batch_parameter_object in 
               feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
            testthat::expect_s4_class(
              batch_parameter_object,
              "featureInfoParametersNormalisationNone")
            testthat::expect_equal(batch_parameter_object@name, feature)
            testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
          }
        }
        
      } else {
        # Iterate over the different batches to determine if batch corrections
        # were performed correctly.
        for (x in split(data_copy@data, by = "batch_id")) {
          # Find the current batch identifier.
          current_batch_id <- x[["batch_id"]][1]

          # Iterate over features.
          for (feature in familiar:::get_feature_columns(data_copy)) {
            # Determine if the feature is numeric.
            if (feature_info_list[[feature]]@feature_type == "numeric") {
              if (feature == "feature_2") {
                # Expect that all batches are named.
                testthat::expect_setequal(
                  names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
                  batch_ids)

                # Expect that all batch parameter objects have the none class.
                for (batch_parameter_object in 
                     feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
                  testthat::expect_s4_class(
                    batch_parameter_object,
                    "featureInfoParametersNormalisationNone")
                  testthat::expect_equal(batch_parameter_object@name, feature)
                  testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
                }
                
              } else {
                # Compute mean.
                mu <- mean(x[[feature]], na.rm = TRUE)

                if (batch_normalisation_method %in% 
                    familiar:::.get_available_batch_normalisation_methods("normalisation")) {
                  # Check that the feature is correctly centred around 0.5.
                  testthat::expect_equal(mu < 0.7 & mu > 0.3, TRUE)
                  
                } else if (batch_normalisation_method %in% 
                           familiar:::.get_available_batch_normalisation_methods("combat")) {
                  # Check that the feature is correctly centred around 0, but
                  # with wider margins for the non-parametric method Check also
                  # that the overall mean is around 0.
                  if (batch_normalisation_method %in% c("combat", "combat_np", "combat_non_parametric")) {
                    testthat::expect_equal(mu < 0.5 & mu > -0.5, TRUE)
                  } else {
                    testthat::expect_equal(mu < 0.2 & mu > -0.2, TRUE)
                  }

                  # Check overall mean.
                  mu_t <- mean(data_copy@data[[feature]], na.rm = TRUE)
                  testthat::expect_equal(mu_t < 0.2 & mu_t > -0.2, TRUE)
                  
                } else {
                  # Check that the feature is correctly centred around 0.
                  testthat::expect_equal(mu < 0.2 & mu > -0.2, TRUE)
                }

                # Test that the container method matches the batch normalisation
                # method.
                testthat::expect_equal(
                  feature_info_list[[feature]]@batch_normalisation_parameters@method,
                  batch_normalisation_method)

                # Expect that all batches are named.
                testthat::expect_setequal(
                  names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
                  batch_ids)
              }
            } else {
              # For categorical features test that the none batch normalisation
              # method is present.
              testthat::expect_equal(
                feature_info_list[[feature]]@batch_normalisation_parameters@method,
                "none")

              # Expect that all batches are named.
              testthat::expect_setequal(
                names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
                batch_ids
              )

              # Expect that all batch parameter objects have the none class.
              for (batch_parameter_object in 
                   feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
                testthat::expect_s4_class(
                  batch_parameter_object,
                  "featureInfoParametersNormalisationNone")
                testthat::expect_equal(batch_parameter_object@name, feature)
                testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
              }
            }
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

  for (batch_normalisation_method in familiar:::.get_available_batch_normalisation_methods()) {
    testthat::test_that(paste0(
      "Batch normalisation is correctly performed using the ",
      batch_normalisation_method, " method and ",
      n_numeric_features, " numeric features for a dataset with invariant features."
    ), {
      # Make a copy of the data.
      data_copy <- data.table::copy(data)

      # Find batch names.
      batch_ids <- unique(data_copy@data[[familiar:::get_id_columns("batch")]])

      # Create a list of featureInfo objects.
      feature_info_list <- familiar:::.get_feature_info_data(
        data = data_copy@data,
        file_paths = NULL,
        project_id = character(),
        outcome_type = outcome_type
      )[[1]]

      # Combat requires global standardisation
      if (batch_normalisation_method %in% familiar:::.get_available_batch_normalisation_methods("combat")) {
        # Create normalisation skeletons.
        feature_info_list <- familiar:::create_normalisation_parameter_skeleton(
          feature_info_list = feature_info_list,
          normalisation_method = "standardisation")

        # Update the feature info list with global standardisation parameters.
        feature_info_list <- familiar:::add_normalisation_parameters(
          feature_info_list = feature_info_list,
          data = data_copy)

        # Act as if the data has been transformed.
        data_copy@preprocessing_level <- "transformation"

        # Perform a global normalisation.
        data_copy <- familiar:::normalise_features(
          data = data_copy,
          feature_info_list = feature_info_list)
      }

      # Create batch normalisation container skeleton.
      feature_info_list <- familiar:::create_batch_normalisation_parameter_skeleton(
        feature_info_list = feature_info_list,
        normalisation_method = batch_normalisation_method)

      # Add batch normalisation parameters.
      feature_info_list <- familiar:::add_batch_normalisation_parameters(
        feature_info_list = feature_info_list,
        data = data_copy)

      # Assume that the data is pre-processed.
      data_copy@preprocessing_level <- "normalisation"

      # Attempt to batch normalise the data.
      data_copy <- familiar:::batch_normalise_features(
        data = data_copy,
        feature_info_list = feature_info_list)

      # Test whether the features are normalised (unless none).
      if (batch_normalisation_method == "none") {
        # Check that the data is not altered.
        testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)

        # Iterate over features.
        for (feature in familiar:::get_feature_columns(data_copy)) {
          # Test that the container method matches the batch normalisation
          # method.
          testthat::expect_equal(
            feature_info_list[[feature]]@batch_normalisation_parameters@method,
            batch_normalisation_method)

          # Expect that all batches are named.
          testthat::expect_setequal(
            names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
            batch_ids)

          # Expect that all batches parameter objects have the none class.
          for (batch_parameter_object in 
               feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
            testthat::expect_s4_class(
              batch_parameter_object,
              "featureInfoParametersNormalisationNone")
            testthat::expect_equal(batch_parameter_object@name, feature)
            testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
          }
        }
        
      } else {
        # Iterate over the different batches to determine if batch corrections
        # were performed correctly.
        for (feature in familiar:::get_feature_columns(data_copy)) {
          for (x in split(data_copy@data, by = "batch_id")) {
            # Find the current batch identifier.
            current_batch_id <- x[["batch_id"]][1]

            # Iterate over features.
            for (feature in familiar:::get_feature_columns(data_copy)) {
              # Determine if the feature is numeric.
              if (feature_info_list[[feature]]@feature_type == "numeric") {
                # Expect that all batches are named.
                testthat::expect_setequal(
                  names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
                  batch_ids)

                # Expect that all batch parameter objects have the none class.
                for (batch_parameter_object in 
                     feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
                  testthat::expect_s4_class(
                    batch_parameter_object,
                    "featureInfoParametersNormalisationNone")
                  testthat::expect_equal(batch_parameter_object@name, feature)
                  testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
                }
                
              } else {
                # For categorical features test that the none batch
                # normalisation method is present.
                testthat::expect_equal(
                  feature_info_list[[feature]]@batch_normalisation_parameters@method,
                  "none")

                # Expect that all batches are named.
                testthat::expect_setequal(
                  names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
                  batch_ids)

                # Expect that all batches parameter objects have the none class.
                for (batch_parameter_object in 
                     feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
                  testthat::expect_s4_class(
                    batch_parameter_object,
                    "featureInfoParametersNormalisationNone")
                  testthat::expect_equal(batch_parameter_object@name, feature)
                  testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
                }
              }
            }
          }
        }
      }
    })
  }
}



# One-batch NA test ------------------------------------------------------------
# Test what happens when all data for feature 1 in batch 1 are missing.
data <- familiar:::test_create_synthetic_series_data(outcome_type = outcome_type)
data@data[batch_id == "1", "feature_1" := NA_real_]

for (batch_normalisation_method in familiar:::.get_available_batch_normalisation_methods()) {
  testthat::test_that(paste0(
    "Batch normalisation is correctly performed using the ",
    batch_normalisation_method,
    " method, when data for feature_1 in batch 1 is missing."
  ), {
    # Make a copy of the data.
    data_copy <- data.table::copy(data)

    # Find batch names.
    batch_ids <- unique(data_copy@data[[familiar:::get_id_columns("batch")]])

    # Create a list of featureInfo objects.
    feature_info_list <- familiar:::.get_feature_info_data(
      data = data_copy@data,
      file_paths = NULL,
      project_id = character(),
      outcome_type = outcome_type
    )[[1]]

    # Combat requires global standardisation
    if (batch_normalisation_method %in% 
        familiar:::.get_available_batch_normalisation_methods("combat")) {
      # Create normalisation skeletons.
      feature_info_list <- familiar:::create_normalisation_parameter_skeleton(
        feature_info_list = feature_info_list,
        normalisation_method = "standardisation")

      # Update the feature info list with global standardisation parameters.
      feature_info_list <- familiar:::add_normalisation_parameters(
        feature_info_list = feature_info_list,
        data = data_copy)

      # Act as if the data has been transformed.
      data_copy@preprocessing_level <- "transformation"

      # Perform a global normalisation.
      data_copy <- familiar:::normalise_features(
        data = data_copy,
        feature_info_list = feature_info_list)
    }

    # Create batch normalisation container skeleton.
    feature_info_list <- familiar:::create_batch_normalisation_parameter_skeleton(
      feature_info_list = feature_info_list,
      normalisation_method = batch_normalisation_method)

    # Add batch normalisation parameters.
    feature_info_list <- familiar:::add_batch_normalisation_parameters(
      feature_info_list = feature_info_list,
      data = data_copy)

    # Assume that the data is pre-processed.
    data_copy@preprocessing_level <- "normalisation"

    # Attempt to batch normalise the data.
    data_copy <- familiar:::batch_normalise_features(
      data = data_copy,
      feature_info_list = feature_info_list)

    # Test whether the features are normalised (unless none).
    if (batch_normalisation_method == "none") {
      # Check that the data is not altered.
      testthat::expect_equal(data.table::fsetequal(data_copy@data, data@data), TRUE)

      # Iterate over features.
      for (feature in familiar:::get_feature_columns(data_copy)) {
        # Test that the container method matches the batch normalisation method.
        testthat::expect_equal(
          feature_info_list[[feature]]@batch_normalisation_parameters@method,
          batch_normalisation_method)

        # Expect that all batches are named.
        testthat::expect_setequal(
          names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
          batch_ids)

        # Expect that all batches parameter objects have the none class.
        for (batch_parameter_object in 
             feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
          testthat::expect_s4_class(
            batch_parameter_object,
            "featureInfoParametersNormalisationNone")
          testthat::expect_equal(batch_parameter_object@name, feature)
          testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
        }
      }
      
    } else {
      # Iterate over the different batches to determine if batch corrections
      # were performed correctly.
      for (x in split(data_copy@data, by = "batch_id")) {
        # Find the current batch identifier.
        current_batch_id <- x[["batch_id"]][1]

        # Iterate over features.
        for (feature in familiar:::get_feature_columns(data_copy)) {
          if (feature == "feature_1" && current_batch_id == "1") {
            # Check that parameter data are inferred.

            # Check that the normalisation object is not none.
            testthat::expect_equal(
              is(
                feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters[[current_batch_id]],
                "featureInfoParametersNormalisationNone"),
              FALSE)

            # Check that the normalisation object is complete.
            testthat::expect_equal(
              familiar:::feature_info_complete(
                feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters[[current_batch_id]]),
              TRUE)
            
          } else if (feature_info_list[[feature]]@feature_type == "numeric") {
            # Determine if the feature is numeric.

            # Compute mean.
            mu <- mean(x[[feature]], na.rm = TRUE)

            if (batch_normalisation_method %in% 
                familiar:::.get_available_batch_normalisation_methods("normalisation")) {
              # Check that the feature is correctly centred around 0.5.
              testthat::expect_equal(mu < 0.7 & mu > 0.3, TRUE)
              
            } else if (batch_normalisation_method %in% 
                       familiar:::.get_available_batch_normalisation_methods("combat")) {
              # Check that the feature is correctly centred around 0, but with
              # wider margins for the non-parametric method Check also that the
              # overall mean is around 0.
              if (batch_normalisation_method %in% c("combat", "combat_np", "combat_non_parametric")) {
                testthat::expect_equal(mu < 0.5 & mu > -0.5, TRUE)
              } else {
                testthat::expect_equal(mu < 0.2 & mu > -0.2, TRUE)
              }

              # Check overall mean.
              mu_t <- mean(data_copy@data[[feature]], na.rm = TRUE)
              testthat::expect_equal(mu_t < 0.2 & mu_t > -0.2, TRUE)
              
            } else {
              # Check that the feature is correctly centred around 0.
              testthat::expect_equal(mu < 0.2 & mu > -0.2, TRUE)
            }

            # Test that the container method matches the batch normalisation
            # method.
            testthat::expect_equal(
              feature_info_list[[feature]]@batch_normalisation_parameters@method,
              batch_normalisation_method)

            # Expect that all batches are named.
            testthat::expect_setequal(
              names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
              batch_ids)
            
          } else {
            # For categorical features test that the none batch normalisation
            # method is present.
            testthat::expect_equal(
              feature_info_list[[feature]]@batch_normalisation_parameters@method,
              "none")

            # Expect that all batches are named.
            testthat::expect_setequal(
              names(feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters),
              batch_ids)

            # Expect that all batches parameter objects have the none class.
            for (batch_parameter_object in 
                 feature_info_list[[feature]]@batch_normalisation_parameters@batch_parameters) {
              testthat::expect_s4_class(
                batch_parameter_object,
                "featureInfoParametersNormalisationNone")
              testthat::expect_equal(batch_parameter_object@name, feature)
              testthat::expect_equal(batch_parameter_object@batch %in% batch_ids, TRUE)
            }
          }
        }
      }
    }
  })
}

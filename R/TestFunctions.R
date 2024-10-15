test_all_learners_available <- function(learners) {
  # Create placeholder flags.
  learner_available <- logical(length(learners))
  names(learner_available) <- learners
  
  # Iterate over learners.
  for (learner in learners) {
    # Determine if the learner is available for any outcome.
    for (outcome_type in c(
      "continuous", "binomial", "multinomial", "survival", "competing_risk"
    )) {
      # Create a familiarModel object.
      object <- methods::new(
        "familiarModel",
        outcome_type = outcome_type,
        learner = learner
      )
      
      # Promote the learner to the right class.
      object <- promote_learner(object = object)
      
      # Check if the learner is available for the outcome.
      if (is_available(object)) {
        learner_available[learner] <- TRUE
        break
      }
    }
  }
  
  # Iterate over learners
  for (learner in learners) {
    testthat::test_that(
      paste0(learner, " is available."), 
      {
        testthat::expect_equal(unname(learner_available[learner]), TRUE)
      }
    )
  }
}



test_all_learners_train_predict_vimp <- function(
    learners,
    hyperparameter_list = NULL,
    except_train = NULL,
    except_naive = NULL,
    except_predict = NULL,
    except_predict_survival = NULL,
    has_vimp = TRUE,
    can_trim = TRUE,
    debug = FALSE
) {
  if (debug) {
    test_fun <- debug_test_that
  } else {
    test_fun <- testthat::test_that
  }
  
  # Iterate over the outcome type.
  for (outcome_type in c("continuous", "binomial", "multinomial", "survival")) {
    # Obtain data.
    full_data <- test_create_good_data(outcome_type)
    full_data_reordered <- test_create_reverse_column_order_data(outcome_type)
    full_one_sample_data <- test_create_one_sample_data(outcome_type)
    one_feature_data <- test_create_single_feature_data(outcome_type)
    one_feature_one_sample_data <- test_create_single_feature_one_sample_data(outcome_type)
    empty_data <- test_create_empty_data(outcome_type)
    no_feature_data <- test_create_data_without_feature(outcome_type)
    bad_data <- test_create_bad_data(outcome_type)
    
    # Prospective datasets with (partially) missing outcomes
    fully_prospective_data <- test_create_prospective_data(outcome_type)
    mostly_prospective_data <- test_create_mostly_prospective_data(outcome_type)
    partially_prospective_data <- test_create_partially_prospective_data(outcome_type)
    
    # Iterate over learners.
    for (learner in learners) {
      # Create a familiarModel object.
      object <- methods::new(
        "familiarModel",
        outcome_type = outcome_type,
        learner = learner
      )
      
      # Promote the learner to the right class.
      object <- promote_learner(object = object)
      
      # Test if the learner is available for the current outcome_type
      if (!is_available(object)) next
      
      # Parse hyperparameter list
      hyperparameters <- c(
        hyperparameter_list[[outcome_type]],
        list("sign_size" = get_n_features(full_data))
      )
      
      # Find required hyperparameters
      learner_hyperparameters <- .get_preset_hyperparameters(
        learner = learner,
        outcome_type = outcome_type,
        names_only = TRUE
      )
      
      # Select hyperparameters that are being used, and are present in the input
      # list of hyperparameters.
      hyperparameters <- hyperparameters[intersect(learner_hyperparameters, names(hyperparameters))]
      
      # Full dataset -----------------------------------------------------------
      
      # Train the model.
      model <- suppressWarnings(test_train(
        data = full_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = learner,
        time_max = 3.5,
        trim_model = FALSE
      ))
      
      # Create a trimmed model -- this is the only instance were we do that
      # without setting the time-out to infinite to test whether the timeout
      # handler returns it correctly.
      trimmed_model <- trim_model(model)
      
      # Generate a file name to save the model to.
      file_name <- tempfile(fileext = ".rds")
      
      # Save file.
      saveRDS(trimmed_model, file = file_name)
      
      # Read file contents as a reloaded model.
      reloaded_model <- readRDS(file = file_name)
      
      # Clean up.
      file.remove(file_name)
      
      # Check that the model can be trimmed.
      test_fun(
        paste0("Model for ", outcome_type, " created using ", learner, " can be trimmed."),
        {
          if (can_trim) {
            testthat::expect_true(trimmed_model@is_trimmed)
            testthat::expect_true(reloaded_model@is_trimmed)
          } else {
            testthat::expect_false(trimmed_model@is_trimmed)
            testthat::expect_false(reloaded_model@is_trimmed)
          }
        }
      )
      
      # Test that models can be created.
      test_fun(
        paste0(
          "Model for ", outcome_type, " can be created using ",
          learner, " using a complete dataset."
        ),
        {
          # Test that the model was successfully created.
          testthat::expect_equal(
            model_is_trained(model),
            ifelse(learner %in% except_train, FALSE, TRUE)
          )
          
          if (outcome_type == "survival") {
            # Calibration info is present
            testthat::expect_true(has_calibration_info(model))
          }
        }
      )
      
      # Test that models can be used to predict the outcome.
      test_fun(
        paste0(
          "Sample predictions for ", outcome_type, " can be made using ",
          learner, " for a complete dataset."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model, data = full_data))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(
            any_predictions_valid(prediction_table),
            !learner %in% c(except_train, except_predict)
          )
          
          if (outcome_type %in% c("binomial", "multinomial")) {
            testthat::expect_s4_class(prediction_table, "predictionTableClassification")
            
            # Expect that the class levels are the same as those in the model.
            testthat::expect_equal(
              get_outcome_class_levels(prediction_table),
              get_outcome_class_levels(model)
            )
            
          } else if (outcome_type == "continuous") {
            testthat::expect_s4_class(prediction_table, "predictionTableRegression")
            
          } else if (outcome_type %in% c("survival")) {
            testthat::expect_s4_class(prediction_table, "predictionTableSurvival")
            
            # Check prediction of survival probability.
            testthat::expect_s4_class(
              suppressWarnings(.predict(model, data = full_data, type = "survival_probability")),
              "predictionTableSurvivalProbability"
            )
            
            # Check prediction of risk groups.
            testthat::expect_s4_class(
              suppressWarnings(.predict(model, data = full_data, type = "risk_stratification")),
              "predictionTableRiskGroups"
            )
          } else {
            ..error_outcome_type_not_implemented(outcome_type)
          }
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(
            trimmed_model,
            data = full_data
          ))
          
          testthat::expect_equal(
            prediction_table,
            prediction_table_trim,
            ignore_attr = TRUE
          )
          
          # Expect that the reloaded model produces the same predictions.
          prediction_table_reloaded <- suppressWarnings(.predict(
            reloaded_model,
            data = full_data
          ))
          
          testthat::expect_equal(
            prediction_table,
            prediction_table_reloaded,
            ignore_attr = TRUE
          )
          
          # Expect that a dataset with different column order produces the same
          # predictions.
          prediction_table_reordered <- suppressWarnings(.predict(
            model,
            data = full_data_reordered
          ))
          
          testthat::expect_equal(
            prediction_table,
            prediction_table_reordered,
            ignore_attr = TRUE
          )
        }
      )
      
      # Test that models can be used to predict the outcome.
      test_fun(
        paste0(
          "Sample predictions for ", outcome_type, " can be made using ",
          learner, " for a one-sample dataset."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model,
            data = full_one_sample_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(
            any_predictions_valid(prediction_table),
            !learner %in% c(except_train, except_predict)
          )
          
          if (outcome_type %in% c("binomial", "multinomial")) {
            # Expect that the class levels are the same as those in the model.
            testthat::expect_equal(
              get_outcome_class_levels(prediction_table),
              get_outcome_class_levels(model)
            )
          }
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(
            trimmed_model,
            data = full_one_sample_data
          ))
          
          testthat::expect_equal(
            prediction_table,
            prediction_table_trim,
            ignore_attr = TRUE
          )
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_reloaded <- suppressWarnings(.predict(
            reloaded_model,
            data = full_one_sample_data
          ))
          
          testthat::expect_equal(
            prediction_table,
            prediction_table_reloaded,
            ignore_attr = TRUE
          )
        }
      )
      
      # Test that models cannot predict for empty datasets.
      test_fun(
        paste0(
          "Sample predictions for ", outcome_type, " can not be made using ",
          learner, " for an empty dataset."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model,
            data = empty_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_false(any_predictions_valid(prediction_table))
        }
      )
      
      # Test that models can be used to predict survival probabilities.
      if (outcome_type %in% c("survival", "competing_risk")) {
        test_fun(
          paste0(
            "Sample survival predictions for ", outcome_type,
            " can be made using ", learner, " for a complete dataset."
          ),
          {
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = full_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict, except_predict_survival)
            )
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = full_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
            
            # Expect that the reloaded model produces the same predictions.
            prediction_table_reloaded <- suppressWarnings(.predict(
              reloaded_model,
              data = full_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_reloaded,
              ignore_attr = TRUE
            )
            
            # Predict stratification.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = full_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict)
            )
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = full_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
            
            # Expect that the reloaded model produces the same predictions.
            prediction_table_reloaded <- suppressWarnings(.predict(
              reloaded_model,
              data = full_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_reloaded,
              ignore_attr = TRUE
            )
          }
        )
        
        test_fun(
          paste0(
            "Sample survival predictions for ", outcome_type, 
            " can be made using ", learner, " for a one-sample dataset."
          ),
          {
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = full_one_sample_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict, except_predict_survival)
            )
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = full_one_sample_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
            
            # Expect that the reloaded model produces the same predictions.
            prediction_table_reloaded <- suppressWarnings(.predict(
              reloaded_model,
              data = full_one_sample_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_reloaded,
              ignore_attr = TRUE
            )
            
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = full_one_sample_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict)
            )
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = full_one_sample_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
            
            # Expect that the reloaded model produces the same predictions.
            prediction_table_reloaded <- suppressWarnings(.predict(
              reloaded_model,
              data = full_one_sample_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_reloaded,
              ignore_attr = TRUE
            )
          }
        )
      }
      
      # Test that the model has variable importance.
      test_fun(
        paste0(
          "Model has variable importance for ", outcome_type, " and ", 
          learner, " for the complete dataset."
        ),
        {
          # Extract the variable importance table.
          vimp_table <- suppressWarnings(get_vimp_table(.vimp(
            model,
            data = full_data
          )))
          
          # Extract the variable importance table for the trimmed model.
          vimp_table_trim <- suppressWarnings(get_vimp_table(.vimp(
            trimmed_model,
            data = full_data
          )))
          
          # Extract the variable importance table for the reloaded model.
          vimp_table_reloaded <- suppressWarnings(get_vimp_table(.vimp(
            reloaded_model,
            data = full_data
          )))
          
          if (has_vimp) {
            # Get the number of features
            n_features <- get_n_features(full_data)
            
            # Expect that the vimp table has two rows.
            testthat::expect_true(nrow(vimp_table) > 0L && nrow(vimp_table) <= n_features)
            testthat::expect_true(nrow(vimp_table_trim) > 0L && nrow(vimp_table_trim) <= n_features)
            testthat::expect_true(nrow(vimp_table_reloaded) > 0L && nrow(vimp_table_reloaded) <= n_features)
            
            # Expect that the names in the vimp table correspond to those of the
            # features.
            testthat::expect_true(all(vimp_table$name %in% get_feature_columns(full_data)))
            testthat::expect_true(all(vimp_table_trim$name %in% get_feature_columns(full_data)))
            testthat::expect_true(all(vimp_table_reloaded$name %in% get_feature_columns(full_data)))
            
          } else {
            # Expect that the vimp table has no rows.
            testthat::expect_true(is_empty(vimp_table))
            testthat::expect_true(is_empty(vimp_table_trim))
            testthat::expect_true(is_empty(vimp_table_reloaded))
          }
        }
      )
      
      # Bootstrapped dataset ---------------------------------------------------
      # Train the model.
      model <- suppressWarnings(test_train(
        data = full_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = learner,
        time_max = 3.5,
        create_bootstrap = TRUE,
        trim_model = FALSE
      ))
      
      # Test that models can be created.
      test_fun(
        paste0(
          "Model for ", outcome_type, " can be created using ",
          learner, " using a complete dataset."
        ),
        {
          # Test that the model was successfully created.
          testthat::expect_equal(model_is_trained(model), !learner %in% except_train)
          
          if (outcome_type == "survival") {
            # Calibration info is present
            testthat::expect_true(has_calibration_info(model))
          }
        }
      )
      
      # Naive model ------------------------------------------------------------
      
      # Train a naive model.
      model <- suppressWarnings(train_familiar(
        data = full_data,
        experimental_design = "fs+mb",
        cluster_method = "none",
        imputation_method = "simple",
        vimp_method = "no_features",
        learner = learner,
        hyperparameter = hyperparameters,
        parallel = FALSE,
        verbose = debug
      ))
      
      test_fun(
        paste0(
          "Naive predictions for ", outcome_type, " can be made using ",
          learner, " for a complete dataset."
        ), 
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model,
            data = full_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(
            any_predictions_valid(prediction_table),
            !learner %in% c(except_train, except_naive)
          )
          
          if (outcome_type %in% c("binomial", "multinomial")) {
            testthat::expect_true(is.factor(.complete(prediction_table)@data$predicted_class))
            
            # Expect that the class levels are the same as those in the model.
            testthat::expect_equal(
              get_outcome_class_levels(prediction_table),
              get_outcome_class_levels(model)
            )
            
          } else if (outcome_type %in% c("continuous", "survival", "competing_risk")) {
            # Expect that the predicted outcome is valid.
            testthat::expect_true(is.numeric(.complete(prediction_table)@data$predicted_outcome))
          }
          
          if (outcome_type %in% c("survival", "competing_risk")) {
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = full_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_naive)
            )
          }
        }
      )
      
      # One-feature dataset ----------------------------------------------------
      # Train the model.
      model <- suppressWarnings(test_train(
        data = one_feature_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = learner,
        time_max = 3.5
      ))
      
      # Create a trimmed model.
      trimmed_model <- trim_model(model, timeout = Inf)
      
      # Test that models can be created.
      test_fun(
        paste0(
          "Model for ", outcome_type, " can be created using ",
          learner, " using a one-feature dataset."
        ),
        {
          # Test that the model was successfully created.
          testthat::expect_equal(model_is_trained(model), !learner %in% except_train)
          
          if (outcome_type == "survival") {
            # Calibration info is present
            testthat::expect_true(has_calibration_info(model))
          }
        }
      )
      
      # Test that models can be used to predict the outcome.
      test_fun(
        paste0(
          "Sample predictions for ", outcome_type, " can be made using ",
          learner, " for a one-feature dataset."
        ), 
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model,
            data = one_feature_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(
            any_predictions_valid(prediction_table),
            !learner %in% c(except_train, except_predict)
          )
          
          if (outcome_type %in% c("binomial", "multinomial")) {
            # Expect that the class levels are the same as those in the model.
            testthat::expect_equal(
              get_outcome_class_levels(prediction_table),
              get_outcome_class_levels(model)
            )
          }
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(
            trimmed_model,
            data = one_feature_data
          ))
          
          testthat::expect_equal(
            prediction_table,
            prediction_table_trim,
            ignore_attr = TRUE
          )
        }
      )
      
      # Test that models can be used to predict the outcome.
      test_fun(
        paste0(
          "Sample predictions for ", outcome_type, " can be made using ",
          learner, " for a one-feature, one-sample dataset."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model,
            data = one_feature_one_sample_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(
            any_predictions_valid(prediction_table),
            !learner %in% c(except_train, except_predict)
          )
          
          if (outcome_type %in% c("binomial", "multinomial")) {
            # Expect that the class levels are the same as those in the model.
            testthat::expect_equal(
              get_outcome_class_levels(prediction_table),
              get_outcome_class_levels(model)
            )
          }
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(
            trimmed_model,
            data = one_feature_one_sample_data
          ))
          
          testthat::expect_equal(
            prediction_table,
            prediction_table_trim,
            ignore_attr = TRUE
          )
        }
      )
      
      # Test that models can be used to predict survival probabilities.
      if (outcome_type %in% c("survival", "competing_risk")) {
        test_fun(
          paste0(
            "Sample survival predictions for ", outcome_type,
            " can be made using ", learner, " for a one-feature dataset."
          ), 
          {
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = one_feature_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = one_feature_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict, except_predict_survival)
            )
            
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = one_feature_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = one_feature_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict)
            )
          }
        )
        
        test_fun(
          paste0(
            "Sample survival predictions for ", outcome_type, 
            " can be made using ", learner, " for a one-feature, one-sample dataset."
          ),
          {
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = one_feature_one_sample_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict, except_predict_survival)
            )
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = one_feature_one_sample_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
            
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = one_feature_one_sample_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict)
            )
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = one_feature_one_sample_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
          }
        )
      }
      
      # Bad dataset ------------------------------------------------------------
      # Train the model.
      model <- suppressWarnings(test_train(
        data = bad_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = learner,
        time_max = 3.5
      ))
      
      # Test that models can be created.
      test_fun(
        paste0(
          "Model for ", outcome_type, " can not be created using ",
          learner, " using a bad dataset."
        ),
        {
          # Test that the model was successfully created.
          testthat::expect_false(model_is_trained(model))
          
          if (outcome_type == "survival") {
            # Calibration info is absent.
            testthat::expect_true(has_calibration_info(model))
          }
        }
      )
      
      # Bad dataset without features -------------------------------------------
      # Train the model.
      model <- suppressWarnings(test_train(
        data = no_feature_data,
        data_bypass = full_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = learner,
        time_max = 3.5
      ))
      
      # Test that models can be created.
      test_fun(
        paste0(
          "Model for ", outcome_type, " can not be created using ", 
          learner, " using a dataset for ."
        ), 
        {
          # Test that the model could not be successfully created.
          testthat::expect_false(model_is_trained(model))
        }
      )
      
      # Dataset without censored instances -------------------------------------
      if (outcome_type %in% c("survival", "competing_risk")) {
        # Set up non-censoring dataset.
        no_censoring_data <- test_create_good_data_without_censoring(outcome_type)
        
        # Train the model.
        model <- suppressWarnings(test_train(
          data = no_censoring_data,
          cluster_method = "none",
          imputation_method = "simple",
          hyperparameter_list = hyperparameters,
          learner = learner,
          time_max = 3.5
        ))
        
        # Create a trimmed model.
        trimmed_model <- trim_model(model, timeout = Inf)
        
        # Test that models can be created.
        test_fun(
          paste0(
            "Model for ", outcome_type, " can be created using ",
            learner, " using a dataset without censoring."
          ),
          {
            # Test that the model was successfully created.
            testthat::expect_equal(model_is_trained(model), !learner %in% except_train)
            
            if (outcome_type == "survival") {
              # Calibration info is present
              testthat::expect_true(has_calibration_info(model))
            }
          }
        )
        
        # Test that models can be used to predict the outcome.
        test_fun(
          paste0(
            "Sample predictions for ", outcome_type, " can be made using ",
            learner, " for a dataset without censoring."
          ),
          {
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = no_censoring_data
            ))
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict)
            )
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = no_censoring_data
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
          }
        )
        
        # Test that models can be used to predict survival probabilities.
        test_fun(
          paste0(
            "Sample survival predictions for ", outcome_type, 
            " can be made using ", learner, " for a dataset without censoring."
          ), 
          {
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = no_censoring_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = no_censoring_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict, except_predict_survival)
            )
            
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = no_censoring_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = no_censoring_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict)
            )
          }
        )
      }
      
      # Dataset with one censored instance -------------------------------------
      if (outcome_type %in% c("survival", "competing_risk")) {
        # Set up non-censoring dataset.
        one_censoring_data <- test_create_good_data_one_censored(outcome_type)
        
        # Train the model.
        model <- suppressWarnings(test_train(
          data = one_censoring_data,
          cluster_method = "none",
          imputation_method = "simple",
          hyperparameter_list = hyperparameters,
          learner = learner,
          time_max = 3.5
        ))
        
        # Create a trimmed model.
        trimmed_model <- trim_model(model, timeout = Inf)
        
        # Test that models can be created.
        test_fun(
          paste0(
            "Model for ", outcome_type, " can be created using ",
            learner, " using a dataset with one censored sample."
          ), 
          {
            # Test that the model was successfully created.
            testthat::expect_equal(model_is_trained(model), !learner %in% except_train)
            
            if (outcome_type == "survival") {
              # Calibration info is present
              testthat::expect_true(has_calibration_info(model))
            }
          }
        )
        
        # Test that models can be used to predict the outcome.
        test_fun(
          paste0(
            "Sample predictions for ", outcome_type, " can be made using ",
            learner, " for a dataset with one censored sample."
          ),
          {
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = one_censoring_data
            ))
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict)
            )
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = one_censoring_data
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
          }
        )
        
        # Test that models can be used to predict survival probabilities.
        test_fun(
          paste0(
            "Sample survival predictions for ", outcome_type,
            " can be made using ", learner, " for a dataset with one censored sample."
          ),
          {
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = one_censoring_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = one_censoring_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict, except_predict_survival)
            )
            
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = one_censoring_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = one_censoring_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict)
            )
          }
        )
      }
      
      
      # Dataset with few censored instances ------------------------------------
      if (outcome_type %in% c("survival", "competing_risk")) {
        # Set up non-censoring dataset.
        few_censoring_data <- test_create_good_data_few_censored(outcome_type)
        
        # Train the model.
        model <- suppressWarnings(test_train(
          data = few_censoring_data,
          cluster_method = "none",
          imputation_method = "simple",
          hyperparameter_list = hyperparameters,
          learner = learner,
          time_max = 3.5
        ))
        
        # Create a trimmed model.
        trimmed_model <- trim_model(model, timeout = Inf)
        
        # Test that models can be created.
        test_fun(
          paste0(
            "Model for ", outcome_type, " can be created using ", learner,
            " using a dataset with few censored samples."
          ), 
          {
            # Test that the model was successfully created.
            testthat::expect_equal(model_is_trained(model), !learner %in% except_train)
            
            if (outcome_type == "survival") {
              # Calibration info is present
              testthat::expect_true(has_calibration_info(model))
            }
          }
        )
        
        # Test that models can be used to predict the outcome.
        test_fun(
          paste0(
            "Sample predictions for ", outcome_type, " can be made using ",
            learner, " for a dataset with few censored samples."
          ),
          {
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = few_censoring_data
            ))
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict)
            )
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = few_censoring_data
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
          }
        )
        
        # Test that models can be used to predict survival probabilities.
        test_fun(
          paste0(
            "Sample survival predictions for ", outcome_type, 
            " can be made using ", learner, " for a dataset with few censored samples."
          ),
          {
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = few_censoring_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = few_censoring_data,
              type = "survival_probability",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict, except_predict_survival)
            )
            
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = few_censoring_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            # Expect that the trimmed model produces the same predictions.
            prediction_table_trim <- suppressWarnings(.predict(
              trimmed_model,
              data = few_censoring_data,
              type = "risk_stratification",
              time = 2.5
            ))
            
            testthat::expect_equal(
              prediction_table,
              prediction_table_trim,
              ignore_attr = TRUE
            )
            
            # Test that the predictions were successfully made.
            testthat::expect_equal(
              any_predictions_valid(prediction_table),
              !learner %in% c(except_train, except_predict)
            )
          }
        )
      }
      
      
      # Fully prospective dataset ----------------------------------------------
      
      # Train the model.
      model <- suppressWarnings(test_train(
        data = fully_prospective_data,
        data_bypass = full_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = learner,
        time_max = 3.5
      ))
      
      # Test that models can be created.
      test_fun(
        paste0(
          "Model for ", outcome_type, " cannot be created using ",
          learner, " for a fully prospective dataset."
        ), 
        {
          # Test that the model was not created.
          testthat::expect_false(model_is_trained(model))
        }
      )
      
      # Mostly prospective dataset ---------------------------------------------
      
      # Train the model.
      model <- suppressWarnings(test_train(
        data = mostly_prospective_data,
        data_bypass = full_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = learner,
        time_max = 3.5
      ))
      
      # Test that models can be created.
      test_fun(
        paste0(
          "Model for ", outcome_type, " cannot be created using ", learner,
          " for an almost fully prospective dataset, where outcome is known for just a single sample."
        ),
        {
          # Test that the model was not created.
          testthat::expect_false(model_is_trained(model))
        }
      )
      
      # Partially prospective dataset ------------------------------------------
      
      # Train the model.
      model <- suppressWarnings(test_train(
        data = partially_prospective_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = learner,
        time_max = 3.5
      ))
      
      # Test that models can be created.
      test_fun(
        paste0(
          "Model for ", outcome_type, " can be created using ", learner,
          " for a partially prospective dataset, where outcome is known for most samples."
        ),
        {
          # Test that the model was successfully created.
          testthat::expect_equal(model_is_trained(model), !learner %in% except_train)
        }
      )
    }
  }
}



test_all_learners_parallel_train_predict_vimp <- function(
    learners,
    hyperparameter_list = NULL,
    has_vimp = TRUE
) {
  # This function serves to test whether packages are loaded correctly for model
  # training, variable importance and so forth.
  
  # Disable randomForestSRC OpenMP core use.
  options(rf.cores = 1L)
  on.exit(options(rf.cores = -1L), add = TRUE)
  
  # Disable multithreading on data.table to prevent reduced performance due to
  # resource collisions with familiar parallelisation.
  data.table::setDTthreads(1L)
  on.exit(data.table::setDTthreads(0L), add = TRUE)
  
  # Iterate over the outcome type.
  for (outcome_type in c("continuous", "binomial", "multinomial", "survival")) {
    # Obtain data.
    full_data <- test_create_good_data(outcome_type)
    
    # Iterate over learners.
    for (learner in learners) {
      if (!.check_learner_outcome_type(
        learner = learner,
        outcome_type = outcome_type,
        as_flag = TRUE
      )) {
        next
      }
      
      # Parse hyperparameter list
      hyperparameters <- c(
        hyperparameter_list[[outcome_type]],
        list("sign_size" = get_n_features(full_data))
      )
      
      # Find required hyperparameters
      learner_hyperparameters <- .get_preset_hyperparameters(
        learner = learner,
        outcome_type = outcome_type,
        names_only = TRUE
      )
      
      # Select hyperparameters that are being used, and are present in the input
      # list of hyperparameters.
      hyperparameters <- hyperparameters[intersect(learner_hyperparameters, names(hyperparameters))]
      
      # Train models -----------------------------------------------------------
      cl_train <- .test_start_cluster(n_cores = 2L)
      
      # Train the models.
      model_list <- parallel::parLapply(
        cl = cl_train,
        list("1" = full_data, "2" = full_data),
        test_train,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = learner,
        time_max = 3.5,
        trim_model = FALSE
      )
      
      # Test that models can be created.
      testthat::test_that(
        paste0(
          "Model for ", outcome_type, " can be created using ",
          learner, " using a complete dataset."
        ),
        {
          # Test that the model was successfully created.
          testthat::expect_true(model_is_trained(model_list[[1L]]))
          testthat::expect_true(model_is_trained(model_list[[2L]]))
        }
      )
      
      # Terminate cluster.
      cl_train <- .terminate_cluster(cl_train)
      
      # Variable importance ----------------------------------------------------
      cl_vimp <- .test_start_cluster(n_cores = 2L)
      
      # Extract variable importance objects.
      vimp_table_list <- parallel::parLapply(
        cl = cl_vimp,
        model_list,
        .vimp,
        data = full_data
      )
      
      # Extract the variable importance tables themselves.
      vimp_table_list <- lapply(vimp_table_list, get_vimp_table)
      
      # Test that the model has variable importance.
      testthat::test_that(
        paste0(
          "Model has variable importance for ", outcome_type, " and ",
          learner, " for the complete dataset."
        ),
        {
          if (has_vimp) {
            # Get the number of features
            n_features <- get_n_features(full_data)
            
            # Expect that the vimp table has two rows.
            testthat::expect_true(nrow(vimp_table_list[[1L]]) > 0L && nrow(vimp_table_list[[1L]]) <= n_features)
            testthat::expect_true(nrow(vimp_table_list[[2L]]) > 0L && nrow(vimp_table_list[[2L]]) <= n_features)
            
            # Expect that the names in the vimp table correspond to those of the
            # features.
            testthat::expect_in(vimp_table_list[[1L]]$name, get_feature_columns(full_data))
            testthat::expect_in(vimp_table_list[[2L]]$name, get_feature_columns(full_data))
            
          } else {
            # Expect that the vimp table has no rows.
            testthat::expect_true(is_empty(vimp_table_list[[1L]]))
            testthat::expect_true(is_empty(vimp_table_list[[2L]]))
          }
        }
      )
      
      # Terminate cluster.
      cl_vimp <- .terminate_cluster(cl_vimp)
      
      # Predictions ------------------------------------------------------------
      cl_predict <- .test_start_cluster(n_cores = 2L)
      
      # Extract predictions.
      prediction_list <- parallel::parLapply(
        cl = cl_predict,
        model_list,
        .predict,
        data = full_data
      )
      
      # Test that models can be used to predict the outcome.
      testthat::test_that(
        paste0(
          "Sample predictions for ", outcome_type, " can be made using ",
          learner, " for a complete dataset."
        ),
        {
          # Test that the predictions were successfully made.
          testthat::expect_true(any_predictions_valid(prediction_list[[1L]]))
          testthat::expect_true(any_predictions_valid(prediction_list[[2L]]))
        }
      )
      
      # Terminate cluster.
      cl_predict <- .terminate_cluster(cl_predict)
    }
  }
}



test_all_novelty_detectors_available <- function(detectors) {
  # Create placeholder flags.
  detector_available <- logical(length(detectors))
  names(detector_available) <- detectors
  
  # Iterate over learners.
  for (detector in detectors) {
    # Create a familiarModel object.
    object <- methods::new(
      "familiarNoveltyDetector",
      learner = detector
    )
    
    # Promote the learner to the right class.
    object <- promote_detector(object = object)
    
    # Check if the learner is available for the outcome.
    if (is_available(object)) {
      detector_available[detector] <- TRUE
    }
  }
  
  # Iterate over learners
  for (detector in detectors) {
    testthat::test_that(
      paste0(detector, " is available."), 
      {
        testthat::expect_true(unname(detector_available[detector]))
      }
    )
  }
}



test_all_novelty_detectors <- function(
    detectors,
    hyperparameter_list = NULL,
    except_train = NULL,
    except_predict = NULL,
    except_predict_survival = NULL,
    can_trim = TRUE,
    debug = FALSE
) {
  if (debug) {
    test_fun <- debug_test_that
  } else {
    test_fun <- testthat::test_that
  }
  
  # Outcome type is not important, but set to get suitable datasets.
  outcome_type <- "continuous"
  
  # Obtain data.
  full_data <- test_create_good_data(outcome_type)
  full_one_sample_data <- test_create_one_sample_data(outcome_type)
  one_feature_data <- test_create_single_feature_data(outcome_type)
  one_feature_one_sample_data <- test_create_single_feature_one_sample_data(outcome_type)
  empty_data <- test_create_empty_data(outcome_type)
  no_feature_data <- test_create_data_without_feature(outcome_type)
  
  # Iterate over learners.
  for (detector in detectors) {
    # Create a familiarNoveltyDetector object.
    object <- methods::new(
      "familiarNoveltyDetector",
      learner = detector
    )
    
    # Promote the novelty detector to the right class.
    object <- promote_detector(object = object)
    
    # Test if the detector is available.
    if (!is_available(object)) next
    
    # Full dataset -------------------------------------------------------------
    
    # Train the novelty detector.
    model <- suppressWarnings(test_train_novelty_detector(
      data = full_data,
      cluster_method = "none",
      imputation_method = "simple",
      hyperparameter_list = hyperparameter_list,
      detector = detector
    ))
    
    # Create a trimmed detector.
    trimmed_model <- trim_model(model, timeout = Inf)
    
    # Check that the the novelty detector can be trimmed.
    test_fun(
      paste0("Detector created using the ", detector, " algorithm can be trimmed."),
      {
        if (can_trim) {
          testthat::expect_true(trimmed_model@is_trimmed)
        } else {
          testthat::expect_false(trimmed_model@is_trimmed)
        }
      }
    )
    
    # Test that the novelty detector can be created.
    test_fun(
      paste0(
        "Detector can be created using the ", detector,
        " algorithm using a complete dataset."
      ),
      {
        # Test that the novelty detector was successfully created.
        testthat::expect_equal(
          model_is_trained(model),
          !detector %in% except_train
        )
      }
    )
    
    # Test that the novelty detector can be used to predict the outcome.
    test_fun(
      paste0(
        "Novely predictions can be made using the ", detector, 
        " algorithm for a complete dataset."
      ), 
      {
        # Expect predictions to be made.
        prediction_table <- suppressWarnings(.predict(
          model,
          data = full_data
        ))
        
        # Test that the predictions were successfully made.
        testthat::expect_equal(
          any_predictions_valid(prediction_table),
          !detector %in% c(except_train, except_predict)
        )
        
        # Expect that the trimmed novelty detector produces the same predictions.
        prediction_table_trim <- suppressWarnings(.predict(
          trimmed_model,
          data = full_data
        ))
        
        testthat::expect_equal(
          prediction_table,
          prediction_table_trim,
          ignore_attr = TRUE
        )
      }
    )
    
    # Test that the novelty detector can be used to predict the outcome.
    test_fun(
      paste0(
        "Novelty predictions can be made using the ", detector, 
        " algorithm for a one-sample dataset."
      ),
      {
        # Expect predictions to be made.
        prediction_table <- suppressWarnings(.predict(
          model,
          data = full_one_sample_data
        ))
        
        # Test that the predictions were successfully made.
        testthat::expect_equal(
          any_predictions_valid(prediction_table),
          !detector %in% c(except_train, except_predict)
        )
        
        # Expect that the trimmed novelty detector produces the same predictions.
        prediction_table_trim <- suppressWarnings(.predict(
          trimmed_model,
          data = full_one_sample_data
        ))
        
        testthat::expect_equal(
          prediction_table,
          prediction_table_trim,
          ignore_attr = TRUE
        )
      }
    )
    
    # Test that the novelty detector cannot predict for empty datasets.
    test_fun(
      paste0(
        "Novelty predictions can not be made using the ", detector,
        " algorithm for an empty dataset."
      ),
      {
        # Expect predictions to be made.
        prediction_table <- suppressWarnings(.predict(
          model,
          data = empty_data
        ))
        testthat::expect_false(any_predictions_valid(prediction_table))
      }
    )
    
    # One-feature dataset ------------------------------------------------------
    # Train the novelty detector.
    model <- suppressWarnings(test_train_novelty_detector(
      data = one_feature_data,
      cluster_method = "none",
      imputation_method = "simple",
      hyperparameter_list = hyperparameter_list,
      detector = detector
    ))
    
    # Create a trimmed novelty detector.
    trimmed_model <- trim_model(model, timeout = Inf)
    
    # Test that the novelty detector can be created.
    test_fun(
      paste0(
        "Detector can be created using the ", detector,
        " algorithm using a one-feature dataset."
      ),
      {
        # Test that the novelty detector was successfully created.
        testthat::expect_equal(
          model_is_trained(model),
          !detector %in% except_train
        )
      }
    )
    
    # Test that the novelty detector can be used to predict the outcome.
    test_fun(
      paste0(
        "Novelty predictions can be made using the ", detector,
        " algorithm for a one-feature dataset."
      ), 
      {
        # Expect predictions to be made.
        prediction_table <- suppressWarnings(.predict(
          model,
          data = one_feature_data
        ))
        
        # Test that the predictions were successfully made.
        testthat::expect_equal(
          any_predictions_valid(prediction_table),
          !detector %in% c(except_train, except_predict)
        )
        
        # Expect that the trimmed novelty detector produces the same
        # predictions.
        prediction_table_trim <- suppressWarnings(.predict(
          trimmed_model,
          data = one_feature_data
        ))
        
        testthat::expect_equal(
          prediction_table,
          prediction_table_trim,
          ignore_attr = TRUE
        )
      }
    )
    
    # Test that the novelty detector can be used to predict the outcome.
    test_fun(
      paste0(
        "Novelty predictions can be made using the ", detector, 
        " algorithm for a one-feature, one-sample dataset."
      ),
      {
        # Expect predictions to be made.
        prediction_table <- suppressWarnings(.predict(
          model,
          data = one_feature_one_sample_data
        ))
        
        # Test that the predictions were successfully made.
        testthat::expect_equal(
          any_predictions_valid(prediction_table),
          !detector %in% c(except_train, except_predict)
        )
        
        # Expect that the trimmed novelty detector produces the same
        # predictions.
        prediction_table_trim <- suppressWarnings(.predict(
          trimmed_model,
          data = one_feature_one_sample_data
        ))
        
        testthat::expect_equal(
          prediction_table,
          prediction_table_trim,
          ignore_attr = TRUE
        )
      }
    )
    
    # Bad dataset with one sample ----------------------------------------------
    # Train the novelty detector.
    model <- suppressWarnings(test_train_novelty_detector(
      data = full_one_sample_data,
      data_bypass = full_data,
      cluster_method = "none",
      imputation_method = "simple",
      hyperparameter_list = hyperparameter_list,
      detector = detector
    ))
    
    # Test that the novelty detector can be created.
    test_fun(
      paste0(
        "Detector can not be created using the ", detector,
        " algorithm using a bad dataset."
      ), 
      {
        # Test that the novelty detector was successfully created.
        testthat::expect_false(model_is_trained(model))
      }
    )
    
    # Bad dataset without features -----------------------------------------------
    # Train the novelty detector.
    model <- suppressWarnings(test_train_novelty_detector(
      data = no_feature_data,
      data_bypass = full_data,
      imputation_method = "simple",
      hyperparameter_list = hyperparameter_list,
      detector = detector
    ))
    
    # Test that the novelty detector can be created.
    test_fun(
      paste0(
        "Detector can not be created using the ", detector,
        " algorithm using a dataset without features."
      ), 
      {
        # Test that the novelty detector was successfully created.
        testthat::expect_false(model_is_trained(model))
      }
    )
  }
}



test_all_novelty_detectors_parallel <- function(
    detectors,
    except_train = NULL,
    except_predict = NULL,
    hyperparameter_list = NULL
) {
  # This function serves to test whether packages are loaded correctly for
  # novelty detection.
  
  # Disable multithreading on data.table to prevent reduced performance due to
  # resource collisions with familiar parallelisation.
  data.table::setDTthreads(1L)
  on.exit(data.table::setDTthreads(0L), add = TRUE)
  
  # Outcome type is not important, but set to get suitable datasets.
  outcome_type <- "continuous"
  
  # Obtain data.
  full_data <- test_create_good_data(outcome_type)
  
  # Iterate over detectors.
  for (detector in detectors) {
    if (!.check_novelty_detector_available(
      detector = detector,
      as_flag = TRUE
    )) {
      next
    }
    
    # Train models -------------------------------------------------------------
    cl_train <- .test_start_cluster(n_cores = 2L)
    
    # Train the models.
    model_list <- parallel::parLapply(
      cl = cl_train,
      list(
        "1" = full_data,
        "2" = full_data
      ),
      test_train_novelty_detector,
      cluster_method = "none",
      imputation_method = "simple",
      detector = detector,
      hyperparameter_list = hyperparameter_list
    )
    
    # Test that models can be created.
    testthat::test_that(
      paste0("Novelty detector can be created using ", detector, " and a complete dataset."),
      {
        # Test that the model was successfully created.
        testthat::expect_equal(
          model_is_trained(model_list[[1L]]),
          !detector %in% except_train
        )
        testthat::expect_equal(
          model_is_trained(model_list[[2L]]),
          !detector %in% except_train
        )
      }
    )
    
    # Terminate cluster.
    cl_train <- .terminate_cluster(cl_train)
    
    # Predictions --------------------------------------------------------------
    cl_predict <- .test_start_cluster(n_cores = 2L)
    
    # Extract predictions.
    prediction_list <- parallel::parLapply(
      cl = cl_predict,
      model_list,
      .predict,
      data = full_data
    )
    
    # Test that models can be used to assess novelty.
    testthat::test_that(
      paste0("Sample predictions can be made using ", detector, " for a complete dataset."),
      {
        # Test that the predictions were successfully made.
        testthat::expect_equal(
          any_predictions_valid(prediction_list[[1L]]),
          !detector %in% c(except_train, except_predict)
        )
        testthat::expect_equal(
          any_predictions_valid(prediction_list[[2L]]),
          !detector %in% c(except_train, except_predict)
        )
      }
    )
    
    # Terminate cluster.
    cl_predict <- .terminate_cluster(cl_predict)
  }
}




test_all_vimp_methods_available <- function(vimp_methods) {
  # Create placeholder flags.
  vimp_method_available <- logical(length(vimp_methods))
  names(vimp_method_available) <- vimp_methods
  
  # Iterate over learners.
  for (vimp_method in vimp_methods) {
    # Determine if the learner is available for any outcome.
    for (outcome_type in c(
      "continuous", "binomial", "multinomial", "survival", "competing_risk"
    )) {
      # Create a familiarModel object.
      object <- methods::new(
        "familiarVimpMethod",
        outcome_type = outcome_type,
        vimp_method = vimp_method
      )
      
      # Promote the learner to the right class.
      object <- promote_vimp_method(object = object)
      
      # Check if the learner is available for the outcome.
      if (is_available(object)) {
        vimp_method_available[vimp_method] <- TRUE
        break
      }
    }
  }
  
  # Iterate over learners
  for (vimp_method in vimp_methods) {
    testthat::test_that(
      paste0(vimp_method, " is available."),
      {
        testthat::expect_true(unname(vimp_method_available[vimp_method]))
      }
    )
  }
}



test_all_vimp_methods <- function(
    vimp_methods,
    hyperparameter_list = NULL,
    debug = FALSE
) {
  if (debug) {
    test_fun <- debug_test_that
  } else {
    test_fun <- testthat::test_that
  }
  
  # Iterate over the outcome type.
  for (outcome_type in c("continuous", "binomial", "multinomial", "survival")) {
    # Obtain data.
    full_data <- test_create_good_data(outcome_type)
    full_one_sample_data <- test_create_one_sample_data(outcome_type)
    full_one_invariant_data <- test_create_invariant_good_data(outcome_type)
    one_feature_data <- test_create_single_feature_data(outcome_type)
    one_feature_invariant_data <- test_create_single_feature_invariant_data(outcome_type)
    one_feature_one_sample_data <- test_create_single_feature_one_sample_data(outcome_type)
    empty_data <- test_create_empty_data(outcome_type)
    bad_data <- test_create_bad_data(outcome_type)
    
    # Prospective datasets with (partially) missing outcomes
    fully_prospective_data <- test_create_prospective_data(outcome_type)
    mostly_prospective_data <- test_create_mostly_prospective_data(outcome_type)
    partially_prospective_data <- test_create_partially_prospective_data(outcome_type)
    
    # Iterate over variable importance methods.
    for (vimp_method in vimp_methods) {
      # Create a familiarVimpMethod object.
      object <- methods::new(
        "familiarVimpMethod",
        outcome_type = outcome_type,
        vimp_method = vimp_method
      )
      
      # Promote the learner to the right class.
      object <- promote_vimp_method(object = object)
      
      # Test if the learner is available for the current outcome_type
      if (!is_available(object)) next
      
      # Parse hyperparameter list
      hyperparameters <- c(hyperparameter_list[[outcome_type]])
      
      # Find required hyperparameters
      vimp_method_hyperparameters <- .get_preset_hyperparameters(
        vimp_method = vimp_method,
        outcome_type = outcome_type,
        names_only = TRUE
      )
      
      # Select hyperparameters that are being used, and are present in the input
      # list of hyperparameters.
      hyperparameters <- hyperparameters[intersect(vimp_method_hyperparameters, names(hyperparameters))]
      
      # Full dataset -----------------------------------------------------------
      
      # Process dataset.
      vimp_object <- prepare_vimp_object(
        data = full_data,
        vimp_method = vimp_method,
        vimp_method_parameter_list = hyperparameters,
        outcome_type = outcome_type,
        cluster_method = "none",
        imputation_method = "simple"
      )
      
      test_fun(
        paste0(
          "Variable importance can be computed for ", outcome_type, " with the ",
          vimp_method, " using a complete dataset."
        ),
        {
          vimp_table <- suppressWarnings(get_vimp_table(.vimp(
            vimp_object,
            full_data
          )))
          
          # Get the number of features
          n_features <- get_n_features(full_data)
          
          # Expect that the vimp table is not empty.
          testthat::expect_true(nrow(vimp_table) > 0L && nrow(vimp_table) <= n_features)
          
          # Expect that the names in the vimp table correspond to those of the
          # features.
          testthat::expect_in(vimp_table$name, get_feature_columns(full_data))
          
          # Expect that not all features have the same rank.
          if (nrow(vimp_table) > 1L) {
            testthat::expect_true(!all(vimp_table$rank == 1L))
          }
        }
      )
      
      test_fun(
        paste0(
          "Variable importance can be computed for ", outcome_type, " with the ",
          vimp_method, " using a complete dataset with one invariant feature."
        ),
        {
          vimp_table <- suppressWarnings(get_vimp_table(.vimp(
            vimp_object,
            full_one_invariant_data
          )))
          
          # Get the number of features
          n_features <- get_n_features(full_one_invariant_data)
          
          # Expect that the vimp table is not empty.
          testthat::expect_true(nrow(vimp_table) > 0L && nrow(vimp_table) <= n_features)
          
          # Expect that the names in the vimp table correspond to those of the
          # features.
          testthat::expect_in(vimp_table$name, get_feature_columns(full_one_invariant_data))
        }
      )
      
      test_fun(
        paste0(
          "Variable importance cannot be computed for ", outcome_type, 
          " with the ", vimp_method, " using an empty dataset."
        ),
        {
          vimp_table <- suppressWarnings(get_vimp_table(.vimp(
            vimp_object,
            empty_data
          )))
          
          # Expect that the vimp table has two rows.
          testthat::expect_true(is_empty(vimp_table))
        }
      )
      
      test_fun(
        paste0(
          "Variable importance cannot be computed for ", outcome_type, 
          " with the ", vimp_method, " using a bad dataset."
        ),
        {
          vimp_table <- suppressWarnings(get_vimp_table(.vimp(
            vimp_object,
            bad_data
          )))
          
          # Expect that the vimp table has two rows.
          testthat::expect_true(is_empty(vimp_table))
        }
      )
      
      test_fun(
        paste0(
          "Variable importance cannot be computed for ", outcome_type, 
          " with the ", vimp_method, " using a one-sample dataset."
        ),
        {
          vimp_table <- suppressWarnings(get_vimp_table(.vimp(
            vimp_object,
            full_one_sample_data
          )))
          
          # Expect that the vimp table has two rows.
          testthat::expect_true(is_empty(vimp_table))
        }
      )
      
      # One-feature dataset ----------------------------------------------------
      
      # Process dataset.
      vimp_object <- prepare_vimp_object(
        data = one_feature_data,
        vimp_method = vimp_method,
        vimp_method_parameter_list = hyperparameters,
        outcome_type = outcome_type,
        cluster_method = "none",
        imputation_method = "simple"
      )
      
      test_fun(
        paste0(
          "Variable importance can be computed for ", outcome_type, " with the ",
          vimp_method, " using a one-feature dataset."
        ), 
        {
          vimp_table <- suppressWarnings(get_vimp_table(.vimp(
            vimp_object,
            one_feature_data
          )))
          
          # Expect that the vimp table is not empty.
          testthat::expect_equal(nrow(vimp_table), 1L)
          
          # Expect that the names in the vimp table correspond to those of the
          # features.
          testthat::expect_in(vimp_table$name, get_feature_columns(one_feature_data))
        }
      )
      
      test_fun(
        paste0(
          "Variable importance cannot be computed for ", outcome_type, " with the ",
          vimp_method, " using a one-feature dataset with an invariant feature."
        ),
        {
          vimp_table <- suppressWarnings(get_vimp_table(.vimp(
            vimp_object,
            one_feature_invariant_data
          )))
          
          # Expect that the vimp table is empty.
          testthat::expect_true(is_empty(vimp_table))
        }
      )
      
      test_fun(
        paste0(
          "Variable importance cannot be computed for ", outcome_type,
          " with the ", vimp_method, " using a one-feature, one-sample dataset."
        ),
        {
          vimp_table <- suppressWarnings(get_vimp_table(.vimp(
            vimp_object,
            one_feature_one_sample_data
          )))
          
          # Expect that the vimp table is empty.
          testthat::expect_true(is_empty(vimp_table))
        }
      )
      
      if (outcome_type %in% c("survival", "competing_risk")) {
        # Dataset without censored instances -----------------------------------
        
        no_censoring_data <- test_create_good_data_without_censoring(outcome_type)
        
        # Process dataset.
        vimp_object <- prepare_vimp_object(
          data = no_censoring_data,
          vimp_method = vimp_method,
          vimp_method_parameter_list = hyperparameters,
          outcome_type = outcome_type,
          cluster_method = "none",
          imputation_method = "simple"
        )
        
        test_fun(
          paste0(
            "Variable importance can be computed for ", outcome_type, " with the ",
            vimp_method, " using a dataset without censoring."
          ),
          {
            vimp_table <- suppressWarnings(get_vimp_table(.vimp(
              vimp_object,
              no_censoring_data
            )))
            
            # Get the number of features
            n_features <- get_n_features(full_data)
            
            # Expect that the vimp table is not empty..
            testthat::expect_true(nrow(vimp_table) > 0L && nrow(vimp_table) <= n_features)
            
            # Expect that the names in the vimp table correspond to those of the
            # features.
            testthat::expect_in(vimp_table$name, get_feature_columns(full_data))
          }
        )
        
        # Dataset with one censored instance -----------------------------------
        
        one_censored_data <- test_create_good_data_one_censored(outcome_type)
        
        # Process dataset.
        vimp_object <- prepare_vimp_object(
          data = one_censored_data,
          vimp_method = vimp_method,
          vimp_method_parameter_list = hyperparameters,
          outcome_type = outcome_type,
          cluster_method = "none",
          imputation_method = "simple"
        )
        
        test_fun(
          paste0(
            "Variable importance can be computed for ", outcome_type, " with the ",
            vimp_method, " using a dataset with one censored instance."
          ),
          {
            vimp_table <- suppressWarnings(get_vimp_table(.vimp(
              vimp_object,
              one_censored_data
            )))
            
            # Get the number of features
            n_features <- get_n_features(full_data)
            
            # Expect that the vimp table is not empty..
            testthat::expect_true(nrow(vimp_table) > 0L && nrow(vimp_table) <= n_features)
            
            # Expect that the names in the vimp table correspond to those of the
            # features.
            testthat::expect_in(vimp_table$name, get_feature_columns(full_data))
          }
        )
        
        # Dataset with few censored instances ----------------------------------
        few_censored_data <- test_create_good_data_few_censored(outcome_type)
        
        # Process dataset.
        vimp_object <- prepare_vimp_object(
          data = few_censored_data,
          vimp_method = vimp_method,
          vimp_method_parameter_list = hyperparameters,
          outcome_type = outcome_type,
          cluster_method = "none",
          imputation_method = "simple"
        )
        
        test_fun(
          paste0(
            "Variable importance can be computed for ", outcome_type, " with the ",
            vimp_method, " using a dataset with few censored instances."
          ),
          {
            vimp_table <- suppressWarnings(get_vimp_table(.vimp(
              vimp_object,
              few_censored_data
            )))
            
            # Get the number of features
            n_features <- get_n_features(full_data)
            
            # Expect that the vimp table is not empty..
            testthat::expect_true(nrow(vimp_table) > 0L && nrow(vimp_table) <= n_features)
            
            # Expect that the names in the vimp table correspond to those of the
            # features.
            testthat::expect_in(vimp_table$name, get_feature_columns(full_data))
          }
        )
      }
      
      # Fully prospective dataset ----------------------------------------------
      
      # Set up the vimp object.
      vimp_object <- prepare_vimp_object(
        data = full_data,
        vimp_method = vimp_method,
        vimp_method_parameter_list = hyperparameters,
        outcome_type = outcome_type,
        cluster_method = "none",
        imputation_method = "simple"
      )
      
      test_fun(
        paste0(
          "Variable importance cannot be computed for ", outcome_type, " with the ",
          vimp_method, " for a fully prospective dataset."
        ),
        {
          vimp_table <- suppressWarnings(get_vimp_table(.vimp(
            vimp_object,
            fully_prospective_data
          )))
          
          # Expect that the vimp table is empty.
          testthat::expect_true(is_empty(vimp_table))
        }
      )
      
      # Mostly prospective dataset ---------------------------------------------
      
      test_fun(
        paste0(
          "Variable importance cannot be computed for ", outcome_type, " with the ", vimp_method,
          " for an almost fully prospective dataset, where outcome is known for just a single sample."
        ),
        {
          vimp_table <- suppressWarnings(get_vimp_table(.vimp(
            vimp_object,
            mostly_prospective_data
          )))
          
          # Expect that the vimp table is empty.
          testthat::expect_true(is_empty(vimp_table))
        }
      )
      
      # Partially prospective dataset ------------------------------------------
      
      test_fun(
        paste0(
          "Variable importance can be computed for ", outcome_type, " with the ", vimp_method,
          " for a partially prospective dataset, where outcome is known for most samples."
        ),
        {
          vimp_table <- suppressWarnings(get_vimp_table(.vimp(
            vimp_object,
            partially_prospective_data
          )))
          
          # Get the number of features
          n_features <- get_n_features(full_data)
          
          # Expect that the vimp table is not empty.
          testthat::expect_true(nrow(vimp_table) > 0L && nrow(vimp_table) <= n_features)
          
          # Expect that the names in the vimp table correspond to those of the
          # features.
          testthat::expect_in(vimp_table$name, get_feature_columns(partially_prospective_data))
        }
      )
    }
  }
}



test_all_vimp_methods_parallel <- function(
    vimp_methods,
    hyperparameter_list = NULL
) {
  # This function serves to test whether packages are loaded correctly for
  # assessing variable importance.
  
  # Disable randomForestSRC OpenMP core use.
  options(rf.cores = 1L)
  on.exit(options(rf.cores = -1L), add = TRUE)
  
  # Disable multithreading on data.table to prevent reduced performance due to
  # resource collisions with familiar parallelisation.
  data.table::setDTthreads(1L)
  on.exit(data.table::setDTthreads(0L), add = TRUE)
  
  # Iterate over the outcome type.
  for (outcome_type in c("continuous", "binomial", "multinomial", "survival")) {
    # Obtain data.
    full_data <- test_create_good_data(outcome_type)
    
    for (vimp_method in vimp_methods) {
      if (!.check_vimp_outcome_type(
        method = vimp_method,
        outcome_type = outcome_type,
        as_flag = TRUE
      )) {
        next
      }
      
      # Parse hyperparameter list
      hyperparameters <- c(hyperparameter_list[[outcome_type]])
      
      # Find required hyperparameters
      vimp_method_hyperparameters <- .get_preset_hyperparameters(
        vimp_method = vimp_method,
        outcome_type = outcome_type,
        names_only = TRUE
      )
      
      # Select hyperparameters that are being used, and are present in the input
      # list of hyperparameters.
      hyperparameters <- hyperparameters[intersect(vimp_method_hyperparameters, names(hyperparameters))]
      
      # Prepare vimp object ----------------------------------------------------
      cl_train <- .test_start_cluster(n_cores = 2L)
      
      # Prepare the variable importance objects.
      vimp_object_list <- parallel::parLapply(
        cl = cl_train,
        list(
          "1" = full_data, 
          "2" = full_data
        ),
        prepare_vimp_object,
        vimp_method = vimp_method,
        vimp_method_parameter_list = hyperparameters,
        outcome_type = outcome_type,
        cluster_method = "none",
        imputation_method = "simple"
      )
      
      # Terminate cluster.
      cl_train <- .terminate_cluster(cl_train)
      
      # Variable importance ----------------------------------------------------
      cl_vimp <- .test_start_cluster(n_cores = 2L)
      
      # Extract variable importance data.
      vimp_table_list <- parallel::parLapply(
        cl = cl_vimp,
        vimp_object_list,
        .vimp,
        data = full_data
      )
      
      # Extract the actual tables.
      vimp_table_list <- lapply(vimp_table_list, get_vimp_table)
      
      # Test that the model has variable importance.
      testthat::test_that(
        paste0(
          "Variable importance method produces variable importance for ",
          outcome_type, " and ", vimp_method, " for the complete dataset."
        ),
        {
          # Get the number of features
          n_features <- get_n_features(full_data)
          
          # Expect that the vimp table has two rows.
          testthat::expect_true(
            nrow(vimp_table_list[[1L]]) > 0L && nrow(vimp_table_list[[1L]]) <= n_features
          )
          testthat::expect_true(
            nrow(vimp_table_list[[2L]]) > 0L && nrow(vimp_table_list[[2L]]) <= n_features
          )
          
          # Expect that the names in the vimp table correspond to those of the
          # features.
          testthat::expect_true(
            all(vimp_table_list[[1L]]$name %in% get_feature_columns(full_data))
          )
          testthat::expect_true(
            all(vimp_table_list[[2L]]$name %in% get_feature_columns(full_data))
          )
        }
      )
      
      # Terminate cluster.
      cl_vimp <- .terminate_cluster(cl_vimp)
    }
  }
}



test_all_metrics_available <- function(metrics) {
  # Create placeholder flags.
  metric_available <- logical(length(metrics))
  names(metric_available) <- metrics
  
  # Iterate over metrics
  for (metric in metrics) {
    # Determine if the metric is available for any outcome.
    for (outcome_type in c(
      "continuous", "binomial", "multinomial", "survival", "competing_risk"
    )) {
      # Create a metric object
      object <- as_metric(
        metric = metric,
        outcome_type = outcome_type
      )
      
      # Check if the learner is available for the outcome.
      if (is_available(object)) {
        metric_available[metric] <- TRUE
        break
      }
    }
  }
  
  # Iterate over learners
  for (metric in metrics) {
    testthat::test_that(
      paste0(metric, " is available."),
      {
        testthat::expect_equal(unname(metric_available[metric]), TRUE)
      }
    )
  }
}



test_all_metrics <- function(
    metrics,
    not_available_single_sample = FALSE,
    not_available_all_samples_identical = FALSE,
    not_available_all_predictions_identical = FALSE,
    debug = FALSE
) {
  if (debug) {
    test_fun <- debug_test_that
  } else {
    test_fun <- testthat::test_that
  }
  
  # Iterate over the outcome type.
  for (outcome_type in c("continuous", "binomial", "multinomial", "survival")) {
    # Obtain data.
    full_data <- test_create_good_data(outcome_type)
    identical_sample_data <- test_create_all_identical_data(outcome_type)
    full_one_sample_data <- test_create_one_sample_data(outcome_type)
    one_feature_data <- test_create_single_feature_data(outcome_type)
    one_feature_one_sample_data <- test_create_single_feature_one_sample_data(outcome_type)
    one_feature_invariant_data <- test_create_single_feature_invariant_data(outcome_type)
    empty_data <- test_create_empty_data(outcome_type)
    bad_data <- test_create_bad_data(outcome_type)
    
    # Data with different degrees of censoring.
    no_censoring_data <- test_create_good_data_without_censoring(outcome_type)
    one_censored_data <- test_create_good_data_one_censored(outcome_type)
    few_censored_data <- test_create_good_data_few_censored(outcome_type)
    
    # Prospective datasets with (partially) missing outcomes
    fully_prospective_data <- test_create_prospective_data(outcome_type)
    mostly_prospective_data <- test_create_mostly_prospective_data(outcome_type)
    partially_prospective_data <- test_create_partially_prospective_data(outcome_type)
    
    # Set exceptions per outcome type.
    .not_available_single_sample <- not_available_single_sample
    if (is.character(.not_available_single_sample)) {
      .not_available_single_sample <- any(
        .not_available_single_sample == outcome_type
      )
    }
    
    .not_available_all_samples_identical <- not_available_all_samples_identical
    if (is.character(.not_available_all_samples_identical)) {
      .not_available_all_samples_identical <- any(
        .not_available_all_samples_identical == outcome_type
      )
    }
    
    .not_available_all_predictions_identical <- not_available_all_predictions_identical
    if (is.character(.not_available_all_predictions_identical)) {
      .not_available_all_predictions_identical <- any(
        .not_available_all_predictions_identical == outcome_type
      )
    }
    
    # Iterate over metrics
    for (metric in metrics) {
      # Check if the metric is available for the current outcome type, and skip
      # otherwise.
      if (!.check_metric_outcome_type(
        metric = metric, 
        outcome_type = outcome_type, 
        as_flag = TRUE
      )) {
        break
      }
      
      # Parse hyperparameter list
      hyperparameters <- list(
        "sign_size" = get_n_features(full_data),
        "family" = switch(
          outcome_type,
          "continuous" = "gaussian",
          "binomial" = "logistic",
          "multinomial" = "multinomial",
          "survival" = "cox"
        )
      )
      
      # Parse hyperparameter list for glmnet test.
      hyperparameters_lasso <- list(
        "sign_size" = get_n_features(full_data),
        "family" = switch(
          outcome_type,
          "continuous" = "gaussian",
          "binomial" = "binomial",
          "multinomial" = "multinomial",
          "survival" = "cox"
        )
      )
      
      # Full dataset -----------------------------------------------------------
      
      # Train the model.
      model <- suppressWarnings(test_train(
        data = full_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = "glm",
        time_max = 3.5
      )
      )
      
      # Create metric object
      metric_object <- as_metric(
        metric = metric,
        outcome_type = outcome_type
      )
      
      # Test that metric values can be computed for the full model.
      test_fun(
        paste0(
          "1A. Model performance for ", outcome_type, " outcomes can be assessed by the ",
          metric_object@name, " (", metric_object@metric, ") metric for a complete dataset."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model, data = full_data))
          
          # Test that the predictions were successfully made.
          testthat::expect_true(any_predictions_valid(prediction_table))
          
          if (outcome_type %in% c("binomial", "multinomial")) {
            # Expect that the class levels are the same as those in the model.
            testthat::expect_equal(
              get_outcome_class_levels(prediction_table),
              get_outcome_class_levels(model)
            )
          }
          
          # Compute a score.
          score <- compute_metric_score(
            metric = metric_object,
            data = prediction_table
          )
          
          # Compute an objective score.
          objective_score <- compute_objective_score(
            metric = metric_object,
            data = prediction_table,
            object = model
          )
          
          # Expect that the score is a finite, non-missing number.
          testthat::expect_true(data.table::between(
            score,
            lower = metric_object@value_range[1L],
            upper = metric_object@value_range[2L]
          ))
          
          # Expect that the objective score is a non-missing number in the range
          # [-1, 1].
          testthat::expect_true(data.table::between(
            objective_score,
            lower = -1.0,
            upper = 1.0
          ))
        }
      )
      
      if (outcome_type %in% c("survival", "competing_risk")) {
        # Test that metric values can be computed for the full model, but with
        # data without censored instances.
        test_fun(
          paste0(
            "1B. Model performance for ", outcome_type, " outcomes can be assessed by the ",
            metric_object@name, " (", metric_object@metric, ") metric for a data set without censoring."
          ),
          {
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = no_censoring_data
            ))
            
            # Compute a score.
            score <- compute_metric_score(
              metric = metric_object,
              data = prediction_table
            )
            
            # Compute an objective score.
            objective_score <- compute_objective_score(
              metric = metric_object,
              data = prediction_table,
              object = model
            )
            
            # Expect that the score is a finite, non-missing number.
            testthat::expect_true(data.table::between(
              score,
              lower = metric_object@value_range[1L],
              upper = metric_object@value_range[2L]
            ))
            
            # Expect that the objective score is a non-missing number in the
            # range [-1, 1].
            testthat::expect_true(data.table::between(
              objective_score,
              lower = -1.0,
              upper = 1.0
            ))
          }
        )
        
        # Test that metric values can be computed for the full model, but with
        # data with one censored instance.
        test_fun(
          paste0(
            "1C. Model performance for ", outcome_type, " outcomes can be assessed by the ",
            metric_object@name, " (", metric_object@metric, ") metric for a dataset ",
            "with one censored instances."
          ),
          {
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model,
              data = one_censored_data
            ))
            
            # Test that the predictions were successfully made.
            testthat::expect_true(any_predictions_valid(prediction_table))
            
            # Compute a score.
            score <- compute_metric_score(
              metric = metric_object,
              data = prediction_table
            )
            
            # Compute an objective score.
            objective_score <- compute_objective_score(
              metric = metric_object,
              data = prediction_table,
              object = model
            )
            
            # Expect that the score is a finite, non-missing number.
            testthat::expect_true(data.table::between(
              score,
              lower = metric_object@value_range[1L],
              upper = metric_object@value_range[2L]
            ))
            
            # Expect that the objective score is a non-missing number in the
            # range [-1, 1].
            testthat::expect_true(data.table::between(
              objective_score,
              lower = -1.0,
              upper = 1.0
            ))
          }
        )
        
        # Test that metric values can be computed for the full model, but with
        # data with few censored instances.
        test_fun(
          paste0(
            "1D. Model performance for ", outcome_type, " outcomes can be assessed by the ",
            metric_object@name, " (", metric_object@metric, ") metric for a dataset ",
            "with few censored samples."
          ),
          {
            # Expect predictions to be made.
            prediction_table <- suppressWarnings(.predict(
              model, 
              data = few_censored_data
            ))
            
            # Test that the predictions were successfully made.
            testthat::expect_true(any_predictions_valid(prediction_table))
            
            # Compute a score.
            score <- compute_metric_score(
              metric = metric_object,
              data = prediction_table
            )
            
            # Compute an objective score.
            objective_score <- compute_objective_score(
              metric = metric_object,
              data = prediction_table,
              object = model
            )
            
            # Expect that the score is a finite, non-missing number.
            testthat::expect_true(data.table::between(
              score,
              lower = metric_object@value_range[1L],
              upper = metric_object@value_range[2L]
            ))
            
            # Expect that the objective score is a non-missing number in the
            # range [-1, 1].
            testthat::expect_true(data.table::between(
              objective_score,
              lower = -1.0,
              upper = 1.0
            ))
          }
        )
      }
      
      # Test that metric values can be computed for the full model.
      test_fun(
        paste0(
          "1E. Model performance for ", outcome_type, " outcomes can be assessed by the ",
          metric_object@name, " (", metric_object@metric, ") metric for a dataset ",
          "with some missing outcome values."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model, 
            data = partially_prospective_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_true(any_predictions_valid(prediction_table))
          
          if (outcome_type %in% c("binomial", "multinomial")) {
            # Expect that the class levels are the same as those in the model.
            testthat::expect_equal(
              get_outcome_class_levels(prediction_table),
              get_outcome_class_levels(model)
            )
          }
          
          # Compute a score.
          score <- compute_metric_score(
            metric = metric_object,
            data = prediction_table
          )
          
          # Compute an objective score.
          objective_score <- compute_objective_score(
            metric = metric_object,
            data = prediction_table,
            object = model
          )
          
          # Expect that the score is a finite, non-missing number.
          testthat::expect_true(data.table::between(
            score,
            lower = metric_object@value_range[1L],
            upper = metric_object@value_range[2L]
          ))
          
          # Expect that the objective score is a non-missing number in the range
          # [-1, 1].
          testthat::expect_true(data.table::between(
            objective_score,
            lower = -1.0,
            upper = 1.0
          ))
        }
      )
      
      # Test for a dataset with fully missing outcomes.
      test_fun(
        paste0(
          "1F. Model performance for ", outcome_type, " outcomes cannot be assessed by the ",
          metric_object@name, " (", metric_object@metric, ") metric for a dataset ",
          "that misses observed outcomes."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model,
            data = fully_prospective_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_true(any_predictions_valid(prediction_table))
          
          if (outcome_type %in% c("binomial", "multinomial")) {
            # Expect that the class levels are the same as those in the model.
            testthat::expect_equal(
              get_outcome_class_levels(prediction_table),
              get_outcome_class_levels(model)
            )
          }
          
          # Compute a score.
          score <- compute_metric_score(
            metric = metric_object,
            data = prediction_table
          )
          
          # Compute an objective score.
          objective_score <- compute_objective_score(
            metric = metric_object,
            data = prediction_table,
            object = model
          )
          
          # Expect that the score is NA.
          testthat::expect_true(is.na(score))
          testthat::expect_true(is.na(objective_score))
        }
      )
      
      # Test that metric values can/cannot be computed for a one-sample dataset.
      test_fun(
        paste0(
          "2A. Model performance for ", outcome_type, " outcomes ",
          ifelse(.not_available_single_sample, "cannot", "can"),
          " be assessed by the ", metric_object@name,
          " (", metric_object@metric, ") metric for a one-sample data set."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model,
            data = full_one_sample_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_true(any_predictions_valid(prediction_table))
          
          if (outcome_type %in% c("binomial", "multinomial")) {
            # Expect that the class levels are the same as those in the model.
            testthat::expect_equal(
              get_outcome_class_levels(prediction_table),
              get_outcome_class_levels(model)
            )
          }
          
          # Compute a score.
          score <- compute_metric_score(
            metric = metric_object,
            data = prediction_table
          )
          
          # Compute an objective score.
          objective_score <- compute_objective_score(
            metric = metric_object,
            data = prediction_table,
            object = model
          )
          
          # Expect that the score is a finite, non-missing number, and NA
          # otherwise.
          if (.not_available_single_sample) {
            testthat::expect_true(is.na(score))
            
          } else {
            testthat::expect_true(data.table::between(
              score,
              lower = metric_object@value_range[1L],
              upper = metric_object@value_range[2L]
            ))
          }
          
          # Expect that the objective score is a non-missing number in the range
          # [-1, 1] and NA otherwise.
          if (.not_available_single_sample) {
            testthat::expect_true(is.na(objective_score))
          } else {
            testthat::expect_true(data.table::between(
              objective_score,
              lower = -1.0,
              upper = 1.0
            ))
          }
        }
      )
      
      test_fun(
        paste0(
          "2B. Model performance for ", outcome_type, " outcomes ",
          ifelse(.not_available_single_sample, "cannot", "can"),
          " be assessed by the ", metric_object@name,
          " (", metric_object@metric, ") metric for a dataset with only ",
          "one instance with known outcomes."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model, 
            data = mostly_prospective_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_true(any_predictions_valid(prediction_table))
          
          if (outcome_type %in% c("binomial", "multinomial")) {
            # Expect that the class levels are the same as those in the model.
            testthat::expect_equal(
              get_outcome_class_levels(prediction_table),
              get_outcome_class_levels(model)
            )
          }
          
          # Compute a score.
          score <- compute_metric_score(
            metric = metric_object,
            data = prediction_table
          )
          
          # Compute an objective score.
          objective_score <- compute_objective_score(
            metric = metric_object,
            data = prediction_table,
            object = model
          )
          
          # Expect that the score is a finite, non-missing number, and NA
          # otherwise.
          if (.not_available_single_sample) {
            testthat::expect_true(is.na(score))
            
          } else {
            testthat::expect_true(data.table::between(
              score,
              lower = metric_object@value_range[1L],
              upper = metric_object@value_range[2L]
            ))
          }
          
          # Expect that the objective score is a non-missing number in the range
          # [-1, 1] and NA otherwise.
          if (.not_available_single_sample) {
            testthat::expect_true(is.na(objective_score))
            
          } else {
            testthat::expect_true(data.table::between(
              objective_score,
              lower = -1.0,
              upper = 1.0
            ))
          }
        }
      )
      
      
      # Test that metric values cannot be computed for the empty model.
      test_fun(
        paste0(
          "3. Model performance for ", outcome_type, " outcomes cannot be assessed by the ",
          metric_object@name, " (", metric_object@metric, ") metric for an empty dataset."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model,
            data = empty_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_false(any_predictions_valid(prediction_table))
          
          # Compute a score.
          score <- compute_metric_score(
            metric = metric_object,
            data = prediction_table
          )
          
          # Compute an objective score.
          objective_score <- compute_objective_score(
            metric = metric_object,
            data = prediction_table,
            object = model
          )
          
          # Expect that the score is NA.
          testthat::expect_true(is.na(score))
          testthat::expect_true(is.na(objective_score))
        }
      )
      
      # Test that metric values can be computed for a dataset where are samples
      # identical.
      test_fun(paste0(
        "4. Model performance for ", outcome_type, " outcomes ",
        ifelse(.not_available_all_samples_identical, "cannot", "can"),
        " be assessed by the ",
        metric_object@name, " (", metric_object@metric, ") metric for a dataset ",
        "with identical samples."
      ),
      {
        # Expect predictions to be made.
        prediction_table <- suppressWarnings(.predict(
          model,
          data = identical_sample_data
        ))
        
        # Test that the predictions were successfully made.
        testthat::expect_true(any_predictions_valid(prediction_table))
        
        # Compute a score.
        score <- compute_metric_score(
          metric = metric_object,
          data = prediction_table
        )
        
        # Compute an objective score.
        objective_score <- compute_objective_score(
          metric = metric_object,
          data = prediction_table,
          object = model
        )
        
        # Expect that the score is a finite, non-missing number.
        if (.not_available_all_samples_identical) {
          testthat::expect_true(is.na(score))
        } else {
          testthat::expect_true(data.table::between(
            score,
            lower = metric_object@value_range[1L],
            upper = metric_object@value_range[2L]
          ))
        }
        
        # Expect that the objective score is a non-missing number in the range
        # [-1, 1].
        if (.not_available_all_samples_identical) {
          testthat::expect_true(is.na(objective_score))
        } else {
          testthat::expect_true(data.table::between(
            objective_score,
            lower = -1.0,
            upper = 1.0
          ))
        }
      }
      )
      
      # One-feature data set ---------------------------------------------------
      # Train the model.
      model <- suppressWarnings(test_train(
        data = one_feature_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = "glm",
        time_max = 3.5
      ))
      
      # Create metric object
      metric_object <- as_metric(
        metric = metric,
        outcome_type = outcome_type
      )
      
      # Test that metric values can be computed for the one-feature model.
      test_fun(
        paste0(
          "5. Model performance for ", outcome_type, " outcomes can be assessed by the ",
          metric_object@name, " (", metric_object@metric, ") metric for a one-feature dataset."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model,
            data = one_feature_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_true(any_predictions_valid(prediction_table))
          
          if (outcome_type %in% c("binomial", "multinomial")) {
            # Expect that the class levels are the same as those in the model.
            testthat::expect_equal(
              get_outcome_class_levels(prediction_table),
              get_outcome_class_levels(model)
            )
          }
          
          # Compute a score.
          score <- compute_metric_score(
            metric = metric_object,
            data = prediction_table
          )
          
          # Compute an objective score.
          objective_score <- compute_objective_score(
            metric = metric_object,
            data = prediction_table,
            object = model
          )
          
          # Expect that the score is a finite, non-missing number.
          testthat::expect_true(data.table::between(
            score,
            lower = metric_object@value_range[1L],
            upper = metric_object@value_range[2L]
          ))
          
          # Expect that the objective score is a non-missing number in the range
          # [-1, 1].
          testthat::expect_true(data.table::between(
            objective_score,
            lower = -1.0,
            upper = 1.0
          ))
        }
      )
      
      # Test that metric values cannot be computed for a one-sample dataset.
      test_fun(
        paste0(
          "6. Model performance for ", outcome_type, " outcomes ",
          ifelse(.not_available_single_sample, "cannot", "can"),
          " be assessed by the ", metric_object@name,
          " (", metric_object@metric, ") metric for a one-feature, one-sample dataset."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model,
            data = one_feature_one_sample_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_true(any_predictions_valid(prediction_table))
          
          if (outcome_type %in% c("binomial", "multinomial")) {
            # Expect that the class levels are the same as those in the model.
            testthat::expect_equal(
              get_outcome_class_levels(prediction_table),
              get_outcome_class_levels(model)
            )
          }
          
          # Compute a score.
          score <- compute_metric_score(
            metric = metric_object,
            data = prediction_table
          )
          
          # Compute an objective score.
          objective_score <- compute_objective_score(
            metric = metric_object,
            data = prediction_table,
            object = model
          )
          
          # Expect that the score is a finite, non-missing number, and NA
          # otherwise.
          if (.not_available_single_sample) {
            testthat::expect_true(is.na(score))
            
          } else {
            testthat::expect_true(data.table::between(
              score,
              lower = metric_object@value_range[1L],
              upper = metric_object@value_range[2L]
            ))
          }
          
          # Expect that the objective score is a non-missing number in the range
          # [-1, 1] and NA otherwise.
          if (.not_available_single_sample) {
            testthat::expect_true(is.na(objective_score))
            
          } else {
            testthat::expect_true(data.table::between(
              objective_score,
              lower = -1.0,
              upper = 1.0
            ))
          }
        }
      )
      
      # Test that metric values can be computed for the one-feature model with
      # invariant predicted outcomes for all samples.
      test_fun(
        paste0(
          "7. Model performance for ", outcome_type, " outcomes ",
          ifelse(.not_available_all_predictions_identical, "can", "cannot"),
          " be assessed by the ",
          metric_object@name, " (", metric_object@metric, ") metric for a ",
          "one-feature dataset with identical predictions."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model,
            data = one_feature_invariant_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_true(any_predictions_valid(prediction_table))
          
          if (outcome_type %in% c("binomial", "multinomial")) {
            # Expect that the class levels are the same as those in the model.
            testthat::expect_equal(
              get_outcome_class_levels(prediction_table),
              get_outcome_class_levels(model)
            )
          }
          
          # Compute a score.
          score <- compute_metric_score(
            metric = metric_object,
            data = prediction_table
          )
          
          # Compute an objective score.
          objective_score <- compute_objective_score(
            metric = metric_object,
            data = prediction_table,
            object = model
          )
          
          # Expect that the score is a finite, non-missing number, and NA
          # otherwise.
          if (.not_available_all_predictions_identical) {
            testthat::expect_true(is.na(score))
            
          } else {
            testthat::expect_true(data.table::between(
              score,
              lower = metric_object@value_range[1L],
              upper = metric_object@value_range[2L]
            ))
          }
          
          # Expect that the objective score is a non-missing number in the range
          # [-1, 1] and NA otherwise.
          if (.not_available_all_predictions_identical) {
            testthat::expect_true(is.na(objective_score))
            
          } else {
            testthat::expect_true(data.table::between(
              objective_score,
              lower = -1.0,
              upper = 1.0
            ))
          }
        }
      )
      
      # Bad dataset ------------------------------------------------------------
      # Train the model.
      model <- suppressWarnings(test_train(
        data = bad_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = "glm",
        time_max = 3.5
      ))
      
      # Create metric object
      metric_object <- as_metric(
        metric = metric,
        outcome_type = outcome_type
      )
      
      # Test that metric values can be computed for the one-feature model with
      # invariant predicted outcomes for all samples.
      test_fun(
        paste0(
          "8. Model performance for ", outcome_type, " outcomes cannot be assessed by the ",
          metric_object@name, " (", metric_object@metric, ") metric for a bad ",
          "dataset where the model fails to train."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model, 
            data = bad_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_false(any_predictions_valid(prediction_table))
          
          # Compute a score.
          score <- compute_metric_score(
            metric = metric_object,
            data = prediction_table
          )
          
          # Compute an objective score.
          objective_score <- compute_objective_score(
            metric = metric_object,
            data = prediction_table,
            object = model
          )
          
          # Expect that the score is NA.
          testthat::expect_true(is.na(score))
          testthat::expect_true(is.na(objective_score))
        }
      )
      
      # Without any valid predictions ------------------------------------------
      model <- suppressWarnings(test_train(
        data = full_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters_lasso,
        learner = "lasso_test_all_fail",
        time_max = 3.5
      ))
      
      # Create metric object
      metric_object <- as_metric(
        metric = metric,
        outcome_type = outcome_type
      )
      
      test_fun(
        paste0(
          "9. Model performance for ", outcome_type, " outcomes cannot be assessed by the ",
          metric_object@name, " (", metric_object@metric, ") metric for a model ",
          "that only produces invalid predictions."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model, 
            data = full_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_false(any_predictions_valid(prediction_table))
          
          if (outcome_type %in% c("binomial", "multinomial")) {
            # Expect that the class levels are the same as those in the model.
            testthat::expect_equal(
              get_outcome_class_levels(prediction_table),
              get_outcome_class_levels(model)
            )
          }
          
          # Compute a score.
          score <- compute_metric_score(
            metric = metric_object,
            data = prediction_table
          )
          
          # Compute an objective score.
          objective_score <- compute_objective_score(
            metric = metric_object,
            data = prediction_table,
            object = model
          )
          
          # Expect that the score is NA.
          testthat::expect_true(is.na(score))
          testthat::expect_true(is.na(objective_score))
        }
      )
      
      # With some invalid predictions ------------------------------------------
      model <- suppressWarnings(test_train(
        data = full_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters_lasso,
        learner = "lasso_test_some_fail",
        time_max = 3.5
      ))
      
      # Create metric object
      metric_object <- as_metric(
        metric = metric,
        outcome_type = outcome_type
      )
      
      test_fun(
        paste0(
          "10. Model performance for ", outcome_type, " outcomes can be assessed by the ",
          metric_object@name, " (", metric_object@metric, ") metric for a model ",
          "that produces some invalid predictions."
        ),
        {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(
            model, 
            data = full_data
          ))
          
          # Test that the predictions were successfully made.
          testthat::expect_true(any_predictions_valid(prediction_table))
          
          if (outcome_type %in% c("binomial", "multinomial")) {
            # Expect that the class levels are the same as those in the model.
            testthat::expect_equal(
              get_outcome_class_levels(prediction_table),
              get_outcome_class_levels(model)
            )
          }
          
          # Compute a score.
          score <- compute_metric_score(
            metric = metric_object,
            data = prediction_table
          )
          
          # Compute an objective score.
          objective_score <- compute_objective_score(
            metric = metric_object,
            data = prediction_table,
            object = model
          )
          
          # Expect that the score is a finite, non-missing number.
          testthat::expect_true(data.table::between(
            score,
            lower = metric_object@value_range[1L],
            upper = metric_object@value_range[2L]
          ))
          
          # Expect that the objective score is a non-missing number in the range
          # [-1, 1].
          testthat::expect_true(data.table::between(
            objective_score,
            lower = -1.0,
            upper = 1.0
          ))
        }
      )
    }
  }
}



test_hyperparameter_optimisation <- function(
    vimp_methods = NULL,
    learners = NULL,
    outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
    not_available_no_samples = TRUE,
    n_max_bootstraps = 25L,
    n_max_optimisation_steps = 3L,
    n_max_intensify_steps = 2L,
    n_random_sets = 20L,
    n_challengers = 10L,
    ...,
    test_specific_config = FALSE,
    debug = FALSE,
    parallel = waiver()
) {
  if (debug) {
    test_fun <- debug_test_that
    verbose <- TRUE
  } else {
    test_fun <- testthat::test_that
    verbose <- FALSE
  }
  
  # Set parallelisation.
  if (is.waive(parallel)) parallel <- !debug
  
  if (parallel) {
    # Set options.
    # Disable randomForestSRC OpenMP core use.
    options(rf.cores = 1L)
    on.exit(options(rf.cores = -1L), add = TRUE)
    
    # Disable multithreading on data.table to prevent reduced performance due to
    # resource collisions with familiar parallelisation.
    data.table::setDTthreads(1L)
    on.exit(data.table::setDTthreads(0L), add = TRUE)
    
    # Start local cluster in the overall process.
    cl <- .test_start_cluster(n_cores = 2L)
    on.exit(.terminate_cluster(cl), add = TRUE)
    
  } else {
    cl <- NULL
  }
  
  # Clean up dots
  dots <- list(...)
  dots$cl <- NULL
  dots$verbose <- NULL
  
  if (is.null(learners)) {
    is_vimp <- TRUE
    method_pool <- vimp_methods
    
    if (is.null(learners)) learners <- "glm"
  } else {
    is_vimp <- FALSE
    method_pool <- learners
    
    if (is.null(vimp_methods)) vimp_methods <- "mim"
  }
  
  # Iterate over the outcome type.
  for (outcome_type in outcome_type_available) {
    # Multi-feature data sets.
    full_data <- test_create_good_data(outcome_type)
    identical_sample_data <- test_create_all_identical_data(outcome_type)
    full_one_sample_data <- test_create_one_sample_data(outcome_type)
    empty_data <- test_create_empty_data(outcome_type)
    
    # One-feature data sets.
    one_feature_data <- test_create_single_feature_data(outcome_type)
    one_feature_one_sample_data <- test_create_single_feature_one_sample_data(outcome_type)
    one_feature_invariant_data <- test_create_single_feature_invariant_data(outcome_type)
    
    # Set exceptions per outcome type.
    .not_available_no_samples <- not_available_no_samples
    if (is.character(.not_available_no_samples)) {
      .not_available_no_samples <- any(.not_available_no_samples == outcome_type)
    }
    
    .not_available_invariant_data <- FALSE
    if (is.character(.not_available_invariant_data)) {
      .not_available_invariant_data <- any(.not_available_invariant_data == outcome_type)
    }
    
    # Iterate over learners or variable importance methods.
    for (current_method in method_pool) {
      if (is_vimp) {
        learner <- learners
        vimp_method <- current_method
      } else {
        learner <- current_method
        vimp_method <- vimp_methods
      }
      
      if (!.check_learner_outcome_type(
        learner = learner,
        outcome_type = outcome_type,
        as_flag = TRUE
      )) { 
        next 
      }
      
      if (!.check_vimp_outcome_type(
        method = vimp_method, 
        outcome_type = outcome_type,
        as_flag = TRUE
      )) {
        next
      }
      
      # Full data set-----------------------------------------------------------
      
      # Create object
      object <- .test_create_hyperparameter_object(
        data = full_data,
        vimp_method = vimp_method,
        learner = learner,
        is_vimp = is_vimp,
        set_signature_feature = TRUE
      )
      
      # Check that object is available for the outcome.
      if (!is_available(object)) next
      
      .not_available_invariant_data <- FALSE
      .no_hyperparameters <- FALSE
      
      # Check default parameters
      default_hyperparameters <- get_default_hyperparameters(
        object,
        data = full_data
      )
      
      if (length(default_hyperparameters) > 0L) {
        randomised_hyperparameters <- sapply(default_hyperparameters, function(x) x$randomise)
        
        if ("sign_size" %in% names(randomised_hyperparameters)) {
          .not_available_invariant_data <- sum(randomised_hyperparameters) > 1L ||
            !randomised_hyperparameters["sign_size"]
        } else {
          .not_available_invariant_data <- sum(randomised_hyperparameters) > 0L
        }
      } else {
        .no_hyperparameters <- TRUE
        .not_available_invariant_data <- TRUE
      }
      
      if (verbose) {
        message(paste0(
          "\nComputing hyperparameters for ", current_method,
          ifelse(is_vimp, " variable importance method", " learner"), " and ",
          outcome_type, " outcomes for a complete data set."
        ))
      }
      
      # Hyperparameter optimisation on a full dataset.
      new_object <- do.call(
        optimise_hyperparameters,
        args = c(
          list(
            "object" = object,
            "data" = full_data,
            "cl" = cl,
            "n_max_bootstraps" = n_max_bootstraps,
            "n_max_optimisation_steps" = n_max_optimisation_steps,
            "n_max_intensify_steps" = n_max_intensify_steps,
            "n_random_sets" = n_random_sets,
            "n_challengers" = n_challengers,
            "is_vimp" = is_vimp,
            "verbose" = verbose
          ),
          dots
        )
      )
      
      # Test that hyperparameters were set.
      test_fun(
        paste0(
          "1. Hyperparameters for the ", current_method,
          ifelse(is_vimp, " variable importance method", " learner"), " and ",
          outcome_type, " outcomes can be created for a complete data set."
        ),
        {
          if (.no_hyperparameters) {
            # Test that no hyperparameters are set.
            testthat::expect_true(is.null(new_object@hyperparameters))
            
          } else if (!.no_hyperparameters || !not_available_no_samples) {
            # Test that hyperparameters are set.
            testthat::expect_false(is.null(new_object@hyperparameters))
            
            # Test that all hyperparameters are set.
            testthat::expect_setequal(
              names(new_object@hyperparameters),
              names(get_default_hyperparameters(object))
            )
            
            if (!is_vimp) {
              if (!is.null(new_object@hyperparameter_data$parameter_table)) {
                # Test that sign_size hyperparameters make
                # sense.
                testthat::expect_true(
                  all(new_object@hyperparameter_data$parameter_table$sign_size >= 2L)
                )
                testthat::expect_true(
                  all(new_object@hyperparameter_data$parameter_table$sign_size <= get_n_features(full_data))
                )
                
                if (vimp_method %in% .get_available_signature_only_vimp_methods()) {
                  testthat::expect_true(
                    all(new_object@hyperparameter_data$parameter_table$sign_size == 2L)
                  )
                }
                
                if (vimp_method %in% .get_available_none_vimp_methods()) {
                  testthat::expect_true(
                    all(new_object@hyperparameter_data$parameter_table$sign_size == get_n_features(full_data))
                  )
                }
                
                if (vimp_method %in% .get_available_no_features_vimp_methods()) {
                  testthat::expect_true(
                    all(new_object@hyperparameter_data$parameter_table$sign_size == 0L)
                  )
                }
              }
            }
          }
        }
      )
      
      # Go to next outcome type if only a specific configuration needs to be
      # tested.
      if (test_specific_config) next
      
      if (verbose) {
        message(paste0(
          "\nComputing hyperparameters for ", current_method,
          ifelse(is_vimp, " variable importance method", " learner"), " and ",
          outcome_type, " outcomes for a data set with only identical entries."
        ))
      }
      
      # Optimise for data that are completely identical.
      new_object <- do.call(
        optimise_hyperparameters,
        args = c(
          list(
            "object" = object,
            "data" = identical_sample_data,
            "cl" = cl,
            "n_max_bootstraps" = n_max_bootstraps,
            "n_max_optimisation_steps" = n_max_optimisation_steps,
            "n_max_intensify_steps" = n_max_intensify_steps,
            "n_random_sets" = n_random_sets,
            "n_challengers" = n_challengers,
            "is_vimp" = is_vimp,
            "verbose" = verbose
          ),
          dots
        )
      )
      
      # Test that hyperparameters were set.
      test_fun(
        paste0(
          "2. Hyperparameters for the ", current_method,
          ifelse(is_vimp, " variable importance method", " learner"), " and ",
          outcome_type, " outcomes can be created for a data set with only identical entries."
        ),
        {
          if (.no_hyperparameters || .not_available_invariant_data) {
            # Test that no hyperparameters are set. Models cannot
            # train on completely invariant data.
            testthat::expect_true(is.null(new_object@hyperparameters))
          } else if (!.not_available_invariant_data) {
            # Test that hyperparameters are set.
            testthat::expect_false(is.null(new_object@hyperparameters))
            
            # Test that all hyperparameters are set.
            testthat::expect_setequal(
              names(new_object@hyperparameters),
              names(get_default_hyperparameters(object))
            )
            
            if (!is_vimp) {
              if (!is.null(new_object@hyperparameter_data$parameter_table)) {
                # Test that sign_size hyperparameters make
                # sense.
                testthat::expect_true(
                  all(new_object@hyperparameter_data$parameter_table$sign_size >= 2L)
                )
                testthat::expect_true(
                  all(new_object@hyperparameter_data$parameter_table$sign_size <= get_n_features(full_data))
                )
                
                if (vimp_method %in% .get_available_signature_only_vimp_methods()) {
                  testthat::expect_true(
                    all(new_object@hyperparameter_data$parameter_table$sign_size == 2L)
                  )
                }
                
                if (vimp_method %in% .get_available_none_vimp_methods()) {
                  testthat::expect_true(
                    all(new_object@hyperparameter_data$parameter_table$sign_size == get_n_features(full_data))
                  )
                }
                
                if (vimp_method %in% .get_available_no_features_vimp_methods()) {
                  testthat::expect_true(
                    all(new_object@hyperparameter_data$parameter_table$sign_size == 0L)
                  )
                }
              }
            }
          }
        }
      )
      
      if (verbose) {
        message(paste0(
          "\nComputing hyperparameters for ", current_method,
          ifelse(is_vimp, " variable importance method", " learner"), " and ",
          outcome_type, " outcomes for a data set with only one entry."
        ))
      }
      
      # Optimise for data that consist of only one sample.
      new_object <- do.call(
        optimise_hyperparameters,
        args = c(
          list(
            "object" = object,
            "data" = full_one_sample_data,
            "cl" = cl,
            "n_max_bootstraps" = n_max_bootstraps,
            "n_max_optimisation_steps" = n_max_optimisation_steps,
            "n_max_intensify_steps" = n_max_intensify_steps,
            "n_random_sets" = n_random_sets,
            "n_challengers" = n_challengers,
            "is_vimp" = is_vimp,
            "verbose" = verbose
          ),
          dots
        )
      )
      
      # Test.
      test_fun(
        paste0(
          "3. Hyperparameters for the ", current_method,
          ifelse(is_vimp, " variable importance method", " learner"), " and ",
          outcome_type, " outcomes can be created for a data set with only one entry."
        ),
        {
          if (.no_hyperparameters) {
            # Test that no hyperparameters are set. Single entry data cannot be
            # used to generate hyperparameter sets unless they are always
            # available.
            testthat::expect_true(is.null(new_object@hyperparameters))
            
          } else if (!not_available_no_samples) {
            # Test that hyperparameters are set.
            testthat::expect_false(is.null(new_object@hyperparameters))
            
            # Test that all hyperparameters are set.
            testthat::expect_setequal(
              names(new_object@hyperparameters),
              names(get_default_hyperparameters(object))
            )
            
            if (!is_vimp) {
              if (!is.null(new_object@hyperparameter_data$parameter_table)) {
                # Test that sign_size hyperparameters make
                # sense.
                testthat::expect_true(
                  all(new_object@hyperparameter_data$parameter_table$sign_size >= 2L)
                )
                testthat::expect_true(
                  all(new_object@hyperparameter_data$parameter_table$sign_size <= get_n_features(full_data))
                )
                
                if (vimp_method %in% .get_available_signature_only_vimp_methods()) {
                  testthat::expect_true(
                    all(new_object@hyperparameter_data$parameter_table$sign_size == 2L)
                  )
                }
                
                if (vimp_method %in% .get_available_none_vimp_methods()) {
                  testthat::expect_true(
                    all(new_object@hyperparameter_data$parameter_table$sign_size == get_n_features(full_data))
                  )
                }
                
                if (vimp_method %in% .get_available_no_features_vimp_methods()) {
                  testthat::expect_true(
                    all(new_object@hyperparameter_data$parameter_table$sign_size == 0L)
                  )
                }
              }
            }
            
          } else {
            # Not always available, but with hyperparameters. For some methods
            # all hyperparameters can still be set, i.e. all typically
            # randomised hyperparameters depend only on the number of features.
            # Therefore, this is a softer check.
            
            if (!is.null(new_object@hyperparameters)) {
              # Test that all hyperparameters are set.
              testthat::expect_setequal(
                names(new_object@hyperparameters), 
                names(get_default_hyperparameters(object))
              )
              
              if (!is_vimp) {
                if (!is.null(new_object@hyperparameter_data$parameter_table)) {
                  # Test that sign_size hyperparameters make
                  # sense.
                  testthat::expect_true(
                    all(new_object@hyperparameter_data$parameter_table$sign_size == 2L)
                  )
                }
              }
              
            } else {
              # Bogus test to prevent skipping.
              testthat::expect_true(is.null(new_object@hyperparameters))
            }
          }
        }
      )
      
      if (verbose) {
        message(paste0(
          "\nComputing hyperparameters for ", current_method,
          ifelse(is_vimp, " variable importance method", " learner"), " and ",
          outcome_type, " outcomes for an empty data set."
        ))
      }
      
      # Optimise when data is missing.
      new_object <- do.call(
        optimise_hyperparameters,
        args = c(
          list(
            "object" = object,
            "data" = empty_data,
            "cl" = cl,
            "n_max_bootstraps" = n_max_bootstraps,
            "n_max_optimisation_steps" = n_max_optimisation_steps,
            "n_max_intensify_steps" = n_max_intensify_steps,
            "n_random_sets" = n_random_sets,
            "n_challengers" = n_challengers,
            "is_vimp" = is_vimp,
            "verbose" = verbose
          ),
          dots
        )
      )
      
      # Test.
      test_fun(
        paste0(
          "4. Hyperparameters for the ", current_method,
          ifelse(is_vimp, " variable importance method", " learner"), " and ",
          outcome_type, " outcomes ",
          ifelse(!not_available_no_samples, "can", "cannot"),
          " be created for an empty data set."
        ), 
        {
          if (.no_hyperparameters) {
            # Test that no hyperparameters are set. Empty datasets cannot be
            # used to create hyperparameters.
            testthat::expect_true(is.null(new_object@hyperparameters))
            
          } else if (!not_available_no_samples) {
            # Test that hyperparameters are set.
            testthat::expect_false(is.null(new_object@hyperparameters))
            
            # Test that all hyperparameters are set.
            testthat::expect_setequal(
              names(new_object@hyperparameters),
              names(get_default_hyperparameters(object))
            )
            
          } else {
            # Not always available, but with hyperparameters. For some methods
            # all hyperparameters can still be set, i.e. all typically
            # randomised hyperparameters depend only on the number of features.
            # Therefore, this is a softer check.
            
            if (!is.null(new_object@hyperparameters)) {
              # Test that all hyperparameters are set.
              testthat::expect_setequal(
                names(new_object@hyperparameters),
                names(get_default_hyperparameters(object))
              )
              
              if (!is_vimp && !is.null(new_object@hyperparameter_data$parameter_table)) {
                # Test that sign_size hyperparameters make
                # sense.
                testthat::expect_true(
                  all(new_object@hyperparameter_data$parameter_table$sign_size == 2L)
                )
              }
              
            } else {
              # Bogus test to prevent skipping.
              testthat::expect_true(is.null(new_object@hyperparameters))
            }
          }
        }
      )
      
      # One-feature data set ---------------------------------------------------
      # Create object
      object <- .test_create_hyperparameter_object(
        data = one_feature_data,
        vimp_method = vimp_method,
        learner = learner,
        is_vimp = is_vimp,
        set_signature_feature = FALSE
      )
      
      if (verbose) {
        message(paste0(
          "\nComputing hyperparameters for ", current_method,
          ifelse(is_vimp, " variable importance method", " learner"), " and ",
          outcome_type, " outcomes for a data set with only one feature."
        ))
      }
      
      # Optimise parameters for a dataset with only one feature.
      new_object <- do.call(
        optimise_hyperparameters,
        args = c(
          list(
            "object" = object,
            "data" = one_feature_data,
            "cl" = cl,
            "n_max_bootstraps" = n_max_bootstraps,
            "n_max_optimisation_steps" = n_max_optimisation_steps,
            "n_max_intensify_steps" = n_max_intensify_steps,
            "n_random_sets" = n_random_sets,
            "n_challengers" = n_challengers,
            "is_vimp" = is_vimp,
            "verbose" = verbose
          ),
          dots
        )
      )
      
      test_fun(
        paste0(
          "5. Hyperparameters for the ", current_method,
          ifelse(is_vimp, " variable importance method", " learner"), " and ",
          outcome_type, " outcomes can be created for a data set with only one feature."
        ),
        {
          if (.no_hyperparameters) {
            # Test that no hyperparameters are set.
            testthat::expect_true(is.null(new_object@hyperparameters))
          } else if (!.no_hyperparameters || !not_available_no_samples) {
            # Test that hyperparameters are set.
            testthat::expect_false(is.null(new_object@hyperparameters))
            
            # Test that all hyperparameters are set.
            testthat::expect_setequal(
              names(new_object@hyperparameters),
              names(get_default_hyperparameters(object))
            )
            
            if (!is_vimp) {
              if (!is.null(new_object@hyperparameter_data$parameter_table)) {
                # Test that sign_size hyperparameters make sense.
                testthat::expect_true(
                  all(new_object@hyperparameter_data$parameter_table$sign_size == 1L)
                )
              }
            }
          }
        }
      )
      
      if (verbose) {
        message(paste0(
          "\nComputing hyperparameters for ", current_method,
          ifelse(is_vimp, " variable importance method", " learner"), " and ",
          outcome_type, " outcomes for a data set with only one feature and sample."
        ))
      }
      
      # Optimise parameters for a dataset with only one feature and sample.
      new_object <- do.call(
        optimise_hyperparameters,
        args = c(
          list(
            "object" = object,
            "data" = one_feature_one_sample_data,
            "cl" = cl,
            "n_max_bootstraps" = n_max_bootstraps,
            "n_max_optimisation_steps" = n_max_optimisation_steps,
            "n_max_intensify_steps" = n_max_intensify_steps,
            "n_random_sets" = n_random_sets,
            "n_challengers" = n_challengers,
            "is_vimp" = is_vimp,
            "verbose" = verbose
          ),
          dots
        )
      )
      
      test_fun(
        paste0(
          "6. Hyperparameters for the ", current_method,
          ifelse(is_vimp, " variable importance method", " learner"), " and ",
          outcome_type, " outcomes can be created for a data set with only one feature and sample."
        ),
        {
          if (.no_hyperparameters) {
            # Test that no hyperparameters are set. Hyperparameters cannot be
            # set for datasets with only a single sample.
            testthat::expect_true(is.null(new_object@hyperparameters))
            
          } else if (!not_available_no_samples) {
            # Test that hyperparameters are set.
            testthat::expect_false(is.null(new_object@hyperparameters))
            
            # Test that all hyperparameters are set.
            testthat::expect_setequal(
              names(new_object@hyperparameters),
              names(get_default_hyperparameters(object))
            )
            
            if (!is_vimp) {
              if (!is.null(new_object@hyperparameter_data$parameter_table)) {
                # Test that sign_size hyperparameters make sense.
                testthat::expect_true(
                  all(new_object@hyperparameter_data$parameter_table$sign_size == 1L)
                )
              }
            }
            
          } else {
            # Not always available, but with hyperparameters. For some methods
            # all hyperparameters can still be set, i.e. all typically
            # randomised hyperparameters depend only on the number of features.
            # Therefore, this is a softer check.
            
            if (!is.null(new_object@hyperparameters)) {
              # Test that all hyperparameters are set.
              testthat::expect_setequal(
                names(new_object@hyperparameters),
                names(get_default_hyperparameters(object))
              )
              
              if (!is_vimp) {
                if (!is.null(new_object@hyperparameter_data$parameter_table)) {
                  # Test that sign_size hyperparameters make sense.
                  testthat::expect_true(
                    all(new_object@hyperparameter_data$parameter_table$sign_size == 1L)
                  )
                }
              }
            } else {
              # Bogus test to prevent skipping.
              testthat::expect_equal(is.null(new_object@hyperparameters), TRUE)
            }
          }
        }
      )
      
      if (verbose) {
        message(paste0(
          "\nComputing hyperparameters for ", current_method,
          ifelse(is_vimp, " variable importance method", " learner"), " and ",
          outcome_type, " outcomes for a data set with only one, invariant feature."
        ))
      }
      
      # Optimise parameters for a dataset with only one, invariant feature.
      new_object <- do.call(
        optimise_hyperparameters,
        args = c(
          list(
            "object" = object,
            "data" = one_feature_invariant_data,
            "cl" = cl,
            "n_max_bootstraps" = n_max_bootstraps,
            "n_max_optimisation_steps" = n_max_optimisation_steps,
            "n_max_intensify_steps" = n_max_intensify_steps,
            "n_random_sets" = n_random_sets,
            "n_challengers" = n_challengers,
            "is_vimp" = is_vimp,
            "verbose" = verbose
          ),
          dots
        )
      )
      
      test_fun(
        paste0(
          "7. Hyperparameters for the ", current_method,
          ifelse(is_vimp, " variable importance method", " learner"), " and ",
          outcome_type, " outcomes can be created for a data set with ",
          "only one, invariant feature."
        ),
        {
          if (.no_hyperparameters) {
            # Test that no hyperparameters are set. Hyperparameters cannot be
            # set for datasets with invariant features.
            testthat::expect_true(is.null(new_object@hyperparameters))
            
          } else if (!not_available_no_samples) {
            # Test that hyperparameters are set.
            testthat::expect_false(is.null(new_object@hyperparameters))
            
            # Test that all hyperparameters are set.
            testthat::expect_setequal(
              names(new_object@hyperparameters),
              names(get_default_hyperparameters(object))
            )
            
            if (!is_vimp) {
              if (!is.null(new_object@hyperparameter_data$parameter_table)) {
                # Test that sign_size hyperparameters make sense.
                testthat::expect_true(
                  all(new_object@hyperparameter_data$parameter_table$sign_size == 1L)
                )
              }
            }
            
          } else {
            # Not always available, but with hyperparameters. For some methods
            # all hyperparameters can still be set, i.e. all typically
            # randomised hyperparameters depend only on the number of features.
            # Therefore, this is a softer check.
            
            if (!is.null(new_object@hyperparameters)) {
              # Test that all hyperparameters are set.
              testthat::expect_setequal(
                names(new_object@hyperparameters),
                names(get_default_hyperparameters(object))
              )
              
              if (!is_vimp) {
                if (!is.null(new_object@hyperparameter_data$parameter_table)) {
                  # Test that sign_size hyperparameters make sense.
                  testthat::expect_true(
                    all(new_object@hyperparameter_data$parameter_table$sign_size == 1L)
                  )
                }
              }
            } else {
              # Bogus test to prevent skipping.
              testthat::expect_equal(is.null(new_object@hyperparameters), TRUE)
            }
          }
        }
      )
    }
  }
}



test_plots <- function(
    plot_function,
    ...,
    plot_args = list(),
    debug = FALSE,
    parallel = waiver()
) {
  if (debug) {
    test_fun <- debug_test_that
  } else {
    test_fun <- testthat::test_that
  }
  
  # Set parallelisation.
  if (is.waive(parallel)) parallel <- !debug
  
  if (parallel) {
    # Set options.
    # Disable randomForestSRC OpenMP core use.
    options(rf.cores = 1L)
    on.exit(options(rf.cores = -1L), add = TRUE)
    
    # Disable multithreading on data.table to prevent reduced performance due to
    # resource collisions with familiar parallelisation.
    data.table::setDTthreads(1L)
    on.exit(data.table::setDTthreads(0L), add = TRUE)
    
    # Start local cluster in the overall process.
    cl <- .test_start_cluster(n_cores = 2L)
    on.exit(.terminate_cluster(cl), add = TRUE)
    
  } else {
    cl <- NULL
  }
  
  test_collection_generator <- .generate_test_collection(
    ...,
    cl = cl
  )
  
  while (TRUE) {
    # Generate parameters.
    collection <- test_collection_generator()
    if (coro::is_exhausted(collection)) break
    
    test_fun(
      collection$message,
      {
        plot_list <- do.call(
          plot_function,
          args = c(
            list("object" = collection$collection),
            plot_args
          )
        )
        
        # Determine which plots are present.
        which_present <- .test_which_plot_present(plot_list)
        
        if (collection$expectation == "all_present") {
          testthat::expect_true(all(which_present))
        } else if (collection$expectation == "all_absent") {
          testthat::expect_true(!any(which_present))
        } else if (collection$expectation == "any_absent") {
          testthat::expect_true(!all(which_present))
        } else {
          ..error_reached_unreachable_code(paste0("unexpected expectation value:", collection$expectation))
        }
      }
    )
  }
  
}



test_plot_ordering <- function(
    plot_function,
    data_element,
    outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
    ...,
    experiment_args = list(),
    plot_args = list(),
    create_novelty_detector = FALSE,
    use_prediction_table = FALSE,
    prediction_type = NULL,
    debug = FALSE,
    parallel = waiver()
) {
  
  ..as_familiar_data_object <- function(
    object, 
    data, 
    data_element, 
    cl, 
    use_prediction_table, 
    prediction_type,
    ...
  ) {
    if (use_prediction_table) {
      if (is.null(prediction_type)) prediction_type <- "default"
      
      # Generate data from prediction tables.
      familiar_data_object <- as_familiar_data(
        object = .predict(object = object, data = data, type = prediction_type, ...),
        data_element = data_element,
        cl = cl,
        ...
      )
      
    } else {
      # Generate data from models and ensembles.
      familiar_data_object <- as_familiar_data(
        object = object,
        data = data,
        data_element = data_element,
        cl = cl,
        ...
      )
    }
    
    return(familiar_data_object)
  }
  
  ..duplicate_familiar_data_object <- function(x) {
    x <- set_object_name(x)
    x@vimp_method <- "mifs"
    return(x)
  }
  
  # Set debug options.
  if (debug) {
    test_fun <- debug_test_that
    plot_args$draw <- TRUE
  } else {
    test_fun <- testthat::test_that
  }
  
  # Set parallelisation.
  if (is.waive(parallel)) parallel <- !debug
  
  if (parallel) {
    # Set options.
    # Disable randomForestSRC OpenMP core use.
    options(rf.cores = 1L)
    on.exit(options(rf.cores = -1L), add = TRUE)
    
    # Disable multithreading on data.table to prevent reduced performance due to
    # resource collisions with familiar parallelisation.
    data.table::setDTthreads(1L)
    on.exit(data.table::setDTthreads(0L), add = TRUE)
    
    # Start local cluster in the overall process.
    cl <- .test_start_cluster(n_cores = 2L)
    on.exit(.terminate_cluster(cl), add = TRUE)
    
  } else {
    cl <- NULL
  }
  
  if (is.null(experiment_args$imputation_method)) experiment_args$imputation_method <- "simple"
  if (is.null(experiment_args$cluster_method)) experiment_args$cluster_method <- "none"
  if (is.null(experiment_args$vimp_method)) experiment_args$vimp_method <- "mim"
  if (is.null(experiment_args$time_max)) experiment_args$time_max <- 3.5
  
  # Iterate over the outcome type.
  for (outcome_type in outcome_type_available) {
    # Obtain data.
    full_data <- test_create_good_data(outcome_type)
    empty_data <- test_create_empty_data(outcome_type)
    
    # Parse hyperparameter list
    hyperparameters_lasso <- list(
      "sign_size" = get_n_features(full_data),
      "family" = switch(
        outcome_type,
        "continuous" = "gaussian",
        "binomial" = "binomial",
        "multinomial" = "multinomial",
        "survival" = "cox"
      )
    )
    
    # Train the lasso model.
    model_full_lasso <- suppressWarnings(do.call(
      test_train,
      args = c(
        list(
          "data" = full_data,
          "hyperparameter_list" = hyperparameters_lasso,
          "learner" = "lasso",
          "create_novelty_detector" = create_novelty_detector
        ),
        experiment_args
      )
    ))
    
    # Parse hyperparameter list
    hyperparameters_glm <- list(
      "sign_size" = get_n_features(full_data),
      "family" = switch(
        outcome_type,
        "continuous" = "gaussian",
        "binomial" = "logistic",
        "multinomial" = "multinomial",
        "survival" = "cox"
      )
    )
    
    # Train the lasso model.
    model_full_glm <- suppressWarnings(do.call(
      test_train,
      args = c(
        list(
          "data" = full_data,
          "hyperparameter_list" = hyperparameters_glm,
          "learner" = "glm",
          "create_novelty_detector" = create_novelty_detector
        ),
        experiment_args
      )
    ))
    
    # Create familiar data objects.
    data_good_full_lasso_1 <- ..as_familiar_data_object(
      object = model_full_lasso,
      data = full_data,
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      prediction_type = prediction_type[[outcome_type]],
      ...
    )
    data_good_full_lasso_2 <- ..duplicate_familiar_data_object(data_good_full_lasso_1)
    data_good_full_glm_1 <- ..as_familiar_data_object(
      object = model_full_glm,
      data = full_data,
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      prediction_type = prediction_type[[outcome_type]],
      ...
    )
    data_good_full_glm_2 <- ..duplicate_familiar_data_object(data_good_full_glm_1)
    data_empty_glm_1 <- ..as_familiar_data_object(
      object = model_full_glm,
      data = empty_data,
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      prediction_type = prediction_type[[outcome_type]],
      ...
    )
    
    data_empty_lasso_1 <- ..as_familiar_data_object(
      object = model_full_lasso,
      data = empty_data,
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      prediction_type = prediction_type[[outcome_type]],
      ...
    )
    data_empty_lasso_2 <- ..duplicate_familiar_data_object(data_empty_lasso_1)
    
    # Create a test dataset with multiple components
    test_fun(
      paste0("Plots for ", outcome_type, " outcomes can be created."),
      {
        object <- list(
          data_good_full_lasso_1, data_empty_lasso_2, data_good_full_lasso_1, data_good_full_lasso_2,
          data_good_full_glm_1, data_good_full_glm_2, data_empty_glm_1, data_good_full_glm_2
        )
        
        object <- mapply(
          set_object_name,
          object,
          c(
            "development_lasso_1", "development_lasso_2", "validation_lasso_1", "validation_lasso_2",
            "development_glm_1", "development_glm_2", "validation_glm_1", "validation_glm_2"
          )
        )
        
        collection <- suppressWarnings(as_familiar_collection(
          object,
          familiar_data_names = c(
            "development", "development", "validation", "validation",
            "development", "development", "validation", "validation"
          )
        ))
        
        plot_list <- do.call(
          plot_function,
          args = c(
            list("object" = collection),
            plot_args
          )
        )
        
        which_present <- .test_which_plot_present(plot_list)
        
        if (outcome_type %in% outcome_type_available) {
          testthat::expect_equal(all(which_present))
          
        } else {
          testthat::expect_true(!any(which_present))
        }
      }
    )
  }
}



test_export <- function(
    export_function,
    ...,
    export_args = list(),
    debug = FALSE,
    parallel = waiver()
) {
  if (debug) {
    test_fun <- debug_test_that
  } else {
    test_fun <- testthat::test_that
  }
  
  # Set parallelisation.
  if (is.waive(parallel)) parallel <- !debug
  
  if (parallel) {
    # Set options.
    # Disable randomForestSRC OpenMP core use.
    options(rf.cores = 1L)
    on.exit(options(rf.cores = -1L), add = TRUE)
    
    # Disable multithreading on data.table to prevent reduced performance due to
    # resource collisions with familiar parallelisation.
    data.table::setDTthreads(1L)
    on.exit(data.table::setDTthreads(0L), add = TRUE)
    
    # Start local cluster in the overall process.
    cl <- .test_start_cluster(n_cores = 2L)
    on.exit(.terminate_cluster(cl), add = TRUE)
    
  } else {
    cl <- NULL
  }
  
  test_collection_generator <- .generate_test_collection(
    ...,
    cl = cl
  )
  
  while (TRUE) {
    # Generate parameters.
    collection <- test_collection_generator()
    if (coro::is_exhausted(collection)) break
    
    test_fun(
      collection$message,
      {
        data_elements <- do.call(
          export_function,
          args = c(
            list("object" = collection$collection),
            export_args
          )
        )
        
        # Determine which elements are present.
        which_present <- .test_which_data_element_present(
          data_elements,
          outcome_type = collection$collection@outcome_type
        )
        
        if (collection$expectation == "all_present") {
          testthat::expect_true(all(which_present))
          
          if (debug) show(data_elements)
          
        } else if (collection$expectation == "all_absent") {
          testthat::expect_true(!any(which_present))
        } else if (collection$expectation == "any_absent") {
          testthat::expect_true(!all(which_present))
        } else {
          ..error_reached_unreachable_code(paste0("unexpected expectation value:", collection$expectation))
        }
      }
    )
  }
}



test_export_specific <- function(
    export_function,
    data_element,
    outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
    ...,
    export_args = list(),
    use_data_set = "full",
    use_prediction_table = FALSE,
    n_models = 1L,
    create_novelty_detector = FALSE,
    debug = FALSE
) {
  
  ..duplicate_familiar_data_object <- function(x) {
    x <- set_object_name(x)
    x@vimp_method <- "mifs"
    return(x)
  }
  
  # Create list for output.
  out_elements <- list()
  
  # Iterate over the outcome type.
  for (outcome_type in outcome_type_available) {
    # Obtain data.
    main_data <- test_create_good_data(outcome_type)
    
    data <- switch(
      use_data_set,
      "full" = test_create_good_data(outcome_type),
      "identical" = test_create_all_identical_data(outcome_type),
      "one_sample" = test_create_one_sample_data(outcome_type)
    )
    
    # Parse hyperparameter list
    hyperparameters <- list(
      "sign_size" = get_n_features(main_data),
      "family" = switch(
        outcome_type,
        "continuous" = "gaussian",
        "binomial" = "binomial",
        "multinomial" = "multinomial",
        "survival" = "cox"
      )
    )
    
    if (n_models == 1L) {
      # Train the model.
      model_full <- suppressWarnings(test_train(
        data = main_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = "lasso",
        time_max = 3.5,
        create_novelty_detector = create_novelty_detector
      ))
      
    } else {
      # Train a set of models.
      model_full <- list()
      
      for (ii in seq_len(n_models)) {
        temp_model <- suppressWarnings(test_train(
          data = test_create_bootstrapped_data(outcome_type, seed = ii),
          cluster_method = "none",
          imputation_method = "simple",
          vimp_method = "mim",
          hyperparameter_list = hyperparameters,
          learner = "lasso",
          time_max = 3.5,
          create_bootstrap = TRUE,
          create_novelty_detector = create_novelty_detector
        ))
        
        model_full[[ii]] <- temp_model
      }
    }
    
    if (use_prediction_table) {
      # Generate data from prediction tables.
      data_good_full_1 <- as_familiar_data(
        object = .predict(object = model_full, data = data, ...),
        data_element = data_element,
        ...
      )
      
    } else {
      # Generate data from models and ensembles.
      data_good_full_1 <- as_familiar_data(
        object = model_full,
        data = data,
        data_element = data_element,
        ...
      )
    }
    
    data_good_full_2 <- ..duplicate_familiar_data_object(data_good_full_1)
    
    # Generate data objects and names.
    object <- list(data_good_full_1, data_good_full_2, data_good_full_1, data_good_full_2)
    object <- mapply(
      set_object_name,
      object, 
      c("development_1", "development_2", "validation_1", "validation_2")
    )
    
    # Process to collect.
    collection <- suppressWarnings(as_familiar_collection(
      object,
      familiar_data_names = c("development", "development", "validation", "validation")
    ))
    
    # Create data elements.
    data_elements <- do.call(
      export_function,
      args = c(
        list("object" = collection),
        export_args
      )
    )
    
    # Save data elements and add name.
    current_element <- list(data_elements)
    names(current_element) <- outcome_type
    
    out_elements <- c(out_elements, current_element)
  }
  
  return(out_elements)
}



integrated_test <- function(
    ...,
    learner = NULL,
    hyperparameters = NULL,
    outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
    warning_good = NULL,
    warning_bad = NULL,
    debug = FALSE
) {
  if (debug) {
    test_fun <- debug_test_that
    suppress_fun <- identity
  } else {
    test_fun <- testthat::test_that
    suppress_fun <- suppressMessages
  }
  
  # Set flag for missing learner.
  learner_unset <- is.null(learner)
  
  for (outcome_type in outcome_type_available) {
    .warning_good <- warning_good
    if (is.list(warning_good)) {
      .warning_good <- warning_good[[outcome_type]]
    }
    
    .warning_bad <- warning_bad
    if (is.list(warning_bad)) {
      .warning_bad <- warning_bad[[outcome_type]]
    }
    
    test_fun(
      paste0(
        "Experiment for a good dataset with ", outcome_type, 
        " outcome functions correctly."
      ),
      {
        # Create datasets
        full_data <- test_create_good_data(outcome_type)
        
        if (learner_unset) {
          # Set learner
          learner <- "lasso"
          
          # Parse hyperparameter list
          hyperparameters <- list(
            "sign_size" = get_n_features(full_data),
            "family" = switch(
              outcome_type,
              "continuous" = "gaussian",
              "binomial" = "binomial",
              "multinomial" = "multinomial",
              "survival" = "cox"
            )
          )
          
          # Parse as list.
          hyperparameters <- list("lasso" = hyperparameters)
        }
        
        if (!is.null(.warning_good)) {
          testthat::expect_warning(
            output <- suppress_fun(summon_familiar(
              data = full_data,
              learner = learner,
              hyperparameter = hyperparameters,
              time_max = 3.5,
              verbose = debug,
              ...
            )),
            .warning_good
          )
          
        } else {
          output <- suppress_fun(summon_familiar(
            data = full_data,
            learner = learner,
            hyperparameter = hyperparameters,
            time_max = 3.5,
            verbose = debug,
            ...
          ))
        }
        
        testthat::expect_equal(is.null(output), FALSE)
      }
    )
    
    test_fun(
      paste0(
        "Experiment for a bad dataset with ", outcome_type, 
        " outcome functions correctly."
      ), 
      {
        # Create datasets. We explicitly insert NA data to circumvent an initial
        # plausibility check.
        bad_data <- test_create_bad_data(
          outcome_type = outcome_type,
          add_na_data = TRUE
        )
        
        if (learner_unset) {
          # Set learner
          learner <- "lasso"
          
          # Parse hyperparameter list
          hyperparameters <- list(
            "sign_size" = get_n_features(bad_data),
            "family" = switch(
              outcome_type,
              "continuous" = "gaussian",
              "binomial" = "binomial",
              "multinomial" = "multinomial",
              "survival" = "cox"
            )
          )
          
          # Parse as list.
          hyperparameters <- list("lasso" = hyperparameters)
        }
        
        if (!is.null(.warning_bad)) {
          testthat::expect_warning(
            output <- suppress_fun(summon_familiar(
              data = bad_data,
              learner = learner,
              hyperparameter = hyperparameters,
              feature_max_fraction_missing = 0.95,
              time_max = 3.5,
              verbose = debug,
              ...
            )),
            .warning_bad
          )
          
        } else {
          # Note that we set a very high feature_max_fraction_missing to deal
          # with NA rows in the dataset. Also time is explicitly set to prevent
          # an error.
          output <- suppress_fun(summon_familiar(
            data = bad_data,
            learner = learner,
            hyperparameter = hyperparameters,
            feature_max_fraction_missing = 0.95,
            time_max = 3.5,
            verbose = debug,
            ...
          ))
        }
        
        testthat::expect_equal(is.null(output), FALSE)
      }
    )
  }
}



debug_test_that <- function(desc, code) {
  # This is a drop-in replacement for testthat::test_that that makes it easier
  # to debug errors.
  
  if (!is.character(desc) || length(desc) != 1L) {
    stop("\"desc\" should be a character string")
  }
  
  # Execute the code
  code <- substitute(code)
  eval(code, envir = parent.frame())
}



test_not_deprecated <- function(x, deprecation_string = c("deprec", "replac")) {
  # Test that no deprecation warnings are given.
  if (length(x) > 0L) {
    for (current_string in deprecation_string) {
      testthat::expect_false(any(grepl(
        x = x, pattern = current_string, fixed = TRUE
      )))
    }
  }
}



.test_which_plot_present <- function(p) {
  # Check if the top element is null or empty.
  if (is.null(p)) return(FALSE)
  if (length(p) == 0L) return(FALSE)
  
  # Check that the top element is a gtable or ggplot.
  if (gtable::is.gtable(p) || ggplot2::is.ggplot(p)) {
    return(TRUE)
  }
  
  plot_present <- sapply(p, gtable::is.gtable) | sapply(p, ggplot2::is.ggplot)
  if (any(plot_present)) {
    return(plot_present)
  }
  
  if (all(sapply(p, is.null))) {
    return(!sapply(p, is.null))
  }
  
  # If the code gets here, p is a nested list.
  p <- unlist(p, recursive = FALSE)
  
  if (is.null(p)) return(FALSE)
  if (length(p) == 0L) return(FALSE)
  
  return(sapply(p, gtable::is.gtable) | sapply(p, ggplot2::is.ggplot))
}



.test_which_data_element_present <- function(x, outcome_type) {
  # Check if the top element is null or empty.
  if (is_empty(x)) return(FALSE)
  
  data_element_present <- !sapply(x, is_empty)
  if (!any(data_element_present)) return(FALSE)
  
  return(data_element_present)
}



.test_start_cluster <- function(n_cores = NULL) {
  # Determine the number of available cores.
  n_cores_available <- parallel::detectCores() - 1L
  
  # Determine the number of available cores.
  if (is.null(n_cores)) n_cores <- n_cores_available
  if (n_cores > n_cores_available) n_cores <- n_cores_available
  if (n_cores < 2L) return(NULL)
  
  assign("is_external_cluster", FALSE, envir = familiar_global_env)
  
  # Start a new cluster
  cl <- .start_cluster(
    n_cores = n_cores,
    cluster_type = "psock"
  )
  
  # If the cluster doesn't start, return a NULL
  if (is.null(cl)) return(NULL)
  
  # Set library paths to avoid issues with non-standard library locations.
  libs <- .libPaths()
  parallel::clusterExport(cl = cl, varlist = "libs", envir = environment())
  parallel::clusterEvalQ(cl = cl, .libPaths(libs))
  
  # Load familiar and data.table libraries to each cluster node.
  parallel::clusterEvalQ(cl = cl, library(familiar))
  parallel::clusterEvalQ(cl = cl, library(data.table))
  
  # Set options on each cluster node.
  parallel::clusterEvalQ(cl = cl, options(rf.cores = 1L))
  parallel::clusterEvalQ(cl = cl, data.table::setDTthreads(1L))
  
  return(cl)
}



.test_create_hyperparameter_object <- function(
    data,
    vimp_method,
    learner,
    is_vimp,
    cluster_method = "none",
    ...,
    set_signature_feature = FALSE
) {
  
  if (set_signature_feature) {
    signature_features <- get_feature_columns(data)[1L:2L]
  } else {
    signature_features <- NULL
  }
  
  # Create feature info list.
  feature_info_list <- create_feature_info(
    data = data,
    vimp_method = vimp_method,
    learner = learner,
    cluster_method = cluster_method,
    imputation_method = "simple",
    ...,
    signature = signature_features,
    parallel = FALSE
  )
  
  # Find required features.
  required_features <- get_required_features(
    x = data,
    feature_info_list = feature_info_list
  )
  
  if (is_vimp) {
    # Create the variable importance met hod object or familiar model object
    # to compute variable importance with.
    object <- promote_vimp_method(
      object = methods::new(
        "familiarVimpMethod",
        outcome_type = data@outcome_type,
        vimp_method = vimp_method,
        required_features = required_features,
        feature_info = feature_info_list,
        outcome_info = data@outcome_info
      )
    )
    
  } else {
    # Create familiar model object.
    object <- promote_learner(
      object = methods::new(
        "familiarModel",
        outcome_type = data@outcome_type,
        learner = learner,
        vimp_method = vimp_method,
        required_features = required_features,
        feature_info = feature_info_list,
        outcome_info = data@outcome_info
      )
    )
  }
  
  return(object)
}


.is_testing <- function() {
  return(identical(Sys.getenv("TESTTHAT"), "true"))
}


# .generate_test_collection ----------------------------------------------------
.generate_test_collection <- coro::generator(function(
    data_element,
    outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
    not_available_no_samples = TRUE,
    not_available_single_feature = FALSE,
    not_available_all_predictions_fail = TRUE,
    not_available_some_predictions_fail = TRUE,
    not_available_all_prospective = FALSE,
    not_available_any_prospective = FALSE,
    not_available_single_sample = FALSE,
    not_available_extreme_probability = FALSE,
    use_prediction_table = FALSE,
    ...,
    cl = NULL,
    test_specific_config = FALSE,
    n_models = 1L,
    create_novelty_detector = FALSE
) {
  
  ..as_familiar_data_object <- function(
    object, 
    data, 
    data_element, 
    cl, 
    use_prediction_table, 
    ...
  ) {
    if (use_prediction_table) {
      # Generate data from prediction tables.
      familiar_data_object <- as_familiar_data(
        object = .predict(object = object, data = data, ...),
        data_element = data_element,
        cl = cl,
        ...
      )
      
    } else {
      # Generate data from models and ensembles.
      familiar_data_object <- as_familiar_data(
        object = object,
        data = data,
        data_element = data_element,
        cl = cl,
        ...
      )
    }
    
    return(familiar_data_object)
  }
  
  ..duplicate_familiar_data_object <- function(x) {
    x <- set_object_name(x)
    x@vimp_method <- "mifs"
    return(x)
  }
  
  # Iterate over the outcome type.
  for (outcome_type in c("continuous", "binomial", "multinomial", "survival")) {
    
    # Set up full dataset.
    full_data <- test_create_good_data(outcome_type)
    
    # Set exceptions per outcome type.
    .not_available_no_samples <- not_available_no_samples
    if (is.character(.not_available_no_samples)) {
      .not_available_no_samples <- any(.not_available_no_samples == outcome_type)
    }
    
    .not_available_single_feature <- not_available_single_feature
    if (is.character(.not_available_single_feature)) {
      .not_available_single_feature <- any(.not_available_single_feature == outcome_type)
    }
    
    .not_available_any_prospective <- not_available_any_prospective
    if (is.character(.not_available_any_prospective)) {
      .not_available_any_prospective <- any(.not_available_any_prospective == outcome_type)
    }
    
    .not_available_all_prospective <- not_available_all_prospective
    if (is.character(.not_available_all_prospective)) {
      .not_available_all_prospective <- any(.not_available_all_prospective == outcome_type)
    }
    
    .not_available_all_predictions_fail <- not_available_all_predictions_fail
    if (is.character(.not_available_all_predictions_fail)) {
      .not_available_all_predictions_fail <- any(.not_available_all_predictions_fail == outcome_type)
    }
    
    .not_available_some_predictions_fail <- not_available_some_predictions_fail
    if (is.character(.not_available_some_predictions_fail)) {
      .not_available_some_predictions_fail <- any(.not_available_some_predictions_fail == outcome_type)
    }
    
    .not_available_single_sample <- not_available_single_sample
    if (is.character(.not_available_single_sample)) {
      .not_available_single_sample <- any(.not_available_single_sample == outcome_type)
    }
    
    .not_available_extreme_probability <- not_available_extreme_probability
    if (is.character(.not_available_extreme_probability)) {
      .not_available_extreme_probability <- any(.not_available_extreme_probability == outcome_type)
    }
    
    # Parse hyperparameter.
    hyperparameters <- list(
      "sign_size" = get_n_features(full_data),
      "family" = switch(
        outcome_type,
        "continuous" = "gaussian",
        "binomial" = "binomial",
        "multinomial" = "multinomial",
        "survival" = "cox"
      )
    )
    
    # Full data ----------------------------------------------------------------
    
    if (n_models == 1L) {
      # Train the model.
      model_full <- suppressWarnings(test_train(
        cl = cl,
        data = full_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = "lasso",
        time_max = 3.5,
        create_novelty_detector = create_novelty_detector
      ))
      
    } else {
      # Train a set of models.
      model_full <- list()
      
      for (ii in seq_len(n_models)) {
        temp_model <- suppressWarnings(test_train(
          cl = cl,
          data = test_create_bootstrapped_data(
            outcome_type = outcome_type,
            seed = ii
          ),
          cluster_method = "none",
          imputation_method = "simple",
          vimp_method = "mim",
          hyperparameter_list = hyperparameters,
          learner = "lasso",
          time_max = 3.5,
          create_bootstrap = TRUE,
          create_novelty_detector = create_novelty_detector
        ))
        
        model_full[[ii]] <- temp_model
      }
    }
    
    good_data_1 <- ..as_familiar_data_object(
      object = model_full,
      data = full_data,
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    good_data_2 <- ..duplicate_familiar_data_object(good_data_1)
    
    familiar_data_list <- mapply(
      set_object_name,
      list(good_data_1, good_data_2, good_data_1, good_data_2),
      c("development_1", "development_2", "validation_1", "validation_2")
    )
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("development", "development", "validation", "validation")
    ))
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(outcome_type %in% outcome_type_available, "can", "cannot"),
      " be created for a complete data set."
    )
    
    test_expectation <- "all_absent"
    if (outcome_type %in% outcome_type_available) test_expectation <- "all_present"
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    # Go to next outcome type if only a specific configuration needs to be
    # tested.
    if (test_specific_config) next
    
    # Fully prospective data ---------------------------------------------------
    
    fully_prospective_data <- ..as_familiar_data_object(
      object = model_full,
      data = test_create_prospective_data(outcome_type),
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    
    familiar_data_list <- mapply(set_object_name, list(fully_prospective_data), c("prospective"))
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("prospective")
    ))
    
    test_expectation <- "all_absent"
    if (outcome_type %in% outcome_type_available && !.not_available_all_prospective) {
      test_expectation <- "all_present"
    }
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(test_expectation == "all_present", "can", "cannot"),
      " be created for a prospective data set without known outcome."
    )
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    rm("fully_prospective_data")
    
    # Mostly prospective data --------------------------------------------------
    
    mostly_prospective_data <- ..as_familiar_data_object(
      object = model_full,
      data = test_create_mostly_prospective_data(outcome_type),
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    
    familiar_data_list <- mapply(set_object_name, list(mostly_prospective_data), c("prospective"))
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("prospective")
    ))
    
    test_expectation <- "all_absent"
    if (
      outcome_type %in% outcome_type_available &&
      (!.not_available_any_prospective || !.not_available_single_sample)
    ) {
      test_expectation <- "all_present"
    }
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(test_expectation == "all_present", "can", "cannot"),
      " be created for a prospective data set with one instance with known outcome."
    )
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    rm("mostly_prospective_data")
    
    # Mostly retrospective data ------------------------------------------------
    
    mostly_retrospective_data <- ..as_familiar_data_object(
      object = model_full,
      data = test_create_partially_prospective_data(outcome_type),
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    
    familiar_data_list <- mapply(set_object_name, list(mostly_retrospective_data), c("prospective"))
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("prospective")
    ))
    
    test_expectation <- "all_absent"
    if (outcome_type %in% outcome_type_available) test_expectation <- "all_present"
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(test_expectation == "all_present", "can", "cannot"),
      " be created for a prospective data set where most instances are known."
    )
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    rm("mostly_retrospective_data")
    
    # Single-instance data -----------------------------------------------------
    
    single_instance_data <- ..as_familiar_data_object(
      object = model_full,
      data = test_create_one_sample_data(outcome_type),
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    
    familiar_data_list <- mapply(set_object_name, list(single_instance_data), c("one_sample"))
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("one_sample")
    ))
    
    test_expectation <- "all_absent"
    if (outcome_type %in% outcome_type_available && !.not_available_single_sample) {
      test_expectation <- "all_present"
    }
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(test_expectation == "all_present", "can", "cannot"),
      " be created for a prospective data set with one instance."
    )
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    rm("single_instance_data")
    
    # Bootstrapped data --------------------------------------------------------
    
    bootstrapped_data <- ..as_familiar_data_object(
      object = model_full,
      data = test_create_bootstrapped_data(outcome_type),
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    
    familiar_data_list <- mapply(set_object_name, list(bootstrapped_data), c("bootstrapped"))
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("bootstrapped")
    ))
    
    test_expectation <- "all_absent"
    if (outcome_type %in% outcome_type_available) test_expectation <- "all_present"
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(test_expectation == "all_present", "can", "cannot"),
      " be created for a prospective, bootstrapped, data set."
    )
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    rm("bootstrapped_data")
    
    # Partially absent data ----------------------------------------------------
    
    # Train a naive model.
    naive_model <- suppressWarnings(train_familiar(
      data = full_data,
      experimental_design = "fs+mb",
      cluster_method = "hclust",
      imputation_method = "simple",
      vimp_method = "no_features",
      learner = "lasso",
      hyperparameter = hyperparameters,
      cluster_similarity_threshold = 0.7,
      time_max = 3.5,
      parallel = FALSE,
      verbose = FALSE
    ))
    
    # Replace vimp_method attribute
    naive_model@vimp_method <- "mifs"
    
    naive_data <- ..as_familiar_data_object(
      object = naive_model,
      data = test_create_good_data(outcome_type),
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    
    empty_data_1 <- ..as_familiar_data_object(
      object = model_full,
      data = test_create_empty_data(outcome_type),
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    
    familiar_data_list <- mapply(
      set_object_name,
      list(good_data_1, naive_data, empty_data_1, good_data_2),
      c("development_1", "development_2", "validation_1", "validation_2")
    )
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("development", "development", "validation", "validation")
    ))
    
    test_expectation <- "all_absent"
    if (outcome_type %in% outcome_type_available) test_expectation <- "all_present"
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(test_expectation == "all_present", "can", "cannot"),
      " be created for a dataset with some missing data."
    )
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    rm("naive_data")
    
    # Fully absent data --------------------------------------------------------
    
    empty_data_2 <- ..duplicate_familiar_data_object(empty_data_1)
    
    familiar_data_list <- mapply(
      set_object_name,
      list(empty_data_1, empty_data_2, empty_data_1, empty_data_2),
      c("development_1", "development_2", "validation_1", "validation_2")
    )
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("development", "development", "validation", "validation")
    ))
    
    test_expectation <- "all_absent"
    if (outcome_type %in% outcome_type_available && !.not_available_no_samples) test_expectation <- "all_present"
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(test_expectation == "all_present", "can", "cannot"),
      " be created for a dataset with completely missing data."
    )
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    rm("empty_data_1", "empty_data_2")
    
    # Partially single-instance data -------------------------------------------
    
    one_sample_data_1 <- ..as_familiar_data_object(
      object = model_full,
      data = test_create_one_sample_data(outcome_type),
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    
    one_sample_data_2 <- ..duplicate_familiar_data_object(one_sample_data_1)
    
    familiar_data_list <- mapply(
      set_object_name,
      list(good_data_1, good_data_2, one_sample_data_1, one_sample_data_2),
      c("development_1", "development_2", "validation_1", "validation_2")
    )
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("development", "development", "validation", "validation")
    ))
    
    test_expectation <- "all_absent"
    if (outcome_type %in% outcome_type_available) test_expectation <- "all_present"
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(test_expectation == "all_present", "can", "cannot"),
      " be created for a dataset where some data only have one sample."
    )
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    rm("one_sample_data_1", "one_sample_data_2")
    
    # Partially identical data -------------------------------------------------
    
    identical_sample_data_1 <- ..as_familiar_data_object(
      object = model_full,
      data = test_create_all_identical_data(outcome_type),
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    
    identical_sample_data_2 <- ..duplicate_familiar_data_object(identical_sample_data_1)
    
    familiar_data_list <- mapply(
      set_object_name,
      list(good_data_1, good_data_2, identical_sample_data_1, identical_sample_data_2),
      c("development_1", "development_2", "validation_1", "validation_2")
    )
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("development", "development", "validation", "validation")
    ))
    
    test_expectation <- "all_absent"
    if (outcome_type %in% outcome_type_available) test_expectation <- "all_present"
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(test_expectation == "all_present", "can", "cannot"),
      " be created for a dataset where some data only have identical samples."
    )
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    rm("identical_sample_data_1", "identical_sample_data_2")
    
    # Multi-model ensemble -----------------------------------------------------
    
    multi_model_data <- list(
      test_create_good_data(outcome_type),
      test_create_bootstrapped_data(outcome_type, seed = 1844L),
      test_create_bootstrapped_data(outcome_type, seed = 1863L)
    )
    
    multi_model_set <- suppressWarnings(lapply(
      multi_model_data,
      test_train,
      cluster_method = "hclust",
      imputation_method = "simple",
      hyperparameter_list = hyperparameters,
      learner = "lasso",
      cluster_similarity_threshold = 0.7,
      time_max = 3.5,
      create_novelty_detector = create_novelty_detector
    ))
    
    # Replace vimp_method attribute and add naive_model to the multi-model
    # ensemble.
    naive_model@vimp_method <- "none"
    multi_model_set <- c(multi_model_set, list(naive_model))
    
    multi_model_data <- ..as_familiar_data_object(
      object = multi_model_set,
      data = full_data,
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    
    familiar_data_list <- mapply(set_object_name, list(multi_model_data), c("development_1"))
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("development")
    ))
    
    test_expectation <- "all_absent"
    if (outcome_type %in% outcome_type_available) test_expectation <- "all_present"
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(test_expectation == "all_present", "can", "cannot"),
      " be created for a dataset created from an ensemble of multiple models."
    )
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    rm("multi_model_data", "multi_model_set", "naive_model")
    rm("good_data_1", "good_data_2", "model_full")
    
    # One-feature data ---------------------------------------------------------
    
    # Train the model.
    one_feature_model <- suppressWarnings(test_train(
      cl = cl,
      data = test_create_single_feature_data(outcome_type),
      cluster_method = "none",
      imputation_method = "simple",
      hyperparameter_list = hyperparameters,
      learner = "lasso",
      time_max = 3.5,
      create_novelty_detector = create_novelty_detector
    ))
    
    good_one_feature_data_1 <- ..as_familiar_data_object(
      object = one_feature_model,
      data = test_create_single_feature_data(outcome_type),
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    good_one_feature_data_2 <- ..duplicate_familiar_data_object(good_one_feature_data_1)
    
    familiar_data_list <- mapply(
      set_object_name,
      list(good_one_feature_data_1, good_one_feature_data_2, good_one_feature_data_1, good_one_feature_data_2),
      c("development_1", "development_2", "validation_1", "validation_2")
    )
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("development", "development", "validation", "validation")
    ))
    
    test_expectation <- "all_absent"
    if (outcome_type %in% outcome_type_available && !.not_available_single_feature) {
      test_expectation <- "all_present"
    }
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(test_expectation == "all_present", "can", "cannot"),
      " be created for a complete one-feature data set."
    )
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    # Partially single-instance one-feature data -------------------------------
    
    good_one_sample_one_feature_data_1 <- ..as_familiar_data_object(
      object = one_feature_model,
      data = test_create_single_feature_one_sample_data(outcome_type),
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    good_one_sample_one_feature_data_2 <- ..duplicate_familiar_data_object(good_one_sample_one_feature_data_1)
    
    familiar_data_list <- mapply(
      set_object_name,
      list(
        good_one_feature_data_1, good_one_feature_data_2, 
        good_one_sample_one_feature_data_1, good_one_sample_one_feature_data_2
      ),
      c("development_1", "development_2", "validation_1", "validation_2")
    )
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("development", "development", "validation", "validation")
    ))
    
    test_expectation <- "all_absent"
    if (outcome_type %in% outcome_type_available && !.not_available_single_feature) {
      test_expectation <- "all_present"
    }
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(test_expectation == "all_present", "can", "cannot"),
      " be created for a one-feature dataset with some one-sample data."
    )
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    rm("good_one_sample_one_feature_data_1", "good_one_sample_one_feature_data_2")
    
    # Partially identical one-feature data -------------------------------------
    
    good_identical_one_feature_data_1 <- ..as_familiar_data_object(
      object = one_feature_model,
      data = test_create_single_feature_invariant_data(outcome_type),
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    good_identical_one_feature_data_2 <- ..duplicate_familiar_data_object(good_identical_one_feature_data_1)
    
    familiar_data_list <- mapply(
      set_object_name,
      list(
        good_one_feature_data_1, good_one_feature_data_2, 
        good_identical_one_feature_data_1, good_identical_one_feature_data_2
      ),
      c("development_1", "development_2", "validation_1", "validation_2")
    )
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("development", "development", "validation", "validation")
    ))
    
    test_expectation <- "all_absent"
    if (outcome_type %in% outcome_type_available && !.not_available_single_feature) {
      test_expectation <- "all_present"
    }
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(test_expectation == "all_present", "can", "cannot"),
      " be created for a one-feature dataset with some invariant data."
    )
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    rm("good_identical_one_feature_data_1", "good_identical_one_feature_data_2")
    rm("good_one_feature_data_1", "good_one_feature_data_2", "one_feature_model")
    
    # Data with limited censoring ----------------------------------------------
    if (outcome_type %in% c("survival", "competing_risk")) {
      # Train the model.
      model_cens_1 <- suppressWarnings(test_train(
        cl = cl,
        data = test_create_good_data_without_censoring(outcome_type),
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = "lasso",
        time_max = 3.5
      ))
      
      model_cens_2 <- suppressWarnings(test_train(
        cl = cl,
        data = test_create_good_data_one_censored(outcome_type),
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = "lasso",
        time_max = 3.5
      ))
      
      model_cens_3 <- suppressWarnings(test_train(
        cl = cl,
        data = test_create_good_data_few_censored(outcome_type),
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = "lasso",
        time_max = 3.5
      ))
      
      data_cens_1 <- as_familiar_data(
        object = model_cens_1,
        data = test_create_good_data_without_censoring(outcome_type),
        data_element = data_element,
        cl = cl,
        use_prediction_table = use_prediction_table,
        ...
      )
      data_cens_2 <- as_familiar_data(
        object = model_cens_2, 
        data = test_create_good_data_one_censored(outcome_type),
        data_element = data_element,
        cl = cl,
        use_prediction_table = use_prediction_table,
        ...
      )
      data_cens_3 <- as_familiar_data(
        object = model_cens_3,
        data = test_create_good_data_few_censored(outcome_type),
        data_element = data_element, 
        cl = cl,
        use_prediction_table = use_prediction_table,
        ...
      )
      
      familiar_data_list <- mapply(
        set_object_name,
        list(data_cens_1, data_cens_2, data_cens_3),
        c("no_censoring", "one_censored", "few_censored")
      )
      
      familiar_collection <- suppressWarnings(as_familiar_collection(
        familiar_data_list,
        familiar_data_names = c("no_censoring", "one_censored", "few_censored")
      ))
      
      test_expectation <- "all_absent"
      if (outcome_type %in% outcome_type_available) {
        test_expectation <- "all_present"
      }
      
      test_message <- paste0(
        "Data for ", outcome_type, " outcomes ",
        ifelse(test_expectation == "all_present", "can", "cannot"),
        " be created for a data set that includes no or limited censoring."
      )
      
      coro::yield(list(
        "collection" = familiar_collection,
        "message" = test_message,
        "expectation" = test_expectation
      ))
      
      rm("data_cens_1", "data_cens_2", "data_cens_3", "model_cens_1", "model_cens_2", "model_cens_3")
    }
    
    # Only invalid predictions -------------------------------------------------
    
    model_failed_predictions <- suppressWarnings(test_train(
      cl = cl,
      data = full_data,
      cluster_method = "none",
      imputation_method = "simple",
      hyperparameter_list = hyperparameters,
      learner = "lasso_test_all_fail",
      time_max = 3.5,
      create_novelty_detector = create_novelty_detector
    ))
    
    failed_prediction_data <- ..as_familiar_data_object(
      object = model_failed_predictions,
      data = full_data,
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    
    familiar_data_list <- mapply(set_object_name, list(failed_prediction_data), c("all_failed_predictions"))
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("all_failed_predictions")
    ))
    
    test_expectation <- "all_absent"
    if (outcome_type %in% outcome_type_available && !.not_available_all_predictions_fail) {
      test_expectation <- "all_present"
    }
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(test_expectation == "all_present", "can", "cannot"),
      " be created for models that do not provide valid predictions."
    )
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    rm("failed_prediction_data", "model_failed_predictions")
    
    # Some invalid predictions -------------------------------------------------
    
    model_failing_predictions <- suppressWarnings(test_train(
      cl = cl,
      data = full_data,
      cluster_method = "none",
      imputation_method = "simple",
      hyperparameter_list = hyperparameters,
      learner = "lasso_test_some_fail",
      time_max = 3.5,
      create_novelty_detector = create_novelty_detector
    ))
    
    failing_prediction_data <- ..as_familiar_data_object(
      object = model_failing_predictions,
      data = full_data,
      data_element = data_element,
      cl = cl,
      use_prediction_table = use_prediction_table,
      ...
    )
    
    familiar_data_list <- mapply(set_object_name, list(failing_prediction_data), c("some_failed_predictions"))
    
    familiar_collection <- suppressWarnings(as_familiar_collection(
      familiar_data_list,
      familiar_data_names = c("some_failed_predictions")
    ))
    
    test_expectation <- "all_absent"
    if (outcome_type %in% outcome_type_available && !.not_available_some_predictions_fail) {
      test_expectation <- "all_present"
    }
    
    test_message <- paste0(
      "Data for ", outcome_type, " outcomes ",
      ifelse(test_expectation == "all_present", "can", "cannot"),
      " be created for models that provide some invalid predictions."
    )
    
    coro::yield(list(
      "collection" = familiar_collection,
      "message" = test_message,
      "expectation" = test_expectation
    ))
    
    rm("failing_prediction_data", "model_failing_predictions")
    
    # Extreme predicted values -------------------------------------------------
    
    if (outcome_type %in% c("binomial", "multinomial")) {
      model_extreme_predictions <- suppressWarnings(test_train(
        cl = cl,
        data = full_data,
        cluster_method = "none",
        imputation_method = "simple",
        hyperparameter_list = hyperparameters,
        learner = "lasso_test_extreme",
        time_max = 3.5,
        create_novelty_detector = create_novelty_detector
      ))
      
      extreme_prediction_data <- ..as_familiar_data_object(
        object = model_extreme_predictions,
        data = full_data,
        data_element = data_element,
        cl = cl,
        use_prediction_table = use_prediction_table,
        ...
      )
      
      familiar_data_list <- mapply(set_object_name, list(extreme_prediction_data), c("extreme_predictions"))
      
      familiar_collection <- suppressWarnings(as_familiar_collection(
        familiar_data_list,
        familiar_data_names = c("extreme_predictions")
      ))
      
      test_expectation <- "all_absent"
      if (outcome_type %in% outcome_type_available && !.not_available_extreme_probability) {
        test_expectation <- "all_present"
      }
      
      test_message <- paste0(
        "Data for ", outcome_type, " outcomes ",
        ifelse(test_expectation == "all_present", "can", "cannot"),
        " be created for models yielding extreme predictions."
      )
      
      coro::yield(list(
        "collection" = familiar_collection,
        "message" = test_message,
        "expectation" = test_expectation
      ))
      
      rm("extreme_prediction_data", "model_extreme_predictions")
    }
  }
})

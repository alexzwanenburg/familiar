test_all_learners_available <- function(learners){
  
  # Create placeholder flags.
  learner_available <- logical(length(learners))
  names(learner_available) <- learners
  
  # Iterate over learners.
  for(learner in learners){
    
    # Determine if the learner is available for any outcome.
    for(outcome_type in c("count", "continuous", "binomial", "multinomial", "survival", "competing_risk")){
      
      # Create a familiarModel object.
      object <- methods::new("familiarModel",
                             outcome_type=outcome_type,
                             learner=learner)
      
      # Promote the learner to the right class.
      object <- promote_learner(object=object)
      
      # Check if the learner is available for the outcome.
      if(is_available(object)){
        learner_available[learner] <- TRUE
        break()
      }
    }
  }
  
  # Iterate over learners
  for(learner in learners){
    testthat::test_that(paste0(learner, " is available."), {
      testthat::expect_equal(unname(learner_available[learner]), TRUE)
    })
  }
}



test_all_learners_train_predict_vimp <- function(learners,
                                                 hyperparameter_list=NULL,
                                                 except_train=NULL,
                                                 except_predict=NULL,
                                                 except_predict_survival=NULL,
                                                 has_vimp=TRUE,
                                                 can_trim=TRUE,
                                                 debug=FALSE){
  
  if(debug){
    test_fun <- debug_test_that
    
  } else {
    test_fun <- testthat::test_that
  }
  
  # Iterate over the outcome type.
  for(outcome_type in c("count", "continuous", "binomial", "multinomial", "survival")){
    
    # Obtain data.
    full_data <- test.create_good_data_set(outcome_type)
    full_one_sample_data <- test.create_one_sample_data_set(outcome_type)
    one_feature_data <- test.create_one_feature_data_set(outcome_type)
    one_feature_one_sample_data <- test.create_one_feature_one_sample_data_set(outcome_type)
    empty_data <- test.create_empty_data_set(outcome_type)
    bad_data <- test.create_bad_data_set(outcome_type)
    
    # Iterate over learners.
    for(learner in learners){
      
      # Create a familiarModel object.
      object <- methods::new("familiarModel",
                             outcome_type=outcome_type,
                             learner=learner)
      
      # Promote the learner to the right class.
      object <- promote_learner(object=object)
      
      # Test if the learner is available for the current outcome_type
      if(!is_available(object)) next()
      
      # Parse hyperparameter list
      hyperparameters <- c(hyperparameter_list[[outcome_type]],
                           list("sign_size"=get_n_features(full_data)))
      
      # Find required hyperparameters
      learner_hyperparameters <- .get_preset_hyperparameters(learner=learner,
                                                             outcome_type=outcome_type,
                                                             names_only=TRUE)
      
      # Select hyperparameters that are being used, and are present in the input
      # list of hyperparameters.
      hyperparameters <- hyperparameters[intersect(learner_hyperparameters, names(hyperparameters))]
      
      #####Full data set########################################################
      
      # Train the model.
      model <- suppressWarnings(train(data=full_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=hyperparameters,
                                      learner=learner,
                                      time_max=1832,
                                      trim_model = FALSE))
      
      # Create a trimmed model.
      trimmed_model <- trim_model(model)
      
      # Check that the model can be trimmed.
      test_fun(paste0("Model for ", outcome_type, " created using ", learner, " can be trimmed."), {
        if(can_trim){
          testthat::expect_equal(trimmed_model@is_trimmed, TRUE)
          
        } else {
          testthat::expect_equal(trimmed_model@is_trimmed, FALSE)
        }
      })
      
      # Test that models can be created.
      test_fun(paste0("Model for ", outcome_type, " can be created using ", learner, " using a complete data set."), {
        
        # Test that the model was successfully created.
        testthat::expect_equal(model_is_trained(model),
                               ifelse(learner %in% except_train, FALSE, TRUE))
        
        if(outcome_type == "survival"){
          # Calibration info is present
          testthat::expect_equal(has_calibration_info(model), TRUE)
        }
      })
      
      # Test that models can be used to predict the outcome.
      test_fun(paste0("Sample predictions for ", outcome_type, " can be made using ", learner, " for a complete data set."), {
        # Expect predictions to be made.
        prediction_table <- suppressWarnings(.predict(model, data=full_data))
        
        # Test that the predictions were successfully made.
        testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                               ifelse(learner %in% c(except_train, except_predict), FALSE, TRUE))
        
        if(outcome_type %in% c("binomial", "multinomial")){
          # Expect that the predicted_class column is a factor.
          testthat::expect_s3_class(prediction_table$predicted_class, "factor")
          
          # Expect that the class levels are the same as those in the model.
          testthat::expect_equal(levels(prediction_table$predicted_class), get_outcome_class_levels(model))
        }
        
        # Expect that the trimmed model produces the same predictions.
        prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                           data=full_data))
        
        testthat::expect_equal(prediction_table,
                               prediction_table_trim,
                               ignore_attr=TRUE)
      })
      
      # Test that models can be used to predict the outcome.
      test_fun(paste0("Sample predictions for ", outcome_type, " can be made using ", learner, " for a one-sample data set."), {
        # Expect predictions to be made.
        prediction_table <- suppressWarnings(.predict(model, data=full_one_sample_data))
        
        # Test that the predictions were successfully made.
        testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                               ifelse(learner %in% c(except_train, except_predict), FALSE, TRUE))
        
        if(outcome_type %in% c("binomial", "multinomial")){
          # Expect that the predicted_class column is a factor.
          testthat::expect_s3_class(prediction_table$predicted_class, "factor")
          
          # Expect that the class levels are the same as those in the model.
          testthat::expect_equal(levels(prediction_table$predicted_class), get_outcome_class_levels(model))
        }
        
        # Expect that the trimmed model produces the same predictions.
        prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                           data=full_one_sample_data))
        
        testthat::expect_equal(prediction_table,
                               prediction_table_trim,
                               ignore_attr=TRUE)
      })
      
      # Test that models cannot predict for empty datasets.
      test_fun(paste0("Sample predictions for ", outcome_type, " can not be made using ", learner, " for an empty data set."), {
        # Expect predictions to be made.
        prediction_table <- suppressWarnings(.predict(model, data=empty_data))
        
        # Test that the predictions were successfully made.
        testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), FALSE)
      })
      
      # Test that models can be used to predict survival probabilities.
      if(outcome_type %in% c("survival", "competing_risk")){
        test_fun(paste0("Sample survival predictions for ", outcome_type, " can be made using ", learner, " for a complete data set."), {
          
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=full_data,
                                                        type="survival_probability",
                                                        time=1000))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                                 ifelse(learner %in% c(except_train, except_predict, except_predict_survival), FALSE, TRUE))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=full_data,
                                                             type="survival_probability",
                                                             time=1000))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
          
          # Predict stratification.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=full_data,
                                                        type="risk_stratification",
                                                        time=1000))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), !learner %in% c(except_train, except_predict))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=full_data,
                                                             type="risk_stratification",
                                                             time=1000))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
        })
        
        test_fun(paste0("Sample survival predictions for ", outcome_type, " can be made using ", learner, " for a one-sample data set."), {
          
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=full_one_sample_data,
                                                        type="survival_probability",
                                                        time=1000))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                                 ifelse(learner %in% c(except_train, except_predict, except_predict_survival), FALSE, TRUE))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=full_one_sample_data,
                                                             type="survival_probability",
                                                             time=1000))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
          
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=full_one_sample_data,
                                                        type="risk_stratification",
                                                        time=1000))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), !learner %in% c(except_train, except_predict))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=full_one_sample_data,
                                                             type="risk_stratification",
                                                             time=1000))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
          
        })
      }
      
      # Test that the model has variable importance.
      test_fun(paste0("Model has variable importance for ", outcome_type, " and ", learner, " for the complete data set."), {
        # Extract the variable importance table.
        vimp_table <- suppressWarnings(..vimp(model, data=full_data))
        
        # Extract the variable importanct table for the trimmed model.
        vimp_table_trim <- suppressWarnings(..vimp(trimmed_model,
                                                   data=full_data))
        
        if(has_vimp){
          # Get the number of features
          n_features <- get_n_features(full_data)
          
          # Expect that the vimp table has two rows.
          testthat::expect_equal(nrow(vimp_table) > 0 & nrow(vimp_table) <= n_features, TRUE)
          testthat::expect_equal(nrow(vimp_table_trim) > 0 & nrow(vimp_table_trim) <= n_features, TRUE)
          
          # Expect that the names in the vimp table correspond to those of the
          # features.
          testthat::expect_equal(all(vimp_table$name %in% get_feature_columns(full_data)), TRUE)
          testthat::expect_equal(all(vimp_table_trim$name %in% get_feature_columns(full_data)), TRUE)
          
        } else {
          # Expect that the vimp table has no rows.
          testthat::expect_equal(nrow(vimp_table), 0)
          testthat::expect_equal(nrow(vimp_table_trim), 0)
        }
      })
      
      
      
      #####Bootstrapped data set################################################
      # Train the model.
      model <- suppressWarnings(train(data=full_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=hyperparameters,
                                      learner=learner,
                                      time_max=1832,
                                      create_bootstrap=TRUE,
                                      trim_model=FALSE))
      
      # Test that models can be created.
      test_fun(paste0("Model for ", outcome_type, " can be created using ", learner, " using a complete data set."), {
        
        # Test that the model was successfully created.
        testthat::expect_equal(model_is_trained(model),
                               ifelse(learner %in% except_train, FALSE, TRUE))
        
        if(outcome_type == "survival"){
          # Calibration info is present
          testthat::expect_equal(has_calibration_info(model), TRUE)
        }
      })
      
      
      
      #####One-feature data set#################################################
      # Train the model.
      model <- suppressWarnings(train(data=one_feature_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=hyperparameters,
                                      learner=learner,
                                      time_max=1832))
      
      # Create a trimmed model.
      trimmed_model <- trim_model(model)
      
      # Test that models can be created.
      test_fun(paste0("Model for ", outcome_type, " can be created using ", learner, " using a one-feature data set."), {
        
        # Test that the model was successfully created.
        testthat::expect_equal(model_is_trained(model),
                               ifelse(learner %in% except_train, FALSE, TRUE))
        
        if(outcome_type == "survival"){
          # Calibration info is present
          testthat::expect_equal(has_calibration_info(model), TRUE)
        }
      })
      
      # Test that models can be used to predict the outcome.
      test_fun(paste0("Sample predictions for ", outcome_type, " can be made using ", learner, " for a one-feature data set."), {
        # Expect predictions to be made.
        prediction_table <- suppressWarnings(.predict(model,
                                                      data=one_feature_data))
        
        # Test that the predictions were successfully made.
        testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                               ifelse(learner %in% c(except_train, except_predict), FALSE, TRUE))
        
        if(outcome_type %in% c("binomial", "multinomial")){
          # Expect that the predicted_class column is a factor.
          testthat::expect_s3_class(prediction_table$predicted_class, "factor")
          
          # Expect that the class levels are the same as those in the model.
          testthat::expect_equal(levels(prediction_table$predicted_class), get_outcome_class_levels(model))
        }
        
        # Expect that the trimmed model produces the same predictions.
        prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                           data=one_feature_data))
        
        testthat::expect_equal(prediction_table,
                               prediction_table_trim,
                               ignore_attr=TRUE)
      })
      
      # Test that models can be used to predict the outcome.
      test_fun(paste0("Sample predictions for ", outcome_type, " can be made using ", learner, " for a one-feature, one-sample data set."), {
        # Expect predictions to be made.
        prediction_table <- suppressWarnings(.predict(model,
                                                      data=one_feature_one_sample_data))
        
        # Test that the predictions were successfully made.
        testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                               ifelse(learner %in% c(except_train, except_predict), FALSE, TRUE))
        
        if(outcome_type %in% c("binomial", "multinomial")){
          # Expect that the predicted_class column is a factor.
          testthat::expect_s3_class(prediction_table$predicted_class, "factor")
          
          # Expect that the class levels are the same as those in the model.
          testthat::expect_equal(levels(prediction_table$predicted_class), get_outcome_class_levels(model))
        }
        
        # Expect that the trimmed model produces the same predictions.
        prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                           data=one_feature_one_sample_data))
        
        testthat::expect_equal(prediction_table,
                               prediction_table_trim,
                               ignore_attr=TRUE)
      })
      
      # Test that models can be used to predict survival probabilities.
      if(outcome_type %in% c("survival", "competing_risk")){
        test_fun(paste0("Sample survival predictions for ", outcome_type, " can be made using ", learner, " for a one-feature data set."), {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=one_feature_data,
                                                        type="survival_probability",
                                                        time=1000))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=one_feature_data,
                                                             type="survival_probability",
                                                             time=1000))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                                 ifelse(learner %in% c(except_train, except_predict, except_predict_survival), FALSE, TRUE))
          
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=one_feature_data,
                                                        type="risk_stratification",
                                                        time=1000))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=one_feature_data,
                                                             type="risk_stratification",
                                                             time=1000))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), !learner %in% c(except_train, except_predict))
        })
        
        test_fun(paste0("Sample survival predictions for ", outcome_type, " can be made using ", learner, " for a one-feature, one-sample data set."), {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=one_feature_one_sample_data,
                                                        type="survival_probability",
                                                        time=1000))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                                 ifelse(learner %in% c(except_train, except_predict, except_predict_survival), FALSE, TRUE))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=one_feature_one_sample_data,
                                                             type="survival_probability",
                                                             time=1000))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
          
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=one_feature_one_sample_data,
                                                        type="risk_stratification",
                                                        time=1000))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), !learner %in% c(except_train, except_predict))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=one_feature_one_sample_data,
                                                             type="risk_stratification",
                                                             time=1000))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
        })
      }
      
      
      #####Bad data-set#########################################################
      # Train the model.
      model <- suppressWarnings(train(data=bad_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=hyperparameters,
                                      learner=learner,
                                      time_max=1832))
      
      # Test that models can be created.
      test_fun(paste0("Model for ", outcome_type, " can not be created using ", learner, " using a bad data set."), {
        
        # Test that the model was successfully created.
        testthat::expect_equal(model_is_trained(model), FALSE)
        
        if(outcome_type == "survival"){
          # Calibration info is absent.
          testthat::expect_equal(has_calibration_info(model), TRUE)
        }
      })
      
      
      #####No censoring data sets##################################################
      if(outcome_type %in% c("survival", "competing_risk")){
        
        # Set up non-censoring dataset.
        no_censoring_data <- test.create_good_data_no_censoring_set(outcome_type)
        
        # Train the model.
        model <- suppressWarnings(train(data=no_censoring_data,
                                        cluster_method="none",
                                        imputation_method="simple",
                                        hyperparameter_list=hyperparameters,
                                        learner=learner,
                                        time_max=1832))
        
        # Create a trimmed model.
        trimmed_model <- trim_model(model)
        
        # Test that models can be created.
        test_fun(paste0("Model for ", outcome_type, " can be created using ", learner, " using a data set without censoring."), {
          
          # Test that the model was successfully created.
          testthat::expect_equal(model_is_trained(model),
                                 ifelse(learner %in% except_train, FALSE, TRUE))
          
          if(outcome_type == "survival"){
            # Calibration info is present
            testthat::expect_equal(has_calibration_info(model), TRUE)
          }
        })
        
        # Test that models can be used to predict the outcome.
        test_fun(paste0("Sample predictions for ", outcome_type, " can be made using ", learner, " for a data set without censoring."), {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=no_censoring_data))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                                 ifelse(learner %in% c(except_train, except_predict), FALSE, TRUE))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=no_censoring_data))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
        })
        
        
        # Test that models can be used to predict survival probabilities.
        test_fun(paste0("Sample survival predictions for ", outcome_type, " can be made using ", learner, " for a data set without censoring."), {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=no_censoring_data,
                                                        type="survival_probability",
                                                        time=1000))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=no_censoring_data,
                                                             type="survival_probability",
                                                             time=1000))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                                 ifelse(learner %in% c(except_train, except_predict, except_predict_survival), FALSE, TRUE))
          
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=no_censoring_data,
                                                        type="risk_stratification",
                                                        time=1000))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=no_censoring_data,
                                                             type="risk_stratification",
                                                             time=1000))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), !learner %in% c(except_train, except_predict))
        })
      }
      
      #####Data set with one censored instance#####################################
      if(outcome_type %in% c("survival", "competing_risk")){
        
        # Set up non-censoring dataset.
        one_censoring_data <- test.create_good_data_one_censored_set(outcome_type)
        
        # Train the model.
        model <- suppressWarnings(train(data=one_censoring_data,
                                        cluster_method="none",
                                        imputation_method="simple",
                                        hyperparameter_list=hyperparameters,
                                        learner=learner,
                                        time_max=1832))
        
        # Create a trimmed model.
        trimmed_model <- trim_model(model)
        
        # Test that models can be created.
        test_fun(paste0("Model for ", outcome_type, " can be created using ", learner, " using a data set with one censored sample."), {
          
          # Test that the model was successfully created.
          testthat::expect_equal(model_is_trained(model),
                                 ifelse(learner %in% except_train, FALSE, TRUE))
          
          if(outcome_type == "survival"){
            # Calibration info is present
            testthat::expect_equal(has_calibration_info(model), TRUE)
          }
        })
        
        # Test that models can be used to predict the outcome.
        test_fun(paste0("Sample predictions for ", outcome_type, " can be made using ", learner, " for a data set with one censored sample."), {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=one_censoring_data))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                                 ifelse(learner %in% c(except_train, except_predict), FALSE, TRUE))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=one_censoring_data))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
        })
        
        
        # Test that models can be used to predict survival probabilities.
        test_fun(paste0("Sample survival predictions for ", outcome_type, " can be made using ", learner, " for a data set with one censored sample."), {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=one_censoring_data,
                                                        type="survival_probability",
                                                        time=1000))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=one_censoring_data,
                                                             type="survival_probability",
                                                             time=1000))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                                 ifelse(learner %in% c(except_train, except_predict, except_predict_survival), FALSE, TRUE))
          
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=one_censoring_data,
                                                        type="risk_stratification",
                                                        time=1000))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=one_censoring_data,
                                                             type="risk_stratification",
                                                             time=1000))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), !learner %in% c(except_train, except_predict))
        })
      }
      
      #####Data set with few censored instances#####################################
      if(outcome_type %in% c("survival", "competing_risk")){
        
        # Set up non-censoring dataset.
        few_censoring_data <- test.create_good_data_few_censored_set(outcome_type)
        
        # Train the model.
        model <- suppressWarnings(train(data=few_censoring_data,
                                        cluster_method="none",
                                        imputation_method="simple",
                                        hyperparameter_list=hyperparameters,
                                        learner=learner,
                                        time_max=1832))
        
        # Create a trimmed model.
        trimmed_model <- trim_model(model)
        
        # Test that models can be created.
        test_fun(paste0("Model for ", outcome_type, " can be created using ", learner, " using a data set with few censored samples."), {
          
          # Test that the model was successfully created.
          testthat::expect_equal(model_is_trained(model),
                                 ifelse(learner %in% except_train, FALSE, TRUE))
          
          if(outcome_type == "survival"){
            # Calibration info is present
            testthat::expect_equal(has_calibration_info(model), TRUE)
          }
        })
        
        # Test that models can be used to predict the outcome.
        test_fun(paste0("Sample predictions for ", outcome_type, " can be made using ", learner, " for a data set with few censored samples."), {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=few_censoring_data))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                                 ifelse(learner %in% c(except_train, except_predict), FALSE, TRUE))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=few_censoring_data))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
        })
        
        
        # Test that models can be used to predict survival probabilities.
        test_fun(paste0("Sample survival predictions for ", outcome_type, " can be made using ", learner, " for a data set with few censored samples."), {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=few_censoring_data,
                                                        type="survival_probability",
                                                        time=1000))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=few_censoring_data,
                                                             type="survival_probability",
                                                             time=1000))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                                 ifelse(learner %in% c(except_train, except_predict, except_predict_survival), FALSE, TRUE))
          
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model,
                                                        data=few_censoring_data,
                                                        type="risk_stratification",
                                                        time=1000))
          
          # Expect that the trimmed model produces the same predictions.
          prediction_table_trim <- suppressWarnings(.predict(trimmed_model,
                                                             data=few_censoring_data,
                                                             type="risk_stratification",
                                                             time=1000))
          
          testthat::expect_equal(prediction_table,
                                 prediction_table_trim,
                                 ignore_attr=TRUE)
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), !learner %in% c(except_train, except_predict))
        })
      }
    }
  }
}



test_all_vimp_methods_available <- function(vimp_methods){
  
  # Create placeholder flags.
  vimp_method_available <- logical(length(vimp_methods))
  names(vimp_method_available) <- vimp_methods
  
  # Iterate over learners.
  for(vimp_method in vimp_methods){
    
    # Determine if the learner is available for any outcome.
    for(outcome_type in c("count", "continuous", "binomial", "multinomial", "survival", "competing_risk")){
      
      # Create a familiarModel object.
      object <- methods::new("familiarVimpMethod",
                             outcome_type=outcome_type,
                             vimp_method=vimp_method)
      
      # Promote the learner to the right class.
      object <- promote_vimp_method(object=object)
      
      # Check if the learner is available for the outcome.
      if(is_available(object)){
        vimp_method_available[vimp_method] <- TRUE
        break
      }
    }
  }
  
  # Iterate over learners
  for(vimp_method in vimp_methods){
    testthat::test_that(paste0(vimp_method, " is available."), {
      testthat::expect_equal(unname(vimp_method_available[vimp_method]), TRUE)
    })
  }
}



test_all_vimp_methods <- function(vimp_methods,
                                  hyperparameter_list=NULL,
                                  debug=FALSE){
  
  if(debug){
    test_fun <- debug_test_that
    
  } else {
    test_fun <- testthat::test_that
  }
  
  # Iterate over the outcome type.
  for(outcome_type in c("count", "continuous", "binomial", "multinomial", "survival")){
    
    # Obtain data.
    full_data <- test.create_good_data_set(outcome_type)
    full_one_sample_data <- test.create_one_sample_data_set(outcome_type)
    full_one_invariant_data <- test.create_good_data_invariant_set(outcome_type)
    one_feature_data <- test.create_one_feature_data_set(outcome_type)
    one_feature_invariant_data <- test.create_one_feature_invariant_data_set(outcome_type)
    one_feature_one_sample_data <- test.create_one_feature_one_sample_data_set(outcome_type)
    empty_data <- test.create_empty_data_set(outcome_type)
    bad_data <- test.create_bad_data_set(outcome_type)
    
    # Iterate over variable importance methods.
    for(vimp_method in vimp_methods){
      
      # Create a familiarModel object.
      object <- methods::new("familiarVimpMethod",
                             outcome_type=outcome_type,
                             vimp_method=vimp_method)
      
      # Promote the learner to the right class.
      object <- promote_vimp_method(object=object)
      
      # Test if the learner is available for the current outcome_type
      if(!is_available(object)) next()
      
      # Parse hyperparameter list
      hyperparameters <- c(hyperparameter_list[[outcome_type]])
      
      # Find required hyperparameters
      vimp_method_hyperparameters <- .get_preset_hyperparameters(fs_method = vimp_method,
                                                                 outcome_type=outcome_type,
                                                                 names_only=TRUE)
      
      # Select hyperparameters that are being used, and are present in the input
      # list of hyperparameters.
      hyperparameters <- hyperparameters[intersect(vimp_method_hyperparameters, names(hyperparameters))]
      
      #####Full data set########################################################
      
      # Process dataset.
      vimp_object <- prepare_vimp_object(data=full_data,
                                         vimp_method=vimp_method,
                                         vimp_method_parameter_list=hyperparameters,
                                         outcome_type=outcome_type,
                                         cluster_method="none",
                                         imputation_method="simple")
      
      
      test_fun(paste0("Variable importance can be computed for ", outcome_type, " with the ", vimp_method, " using a complete data set."), {
        
        vimp_table <- suppressWarnings(.vimp(vimp_object, full_data))
        
        # Get the number of features
        n_features <- get_n_features(full_data)
        
        # Expect that the vimp table is not empty..
        testthat::expect_equal(nrow(vimp_table) > 0 & nrow(vimp_table) <= n_features, TRUE)
        
        # Expect that the names in the vimp table correspond to those of the
        # features.
        testthat::expect_equal(all(vimp_table$name %in% get_feature_columns(full_data)), TRUE)
      })
      
      
      test_fun(paste0("Variable importance can be computed for ", outcome_type, " with the ", vimp_method, " using a complete data set with one invariant feature."), {
        
        vimp_table <- suppressWarnings(.vimp(vimp_object, full_one_invariant_data))
        
        # Get the number of features
        n_features <- get_n_features(full_one_invariant_data)
        
        # Expect that the vimp table is not empty..
        testthat::expect_equal(nrow(vimp_table) > 0 & nrow(vimp_table) <= n_features, TRUE)
        
        # Expect that the names in the vimp table correspond to those of the
        # features.
        testthat::expect_equal(all(vimp_table$name %in% get_feature_columns(full_one_invariant_data)), TRUE)
      })
      
      
      test_fun(paste0("Variable importance cannot be computed for ", outcome_type, " with the ", vimp_method, " using an empty data set."), {
        
        vimp_table <- suppressWarnings(.vimp(vimp_object, empty_data))
       
        # Expect that the vimp table has two rows.
        testthat::expect_equal(is_empty(vimp_table), TRUE)
      })
      
      
      test_fun(paste0("Variable importance cannot be computed for ", outcome_type, " with the ", vimp_method, " using a bad data set."), {
        
        vimp_table <- suppressWarnings(.vimp(vimp_object, bad_data))
        
        # Expect that the vimp table has two rows.
        testthat::expect_equal(is_empty(vimp_table), TRUE)
      })
      
      
      test_fun(paste0("Variable importance cannot be computed for ", outcome_type, " with the ", vimp_method, " using a one-sample data set."), {
        
        vimp_table <- suppressWarnings(.vimp(vimp_object, full_one_sample_data))
        
        # Expect that the vimp table has two rows.
        testthat::expect_equal(is_empty(vimp_table), TRUE)
      })
      
      
      #####One-feature data set#################################################
      
      # Process dataset.
      vimp_object <- prepare_vimp_object(data=one_feature_data,
                                                    vimp_method=vimp_method,
                                                    vimp_method_parameter_list=hyperparameters,
                                                    outcome_type=outcome_type,
                                                    cluster_method="none",
                                                    imputation_method="simple")
      
      
      test_fun(paste0("Variable importance can be computed for ", outcome_type, " with the ", vimp_method, " using a one-feature data set."), {
        
        vimp_table <- suppressWarnings(.vimp(vimp_object, one_feature_data))
        
        # Expect that the vimp table is not empty.
        testthat::expect_equal(nrow(vimp_table), 1)
        
        # Expect that the names in the vimp table correspond to those of the
        # features.
        testthat::expect_equal(all(vimp_table$name %in% get_feature_columns(one_feature_data)), TRUE)
      })
      
      
      test_fun(paste0("Variable importance cannot be computed for ", outcome_type, " with the ", vimp_method, " using a one-feature data set with an invariant feature."), {
        
        vimp_table <- suppressWarnings(.vimp(vimp_object, one_feature_invariant_data))
        
        # Expect that the vimp table is empty.
        testthat::expect_equal(is_empty(vimp_table), TRUE)
      })
      
      
      test_fun(paste0("Variable importance cannot be computed for ", outcome_type, " with the ", vimp_method, " using a one-feature, one-sample data set."), {
        
        vimp_table <- suppressWarnings(.vimp(vimp_object, one_feature_one_sample_data))
        
        # Expect that the vimp table is empty.
        testthat::expect_equal(is_empty(vimp_table), TRUE)
      })
      
      if(outcome_type %in% c("survival", "competing_risk")){
        #####No censoring data set########################################################
        
        no_censoring_data <- test.create_good_data_no_censoring_set(outcome_type)
        
        # Process dataset.
        vimp_object <- prepare_vimp_object(data=no_censoring_data,
                                           vimp_method=vimp_method,
                                           vimp_method_parameter_list=hyperparameters,
                                           outcome_type=outcome_type,
                                           cluster_method="none",
                                           imputation_method="simple")
        
        
        test_fun(paste0("Variable importance can be computed for ", outcome_type, " with the ", vimp_method, " using a data set without censoring."), {
          
          vimp_table <- suppressWarnings(.vimp(vimp_object, no_censoring_data))
          
          # Get the number of features
          n_features <- get_n_features(full_data)
          
          # Expect that the vimp table is not empty..
          testthat::expect_equal(nrow(vimp_table) > 0 & nrow(vimp_table) <= n_features, TRUE)
          
          # Expect that the names in the vimp table correspond to those of the
          # features.
          testthat::expect_equal(all(vimp_table$name %in% get_feature_columns(full_data)), TRUE)
        })
        
        
        #####Data set with one censored instance########################################################
        
        one_censored_data <- test.create_good_data_one_censored_set(outcome_type)
        
        # Process dataset.
        vimp_object <- prepare_vimp_object(data=one_censored_data,
                                           vimp_method=vimp_method,
                                           vimp_method_parameter_list=hyperparameters,
                                           outcome_type=outcome_type,
                                           cluster_method="none",
                                           imputation_method="simple")
        
        
        test_fun(paste0("Variable importance can be computed for ", outcome_type, " with the ", vimp_method, " using a data set with one censored instance."), {
          
          vimp_table <- suppressWarnings(.vimp(vimp_object, one_censored_data))
          
          # Get the number of features
          n_features <- get_n_features(full_data)
          
          # Expect that the vimp table is not empty..
          testthat::expect_equal(nrow(vimp_table) > 0 & nrow(vimp_table) <= n_features, TRUE)
          
          # Expect that the names in the vimp table correspond to those of the
          # features.
          testthat::expect_equal(all(vimp_table$name %in% get_feature_columns(full_data)), TRUE)
        })
        
        
        #####Data set with few censored instances########################################################
        
        few_censored_data <- test.create_good_data_few_censored_set(outcome_type)
        
        # Process dataset.
        vimp_object <- prepare_vimp_object(data=few_censored_data,
                                           vimp_method=vimp_method,
                                           vimp_method_parameter_list=hyperparameters,
                                           outcome_type=outcome_type,
                                           cluster_method="none",
                                           imputation_method="simple")
        
        
        test_fun(paste0("Variable importance can be computed for ", outcome_type, " with the ", vimp_method, " using a data set with few censored instances."), {
          
          vimp_table <- suppressWarnings(.vimp(vimp_object, few_censored_data))
          
          # Get the number of features
          n_features <- get_n_features(full_data)
          
          # Expect that the vimp table is not empty..
          testthat::expect_equal(nrow(vimp_table) > 0 & nrow(vimp_table) <= n_features, TRUE)
          
          # Expect that the names in the vimp table correspond to those of the
          # features.
          testthat::expect_equal(all(vimp_table$name %in% get_feature_columns(full_data)), TRUE)
        })
      }
    }
  }
}



test_all_metrics_available <- function(metrics){
  
  # Create placeholder flags.
  metric_available <- logical(length(metrics))
  names(metric_available) <- metrics
  
  # Iterate over metrics
  for(metric in metrics){
    
    # Determine if the metric is available for any outcome.
    for(outcome_type in c("count", "continuous", "binomial", "multinomial", "survival", "competing_risk")){
      
      # Create a metric object
      object <- as_metric(metric=metric,
                          outcome_type=outcome_type)
      
      # Check if the learner is available for the outcome.
      if(is_available(object)){
        metric_available[metric] <- TRUE
        break
      }
    }
  }
  
  # Iterate over learners
  for(metric in metrics){
    testthat::test_that(paste0(metric, " is available."), {
      testthat::expect_equal(unname(metric_available[metric]), TRUE)
    })
  }
}



test_all_metrics <- function(metrics,
                             except_one_sample=FALSE,
                             except_identical=FALSE,
                             except_same_prediction=FALSE){
  
  # Iterate over the outcome type.
  for(outcome_type in c("count", "continuous", "binomial", "multinomial", "survival")){
    
    # Obtain data.
    full_data <- test.create_good_data_set(outcome_type)
    identical_sample_data <- test.create_all_identical_data_set(outcome_type)
    full_one_sample_data <- test.create_one_sample_data_set(outcome_type)
    one_feature_data <- test.create_one_feature_data_set(outcome_type)
    one_feature_one_sample_data <- test.create_one_feature_one_sample_data_set(outcome_type)
    one_feature_invariant_data <- test.create_one_feature_invariant_data_set(outcome_type)
    empty_data <- test.create_empty_data_set(outcome_type)
    bad_data <- test.create_bad_data_set(outcome_type)
    no_censoring_data <- test.create_good_data_no_censoring_set(outcome_type)
    one_censored_data <- test.create_good_data_one_censored_set(outcome_type)
    few_censored_data <- test.create_good_data_few_censored_set(outcome_type)
    
    # Set exceptions per outcome type.
    .except_one_sample <- except_one_sample
    if(is.character(.except_one_sample)) .except_one_sample <- any(.except_one_sample == outcome_type)
    
    .except_identical <- except_identical
    if(is.character(.except_identical)) .except_identical <- any(.except_identical == outcome_type)
    
    .except_same_prediction <- except_same_prediction
    if(is.character(.except_same_prediction)) .except_same_prediction <- any(.except_same_prediction == outcome_type)
    
    # Iterate over metrics
    for(metric in metrics){
      
      # Check if the metric is available for the current outcome type, and skip
      # otherwise.
      if(!metric.check_outcome_type(metric=metric, outcome_type=outcome_type, as_flag=TRUE)) break()
      
      # Parse hyperparameter list
      hyperparameters <- list("sign_size"=get_n_features(full_data),
                              "family"=switch(outcome_type,
                                              "continuous"="gaussian",
                                              "count"="poisson",
                                              "binomial"="logistic",
                                              "multinomial"="multinomial",
                                              "survival"="cox"))
      
      #####Full data set########################################################
      
      # Train the model.
      model <- suppressWarnings(train(data=full_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=hyperparameters,
                                      learner="glm",
                                      time_max=1832))
      
      # Create metric object
      metric_object <- as_metric(metric=metric,
                                 object=model)
      
      # Test that metric values can be computed for the full model.
      testthat::test_that(paste0("1A. Model performance for ", outcome_type, " outcomes can be assessed by the ",
                                 metric_object@name, " (", metric_object@metric, ") metric for a complete data set."), {
                                   
                                   # Expect predictions to be made.
                                   prediction_table <- suppressWarnings(.predict(model, data=full_data))
                                   
                                   # Test that the predictions were successfully made.
                                   testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), TRUE)
                                   
                                   if(outcome_type %in% c("binomial", "multinomial")){
                                     # Expect that the predicted_class column is
                                     # a factor.
                                     testthat::expect_s3_class(prediction_table$predicted_class, "factor")
                                     
                                     # Expect that the class levels are the same
                                     # as those in the model.
                                     testthat::expect_equal(levels(prediction_table$predicted_class), get_outcome_class_levels(model))
                                   }
                                   
                                   # Compute a score.
                                   score <- compute_metric_score(metric=metric_object,
                                                                 data=prediction_table,
                                                                 object=model)
                                   
                                   # Compute an objective score.
                                   objective_score <- compute_objective_score(metric=metric_object,
                                                                              data=prediction_table,
                                                                              object=model)
                                   
                                   # Expect that the score is a finite,
                                   # non-missing number.
                                   testthat::expect_equal(data.table::between(score,
                                                                              lower=metric_object@value_range[1],
                                                                              upper=metric_object@value_range[2]),
                                                          TRUE)
                                   
                                   # Expect that the objective score is a
                                   # non-missing number in the range [-1, 1].
                                   testthat::expect_equal(data.table::between(objective_score,
                                                                              lower=-1.0,
                                                                              upper=1.0),
                                                          TRUE)
                                 })
      
      if(outcome_type %in% c("survival", "competing_risk")){
        # Test that metric values can be computed for the full model, but with
        # data without censored instances.
        testthat::test_that(paste0("1B. Model performance for ", outcome_type, " outcomes can be assessed by the ",
                                   metric_object@name, " (", metric_object@metric, ") metric for a data set without censoring."), {
                                     
                                     # Expect predictions to be made.
                                     prediction_table <- suppressWarnings(.predict(model, data=no_censoring_data))
                                     
                                     # Test that the predictions were successfully made.
                                     testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), TRUE)
                                     
                                     if(outcome_type %in% c("binomial", "multinomial")){
                                       # Expect that the predicted_class column is
                                       # a factor.
                                       testthat::expect_s3_class(prediction_table$predicted_class, "factor")
                                       
                                       # Expect that the class levels are the same
                                       # as those in the model.
                                       testthat::expect_equal(levels(prediction_table$predicted_class), get_outcome_class_levels(model))
                                     }
                                     
                                     # Compute a score.
                                     score <- compute_metric_score(metric=metric_object,
                                                                   data=prediction_table,
                                                                   object=model)
                                     
                                     # Compute an objective score.
                                     objective_score <- compute_objective_score(metric=metric_object,
                                                                                data=prediction_table,
                                                                                object=model)
                                     
                                     # Expect that the score is a finite,
                                     # non-missing number.
                                     testthat::expect_equal(data.table::between(score,
                                                                                lower=metric_object@value_range[1],
                                                                                upper=metric_object@value_range[2]),
                                                            TRUE)
                                     
                                     # Expect that the objective score is a
                                     # non-missing number in the range [-1, 1].
                                     testthat::expect_equal(data.table::between(objective_score,
                                                                                lower=-1.0,
                                                                                upper=1.0),
                                                            TRUE)
                                   })
        
        # Test that metric values can be computed for the full model, but with
        # data with one censored instance.
        testthat::test_that(paste0("1C. Model performance for ", outcome_type, " outcomes can be assessed by the ",
                                   metric_object@name, " (", metric_object@metric, ") metric for a data set with one censored insteances."), {
                                     
                                     # Expect predictions to be made.
                                     prediction_table <- suppressWarnings(.predict(model, data=one_censored_data))
                                     
                                     # Test that the predictions were successfully made.
                                     testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), TRUE)
                                     
                                     if(outcome_type %in% c("binomial", "multinomial")){
                                       # Expect that the predicted_class column is
                                       # a factor.
                                       testthat::expect_s3_class(prediction_table$predicted_class, "factor")
                                       
                                       # Expect that the class levels are the same
                                       # as those in the model.
                                       testthat::expect_equal(levels(prediction_table$predicted_class), get_outcome_class_levels(model))
                                     }
                                     
                                     # Compute a score.
                                     score <- compute_metric_score(metric=metric_object,
                                                                   data=prediction_table,
                                                                   object=model)
                                     
                                     # Compute an objective score.
                                     objective_score <- compute_objective_score(metric=metric_object,
                                                                                data=prediction_table,
                                                                                object=model)
                                     
                                     # Expect that the score is a finite,
                                     # non-missing number.
                                     testthat::expect_equal(data.table::between(score,
                                                                                lower=metric_object@value_range[1],
                                                                                upper=metric_object@value_range[2]),
                                                            TRUE)
                                     
                                     # Expect that the objective score is a
                                     # non-missing number in the range [-1, 1].
                                     testthat::expect_equal(data.table::between(objective_score,
                                                                                lower=-1.0,
                                                                                upper=1.0),
                                                            TRUE)
                                   })
        
        # Test that metric values can be computed for the full model, but with
        # data with few censored instances.
        testthat::test_that(paste0("1D. Model performance for ", outcome_type, " outcomes can be assessed by the ",
                                   metric_object@name, " (", metric_object@metric, ") metric for a data set with few censored samples."), {
                                     
                                     # Expect predictions to be made.
                                     prediction_table <- suppressWarnings(.predict(model, data=few_censored_data))
                                     
                                     # Test that the predictions were successfully made.
                                     testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), TRUE)
                                     
                                     if(outcome_type %in% c("binomial", "multinomial")){
                                       # Expect that the predicted_class column is
                                       # a factor.
                                       testthat::expect_s3_class(prediction_table$predicted_class, "factor")
                                       
                                       # Expect that the class levels are the same
                                       # as those in the model.
                                       testthat::expect_equal(levels(prediction_table$predicted_class), get_outcome_class_levels(model))
                                     }
                                     
                                     # Compute a score.
                                     score <- compute_metric_score(metric=metric_object,
                                                                   data=prediction_table,
                                                                   object=model)
                                     
                                     # Compute an objective score.
                                     objective_score <- compute_objective_score(metric=metric_object,
                                                                                data=prediction_table,
                                                                                object=model)
                                     
                                     # Expect that the score is a finite,
                                     # non-missing number.
                                     testthat::expect_equal(data.table::between(score,
                                                                                lower=metric_object@value_range[1],
                                                                                upper=metric_object@value_range[2]),
                                                            TRUE)
                                     
                                     # Expect that the objective score is a
                                     # non-missing number in the range [-1, 1].
                                     testthat::expect_equal(data.table::between(objective_score,
                                                                                lower=-1.0,
                                                                                upper=1.0),
                                                            TRUE)
                                   })
      }
      
      # Test that metric values cannot be computed for a one-sample dataset.
      testthat::test_that(paste0("2. Model performance for ", outcome_type, " outcomes ",
                                 ifelse(.except_one_sample, "cannot", "can"),
                                 " be assessed by the ", metric_object@name,
                                 " (", metric_object@metric, ") metric for a one-sample data set."), {
                                   
                                   # Expect predictions to be made.
                                   prediction_table <- suppressWarnings(.predict(model, data=full_one_sample_data))
                                   
                                   # Test that the predictions were successfully made.
                                   testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), TRUE)
                                   
                                   if(outcome_type %in% c("binomial", "multinomial")){
                                     # Expect that the predicted_class column is
                                     # a factor.
                                     testthat::expect_s3_class(prediction_table$predicted_class, "factor")
                                     
                                     # Expect that the class levels are the same
                                     # as those in the model.
                                     testthat::expect_equal(levels(prediction_table$predicted_class), get_outcome_class_levels(model))
                                   }
                                   
                                   # Compute a score.
                                   score <- compute_metric_score(metric=metric_object,
                                                                 data=prediction_table,
                                                                 object=model)
                                   
                                   # Compute an objective score.
                                   objective_score <- compute_objective_score(metric=metric_object,
                                                                              data=prediction_table,
                                                                              object=model)
                                   
                                   # Expect that the score is a finite,
                                   # non-missing number, and NA otherwise.
                                   if(.except_one_sample){
                                     testthat::expect_equal(is.na(score), TRUE)
                                   } else {
                                     testthat::expect_equal(data.table::between(score,
                                                                                lower=metric_object@value_range[1],
                                                                                upper=metric_object@value_range[2]),
                                                            TRUE)
                                   }
                                   
                                   # Expect that the objective score is a
                                   # non-missing number in the range [-1, 1] and
                                   # NA otherwise.
                                   if(.except_one_sample){
                                     testthat::expect_equal(is.na(objective_score), TRUE)
                                   } else {
                                     testthat::expect_equal(data.table::between(objective_score,
                                                                                lower=-1.0,
                                                                                upper=1.0),
                                                            TRUE)
                                   }
                                 })
      
      # Test that metric values cannot be computed for the empty model.
      testthat::test_that(paste0("3. Model performance for ", outcome_type, " outcomes cannot be assessed by the ",
                                 metric_object@name, " (", metric_object@metric, ") metric for an empty data set."), {
                                   
                                   # Expect predictions to be made.
                                   prediction_table <- suppressWarnings(.predict(model, data=empty_data))
                                   
                                   # Test that the predictions were successfully made.
                                   testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), FALSE)
                                   
                                   # Compute a score.
                                   score <- compute_metric_score(metric=metric_object,
                                                                 data=prediction_table,
                                                                 object=model)
                                   
                                   # Compute an objective score.
                                   objective_score <- compute_objective_score(metric=metric_object,
                                                                              data=prediction_table,
                                                                              object=model)
                                   
                                   # Expect that the score is NA.
                                   testthat::expect_equal(is.na(score), TRUE)
                                   
                                   # Expect that the objective score is NA.
                                   testthat::expect_equal(is.na(objective_score), TRUE)
                                 })
      
      
      # Test that metric values can be computed for a dataset where are samples identical.
      testthat::test_that(paste0("4. Model performance for ", outcome_type, " outcomes ",
                                 ifelse(.except_identical, "cannot", "can"),
                                 " be assessed by the ",
                                 metric_object@name, " (", metric_object@metric, ") metric for a dataset with identical samples."), {
                                   
                                   # Expect predictions to be made.
                                   prediction_table <- suppressWarnings(.predict(model, data=identical_sample_data))
                                   
                                   # Test that the predictions were successfully made.
                                   testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), TRUE)
                                   
                                   # Compute a score.
                                   score <- compute_metric_score(metric=metric_object,
                                                                 data=prediction_table,
                                                                 object=model)
                                   
                                   # Compute an objective score.
                                   objective_score <- compute_objective_score(metric=metric_object,
                                                                              data=prediction_table,
                                                                              object=model)
                                   
                                   # Expect that the score is a finite,
                                   # non-missing number.
                                   if(.except_identical){
                                     testthat::expect_equal(is.na(score), TRUE)
                                     
                                   } else {
                                     testthat::expect_equal(data.table::between(score,
                                                                                lower=metric_object@value_range[1],
                                                                                upper=metric_object@value_range[2]),
                                                            TRUE)
                                   }
                                   
                                   
                                   # Expect that the objective score is a
                                   # non-missing number in the range [-1, 1].
                                   if(.except_identical){
                                     testthat::expect_equal(is.na(objective_score), TRUE)
                                     
                                   } else {
                                     testthat::expect_equal(data.table::between(objective_score,
                                                                                lower=-1.0,
                                                                                upper=1.0),
                                                            TRUE)
                                   }
                                 })
      
      
      
      #####One-feature data set#################################################
      # Train the model.
      model <- suppressWarnings(train(data=one_feature_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=hyperparameters,
                                      learner="glm",
                                      time_max=1832))
      
      # Create metric object
      metric_object <- as_metric(metric=metric,
                                 object=model)
      
      # Test that metric values can be computed for the one-feature model.
      testthat::test_that(paste0("5. Model performance for ", outcome_type, " outcomes can be assessed by the ",
                                 metric_object@name, " (", metric_object@metric, ") metric for a one-feature data set."), {
                                   
                                   # Expect predictions to be made.
                                   prediction_table <- suppressWarnings(.predict(model, data=one_feature_data))
                                   
                                   # Test that the predictions were successfully made.
                                   testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), TRUE)
                                   
                                   if(outcome_type %in% c("binomial", "multinomial")){
                                     # Expect that the predicted_class column is
                                     # a factor.
                                     testthat::expect_s3_class(prediction_table$predicted_class, "factor")
                                     
                                     # Expect that the class levels are the same
                                     # as those in the model.
                                     testthat::expect_equal(levels(prediction_table$predicted_class), get_outcome_class_levels(model))
                                   }
                                   
                                   # Compute a score.
                                   score <- compute_metric_score(metric=metric_object,
                                                                 data=prediction_table,
                                                                 object=model)
                                   
                                   # Compute an objective score.
                                   objective_score <- compute_objective_score(metric=metric_object,
                                                                              data=prediction_table,
                                                                              object=model)
                                   
                                   # Expect that the score is a finite,
                                   # non-missing number.
                                   testthat::expect_equal(data.table::between(score,
                                                                              lower=metric_object@value_range[1],
                                                                              upper=metric_object@value_range[2]),
                                                          TRUE)
                                   
                                   # Expect that the objective score is a
                                   # non-missing number in the range [-1, 1].
                                   testthat::expect_equal(data.table::between(objective_score,
                                                                              lower=-1.0,
                                                                              upper=1.0),
                                                          TRUE)
                                 })
      
      
      # Test that metric values cannot be computed for a one-sample dataset.
      testthat::test_that(paste0("6. Model performance for ", outcome_type, " outcomes ",
                                 ifelse(.except_one_sample, "cannot", "can"),
                                 " be assessed by the ", metric_object@name,
                                 " (", metric_object@metric, ") metric for a one-feature, one-sample data set."), {
                                   
                                   # Expect predictions to be made.
                                   prediction_table <- suppressWarnings(.predict(model, data=one_feature_one_sample_data))
                                   
                                   # Test that the predictions were successfully made.
                                   testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), TRUE)
                                   
                                   if(outcome_type %in% c("binomial", "multinomial")){
                                     # Expect that the predicted_class column is
                                     # a factor.
                                     testthat::expect_s3_class(prediction_table$predicted_class, "factor")
                                     
                                     # Expect that the class levels are the same
                                     # as those in the model.
                                     testthat::expect_equal(levels(prediction_table$predicted_class), get_outcome_class_levels(model))
                                   }
                                   
                                   # Compute a score.
                                   score <- compute_metric_score(metric=metric_object,
                                                                 data=prediction_table,
                                                                 object=model)
                                   
                                   # Compute an objective score.
                                   objective_score <- compute_objective_score(metric=metric_object,
                                                                              data=prediction_table,
                                                                              object=model)
                                   
                                   # Expect that the score is a finite,
                                   # non-missing number, and NA otherwise.
                                   if(.except_one_sample){
                                     testthat::expect_equal(is.na(score), TRUE)
                                   } else {
                                     testthat::expect_equal(data.table::between(score,
                                                                                lower=metric_object@value_range[1],
                                                                                upper=metric_object@value_range[2]),
                                                            TRUE)
                                   }
                                   
                                   # Expect that the objective score is a
                                   # non-missing number in the range [-1, 1] and
                                   # NA otherwise.
                                   if(.except_one_sample){
                                     testthat::expect_equal(is.na(objective_score), TRUE)
                                   } else {
                                     testthat::expect_equal(data.table::between(objective_score,
                                                                                lower=-1.0,
                                                                                upper=1.0),
                                                            TRUE)
                                   }
                                 })
      
      
      # Test that metric values can be computed for the one-feature model with
      # invariant predicted outcomes for all samples.
      testthat::test_that(paste0("7. Model performance for ", outcome_type, " outcomes ",
                                 ifelse(.except_same_prediction, "can", "cannot"),
                                 " be assessed by the ",
                                 metric_object@name, " (", metric_object@metric, ") metric for a one-feature data set with identical predictions."), {
                                   
                                   # Expect predictions to be made.
                                   prediction_table <- suppressWarnings(.predict(model, data=one_feature_invariant_data))
                                   
                                   # Test that the predictions were successfully made.
                                   testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), TRUE)
                                   
                                   if(outcome_type %in% c("binomial", "multinomial")){
                                     # Expect that the predicted_class column is
                                     # a factor.
                                     testthat::expect_s3_class(prediction_table$predicted_class, "factor")
                                     
                                     # Expect that the class levels are the same
                                     # as those in the model.
                                     testthat::expect_equal(levels(prediction_table$predicted_class), get_outcome_class_levels(model))
                                   }
                                   
                                   # Compute a score.
                                   score <- compute_metric_score(metric=metric_object,
                                                                 data=prediction_table,
                                                                 object=model)
                                   
                                   # Compute an objective score.
                                   objective_score <- compute_objective_score(metric=metric_object,
                                                                              data=prediction_table,
                                                                              object=model)
                                   
                                   # Expect that the score is a finite,
                                   # non-missing number, and NA otherwise.
                                   if(.except_same_prediction){
                                     testthat::expect_equal(is.na(score), TRUE)
                                   } else {
                                     testthat::expect_equal(data.table::between(score,
                                                                                lower=metric_object@value_range[1],
                                                                                upper=metric_object@value_range[2]),
                                                            TRUE)
                                   }
                                   
                                   # Expect that the objective score is a
                                   # non-missing number in the range [-1, 1] and
                                   # NA otherwise.
                                   if(.except_same_prediction){
                                     testthat::expect_equal(is.na(objective_score), TRUE)
                                   } else {
                                     testthat::expect_equal(data.table::between(objective_score,
                                                                                lower=-1.0,
                                                                                upper=1.0),
                                                            TRUE)
                                   }
                                 })
      
      
      #####Bad data-set#########################################################
      # Train the model.
      model <- suppressWarnings(train(data=bad_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=hyperparameters,
                                      learner="glm",
                                      time_max=1832))
      
      # Create metric object
      metric_object <- as_metric(metric=metric,
                                 object=model)
      
      # Test that metric values can be computed for the one-feature model with
      # invariant predicted outcomes for all samples.
      testthat::test_that(paste0("8. Model performance for ", outcome_type, " outcomes cannot be assessed by the ",
                                 metric_object@name, " (", metric_object@metric, ") metric for a bad dataset where the model fails to train."), {
                                   
                                   # Expect predictions to be made.
                                   prediction_table <- suppressWarnings(.predict(model, data=bad_data))
                                   
                                   # Test that the predictions were successfully made.
                                   testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), FALSE)
                                   
                                   if(outcome_type %in% c("binomial", "multinomial")){
                                     # Expect that the predicted_class column is
                                     # a factor.
                                     testthat::expect_s3_class(prediction_table$predicted_class, "factor")
                                     
                                     # Expect that the class levels are the same
                                     # as those in the model.
                                     testthat::expect_equal(levels(prediction_table$predicted_class), get_outcome_class_levels(model))
                                   }
                                   
                                   # Compute a score.
                                   score <- compute_metric_score(metric=metric_object,
                                                                 data=prediction_table,
                                                                 object=model)
                                   
                                   # Compute an objective score.
                                   objective_score <- compute_objective_score(metric=metric_object,
                                                                              data=prediction_table,
                                                                              object=model)
                                   
                                   # Expect that the score is NA.
                                   testthat::expect_equal(is.na(score), TRUE)
                                   
                                   # Expect that the objective score is a
                                   # non-missing number in the range [-1, 1].
                                   testthat::expect_equal(is.na(objective_score), TRUE)
                                 })
    }
    
  }
  
}



test_hyperparameter_optimisation <- function(vimp_methods=NULL,
                                             learners=NULL,
                                             outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                                             always_available=FALSE,
                                             no_hyperparameters=FALSE,
                                             n_max_bootstraps=25L,
                                             n_max_optimisation_steps=3L,
                                             n_max_intensify_steps=2L,
                                             n_random_sets=20L,
                                             n_challengers=10L,
                                             ...,
                                             test_specific_config=FALSE,
                                             debug=FALSE,
                                             parallel=waiver()){
  
  if(debug){
    test_fun <- debug_test_that
    verbose <- TRUE
  } else {
    test_fun <- testthat::test_that
    verbose <- FALSE
  }
  
  # Set parallelisation.
  if(is.waive(parallel)) parallel <- !debug
  
  if(parallel){
    # Set options.
    # Disable randomForestSRC OpenMP core use.
    options(rf.cores=as.integer(1))
    on.exit(options(rf.cores=-1L), add=TRUE)
    
    # Disable multithreading on data.table to prevent reduced performance due to
    # resource collisions with familiar parallelisation.
    data.table::setDTthreads(1L)
    on.exit(data.table::setDTthreads(0L), add=TRUE)
    
    # Start local cluster in the overall process.
    cl <- .test_start_cluster(n_cores=4L)
    on.exit(.terminate_cluster(cl), add=TRUE)
    
  } else {
    cl <- NULL
  }
  
  # Clean up dots
  dots <- list(...)
  dots$cl <- NULL
  dots$verbose <- NULL
  
  
  if(is.null(learners)){
    is_vimp <- TRUE
    method_pool <- vimp_methods
    
    if(is.null(learners)) learners <- "glm"
    
  } else {
    is_vimp <- FALSE
    method_pool <- learners
    
    if(is.null(vimp_methods)) vimp_methods <- "mim"
  }
  
  # Iterate over the outcome type.
  for(outcome_type in outcome_type_available){

    # Multi-feature data sets.
    full_data <- test.create_good_data_set(outcome_type)
    identical_sample_data <- test.create_all_identical_data_set(outcome_type)
    full_one_sample_data <- test.create_one_sample_data_set(outcome_type)
    empty_data <- test.create_empty_data_set(outcome_type)
    
    # One-feature data sets.
    one_feature_data <- test.create_one_feature_data_set(outcome_type)
    one_feature_one_sample_data <- test.create_one_feature_one_sample_data_set(outcome_type)
    one_feature_invariant_data <- test.create_one_feature_invariant_data_set(outcome_type)

    # Set exceptions per outcome type.
    .always_available <- always_available
    if(is.character(.always_available)) .always_available <- any(.always_available == outcome_type)
    
    # Iterate over learners or variable importance methods..
    for(current_method in method_pool){
      
      if(is_vimp){
        learner <- learners
        vimp_method <- current_method
        
      } else {
        learner <- current_method
        vimp_method <- vimp_methods
      }
      
      
      if(!learner.check_outcome_type(learner=learner, outcome_type=outcome_type, as_flag=TRUE)) next()
      if(!vimp.check_outcome_type(method=vimp_method, outcome_type=outcome_type, as_flag=TRUE)) next()
      
      #####Full data set--------------------------------------------------------
      
      # Create object
      object <- .test_create_hyperparameter_object(data=full_data,
                                                   vimp_method=vimp_method,
                                                   learner=learner,
                                                   is_vimp=is_vimp,
                                                   set_signature_feature=TRUE)
      
      # Check that object is available for the outcome.
      if(!is_available(object)) next()
      
      if(verbose) message(paste0("\nComputing hyperparameters for ", current_method,
                                 ifelse(is_vimp, " variable importance method", " learner"), " and ",
                                 outcome_type, " outcomes for a complete data set."))
      
      # Hyperparameter optimisation on a full dataset.
      new_object <- do.call(optimise_hyperparameters,
                            args=c(list("object"=object,
                                        "data"=full_data,
                                        "cl"=cl,
                                        "n_max_bootstraps"=n_max_bootstraps,
                                        "n_max_optimisation_steps"=n_max_optimisation_steps,
                                        "n_max_intensify_steps"=n_max_intensify_steps,
                                        "n_random_sets"=n_random_sets,
                                        "n_challengers"=n_challengers,
                                        "is_vimp"=is_vimp,
                                        "verbose"=verbose),
                                   dots))

      # Test that hyperparameters were set.
      test_fun(paste0("1. Hyperparameters for the ", current_method,
                      ifelse(is_vimp, " variable importance method", " learner"), " and ",
                      outcome_type, " outcomes can be created for a complete data set."), {
                        
                        if(no_hyperparameters){
                          # Test that no hyperparameters are set.
                          testthat::expect_equal(is.null(new_object@hyperparameters), TRUE)
                          
                        } else if(!no_hyperparameters | always_available){
                          # Test that hyperparameters are set.
                          testthat::expect_equal(is.null(new_object@hyperparameters), FALSE)
                          
                          # Test that all hyperparameters are set.
                          testthat::expect_setequal(names(new_object@hyperparameters), names(get_default_hyperparameters(object)))
                          
                          if(!is_vimp){
                            if(!is.null(new_object@hyperparameter_data)){
                              # Test that sign_size hyperparameters make
                              # sense. 
                              testthat::expect_equal(all(new_object@hyperparameter_data$sign_size >= 2), TRUE)
                              testthat::expect_equal(all(new_object@hyperparameter_data$sign_size <= get_n_features(full_data)), TRUE)
                              
                              if(vimp_method == "signature_only"){
                                testthat::expect_equal(all(new_object@hyperparameter_data$sign_size == 2), TRUE)
                              }
                              
                              if(vimp_method == "none"){
                                testthat::expect_equal(all(new_object@hyperparameter_data$sign_size == get_n_features(full_data)), TRUE)
                              }
                            }
                          }
                        }
                      })
      
      if(verbose) message(paste0("\nComputing hyperparameters for ", current_method,
                                 ifelse(is_vimp, " variable importance method", " learner"), " and ",
                                 outcome_type, " outcomes for a data set with only identical entries."))
      
      # Optimise for data that are completely identical.
      new_object <- do.call(optimise_hyperparameters,
                            args=c(list("object"=object,
                                        "data"=identical_sample_data,
                                        "cl"=cl,
                                        "n_max_bootstraps"=n_max_bootstraps,
                                        "n_max_optimisation_steps"=n_max_optimisation_steps,
                                        "n_max_intensify_steps"=n_max_intensify_steps,
                                        "n_random_sets"=n_random_sets,
                                        "n_challengers"=n_challengers,
                                        "is_vimp"=is_vimp,
                                        "verbose"=verbose),
                                   dots))
      
      # Test that hyperparameters were set.
      test_fun(paste0("2. Hyperparameters for the ", current_method,
                      ifelse(is_vimp, " variable importance method", " learner"), " and ",
                      outcome_type, " outcomes can be created for a data set with only identical entries."), {
                        
                        if(no_hyperparameters | !always_available){
                          # Test that no hyperparameters are set. Models cannot
                          # train on completely invariant data.
                          testthat::expect_equal(is.null(new_object@hyperparameters), TRUE)
                          
                        } else if(always_available){
                          # Test that hyperparameters are set.
                          testthat::expect_equal(is.null(new_object@hyperparameters), FALSE)
                          
                          # Test that all hyperparameters are set.
                          testthat::expect_setequal(names(new_object@hyperparameters), names(get_default_hyperparameters(object)))
                          
                          if(!is_vimp){
                            if(!is.null(new_object@hyperparameter_data)){
                              # Test that sign_size hyperparameters make
                              # sense. 
                              testthat::expect_equal(all(new_object@hyperparameter_data$sign_size >= 2), TRUE)
                              testthat::expect_equal(all(new_object@hyperparameter_data$sign_size <= get_n_features(full_data)), TRUE)
                              
                              if(vimp_method == "signature_only"){
                                testthat::expect_equal(all(new_object@hyperparameter_data$sign_size == 2), TRUE)
                              }
                              
                              if(vimp_method == "none"){
                                testthat::expect_equal(all(new_object@hyperparameter_data$sign_size == get_n_features(full_data)), TRUE)
                              }
                            }
                          }
                        }
                      })
      
      if(verbose) message(paste0("\nComputing hyperparameters for ", current_method,
                                 ifelse(is_vimp, " variable importance method", " learner"), " and ",
                                 outcome_type, " outcomes for a data set with only one entry."))
      
      # Optimise for data that consist of only one sample.
      new_object <- do.call(optimise_hyperparameters,
                            args=c(list("object"=object,
                                        "data"=full_one_sample_data,
                                        "cl"=cl,
                                        "n_max_bootstraps"=n_max_bootstraps,
                                        "n_max_optimisation_steps"=n_max_optimisation_steps,
                                        "n_max_intensify_steps"=n_max_intensify_steps,
                                        "n_random_sets"=n_random_sets,
                                        "n_challengers"=n_challengers,
                                        "is_vimp"=is_vimp,
                                        "verbose"=verbose),
                                   dots))
      
      # Test.
      test_fun(paste0("3. Hyperparameters for the ", current_method,
                      ifelse(is_vimp, " variable importance method", " learner"), " and ",
                      outcome_type, " outcomes can be created for a data set with only one entry."), {
                        
                        if(no_hyperparameters){
                          # Test that no hyperparameters are set. Single entry
                          # data cannot be used to generate hyperparameter sets
                          # unless they are always available.
                          testthat::expect_equal(is.null(new_object@hyperparameters), TRUE)
                          
                        } else if(always_available){
                          # Test that hyperparameters are set.
                          testthat::expect_equal(is.null(new_object@hyperparameters), FALSE)
                          
                          # Test that all hyperparameters are set.
                          testthat::expect_setequal(names(new_object@hyperparameters), names(get_default_hyperparameters(object)))
                          
                          if(!is_vimp) {
                            if(!is.null(new_object@hyperparameter_data)){
                              # Test that sign_size hyperparameters make
                              # sense. 
                              testthat::expect_equal(all(new_object@hyperparameter_data$sign_size >= 2), TRUE)
                              testthat::expect_equal(all(new_object@hyperparameter_data$sign_size <= get_n_features(full_data)), TRUE)
                              
                              if(vimp_method == "signature_only"){
                                testthat::expect_equal(all(new_object@hyperparameter_data$sign_size == 2), TRUE)
                              }
                              
                              if(vimp_method == "none"){
                                testthat::expect_equal(all(new_object@hyperparameter_data$sign_size == get_n_features(full_data)), TRUE)
                              }
                            }
                          }
                          
                        } else {
                          # Not always available, but with hyperparameters. For
                          # some methods all hyperparameters can still be set,
                          # i.e. all typically randomised hyperparameters depend
                          # only on the number of features. Therefore, this is a
                          # softer check.
                          
                          if(!is.null(new_object@hyperparameters)){
                            # Test that all hyperparameters are set.
                            testthat::expect_setequal(names(new_object@hyperparameters), names(get_default_hyperparameters(object)))
                            
                            if(!is_vimp){
                              if(!is.null(new_object@hyperparameter_data)){
                                # Test that sign_size hyperparameters make
                                # sense. 
                                testthat::expect_equal(all(new_object@hyperparameter_data$sign_size == 2), TRUE)
                              }
                            }
                            
                          } else {
                            # Bogus test to prevent skipping.
                            testthat::expect_equal(is.null(new_object@hyperparameters), TRUE)
                          }
                        }
                      })
      
      if(verbose) message(paste0("\nComputing hyperparameters for ", current_method,
                                 ifelse(is_vimp, " variable importance method", " learner"), " and ",
                                 outcome_type, " outcomes for an empty data set."))
      
      # Optimise when data is missing.
      new_object <- do.call(optimise_hyperparameters,
                            args=c(list("object"=object,
                                        "data"=empty_data,
                                        "cl"=cl,
                                        "n_max_bootstraps"=n_max_bootstraps,
                                        "n_max_optimisation_steps"=n_max_optimisation_steps,
                                        "n_max_intensify_steps"=n_max_intensify_steps,
                                        "n_random_sets"=n_random_sets,
                                        "n_challengers"=n_challengers,
                                        "is_vimp"=is_vimp,
                                        "verbose"=verbose),
                                   dots))
      
      # Test.
      test_fun(paste0("4. Hyperparameters for the ", current_method,
                      ifelse(is_vimp, " variable importance method", " learner"), " and ",
                      outcome_type, " outcomes ",
                      ifelse(always_available, "can", "cannot"),
                      " be created for an empty data set."), {
                        
                        if(no_hyperparameters){
                          # Test that no hyperparameters are set. Empty datasets
                          # cannot be used to create hyperparameters.
                          testthat::expect_equal(is.null(new_object@hyperparameters), TRUE)
                          
                        } else if(always_available){
                          # Test that hyperparameters are set.
                          testthat::expect_equal(is.null(new_object@hyperparameters), FALSE)
                          
                          # Test that all hyperparameters are set.
                          testthat::expect_setequal(names(new_object@hyperparameters), names(get_default_hyperparameters(object)))
                          
                        } else {
                          # Not always available, but with hyperparameters. For
                          # some methods all hyperparameters can still be set,
                          # i.e. all typically randomised hyperparameters depend
                          # only on the number of features. Therefore, this is a
                          # softer check.
                          
                          if(!is.null(new_object@hyperparameters)){
                            # Test that all hyperparameters are set.
                            testthat::expect_setequal(names(new_object@hyperparameters), names(get_default_hyperparameters(object)))
                            
                            if(!is_vimp & !is.null(new_object@hyperparameter_data)){
                              # Test that sign_size hyperparameters make
                              # sense. 
                              testthat::expect_equal(all(new_object@hyperparameter_data$sign_size == 2), TRUE)
                            }
                            
                          } else {
                            # Bogus test to prevent skipping.
                            testthat::expect_equal(is.null(new_object@hyperparameters), TRUE)
                          }
                        }
                      })
      
      
      
      #####One-feature data set-------------------------------------------------
      # Create object
      object <- .test_create_hyperparameter_object(data=one_feature_data,
                                                   vimp_method=vimp_method,
                                                   learner=learner,
                                                   is_vimp=is_vimp,
                                                   set_signature_feature=FALSE)
      
      if(verbose) message(paste0("\nComputing hyperparameters for ", current_method,
                                 ifelse(is_vimp, " variable importance method", " learner"), " and ",
                                 outcome_type, " outcomes for a data set with only one feature."))
      
      # Optimise parameters for a dataset with only one feature.
      new_object <- do.call(optimise_hyperparameters,
                            args=c(list("object"=object,
                                        "data"=one_feature_data,
                                        "cl"=cl,
                                        "n_max_bootstraps"=n_max_bootstraps,
                                        "n_max_optimisation_steps"=n_max_optimisation_steps,
                                        "n_max_intensify_steps"=n_max_intensify_steps,
                                        "n_random_sets"=n_random_sets,
                                        "n_challengers"=n_challengers,
                                        "is_vimp"=is_vimp,
                                        "verbose"=verbose),
                                   dots))
      
      test_fun(paste0("5. Hyperparameters for the ", current_method,
                      ifelse(is_vimp, " variable importance method", " learner"), " and ",
                      outcome_type, " outcomes can be created for a data set with only one feature."), {
                        
                        if(no_hyperparameters){
                          # Test that no hyperparameters are set.
                          testthat::expect_equal(is.null(new_object@hyperparameters), TRUE)
                          
                        } else if(!no_hyperparameters | always_available){
                          # Test that hyperparameters are set.
                          testthat::expect_equal(is.null(new_object@hyperparameters), FALSE)
                          
                          # Test that all hyperparameters are set.
                          testthat::expect_setequal(names(new_object@hyperparameters), names(get_default_hyperparameters(object)))
                          
                          if(!is_vimp){
                            if(!is.null(new_object@hyperparameter_data)){
                              # Test that sign_size hyperparameters make
                              # sense. 
                              testthat::expect_equal(all(new_object@hyperparameter_data$sign_size == 1), TRUE)
                            }
                          }
                        }
                      })
      
      if(verbose) message(paste0("\nComputing hyperparameters for ", current_method,
                                 ifelse(is_vimp, " variable importance method", " learner"), " and ",
                                 outcome_type, " outcomes for a data set with only one feature and sample."))
      
      # Optimise parameters for a dataset with only one feature and sample.
      new_object <- do.call(optimise_hyperparameters,
                            args=c(list("object"=object,
                                        "data"=one_feature_one_sample_data,
                                        "cl"=cl,
                                        "n_max_bootstraps"=n_max_bootstraps,
                                        "n_max_optimisation_steps"=n_max_optimisation_steps,
                                        "n_max_intensify_steps"=n_max_intensify_steps,
                                        "n_random_sets"=n_random_sets,
                                        "n_challengers"=n_challengers,
                                        "is_vimp"=is_vimp,
                                        "verbose"=verbose),
                                   dots))
      
      test_fun(paste0("6. Hyperparameters for the ", current_method,
                      ifelse(is_vimp, " variable importance method", " learner"), " and ",
                      outcome_type, " outcomes can be created for a data set with only one feature and sample."), {
                        
                        if(no_hyperparameters){
                          # Test that no hyperparameters are set.
                          # Hyperparameters cannot be set for datasets with only
                          # a single sample.
                          testthat::expect_equal(is.null(new_object@hyperparameters), TRUE)
                          
                        } else if(always_available){
                          # Test that hyperparameters are set.
                          testthat::expect_equal(is.null(new_object@hyperparameters), FALSE)
                          
                          # Test that all hyperparameters are set.
                          testthat::expect_setequal(names(new_object@hyperparameters), names(get_default_hyperparameters(object)))
                          
                          if(!is_vimp){
                            if(!is.null(new_object@hyperparameter_data)){
                              # Test that sign_size hyperparameters make
                              # sense. 
                              testthat::expect_equal(all(new_object@hyperparameter_data$sign_size == 1), TRUE)
                            }
                          }
                          
                        } else {
                          # Not always available, but with hyperparameters. For
                          # some methods all hyperparameters can still be set,
                          # i.e. all typically randomised hyperparameters depend
                          # only on the number of features. Therefore, this is a
                          # softer check.
                          
                          if(!is.null(new_object@hyperparameters)){
                            # Test that all hyperparameters are set.
                            testthat::expect_setequal(names(new_object@hyperparameters), names(get_default_hyperparameters(object)))
                            
                            if(!is_vimp){
                              if(!is.null(new_object@hyperparameter_data)){
                                # Test that sign_size hyperparameters make
                                # sense. 
                                testthat::expect_equal(all(new_object@hyperparameter_data$sign_size == 1), TRUE)
                              }
                            }
                            
                          } else {
                            # Bogus test to prevent skipping.
                            testthat::expect_equal(is.null(new_object@hyperparameters), TRUE)
                          }
                        } 
                      })
      
      if(verbose) message(paste0("\nComputing hyperparameters for ", current_method,
                                 ifelse(is_vimp, " variable importance method", " learner"), " and ",
                                 outcome_type, " outcomes for a data set with only one, invariant feature."))
      
      # Optimise parameters for a dataset with only one, invariant feature.
      new_object <- do.call(optimise_hyperparameters,
                            args=c(list("object"=object,
                                        "data"=one_feature_invariant_data,
                                        "cl"=cl,
                                        "n_max_bootstraps"=n_max_bootstraps,
                                        "n_max_optimisation_steps"=n_max_optimisation_steps,
                                        "n_max_intensify_steps"=n_max_intensify_steps,
                                        "n_random_sets"=n_random_sets,
                                        "n_challengers"=n_challengers,
                                        "is_vimp"=is_vimp,
                                        "verbose"=verbose),
                                   dots))
      
      test_fun(paste0("7. Hyperparameters for the ", current_method,
                      ifelse(is_vimp, " variable importance method", " learner"), " and ",
                      outcome_type, " outcomes can be created for a data set with only one, invariant feature."), {
                        
                        if(no_hyperparameters){
                          # Test that no hyperparameters are set.
                          # Hyperparameters cannot be set for datasets with
                          # invariant features.
                          testthat::expect_equal(is.null(new_object@hyperparameters), TRUE)
                          
                        } else if(always_available){
                          # Test that hyperparameters are set.
                          testthat::expect_equal(is.null(new_object@hyperparameters), FALSE)
                          
                          # Test that all hyperparameters are set.
                          testthat::expect_setequal(names(new_object@hyperparameters), names(get_default_hyperparameters(object)))
                          
                          if(!is_vimp){
                            if(!is.null(new_object@hyperparameter_data)){
                              # Test that sign_size hyperparameters make
                              # sense. 
                              testthat::expect_equal(all(new_object@hyperparameter_data$sign_size == 1), TRUE)
                            }
                          }
                          
                        } else {
                          # Not always available, but with hyperparameters. For
                          # some methods all hyperparameters can still be set,
                          # i.e. all typically randomised hyperparameters depend
                          # only on the number of features. Therefore, this is a
                          # softer check.
                          
                          if(!is.null(new_object@hyperparameters)){
                            # Test that all hyperparameters are set.
                            testthat::expect_setequal(names(new_object@hyperparameters), names(get_default_hyperparameters(object)))
                            
                            if(!is_vimp){
                              if(!is.null(new_object@hyperparameter_data)){
                                # Test that sign_size hyperparameters make
                                # sense. 
                                testthat::expect_equal(all(new_object@hyperparameter_data$sign_size == 1), TRUE)
                              }
                            }                            
                          } else {
                            # Bogus test to prevent skipping.
                            testthat::expect_equal(is.null(new_object@hyperparameters), TRUE)
                          }
                        }
                      })
    }   
  }
}


test_plots <- function(plot_function,
                       data_element,
                       outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                       always_available=FALSE,
                       except_one_feature=FALSE,
                       except_failed_survival_prediction=TRUE,
                       except_prospective=FALSE,
                       ...,
                       plot_args=list(),
                       test_specific_config=FALSE,
                       create_novelty_detector=FALSE,
                       debug=FALSE,
                       parallel=waiver()){
  
  if(debug){
    test_fun <- debug_test_that
    plot_args$draw <- TRUE
    
  } else {
    test_fun <- testthat::test_that
  }
  
  # Set parallelisation.
  if(is.waive(parallel)) parallel <- !debug
  
  if(parallel){
    # Set options.
    # Disable randomForestSRC OpenMP core use.
    options(rf.cores=as.integer(1))
    on.exit(options(rf.cores=-1L), add=TRUE)
    
    # Disable multithreading on data.table to prevent reduced performance due to
    # resource collisions with familiar parallelisation.
    data.table::setDTthreads(1L)
    on.exit(data.table::setDTthreads(0L), add=TRUE)
    
    # Start local cluster in the overall process.
    cl <- .test_start_cluster(n_cores=4L)
    on.exit(.terminate_cluster(cl), add=TRUE)
    
  } else {
    cl <- NULL
  }
  
  # Iterate over the outcome type.
  for(outcome_type in c("survival", "binomial", "multinomial", "count", "continuous")){
  
    # Obtain data.
    full_data <- test.create_good_data_set(outcome_type)
    identical_sample_data <- test.create_all_identical_data_set(outcome_type)
    full_one_sample_data <- test.create_one_sample_data_set(outcome_type)
    one_feature_data <- test.create_one_feature_data_set(outcome_type)
    one_feature_one_sample_data <- test.create_one_feature_one_sample_data_set(outcome_type)
    one_feature_invariant_data <- test.create_one_feature_invariant_data_set(outcome_type)
    empty_data <- test.create_empty_data_set(outcome_type)
    multi_data <- test_create_multiple_synthetic_series(outcome_type=outcome_type)
    no_censoring_data <- test.create_good_data_no_censoring_set(outcome_type)
    one_censored_data <- test.create_good_data_one_censored_set(outcome_type)
    few_censored_data <- test.create_good_data_few_censored_set(outcome_type)
    prospective_data <- test.create_prospective_data_set(outcome_type)
    
    # Set exceptions per outcome type.
    .always_available <- always_available
    if(is.character(.always_available)) .always_available <- any(.always_available == outcome_type)
    
    .except_one_feature <- except_one_feature
    if(is.character(.except_one_feature)) .except_one_feature <- any(.except_one_feature == outcome_type)
    
    .except_failed_survival_prediction <- except_failed_survival_prediction
    if(is.character(.except_failed_survival_prediction)) .except_failed_survival_prediction <- any(.except_failed_survival_prediction == outcome_type)
    
    .except_prospective <- except_prospective
    if(is.character(.except_prospective)) .except_prospective <- any(.except_prospective == outcome_type)
    
    if(.always_available){
      .except_one_feature <- .except_prospective <- .except_failed_survival_prediction <- FALSE
    }
    
    # Parse hyperparameter list
    hyperparameters <- list("sign_size"=get_n_features(full_data),
                            "family"=switch(outcome_type,
                                            "continuous"="gaussian",
                                            "count"="poisson",
                                            "binomial"="binomial",
                                            "multinomial"="multinomial",
                                            "survival"="cox"))
    
    
    #####Full data set########################################################

    # Train the model.
    model_full_1 <- suppressWarnings(train(cl=cl,
                                           data=full_data,
                                           cluster_method="none",
                                           imputation_method="simple",
                                           fs_method="mim",
                                           hyperparameter_list=hyperparameters,
                                           learner="lasso",
                                           time_max=1832,
                                           create_novelty_detector=create_novelty_detector))

    model_full_2 <- model_full_1
    model_full_2@fs_method <- "mifs"

    # Create familiar data objects.
    data_good_full_1 <- as_familiar_data(object=model_full_1,
                                         data=full_data,
                                         data_element=data_element,
                                         cl=cl,
                                         ...)
    data_good_full_2 <- as_familiar_data(object=model_full_2,
                                         data=full_data,
                                         data_element=data_element,
                                         cl=cl,
                                         ...)

    # Create a completely intact dataset.
    test_fun(paste0("1. Plots for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available, "can", "cannot"),
                    " be created for a complete data set."), {

                      object <- list(data_good_full_1, data_good_full_2, data_good_full_1, data_good_full_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))

                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))

                      plot_list <- do.call(plot_function, args=c(list("object"=collection), plot_args))
                      which_present <- .test_which_plot_present(plot_list)

                      if(outcome_type %in% outcome_type_available){
                        testthat::expect_equal(all(which_present), TRUE)

                      } else {
                        testthat::expect_equal(all(!which_present), TRUE)
                      }
                    })
    
    # Go to next outcome type if only a specific configuration needs to be
    # tested.
    if(test_specific_config) next()
    
    # Create familiar data objects.
    data_prospective_full_1 <- as_familiar_data(object=model_full_1,
                                                data=prospective_data,
                                                data_element=data_element,
                                                cl=cl,
                                                ...)
    
    # Create a completely intact dataset.
    test_fun(paste0("2. Plots for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available & !.except_prospective, "can", "cannot"),
                    " be created for a prospective data set without known outcome."), {
                      
                      object <- list(data_prospective_full_1)
                      object <- mapply(set_object_name, object, c("prospective"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("prospective")))
                      
                      plot_list <- do.call(plot_function, args=c(list("object"=collection), plot_args))
                      which_present <- .test_which_plot_present(plot_list)
                      
                      if(outcome_type %in% outcome_type_available & !.except_prospective){
                        testthat::expect_equal(all(which_present), TRUE) 
                        
                      } else if(!outcome_type %in% outcome_type_available){
                        testthat::expect_equal(all(!which_present), TRUE)
                        
                      } else {
                        testthat::expect_equal(any(!which_present), TRUE)
                      }
                    })
    
    
    # Ensemble from multiple datasets.
    multi_model_set <- suppressWarnings(lapply(multi_data,
                                               train,
                                               cluster_method="hclust",
                                               imputation_method="simple",
                                               fs_method="mim",
                                               hyperparameter_list=hyperparameters,
                                               learner="lasso",
                                               cluster_similarity_threshold=0.7,
                                               time_max=60,
                                               create_novelty_detector=create_novelty_detector))
    
    # Create data from ensemble of multiple models
    multi_model_full <- as_familiar_data(object=multi_model_set,
                                         data=multi_data[[1]],
                                         data_element=data_element,
                                         cl=cl,
                                         ...)
    
    # Create additional familiar data objects.
    data_empty_full_1 <- as_familiar_data(object=model_full_1,
                                          data=empty_data,
                                          data_element=data_element,
                                          cl=cl,
                                          ...)
    data_empty_full_2 <- as_familiar_data(object=model_full_2,
                                          data=empty_data,
                                          data_element=data_element,
                                          cl=cl,
                                          ...)
    data_one_sample_full_1 <- as_familiar_data(object=model_full_1,
                                               data=full_one_sample_data,
                                               data_element=data_element,
                                               cl=cl,
                                               ...)
    data_one_sample_full_2 <- as_familiar_data(object=model_full_2,
                                               data=full_one_sample_data,
                                               data_element=data_element,
                                               cl=cl,
                                               ...)
    data_identical_full_1 <- as_familiar_data(object=model_full_1,
                                              data=identical_sample_data,
                                              data_element=data_element,
                                              cl=cl,
                                              ...)
    data_identical_full_2 <- as_familiar_data(object=model_full_2,
                                              data=identical_sample_data,
                                              data_element=data_element,
                                              cl=cl,
                                              ...)
    
    # Create a dataset with a missing quadrant.
    test_fun(paste0("3. Plots for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available, "can", "cannot"),
                    " be created for a dataset with some missing data."), {
                      
                      object <- list(data_good_full_1, data_good_full_2, data_empty_full_1, data_good_full_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
                      
                      plot_list <- do.call(plot_function, args=c(list("object"=collection), plot_args))
                      which_present <- .test_which_plot_present(plot_list)
                      
                      if(outcome_type %in% outcome_type_available){
                        testthat::expect_equal(any(which_present), TRUE) 
                        
                      } else {
                        testthat::expect_equal(all(!which_present), TRUE)
                      }
                    })
    
    # Create a dataset with all missing quadrants
    test_fun(paste0("4. Plots for ", outcome_type, " outcomes ",
                    ifelse(.always_available, "can", "cannot"),
                    " be created for a dataset with completely missing data."), {
                      
                      object <- list(data_empty_full_1, data_empty_full_2, data_empty_full_1, data_empty_full_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
                      
                      plot_list <- do.call(plot_function, args=c(list("object"=collection), plot_args))
                      which_present <- .test_which_plot_present(plot_list)
                      
                      if(outcome_type %in% outcome_type_available & .always_available){
                        testthat::expect_equal(all(which_present), TRUE) 
                        
                      } else {
                        testthat::expect_equal(all(!which_present), TRUE)
                      }
                    })
    
    # Create dataset with one-sample quadrants for validation
    test_fun(paste0("5. Plots for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available, "can", "cannot"),
                    " be created for a dataset where some data only have one sample."), {
                      
                      object <- list(data_good_full_1, data_good_full_2, data_one_sample_full_1, data_one_sample_full_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
                      
                      plot_list <- do.call(plot_function, args=c(list("object"=collection), plot_args))
                      which_present <- .test_which_plot_present(plot_list)
                      
                      if(outcome_type %in% outcome_type_available){
                        testthat::expect_equal(any(which_present), TRUE) 
                        
                      } else {
                        testthat::expect_equal(all(!which_present), TRUE)
                      }
                    })
    
    # Create dataset with some quadrants with identical data
    test_fun(paste0("6. Plots for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available, "can", "cannot"),
                    " be created for a dataset where some data only have identical samples."), {
                      
                      object <- list(data_good_full_1, data_good_full_2, data_identical_full_1, data_identical_full_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
                      
                      plot_list <- do.call(plot_function, args=c(list("object"=collection), plot_args))
                      which_present <- .test_which_plot_present(plot_list)
                      
                      if(outcome_type %in% outcome_type_available){
                        testthat::expect_equal(all(which_present), TRUE) 
                        
                      } else {
                        testthat::expect_equal(all(!which_present), TRUE)
                      }
                    })
    
    test_fun(paste0("7. Plots for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available, "can", "cannot"),
                    " be created for a dataset created from an ensemble of multiple models."), {
                      
                      object <- list(multi_model_full)
                      object <- mapply(set_object_name, object, c("development_1"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development")))
                      
                      plot_list <- do.call(plot_function, args=c(list("object"=collection), plot_args))
                      which_present <- .test_which_plot_present(plot_list)
                      
                      if(outcome_type %in% outcome_type_available){
                        testthat::expect_equal(all(which_present), TRUE) 
                        
                      } else {
                        testthat::expect_equal(all(!which_present), TRUE)
                      }
                    })
    
    #####One-feature data set###################################################
    
    # Train the model.
    model_one_1 <- suppressWarnings(train(data=one_feature_data,
                                          cluster_method="none",
                                          imputation_method="simple",
                                          fs_method="mim",
                                          hyperparameter_list=hyperparameters,
                                          learner="lasso",
                                          time_max=1832,
                                          create_novelty_detector=create_novelty_detector))
    
    model_one_2 <- model_one_1
    model_one_2@fs_method <- "mifs"
    
    # Create familiar data objects.
    data_good_one_1 <- as_familiar_data(object=model_one_1, data=one_feature_data, data_element=data_element, cl=cl, ...)
    data_good_one_2 <- as_familiar_data(object=model_one_2, data=one_feature_data, data_element=data_element, cl=cl, ...)
    data_one_sample_one_1 <- as_familiar_data(object=model_one_1, data=one_feature_one_sample_data, data_element=data_element, cl=cl, ...)
    data_one_sample_one_2 <- as_familiar_data(object=model_one_2, data=one_feature_one_sample_data, data_element=data_element, cl=cl, ...)
    data_identical_one_1 <- as_familiar_data(object=model_one_1, data=one_feature_invariant_data, data_element=data_element, cl=cl, ...)
    data_identical_one_2 <- as_familiar_data(object=model_one_2, data=one_feature_invariant_data, data_element=data_element, cl=cl, ...)
    
    
    # Create a completely intact, one sample dataset.
    test_fun(paste0("8. Plots for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available && !.except_one_feature, "can", "cannot"),
                    " be created for a complete one-feature data set."), {
                      
                      object <- list(data_good_one_1, data_good_one_2, data_good_one_1, data_good_one_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
                      
                      plot_list <- do.call(plot_function, args=c(list("object"=collection), plot_args))
                      which_present <- .test_which_plot_present(plot_list)
                      
                      if(outcome_type %in% outcome_type_available & !.except_one_feature){
                        testthat::expect_equal(all(which_present), TRUE) 
                        
                      } else if(!outcome_type %in% outcome_type_available){
                        testthat::expect_equal(all(!which_present), TRUE)
                        
                      } else {
                        testthat::expect_equal(any(!which_present), TRUE)
                      }
                    })
    
    # Create a dataset with a one-sample quadrant.
    test_fun(paste0("9. Plots for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available && !.except_one_feature, "can", "cannot"),
                    " be created for a dataset with some one-sample data."), {
                      
                      object <- list(data_good_one_1, data_good_one_2, data_one_sample_one_1, data_one_sample_one_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
                      
                      plot_list <- do.call(plot_function, args=c(list("object"=collection), plot_args))
                      which_present <- .test_which_plot_present(plot_list)
                      
                      if(outcome_type %in% outcome_type_available & !.except_one_feature){
                        testthat::expect_equal(any(which_present), TRUE) 
                        
                      } else if(!outcome_type %in% outcome_type_available){
                        testthat::expect_equal(all(!which_present), TRUE)
                        
                      } else {
                        testthat::expect_equal(any(!which_present), TRUE)
                      }
                    })
    
    # Create a dataset with some identical data.
    test_fun(paste0("10. Plots for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available && !.except_one_feature, "can", "cannot"),
                    " be created for a dataset with some invariant data."), {
                      
                      object <- list(data_good_one_1, data_good_one_2, data_identical_one_1, data_identical_one_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
                      
                      plot_list <- suppressWarnings(do.call(plot_function, args=c(list("object"=collection), plot_args)))
                      which_present <- .test_which_plot_present(plot_list)
                      
                      if(outcome_type %in% outcome_type_available & !.except_one_feature){
                        testthat::expect_equal(any(which_present), TRUE) 
                        
                      } else if(!outcome_type %in% outcome_type_available){
                        testthat::expect_equal(all(!which_present), TRUE)
                        
                      } else {
                        testthat::expect_equal(any(!which_present), TRUE)
                      }
                    })
    
    #####Data set with limited censoring########################################
    if(outcome_type %in% c("survival", "competing_risk")){
      # Train the model.
      model_cens_1 <- suppressWarnings(train(cl=cl,
                                             data=no_censoring_data,
                                             cluster_method="none",
                                             imputation_method="simple",
                                             fs_method="mim",
                                             hyperparameter_list=hyperparameters,
                                             learner="lasso",
                                             time_max=1832,
                                             create_novelty_detector=create_novelty_detector))
      
      model_cens_2 <- suppressWarnings(train(cl=cl,
                                             data=one_censored_data,
                                             cluster_method="none",
                                             imputation_method="simple",
                                             fs_method="mim",
                                             hyperparameter_list=hyperparameters,
                                             learner="lasso",
                                             time_max=1832,
                                             create_novelty_detector=create_novelty_detector))
      
      model_cens_3 <- suppressWarnings(train(cl=cl,
                                             data=few_censored_data,
                                             cluster_method="none",
                                             imputation_method="simple",
                                             fs_method="mim",
                                             hyperparameter_list=hyperparameters,
                                             learner="lasso",
                                             time_max=1832,
                                             create_novelty_detector=create_novelty_detector))
      
      data_cens_1 <- as_familiar_data(object=model_cens_1, data=no_censoring_data, data_element=data_element, cl=cl, ...)
      data_cens_2 <- as_familiar_data(object=model_cens_2, data=one_censored_data, data_element=data_element, cl=cl, ...)
      data_cens_3 <- as_familiar_data(object=model_cens_3, data=few_censored_data, data_element=data_element, cl=cl, ...)
      
      # Create a dataset with some identical data.
      test_fun(paste0("11. Plots for ", outcome_type, " outcomes ",
                      ifelse(outcome_type %in% outcome_type_available, "can", "cannot"),
                      " be created for a data set that includes no or limited censoring."), {
                        
                        object <- list(data_cens_1, data_cens_2, data_cens_3)
                        object <- mapply(set_object_name, object, c("no_censoring", "one_censored", "few_censored"))
                        
                        collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("no_censoring", "one_censored", "few_censored")))
                        
                        plot_list <- do.call(plot_function, args=c(list("object"=collection), plot_args))
                        which_present <- .test_which_plot_present(plot_list)
                        
                        if(outcome_type %in% outcome_type_available){
                          testthat::expect_equal(all(which_present), TRUE)
                          
                        } else {
                          testthat::expect_equal(all(!which_present), TRUE)
                        }
                      })
    }
    
    ##### Model with missing survival predictions ##############################
    if(outcome_type %in% c("survival", "competing_risk")){
      # Train the model.
      model_failed_predictions <- suppressWarnings(train(cl=cl,
                                                         data=full_data,
                                                         cluster_method="none",
                                                         imputation_method="simple",
                                                         fs_method="mim",
                                                         hyperparameter_list=hyperparameters,
                                                         learner="lasso_test",
                                                         time_max=1832,
                                                         create_novelty_detector=create_novelty_detector))
      
      failed_prediction_data <- as_familiar_data(object=model_failed_predictions, data=full_data, data_element=data_element, cl=cl, ...)
      
      test_fun(paste0("12. Plots for ", outcome_type, " outcomes ",
                      ifelse(outcome_type %in% outcome_type_available && .except_failed_survival_prediction, "can", "cannot"),
                      " be created for models that do not allow for predicting survival probabilitiies."), {
                        
                        collection <- suppressWarnings(as_familiar_collection(failed_prediction_data, familiar_data_names=c("no_survival_predictions")))
                        
                        plot_list <- do.call(plot_function, args=c(list("object"=collection), plot_args))
                        which_present <- .test_which_plot_present(plot_list)
                        
                        if(outcome_type %in% outcome_type_available & !.except_failed_survival_prediction){
                          testthat::expect_equal(all(which_present), TRUE) 
                          
                        } else if(!outcome_type %in% outcome_type_available){
                          testthat::expect_equal(all(!which_present), TRUE)
                          
                        } else {
                          testthat::expect_equal(any(!which_present), TRUE)
                        }
                      })
    }
  }
}



test_plot_ordering <- function(plot_function,
                               data_element,
                               outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                               ...,
                               plot_args=list(),
                               create_novelty_detector=FALSE,
                               debug=FALSE,
                               parallel=waiver()){
  
  # Set debug options.
  if(debug){
    test_fun <- debug_test_that
    plot_args$draw <- TRUE
    
  } else {
    test_fun <- testthat::test_that
  }
  
  # Set parallelisation.
  if(is.waive(parallel)) parallel <- !debug
  
  if(parallel){
    # Set options.
    # Disable randomForestSRC OpenMP core use.
    options(rf.cores=as.integer(1))
    on.exit(options(rf.cores=-1L), add=TRUE)
    
    # Disable multithreading on data.table to prevent reduced performance due to
    # resource collisions with familiar parallelisation.
    data.table::setDTthreads(1L)
    on.exit(data.table::setDTthreads(0L), add=TRUE)
    
    # Start local cluster in the overall process.
    cl <- .test_start_cluster(n_cores=4L)
    on.exit(.terminate_cluster(cl), add=TRUE)
    
  } else {
    cl <- NULL
  }
  
  # Iterate over the outcome type.
  for(outcome_type in outcome_type_available){
    
    # Obtain data.
    full_data <- test.create_good_data_set(outcome_type)
    empty_data <- test.create_empty_data_set(outcome_type)
    
    ##### Train the lasso model ################################################
    # Parse hyperparameter list
    hyperparameters_lasso <- list("sign_size"=get_n_features(full_data),
                                  "family"=switch(outcome_type,
                                                  "continuous"="gaussian",
                                                  "count"="poisson",
                                                  "binomial"="binomial",
                                                  "multinomial"="multinomial",
                                                  "survival"="cox"))
    
    # Train the lasso model.
    model_full_lasso_1 <- suppressWarnings(train(data=full_data,
                                                 cluster_method="none",
                                                 imputation_method="simple",
                                                 fs_method="mim",
                                                 hyperparameter_list=hyperparameters_lasso,
                                                 learner="lasso",
                                                 time_max=1832,
                                                 create_novelty_detector=create_novelty_detector))
    
    model_full_lasso_2 <- model_full_lasso_1
    model_full_lasso_2@fs_method <- "mifs"
    
    ##### Train the glm model ##################################################
    # Parse hyperparameter list
    hyperparameters_glm <- list("sign_size"=get_n_features(full_data),
                                "family"=switch(outcome_type,
                                                "continuous"="gaussian",
                                                "count"="poisson",
                                                "binomial"="logistic",
                                                "multinomial"="multinomial",
                                                "survival"="cox"))
    
    # Train the model.
    model_full_glm_1 <- suppressWarnings(train(data=full_data,
                                               cluster_method="none",
                                               imputation_method="simple",
                                               fs_method="mim",
                                               hyperparameter_list=hyperparameters_glm,
                                               learner="glm",
                                               time_max=1832,
                                               create_novelty_detector=create_novelty_detector))
    
    model_full_glm_2 <- model_full_glm_1
    model_full_glm_2@fs_method <- "mifs"
    
    ##### Create the plot ######################################################
    
    # Create familiar data objects.
    data_good_full_lasso_1 <- as_familiar_data(object=model_full_lasso_1, data=full_data, data_element=data_element, cl=cl, ...)
    data_good_full_lasso_2 <- as_familiar_data(object=model_full_lasso_2, data=full_data, data_element=data_element, cl=cl, ...)
    data_good_full_glm_1 <- as_familiar_data(object=model_full_glm_1, data=full_data, data_element=data_element, cl=cl, ...)
    data_good_full_glm_2 <- as_familiar_data(object=model_full_glm_2, data=full_data, data_element=data_element, cl=cl, ...)
    data_empty_glm_1 <- as_familiar_data(object=model_full_glm_1, data=empty_data, data_element=data_element, cl=cl, ...)
    data_empty_lasso_2 <- as_familiar_data(object=model_full_lasso_2, data=empty_data, data_element=data_element, cl=cl, ...)
    
    # Create a test dataset with multiple components
    test_fun(paste0("Plots for ", outcome_type, " outcomes can be created."), {
                      
                      object <- list(data_good_full_lasso_1, data_empty_lasso_2, data_good_full_lasso_1, data_good_full_lasso_2,
                                     data_good_full_glm_1, data_good_full_glm_2, data_empty_glm_1, data_good_full_glm_2)
                      object <- mapply(set_object_name, object, c("development_lasso_1", "development_lasso_2", "validation_lasso_1", "validation_lasso_2",
                                                                  "development_glm_1", "development_glm_2", "validation_glm_1", "validation_glm_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation",
                                                                                                          "development", "development", "validation", "validation")))
                      
                      plot_list <- do.call(plot_function, args=c(list("object"=collection), plot_args))
                      which_present <- .test_which_plot_present(plot_list)
                      
                      if(outcome_type %in% outcome_type_available){
                        testthat::expect_equal(all(which_present), TRUE) 
                        
                      } else {
                        testthat::expect_equal(all(!which_present), TRUE)
                      }
                    })
  }
}



test_export <- function(export_function,
                        data_element,
                        outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                        always_available=FALSE,
                        except_one_feature=FALSE,
                        except_prospective=FALSE,
                        except_failed_survival_prediction=TRUE,
                        ...,
                        export_args=list(),
                        test_specific_config=FALSE,
                        n_models=1L,
                        create_novelty_detector=FALSE,
                        debug=FALSE,
                        parallel=waiver()){
  
  if(debug){
    test_fun <- debug_test_that

  } else {
    test_fun <- testthat::test_that
  }
  
  # Set parallelisation.
  if(is.waive(parallel)) parallel <- !debug
  
  if(parallel){
    # Set options.
    # Disable randomForestSRC OpenMP core use.
    options(rf.cores=as.integer(1))
    on.exit(options(rf.cores=-1L), add=TRUE)
    
    # Disable multithreading on data.table to prevent reduced performance due to
    # resource collisions with familiar parallelisation.
    data.table::setDTthreads(1L)
    on.exit(data.table::setDTthreads(0L), add=TRUE)
    
    # Start local cluster in the overall process.
    cl <- .test_start_cluster(n_cores=4L)
    on.exit(.terminate_cluster(cl), add=TRUE)
    
  } else {
    cl <- NULL
  }
  
  # Iterate over the outcome type.
  for(outcome_type in c("count", "continuous", "binomial", "multinomial", "survival")){
    
    # Obtain data.
    full_data <- test.create_good_data_set(outcome_type)
    identical_sample_data <- test.create_all_identical_data_set(outcome_type)
    full_one_sample_data <- test.create_one_sample_data_set(outcome_type)
    one_feature_data <- test.create_one_feature_data_set(outcome_type)
    one_feature_one_sample_data <- test.create_one_feature_one_sample_data_set(outcome_type)
    one_feature_invariant_data <- test.create_one_feature_invariant_data_set(outcome_type)
    empty_data <- test.create_empty_data_set(outcome_type)
    multi_data <- test_create_multiple_synthetic_series(outcome_type=outcome_type)
    no_censoring_data <- test.create_good_data_no_censoring_set(outcome_type)
    one_censored_data <- test.create_good_data_one_censored_set(outcome_type)
    few_censored_data <- test.create_good_data_few_censored_set(outcome_type)
    prospective_data <- test.create_prospective_data_set(outcome_type)
    
    # Set exceptions per outcome type.
    .always_available <- always_available
    if(is.character(.always_available)) .always_available <- any(.always_available == outcome_type)
    
    .except_one_feature <- except_one_feature
    if(is.character(.except_one_feature)) .except_one_feature <- any(.except_one_feature == outcome_type)
    
    .except_prospective <- except_prospective
    if(is.character(.except_prospective)) .except_prospective <- any(.except_prospective == outcome_type)
    
    .except_failed_survival_prediction <- except_failed_survival_prediction
    if(is.character(.except_failed_survival_prediction)) .except_failed_survival_prediction <- any(.except_failed_survival_prediction == outcome_type)
    
    if(.always_available){
      .except_one_feature <- .except_prospective <- .except_failed_survival_prediction <- FALSE
    }
    
    # Parse hyperparameter list
    hyperparameters <- list("sign_size"=get_n_features(full_data),
                            "family"=switch(outcome_type,
                                            "continuous"="gaussian",
                                            "count"="poisson",
                                            "binomial"="binomial",
                                            "multinomial"="multinomial",
                                            "survival"="cox"))
    
    #####Full data set########################################################
    
    if(n_models == 1){
      # Train the model.
      model_full_1 <- suppressWarnings(train(cl=cl,
                                             data=full_data,
                                             cluster_method="none",
                                             imputation_method="simple",
                                             fs_method="mim",
                                             hyperparameter_list=hyperparameters,
                                             learner="lasso",
                                             time_max=1832,
                                             create_novelty_detector=create_novelty_detector))
      
      model_full_2 <- model_full_1
      model_full_2@fs_method <- "mifs"
      
    } else {
      # Train a set of models.
      model_full_1 <- list()
      model_full_2 <- list()
      
      for(ii in seq_len(n_models)){
        temp_model_1 <- suppressWarnings(train(cl=cl,
                                               data=full_data,
                                               cluster_method="none",
                                               imputation_method="simple",
                                               fs_method="mim",
                                               hyperparameter_list=hyperparameters,
                                               learner="lasso",
                                               time_max=1832,
                                               create_bootstrap=TRUE,
                                               create_novelty_detector=create_novelty_detector))
        
        temp_model_2 <- temp_model_1
        temp_model_2@fs_method <- "mifs"
        
        model_full_1[[ii]] <- temp_model_1
        model_full_2[[ii]] <- temp_model_2
      }
    }
    
    
    # Create familiar data objects.
    data_good_full_1 <- as_familiar_data(object=model_full_1,
                                         data=full_data,
                                         data_element=data_element,
                                         cl=cl,
                                         ...)
    
    data_good_full_2 <- as_familiar_data(object=model_full_2,
                                         data=full_data,
                                         data_element=data_element,
                                         cl=cl,
                                         ...)
    
    # Create a completely intact dataset.
    test_fun(paste0("1. Export data for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available, "can", "cannot"),
                    " be created for a complete data set."), {
                      
                      object <- list(data_good_full_1, data_good_full_2, data_good_full_1, data_good_full_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
                      
                      data_elements <- do.call(export_function, args=c(list("object"=collection), export_args))
                      which_present <- .test_which_data_element_present(data_elements, outcome_type=outcome_type)

                      if(outcome_type %in% outcome_type_available){
                        testthat::expect_equal(all(which_present), TRUE) 
                        
                        if(debug) show(data_elements)
                        
                      } else {
                        testthat::expect_equal(all(!which_present), TRUE)
                      }
                    })
    
    # Go to next outcome type if only a specific configuration needs to be
    # tested.
    if(test_specific_config) next()
    
    data_prospective_full_1 <- as_familiar_data(object=model_full_1,
                                                data=prospective_data,
                                                data_element=data_element,
                                                cl=cl,
                                                ...)
    
    # Test prospective data set..
    test_fun(paste0("2. Export data for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available & !.except_prospective, "can", "cannot"),
                    " be created for a prospective data set without known outcome."), {
                      
                      object <- list(data_prospective_full_1)
                      object <- mapply(set_object_name, object, c("prospective"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("prospective")))
                      
                      data_elements <- do.call(export_function, args=c(list("object"=collection), export_args))
                      which_present <- .test_which_data_element_present(data_elements, outcome_type=outcome_type)
                      
                      if(outcome_type %in% outcome_type_available & !.except_prospective){
                        testthat::expect_equal(all(which_present), TRUE) 
                        
                        if(debug) show(data_elements)
                        
                      } else if(!outcome_type %in% outcome_type_available){
                        testthat::expect_equal(all(!which_present), TRUE)
                        
                      } else {
                        testthat::expect_equal(any(!which_present), TRUE)
                      }
                    })
    
    
    # Ensemble from multiple datasets.
    multi_model_set <- suppressWarnings(lapply(multi_data,
                                               train,
                                               cluster_method="hclust",
                                               imputation_method="simple",
                                               fs_method="mim",
                                               hyperparameter_list=hyperparameters,
                                               learner="lasso",
                                               cluster_similarity_threshold=0.7,
                                               time_max=1832,
                                               create_novelty_detector=create_novelty_detector))
    
    # Create data from ensemble of multiple models
    multi_model_full <- as_familiar_data(object=multi_model_set,
                                         data=multi_data[[1]],
                                         data_element=data_element,
                                         cl=cl,
                                         ...)
    
    # Create additional familiar data objects.
    data_empty_full_1 <- as_familiar_data(object=model_full_1,
                                          data=empty_data,
                                          data_element=data_element,
                                          cl=cl,
                                          ...)
    data_empty_full_2 <- as_familiar_data(object=model_full_2,
                                          data=empty_data,
                                          data_element=data_element,
                                          cl=cl,
                                          ...)
    data_one_sample_full_1 <- as_familiar_data(object=model_full_1,
                                               data=full_one_sample_data,
                                               data_element=data_element,
                                               cl=cl,
                                               ...)
    data_one_sample_full_2 <- as_familiar_data(object=model_full_2,
                                               data=full_one_sample_data,
                                               data_element=data_element,
                                               cl=cl,
                                               ...)
    data_identical_full_1 <- as_familiar_data(object=model_full_1,
                                              data=identical_sample_data,
                                              data_element=data_element,
                                              cl=cl,
                                              ...)
    data_identical_full_2 <- as_familiar_data(object=model_full_2,
                                              data=identical_sample_data,
                                              data_element=data_element,
                                              cl=cl,
                                              ...)
    
    # Create a dataset with a missing quadrant.
    test_fun(paste0("3. Export data for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available, "can", "cannot"),
                    " be created for a dataset with some missing data."), {
                      
                      object <- list(data_good_full_1, data_good_full_2, data_empty_full_1, data_good_full_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
                      
                      data_elements <- do.call(export_function, args=c(list("object"=collection), export_args))
                      which_present <- .test_which_data_element_present(data_elements, outcome_type=outcome_type)
                      
                      if(outcome_type %in% outcome_type_available){
                        testthat::expect_equal(any(which_present), TRUE) 
                        
                        if(debug) show(data_elements)
                        
                      } else {
                        testthat::expect_equal(all(!which_present), TRUE)
                      }
                    })
    
    # Create a dataset with all missing quadrants
    test_fun(paste0("4. Export data for ", outcome_type, " outcomes ",
                    ifelse(.always_available, "can", "cannot"),
                    " be created for a dataset with completely missing data."), {
                      
                      object <- list(data_empty_full_1, data_empty_full_2, data_empty_full_1, data_empty_full_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
                      
                      data_elements <- do.call(export_function, args=c(list("object"=collection), export_args))
                      which_present <- .test_which_data_element_present(data_elements, outcome_type=outcome_type)
                      
                      if(outcome_type %in% outcome_type_available & .always_available){
                        testthat::expect_equal(all(which_present), TRUE) 
                        
                        if(debug) show(data_elements)
                        
                      } else {
                        testthat::expect_equal(all(!which_present), TRUE)
                      }
                    })
    
    # Create dataset with one-sample quadrants for validation
    test_fun(paste0("5. Export data for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available, "can", "cannot"),
                    " be created for a dataset where some data only have one sample."), {
                      
                      object <- list(data_good_full_1, data_good_full_2, data_one_sample_full_1, data_one_sample_full_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
                      
                      data_elements <- do.call(export_function, args=c(list("object"=collection), export_args))
                      which_present <- .test_which_data_element_present(data_elements, outcome_type=outcome_type)
                      
                      if(outcome_type %in% outcome_type_available){
                        testthat::expect_equal(any(which_present), TRUE) 
                        
                        if(debug) show(data_elements)
                        
                      } else {
                        testthat::expect_equal(all(!which_present), TRUE)
                      }
                    })
    
    # Create dataset with some quadrants with identical data
    test_fun(paste0("6. Export data for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available, "can", "cannot"),
                    " be created for a dataset where some data only have identical samples."), {
                      
                      object <- list(data_good_full_1, data_good_full_2, data_identical_full_1, data_identical_full_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
                      
                      data_elements <- do.call(export_function, args=c(list("object"=collection), export_args))
                      which_present <- .test_which_data_element_present(data_elements, outcome_type=outcome_type)
                      
                      if(outcome_type %in% outcome_type_available){
                        testthat::expect_equal(all(which_present), TRUE) 
                        
                        if(debug) show(data_elements)
                        
                      } else {
                        testthat::expect_equal(all(!which_present), TRUE)
                      }
                    })
    
    test_fun(paste0("7. Export data for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available, "can", "cannot"),
                    " be created for a dataset created from an ensemble of multiple models."), {
                      
                      object <- list(multi_model_full)
                      object <- mapply(set_object_name, object, c("development_1"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development")))
                      
                      data_elements <- do.call(export_function, args=c(list("object"=collection), export_args))
                      which_present <- .test_which_data_element_present(data_elements, outcome_type=outcome_type)
                      
                      if(outcome_type %in% outcome_type_available){
                        testthat::expect_equal(all(which_present), TRUE) 
                        
                        if(debug) show(data_elements)
                        
                      } else {
                        testthat::expect_equal(all(!which_present), TRUE)
                      }
                    })
    
    #####One-feature data set###################################################
    
    # Train the model.
    model_one_1 <- suppressWarnings(train(cl=cl,
                                          data=one_feature_data,
                                          cluster_method="none",
                                          imputation_method="simple",
                                          fs_method="mim",
                                          hyperparameter_list=hyperparameters,
                                          learner="lasso",
                                          time_max=1832,
                                          create_novelty_detector=create_novelty_detector))
    
    model_one_2 <- model_one_1
    model_one_2@fs_method <- "mifs"
    
    # Create familiar data objects.
    data_good_one_1 <- as_familiar_data(object=model_one_1, data=one_feature_data, data_element=data_element, cl=cl, ...)
    data_good_one_2 <- as_familiar_data(object=model_one_2, data=one_feature_data, data_element=data_element, cl=cl, ...)
    data_one_sample_one_1 <- as_familiar_data(object=model_one_1, data=one_feature_one_sample_data, data_element=data_element, cl=cl, ...)
    data_one_sample_one_2 <- as_familiar_data(object=model_one_2, data=one_feature_one_sample_data, data_element=data_element, cl=cl, ...)
    data_identical_one_1 <- as_familiar_data(object=model_one_1, data=one_feature_invariant_data, data_element=data_element, cl=cl, ...)
    data_identical_one_2 <- as_familiar_data(object=model_one_2, data=one_feature_invariant_data, data_element=data_element, cl=cl, ...)
    
    
    # Create a completely intact, one sample dataset.
    test_fun(paste0("8. Export data for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available && !.except_one_feature, "can", "cannot"),
                    " be created for a complete one-feature data set."), {
                      
                      object <- list(data_good_one_1, data_good_one_2, data_good_one_1, data_good_one_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
                      
                      data_elements <- do.call(export_function, args=c(list("object"=collection), export_args))
                      which_present <- .test_which_data_element_present(data_elements, outcome_type=outcome_type)
                      
                      if(outcome_type %in% outcome_type_available & !.except_one_feature){
                        testthat::expect_equal(all(which_present), TRUE) 
                        
                        if(debug) show(data_elements)
                        
                      } else if(!outcome_type %in% outcome_type_available){
                        testthat::expect_equal(all(!which_present), TRUE)

                      } else {
                        testthat::expect_equal(any(!which_present), TRUE)
                      }
                    })
    
    # Create a dataset with a one-sample quadrant.
    test_fun(paste0("9. Export data for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available && !.except_one_feature, "can", "cannot"),
                    " be created for a dataset with some one-sample data."), {
                      
                      object <- list(data_good_one_1, data_good_one_2, data_one_sample_one_1, data_one_sample_one_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
                      
                      data_elements <- do.call(export_function, args=c(list("object"=collection), export_args))
                      which_present <- .test_which_data_element_present(data_elements, outcome_type=outcome_type)
                      
                      if(outcome_type %in% outcome_type_available & !.except_one_feature){
                        testthat::expect_equal(any(which_present), TRUE) 
                        
                        if(debug) show(data_elements)
                        
                      } else if(!outcome_type %in% outcome_type_available){
                        testthat::expect_equal(all(!which_present), TRUE)
                        
                      } else {
                        testthat::expect_equal(any(!which_present), TRUE)
                      }
                    })
    
    # Create a dataset with some identical data.
    test_fun(paste0("10. Export data for ", outcome_type, " outcomes ",
                    ifelse(outcome_type %in% outcome_type_available && !.except_one_feature, "can", "cannot"),
                    " be created for a dataset with some invariant data."), {
                      
                      object <- list(data_good_one_1, data_good_one_2, data_identical_one_1, data_identical_one_2)
                      object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
                      
                      collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
                      
                      data_elements <- do.call(export_function, args=c(list("object"=collection), export_args))
                      which_present <- .test_which_data_element_present(data_elements, outcome_type=outcome_type)
                      
                      if(outcome_type %in% outcome_type_available & !.except_one_feature){
                        testthat::expect_equal(any(which_present), TRUE) 
                        
                        if(debug) show(data_elements)
                        
                      } else if(!outcome_type %in% outcome_type_available){
                        testthat::expect_equal(all(!which_present), TRUE)
                        
                      } else {
                        testthat::expect_equal(any(!which_present), TRUE)
                      }
                    })
    
    #####Data set with limited censoring########################################
    if(outcome_type %in% c("survival", "competing_risk")){
      # Train the model.
      model_cens_1 <- suppressWarnings(train(cl=cl,
                                             data=no_censoring_data,
                                             cluster_method="none",
                                             imputation_method="simple",
                                             fs_method="mim",
                                             hyperparameter_list=hyperparameters,
                                             learner="lasso",
                                             time_max=1832))
      
      model_cens_2 <- suppressWarnings(train(cl=cl,
                                             data=one_censored_data,
                                             cluster_method="none",
                                             imputation_method="simple",
                                             fs_method="mim",
                                             hyperparameter_list=hyperparameters,
                                             learner="lasso",
                                             time_max=1832))
      
      model_cens_3 <- suppressWarnings(train(cl=cl,
                                             data=few_censored_data,
                                             cluster_method="none",
                                             imputation_method="simple",
                                             fs_method="mim",
                                             hyperparameter_list=hyperparameters,
                                             learner="lasso",
                                             time_max=1832))
      
      data_cens_1 <- as_familiar_data(object=model_cens_1, data=no_censoring_data, data_element=data_element, cl=cl, ...)
      data_cens_2 <- as_familiar_data(object=model_cens_2, data=one_censored_data, data_element=data_element, cl=cl, ...)
      data_cens_3 <- as_familiar_data(object=model_cens_3, data=few_censored_data, data_element=data_element, cl=cl, ...)
      
      # Create a dataset with some identical data.
      test_fun(paste0("11. Exports for ", outcome_type, " outcomes ",
                      ifelse(outcome_type %in% outcome_type_available, "can", "cannot"),
                      " be created for a data set that includes no or limited censoring."), {
                        
                        object <- list(data_cens_1, data_cens_2, data_cens_3)
                        object <- mapply(set_object_name, object, c("no_censoring", "one_censored", "few_censored"))
                        
                        collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("no_censoring", "one_censored", "few_censored")))
                        
                        data_elements <- do.call(export_function, args=c(list("object"=collection), export_args))
                        which_present <- .test_which_data_element_present(data_elements, outcome_type=outcome_type)
                        
                        if(outcome_type %in% outcome_type_available){
                          testthat::expect_equal(all(which_present), TRUE) 
                          
                          if(debug) show(data_elements)
                          
                        } else {
                          testthat::expect_equal(all(!which_present), TRUE)
                        }
                      })
      
    }
    
    ##### Model with missing survival predictions ##############################
    if(outcome_type %in% c("survival", "competing_risk")){
      # Train the model.
      model_failed_predictions <- suppressWarnings(train(cl=cl,
                                                         data=full_data,
                                                         cluster_method="none",
                                                         imputation_method="simple",
                                                         fs_method="mim",
                                                         hyperparameter_list=hyperparameters,
                                                         learner="lasso_test",
                                                         time_max=1832,
                                                         create_novelty_detector=create_novelty_detector))
      
      failed_prediction_data <- as_familiar_data(object=model_failed_predictions, data=full_data, data_element=data_element, cl=cl, ...)
      
      test_fun(paste0("12. Exports for ", outcome_type, " outcomes ",
                      ifelse(outcome_type %in% outcome_type_available && .except_failed_survival_prediction, "can", "cannot"),
                      " be created for models that do not allow for predicting survival probabilitiies."), {
                        
                        collection <- suppressWarnings(as_familiar_collection(failed_prediction_data, familiar_data_names=c("no_survival_predictions")))
                        
                        data_elements <- do.call(export_function, args=c(list("object"=collection), export_args))
                        which_present <- .test_which_data_element_present(data_elements, outcome_type=outcome_type)
                        
                        if(outcome_type %in% outcome_type_available & !.except_failed_survival_prediction){
                          testthat::expect_equal(all(which_present), TRUE) 
                          
                          if(debug) show(data_elements)
                          
                        } else if(!outcome_type %in% outcome_type_available){
                          testthat::expect_equal(all(!which_present), TRUE)
                          
                        } else {
                          testthat::expect_equal(any(!which_present), TRUE)
                        }
                      })
    }
    
  }
}



test_export_specific <- function(export_function,
                                 data_element,
                                 outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                                 ...,
                                 export_args=list(),
                                 use_data_set="full",
                                 n_models=1L,
                                 create_novelty_detector=FALSE,
                                 debug=FALSE){
  
  if(debug){
    test_fun <- debug_test_that
    
  } else {
    test_fun <- testthat::test_that
  }
  
  # Create list for output.
  out_elements <- list()
  
  # Iterate over the outcome type.
  for(outcome_type in outcome_type_available){
    
    # Obtain data.
    main_data <- test.create_good_data_set(outcome_type)
    
    data <- switch(use_data_set,
                   "full"=test.create_good_data_set(outcome_type),
                   "identical"=test.create_all_identical_data_set(outcome_type),
                   "one_sample"=test.create_one_sample_data_set(outcome_type))
    
    # Parse hyperparameter list
    hyperparameters <- list("sign_size"=get_n_features(main_data),
                            "family"=switch(outcome_type,
                                            "continuous"="gaussian",
                                            "count"="poisson",
                                            "binomial"="binomial",
                                            "multinomial"="multinomial",
                                            "survival"="cox"))

    if(n_models == 1){
      # Train the model.
      model_full_1 <- suppressWarnings(train(data=main_data,
                                             cluster_method="none",
                                             imputation_method="simple",
                                             fs_method="mim",
                                             hyperparameter_list=hyperparameters,
                                             learner="lasso",
                                             time_max=1832,
                                             create_novelty_detector=create_novelty_detector))
      
      model_full_2 <- model_full_1
      model_full_2@fs_method <- "mifs"
      
    } else {
      # Train a set of models.
      model_full_1 <- list()
      model_full_2 <- list()
      
      for(ii in seq_len(n_models)){
        temp_model_1 <- suppressWarnings(train(data=main_data,
                                               cluster_method="none",
                                               imputation_method="simple",
                                               fs_method="mim",
                                               hyperparameter_list=hyperparameters,
                                               learner="lasso",
                                               time_max=1832,
                                               create_bootstrap=TRUE,
                                               create_novelty_detector=create_novelty_detector))
        
        temp_model_2 <- temp_model_1
        temp_model_2@fs_method <- "mifs"
        
        model_full_1[[ii]] <- temp_model_1
        model_full_2[[ii]] <- temp_model_2
      }
    }
    
    
    # Create familiar data objects.
    data_good_full_1 <- as_familiar_data(object=model_full_1,
                                         data=data,
                                         data_element=data_element,
                                         ...)
    
    data_good_full_2 <- as_familiar_data(object=model_full_2,
                                         data=data,
                                         data_element=data_element,
                                         ...)
    
    # Generate data objects and names.
    object <- list(data_good_full_1, data_good_full_2, data_good_full_1, data_good_full_2)
    object <- mapply(set_object_name, object, c("development_1", "development_2", "validation_1", "validation_2"))
    
    # Process to collect.
    collection <- suppressWarnings(as_familiar_collection(object, familiar_data_names=c("development", "development", "validation", "validation")))
    
    # Create data elements.
    data_elements <- do.call(export_function, args=c(list("object"=collection),
                                                     export_args))
    
    # Save data elements and add name.
    current_element <- list(data_elements)
    names(current_element) <- outcome_type
    
    out_elements <- c(out_elements, current_element)
  }
  
  return(out_elements)
}



integrated_test <- function(..., debug=FALSE){
  
  if(debug){
    test_fun <- debug_test_that
    suppress_fun <- identity
    
  } else {
    test_fun <- testthat::test_that
    suppress_fun <- suppressMessages
  }
  
  for(outcome_type in c("count", "continuous", "binomial", "multinomial", "survival")){
    
    test_fun(paste0("Experiment for a good dataset with ", outcome_type, " outcome functions correctly."), {

      # Create datasets
      full_data <- test.create_good_data_set(outcome_type)

      # Parse hyperparameter list
      hyperparameters <- list("sign_size"=get_n_features(full_data),
                              "family"=switch(outcome_type,
                                              "continuous"="gaussian",
                                              "count"="poisson",
                                              "binomial"="binomial",
                                              "multinomial"="multinomial",
                                              "survival"="cox"))

      output <- suppress_fun(summon_familiar(data=full_data,
                                             learner="lasso",
                                             hyperparameter=list("lasso"=hyperparameters),
                                             ...))

      testthat::expect_equal(is.null(output), FALSE)
    })

    
    test_fun(paste0("Experiment for a bad dataset with ", outcome_type, " outcome functions correctly."), {
      
      # Create datasets. We explicitly insert NA data to pass an initial
      # plausibility check.
      bad_data <- test.create_bad_data_set(outcome_type=outcome_type,
                                           add_na_data=TRUE)
      
      # Parse hyperparameter list
      hyperparameters <- list("sign_size"=get_n_features(bad_data),
                              "family"=switch(outcome_type,
                                              "continuous"="gaussian",
                                              "count"="poisson",
                                              "binomial"="binomial",
                                              "multinomial"="multinomial",
                                              "survival"="cox"))
      
      # Note that we set a very high feature_max_fraction_missing to deal with
      # NA rows in the dataset. Also time is explicitly set to prevent an error.
      output <- suppress_fun(summon_familiar(data=bad_data,
                                             learner="lasso",
                                             hyperparameter=list("lasso"=hyperparameters),
                                             feature_max_fraction_missing=0.95,
                                             time_max=1832,
                                             ...))
      
      testthat::expect_equal(is.null(output), FALSE) 
    })
  }
}



debug_test_that <- function(desc, code){
  # This is a drop-in replacement for testthat::test_that that makes it easier
  # to debug errors.
  
  if(!is.character(desc) || length(desc) != 1){
    stop("\"desc\" should be a character string")
  }
  
  # Execute the code
  code <- substitute(code)
  eval(code, envir=parent.frame())
}



.test_which_plot_present <- function(p){
  
  # Check if the top element is null or empty.
  if(is.null(p)) return(FALSE)
  if(length(p) == 0) return(FALSE)
  
  # Check that the top element is a gtable or ggplot.
  if(gtable::is.gtable(p) | ggplot2::is.ggplot(p)) return(TRUE)
  
  plot_present <- sapply(p, gtable::is.gtable) | sapply(p, ggplot2::is.ggplot)
  if(any(plot_present)) return(plot_present)
  
  if(all(sapply(p, is.null))) return(!sapply(p, is.null))
  
  # If the code gets here, p is a nested list.
  p <- unlist(p, recursive=FALSE)
  
  if(is.null(p)) return(FALSE)
  if(length(p) == 0) return(FALSE)
  
  return(sapply(p, gtable::is.gtable) | sapply(p, ggplot2::is.ggplot))
}



.test_which_data_element_present <- function(x, outcome_type){
  
  # Check if the top element is null or empty.
  if(is_empty(x)) return(FALSE)
  
  data_element_present <- !sapply(x, is_empty)
  if(!any(data_element_present)) return(FALSE)
  
  # Class-specific tests.
  if(all(sapply(x, is, class2="familiarDataElementPredictionTable"))){
    return(sapply(x, function(x, outcome_type) (any_predictions_valid(x@data, outcome_type)), outcome_type=outcome_type))
    
  }
  
  return(data_element_present)
}



.test_start_cluster <- function(n_cores=NULL){
  
  # Determine the number of available cores.
  n_cores_available <- parallel::detectCores() - 1L
  
  # Determine the number of available cores.
  if(is.null(n_cores)) n_cores <- n_cores_available
  
  if(n_cores > n_cores_available) n_cores <- n_cores_available
  
  if(n_cores < 2) return(NULL)
  
  assign("is_external_cluster", FALSE, envir=familiar_global_env)
  
  # Start a new cluster
  cl <- .start_cluster(n_cores=n_cores,
                       cluster_type="psock")
  
  # If the cluster doesn't start, return a NULL
  if(is.null(cl)) return(NULL)
  
  # Set library paths to avoid issues with non-standard library locations.
  libs <- .libPaths()
  parallel::clusterExport(cl=cl, varlist="libs", envir=environment())
  parallel::clusterEvalQ(cl=cl, .libPaths(libs))
  
  # Load familiar and data.table libraries to each cluster node.
  parallel::clusterEvalQ(cl=cl, library(familiar))
  parallel::clusterEvalQ(cl=cl, library(data.table))
  
  # Set options on each cluster node.
  parallel::clusterEvalQ(cl=cl, options(rf.cores=as.integer(1)))
  parallel::clusterEvalQ(cl=cl, data.table::setDTthreads(1L))
  
  return(cl)
}



.test_create_hyperparameter_object <- function(data,
                                               vimp_method,
                                               learner,
                                               is_vimp,
                                               set_signature_feature=FALSE){
  
  if(set_signature_feature){
    signature_features <- get_feature_columns(data)[1:2]
    
  } else {
    signature_features <- NULL
  }
  
  # Create feature info list.
  feature_info_list <- create_feature_info(data=data,
                                           fs_method=vimp_method,
                                           learner=learner,
                                           cluster_method="none",
                                           imputation_method="simple",
                                           signature=signature_features,
                                           parallel=FALSE)
  
  # Find required features.
  required_features <- find_required_features(features=get_available_features(feature_info_list=feature_info_list),
                                              feature_info_list=feature_info_list)
  
 
  if(is_vimp){
    # Create the variable importance met hod object or familiar model object
    # to compute variable importance with.
    object <- promote_vimp_method(object=methods::new("familiarVimpMethod",
                                                      outcome_type = data@outcome_type,
                                                      vimp_method = vimp_method,
                                                      required_features = required_features,
                                                      feature_info = feature_info_list,
                                                      outcome_info = data@outcome_info))
    
  } else {
    # Create familiar model object.
    object <- promote_learner(object=methods::new("familiarModel",
                                                  outcome_type = data@outcome_type,
                                                  learner = learner,
                                                  fs_method = vimp_method,
                                                  required_features =  required_features,
                                                  feature_info = feature_info_list,
                                                  outcome_info = data@outcome_info))
  }
  
  return(object)
}

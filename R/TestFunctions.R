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
        break
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



test_all_learners_train_predict_vimp <- function(learners, hyperparameter_list=NULL,
                                                 except_train=NULL, except_predict=NULL,
                                                 except_predict_survival=NULL,
                                                 has_vimp=TRUE){
  
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
      
      # Train the model..
      model <- suppressWarnings(train(data=full_data,
                                      cluster_method="none",
                                      imputation_method="simple",
                                      hyperparameter_list=hyperparameters,
                                      learner=learner,
                                      time_max=1832))
      
      # Test that models can be created.
      testthat::test_that(paste0("Model for ", outcome_type, " can be created using ", learner, " using a complete data set."), {
        
        # Test that the model was successfully created.
        testthat::expect_equal(model_is_trained(model),
                               ifelse(learner %in% except_train, FALSE, TRUE))
        
        if(outcome_type == "survival"){
          # Calibration info is present
          testthat::expect_equal(familiar:::has_calibration_info(model), TRUE)
        }
      })
      
      # Test that models can be used to predict the outcome.
      testthat::test_that(paste0("Sample predictions for ", outcome_type, " can be made using ", learner, " for a complete data set."), {
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
      })
      
      # Test that models can be used to predict the outcome.
      testthat::test_that(paste0("Sample predictions for ", outcome_type, " can be made using ", learner, " for a one-sample data set."), {
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
      })
      
      # Test that models cannot predict for empty datasets.
      testthat::test_that(paste0("Sample predictions for ", outcome_type, " can not be made using ", learner, " for an empty data set."), {
        # Expect predictions to be made.
        prediction_table <- suppressWarnings(.predict(model, data=empty_data))
        
        # Test that the predictions were successfully made.
        testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type), FALSE)
      })
      
      # Test that models can be used to predict survival probabilities.
      if(outcome_type %in% c("survival", "competing_risk")){
        testthat::test_that(paste0("Sample survival predictions for ", outcome_type, " can be made using ", learner, " for a complete data set."), {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model, full_data, type="survival_probability", time=1000))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                                 ifelse(learner %in% c(except_train, except_predict, except_predict_survival), FALSE, TRUE))
        })
        
        testthat::test_that(paste0("Sample survival predictions for ", outcome_type, " can be made using ", learner, " for a one-sample data set."), {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model, data=full_one_sample_data, type="survival_probability", time=1000))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                                 ifelse(learner %in% c(except_train, except_predict, except_predict_survival), FALSE, TRUE))
        })
      }
      
      # Test that the model has variable importance.
      testthat::test_that(paste0("Model has variable importance for ", outcome_type, " and ", learner, "for the complete data set."), {
        # Extract the variable importance table.
        vimp_table <- suppressWarnings(familiar:::..vimp(model))
        
        if(has_vimp){
          # Get the number of features
          n_features <- get_n_features(full_data)
          
          # Expect that the vimp table has two rows.
          testthat::expect_equal(nrow(vimp_table) > 0 & nrow(vimp_table) <= n_features, TRUE)
          
          # Expect that the names in the vimp table correspond to those of the
          # features.
          testthat::expect_equal(all(vimp_table$name %in% familiar:::get_feature_columns(full_data)), TRUE)
          
        } else {
          
          # Expect that the vimp table has no rows.
          testthat::expect_equal(nrow(vimp_table), 0)
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
                                      create_bootstrap=TRUE))
      
      # Test that models can be created.
      testthat::test_that(paste0("Model for ", outcome_type, " can be created using ", learner, " using a complete data set."), {
        
        # Test that the model was successfully created.
        testthat::expect_equal(model_is_trained(model),
                               ifelse(learner %in% except_train, FALSE, TRUE))
        
        if(outcome_type == "survival"){
          # Calibration info is present
          testthat::expect_equal(familiar:::has_calibration_info(model), TRUE)
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
      
      # Test that models can be created.
      testthat::test_that(paste0("Model for ", outcome_type, " can be created using ", learner, " using a one-feature data set."), {
        
        # Test that the model was successfully created.
        testthat::expect_equal(model_is_trained(model),
                               ifelse(learner %in% except_train, FALSE, TRUE))
        
        if(outcome_type == "survival"){
          # Calibration info is present
          testthat::expect_equal(familiar:::has_calibration_info(model), TRUE)
        }
      })
      
      # Test that models can be used to predict the outcome.
      testthat::test_that(paste0("Sample predictions for ", outcome_type, " can be made using ", learner, " for a one-feature data set."), {
        # Expect predictions to be made.
        prediction_table <- suppressWarnings(.predict(model, data=one_feature_data))
        
        # Test that the predictions were successfully made.
        testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                               ifelse(learner %in% c(except_train, except_predict), FALSE, TRUE))
        
        if(outcome_type %in% c("binomial", "multinomial")){
          # Expect that the predicted_class column is a factor.
          testthat::expect_s3_class(prediction_table$predicted_class, "factor")
          
          # Expect that the class levels are the same as those in the model.
          testthat::expect_equal(levels(prediction_table$predicted_class), get_outcome_class_levels(model))
        }
      })
      
      # Test that models can be used to predict the outcome.
      testthat::test_that(paste0("Sample predictions for ", outcome_type, " can be made using ", learner, " for a one-feature, one-sample data set."), {
        # Expect predictions to be made.
        prediction_table <- suppressWarnings(.predict(model, data=one_feature_one_sample_data))
        
        # Test that the predictions were successfully made.
        testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                               ifelse(learner %in% c(except_train, except_predict), FALSE, TRUE))
        
        if(outcome_type %in% c("binomial", "multinomial")){
          # Expect that the predicted_class column is a factor.
          testthat::expect_s3_class(prediction_table$predicted_class, "factor")
          
          # Expect that the class levels are the same as those in the model.
          testthat::expect_equal(levels(prediction_table$predicted_class), get_outcome_class_levels(model))
        }
      })
      
      # Test that models can be used to predict survival probabilities.
      if(outcome_type %in% c("survival", "competing_risk")){
        testthat::test_that(paste0("Sample survival predictions for ", outcome_type, " can be made using ", learner, " for a one-feature data set."), {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model, one_feature_data, type="survival_probability", time=1000))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                                 ifelse(learner %in% c(except_train, except_predict, except_predict_survival), FALSE, TRUE))
        })
        
        testthat::test_that(paste0("Sample survival predictions for ", outcome_type, " can be made using ", learner, " for a one-feature, one-sample data set."), {
          # Expect predictions to be made.
          prediction_table <- suppressWarnings(.predict(model, data=one_feature_one_sample_data, type="survival_probability", time=1000))
          
          # Test that the predictions were successfully made.
          testthat::expect_equal(any_predictions_valid(prediction_table, outcome_type),
                                 ifelse(learner %in% c(except_train, except_predict, except_predict_survival), FALSE, TRUE))
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
      testthat::test_that(paste0("Model for ", outcome_type, " can not be created using ", learner, " using a bad data set."), {
        
        # Test that the model was successfully created.
        testthat::expect_equal(model_is_trained(model), FALSE)
        
        if(outcome_type == "survival"){
          # Calibration info is absent.
          testthat::expect_equal(familiar:::has_calibration_info(model), TRUE)
        }
      })
      
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
                                  hyperparameter_list=NULL){
  
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
      vimp_object <- familiar:::prepare_vimp_object(data=full_data,
                                                    vimp_method=vimp_method,
                                                    vimp_method_parameter_list=hyperparameters,
                                                    outcome_type=outcome_type,
                                                    cluster_method="none",
                                                    imputation_method="simple")
      
      
      testthat::test_that(paste0("Variable importance can be computed for ", outcome_type, " with the ", vimp_method, " using a complete data set."), {
        
        vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, full_data))
        
        # Get the number of features
        n_features <- get_n_features(full_data)
        
        # Expect that the vimp table is not empty..
        testthat::expect_equal(nrow(vimp_table) > 0 & nrow(vimp_table) <= n_features, TRUE)
        
        # Expect that the names in the vimp table correspond to those of the
        # features.
        testthat::expect_equal(all(vimp_table$name %in% familiar:::get_feature_columns(full_data)), TRUE)
      })
      
      
      testthat::test_that(paste0("Variable importance can be computed for ", outcome_type, " with the ", vimp_method, " using a complete data set with one invariant feature."), {
        
        vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, full_one_invariant_data))
        
        # Get the number of features
        n_features <- get_n_features(full_one_invariant_data)
        
        # Expect that the vimp table is not empty..
        testthat::expect_equal(nrow(vimp_table) > 0 & nrow(vimp_table) <= n_features, TRUE)
        
        # Expect that the names in the vimp table correspond to those of the
        # features.
        testthat::expect_equal(all(vimp_table$name %in% familiar:::get_feature_columns(full_one_invariant_data)), TRUE)
      })
      
      
      testthat::test_that(paste0("Variable importance cannot be computed for ", outcome_type, " with the ", vimp_method, " using an empty data set."), {
        
        vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, empty_data))
       
        # Expect that the vimp table has two rows.
        testthat::expect_equal(is_empty(vimp_table), TRUE)
      })
      
      
      testthat::test_that(paste0("Variable importance cannot be computed for ", outcome_type, " with the ", vimp_method, " using a bad data set."), {
        
        vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, bad_data))
        
        # Expect that the vimp table has two rows.
        testthat::expect_equal(is_empty(vimp_table), TRUE)
      })
      
      
      testthat::test_that(paste0("Variable importance cannot be computed for ", outcome_type, " with the ", vimp_method, " using a one-sample data set."), {
        
        vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, full_one_sample_data))
        
        # Expect that the vimp table has two rows.
        testthat::expect_equal(is_empty(vimp_table), TRUE)
      })
      
      
      #####One-feature data set#################################################
      
      # Process dataset.
      vimp_object <- familiar:::prepare_vimp_object(data=one_feature_data,
                                                    vimp_method=vimp_method,
                                                    vimp_method_parameter_list=hyperparameters,
                                                    outcome_type=outcome_type,
                                                    cluster_method="none",
                                                    imputation_method="simple")
      
      
      testthat::test_that(paste0("Variable importance can be computed for ", outcome_type, " with the ", vimp_method, " using a one-feature data set."), {
        
        vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, one_feature_data))
        
        # Expect that the vimp table is not empty.
        testthat::expect_equal(nrow(vimp_table), 1)
        
        # Expect that the names in the vimp table correspond to those of the
        # features.
        testthat::expect_equal(all(vimp_table$name %in% familiar:::get_feature_columns(one_feature_data)), TRUE)
      })
      
      
      testthat::test_that(paste0("Variable importance cannot be computed for ", outcome_type, " with the ", vimp_method, " using a one-feature data set with an invariant feature."), {
        
        vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, one_feature_invariant_data))
        
        # Expect that the vimp table is empty.
        testthat::expect_equal(is_empty(vimp_table), TRUE)
      })
      
      
      testthat::test_that(paste0("Variable importance cannot be computed for ", outcome_type, " with the ", vimp_method, " using a one-feature, one-sample data set."), {
        
        vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, one_feature_one_sample_data))
        
        # Expect that the vimp table is empty.
        testthat::expect_equal(is_empty(vimp_table), TRUE)
      })
      
    }
    
  }
  
}

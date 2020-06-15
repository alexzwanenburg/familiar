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



test_all_learners_train_and_predict <- function(learners, hyperparameter_list=NULL,
                                                except_train=NULL, except_predict=NULL,
                                                except_predict_survival=NULL){
  
  # Iterate over the outcome type.
  for(outcome_type in c("count", "continuous", "binomial", "multinomial", "survival")){
    
    # Obtain data.
    full_data <- test.create_good_data_set(outcome_type)
    full_one_sample_data <- test.create_one_sample_data_set(outcome_type)
    one_feature_data <- test.create_one_feature_data_set(outcome_type)
    one_feature_one_sample_data <- test.create_one_feature_one_sample_data_set(outcome_type)
    
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
      
      #####Full data set########################################################
      
      # Train the model..
      model <- train(data=full_data,
                     cluster_method="none",
                     imputation_method="simple",
                     hyperparameter_list=c(hyperparameter_list[[outcome_type]],
                                           list("sign_size"=get_n_features(full_data))),
                     learner=learner,
                     time_max=1832)
      
      # Test that models can be created.
      testthat::test_that(paste0("Model for ", outcome_type, " can be created using ", learner, " using a complete data set."), {
        
        # Test that the model was successfully created.
        expect_equal(model_is_trained(model),
                     ifelse(learner %in% except_train, FALSE, TRUE))
      })
      
      # Test that models can be used to predict the outcome.
      testthat::test_that(paste0("Sample predictions for ", outcome_type, " can be made using ", learner, " for a complete data set."), {
        # Expect predictions to be made.
        prediction_table <- .predict(model, data=full_data)
        
        # Test that the predictions were successfully made.
        expect_equal(any_predictions_valid(prediction_table, outcome_type),
                     ifelse(learner %in% c(except_train, except_predict), FALSE, TRUE))
      })
      
      # Test that models can be used to predict the outcome.
      testthat::test_that(paste0("Sample predictions for ", outcome_type, " can be made using ", learner, " for a one-sample data set."), {
        # Expect predictions to be made.
        prediction_table <- .predict(model, data=full_one_sample_data)
        
        # Test that the predictions were successfully made.
        expect_equal(any_predictions_valid(prediction_table, outcome_type),
                     ifelse(learner %in% c(except_train, except_predict), FALSE, TRUE))
      })
      
      
      # Test that models can be used to predict survival probabilities.
      if(outcome_type %in% c("survival", "competing_risk")){
        testthat::test_that(paste0("Sample survival predictions for ", outcome_type, " can be made using ", learner, " for a complete data set."), {
          # Expect predictions to be made.
          prediction_table <- .predict(model, full_data, type="survival_probability", time=1000)
          
          # Test that the predictions were successfully made.
          expect_equal(any_predictions_valid(prediction_table, outcome_type),
                       ifelse(learner %in% c(except_train, except_predict, except_predict_survival), FALSE, TRUE))
        })
        
        testthat::test_that(paste0("Sample survival predictions for ", outcome_type, " can be made using ", learner, " for a one-sample data set."), {
          # Expect predictions to be made.
          prediction_table <- .predict(model, data=full_one_sample_data, type="survival_probability", time=1000)
          
          # Test that the predictions were successfully made.
          expect_equal(any_predictions_valid(prediction_table, outcome_type),
                       ifelse(learner %in% c(except_train, except_predict, except_predict_survival), FALSE, TRUE))
        })
      }
      
      #####One-feature data set#################################################
      # Train the model..
      model <- train(data=one_feature_data,
                     cluster_method="none",
                     imputation_method="simple",
                     hyperparameter_list=c(hyperparameter_list[[outcome_type]],
                                           list("sign_size"=get_n_features(one_feature_data))),
                     learner=learner,
                     time_max=1832)
      
      # Test that models can be created.
      testthat::test_that(paste0("Model for ", outcome_type, " can be created using ", learner, " using a one-feature data set."), {
        
        # Test that the model was successfully created.
        expect_equal(model_is_trained(model),
                     ifelse(learner %in% except_train, FALSE, TRUE))
      })
      
      # Test that models can be used to predict the outcome.
      testthat::test_that(paste0("Sample predictions for ", outcome_type, " can be made using ", learner, " for a one-feature data set."), {
        # Expect predictions to be made.
        prediction_table <- .predict(model, data=one_feature_data)
        
        # Test that the predictions were successfully made.
        expect_equal(any_predictions_valid(prediction_table, outcome_type),
                     ifelse(learner %in% c(except_train, except_predict), FALSE, TRUE))
      })
      
      # Test that models can be used to predict the outcome.
      testthat::test_that(paste0("Sample predictions for ", outcome_type, " can be made using ", learner, " for a one-feature, one-sample data set."), {
        # Expect predictions to be made.
        prediction_table <- .predict(model, data=one_feature_one_sample_data)
        
        # Test that the predictions were successfully made.
        expect_equal(any_predictions_valid(prediction_table, outcome_type),
                     ifelse(learner %in% c(except_train, except_predict), FALSE, TRUE))
      })
      
      
      # Test that models can be used to predict survival probabilities.
      if(outcome_type %in% c("survival", "competing_risk")){
        testthat::test_that(paste0("Sample survival predictions for ", outcome_type, " can be made using ", learner, " for a one-feature data set."), {
          # Expect predictions to be made.
          prediction_table <- .predict(model, one_feature_data, type="survival_probability", time=1000)
          
          # Test that the predictions were successfully made.
          expect_equal(any_predictions_valid(prediction_table, outcome_type),
                       ifelse(learner %in% c(except_train, except_predict, except_predict_survival), FALSE, TRUE))
        })
        
        testthat::test_that(paste0("Sample survival predictions for ", outcome_type, " can be made using ", learner, " for a one-feature, one-sample data set."), {
          # Expect predictions to be made.
          prediction_table <- .predict(model, data=one_feature_one_sample_data, type="survival_probability", time=1000)
          
          # Test that the predictions were successfully made.
          expect_equal(any_predictions_valid(prediction_table, outcome_type),
                       ifelse(learner %in% c(except_train, except_predict, except_predict_survival), FALSE, TRUE))
        })
      }
      
    }
    
  }
  
}

library(familiar)

data_good_no_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                      "outcome_event"=c(1, 1, 1, 1, 1),
                                                      "outcome_pred"=c(10, 8, 6, 4, 2))

data_inv_no_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                      "outcome_event"=c(1, 1, 1, 1, 1),
                                                      "outcome_pred"=c(2, 4, 6, 8, 10))

data_bad_no_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                     "outcome_event"=c(1, 1, 1, 1, 1),
                                                     "outcome_pred"=c(5, 5, 5, 5, 5))

data_moderate_no_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                          "outcome_event"=c(1, 1, 1, 1, 1),
                                                          "outcome_pred"=c(10, 6, 8, 2, 4))

data_good_init_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                      "outcome_event"=c(0, 1, 1, 1, 1),
                                                      "outcome_pred"=c(10, 8, 6, 4, 2))

data_inv_init_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                     "outcome_event"=c(0, 1, 1, 1, 1),
                                                     "outcome_pred"=c(2, 4, 6, 8, 10))

data_bad_init_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                     "outcome_event"=c(0, 1, 1, 1, 1),
                                                     "outcome_pred"=c(5, 5, 5, 5, 5))

data_moderate_init_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                          "outcome_event"=c(0, 1, 1, 1, 1),
                                                          "outcome_pred"=c(10, 6, 8, 2, 4))

data_good_end_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                        "outcome_event"=c(1, 1, 1, 1, 0),
                                                        "outcome_pred"=c(10, 8, 6, 4, 2))

data_inv_end_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                       "outcome_event"=c(1, 1, 1, 1, 0),
                                                       "outcome_pred"=c(2, 4, 6, 8, 10))

data_bad_end_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                       "outcome_event"=c(1, 1, 1, 1, 0),
                                                       "outcome_pred"=c(5, 5, 5, 5, 5))

data_moderate_end_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                            "outcome_event"=c(1, 1, 1, 1, 0),
                                                            "outcome_pred"=c(10, 6, 8, 2, 4))

data_good_mid_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                       "outcome_event"=c(1, 1, 0, 0, 1),
                                                       "outcome_pred"=c(10, 8, 6, 4, 2))

data_inv_mid_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                      "outcome_event"=c(1, 1, 0, 0, 1),
                                                      "outcome_pred"=c(2, 4, 6, 8, 10))

data_bad_mid_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                      "outcome_event"=c(1, 1, 0, 0, 1),
                                                      "outcome_pred"=c(5, 5, 5, 5, 5))

data_moderate_mid_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                           "outcome_event"=c(1, 1, 0, 0, 1),
                                                           "outcome_pred"=c(10, 6, 8, 2, 4))

data_all_censoring_risk <- data.table::data.table("outcome_time"=c(1, 2, 3, 4, 5),
                                                  "outcome_event"=c(0, 0, 0, 0, 0),
                                                  "outcome_pred"=c(10, 8, 6, 4, 2))

data_list <- list("good_no_censoring_risk"       = list("data"=data_good_no_censoring_risk),
                  "inv_no_censoring_risk"        = list("data"=data_inv_no_censoring_risk),
                  "bad_no_censoring_risk"        = list("data"=data_bad_no_censoring_risk),
                  "moderate_no_censoring_risk"   = list("data"=data_moderate_no_censoring_risk),
                  "good_init_censoring_risk"     = list("data"=data_good_init_censoring_risk),
                  "inv_init_censoring_risk"      = list("data"=data_inv_init_censoring_risk),
                  "bad_init_censoring_risk"      = list("data"=data_bad_init_censoring_risk),
                  "moderate_init_censoring_risk" = list("data"=data_moderate_init_censoring_risk),
                  "good_end_censoring_risk"      = list("data"=data_good_end_censoring_risk),
                  "inv_end_censoring_risk"       = list("data"=data_inv_end_censoring_risk),
                  "bad_end_censoring_risk"       = list("data"=data_bad_end_censoring_risk),
                  "moderate_end_censoring_risk"  = list("data"=data_moderate_end_censoring_risk),
                  "good_mid_censoring_risk"      = list("data"=data_good_mid_censoring_risk),
                  "inv_mid_censoring_risk"       = list("data"=data_inv_mid_censoring_risk),
                  "bad_mid_censoring_risk"       = list("data"=data_bad_mid_censoring_risk),
                  "moderate_mid_censoring_risk"  = list("data"=data_moderate_mid_censoring_risk),
                  "all_censoring_risk"           = list("data"=data_all_censoring_risk))

##### Test for risk-like predictions
testthat::test_that("Concordance index is correct", {
  
  expected_score     <- c(1.0,  0.0, 0.5, 0.8, 1.0,  0.0, 0.5, 2/3, 1.0,  0.0, 0.5, 0.8, 1.0,  0.0, 0.5, 6/7, NA)
  expected_objective <- c(1.0, -1.0, 0.0, 0.6, 1.0, -1.0, 0.0, 1/3, 1.0, -1.0, 0.0, 0.6, 1.0, -1.0, 0.0, 5/7, NA)
  
  # Iterate over the data sets.
  for(ii in seq_len(length(data_list))){
    
    # Compute the concordance index
    score <- familiar:::metric.concordance_index.calc(dt=data_list[[ii]]$data,
                                                      outcome_type="survival",
                                                      learner="cox")
    
    obj_score <- familiar:::metric.concordance_index.to_objective(score=score)
    
    # Test the values.
    testthat::expect_equal(score, expected_score[ii])
    testthat::expect_equal(obj_score, expected_objective[ii])
  }
})

##### Test for time-like predictions
testthat::test_that("Concordance index is correct", {
  
  expected_score     <- c(1.0,  0.0, 0.5, 0.8, 1.0,  0.0, 0.5, 2/3, 1.0,  0.0, 0.5, 0.8, 1.0,  0.0, 0.5, 6/7, NA)
  expected_objective <- c(1.0, -1.0, 0.0, 0.6, 1.0, -1.0, 0.0, 1/3, 1.0, -1.0, 0.0, 0.6, 1.0, -1.0, 0.0, 5/7, NA)
  
  # Iterate over the data sets.
  for(ii in seq_len(length(data_list))){
    
    data <- data_list[[ii]]$data
    data$outcome_pred <- 10 - data$outcome_pred
    
    # Compute the concordance index
    score <- familiar:::metric.concordance_index.calc(dt=data,
                                                      outcome_type="survival",
                                                      learner="survival_regr")
    
    obj_score <- familiar:::metric.concordance_index.to_objective(score=score)
    
    # Test the values.
    testthat::expect_equal(score, expected_score[ii])
    testthat::expect_equal(obj_score, expected_objective[ii])
  }
})

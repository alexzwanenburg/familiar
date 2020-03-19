library(familiar)

data_good_binomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels=c("a", "b")),
                                             "outcome_pred_class"=factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels=c("a", "b")),
                                             "outcome_pred_prob_a"=c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0),
                                             "outcome_pred_prob_b"=c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1))

data_bad_binomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels=c("a", "b")),
                                            "outcome_pred_class"=factor(c("a", "b", "a", "b", "a", "b", "a", "b", "a", "b"), levels=c("a", "b")),
                                            "outcome_pred_prob_a"=c(.5, .5, .5, .5, .5, .5, .5, .5, .5, .5),
                                            "outcome_pred_prob_b"=c(.5, .5, .5, .5, .5, .5, .5, .5, .5, .5))

data_ok_binomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels=c("a", "b")),
                                            "outcome_pred_class"=factor(c("a", "a", "a", "b", "b", "a", "a", "b", "b", "b"), levels=c("a", "b")),
                                            "outcome_pred_prob_a"=c(1.0, 0.9, 0.8, 0.4, 0.3, 0.7, 0.6, 0.2, 0.1, 0.0),
                                            "outcome_pred_prob_b"=c(0.0, 0.1, 0.2, 0.6, 0.7, 0.3, 0.4, 0.8, 0.9, 1.0))

data_inv_binomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels=c("a", "b")),
                                            "outcome_pred_class"=factor(c("b", "b", "b", "b", "b", "a", "a", "a", "b", "b"), levels=c("a", "b")),
                                            "outcome_pred_prob_a"=c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1),
                                            "outcome_pred_prob_b"=c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0))

data_good_multinomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels=c("a", "b", "c")),
                                                "outcome_pred_class"=factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels=c("a", "b", "c")),
                                                "outcome_pred_prob_a"=c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0),
                                                "outcome_pred_prob_b"=c(0, 0, 0, 1, 1, 1, 0, 0, 0, 0),
                                                "outcome_pred_prob_c"=c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1))

data_bad_multinomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels=c("a", "b", "c")),
                                               "outcome_pred_class"=factor(c("a", "b", "c", "a", "b", "c", "a", "b", "c", "a"), levels=c("a", "b", "c")),
                                               "outcome_pred_prob_a"=c(1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3),
                                               "outcome_pred_prob_b"=c(1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3),
                                               "outcome_pred_prob_c"=c(1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3, 1/3))

data_ok_multinomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels=c("a", "b", "c")),
                                               "outcome_pred_class"=factor(c("a", "b", "a", "b", "a", "b", "c", "b", "c", "c"), levels=c("a", "b", "c")),
                                               "outcome_pred_prob_a"=c(1.0, 0.2, 0.4, 0.0, 0.4, 0.1, 0.0, 0.0, 0.0, 0.1),
                                               "outcome_pred_prob_b"=c(0.0, 0.5, 0.3, 1.0, 0.3, 0.5, 0.0, 0.6, 0.4, 0.3),
                                               "outcome_pred_prob_c"=c(0.0, 0.3, 0.3, 0.0, 0.3, 0.4, 1.0, 0.4, 0.6, 0.7))

data_inv_multinomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels=c("a", "b", "c")),
                                               "outcome_pred_class"=factor(c("b", "b", "b", "c", "c", "c", "a", "a", "a", "a"), levels=c("a", "b", "c")),
                                               "outcome_pred_prob_a"=c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1),
                                               "outcome_pred_prob_b"=c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0),
                                               "outcome_pred_prob_c"=c(0, 0, 0, 1, 1, 1, 0, 0, 0, 0))

# Package data in a list for easier iterative tests
data_list <- list("good_binomial"    = list("data"=data_good_binomial,    "outcome_type"="binomial"),
                  "bad_binomial"     = list("data"=data_bad_binomial,     "outcome_type"="binomial"),
                  "ok_binomial"      = list("data"=data_ok_binomial,      "outcome_type"="binomial"),
                  "inv_binomial"     = list("data"=data_inv_binomial,     "outcome_type"="binomial"),
                  "good_multinomial" = list("data"=data_good_multinomial, "outcome_type"="multinomial"),
                  "bad_multinomial"  = list("data"=data_bad_multinomial,  "outcome_type"="multinomial"),
                  "ok_multinomial"   = list("data"=data_ok_multinomial,   "outcome_type"="multinomial"),
                  "inv_multinomial"  = list("data"=data_inv_multinomial,  "outcome_type"="multinomial"))


##### Area under the curve #####################################################
testthat::test_that("AUC-ROC is correct", {
  
  expected_score     <- c(1.0, 0.5, 21/25, 0.0, 1.0, 0.5, 61/72, 1/3)
  expected_objective <- c(1.0, 0.0, 17/25, -1.0, 1.0, 0.0, 25/36, -1/3)
  
  # Iterate over the data sets.
  for(ii in seq_len(length(data_list))){

    # Compute the AUC.
    score <- familiar:::metric.auc.calc(dt=data_list[[ii]]$data,
                                        outcome_type=data_list[[ii]]$outcome_type,
                                        metric="auc")
    
    obj_score <- familiar:::metric.auc.to_objective(score)
    
    # Test the values.
    testthat::expect_equal(score, expected_score[ii])
    testthat::expect_equal(obj_score, expected_objective[ii])
  }
})

##### Brier score
testthat::test_that("Brier score is correct", {
  
  expected_score     <- c(0.0, 1/4, 0.18, 1.0, 0.0, 1/3, 391/2000, 1.0)
  expected_objective <- c(1.0, 3/4, 0.82, 0.0, 1.0, 2/3, 1609/2000, 0.0)
  
  for(ii in seq_len(length(data_list))){
    
    # Compute the AUC.
    score <- familiar:::metric.brier.calc(dt=data_list[[ii]]$data,
                                          outcome_type=data_list[[ii]]$outcome_type,
                                          metric="brier")
    
    obj_score <- familiar:::metric.brier.to_objective(score)
    
    # Test whether the Brier score is as expected.
    testthat::expect_equal(score, expected_score[ii])
    testthat::expect_equal(obj_score, expected_objective[ii])
  }
  
})

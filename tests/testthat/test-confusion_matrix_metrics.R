library(familiar)

cm_metric_test <- function(data_list, metric, expected_score, averaging="macro"){
  for(ii in seq_len(length(data_list))){
    score <- familiar:::metric.confusion_matrix.calc(data=data_list[[ii]]$data,
                                                     outcome_type=data_list[[ii]]$outcome_type,
                                                     metric=metric,
                                                     averaging=averaging)
    testthat::expect_equal(score, expected_score[ii])
  }
}

# Set up examples
data_good_binomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels=c("a", "b")),
                                             "outcome_pred_class"=factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels=c("a", "b")))

data_bad_binomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels=c("a", "b")),
                                            "outcome_pred_class"=factor(c("b", "b", "b", "b", "b", "a", "a", "a", "a", "a"), levels=c("a", "b")))

data_ok_binomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels=c("a", "b")),
                                           "outcome_pred_class"=factor(c("a", "a", "a", "a", "b", "a", "a", "b", "b", "b"), levels=c("a", "b")))

data_good_multinomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels=c("a", "b", "c")),
                                                "outcome_pred_class"=factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels=c("a", "b", "c")))

data_bad_multinomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels=c("a", "b", "c")),
                                               "outcome_pred_class"=factor(c("c", "c", "c", "a", "a", "a", "b", "b", "b", "b"), levels=c("a", "b", "c")))

data_ok_multinomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels=c("a", "b", "c")),
                                              "outcome_pred_class"=factor(c("a", "a", "b", "a", "b", "c", "c", "c", "c", "c"), levels=c("a", "b", "c")))

# Package data in a list for easier iterative tests
data_list <- list("good_binomial"    = list("data"=data_good_binomial,    "outcome_type"="binomial"),
                  "bad_binomial"     = list("data"=data_bad_binomial,     "outcome_type"="binomial"),
                  "ok_binomial"      = list("data"=data_ok_binomial,      "outcome_type"="binomial"),
                  "good_multinomial" = list("data"=data_good_multinomial, "outcome_type"="multinomial"),
                  "bad_multinomial"  = list("data"=data_bad_multinomial,  "outcome_type"="multinomial"),
                  "ok_multinomial"   = list("data"=data_ok_multinomial,   "outcome_type"="multinomial"))

##### Confusion matrix #########################################################
testthat::test_that("confusion matrix is correct", {
  # Good binomial
  cm_gb <- familiar:::metric.confusion_matrix.get_confusion_matrix(data=data_good_binomial, outcome_type="binomial")
  
  testthat::expect_equal(cm_gb$tp, c(5, 5))
  testthat::expect_equal(cm_gb$tn, c(5, 5))
  testthat::expect_equal(cm_gb$fp, c(0, 0))
  testthat::expect_equal(cm_gb$fn, c(0, 0))
  testthat::expect_equal(cm_gb$prevalence, c(0.5, 0.5))
  testthat::expect_equal(cm_gb$bias, c(0.5, 0.5))
  testthat::expect_equal(cm_gb$n_samples, 10)
  testthat::expect_equal(cm_gb$n_classes, 2)
  
  # Bad binomial
  cm_bb <- familiar:::metric.confusion_matrix.get_confusion_matrix(data=data_bad_binomial, outcome_type="binomial")
  
  testthat::expect_equal(cm_bb$tp, c(0, 0))
  testthat::expect_equal(cm_bb$tn, c(0, 0))
  testthat::expect_equal(cm_bb$fp, c(5, 5))
  testthat::expect_equal(cm_bb$fn, c(5, 5))
  testthat::expect_equal(cm_bb$prevalence, c(0.5, 0.5))
  testthat::expect_equal(cm_bb$bias, c(0.5, 0.5))
  testthat::expect_equal(cm_bb$n_samples, 10)
  testthat::expect_equal(cm_bb$n_classes, 2)
  
  # Ok binomial
  cm_ob <- familiar:::metric.confusion_matrix.get_confusion_matrix(data=data_ok_binomial, outcome_type="binomial")
  
  testthat::expect_equal(cm_ob$tp, c(4, 3))
  testthat::expect_equal(cm_ob$tn, c(3, 4))
  testthat::expect_equal(cm_ob$fp, c(2, 1))
  testthat::expect_equal(cm_ob$fn, c(1, 2))
  testthat::expect_equal(cm_ob$prevalence, c(0.5, 0.5))
  testthat::expect_equal(cm_ob$bias, c(0.6, 0.4))
  testthat::expect_equal(cm_ob$n_samples, 10)
  testthat::expect_equal(cm_ob$n_classes, 2)
  
  # Good multinomial
  cm_gm <- familiar:::metric.confusion_matrix.get_confusion_matrix(data=data_good_multinomial, outcome_type="multinomial")
  
  testthat::expect_equal(cm_gm$tp, c(3, 3, 4))
  testthat::expect_equal(cm_gm$tn, c(7, 7, 6))
  testthat::expect_equal(cm_gm$fp, c(0, 0, 0))
  testthat::expect_equal(cm_gm$fn, c(0, 0, 0))
  testthat::expect_equal(cm_gm$prevalence, c(0.3, 0.3, 0.4))
  testthat::expect_equal(cm_gm$bias, c(0.3, 0.3, 0.4))
  testthat::expect_equal(cm_gm$n_samples, 10)
  testthat::expect_equal(cm_gm$n_classes, 3)
  
  # Bad multinomial
  cm_bm <- familiar:::metric.confusion_matrix.get_confusion_matrix(data=data_bad_multinomial, outcome_type="multinomial")
  
  testthat::expect_equal(cm_bm$tp, c(0, 0, 0))
  testthat::expect_equal(cm_bm$tn, c(4, 3, 3))
  testthat::expect_equal(cm_bm$fp, c(3, 4, 3))
  testthat::expect_equal(cm_bm$fn, c(3, 3, 4))
  testthat::expect_equal(cm_bm$prevalence, c(0.3, 0.3, 0.4))
  testthat::expect_equal(cm_bm$bias, c(0.3, 0.4, 0.3))
  testthat::expect_equal(cm_bm$n_samples, 10)
  testthat::expect_equal(cm_bm$n_classes, 3)
  
  # Ok multinomial
  cm_om <- familiar:::metric.confusion_matrix.get_confusion_matrix(data=data_ok_multinomial, outcome_type="multinomial")
  
  testthat::expect_equal(cm_om$tp, c(2, 1, 4))
  testthat::expect_equal(cm_om$tn, c(6, 6, 5))
  testthat::expect_equal(cm_om$fp, c(1, 1, 1))
  testthat::expect_equal(cm_om$fn, c(1, 2, 0))
  testthat::expect_equal(cm_om$prevalence, c(0.3, 0.3, 0.4))
  testthat::expect_equal(cm_om$bias, c(0.3, 0.2, 0.5))
  testthat::expect_equal(cm_om$n_samples, 10)
  testthat::expect_equal(cm_om$n_classes, 3)
})


##### Accuracy #################################################################
testthat::test_that("accuracy is correct", {
  cm_metric_test(data_list=data_list, metric="accuracy",
                 expected_score = c(1.0, 0.0, 7/10, 1.0, 0.0, 7/10))
})

##### Balanced accuracy ########################################################
testthat::test_that("balanced accuracy is correct", {
  cm_metric_test(data_list=data_list, metric="balanced_accuracy",
                 expected_score = c(1.0, 0.0, 7/10, 1.0, 0.0, 2/3))
})

##### Balanced error rate ######################################################
testthat::test_that("balanced error rate is correct", {
  cm_metric_test(data_list=data_list, metric="balanced_error_rate",
                 expected_score = c(0.0, 1.0, 3/10, 0.0, 1.0, 1/3))
})

##### Cohen's kappa ############################################################
testthat::test_that("Cohen's kappa is correct", {
  cm_metric_test(data_list=data_list, metric="kappa",
                 expected_score = c(1.0, -1.0, 2/5, 1.0, -33/67, 35/65))
})

##### F1 score #################################################################
testthat::test_that("F1-score is correct", {
  # MACRO
  cm_metric_test(data_list=data_list, metric="f1_score", averaging="macro",
                 expected_score = c(1.0, 0.0, 2/3, 1.0, 0.0, 88/135))
  
  # MICRO
  cm_metric_test(data_list=data_list, metric="f1_score", averaging="micro",
                 expected_score = c(1.0, 0.0, 2/3, 1.0, 0.0, 7/10))
  
  # WEIGHTED
  cm_metric_test(data_list=data_list, metric="f1_score", averaging="weighted",
                 expected_score = c(1.0, 0.0, 2/3, 1.0, 0.0, 304/450))
})

##### False detection rate #####################################################
testthat::test_that("False detection rate is correct", {
  # MACRO
  cm_metric_test(data_list=data_list, metric="fdr", averaging="macro",
                 expected_score = c(0.0, 1.0, 1/4, 0.0, 1.0, 31/90))
  
  # MICRO
  cm_metric_test(data_list=data_list, metric="fdr", averaging="micro",
                 expected_score = c(0.0, 1.0, 1/4, 0.0, 1.0, 3/10))
  
  # WEIGHTED
  cm_metric_test(data_list=data_list, metric="fdr", averaging="weighted",
                 expected_score = c(0.0, 1.0, 1/4, 0.0, 1.0, 33/100))
})

##### Informedness #############################################################
testthat::test_that("Informedness is correct", {
  cm_metric_test(data_list=data_list, metric="informedness",
                 expected_score = c(1.0, -1.0, 2/5, 1.0, -1/2, 23/42))
})

##### Markedness ###############################################################
testthat::test_that("Markedness is correct", {
  cm_metric_test(data_list=data_list, metric="markedness",
                 expected_score = c(1.0, -1.0, 5/12, 1.0, -1/2, 17/28))
})

##### Matthews' correlation coefficient ########################################
testthat::test_that("Matthew's correlation coefficient is correct", {
  cm_metric_test(data_list=data_list, metric="mcc",
                 expected_score = c(1.0, -1.0, 1/sqrt(6), 1.0, -1/2, 35/sqrt(4092)))
})

##### Negative predictive value ################################################
testthat::test_that("Negative predictive value is correct", {
  
  # MACRO
  cm_metric_test(data_list=data_list, metric="npv", averaging="macro",
                 expected_score = c(1.0, 0.0, 2/3, 1.0, 1/2, 73/84))
  
  # MICRO
  cm_metric_test(data_list=data_list, metric="npv", averaging="micro",
                 expected_score = c(1.0, 0.0, 2/3, 1.0, 1/2, 17/20))
  
  # WEIGHTED
  cm_metric_test(data_list=data_list, metric="npv", averaging="weighted",
                 expected_score = c(1.0, 0.0, 2/3, 1.0, 69/140, 247/280))
})

##### Positive predictive value ################################################
testthat::test_that("Positive predictive value is correct", {
  
  # MACRO
  cm_metric_test(data_list=data_list, metric="ppv", averaging="macro",
                 expected_score = c(1.0, 0.0, 3/4, 1.0, 0.0, 59/90))
  
  # MICRO
  cm_metric_test(data_list=data_list, metric="ppv", averaging="micro",
                 expected_score = c(1.0, 0.0, 3/4, 1.0, 0.0, 7/10))
  
  # WEIGHTED
  cm_metric_test(data_list=data_list, metric="ppv", averaging="weighted",
                 expected_score = c(1.0, 0.0, 3/4, 1.0, 0.0, 67/100))
})

##### Recall ###################################################################
testthat::test_that("Recall is correct", {
  
  # MACRO
  cm_metric_test(data_list=data_list, metric="recall", averaging="macro",
                 expected_score = c(1.0, 0.0, 3/5, 1.0, 0.0, 2/3))
  
  # MICRO
  cm_metric_test(data_list=data_list, metric="recall", averaging="micro",
                 expected_score = c(1.0, 0.0, 3/5, 1.0, 0.0, 7/10))
  
  # WEIGHTED
  cm_metric_test(data_list=data_list, metric="recall", averaging="weighted",
                 expected_score = c(1.0, 0.0, 3/5, 1.0, 0.0, 7/10))
})

##### Specificity ##############################################################
testthat::test_that("Specificity is correct", {
  
  # MACRO
  cm_metric_test(data_list=data_list, metric="specificity", averaging="macro",
                 expected_score = c(1.0, 0.0, 4/5, 1.0, 1/2, 107/126))
  
  # MICRO
  cm_metric_test(data_list=data_list, metric="specificity", averaging="micro",
                 expected_score = c(1.0, 0.0, 4/5, 1.0, 1/2, 17/20))
  
  # WEIGHTED
  cm_metric_test(data_list=data_list, metric="specificity", averaging="weighted",
                 expected_score = c(1.0, 0.0, 4/5, 1.0, 1/2, 89/105))
})

##### Youden's J statistic #####################################################
testthat::test_that("Youden's J statistic is correct", {
  # MACRO
  cm_metric_test(data_list=data_list, metric="youden_j", averaging="macro",
                 expected_score = c(1.0, -1.0, 2/5, 1.0, -1/2, 65/126))
  
  # MICRO
  cm_metric_test(data_list=data_list, metric="youden_j", averaging="micro",
                 expected_score = c(1.0, -1.0, 2/5, 1.0, -1/2, 11/20))
  
  # WEIGHTED
  cm_metric_test(data_list=data_list, metric="youden_j", averaging="weighted",
                 expected_score = c(1.0, -1.0, 2/5, 1.0, -1/2, 23/42))
})

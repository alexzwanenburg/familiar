cm_metric_test <- function(metric, data_list, baseline_value, expected_score, expected_objective=NULL){
  
  # For several metrics the objective score and the expected score are the same.
  if(is.null(expected_objective)) expected_objective <- expected_score
  
  # Extract the correct expected_score and expected_objective variables.
  if(stringi::stri_endswith_fixed(str=metric, pattern="_macro")){
    expected_score <- expected_score$macro
    expected_objective <- expected_objective$macro
    
  } else if(stringi::stri_endswith_fixed(str=metric, pattern="_micro")){
    expected_score <- expected_score$micro
    expected_objective <- expected_objective$micro
    
  } else if(stringi::stri_endswith_fixed(str=metric, pattern="_weighted")){
    expected_score <- expected_score$weighted
    expected_objective <- expected_objective$weighted
     
  } else if(is.list(expected_score)){
    expected_score <- expected_score$macro
    expected_objective <- expected_objective$macro
  }
  
  for(ii in seq_along(data_list)){
    
    # Create metric object
    metric_object <- familiar:::as_metric(metric=metric,
                                          outcome_type=data_list[[ii]]$outcome_type)
    
    # Set baseline-value explicitly.
    metric_object@baseline_value <- baseline_value
    
    # Check that the metric is available
    testthat::expect_equal(familiar:::is_available(metric_object), TRUE)
    
    # Compute the metric value.
    score <- familiar:::compute_metric_score(metric=metric_object,
                                             data=data_list[[ii]]$data)
    
    # Compute the objective score.
    objective_score <- familiar:::compute_objective_score(metric=metric_object,
                                                          data=data_list[[ii]]$data)
    
    # Test the values.
    testthat::expect_equal(score, expected_score[ii])
    testthat::expect_equal(objective_score, expected_objective[ii])
  }
}

# Set up examples
data_good_binomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels=c("a", "b")),
                                             "predicted_class"=factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels=c("a", "b")))

data_bad_binomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels=c("a", "b")),
                                            "predicted_class"=factor(c("b", "b", "b", "b", "b", "a", "a", "a", "a", "a"), levels=c("a", "b")))

data_ok_binomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "a", "a", "b", "b", "b", "b", "b"), levels=c("a", "b")),
                                           "predicted_class"=factor(c("a", "a", "a", "a", "b", "a", "a", "b", "b", "b"), levels=c("a", "b")))

data_good_multinomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels=c("a", "b", "c")),
                                                "predicted_class"=factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels=c("a", "b", "c")))

data_bad_multinomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels=c("a", "b", "c")),
                                               "predicted_class"=factor(c("c", "c", "c", "a", "a", "a", "b", "b", "b", "b"), levels=c("a", "b", "c")))

data_ok_multinomial <- data.table::data.table("outcome"=factor(c("a", "a", "a", "b", "b", "b", "c", "c", "c", "c"), levels=c("a", "b", "c")),
                                              "predicted_class"=factor(c("a", "a", "b", "a", "b", "c", "c", "c", "c", "c"), levels=c("a", "b", "c")))

# Package data in a list for easier iterative tests
data_list <- list("good_binomial"    = list("data"=data_good_binomial,    "outcome_type"="binomial"),
                  "bad_binomial"     = list("data"=data_bad_binomial,     "outcome_type"="binomial"),
                  "ok_binomial"      = list("data"=data_ok_binomial,      "outcome_type"="binomial"),
                  "good_multinomial" = list("data"=data_good_multinomial, "outcome_type"="multinomial"),
                  "bad_multinomial"  = list("data"=data_bad_multinomial,  "outcome_type"="multinomial"),
                  "ok_multinomial"   = list("data"=data_ok_multinomial,   "outcome_type"="multinomial"))

familiar:::test_all_metrics_available(metrics=familiar:::.get_available_confusion_matrix_metrics())

# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

##### Confusion matrix #########################################################
testthat::test_that("confusion matrix is correct", {
  # Good binomial
  cm_gb <- familiar:::..compute_confusion_matrix_data(data=data_good_binomial, outcome_type="binomial")
  
  testthat::expect_equal(cm_gb$tp, c(5, 5))
  testthat::expect_equal(cm_gb$tn, c(5, 5))
  testthat::expect_equal(cm_gb$fp, c(0, 0))
  testthat::expect_equal(cm_gb$fn, c(0, 0))
  testthat::expect_equal(cm_gb$prevalence, c(0.5, 0.5))
  testthat::expect_equal(cm_gb$bias, c(0.5, 0.5))
  testthat::expect_equal(cm_gb$n_samples, 10)
  testthat::expect_equal(cm_gb$n_classes, 2)
  
  # Bad binomial
  cm_bb <- familiar:::..compute_confusion_matrix_data(data=data_bad_binomial, outcome_type="binomial")
  
  testthat::expect_equal(cm_bb$tp, c(0, 0))
  testthat::expect_equal(cm_bb$tn, c(0, 0))
  testthat::expect_equal(cm_bb$fp, c(5, 5))
  testthat::expect_equal(cm_bb$fn, c(5, 5))
  testthat::expect_equal(cm_bb$prevalence, c(0.5, 0.5))
  testthat::expect_equal(cm_bb$bias, c(0.5, 0.5))
  testthat::expect_equal(cm_bb$n_samples, 10)
  testthat::expect_equal(cm_bb$n_classes, 2)
  
  # Ok binomial
  cm_ob <- familiar:::..compute_confusion_matrix_data(data=data_ok_binomial, outcome_type="binomial")
  
  testthat::expect_equal(cm_ob$tp, c(4, 3))
  testthat::expect_equal(cm_ob$tn, c(3, 4))
  testthat::expect_equal(cm_ob$fp, c(2, 1))
  testthat::expect_equal(cm_ob$fn, c(1, 2))
  testthat::expect_equal(cm_ob$prevalence, c(0.5, 0.5))
  testthat::expect_equal(cm_ob$bias, c(0.6, 0.4))
  testthat::expect_equal(cm_ob$n_samples, 10)
  testthat::expect_equal(cm_ob$n_classes, 2)
  
  # Good multinomial
  cm_gm <- familiar:::..compute_confusion_matrix_data(data=data_good_multinomial, outcome_type="multinomial")
  
  testthat::expect_equal(cm_gm$tp, c(3, 3, 4))
  testthat::expect_equal(cm_gm$tn, c(7, 7, 6))
  testthat::expect_equal(cm_gm$fp, c(0, 0, 0))
  testthat::expect_equal(cm_gm$fn, c(0, 0, 0))
  testthat::expect_equal(cm_gm$prevalence, c(0.3, 0.3, 0.4))
  testthat::expect_equal(cm_gm$bias, c(0.3, 0.3, 0.4))
  testthat::expect_equal(cm_gm$n_samples, 10)
  testthat::expect_equal(cm_gm$n_classes, 3)
  
  # Bad multinomial
  cm_bm <- familiar:::..compute_confusion_matrix_data(data=data_bad_multinomial, outcome_type="multinomial")
  
  testthat::expect_equal(cm_bm$tp, c(0, 0, 0))
  testthat::expect_equal(cm_bm$tn, c(4, 3, 3))
  testthat::expect_equal(cm_bm$fp, c(3, 4, 3))
  testthat::expect_equal(cm_bm$fn, c(3, 3, 4))
  testthat::expect_equal(cm_bm$prevalence, c(0.3, 0.3, 0.4))
  testthat::expect_equal(cm_bm$bias, c(0.3, 0.4, 0.3))
  testthat::expect_equal(cm_bm$n_samples, 10)
  testthat::expect_equal(cm_bm$n_classes, 3)
  
  # Ok multinomial
  cm_om <- familiar:::..compute_confusion_matrix_data(data=data_ok_multinomial, outcome_type="multinomial")
  
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
familiar:::test_all_metrics(metrics=familiar:::.get_available_accuracy_metrics(),
                            except_one_sample=FALSE,
                            except_identical=FALSE)

testthat::test_that("accuracy is correct", {
  for(metric in familiar:::.get_available_accuracy_metrics()){
    cm_metric_test(data_list = data_list,
                   metric = metric,
                   baseline_value = 0.0,
                   expected_score = c(1.0, 0.0, 7/10, 1.0, 0.0, 7/10))
  }
})

##### Balanced accuracy ########################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_balanced_accuracy_metrics(),
                            except_one_sample=TRUE,
                            except_identical=TRUE)

testthat::test_that("balanced accuracy is correct", {
  for(metric in familiar:::.get_available_balanced_accuracy_metrics()){
    cm_metric_test(data_list = data_list,
                   metric = metric,
                   baseline_value = 0.0,
                   expected_score = c(1.0, 0.0, 7/10, 1.0, 0.0, 2/3))
  }
})

##### Balanced error rate ######################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_balanced_error_rate_metrics(),
                            except_one_sample=TRUE,
                            except_identical=TRUE)

testthat::test_that("balanced error rate is correct", {
  for(metric in familiar:::.get_available_balanced_error_rate_metrics()){
    cm_metric_test(data_list = data_list,
                   metric = metric,
                   baseline_value = 1.0,
                   expected_score = c(0.0, 1.0, 3/10, 0.0, 1.0, 1/3),
                   expected_objective = c(1.0, 0.0, 7/10, 1.0, 0.0, 2/3))
  }
})

##### Cohen's kappa ############################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_cohen_kappa_metrics(),
                            except_one_sample=FALSE,
                            except_identical=FALSE)

testthat::test_that("Cohen's kappa is correct", {
  for(metric in familiar:::.get_available_cohen_kappa_metrics()){
    cm_metric_test(data_list = data_list,
                   metric = metric,
                   baseline_value = 0.0,
                   expected_score = c(1.0, -1.0, 2/5, 1.0, -33/67, 35/65))
  }
})

##### F1 score #################################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_f1_score_metrics(),
                            except_one_sample="binomial",
                            except_identical="binomial")

testthat::test_that("F1-score is correct", {
  for(metric in familiar:::.get_available_f1_score_metrics()){
    cm_metric_test(data_list = data_list,
                   metric = metric,
                   baseline_value = 0.0,
                   expected_score = list("macro"=c(1.0, 0.0, 2/3, 1.0, 0.0, 88/135),
                                         "micro"=c(1.0, 0.0, 2/3, 1.0, 0.0, 7/10),
                                         "weighted"=c(1.0, 0.0, 2/3, 1.0, 0.0, 304/450)))
  }
})

##### False detection rate #####################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_fdr_metrics(),
                            except_one_sample="binomial",
                            except_identical="binomial",
                            except_same_prediction="binomial")

testthat::test_that("False detection rate is correct", {
  for(metric in familiar:::.get_available_fdr_metrics()){
    cm_metric_test(data_list = data_list,
                   metric = metric,
                   baseline_value = 1.0,
                   expected_score = list("macro"=c(0.0, 1.0, 1/4, 0.0, 1.0, 31/90),
                                         "micro"=c(0.0, 1.0, 1/4, 0.0, 1.0, 3/10),
                                         "weighted"=c(0.0, 1.0, 1/4, 0.0, 1.0, 33/100)),
                   expected_objective = list("macro"=c(1.0, 0.0, 3/4, 1.0, 0.0, 59/90),
                                             "micro"=c(1.0, 0.0, 3/4, 1.0, 0.0, 7/10),
                                             "weighted"=c(1.0, 0.0, 3/4, 1.0, 0.0, 67/100)))
  }
})

##### Informedness #############################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_informedness_metrics(),
                            except_one_sample=TRUE,
                            except_identical=TRUE)

testthat::test_that("Informedness is correct", {
  for(metric in familiar:::.get_available_informedness_metrics()){
    cm_metric_test(data_list = data_list,
                   metric = metric,
                   baseline_value = 0.0,
                   expected_score = c(1.0, -1.0, 2/5, 1.0, -1/2, 23/42))
  }
})

##### Markedness ###############################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_markedness_metrics(),
                            except_one_sample=TRUE,
                            except_identical=TRUE,
                            except_same_prediction=TRUE)

testthat::test_that("Markedness is correct", {
  for(metric in familiar:::.get_available_markedness_metrics()){
    cm_metric_test(data_list = data_list,
                   metric = metric,
                   baseline_value = 0.0,
                   expected_score = c(1.0, -1.0, 5/12, 1.0, -1/2, 17/28))
  }
})

##### Matthews' correlation coefficient ########################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_mcc_metrics())

testthat::test_that("Matthew's correlation coefficient is correct", {
  for(metric in familiar:::.get_available_mcc_metrics()){
    cm_metric_test(data_list = data_list,
                   metric = metric,
                   baseline_value = 0.0,
                   expected_score = c(1.0, -1.0, 1/sqrt(6), 1.0, -1/2, 35/sqrt(4092)))
  }
})

##### Negative predictive value ################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_npv_metrics())

testthat::test_that("Negative predictive value is correct", {
  for(metric in familiar:::.get_available_npv_metrics()){
    cm_metric_test(data_list = data_list,
                   metric = metric,
                   baseline_value = 0.0,
                   expected_score = list("macro"=c(1.0, 0.0, 2/3, 1.0, 1/2, 73/84),
                                         "micro"=c(1.0, 0.0, 2/3, 1.0, 1/2, 17/20),
                                         "weighted"=c(1.0, 0.0, 2/3, 1.0, 69/140, 247/280)))
  }
})

##### Positive predictive value ################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_ppv_metrics(),
                            except_one_sample="binomial",
                            except_identical="binomial",
                            except_same_prediction="binomial")

testthat::test_that("Positive predictive value is correct", {
  for(metric in familiar:::.get_available_ppv_metrics()){
    cm_metric_test(data_list = data_list,
                   metric = metric,
                   baseline_value = 0.0,
                   expected_score = list("macro"=c(1.0, 0.0, 3/4, 1.0, 0.0, 59/90),
                                         "micro"=c(1.0, 0.0, 3/4, 1.0, 0.0, 7/10),
                                         "weighted"=c(1.0, 0.0, 3/4, 1.0, 0.0, 67/100)))
  }
})

##### Recall ###################################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_sensitivity_metrics(),
                            except_one_sample="binomial",
                            except_identical="binomial")

testthat::test_that("Recall is correct", {
  for(metric in familiar:::.get_available_sensitivity_metrics()){
    cm_metric_test(data_list = data_list,
                   metric = metric,
                   baseline_value = 0.0,
                   expected_score = list("macro"=c(1.0, 0.0, 3/5, 1.0, 0.0, 2/3),
                                         "micro"=c(1.0, 0.0, 3/5, 1.0, 0.0, 7/10),
                                         "weighted"=c(1.0, 0.0, 3/5, 1.0, 0.0, 7/10)))
  }
})

##### Specificity ##############################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_specificity_metrics())

testthat::test_that("Specificity is correct", {
  for(metric in familiar:::.get_available_specificity_metrics()){
    cm_metric_test(data_list = data_list,
                   metric = metric,
                   baseline_value = 0.0,
                   expected_score = list("macro"=c(1.0, 0.0, 4/5, 1.0, 1/2, 107/126),
                                         "micro"=c(1.0, 0.0, 4/5, 1.0, 1/2, 17/20),
                                         "weighted"=c(1.0, 0.0, 4/5, 1.0, 1/2, 89/105)))
  }
})

##### Youden's J statistic #####################################################
# Test results depend on averaging method.
metric_youden <- familiar:::.get_available_youden_metrics()
metric_non_micro <- metric_youden[!stringi::stri_endswith_fixed(str=metric_youden, pattern="_micro")]
metric_micro <- metric_youden[stringi::stri_endswith_fixed(str=metric_youden, pattern="_micro")]

familiar:::test_all_metrics(metrics=metric_non_micro,
                            except_one_sample=TRUE,
                            except_identical=TRUE)

familiar:::test_all_metrics(metrics=metric_micro,
                            except_one_sample="binomial",
                            except_identical="binomial")

testthat::test_that("Youden's J statistic is correct", {
  for(metric in familiar:::.get_available_youden_metrics()){
    cm_metric_test(data_list = data_list,
                   metric = metric,
                   baseline_value = 0.0,
                   expected_score = list("macro"=c(1.0, -1.0, 2/5, 1.0, -1/2, 65/126),
                                         "micro"=c(1.0, -1.0, 2/5, 1.0, -1/2, 11/20),
                                         "weighted"=c(1.0, -1.0, 2/5, 1.0, -1/2, 23/42)))
  }
})

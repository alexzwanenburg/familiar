regr_metric_test <- function(metric, data_list, expected_score, expected_objective=NULL){
  
  # For several metrics the objective score and the expected score are the same.
  if(is.null(expected_objective)) expected_objective <- expected_score
  
  # Create metric object
  metric_object <- familiar:::as_metric(metric=metric,
                                        outcome_type="continuous")
  
  # Set baseline-value explicitly.
  metric_object@baseline_value <- familiar:::compute_metric_score(metric=metric_object,
                                                                  data=data_list[["no_slope"]]$data)
  
  for(ii in seq_along(data_list)){
    
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


good_data <- data.table::data.table("outcome"=c(1, 2, 3, 4, 5),
                                    "predicted_outcome"=c(1, 2, 3, 4, 5))

bad_data <- data.table::data.table("outcome"=c(1, 2, 3, 4, 5),
                                   "predicted_outcome"=c(5, 4, 3, 2, 1))

no_slope_data <- data.table::data.table("outcome"=c(1, 2, 3, 4, 5),
                                        "predicted_outcome"=c(3, 3, 3, 3, 3))

bias_offset_data <- data.table::data.table("outcome"=c(1, 2, 3, 4, 5),
                                           "predicted_outcome"=c(0, 1, 2, 3, 4))

data_list <- list("good" = list("data"=good_data),
                  "bad" = list("data"=bad_data),
                  "no_slope" = list("data"=no_slope_data),
                  "bias_offset" = list("data"=bias_offset_data))


familiar:::test_all_metrics_available(metrics=familiar:::.get_available_regression_metrics())

# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

#### Mean absolute error #######################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_mae_metrics(),
                            not_available_single_sample=FALSE,
                            not_available_all_samples_identical=FALSE)

testthat::test_that("Mean absolute error is correct", {
  for(metric in familiar:::.get_available_mae_metrics()){
    regr_metric_test(data_list = data_list,
                     metric = metric,
                     expected_score = c(0.0, 12/5, 6/5, 1.0),
                     expected_objective = c(1.0, -1.0, 0.0, 1/6))
  }
})



##### Relative absolute error ##################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_rae_metrics(),
                            not_available_single_sample=TRUE,
                            not_available_all_samples_identical=TRUE)

testthat::test_that("Relative absolute error is correct", {
  for(metric in familiar:::.get_available_rae_metrics()){
    regr_metric_test(data_list = data_list,
                     metric = metric,
                     expected_score = c(0.0, 2.0, 1.0, 5/6),
                     expected_objective = c(1.0, -1.0, 0.0, 1/6))
  }
})


##### Mean log absolute error ##################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_mlae_metrics(),
                            not_available_single_sample=FALSE,
                            not_available_all_samples_identical=FALSE)

testthat::test_that("Mean log absolute error is correct", {
  for(metric in familiar:::.get_available_mlae_metrics()){
    regr_metric_test(data_list = data_list,
                     metric = metric,
                     expected_score = c(0.0,
                                        2/5*log(5) + 2/5*log(3),
                                        2/5*log(3) + 2/5*log(2),
                                        log(2)),
                     expected_objective = c(1.0,
                                            1 - (2/5*log(5) + 2/5*log(3)) / (2/5*log(3) + 2/5*log(2)),
                                            0.0,
                                            1 - (log(2)) / (2/5*log(3) + 2/5*log(2))))
  }
})


##### Mean squared error #######################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_mse_metrics(),
                            not_available_single_sample=FALSE,
                            not_available_all_samples_identical=FALSE)

testthat::test_that("Mean squared error is correct", {
  for(metric in familiar:::.get_available_mse_metrics()){
    regr_metric_test(data_list = data_list,
                     metric = metric,
                     expected_score = c(0.0, 8.0, 2.0, 1.0),
                     expected_objective = c(1.0, -1.0, 0.0, 1/2))
  }
})



##### Relative squared error ###################################################
familiar:::test_all_metrics(metrics=familiar:::.get_available_rse_metrics(),
                            not_available_single_sample=TRUE,
                            not_available_all_samples_identical=TRUE)

testthat::test_that("Relative squared error is correct", {
  for(metric in familiar:::.get_available_rse_metrics()){
    regr_metric_test(data_list = data_list,
                     metric = metric,
                     expected_score = c(0.0, 4.0, 1.0, 1/2),
                     expected_objective = c(1.0, -1.0, 0.0, 1/2))
  }
})



##### Mean squared log error ###############################
familiar:::test_all_metrics(metrics=familiar:::.get_available_msle_metrics(),
                            not_available_single_sample=FALSE,
                            not_available_all_samples_identical=FALSE)

testthat::test_that("Mean squared log error is correct", {
  
  expected_score <- c(0.0,
                      (log(2/6)^2 + log(3/5)^2 + log(5/3)^2 + log(6/2)^2) / 5,
                      (log(2/4)^2 + log(3/4)^2 + log(5/4)^2 + log(6/4)^2) / 5,
                      (log(2/1)^2 + log(3/2)^2 + log(4/3)^2 + log(5/4)^2 + log(6/5)^2) / 5)
  expected_objective <- c(1.0, -1.0, 0.0, 1 - expected_score[4]/expected_score[3])
  
  for(metric in familiar:::.get_available_msle_metrics()){
    regr_metric_test(data_list = data_list,
                     metric = metric,
                     expected_score = expected_score,
                     expected_objective = expected_objective)
  }
})


##### Median absolute error ###############################
familiar:::test_all_metrics(metrics=familiar:::.get_available_medea_metrics(),
                            not_available_single_sample=FALSE,
                            not_available_all_samples_identical=FALSE)

testthat::test_that("Median absolute error is correct", {
  for(metric in familiar:::.get_available_medea_metrics()){
    regr_metric_test(data_list = data_list,
                     metric = "median_absolute_error",
                     expected_score = c(0.0, 2.0, 1.0, 1.0),
                     expected_objective = c(1.0, -1.0, 0.0, 0.0))
  }
})


##### Root mean square error ###############################
familiar:::test_all_metrics(metrics=familiar:::.get_available_rmse_metrics(),
                            not_available_single_sample=FALSE,
                            not_available_all_samples_identical=FALSE)

testthat::test_that("Root mean square error is correct", {
  for(metric in familiar:::.get_available_rmse_metrics()){
    regr_metric_test(data_list = data_list,
                     metric = metric,
                     expected_score = c(0.0, 2*sqrt(2), sqrt(2), 1.0),
                     expected_objective = c(1.0, -1.0, 0.0, 1-1/sqrt(2)))
  }
})


##### Root relative squared error ###########################
familiar:::test_all_metrics(metrics=familiar:::.get_available_rrse_metrics(),
                            not_available_single_sample=TRUE,
                            not_available_all_samples_identical=TRUE)

testthat::test_that("Root relative squared error is correct", {
  for(metric in familiar:::.get_available_rrse_metrics()){
    regr_metric_test(data_list = data_list,
                     metric = metric,
                     expected_score = c(0.0, 2.0, 1.0, 1.0/sqrt(2)),
                     expected_objective = c(1.0, -1.0, 0.0, 1-1/sqrt(2)))
  }
})




##### Root mean square log error ###############################
familiar:::test_all_metrics(metrics=familiar:::.get_available_rmsle_metrics(),
                            not_available_single_sample=FALSE,
                            not_available_all_samples_identical=FALSE)

testthat::test_that("Root mean square log error is correct", {
  
  expected_score <- sqrt(c(0.0,
                           (log(2/6)^2 + log(3/5)^2 + log(5/3)^2 + log(6/2)^2) / 5,
                           (log(2/4)^2 + log(3/4)^2 + log(5/4)^2 + log(6/4)^2) / 5,
                           (log(2/1)^2 + log(3/2)^2 + log(4/3)^2 + log(5/4)^2 + log(6/5)^2) / 5))
  
  expected_objective <- c(1.0,
                          1.0 - expected_score[2]/expected_score[3],
                          0.0,
                          1.0 - expected_score[4]/expected_score[3])
  
  for(metric in familiar:::.get_available_rmsle_metrics()){
    regr_metric_test(data_list = data_list,
                     metric = metric,
                     expected_score = expected_score,
                     expected_objective = expected_objective)
  }
})


##### Explained variance ###############################
familiar:::test_all_metrics(metrics=familiar:::.get_available_explained_variance_metrics(),
                            not_available_single_sample=FALSE,
                            not_available_all_samples_identical=FALSE)

testthat::test_that("Explained variance is correct", {
  for(metric in familiar:::.get_available_explained_variance_metrics()){
    regr_metric_test(data_list = data_list,
                     metric = metric,
                     expected_score = c(1.0, -3.0, 0.0, 1.0),
                     expected_objective = c(1.0, -1.0, 0.0, 1.0))
  }
})


##### R2 score ###############################
familiar:::test_all_metrics(metrics=familiar:::.get_available_r_squared_metrics(),
                            not_available_single_sample=TRUE,
                            not_available_all_samples_identical=TRUE)

testthat::test_that("R2 score is correct", {
  for(metric in familiar:::.get_available_r_squared_metrics()){
    regr_metric_test(data_list = data_list,
                     metric = metric,
                     expected_score = c(1.0, -3.0, 0.0, 0.5),
                     expected_objective = c(1.0, -1.0, 0.0, 0.5))
  }
})

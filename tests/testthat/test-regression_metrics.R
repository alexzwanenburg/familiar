regr_metric_test <- function(data_list, metric, expected_score, expected_objective){
  
  # Compute the expected score for an mean intercept only model.
  score_mean <- familiar:::metric.regression.calc(data_list[["no_slope"]]$data, metric=metric, outcome_type="continuous")
  
  # Iterate over the data sets.
  for(ii in seq_along(data_list)){
    
    score <- familiar:::metric.regression.calc(data_list[[ii]]$data, metric=metric, outcome_type="continuous")
    obj_score <- familiar:::metric.regression.to_objective(metric=metric, score=score,
                                                           score_mean=score_mean, outcome_type="continuous")
    
    # Test the values.
    testthat::expect_equal(score, expected_score[ii])
    testthat::expect_equal(obj_score, expected_objective[ii])
  }
}


good_data <- data.table::data.table("outcome"=c(1, 2, 3, 4, 5),
                                    "outcome_pred"=c(1, 2, 3, 4, 5))

bad_data <- data.table::data.table("outcome"=c(1, 2, 3, 4, 5),
                                   "outcome_pred"=c(5, 4, 3, 2, 1))

no_slope_data <- data.table::data.table("outcome"=c(1, 2, 3, 4, 5),
                                        "outcome_pred"=c(3, 3, 3, 3, 3))

bias_offset_data <- data.table::data.table("outcome"=c(1, 2, 3, 4, 5),
                                           "outcome_pred"=c(0, 1, 2, 3, 4))

data_list <- list("good" = list("data"=good_data),
                  "bad" = list("data"=bad_data),
                  "no_slope" = list("data"=no_slope_data),
                  "bias_offset" = list("data"=bias_offset_data))


#### Mean absolute error ###############################
testthat::test_that("Mean absolute error is correct", {
  
  expected_score     <- c(0.0, 12/5, 6/5, 1.0)
  expected_objective <- c(1.0, -1.0, 0.0, 1/6)

  regr_metric_test(data_list=data_list, metric="mean_absolute_error",
                   expected_score=expected_score, expected_objective=expected_objective)
})


##### Mean log absolute error ###############################
testthat::test_that("Mean log absolute error is correct", {
  
  expected_score     <- c(0.0,
                          2/5*log(5) + 2/5*log(3),
                          2/5*log(3) + 2/5*log(2),
                          log(2))
  expected_objective <- c(1.0,
                          1 - (2/5*log(5) + 2/5*log(3)) / (2/5*log(3) + 2/5*log(2)),
                          0.0,
                          1 - (log(2)) / (2/5*log(3) + 2/5*log(2)))
  
  regr_metric_test(data_list=data_list, metric="mean_log_absolute_error",
                   expected_score=expected_score, expected_objective=expected_objective)
})


##### Mean squared error ###############################
testthat::test_that("Mean squared error is correct", {
  
  expected_score     <- c(0.0, 8.0, 2.0, 1.0)
  expected_objective <- c(1.0, -1.0, 0.0, 1/2)
  
  regr_metric_test(data_list=data_list, metric="mean_squared_error",
                   expected_score=expected_score, expected_objective=expected_objective)
})

##### Mean squared log error ###############################
testthat::test_that("Mean squared log error is correct", {
  
  expected_score <- c(0.0,
                      (log(2/6)^2 + log(3/5)^2 + log(5/3)^2 + log(6/2)^2) / 5,
                      (log(2/4)^2 + log(3/4)^2 + log(5/4)^2 + log(6/4)^2) / 5,
                      (log(2/1)^2 + log(3/2)^2 + log(4/3)^2 + log(5/4)^2 + log(6/5)^2) / 5)
  expected_objective <- c(1.0, -1.0, 0.0, 1 - expected_score[4]/expected_score[3])
  
  regr_metric_test(data_list=data_list, metric="mean_squared_log_error",
                   expected_score=expected_score, expected_objective=expected_objective)
})

##### Median absolute error ###############################
testthat::test_that("Median absolute error is correct", {
  
  expected_score     <- c(0.0, 2.0, 1.0, 1.0)
  expected_objective <- c(1.0, -1.0, 0.0, 0.0)
  
  regr_metric_test(data_list=data_list, metric="median_absolute_error",
                   expected_score=expected_score, expected_objective=expected_objective)
})

##### Median absolute error ###############################
testthat::test_that("Root mean square error is correct", {
  
  expected_score     <- c(0.0, 2*sqrt(2), sqrt(2), 1.0)
  expected_objective <- c(1.0, -1.0, 0.0, 1-1/sqrt(2))
  
  regr_metric_test(data_list=data_list, metric="rmse",
                   expected_score=expected_score, expected_objective=expected_objective)
})

##### Explained variance ###############################
testthat::test_that("Explained variance is correct", {
  
  expected_score     <- c(1.0, -3.0, 0.0, 1.0)
  expected_objective <- c(1.0, -1.0, 0.0, 1.0)
  
  regr_metric_test(data_list=data_list, metric="explained_variance",
                   expected_score=expected_score, expected_objective=expected_objective)
})


##### R2 score ###############################
testthat::test_that("R2 score is correct", {
  
  expected_score     <- c(1.0, -3.0, 0.0, 0.5)
  expected_objective <- c(1.0, -1.0, 0.0, 0.5)
  
  regr_metric_test(data_list=data_list, metric="r2_score",
                   expected_score=expected_score, expected_objective=expected_objective)
})

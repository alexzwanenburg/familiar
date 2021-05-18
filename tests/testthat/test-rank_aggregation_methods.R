data <- list(data.table::data.table("score"=c(1.0, 0.8, 0.6, 0.4, 0.2),
                                    "name"=c("a", "b", "c", "d", "e"),
                                    "rank"=c(1L, 2L, 3L, 4L, 5L),
                                    "run_id"=1L),
             data.table::data.table("score"=c(0.8, 1.0, 0.6),
                                    "name"=c("a", "b", "c"),
                                    "rank"=c(2L, 1L, 3L),
                                    "run_id"=2L),
             data.table::data.table("score"=c(0.6, 1.0),
                                    "name"=c("a", "e"),
                                    "rank"=c(2L, 1L),
                                    "run_id"=3L))

data <- data.table::rbindlist(data)

##### Mean rank aggregation ###############################
testthat::test_that("Mean rank aggregation is correct", {
  expected_rank_table <- data.table::data.table("name"=c("a", "b", "c", "d", "e"),
                                                "aggr_score"=c(5/3, 3/2, 3, 4, 3),
                                                "aggr_rank"=c(2, 1, 3, 5, 3))
  
  aggregated_rank_table <- familiar:::rank.aggregate_feature_ranks(data, 3L, "mean")

  testthat::expect_equal(aggregated_rank_table, expected_rank_table)
})

##### Median rank aggregation ###############################
testthat::test_that("Median rank aggregation is correct", {
  expected_rank_table <- data.table::data.table("name"=c("a", "b", "c", "d", "e"),
                                                "aggr_score"=c(2, 3/2, 3, 4, 3),
                                                "aggr_rank"=c(2, 1, 3, 5, 3))
  
  aggregated_rank_table <- familiar:::rank.aggregate_feature_ranks(data, 3L, "median")
  
  testthat::expect_equal(aggregated_rank_table, expected_rank_table)
})

##### Best rank aggregation ###############################
testthat::test_that("Best rank aggregation is correct", {
  expected_rank_table <- data.table::data.table("name"=c("a", "b", "c", "d", "e"),
                                                "aggr_score"=c(1, 1, 3, 4, 1),
                                                "aggr_rank"=c(1, 1, 4, 5, 1))
  
  aggregated_rank_table <- familiar:::rank.aggregate_feature_ranks(data, 3L, "best")
  
  testthat::expect_equal(aggregated_rank_table, expected_rank_table)
})

##### Worst rank aggregation ###############################
testthat::test_that("Worst rank aggregation is correct", {
  expected_rank_table <- data.table::data.table("name"=c("a", "b", "c", "d", "e"),
                                                "aggr_score"=c(2, 2, 3, 4, 5),
                                                "aggr_rank"=c(1, 1, 3, 4, 5))
  
  aggregated_rank_table <- familiar:::rank.aggregate_feature_ranks(data, 3L, "worst")
  
  testthat::expect_equal(aggregated_rank_table, expected_rank_table)
})

##### Stability rank aggregation ###############################
testthat::test_that("Stability rank aggregation is correct", {
  expected_rank_table <- data.table::data.table("name"=c("a", "b", "c", "d", "e"),
                                                "aggr_score"=c(1, 2/3, 2/3, 0, 1/3),
                                                "aggr_rank"=c(1, 2, 2, 5, 4))
  
  aggregated_rank_table <- familiar:::rank.aggregate_feature_ranks(data, 3L, "stability")
  
  testthat::expect_equal(aggregated_rank_table, expected_rank_table)
})

##### Exponential rank aggregation ###############################
testthat::test_that("Exponential rank aggregation is correct", {
  expected_rank_table <- data.table::data.table("name"=c("a", "b", "c", "d", "e"),
                                                "aggr_score"=c(exp(-1/3) + 2*exp(-2/3),
                                                               exp(-2/3) + exp(-1/3),
                                                               2*exp(-1),
                                                               0.0,
                                                               exp(-1/3)),
                                                "aggr_rank"=c(1, 2, 3, 5, 4))
  
  aggregated_rank_table <- familiar:::rank.aggregate_feature_ranks(data, 3L, "exponential")
  
  testthat::expect_equal(aggregated_rank_table, expected_rank_table)
})

##### Borda aggregation ###############################
testthat::test_that("Borda rank aggregation is correct", {
  expected_rank_table <- data.table::data.table("name"=c("a", "b", "c", "d", "e"),
                                                "aggr_score"=c(13/6, 9/5, 14/15, 2/5, 6/5),
                                                "aggr_rank"=c(1, 2, 4, 5, 3))
  
  aggregated_rank_table <- familiar:::rank.aggregate_feature_ranks(data, 3L, "borda")
  
  testthat::expect_equal(aggregated_rank_table, expected_rank_table)
})

##### Enhanced borda aggregation ###############################
testthat::test_that("Enhanced borda rank aggregation is correct", {
  expected_rank_table <- data.table::data.table("name"=c("a", "b", "c", "d", "e"),
                                                "aggr_score"=c(13/6, 6/5, 28/45, 0, 2/5),
                                                "aggr_rank"=c(1, 2, 3, 5, 4))
  
  aggregated_rank_table <- familiar:::rank.aggregate_feature_ranks(data, 3L, "enhanced_borda")
  
  testthat::expect_equal(aggregated_rank_table, expected_rank_table, ignore_attr=TRUE)
})

##### Truncated borda aggregation ###############################
testthat::test_that("Truncated borda rank aggregation is correct", {
  expected_rank_table <- data.table::data.table("name"=c("a", "b", "c", "d", "e"),
                                                "aggr_score"=c(7/3, 5/3, 2/3, 0, 1),
                                                "aggr_rank"=c(1, 2, 4, 5, 3))
  
  aggregated_rank_table <- familiar:::rank.aggregate_feature_ranks(data, 3L, "truncated_borda")
  
  testthat::expect_equal(aggregated_rank_table, expected_rank_table, ignore_attr=TRUE)
})

##### Enhanced truncated borda aggregation ###############################
testthat::test_that("Enchanced truncated borda rank aggregation is correct", {
  expected_rank_table <- data.table::data.table("name"=c("a", "b", "c", "d", "e"),
                                                "aggr_score"=c(7/3, 10/9, 4/9, 0, 1/3),
                                                "aggr_rank"=c(1, 2, 3, 5, 4))
  
  aggregated_rank_table <- familiar:::rank.aggregate_feature_ranks(data, 3L, "enhanced_truncated_borda")
  
  testthat::expect_equal(aggregated_rank_table, expected_rank_table, ignore_attr=TRUE)
})

# Create prototype.
proto_vimp_table <- familiar:::prepare_vimp_table_object(
  vimp_method = "mim",
  invert = TRUE,
  state = "declustered"
)

# Fill a list with multiple prototypes.
vimp_table_list <- list(
  proto_vimp_table,
  proto_vimp_table,
  proto_vimp_table
)

# Update with actual data.
vimp_table_list[[1]]@vimp_table <- data.table::data.table(
  "score" = c(1.0, 0.8, 0.6, 0.4, 0.2),
  "name" = c("a", "b", "c", "d", "e")
)
vimp_table_list[[2]]@vimp_table <- data.table::data.table(
  "score" = c(0.8, 1.0, 0.6),
  "name" = c("a", "b", "c")
)
vimp_table_list[[3]]@vimp_table <- data.table::data.table(
  "score" = c(0.6, 1.0),
  "name" = c("a", "e")
)

# Mean rank aggregation --------------------------------------------------------
testthat::test_that("Mean rank aggregation is correct", {
  expected_rank_table <- data.table::data.table(
    "name" = c("a", "b", "c", "d", "e"),
    "score" = c(5 / 3, 3 / 2, 3, 4, 3),
    "rank" = c(2, 1, 3, 4, 3)
  )

  aggregated_rank_table <- familiar:::aggregate_vimp_table(
    vimp_table_list,
    aggregation_method = "mean")

  testthat::expect_equal(
    familiar:::get_vimp_table(aggregated_rank_table),
    expected_rank_table,
    ignore_attr = TRUE)
})

# Median rank aggregation ------------------------------------------------------
testthat::test_that("Median rank aggregation is correct", {
  expected_rank_table <- data.table::data.table(
    "name" = c("a", "b", "c", "d", "e"),
    "score" = c(2, 3 / 2, 3, 4, 3),
    "rank" = c(2, 1, 3, 4, 3)
  )

  aggregated_rank_table <- familiar:::aggregate_vimp_table(
    vimp_table_list,
    aggregation_method = "median")

  testthat::expect_equal(
    familiar:::get_vimp_table(aggregated_rank_table),
    expected_rank_table,
    ignore_attr = TRUE)
})

# Best rank aggregation --------------------------------------------------------
testthat::test_that("Best rank aggregation is correct", {
  expected_rank_table <- data.table::data.table(
    "name" = c("a", "b", "c", "d", "e"),
    "score" = c(1, 1, 3, 4, 1),
    "rank" = c(1, 1, 2, 3, 1)
  )

  aggregated_rank_table <- familiar:::aggregate_vimp_table(
    vimp_table_list,
    aggregation_method = "best")

  testthat::expect_equal(
    familiar:::get_vimp_table(aggregated_rank_table),
    expected_rank_table,
    ignore_attr = TRUE)
})

# Worst rank aggregation -------------------------------------------------------
testthat::test_that("Worst rank aggregation is correct", {
  expected_rank_table <- data.table::data.table(
    "name" = c("a", "b", "c", "d", "e"),
    "score" = c(2, 2, 3, 4, 5),
    "rank" = c(1, 1, 2, 3, 4)
  )

  aggregated_rank_table <- familiar:::aggregate_vimp_table(
    vimp_table_list,
    aggregation_method = "worst")

  testthat::expect_equal(
    familiar:::get_vimp_table(aggregated_rank_table),
    expected_rank_table,
    ignore_attr = TRUE)
})

# Stability rank aggregation ---------------------------------------------------
testthat::test_that("Stability rank aggregation is correct", {
  expected_rank_table <- data.table::data.table(
    "name" = c("a", "b", "c", "d", "e"),
    "score" = c(1, 2 / 3, 2 / 3, 0, 1 / 3),
    "rank" = c(1, 2, 2, 4, 3)
  )

  aggregated_rank_table <- familiar:::aggregate_vimp_table(
    vimp_table_list,
    aggregation_method = "stability",
    rank_threshold = 3L)

  testthat::expect_equal(
    familiar:::get_vimp_table(aggregated_rank_table),
    expected_rank_table,
    ignore_attr = TRUE)
})

# Exponential rank aggregation -------------------------------------------------
testthat::test_that("Exponential rank aggregation is correct", {
  expected_rank_table <- data.table::data.table(
    "name" = c("a", "b", "c", "d", "e"),
    "score" = c(
      exp(-1 / 3) + 2 * exp(-2 / 3),
      exp(-2 / 3) + exp(-1 / 3),
      2 * exp(-1),
      0.0,
      exp(-1 / 3)),
    "rank" = c(1, 2, 3, 5, 4)
  )

  aggregated_rank_table <- familiar:::aggregate_vimp_table(
    vimp_table_list,
    aggregation_method = "exponential",
    rank_threshold = 3L)

  testthat::expect_equal(
    familiar:::get_vimp_table(aggregated_rank_table),
    expected_rank_table,
    ignore_attr = TRUE)
})

# Borda aggregation ------------------------------------------------------------
testthat::test_that("Borda rank aggregation is correct", {
  expected_rank_table <- data.table::data.table(
    "name" = c("a", "b", "c", "d", "e"),
    "score" = c(13 / 6, 9 / 5, 14 / 15, 2 / 5, 6 / 5),
    "rank" = c(1, 2, 4, 5, 3)
  )

  aggregated_rank_table <- familiar:::aggregate_vimp_table(
    vimp_table_list,
    aggregation_method = "borda",
    rank_threshold = 3L)

  testthat::expect_equal(
    familiar:::get_vimp_table(aggregated_rank_table),
    expected_rank_table,
    ignore_attr = TRUE)
})

# Enhanced borda aggregation ---------------------------------------------------
testthat::test_that("Enhanced borda rank aggregation is correct", {
  expected_rank_table <- data.table::data.table(
    "name" = c("a", "b", "c", "d", "e"),
    "score" = c(13 / 6, 6 / 5, 28 / 45, 0, 2 / 5),
    "rank" = c(1, 2, 3, 5, 4)
  )

  aggregated_rank_table <- familiar:::aggregate_vimp_table(
    vimp_table_list,
    aggregation_method = "enhanced_borda",
    rank_threshold = 3L)

  testthat::expect_equal(
    familiar:::get_vimp_table(aggregated_rank_table),
    expected_rank_table,
    ignore_attr = TRUE)
})

# Truncated borda aggregation --------------------------------------------------
testthat::test_that("Truncated borda rank aggregation is correct", {
  expected_rank_table <- data.table::data.table(
    "name" = c("a", "b", "c", "d", "e"),
    "score" = c(7 / 3, 5 / 3, 2 / 3, 0, 1),
    "rank" = c(1, 2, 4, 5, 3)
  )

  aggregated_rank_table <- familiar:::aggregate_vimp_table(
    vimp_table_list,
    aggregation_method = "truncated_borda",
    rank_threshold = 3L)

  testthat::expect_equal(
    familiar:::get_vimp_table(aggregated_rank_table),
    expected_rank_table,
    ignore_attr = TRUE)
})

# Enhanced truncated borda aggregation -----------------------------------------
testthat::test_that("Enchanced truncated borda rank aggregation is correct", {
  expected_rank_table <- data.table::data.table(
    "name" = c("a", "b", "c", "d", "e"),
    "score" = c(7 / 3, 10 / 9, 4 / 9, 0, 1 / 3),
    "rank" = c(1, 2, 3, 5, 4)
  )

  aggregated_rank_table <- familiar:::aggregate_vimp_table(
    vimp_table_list,
    aggregation_method = "enhanced_truncated_borda",
    rank_threshold = 3L)

  testthat::expect_equal(
    familiar:::get_vimp_table(aggregated_rank_table),
    expected_rank_table,
    ignore_attr = TRUE)
})

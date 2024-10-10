# Don't perform any further tests on CRAN due to running time.
testthat::skip_on_cran()

# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "binomial",
  to_data_object = FALSE
)

# Test that the duplicated data check ------------------------------------------

# Duplicate the first row.
duplicated_data <- data.table::copy(data)
duplicated_data[2L, ] <- duplicated_data[1L, ]

# Create data assignment object.
testthat::expect_warning(
  experiment_data_assignment <- familiar::precompute_data_assignment(
    data = duplicated_data,
    experimental_design = "bs(fs+mb,3)",
    outcome_type = "binomial",
    outcome_column = "outcome",
    batch_id_column = "batch_id",
    sample_id_column = "sample_id",
    series_id_column = "series_id",
    class_levels = c("red", "green"),
    verbose = FALSE
  ),
  class = "familiar_data_check"
)


# Test the invariant feature check ---------------------------------------------

# Add invariant feature
invariant_data <- data.table::copy(data)
invariant_data[, "feature_5" := 3.0]

testthat::expect_warning(
  experiment_data_assignment <- familiar::precompute_data_assignment(
    data = invariant_data,
    experimental_design = "bs(fs+mb,3)",
    outcome_type = "binomial",
    outcome_column = "outcome",
    batch_id_column = "batch_id",
    sample_id_column = "sample_id",
    series_id_column = "series_id",
    class_levels = c("red", "green"),
    verbose = FALSE
  ),
  class = "familiar_data_check"
)


# Test the one-to-one predictor check ------------------------------------------

# Add one-to-one predictor
one_to_one_data <- data.table::copy(data)
one_to_one_data[, "feature_5" := 0.0]
one_to_one_data[outcome == "red", "feature_5" := 1.0]

testthat::expect_warning(
  experiment_data_assignment <- familiar::precompute_data_assignment(
    data = one_to_one_data,
    experimental_design = "bs(fs+mb,3)",
    outcome_type = "binomial",
    outcome_column = "outcome",
    batch_id_column = "batch_id",
    sample_id_column = "sample_id",
    series_id_column = "series_id",
    class_levels = c("red", "green"),
    verbose = FALSE
  ),
  class = "familiar_data_check"
)

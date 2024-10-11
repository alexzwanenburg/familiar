# Don't perform any further tests on CRAN due to running time.
testthat::skip_on_cran()

# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "binomial",
  to_data_object = FALSE
)

# Check reproducibility of sample assignment -----------------------------------

# Create data assignment object without fixed seed.
experiment_data_assignment_random <- familiar::precompute_data_assignment(
  data = data,
  experimental_design = "bs(fs+mb,3)",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = FALSE
)

# Create data assignment object with fixed seed.
experiment_data_assignment_a <- familiar::precompute_data_assignment(
  data = data,
  experimental_design = "bs(fs+mb,3)",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  iteration_seed = 19L,
  verbose = FALSE
)

# Create data assignment object with fixed seed.
experiment_data_assignment_b <- familiar::precompute_data_assignment(
  data = data,
  experimental_design = "bs(fs+mb,3)",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  iteration_seed = 19L,
  verbose = FALSE
)

# Create data assignment object with fixed seed.
experiment_data_assignment_different <- familiar::precompute_data_assignment(
  data = data,
  experimental_design = "bs(fs+mb,3)",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  iteration_seed = 20L,
  verbose = FALSE
)

# Test that the same fixed seed leads to the same sample assignment.
testthat::expect_equal(
  experiment_data_assignment_a@iteration_list,
  experiment_data_assignment_b@iteration_list,
  ignore_attr = TRUE
)

# Test that the random seed leads to a different sample assignment.
testthat::expect_false(identical(
  experiment_data_assignment_random@iteration_list,
  experiment_data_assignment_a@iteration_list
))

# Test that a different fixed seed leads to a different sample assignment.
testthat::expect_false(identical(
  experiment_data_assignment_different@iteration_list,
  experiment_data_assignment_a@iteration_list
))

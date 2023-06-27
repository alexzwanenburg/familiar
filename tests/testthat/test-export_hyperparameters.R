# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()

debug_flag <- FALSE

familiar:::test_export(
  export_function = familiar:::export_hyperparameters,
  data_element = "hyperparameters",
  not_available_no_samples = FALSE,
  not_available_all_predictions_fail = FALSE,
  not_available_some_predictions_fail = FALSE,
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  debug = debug_flag
)

familiar:::test_export(
  export_function = familiar:::export_hyperparameters,
  data_element = "hyperparameters",
  not_available_no_samples = FALSE,
  not_available_all_predictions_fail = FALSE,
  not_available_some_predictions_fail = FALSE,
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  export_args = list("aggregate_results" = FALSE),
  debug = debug_flag
)

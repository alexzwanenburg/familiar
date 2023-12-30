# Don't perform any further tests on CRAN due to running time.
testthat::skip_on_cran()

debug_flag <- FALSE

# Default
results <- familiar:::test_export_specific(
  export_function = familiar:::export_model_performance,
  data_element = "model_performance",
  not_available_all_prospective = TRUE,
  not_available_any_prospective = c("binomial", "multinomial", "survival"),
  not_available_single_sample = c("binomial", "multinomial", "survival"),
  outcome_type_available = c("continuous", "binomial", "multinomial", "survival"),
  create_novelty_detector = FALSE,
  debug = debug_flag
)

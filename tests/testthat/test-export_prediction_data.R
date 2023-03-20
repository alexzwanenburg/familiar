# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

debug_flag <- FALSE

familiar:::test_export(
  export_function = familiar:::export_prediction_data,
  not_available_all_predictions_fail = FALSE,
  not_available_some_predictions_fail = FALSE,
  data_element = "prediction_data",
  outcome_type_available = c("count", "continuous", "binomial", "multinomial", "survival"),
  detail_level = "ensemble",
  create_novelty_detector = TRUE,
  debug = debug_flag
)

familiar:::test_export(
  export_function = familiar:::export_prediction_data,
  not_available_all_predictions_fail = FALSE,
  not_available_some_predictions_fail = FALSE,
  data_element = "prediction_data",
  outcome_type_available = c("count", "continuous", "binomial", "multinomial", "survival"),
  detail_level = "hybrid",
  estimation_type = "bci",
  confidence_level = 0.80,
  aggregate_results = TRUE,
  create_novelty_detector = TRUE,
  n_models = 100,
  test_specific_config = TRUE,
  debug = debug_flag
)

familiar:::test_export(
  export_function = familiar:::export_prediction_data,
  not_available_all_predictions_fail = FALSE,
  not_available_some_predictions_fail = FALSE,
  data_element = "prediction_data",
  outcome_type_available = c("count", "continuous", "binomial", "multinomial", "survival"),
  detail_level = "hybrid",
  estimation_type = "bias_correction",
  confidence_level = 0.80,
  aggregate_results = TRUE,
  create_novelty_detector = TRUE,
  n_models = 20,
  test_specific_config = TRUE,
  debug = debug_flag
)

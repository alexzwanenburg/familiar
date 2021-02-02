# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

debug_flag <- FALSE

familiar:::test_export(export_function=familiar:::export_prediction_data,
                       data_element="prediction_data",
                       outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                       detail_level="ensemble",
                       create_novelty_detector=TRUE,
                       debug=debug_flag)

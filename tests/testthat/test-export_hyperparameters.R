# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

debug_flag <- FALSE

familiar:::test_export(export_function=familiar:::export_hyperparameters,
                       data_element="hyperparameters",
                       always_available=TRUE,
                       outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                       debug=debug_flag)


familiar:::test_export(export_function=familiar:::export_hyperparameters,
                       data_element="hyperparameters",
                       always_available=TRUE,
                       outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                       export_args=list("aggregate_results"=FALSE),
                       debug=debug_flag)

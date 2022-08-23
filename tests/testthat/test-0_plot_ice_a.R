# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

debug_flag <- FALSE

familiar:::test_plots(plot_function=familiar:::plot_ice,
                      not_available_some_predictions_fail=FALSE,
                      data_element="ice_data",
                      debug=debug_flag)



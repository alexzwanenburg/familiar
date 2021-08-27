# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

debug_flag <- FALSE

# Generic test
# Note that one-sample kaplan-meier curves can be created.
familiar:::test_plots(plot_function=familiar:::plot_kaplan_meier,
                      outcome_type_available=c("survival"),
                      data_element="risk_stratification_data",
                      except_prospective = TRUE,
                      debug=debug_flag)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_kaplan_meier,
                              data_element="risk_stratification_data",
                              outcome_type_available=c("survival"),
                              debug=debug_flag)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_kaplan_meier,
                              data_element="risk_stratification_data",
                              outcome_type_available=c("survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set"),
                                             "color_by"="risk_group"),
                              debug=debug_flag)

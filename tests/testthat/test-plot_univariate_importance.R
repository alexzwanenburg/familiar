# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# FDR-corrected p-values. Note that failure to predict survival probabilities does
# not influence the produced plots.
familiar:::test_plots(plot_function=familiar::plot_univariate_importance,
                      data_element="univariate_analysis",
                      not_available_all_prospective = TRUE,
                      not_available_any_prospective=TRUE,
                      not_available_single_sample=TRUE,
                      not_available_all_predictions_fail=FALSE,
                      not_available_some_predictions_fail=FALSE,
                      outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                      plot_args=list("verbose"=FALSE),
                      debug=debug_flag)

# Uncorrected p-values.
familiar:::test_plots(plot_function=familiar::plot_univariate_importance,
                      data_element="univariate_analysis",
                      not_available_all_prospective = TRUE,
                      not_available_any_prospective=TRUE,
                      not_available_single_sample=TRUE,
                      not_available_all_predictions_fail=FALSE,
                      not_available_some_predictions_fail=FALSE,
                      outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                      plot_args=list("verbose"=FALSE,
                                     "p_adjustment_method"="p_value"),
                      debug=debug_flag)


familiar:::test_plot_ordering(plot_function=familiar::plot_univariate_importance,
                              data_element="univariate_analysis",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("verbose"=FALSE,
                                             "p_adjustment_method"="holm",
                                             "facet_by"=c("data_set", "learner", "fs_method")),
                              debug=debug_flag)


familiar:::test_plot_ordering(plot_function=familiar::plot_univariate_importance,
                              data_element="univariate_analysis",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("verbose"=FALSE,
                                             "p_adjustment_method"="BY",
                                             "facet_by"=c("data_set", "fs_method"),
                                             "color_by"="learner"),
                              debug=debug_flag)

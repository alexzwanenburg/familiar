# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

# FDR-crrected p-values
familiar:::test_plots(plot_function=familiar::plot_univariate_importance,
                      data_element="univariate_analysis",
                      outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                      plot_args=list("verbose"=FALSE))

# Uncorrected p-values.
familiar:::test_plots(plot_function=familiar::plot_univariate_importance,
                      data_element="univariate_analysis",
                      outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                      plot_args=list("verbose"=FALSE,
                                     "importance_metric"="p_value"))


familiar:::test_plot_ordering(plot_function=familiar::plot_univariate_importance,
                              data_element="univariate_analysis",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("verbose"=FALSE,
                                             "importance_metric"="p_value",
                                             "facet_by"=c("data_set", "learner", "fs_method")))


familiar:::test_plot_ordering(plot_function=familiar::plot_univariate_importance,
                              data_element="univariate_analysis",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("verbose"=FALSE,
                                             "importance_metric"="p_value",
                                             "facet_by"=c("data_set", "fs_method"),
                                             "color_by"="learner"))

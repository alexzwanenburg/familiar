# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

debug_flag <- FALSE

# Generic test
familiar:::test_plots(plot_function=familiar:::plot_sample_clustering,
                      not_available_all_predictions_fail=FALSE,
                      not_available_some_predictions_fail=FALSE,
                      outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                      data_element="feature_expressions",
                      plot_args=list("verbose"=FALSE),
                      debug=debug_flag)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_sample_clustering,
                              data_element="feature_expressions",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("verbose"=FALSE),
                              debug=debug_flag)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_sample_clustering,
                              data_element="feature_expressions",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set"),
                                             "verbose"=FALSE),
                              debug=debug_flag)

# Try swapping the axis.
familiar:::test_plot_ordering(plot_function=familiar:::plot_sample_clustering,
                              data_element="feature_expressions",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set"),
                                             "x_axis_by"="sample",
                                             "y_axis_by"="feature",
                                             "verbose"=FALSE),
                              debug=debug_flag)

# Try swapping the axis and putting the outcome below the heatmap
familiar:::test_plot_ordering(plot_function=familiar:::plot_sample_clustering,
                              data_element="feature_expressions",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set"),
                                             "x_axis_by"="sample",
                                             "y_axis_by"="feature",
                                             "show_outcome"="bottom",
                                             "verbose"=FALSE),
                              debug=debug_flag)


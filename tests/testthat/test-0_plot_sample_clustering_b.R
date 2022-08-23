# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

debug_flag <- FALSE

# Generic test
familiar:::test_plots(plot_function=familiar:::plot_sample_clustering,
                      not_available_all_predictions_fail=FALSE,
                      not_available_some_predictions_fail=FALSE,
                      outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                      data_element="feature_expressions",
                      plot_args=list("verbose"=FALSE,
                                     "show_normalised_data"="set_normalisation"),
                      debug=debug_flag)

# No extra elements
familiar:::test_plot_ordering(plot_function=familiar:::plot_sample_clustering,
                              data_element="feature_expressions",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set"),
                                             "x_axis_by"="sample",
                                             "y_axis_by"="feature",
                                             "show_outcome"=FALSE,
                                             "show_feature_dendrogram"=FALSE,
                                             "show_sample_dendrogram"=FALSE,
                                             "verbose"=FALSE),
                              debug=debug_flag)

# No normalisation.
familiar:::test_plot_ordering(plot_function=familiar:::plot_sample_clustering,
                              data_element="feature_expressions",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set"),
                                             "show_normalised_data"="none",
                                             "verbose"=FALSE),
                              debug=debug_flag)

# Normalisation per dataset.
familiar:::test_plot_ordering(plot_function=familiar:::plot_sample_clustering,
                              data_element="feature_expressions",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set"),
                                             "show_normalised_data"="set_normalisation",
                                             "verbose"=FALSE),
                              debug=debug_flag)


# With sample limit
familiar:::test_plot_ordering(plot_function=familiar:::plot_sample_clustering,
                              data_element="feature_expressions",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set"),
                                             "sample_limit"=20L,
                                             "verbose"=FALSE),
                              debug=debug_flag)


# Test multiple evaluation times
familiar:::test_plot_ordering(plot_function=familiar:::plot_sample_clustering,
                              data_element="feature_expressions",
                              outcome_type_available=c("survival"),
                              plot_args=list("evaluation_times"=c(500, 1000, 1500, 2000),
                                             "verbose"=FALSE),
                              debug=debug_flag)

# Generic test
familiar:::test_plots(plot_function=familiar:::plot_sample_clustering,
                      outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                      data_element="feature_expressions",
                      plot_args=list("verbose"=FALSE))

# Generic test
familiar:::test_plots(plot_function=familiar:::plot_sample_clustering,
                      outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                      data_element="feature_expressions",
                      plot_args=list("verbose"=FALSE,
                                     "show_normalised_data"="set_normalisation"))


# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_sample_clustering,
                              data_element="feature_expressions",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("verbose"=FALSE))

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_sample_clustering,
                              data_element="feature_expressions",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set"),
                                             "verbose"=FALSE))

# Try swapping the axis.
familiar:::test_plot_ordering(plot_function=familiar:::plot_sample_clustering,
                              data_element="feature_expressions",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set"),
                                             "x_axis_by"="sample",
                                             "y_axis_by"="feature",
                                             "verbose"=FALSE))

# Try swapping the axis and putting the outcome below the heatmap
familiar:::test_plot_ordering(plot_function=familiar:::plot_sample_clustering,
                              data_element="feature_expressions",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set"),
                                             "x_axis_by"="sample",
                                             "y_axis_by"="feature",
                                             "show_outcome"="bottom",
                                             "verbose"=FALSE))

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
                                             "verbose"=FALSE))

# No normalisation.
familiar:::test_plot_ordering(plot_function=familiar:::plot_sample_clustering,
                              data_element="feature_expressions",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set"),
                                             "show_normalised_data"="none",
                                             "verbose"=FALSE))

# Normalisation per dataset.
familiar:::test_plot_ordering(plot_function=familiar:::plot_sample_clustering,
                              data_element="feature_expressions",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set"),
                                             "show_normalised_data"="set_normalisation",
                                             "verbose"=FALSE))


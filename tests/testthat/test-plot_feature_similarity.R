# Generic test
familiar:::test_plots(plot_function=familiar:::plot_feature_similarity,
                      except_one_feature = TRUE,
                      outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                      data_element="mutual_correlation")

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_feature_similarity,
                              data_element="mutual_correlation",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              debug=TRUE)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_feature_similarity,
                              data_element="mutual_correlation",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              debug=TRUE)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_feature_similarity,
                              data_element="mutual_correlation",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set")),
                              debug=TRUE)

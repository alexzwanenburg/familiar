# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

# Generic test
familiar:::test_plots(plot_function=familiar:::plot_feature_similarity,
                      except_one_feature = TRUE,
                      outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                      data_element="mutual_correlation")

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_feature_similarity,
                              data_element="mutual_correlation",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"))

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_feature_similarity,
                              data_element="mutual_correlation",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set")))

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_feature_similarity,
                              data_element="mutual_correlation",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("show_dendrogram"=c("left", "bottom")))

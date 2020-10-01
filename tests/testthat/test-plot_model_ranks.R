# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

# Generic test.
familiar:::test_plots(plot_function=familiar:::plot_model_signature_ranks,
                      always_available = TRUE,
                      outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                      data_element="model_vimp")

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_model_signature_ranks,
                              data_element="model_vimp",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"))

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_model_signature_ranks,
                              data_element="model_vimp",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                              plot_args=list("facet_by"=c("learner"),
                                             "color_by"=c("fs_method")),
                              debug=TRUE)

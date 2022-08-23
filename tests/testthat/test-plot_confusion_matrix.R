# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

debug_flag <- FALSE

# Generic test. Note that confusion matrices can be created if only one sample
# is present.
familiar:::test_plots(plot_function=familiar:::plot_confusion_matrix,
                      not_available_all_prospective=TRUE,
                      data_element="confusion_matrix",
                      outcome_type_available=c("binomial", "multinomial"),
                      debug=debug_flag)

# Test confusion matrix without transparent cells.
familiar:::test_plots(plot_function=familiar:::plot_confusion_matrix,
                      data_element="confusion_matrix",
                      outcome_type_available=c("binomial", "multinomial"),
                      test_specific_config=TRUE,
                      plot_args=list("show_alpha"="none"),
                      debug=debug_flag)

# Test by confusion matrix with transparency by class.
familiar:::test_plots(plot_function=familiar:::plot_confusion_matrix,
                      data_element="confusion_matrix",
                      outcome_type_available=c("binomial", "multinomial"),
                      test_specific_config=TRUE,
                      plot_args=list("show_alpha"="by_class"),
                      debug=debug_flag)

# Test by confusion matrix with transparency by the entire plot.
familiar:::test_plots(plot_function=familiar:::plot_confusion_matrix,
                      data_element="confusion_matrix",
                      outcome_type_available=c("binomial", "multinomial"),
                      test_specific_config=TRUE,
                      plot_args=list("show_alpha"="by_figure"),
                      debug=debug_flag)

# Test by confusion matrix with transparency by the entire plot.
familiar:::test_plots(plot_function=familiar:::plot_confusion_matrix,
                      data_element="confusion_matrix",
                      outcome_type_available=c("binomial", "multinomial"),
                      test_specific_config=TRUE,
                      plot_args=list("show_alpha"="by_all"),
                      debug=debug_flag)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_confusion_matrix,
                              data_element="confusion_matrix",
                              outcome_type_available=c("binomial", "multinomial"),
                              debug=debug_flag)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_confusion_matrix,
                              data_element="confusion_matrix",
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("facet_by"=c("data_set", "learner", "fs_method")),
                              debug=debug_flag)

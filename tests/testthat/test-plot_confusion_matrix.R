familiar:::test_plots(plot_function=familiar:::plot_confusion_matrix,
                      data_element="confusion_matrix",
                      outcome_type_available=c("binomial", "multinomial"))

# Test confusion matrix without transparent cells.
familiar:::test_plots(plot_function=familiar:::plot_confusion_matrix,
                      data_element="confusion_matrix",
                      outcome_type_available=c("binomial", "multinomial"),
                      test_specific_config=TRUE,
                      plot_args=list("show_alpha"="none"))

# Test by confusion matrix with transparency by class.
familiar:::test_plots(plot_function=familiar:::plot_confusion_matrix,
                      data_element="confusion_matrix",
                      outcome_type_available=c("binomial", "multinomial"),
                      test_specific_config=TRUE,
                      plot_args=list("show_alpha"="by_class"))

# Test by confusion matrix with transparency by the entire plot.
familiar:::test_plots(plot_function=familiar:::plot_confusion_matrix,
                      data_element="confusion_matrix",
                      outcome_type_available=c("binomial", "multinomial"),
                      test_specific_config=TRUE,
                      plot_args=list("show_alpha"="by_figure"))

# Test by confusion matrix with transparency by the entire plot.
familiar:::test_plots(plot_function=familiar:::plot_confusion_matrix,
                      data_element="confusion_matrix",
                      outcome_type_available=c("binomial", "multinomial"),
                      test_specific_config=TRUE,
                      plot_args=list("show_alpha"="by_all"))

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_confusion_matrix,
                              data_element="confusion_matrix",
                              outcome_type_available=c("binomial", "multinomial"))

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_confusion_matrix,
                              data_element="confusion_matrix",
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("facet_by"=c("data_set", "learner", "fs_method")))

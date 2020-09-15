familiar:::test_plots(plot_function=familiar:::plot_decision_curve,
                      outcome_type_available=c("binomial", "multinomial", "survival"),
                      data_element="decision_curve_analyis")

# Test with step-wise confidence interval
familiar:::test_plots(plot_function=familiar:::plot_decision_curve,
                      outcome_type_available=c("binomial", "multinomial", "survival"),
                      data_element="decision_curve_analyis",
                      test_specific_config=TRUE,
                      plot_args=list("conf_int_style"="step"))

# Test without confidence interval shown
familiar:::test_plots(plot_function=familiar:::plot_decision_curve,
                      outcome_type_available=c("binomial", "multinomial", "survival"),
                      data_element="decision_curve_analyis",
                      test_specific_config=TRUE,
                      plot_args=list("conf_int_style"="none"))

# Test without confidence interval
familiar:::test_plots(plot_function=familiar:::plot_decision_curve,
                      outcome_type_available=c("binomial", "multinomial", "survival"),
                      data_element="decision_curve_analyis",
                      compute_ensemble_ci=FALSE,
                      test_specific_config=TRUE)

# Test without pre-aggregation of confidence interval
familiar:::test_plots(plot_function=familiar:::plot_decision_curve,
                      outcome_type_available=c("binomial", "multinomial", "survival"),
                      data_element="decision_curve_analyis",
                      aggregate_ci=FALSE,
                      test_specific_config=TRUE)

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_decision_curve,
                              data_element="decision_curve_analyis",
                              compute_ensemble_ci=FALSE,
                              outcome_type_available=c("binomial", "multinomial", "survival"))

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_decision_curve,
                              data_element="decision_curve_analyis",
                              outcome_type_available=c("binomial"),
                              plot_args=list("facet_by"=c("learner", "fs_method"),
                                             "color_by"="data_set"))

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_decision_curve,
                              data_element="decision_curve_analyis",
                              outcome_type_available=c("multinomial"),
                              plot_args=list("facet_by"=c("learner", "fs_method"),
                                             "color_by"=c("data_set", "pos_class")))

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_decision_curve,
                              data_element="decision_curve_analyis",
                              outcome_type_available=c("survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method"),
                                             "color_by"=c("data_set")))

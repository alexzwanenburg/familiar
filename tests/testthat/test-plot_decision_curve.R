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


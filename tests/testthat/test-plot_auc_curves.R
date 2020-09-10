familiar:::test_plots(plot_function=familiar:::plot_auc_roc_curve,
                      data_element="auc_data",
                      outcome_type_available=c("binomial", "multinomial"))

# Test with step-wise confidence interval
familiar:::test_plots(plot_function=familiar:::plot_auc_roc_curve,
                      data_element="auc_data",
                      outcome_type_available=c("binomial", "multinomial"),
                      test_specific_config=TRUE,
                      plot_args=list("conf_int_style"="step"))

# Test without confidence interval shown
familiar:::test_plots(plot_function=familiar:::plot_auc_roc_curve,
                      data_element="auc_data",
                      outcome_type_available=c("binomial", "multinomial"),
                      test_specific_config=TRUE,
                      plot_args=list("conf_int_style"="none"))

# Test without confidence interval
familiar:::test_plots(plot_function=familiar:::plot_auc_roc_curve,
                      data_element="auc_data",
                      outcome_type_available=c("binomial", "multinomial"),
                      compute_ensemble_ci=FALSE,
                      test_specific_config=TRUE)

# Test without pre-aggregation of confidence interval
familiar:::test_plots(plot_function=familiar:::plot_auc_roc_curve,
                      data_element="auc_data",
                      outcome_type_available=c("binomial", "multinomial"),
                      aggregate_ci=FALSE,
                      test_specific_config=TRUE)

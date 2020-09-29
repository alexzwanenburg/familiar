familiar:::test_plots(plot_function=familiar:::plot_permutation_variable_importance,
                      data_element="permutation_vimp",
                      compute_ensemble_ci=FALSE,
                      outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"), debug=TRUE)

# Point line confidence intervals.
familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              compute_ensemble_ci=TRUE,
                              confidence_level=0.50,
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              plot_args=list("conf_int_style"="point_line"),
                              debug=TRUE)

# Line style confidence intervals.
familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              compute_ensemble_ci=TRUE,
                              confidence_level=0.50,
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              plot_args=list("conf_int_style"="line"),
                              debug=TRUE)

# Bar + line style confidence intervals.
familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              compute_ensemble_ci=TRUE,
                              confidence_level=0.50,
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              plot_args=list("conf_int_style"="bar_line"),
                              debug=TRUE)

# No confidence intervals.
familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              compute_ensemble_ci=TRUE,
                              confidence_level=0.50,
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              plot_args=list("conf_int_style"="none"),
                              debug=TRUE)


familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              compute_ensemble_ci=FALSE,
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              plot_args=list("facet_by"=c("data_set", "learner", "fs_method"),
                                             "color_by"=c("metric", "similarity_threshold")),
                              debug=TRUE)

# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

# Generic test.
familiar:::test_plots(plot_function=familiar:::plot_permutation_variable_importance,
                      data_element="permutation_vimp",
                      compute_ensemble_ci=FALSE,
                      outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"))

# Point line confidence intervals.
familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              compute_ensemble_ci=TRUE,
                              confidence_level=0.50,
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              plot_args=list("conf_int_style"="point_line"))

# Line style confidence intervals.
familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              compute_ensemble_ci=TRUE,
                              confidence_level=0.50,
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              plot_args=list("conf_int_style"="line"))

# Bar + line style confidence intervals.
familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              compute_ensemble_ci=TRUE,
                              confidence_level=0.50,
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              plot_args=list("conf_int_style"="bar_line"))

# No confidence intervals.
familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              compute_ensemble_ci=TRUE,
                              confidence_level=0.50,
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              plot_args=list("conf_int_style"="none"))


familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              compute_ensemble_ci=FALSE,
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              plot_args=list("facet_by"=c("data_set", "learner", "fs_method"),
                                             "color_by"=c("metric", "similarity_threshold")))

# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

debug_flag <- FALSE

# Generic test.
familiar:::test_plots(plot_function=familiar:::plot_permutation_variable_importance,
                      data_element="permutation_vimp",
                      except_prospective = TRUE,
                      except_one_sample = TRUE,
                      except_some_failed_prediction=FALSE,
                      outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"),
                      estimation_type="point",
                      debug=debug_flag)

# Point line confidence intervals.
familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              estimation_type="bci",
                              confidence_level=0.50,
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              plot_args=list("conf_int_style"="point_line"),
                              debug=debug_flag)

# Line style confidence intervals.
familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              estimation_type="bci",
                              confidence_level=0.50,
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              plot_args=list("conf_int_style"="line"),
                              debug=debug_flag)

# Bar + line style confidence intervals.
familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              estimation_type="bci",
                              confidence_level=0.50,
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              plot_args=list("conf_int_style"="bar_line"),
                              debug=debug_flag)

# No confidence intervals.
familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              estimation_type="bci",
                              confidence_level=0.50,
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              plot_args=list("conf_int_style"="none"),
                              debug=debug_flag)


familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              estimation_type="point",
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              plot_args=list("facet_by"=c("data_set", "learner", "fs_method"),
                                             "color_by"=c("metric", "similarity_threshold")),
                              debug=debug_flag)

# Different clustering method.
familiar:::test_plot_ordering(plot_function=familiar:::plot_permutation_variable_importance,
                              data_element="permutation_vimp",
                              feature_cluster_method="pam",
                              feature_cluster_cut_method = "silhouette",
                              estimation_type="point",
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("multinomial"),
                              debug=debug_flag)


familiar:::test_plots(plot_function=familiar:::plot_model_performance,
                      data_element="model_performance",
                      outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"))

##### Violin-plot (categorical) #####
familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("facet_by"=c("data_set", "learner", "fs_method")))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("facet_by"=c("data_set", "learner", "fs_method"),
                                             "x_axis_by"="metric"))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("facet_by"=c("learner", "fs_method"),
                                             "x_axis_by"="data_set",
                                             "color_by"="metric"))

##### Violin-plot (numeric) #####
familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              outcome_type_available=c("count", "continuous"),
                              plot_args=list("facet_by"=c("data_set", "learner", "fs_method")))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("rmse", "mae"),
                              outcome_type_available=c("count", "continuous"),
                              plot_args=list("facet_by"=c("data_set", "learner", "fs_method"),
                                             "x_axis_by"="metric"))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("rmse", "mae"),
                              outcome_type_available=c("count", "continuous"),
                              plot_args=list("facet_by"=c("learner", "fs_method"),
                                             "x_axis_by"="data_set",
                                             "color_by"="metric"))

##### Violin-plot (survival) #####
familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              outcome_type_available=c("survival"),
                              plot_args=list("facet_by"=c("data_set", "learner", "fs_method")))

##### Bar plot (categorical) #####
familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("plot_type"="barplot",
                                             "facet_by"=c("data_set", "learner", "fs_method")))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("plot_type"="barplot",
                                             "facet_by"=c("data_set", "learner", "fs_method"),
                                             "x_axis_by"="metric"))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("plot_type"="barplot",
                                             "facet_by"=c("learner", "fs_method"),
                                             "x_axis_by"="data_set",
                                             "color_by"="metric"))

##### Bar plot (numeric) #####
familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              outcome_type_available=c("count", "continuous"),
                              plot_args=list("plot_type"="barplot",
                                             "facet_by"=c("data_set", "learner", "fs_method")))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("rmse", "mae"),
                              outcome_type_available=c("count", "continuous"),
                              plot_args=list("plot_type"="barplot",
                                             "facet_by"=c("data_set", "learner", "fs_method"),
                                             "x_axis_by"="metric"))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("rmse", "mae"),
                              outcome_type_available=c("count", "continuous"),
                              plot_args=list("plot_type"="barplot",
                                             "facet_by"=c("learner", "fs_method"),
                                             "x_axis_by"="data_set",
                                             "color_by"="metric"))

##### Bar plot (survival) #####
familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              outcome_type_available=c("survival"),
                              plot_args=list("plot_type"="barplot",
                                             "facet_by"=c("data_set", "learner", "fs_method")))


##### Box plot (categorical) #####
familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("plot_type"="boxplot",
                                             "facet_by"=c("data_set", "learner", "fs_method")))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("plot_type"="boxplot",
                                             "facet_by"=c("data_set", "learner", "fs_method"),
                                             "x_axis_by"="metric"))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("plot_type"="boxplot",
                                             "facet_by"=c("learner", "fs_method"),
                                             "x_axis_by"="data_set",
                                             "color_by"="metric"))

##### Box plot (numeric) #####
familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              outcome_type_available=c("count", "continuous"),
                              plot_args=list("plot_type"="boxplot",
                                             "facet_by"=c("data_set", "learner", "fs_method")))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("rmse", "mae"),
                              outcome_type_available=c("count", "continuous"),
                              plot_args=list("plot_type"="boxplot",
                                             "facet_by"=c("data_set", "learner", "fs_method"),
                                             "x_axis_by"="metric"))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("rmse", "mae"),
                              outcome_type_available=c("count", "continuous"),
                              plot_args=list("plot_type"="boxplot",
                                             "facet_by"=c("learner", "fs_method"),
                                             "x_axis_by"="data_set",
                                             "color_by"="metric"))

##### Box plot (survival) #####
familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              outcome_type_available=c("survival"),
                              plot_args=list("plot_type"="boxplot",
                                             "facet_by"=c("data_set", "learner", "fs_method")))

##### Heatmap (categorical) #####
familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("plot_type"="heatmap"))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("plot_type"="heatmap"))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("plot_type"="heatmap",
                                             "facet_by"=c("data_set", "metric"),
                                             "x_axis_by"="learner",
                                             "y_axis_by"="fs_method"))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("auc_roc", "accuracy"),
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("plot_type"="heatmap",
                                             "annotate_performance"="value_ci",
                                             "facet_by"=c("data_set", "metric"),
                                             "x_axis_by"="learner",
                                             "y_axis_by"="fs_method"))

##### Heatmap (numeric) #####
familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              outcome_type_available=c("binomial", "multinomial"),
                              plot_args=list("plot_type"="heatmap"))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("rmse", "mae"),
                              outcome_type_available=c("count", "continuous"),
                              plot_args=list("plot_type"="heatmap"))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("rmse", "mae"),
                              outcome_type_available=c("count", "continuous"),
                              plot_args=list("plot_type"="heatmap",
                                             "facet_by"=c("data_set", "metric"),
                                             "x_axis_by"="learner",
                                             "y_axis_by"="fs_method"))

familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              metric=c("rmse", "mae"),
                              outcome_type_available=c("count", "continuous"),
                              plot_args=list("plot_type"="heatmap",
                                             "annotate_performance"="value_ci",
                                             "facet_by"=c("data_set", "metric"),
                                             "x_axis_by"="learner",
                                             "y_axis_by"="fs_method"))

##### Heatmap (survival) ######
familiar:::test_plot_ordering(plot_function=familiar:::plot_model_performance,
                              data_element="model_performance",
                              outcome_type_available=c("survival"),
                              plot_args=list("plot_type"="heatmap"))


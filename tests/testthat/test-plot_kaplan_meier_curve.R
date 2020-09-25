# Generic test
familiar:::test_plots(plot_function=familiar:::plot_kaplan_meier,
                      outcome_type_available=c("survival"),
                      data_element="kaplan_meier_data")

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_kaplan_meier,
                              data_element="kaplan_meier_data",
                              outcome_type_available=c("survival"))

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_kaplan_meier,
                              data_element="kaplan_meier_data",
                              stratification_ensemble_method="mean_threshold",
                              outcome_type_available=c("survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set"),
                                             "color_by"="risk_group"))

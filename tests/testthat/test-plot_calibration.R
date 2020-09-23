familiar:::test_plots(plot_function=familiar:::plot_calibration_data,
                      data_element="calibration_data")

# Test without density plot
familiar:::test_plots(plot_function=familiar:::plot_calibration_data,
                      data_element="calibration_data",
                      test_specific_config=TRUE,
                      plot_args=list("show_density"=FALSE))

# Test without the goodness of fit data
familiar:::test_plots(plot_function=familiar:::plot_calibration_data,
                      data_element="calibration_data",
                      test_specific_config=TRUE,
                      plot_args=list("show_goodness_of_fit"=FALSE))

# Test without the calibration fit data
familiar:::test_plots(plot_function=familiar:::plot_calibration_data,
                      data_element="calibration_data",
                      test_specific_config=TRUE,
                      plot_args=list("show_calibration_fit"=FALSE))

# Test without either fit data
familiar:::test_plots(plot_function=familiar:::plot_calibration_data,
                      data_element="calibration_data",
                      test_specific_config=TRUE,
                      plot_args=list("show_calibration_fit"=FALSE,
                                     "show_goodness_of_fit"=FALSE))


# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_calibration_data,
                              data_element="calibration_data",
                              outcome_type_available=c("count", "continuous", "binomial", "multinomial", "survival"))

# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_calibration_data,
                              data_element="calibration_data",
                              outcome_type_available=c("count"),
                              plot_args=list("facet_by"=c("fs_method", "learner"),
                                             "color_by"=c("data_set")))

familiar:::test_plot_ordering(plot_function=familiar:::plot_calibration_data,
                              data_element="calibration_data",
                              outcome_type_available=c("continuous"),
                              plot_args=list("facet_by"=c("fs_method", "learner"),
                                             "color_by"=c("data_set")))

familiar:::test_plot_ordering(plot_function=familiar:::plot_calibration_data,
                              data_element="calibration_data",
                              outcome_type_available=c("binomial"),
                              plot_args=list("facet_by"=c("fs_method", "learner"),
                                             "color_by"=c("data_set")))

familiar:::test_plot_ordering(plot_function=familiar:::plot_calibration_data,
                              data_element="calibration_data",
                              outcome_type_available=c("multinomial"),
                              plot_args=list("facet_by"=c("fs_method", "learner"),
                                             "color_by"=c("data_set", "pos_class")))

familiar:::test_plot_ordering(plot_function=familiar:::plot_calibration_data,
                              data_element="calibration_data",
                              outcome_type_available=c("survival"),
                              plot_args=list("facet_by"=c("fs_method", "learner"),
                                             "color_by"=c("data_set")))

familiar:::test_plot_ordering(plot_function=familiar:::plot_calibration_data,
                              data_element="calibration_data",
                              outcome_type_available=c("count", "continuous", "binomial", "survival"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set")))

familiar:::test_plot_ordering(plot_function=familiar:::plot_calibration_data,
                              data_element="calibration_data",
                              outcome_type_available=c("multinomial"),
                              plot_args=list("facet_by"=c("learner", "fs_method", "data_set"),
                                             "color_by"="pos_class"))

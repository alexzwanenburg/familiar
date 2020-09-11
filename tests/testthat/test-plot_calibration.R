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

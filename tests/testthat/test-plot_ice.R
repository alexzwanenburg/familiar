
# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_ice,
                              data_element="ice_data",
                              outcome_type_available=c("survival"),
                              debug=TRUE)


# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_ice,
                              data_element="ice_data",
                              outcome_type_available=c("survival"),
                              debug=TRUE)

familiar:::test_plot_ordering(plot_function=familiar:::plot_ice,
                              data_element="ice_data",
                              outcome_type_available="survival",
                              features=c("rx", "nodes"),
                              plot_args=list("anchor_values"=list("rx"="Obs",
                                                                  "nodes"=2.5),
                                             "value_scales"="figure"),
                              sample_limit=20L,
                              n_sample_points=10L,
                              debug=TRUE)

familiar:::test_plot_ordering(plot_function=familiar:::plot_ice,
                              data_element="ice_data",
                              outcome_type_available="multinomial",
                              features=c("Sepal_Length", "Petal_Width"),
                              plot_args=list("anchor_values"=list("Sepal_Length"=5.05,
                                                                  "Petal_Width"=1.05),
                                             "value_scales"="figure"),
                              sample_limit=20L,
                              n_sample_points=10L,
                              debug=TRUE)

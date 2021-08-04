# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

debug_flag <- FALSE

familiar:::test_plots(plot_function=familiar:::plot_ice,
                      data_element="ice_data",
                      debug=debug_flag)


# Test alignment of different plots, with missing data.
familiar:::test_plot_ordering(plot_function=familiar:::plot_ice,
                              data_element="ice_data",
                              outcome_type_available="survival",
                              features=c("adhere"),
                              sample_limit=20L,
                              n_sample_points=10L,
                              debug=debug_flag)


# 1D plot without novelty, and with anchored values.
familiar:::test_plot_ordering(plot_function=familiar:::plot_ice,
                              data_element="ice_data",
                              outcome_type_available="survival",
                              features=c("nodes"),
                              plot_args=list("anchor_values"=list("nodes"=2.5),
                                             "value_scales"="figure"),
                              sample_limit=20L,
                              n_sample_points=10L,
                              debug=debug_flag)


# 1D plot with novelty, and with anchored values.
familiar:::test_plot_ordering(plot_function=familiar:::plot_ice,
                              data_element="ice_data",
                              outcome_type_available="survival",
                              features=c("nodes"),
                              plot_args=list("anchor_values"=list("nodes"=2.5),
                                             "value_scales"="figure"),
                              sample_limit=20L,
                              n_sample_points=10L,
                              create_novelty_detector=TRUE,
                              debug=debug_flag)


# 1D plot with novelty, but no ICE
familiar:::test_plot_ordering(plot_function=familiar:::plot_ice,
                              data_element="ice_data",
                              outcome_type_available="survival",
                              features=c("nodes"),
                              plot_args=list("show_ice"=FALSE),
                              sample_limit=20L,
                              n_sample_points=10L,
                              create_novelty_detector=TRUE,
                              debug=debug_flag)


# 1D plot with novelty, but no PD
familiar:::test_plot_ordering(plot_function=familiar:::plot_ice,
                              data_element="ice_data",
                              outcome_type_available="survival",
                              features=c("nodes"),
                              plot_args=list("show_pd"=FALSE),
                              sample_limit=20L,
                              n_sample_points=10L,
                              create_novelty_detector=TRUE,
                              debug=debug_flag)


# 1D plot with novelty and colouring, and with anchored values.
familiar:::test_plot_ordering(plot_function=familiar:::plot_ice,
                              data_element="ice_data",
                              outcome_type_available="multinomial",
                              features=c("Sepal_Length"),
                              plot_args=list("anchor_values"=list("Sepal_Length"=5.05),
                                             "value_scales"="figure",
                                             "color_by"="positive_class",
                                             "facet_by"=c("data_set", "fs_method", "learner")),
                              sample_limit=20L,
                              n_sample_points=10L,
                              create_novelty_detector=TRUE,
                              debug=debug_flag)


# 1D plot with novelty and colouring, with anchored values and scaling per facet.
familiar:::test_plot_ordering(plot_function=familiar:::plot_ice,
                              data_element="ice_data",
                              outcome_type_available="multinomial",
                              features=c("Sepal_Length"),
                              plot_args=list("anchor_values"=list("Sepal_Length"=5.05),
                                             "value_scales"="facet",
                                             "color_by"="positive_class",
                                             "facet_by"=c("data_set", "fs_method", "learner")),
                              sample_limit=20L,
                              n_sample_points=10L,
                              create_novelty_detector=TRUE,
                              debug=debug_flag)


# 1D plot with novelty and colouring, and with anchored values.
familiar:::test_plot_ordering(plot_function=familiar:::plot_ice,
                              data_element="ice_data",
                              outcome_type_available="multinomial",
                              features=c("Sepal_Length"),
                              plot_args=list("anchor_values"=list("Sepal_Length"=5.05),
                                             "value_scales"="figure",
                                             "split_by"=c("fs_method", "learner"),
                                             "color_by"="positive_class",
                                             "facet_by"="data_set"),
                              sample_limit=20L,
                              n_sample_points=10L,
                              create_novelty_detector=TRUE,
                              debug=debug_flag)


# 2D plot without novelty, and with anchored values.
familiar:::test_plot_ordering(plot_function=familiar:::plot_ice,
                              data_element="ice_data",
                              outcome_type_available="survival",
                              features=c("rx", "nodes"),
                              plot_args=list("anchor_values"=list("rx"="Obs",
                                                                  "nodes"=2.5),
                                             "value_scales"="figure"),
                              sample_limit=20L,
                              n_sample_points=10L,
                              debug=debug_flag)


# 2D plot with novelty, and with anchored values.
familiar:::test_plot_ordering(plot_function=familiar:::plot_ice,
                              data_element="ice_data",
                              outcome_type_available="survival",
                              features=c("rx", "nodes"),
                              plot_args=list("anchor_values"=list("rx"="Obs",
                                                                  "nodes"=2.5),
                                             "value_scales"="figure"),
                              sample_limit=20L,
                              n_sample_points=10L,
                              create_novelty_detector=TRUE,
                              debug=debug_flag)


# 2D plot with novelty, and with anchored values.
familiar:::test_plot_ordering(plot_function=familiar:::plot_ice,
                              data_element="ice_data",
                              outcome_type_available="multinomial",
                              features=c("Sepal_Length", "Petal_Width"),
                              plot_args=list("anchor_values"=list("Sepal_Length"=5.05,
                                                                  "Petal_Width"=1.05),
                                             "value_scales"="figure"),
                              sample_limit=20L,
                              n_sample_points=10L,
                              debug=debug_flag)

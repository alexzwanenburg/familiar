familiar:::test_all_vimp_methods_available(familiar:::.get_available_corelearn_gini_vimp_method(show_general=TRUE))
familiar:::test_all_vimp_methods_available(familiar:::.get_available_corelearn_mdl_vimp_method(show_general=TRUE))
familiar:::test_all_vimp_methods_available(familiar:::.get_available_corelearn_relieff_exp_rank_vimp_method(show_general=TRUE))
familiar:::test_all_vimp_methods_available(familiar:::.get_available_corelearn_gain_ratio_vimp_method(show_general=TRUE))

# Don't perform any further tests on CRAN due to running time.
testthat::skip_on_cran()

familiar:::test_hyperparameter_optimisation(vimp_methods=familiar:::.get_available_corelearn_gini_vimp_method(show_general=TRUE),
                                            debug=FALSE,
                                            parallel=FALSE,
                                            no_hyperparameters=TRUE)
familiar:::test_all_vimp_methods(familiar:::.get_available_corelearn_gini_vimp_method(show_general=FALSE))
familiar:::test_all_vimp_methods_parallel(familiar:::.get_available_corelearn_gini_vimp_method(show_general=FALSE))


familiar:::test_hyperparameter_optimisation(vimp_methods=familiar:::.get_available_corelearn_mdl_vimp_method(show_general=TRUE),
                                            debug=FALSE,
                                            parallel=FALSE,
                                            no_hyperparameters=TRUE)
familiar:::test_all_vimp_methods(familiar:::.get_available_corelearn_mdl_vimp_method(show_general=FALSE))
familiar:::test_all_vimp_methods_parallel(familiar:::.get_available_corelearn_mdl_vimp_method(show_general=FALSE))


familiar:::test_hyperparameter_optimisation(vimp_methods=familiar:::.get_available_corelearn_relieff_exp_rank_vimp_method(show_general=TRUE),
                                            debug=FALSE,
                                            parallel=FALSE,
                                            no_hyperparameters=TRUE)
familiar:::test_all_vimp_methods(familiar:::.get_available_corelearn_relieff_exp_rank_vimp_method(show_general=FALSE))
familiar:::test_all_vimp_methods_parallel(familiar:::.get_available_corelearn_relieff_exp_rank_vimp_method(show_general=FALSE))


familiar:::test_hyperparameter_optimisation(vimp_methods=familiar:::.get_available_corelearn_gain_ratio_vimp_method(show_general=TRUE),
                                            debug=FALSE,
                                            parallel=FALSE,
                                            no_hyperparameters=TRUE)
familiar:::test_all_vimp_methods(familiar:::.get_available_corelearn_gain_ratio_vimp_method(show_general=FALSE))
familiar:::test_all_vimp_methods_parallel(familiar:::.get_available_corelearn_gain_ratio_vimp_method(show_general=FALSE))

##### Count outcome #####
data <- familiar:::test.create_good_data_set("count")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="relieff_exp_rank",
                                              vimp_method_parameter_list=NULL,
                                              outcome_type="count",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("RReliefF exponentially decreasing ranks method correctly ranks count data."), {
  
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("per_capita_crime", "lower_status_percentage", "avg_rooms")), TRUE)
})



##### Continuous outcome #####
data <- familiar:::test.create_good_data_set("continuous")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="relieff_exp_rank",
                                              vimp_method_parameter_list=NULL,
                                              outcome_type="continuous",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("RReliefF exponentially decreasing ranks method correctly ranks continuous data."), {
  
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
  
  testthat::expect_equal(any(vimp_table[rank <= 2]$name %in% c("avginc", "calwpct")), TRUE)
})



##### Binomial outcome #####
data <- familiar:::test.create_good_data_set("binomial")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="relieff_exp_rank",
                                              vimp_method_parameter_list=NULL,
                                              outcome_type="binomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("RReliefF exponentially decreasing ranks method correctly ranks binomial data."), {
  
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("cell_shape_uniformity", "clump_thickness", "bare_nuclei")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="gini",
                                              vimp_method_parameter_list=NULL,
                                              outcome_type="binomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The Gini method correctly ranks binomial data."), {
  
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("cell_shape_uniformity", "clump_thickness", "bare_nuclei", "normal_nucleoli")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="gain_ratio",
                                              vimp_method_parameter_list=NULL,
                                              outcome_type="binomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The gain ratio method correctly ranks binomial data."), {
  
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("cell_shape_uniformity", "clump_thickness", "bare_nuclei")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="mdl",
                                              vimp_method_parameter_list=NULL,
                                              outcome_type="binomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The MDL method correctly ranks binomial data."), {
  
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("cell_shape_uniformity", "clump_thickness", "bare_nuclei", "normal_nucleoli")), TRUE)
})


##### Multinomial outcome #####
data <- familiar:::test.create_good_data_set("multinomial")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="relieff_exp_rank",
                                              vimp_method_parameter_list=NULL,
                                              outcome_type="multinomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("RReliefF exponentially decreasing ranks method correctly ranks multinomial outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("Petal_Length", "Petal_Width")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="gini",
                                              vimp_method_parameter_list=NULL,
                                              outcome_type="multinomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The Gini method correctly ranks multinomial outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("Petal_Length", "Petal_Width")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="gain_ratio",
                                              vimp_method_parameter_list=NULL,
                                              outcome_type="multinomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The gain ratio method correctly ranks multinomial outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("Petal_Length", "Petal_Width")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="mdl",
                                              vimp_method_parameter_list=NULL,
                                              outcome_type="multinomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The MDL method correctly ranks multinomial outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("Petal_Length", "Petal_Width")), TRUE)
})

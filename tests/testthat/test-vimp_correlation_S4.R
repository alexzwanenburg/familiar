familiar:::test_all_vimp_methods_available(familiar:::.get_available_correlation_vimp_methods(show_general=TRUE))
familiar:::test_all_vimp_methods(familiar:::.get_available_correlation_vimp_methods(show_general=FALSE))


##### Count outcome #####
data <- familiar:::test.create_good_data_set("count")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="spearman",
                                              vimp_method_parameter_list=NULL,
                                              outcome_type="count",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("Spearman correlation correctly ranks count data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("per_capita_crime", "lower_status_percentage")), TRUE)
})



##### Continuous outcome #####
data <- familiar:::test.create_good_data_set("continuous")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="spearman",
                                              vimp_method_parameter_list=NULL,
                                              outcome_type="continuous",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("Spearman correlation correctly ranks continuous data.."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(any(vimp_table[rank <= 2]$name %in% c("enrltot", "avginc", "calwpct")), TRUE)
})



##### Survival outcome #####
data <- familiar:::test.create_good_data_set("survival")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="spearman",
                                              vimp_method_parameter_list=NULL,
                                              outcome_type="survival",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("Spearman correlation correctly ranks survival outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("nodes", "rx")), TRUE)
})

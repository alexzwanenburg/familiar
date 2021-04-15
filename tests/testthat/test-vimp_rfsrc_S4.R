familiar:::test_all_vimp_methods_available(familiar:::.get_available_rfsrc_vimp_methods(show_general=TRUE))

# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

familiar:::test_hyperparameter_optimisation(vimp_methods=familiar:::.get_available_rfsrc_vimp_methods(show_general=TRUE),
                                            debug=TRUE,
                                            parallel=FALSE)

familiar:::test_all_vimp_methods(familiar:::.get_available_rfsrc_vimp_methods(show_general=FALSE),
                                 debug=TRUE,
                                 hyperparameter_list=list("count"=list("n_tree"=4,
                                                                       "sample_size"=0.50,
                                                                       "m_try"=0.3,
                                                                       "node_size"=5,
                                                                       "tree_depth"=5,
                                                                       "fs_vh_fold"=3,
                                                                       "fs_vh_n_rep"=2),
                                                          "continuous"=list("n_tree"=4,
                                                                            "sample_size"=0.50,
                                                                            "m_try"=0.3,
                                                                            "node_size"=5,
                                                                            "tree_depth"=5,
                                                                            "fs_vh_fold"=3,
                                                                            "fs_vh_n_rep"=2),
                                                          "binomial"=list("n_tree"=4,
                                                                          "sample_size"=0.50,
                                                                          "m_try"=0.3,
                                                                          "node_size"=5,
                                                                          "tree_depth"=5,
                                                                          "fs_vh_fold"=3,
                                                                          "fs_vh_n_rep"=2),
                                                          "multinomial"=list("n_tree"=4,
                                                                             "sample_size"=0.50,
                                                                             "m_try"=0.3,
                                                                             "node_size"=5,
                                                                             "tree_depth"=5,
                                                                             "fs_vh_fold"=3,
                                                                             "fs_vh_n_rep"=2),
                                                          "survival"=list("n_tree"=4,
                                                                          "sample_size"=0.50,
                                                                          "m_try"=0.3,
                                                                          "node_size"=5,
                                                                          "tree_depth"=5,
                                                                          "fs_vh_fold"=3,
                                                                          "fs_vh_n_rep"=2)))


##### Count outcome #####
data <- familiar:::test.create_good_data_set("count")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_minimum_depth",
                                              vimp_method_parameter_list=list("n_tree"=8,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5),
                                              outcome_type="count",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest minimum depth method correctly ranks count data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("per_capita_crime", "lower_status_percentage",
                                                               "residence_before_1940_proportion", "avg_rooms")), TRUE)
})

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_permutation",
                                              vimp_method_parameter_list=list("n_tree"=8,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5),
                                              outcome_type="count",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest permutation method correctly ranks count data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("per_capita_crime", "lower_status_percentage",
                                                               "residence_before_1940_proportion", "avg_rooms",
                                                               "industry")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_holdout",
                                              vimp_method_parameter_list=list("n_tree"=12,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5),
                                              outcome_type="count",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest hold-out method correctly ranks count data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("per_capita_crime", "lower_status_percentage",
                                                               "residence_before_1940_proportion", "avg_rooms",
                                                               "property_tax_rate")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_variable_hunting",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "fs_vh_fold"=3,
                                                                              "fs_vh_n_rep"=3),
                                              outcome_type="count",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest variable hunting method correctly ranks count data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% familiar:::get_feature_columns(data)), TRUE)
})


##### Continuous outcome #####
data <- familiar:::test.create_good_data_set("continuous")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_minimum_depth",
                                              vimp_method_parameter_list=list("n_tree"=8,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5),
                                              outcome_type="continuous",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest minimum depth method correctly ranks continuous data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(any(vimp_table[rank <= 2]$name %in% c("enrltot", "avginc", "calwpct")), TRUE)
})

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_permutation",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5),
                                              outcome_type="continuous",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest permutation method correctly ranks continuous data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(any(vimp_table[rank <= 2]$name %in% c("enrltot", "avginc", "calwpct")), TRUE)
})

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_holdout",
                                              vimp_method_parameter_list=list("n_tree"=8,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5),
                                              outcome_type="continuous",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest hold-out method correctly ranks continuous data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(any(vimp_table[rank <= 2]$name %in% c("enrltot", "avginc", "calwpct")), TRUE)
})

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_variable_hunting",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "fs_vh_fold"=3,
                                                                              "fs_vh_n_rep"=3),
                                              outcome_type="continuous",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest variable hunting method correctly ranks continuous data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% familiar:::get_feature_columns(data)), TRUE)
})


##### Binomial outcome #####
data <- familiar:::test.create_good_data_set("binomial")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_minimum_depth",
                                              vimp_method_parameter_list=list("n_tree"=8,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5),
                                              outcome_type="binomial",
                                              cluster_method="none",
                                              imputation_method="simple")

testthat::test_that(paste0("The RFSRC random forest minimum depth method correctly ranks binomial data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("cell_shape_uniformity", "clump_thickness",
                                                               "epithelial_cell_size", "bare_nuclei")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_permutation",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5),
                                              outcome_type="binomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest permutation method correctly ranks binomial data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("cell_shape_uniformity", "clump_thickness",
                                                               "epithelial_cell_size", "bare_nuclei")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_holdout",
                                              vimp_method_parameter_list=list("n_tree"=8,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5),
                                              outcome_type="binomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest hold-out method correctly ranks binomial data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% familiar:::get_feature_columns(data)), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_variable_hunting",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "fs_vh_fold"=3,
                                                                              "fs_vh_n_rep"=3),
                                              outcome_type="binomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest variable hunting method correctly ranks binomial data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% familiar:::get_feature_columns(data)), TRUE)
})



##### Multinomial outcome #####
data <- familiar:::test.create_good_data_set("multinomial")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_minimum_depth",
                                              vimp_method_parameter_list=list("n_tree"=8,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5),
                                              outcome_type="multinomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest minimum depth method correctly ranks multinomial outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("Petal_Length", "Petal_Width")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_permutation",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5),
                                              outcome_type="multinomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest permutation method correctly ranks multinomial outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("Petal_Length", "Petal_Width")), TRUE)
})

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_holdout",
                                              vimp_method_parameter_list=list("n_tree"=8,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5),
                                              outcome_type="multinomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest hold-out method correctly ranks multinomial outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% familiar:::get_feature_columns(data)), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_variable_hunting",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "fs_vh_fold"=3,
                                                                              "fs_vh_n_rep"=3),
                                              outcome_type="multinomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest variable hunting method correctly ranks multinomial outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% familiar:::get_feature_columns(data)), TRUE)
})



##### Survival outcome #####
data <- familiar:::test.create_good_data_set("survival")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_minimum_depth",
                                              vimp_method_parameter_list=list("n_tree"=8,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5),
                                              outcome_type="survival",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest minimum depth method correctly ranks survival outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("nodes", "rx")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_permutation",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5),
                                              outcome_type="survival",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest permutation method correctly ranks survival outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("nodes", "rx")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_holdout",
                                              vimp_method_parameter_list=list("n_tree"=8,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5),
                                              outcome_type="survival",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest hold-out method correctly ranks survival outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("nodes", "rx")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_rfsrc_variable_hunting",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=0.50,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "fs_vh_fold"=3,
                                                                              "fs_vh_n_rep"=3),
                                              outcome_type="survival",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The RFSRC random forest variable hunting method correctly ranks survival outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% familiar:::get_feature_columns(data)), TRUE)
})

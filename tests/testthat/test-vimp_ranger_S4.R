familiar:::test_all_vimp_methods_available(familiar:::.get_available_ranger_vimp_methods(show_general=TRUE))

# Don't perform any further tests on CRAN due to time of running the complete test.
testthat::skip_on_cran()

familiar:::test_all_vimp_methods(familiar:::.get_available_ranger_vimp_methods(show_general=FALSE),
                                 hyperparameter_list=list("count"=list("n_tree"=4,
                                                                       "sample_size"=1.00,
                                                                       "m_try"=0.3,
                                                                       "node_size"=5,
                                                                       "tree_depth"=5,
                                                                       "alpha"=0.1),
                                                          "continuous"=list("n_tree"=4,
                                                                            "sample_size"=1.00,
                                                                            "m_try"=0.3,
                                                                            "node_size"=5,
                                                                            "tree_depth"=5,
                                                                            "alpha"=0.1),
                                                          "binomial"=list("n_tree"=4,
                                                                          "sample_size"=1.00,
                                                                          "m_try"=0.3,
                                                                          "node_size"=5,
                                                                          "tree_depth"=5,
                                                                          "alpha"=0.1),
                                                          "multinomial"=list("n_tree"=4,
                                                                             "sample_size"=1.00,
                                                                             "m_try"=0.3,
                                                                             "node_size"=5,
                                                                             "tree_depth"=5,
                                                                             "alpha"=0.1),
                                                          "survival"=list("n_tree"=4,
                                                                          "sample_size"=1.00,
                                                                          "m_try"=0.3,
                                                                          "node_size"=5,
                                                                          "tree_depth"=5,
                                                                          "alpha"=0.1)))

# Parallel test.
familiar:::test_all_vimp_methods_parallel(familiar:::.get_available_ranger_vimp_methods(show_general=FALSE),
                                          hyperparameter_list=list("count"=list("n_tree"=4,
                                                                                "sample_size"=1.00,
                                                                                "m_try"=0.3,
                                                                                "node_size"=5,
                                                                                "tree_depth"=5,
                                                                                "alpha"=0.1),
                                                                   "continuous"=list("n_tree"=4,
                                                                                     "sample_size"=1.00,
                                                                                     "m_try"=0.3,
                                                                                     "node_size"=5,
                                                                                     "tree_depth"=5,
                                                                                     "alpha"=0.1),
                                                                   "binomial"=list("n_tree"=4,
                                                                                   "sample_size"=1.00,
                                                                                   "m_try"=0.3,
                                                                                   "node_size"=5,
                                                                                   "tree_depth"=5,
                                                                                   "alpha"=0.1),
                                                                   "multinomial"=list("n_tree"=4,
                                                                                      "sample_size"=1.00,
                                                                                      "m_try"=0.3,
                                                                                      "node_size"=5,
                                                                                      "tree_depth"=5,
                                                                                      "alpha"=0.1),
                                                                   "survival"=list("n_tree"=4,
                                                                                   "sample_size"=1.00,
                                                                                   "m_try"=0.3,
                                                                                   "node_size"=5,
                                                                                   "tree_depth"=5,
                                                                                   "alpha"=0.1)))

##### Count outcome #####
data <- familiar:::test.create_good_data_set("count")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_impurity",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=1.00,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "alpha"=0.1),
                                              outcome_type="count",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The ranger random forest impurity method correctly ranks count data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("per_capita_crime", "lower_status_percentage",
                                                               "residence_before_1940_proportion", "avg_rooms")), TRUE)
})

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_permutation",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=1.00,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "alpha"=0.1),
                                              outcome_type="count",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The ranger random forest permutation method correctly ranks count data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(any(vimp_table[rank <= 2]$name %in% c("per_capita_crime",
                                                               "lower_status_percentage",
                                                               "residence_before_1940_proportion",
                                                               "avg_rooms")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_holdout_permutation",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=1.00,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "alpha"=0.1),
                                              outcome_type="count",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The ranger random forest hold-out permutation method correctly ranks count data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(any(vimp_table[rank <= 2]$name %in% c("per_capita_crime", "lower_status_percentage",
                                                               "residence_before_1940_proportion", "avg_rooms")), TRUE)
})



##### Continuous outcome #####
data <- familiar:::test.create_good_data_set("continuous")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_impurity",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=1.00,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "alpha"=0.1),
                                              outcome_type="continuous",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The ranger random forest impurity method correctly ranks continuous data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(any(vimp_table[rank <= 2]$name %in% c("enrltot", "avginc", "calwpct")), TRUE)
})

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_permutation",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=1.00,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "alpha"=0.1),
                                              outcome_type="continuous",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The ranger random forest permutation method correctly ranks continuous data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(any(vimp_table[rank <= 2]$name %in% c("enrltot", "avginc", "calwpct")), TRUE)
})

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_holdout_permutation",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=1.00,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "alpha"=0.1),
                                              outcome_type="continuous",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The ranger random forest hold-out permutation method correctly ranks continuous data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(any(vimp_table[rank <= 2]$name %in% c("enrltot", "avginc", "calwpct")), TRUE)
})



##### Binomial outcome #####
data <- familiar:::test.create_good_data_set("binomial")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_impurity",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=1.00,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "alpha"=0.1),
                                              outcome_type="binomial",
                                              cluster_method="none",
                                              imputation_method="simple")

testthat::test_that(paste0("The ranger random forest impurity method correctly ranks binomial data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(any(vimp_table[rank <= 2]$name %in% c("cell_shape_uniformity",
                                                               "clump_thickness",
                                                               "epithelial_cell_size",
                                                               "bare_nuclei")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_permutation",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=1.00,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "alpha"=0.1),
                                              outcome_type="binomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The ranger random forest permutation method correctly ranks binomial data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(any(vimp_table[rank <= 2]$name %in% c("cell_shape_uniformity", "clump_thickness",
                                                               "epithelial_cell_size", "bare_nuclei")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_holdout_permutation",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=1.00,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "alpha"=0.1),
                                              outcome_type="binomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The ranger random forest hold-out permutation method correctly ranks binomial data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(any(vimp_table[rank <= 2]$name %in% c("cell_shape_uniformity", "clump_thickness",
                                                               "epithelial_cell_size", "bare_nuclei")), TRUE)
})

##### Multinomial outcome #####
data <- familiar:::test.create_good_data_set("multinomial")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_impurity",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=1.00,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "alpha"=0.1),
                                              outcome_type="multinomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The ranger random forest impurity method correctly ranks multinomial outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("Petal_Length", "Petal_Width")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_permutation",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=1.00,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "alpha"=0.1),
                                              outcome_type="multinomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The ranger random forest permutation method correctly ranks multinomial outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("Petal_Length", "Petal_Width")), TRUE)
})

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_holdout_permutation",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=1.00,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "alpha"=0.1),
                                              outcome_type="multinomial",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The ranger random forest hold-out permutation method correctly ranks multinomial outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("Petal_Length", "Petal_Width")), TRUE)
})



##### Survival outcome #####
data <- familiar:::test.create_good_data_set("survival")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_impurity",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=1.00,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "alpha"=0.1),
                                              outcome_type="survival",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The ranger random forest impurity method correctly ranks survival outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("nodes", "rx", "adhere")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_permutation",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=1.00,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "alpha"=0.1),
                                              outcome_type="survival",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The ranger random forest permutation method correctly ranks survival outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("nodes", "rx", "adhere")), TRUE)
})


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_holdout_permutation",
                                              vimp_method_parameter_list=list("n_tree"=4,
                                                                              "sample_size"=1.00,
                                                                              "m_try"=0.3,
                                                                              "node_size"=5,
                                                                              "tree_depth"=5,
                                                                              "alpha"=0.1),
                                              outcome_type="survival",
                                              cluster_method="none",
                                              imputation_method="simple")


testthat::test_that(paste0("The ranger random forest hold-out permutation method correctly ranks survival outcome data."), {
  
  vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))
  
  testthat::expect_equal(all(vimp_table[rank <= 2]$name %in% c("nodes", "rx", "adhere")), TRUE)
})


testthat::skip("Skip hyperparameter optimisation, unless manual.")

familiar:::test_hyperparameter_optimisation(vimp_methods=familiar:::.get_available_ranger_vimp_methods(show_general=TRUE),
                                            debug=FALSE,
                                            parallel=FALSE)

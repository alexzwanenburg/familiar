data <- familiar:::test_create_synthetic_correlated_data(outcome_type="continuous",
                                                         n_numeric=4.0,
                                                         cluster_size=c(2, 3, 2, 3))

vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="random_forest_ranger_permutation_default",
                                              outcome_type="continuous",
                                              transformation_method="none",
                                              normalisation_method="none",
                                              cluster_method="hclust",
                                              cluster_cut_method="fixed_cut",
                                              cluster_similarity_metric="mcfadden_r2",
                                              cluster_similarity_threshold=0.99,
                                              imputation_method="simple")

vimp_table <- suppressWarnings(familiar:::.vimp(vimp_object, data))

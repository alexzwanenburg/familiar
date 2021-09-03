testthat::skip_on_cran()

debug_flag <- FALSE

# Simple test
familiar:::integrated_test(experimental_design="fs+mb",
                           fs_method="none",
                           cluster_method="none",
                           imputation_method="simple",
                           parallel=FALSE,
                           skip_evaluation_elements="all",
                           outcome_type_available="binomial",
                           debug=debug_flag)

# Bootstrap (without optimisation within bootstraps)
familiar:::integrated_test(experimental_design="bt(fs+mb, 5)",
                           fs_method="none",
                           cluster_method="none",
                           imputation_method="simple",
                           parallel=FALSE,
                           skip_evaluation_elements="all",
                           outcome_type_available="binomial",
                           debug=debug_flag)

# Cross-validation
familiar:::integrated_test(experimental_design="cv(fs+mb, 3)",
                           fs_method="none",
                           cluster_method="none",
                           imputation_method="simple",
                           parallel=FALSE,
                           skip_evaluation_elements="all",
                           outcome_type_available="binomial",
                           debug=debug_flag)

# Leave-one-out cross-validation
familiar:::integrated_test(experimental_design="lv(fs+mb)",
                           fs_method="none",
                           cluster_method="none",
                           imputation_method="simple",
                           parallel=FALSE,
                           skip_evaluation_elements="all",
                           outcome_type_available="binomial",
                           debug=debug_flag)

# Imbalance corrections using full undersampling
familiar:::integrated_test(experimental_design="ip(fs+mb)",
                           imbalance_correction_method="full_undersampling",
                           fs_method="none",
                           cluster_method="none",
                           imputation_method="simple",
                           parallel=FALSE,
                           skip_evaluation_elements="all",
                           outcome_type_available="binomial",
                           debug=debug_flag)

# Imbalance corrections using full undersampling
familiar:::integrated_test(experimental_design="ip(fs+mb)",
                           imbalance_correction_method="random_undersampling",
                           imbalance_n_partitions=3,
                           fs_method="none",
                           cluster_method="none",
                           imputation_method="simple",
                           parallel=FALSE,
                           skip_evaluation_elements="all",
                           outcome_type_available="binomial",
                           debug=debug_flag)

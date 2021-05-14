testthat::skip_on_cran()

# Perform an integrated test. Note that learner is set automatically.
familiar:::integrated_test(dynamic_model_loading=TRUE,
                           experimental_design="fs+mb",
                           fs_method="none",
                           cluster_method="none",
                           imputation_method="simple",
                           parallel=FALSE,
                           estimation_type="point")

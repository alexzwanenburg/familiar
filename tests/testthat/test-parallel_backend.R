testthat::skip_on_cran()

# Perform an integrated test. Note that learner is set automatically.
familiar:::integrated_test(backend_type="none",
                           experimental_design="bt(fs+mb,20)",
                           parallel_nr_cores=2,
                           fs_method="none",
                           cluster_method="none",
                           imputation_method="simple",
                           parallel=TRUE,
                           skip_evaluation_elements="all")

# Perform an integrated test using the socket backend.
familiar:::integrated_test(backend_type="socket_server",
                           experimental_design="bt(fs+mb,20)",
                           parallel_nr_cores=2,
                           fs_method="none",
                           cluster_method="none",
                           imputation_method="simple",
                           parallel=TRUE,
                           skip_evaluation_elements="all")

# Test external 

# Start cluster
cl <- familiar:::.start_cluster(n_cores=2L, cluster_type="psock")

familiar:::integrated_test(backend_type="none",
                           cl=cl,
                           experimental_design="bt(fs+mb,20)",
                           fs_method="mrmr",
                           cluster_method="none",
                           imputation_method="simple",
                           parallel=TRUE,
                           skip_evaluation_elements="all",
                           debug=TRUE)

cl <- familiar:::.terminate_cluster(cl=cl)

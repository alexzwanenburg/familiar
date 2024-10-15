testthat::skip_on_cran()
testthat::skip_on_ci()

debug_flag <- FALSE

# Test that creates a single naive model.
familiar:::integrated_test(
  experimental_design = "fs+mb",
  vimp_method = "no_features",
  cluster_method = "none",
  imputation_method = "simple",
  estimation_type = "point",
  parallel = FALSE,
  debug = debug_flag)

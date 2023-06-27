familiar:::test_all_vimp_methods_available(
  familiar:::.get_available_correlation_vimp_methods(show_general = TRUE))

# Don't perform any further tests on CRAN due to running time.
testthat::skip_on_cran()

familiar:::test_hyperparameter_optimisation(
  vimp_methods = familiar:::.get_available_correlation_vimp_methods(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE
)
familiar:::test_all_vimp_methods(
  familiar:::.get_available_correlation_vimp_methods(show_general = FALSE))
familiar:::test_all_vimp_methods_parallel(
  familiar:::.get_available_correlation_vimp_methods(show_general = FALSE))

# Continuous outcome -----------------------------------------------------------
data <- familiar:::test_create_good_data("continuous")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "spearman",
  vimp_method_parameter_list = NULL,
  outcome_type = "continuous",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(paste0("Spearman correlation correctly ranks continuous data."), {
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(
    familiar:::.vimp(vimp_object, data)))

  testthat::expect_true(any(vimp_table[rank <= 2]$name %in% c("enrltot", "avginc", "calwpct")))
})

# Survival outcome -------------------------------------------------------------
data <- familiar:::test_create_good_data("survival")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "spearman",
  vimp_method_parameter_list = NULL,
  outcome_type = "survival",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(paste0(
  "Spearman correlation correctly ranks survival outcome data."), {
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(
    familiar:::.vimp(vimp_object, data)))

  testthat::expect_true(all(vimp_table[rank <= 2]$name %in% c("nodes", "rx")))
})

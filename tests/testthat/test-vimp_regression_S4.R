familiar:::test_all_vimp_methods_available(
  familiar:::.get_available_univariate_regression_vimp_methods(show_general = TRUE)
)
familiar:::test_all_vimp_methods_available(
  familiar:::.get_available_multivariate_regression_vimp_methods(show_general = TRUE)
)

# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()
testthat::skip_on_ci()

familiar:::test_hyperparameter_optimisation(
  vimp_methods = familiar:::.get_available_univariate_regression_vimp_methods(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE,
  not_available_no_samples = FALSE
)

familiar:::test_all_vimp_methods(
  familiar:::.get_available_univariate_regression_vimp_methods(show_general = FALSE)
)
familiar:::test_all_vimp_methods_parallel(
  familiar:::.get_available_univariate_regression_vimp_methods(show_general = FALSE)
)

familiar:::test_hyperparameter_optimisation(
  vimp_methods = familiar:::.get_available_multivariate_regression_vimp_methods(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE,
  not_available_no_samples = FALSE
)

familiar:::test_all_vimp_methods(
  familiar:::.get_available_multivariate_regression_vimp_methods(show_general = FALSE)
)
familiar:::test_all_vimp_methods_parallel(
  familiar:::.get_available_multivariate_regression_vimp_methods(show_general = FALSE)
)


# Continuous outcome -----------------------------------------------------------
data <- familiar:::test_create_good_data("continuous")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "multivariate_regression",
  vimp_method_parameter_list = NULL,
  outcome_type = "continuous",
  cluster_method = "none",
  imputation_method = "simple"
)

testthat::test_that(paste0("Multivariate regression correctly ranks continuous data."), {
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(
    familiar:::.vimp(vimp_object, data)
  ))

  # Expect that the vimp table has at most six rows.
  testthat::expect_lte(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(vimp_table$name %in% familiar:::get_feature_columns(data))
  )
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})


# Binomial outcome -------------------------------------------------------------
data <- familiar:::test_create_good_data("binomial")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "multivariate_regression",
  vimp_method_parameter_list = NULL,
  outcome_type = "binomial",
  cluster_method = "none",
  imputation_method = "simple"
)

testthat::test_that(paste0("Multivariate regression correctly ranks binomial data.."), {
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(
    familiar:::.vimp(vimp_object, data))
  )
  
  # Expect that the vimp table has at most six rows.
  testthat::expect_lte(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(vimp_table$name %in% familiar:::get_feature_columns(data))
  )
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})


# Multinomial outcome ----------------------------------------------------------
data <- familiar:::test_create_good_data("multinomial")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "multivariate_regression",
  vimp_method_parameter_list = NULL,
  outcome_type = "multinomial",
  cluster_method = "none",
  imputation_method = "simple"
)

testthat::test_that(paste0("Multivariate regression correctly ranks multinomial outcome data."), {
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(
    familiar:::.vimp(vimp_object, data))
  )
  
  # Expect that the vimp table has at most six rows.
  testthat::expect_lte(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(vimp_table$name %in% familiar:::get_feature_columns(data))
  )
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})


# Survival outcome -------------------------------------------------------------
data <- familiar:::test_create_good_data("survival")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "multivariate_regression",
  vimp_method_parameter_list = NULL,
  outcome_type = "survival",
  cluster_method = "none",
  imputation_method = "simple"
)

testthat::test_that(paste0("Multivariate regression correctly ranks survival outcome data."), {
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(
    familiar:::.vimp(vimp_object, data))
  )
  
  # Expect that the vimp table has at most six rows.
  testthat::expect_lte(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(vimp_table$name %in% familiar:::get_feature_columns(data))
  )
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})

familiar:::test_all_vimp_methods_available(
  familiar:::.get_available_corelearn_gini_vimp_method(show_general = TRUE))
familiar:::test_all_vimp_methods_available(
  familiar:::.get_available_corelearn_mdl_vimp_method(show_general = TRUE))
familiar:::test_all_vimp_methods_available(
  familiar:::.get_available_corelearn_relieff_exp_rank_vimp_method(show_general = TRUE))
familiar:::test_all_vimp_methods_available(
  familiar:::.get_available_corelearn_gain_ratio_vimp_method(show_general = TRUE))

# Don't perform any further tests on CRAN due to running time.
testthat::skip_on_cran()

familiar:::test_hyperparameter_optimisation(
  vimp_methods = familiar:::.get_available_corelearn_gini_vimp_method(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE)
familiar:::test_all_vimp_methods(
  familiar:::.get_available_corelearn_gini_vimp_method(show_general = FALSE))
familiar:::test_all_vimp_methods_parallel(
  familiar:::.get_available_corelearn_gini_vimp_method(show_general = FALSE))


familiar:::test_hyperparameter_optimisation(
  vimp_methods = familiar:::.get_available_corelearn_mdl_vimp_method(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE)
familiar:::test_all_vimp_methods(
  familiar:::.get_available_corelearn_mdl_vimp_method(show_general = FALSE))
familiar:::test_all_vimp_methods_parallel(
  familiar:::.get_available_corelearn_mdl_vimp_method(show_general = FALSE))


familiar:::test_hyperparameter_optimisation(
  vimp_methods = familiar:::.get_available_corelearn_relieff_exp_rank_vimp_method(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE)
familiar:::test_all_vimp_methods(
  familiar:::.get_available_corelearn_relieff_exp_rank_vimp_method(show_general = FALSE))
familiar:::test_all_vimp_methods_parallel(
  familiar:::.get_available_corelearn_relieff_exp_rank_vimp_method(show_general = FALSE))


familiar:::test_hyperparameter_optimisation(
  vimp_methods = familiar:::.get_available_corelearn_gain_ratio_vimp_method(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE)
familiar:::test_all_vimp_methods(
  familiar:::.get_available_corelearn_gain_ratio_vimp_method(show_general = FALSE))
familiar:::test_all_vimp_methods_parallel(
  familiar:::.get_available_corelearn_gain_ratio_vimp_method(show_general = FALSE))

# Continuous outcome -----------------------------------------------------------
data <- familiar:::test_create_good_data("continuous")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "relieff_exp_rank",
  vimp_method_parameter_list = NULL,
  outcome_type = "continuous",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(paste0(
  "RReliefF exponentially decreasing ranks method correctly ranks continuous data."), {
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(
    familiar:::.vimp(vimp_object, data)))

  # Expect that the vimp table has six rows.
  testthat::expect_equal(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(familiar:::get_feature_columns(data) %in% vimp_table$name))
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})


# Binomial outcome -------------------------------------------------------------
data <- familiar:::test_create_good_data("binomial")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "relieff_exp_rank",
  vimp_method_parameter_list = NULL,
  outcome_type = "binomial",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(paste0(
  "RReliefF exponentially decreasing ranks method correctly ranks binomial data."), {
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(
    familiar:::.vimp(vimp_object, data)))

  # Expect that the vimp table has six rows.
  testthat::expect_equal(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(familiar:::get_feature_columns(data) %in% vimp_table$name))
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "gini",
  vimp_method_parameter_list = NULL,
  outcome_type = "binomial",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(paste0("The Gini method correctly ranks binomial data."), {
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(
    familiar:::.vimp(vimp_object, data)))

  # Expect that the vimp table has six rows.
  testthat::expect_equal(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(familiar:::get_feature_columns(data) %in% vimp_table$name))
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "gain_ratio",
  vimp_method_parameter_list = NULL,
  outcome_type = "binomial",
  cluster_method = "none",
  imputation_method = "simple"
)

testthat::test_that(paste0("The gain ratio method correctly ranks binomial data."), {
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(
    familiar:::.vimp(vimp_object, data)))

  # Expect that the vimp table has six rows.
  testthat::expect_equal(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(familiar:::get_feature_columns(data) %in% vimp_table$name))
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "mdl",
  vimp_method_parameter_list = NULL,
  outcome_type = "binomial",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(paste0("The MDL method correctly ranks binomial data."), {
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(
    familiar:::.vimp(vimp_object, data)))

  # Expect that the vimp table has six rows.
  testthat::expect_equal(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(familiar:::get_feature_columns(data) %in% vimp_table$name))
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})


# Multinomial outcome ----------------------------------------------------------
data <- familiar:::test_create_good_data("multinomial")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "relieff_exp_rank",
  vimp_method_parameter_list = NULL,
  outcome_type = "multinomial",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(paste0(
  "RReliefF exponentially decreasing ranks method correctly ranks multinomial outcome data."), {
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(
    familiar:::.vimp(vimp_object, data)))

  # Expect that the vimp table has six rows.
  testthat::expect_equal(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(familiar:::get_feature_columns(data) %in% vimp_table$name))
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "gini",
  vimp_method_parameter_list = NULL,
  outcome_type = "multinomial",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(paste0("The Gini method correctly ranks multinomial outcome data."), {
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(
    familiar:::.vimp(vimp_object, data)))

  # Expect that the vimp table has six rows.
  testthat::expect_equal(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(familiar:::get_feature_columns(data) %in% vimp_table$name))
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "gain_ratio",
  vimp_method_parameter_list = NULL,
  outcome_type = "multinomial",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(paste0("The gain ratio method correctly ranks multinomial outcome data."), {
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(
    familiar:::.vimp(vimp_object, data)))

  # Expect that the vimp table has six rows.
  testthat::expect_equal(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(familiar:::get_feature_columns(data) %in% vimp_table$name))
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "mdl",
  vimp_method_parameter_list = NULL,
  outcome_type = "multinomial",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(paste0("The MDL method correctly ranks multinomial outcome data."), {
  vimp_table <- suppressWarnings(familiar:::get_vimp_table(
    familiar:::.vimp(vimp_object, data)))

  # Expect that the vimp table has six rows.
  testthat::expect_equal(nrow(vimp_table), 6L)
  
  # Expect that the names are the same as that of the features.
  testthat::expect_true(
    all(familiar:::get_feature_columns(data) %in% vimp_table$name))
  
  # Feature 1 is most important.
  testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
})

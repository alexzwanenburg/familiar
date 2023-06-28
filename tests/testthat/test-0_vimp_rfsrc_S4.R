familiar:::test_all_vimp_methods_available(
  familiar:::.get_available_rfsrc_vimp_methods(show_general = TRUE))
familiar:::test_all_vimp_methods_available(
  familiar:::.get_available_rfsrc_default_vimp_methods(show_general = TRUE))

# Don't perform any further tests on CRAN due to time of running the complete
# test.
testthat::skip_on_cran()

familiar:::test_all_vimp_methods(
  familiar:::.get_available_rfsrc_vimp_methods(show_general = FALSE),
  debug = FALSE,
  hyperparameter_list = list(
    "continuous" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "fs_vh_fold" = 3,
      "fs_vh_n_rep" = 2
    ),
    "binomial" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "fs_vh_fold" = 3,
      "fs_vh_n_rep" = 2
    ),
    "multinomial" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "fs_vh_fold" = 3,
      "fs_vh_n_rep" = 2
    ),
    "survival" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "fs_vh_fold" = 3,
      "fs_vh_n_rep" = 2
    )
  )
)

familiar:::test_all_vimp_methods(
  familiar:::.get_available_rfsrc_default_vimp_methods())

familiar:::test_all_vimp_methods_parallel(
  familiar:::.get_available_rfsrc_vimp_methods(show_general = FALSE),
  hyperparameter_list = list(
    "continuous" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "fs_vh_fold" = 3,
      "fs_vh_n_rep" = 2
    ),
    "binomial" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "fs_vh_fold" = 3,
      "fs_vh_n_rep" = 2
    ),
    "multinomial" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "fs_vh_fold" = 3,
      "fs_vh_n_rep" = 2
    ),
    "survival" = list(
      "n_tree" = 4,
      "sample_size" = 0.50,
      "m_try" = 0.3,
      "node_size" = 5,
      "tree_depth" = 5,
      "fs_vh_fold" = 3,
      "fs_vh_n_rep" = 2
    )
  )
)


# Continuous outcome -----------------------------------------------------------
data <- familiar:::test_create_good_data("continuous")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "random_forest_rfsrc_minimum_depth",
  vimp_method_parameter_list = list(
    "n_tree" = 8,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  outcome_type = "continuous",
  cluster_method = "none",
  imputation_method = "simple")


testthat::test_that(
  paste0(
    "The RFSRC random forest minimum depth method correctly ranks continuous data."),
  {
    vimp_table <- suppressWarnings(
      familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
    
    # Expect that the vimp table has at most six rows.
    testthat::expect_lte(nrow(vimp_table), 6L)
    
    # Expect that the names are the same as that of the features.
    testthat::expect_true(
      all(vimp_table$name %in% familiar:::get_feature_columns(data)))
    
    # Feature 1 is most important.
    testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
  }
)

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "random_forest_rfsrc_permutation",
  vimp_method_parameter_list = list(
    "n_tree" = 8,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  outcome_type = "continuous",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(
  paste0(
    "The RFSRC random forest permutation method correctly ranks continuous data."),
  {
    vimp_table <- suppressWarnings(
      familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
    
    # Expect that the vimp table has at most six rows.
    testthat::expect_lte(nrow(vimp_table), 6L)
    
    # Expect that the names are the same as that of the features.
    testthat::expect_true(
      all(vimp_table$name %in% familiar:::get_feature_columns(data)))
    
    # Feature 1 is most important.
    testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
  }
)

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "random_forest_rfsrc_holdout",
  vimp_method_parameter_list = list(
    "n_tree" = 8,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  outcome_type = "continuous",
  cluster_method = "none",
  imputation_method = "simple")


testthat::test_that(
  paste0(
    "The RFSRC random forest hold-out method correctly ranks continuous data."),
  {
    vimp_table <- suppressWarnings(
      familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
    
    # Expect that the vimp table has at most six rows.
    testthat::expect_lte(nrow(vimp_table), 6L)
    
    # Expect that the names are the same as that of the features.
    testthat::expect_true(
      all(vimp_table$name %in% familiar:::get_feature_columns(data)))
    
    # Feature 1 is most important.
    testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
  }
)



# Binomial outcome -------------------------------------------------------------
data <- familiar:::test_create_good_data("binomial")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "random_forest_rfsrc_minimum_depth",
  vimp_method_parameter_list = list(
    "n_tree" = 8,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  outcome_type = "binomial",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(
  paste0(
    "The RFSRC random forest minimum depth method correctly ranks binomial data."),
  {
    vimp_table <- suppressWarnings(
      familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
    
    # Expect that the vimp table has at most six rows.
    testthat::expect_lte(nrow(vimp_table), 6L)
    
    # Expect that the names are the same as that of the features.
    testthat::expect_true(
      all(vimp_table$name %in% familiar:::get_feature_columns(data)))
    
    # Feature 1 is most important.
    testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
  }
)

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "random_forest_rfsrc_permutation",
  vimp_method_parameter_list = list(
    "n_tree" = 8,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  outcome_type = "binomial",
  cluster_method = "none",
  imputation_method = "simple"
)

testthat::test_that(
  paste0(
    "The RFSRC random forest permutation method correctly ranks binomial data."), 
  {
    vimp_table <- suppressWarnings(
      familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
    
    # Expect that the vimp table has at most six rows.
    testthat::expect_lte(nrow(vimp_table), 6L)
    
    # Expect that the names are the same as that of the features.
    testthat::expect_true(
      all(vimp_table$name %in% familiar:::get_feature_columns(data)))
    
    # Feature 1 is most important.
    testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
  }
)

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "random_forest_rfsrc_holdout",
  vimp_method_parameter_list = list(
    "n_tree" = 8,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  outcome_type = "binomial",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(
  paste0(
    "The RFSRC random forest hold-out method correctly ranks binomial data."),
  {
    vimp_table <- suppressWarnings(
      familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
    
    # Expect that the vimp table has at most six rows.
    testthat::expect_lte(nrow(vimp_table), 6L)
    
    # Expect that the names are the same as that of the features.
    testthat::expect_true(
      all(vimp_table$name %in% familiar:::get_feature_columns(data)))
    
    # Feature 1 is most important.
    testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
  }
)


# Multinomial outcome ----------------------------------------------------------
data <- familiar:::test_create_good_data("multinomial")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "random_forest_rfsrc_minimum_depth",
  vimp_method_parameter_list = list(
    "n_tree" = 8,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  outcome_type = "multinomial",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(
  paste0(
    "The RFSRC random forest minimum depth method correctly ranks multinomial outcome data."),
  {
    vimp_table <- suppressWarnings(
      familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
    
    # Expect that the vimp table has at most six rows.
    testthat::expect_lte(nrow(vimp_table), 6L)
    
    # Expect that the names are the same as that of the features.
    testthat::expect_true(
      all(vimp_table$name %in% familiar:::get_feature_columns(data)))
    
    # Feature 1 is most important.
    testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
  }
)

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "random_forest_rfsrc_permutation",
  vimp_method_parameter_list = list(
    "n_tree" = 8,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  outcome_type = "multinomial",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(
  paste0(
    "The RFSRC random forest permutation method correctly ranks multinomial outcome data."),
  {
    vimp_table <- suppressWarnings(
      familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
    
    # Expect that the vimp table has at most six rows.
    testthat::expect_lte(nrow(vimp_table), 6L)
    
    # Expect that the names are the same as that of the features.
    testthat::expect_true(
      all(vimp_table$name %in% familiar:::get_feature_columns(data)))
    
    # Feature 1 is most important.
    testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
  }
)

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "random_forest_rfsrc_holdout",
  vimp_method_parameter_list = list(
    "n_tree" = 8,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  outcome_type = "multinomial",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(
  paste0(
    "The RFSRC random forest hold-out method correctly ranks multinomial outcome data."),
  {
    vimp_table <- suppressWarnings(
      familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
    
    # Expect that the vimp table has at most six rows.
    testthat::expect_lte(nrow(vimp_table), 6L)
    
    # Expect that the names are the same as that of the features.
    testthat::expect_true(
      all(vimp_table$name %in% familiar:::get_feature_columns(data)))
    
    # Feature 1 is most important.
    testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
  }
)


# Survival outcome -------------------------------------------------------------
data <- familiar:::test_create_good_data("survival")

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "random_forest_rfsrc_minimum_depth",
  vimp_method_parameter_list = list(
    "n_tree" = 8,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  outcome_type = "survival",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(
  paste0(
    "The RFSRC random forest minimum depth method correctly ranks survival outcome data."), 
  {
    vimp_table <- suppressWarnings(
      familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
    
    # Expect that the vimp table has at most six rows.
    testthat::expect_lte(nrow(vimp_table), 6L)
    
    # Expect that the names are the same as that of the features.
    testthat::expect_true(
      all(vimp_table$name %in% familiar:::get_feature_columns(data)))
    
    # Feature 1 is most important.
    testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
  }
)

# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "random_forest_rfsrc_permutation",
  vimp_method_parameter_list = list(
    "n_tree" = 8,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  outcome_type = "survival",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(
  paste0(
    "The RFSRC random forest permutation method correctly ranks survival outcome data."), {
      vimp_table <- suppressWarnings(
        familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
      
      # Expect that the vimp table has at most six rows.
      testthat::expect_lte(nrow(vimp_table), 6L)
      
      # Expect that the names are the same as that of the features.
      testthat::expect_true(
        all(vimp_table$name %in% familiar:::get_feature_columns(data)))
      
      # Feature 1 is most important.
      testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
    }
)


# Process dataset.
vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "random_forest_rfsrc_holdout",
  vimp_method_parameter_list = list(
    "n_tree" = 8,
    "sample_size" = 0.50,
    "m_try" = 0.3,
    "node_size" = 5,
    "tree_depth" = 5),
  outcome_type = "survival",
  cluster_method = "none",
  imputation_method = "simple")

testthat::test_that(
  paste0(
    "The RFSRC random forest hold-out method correctly ranks survival outcome data."),
  {
    vimp_table <- suppressWarnings(
      familiar:::get_vimp_table(familiar:::.vimp(vimp_object, data)))
    
    # Expect that the vimp table has at most six rows.
    testthat::expect_lte(nrow(vimp_table), 6L)
    
    # Expect that the names are the same as that of the features.
    testthat::expect_true(
      all(vimp_table$name %in% familiar:::get_feature_columns(data)))
    
    # Feature 1 is most important.
    testthat::expect_equal(vimp_table[rank == 1, ]$name, "feature_1")
  }
)


testthat::skip("Skip hyperparameter optimisation, unless manual.")

familiar:::test_hyperparameter_optimisation(
  vimp_methods = familiar:::.get_available_rfsrc_vimp_methods(show_general = TRUE),
  debug = FALSE,
  parallel = FALSE)

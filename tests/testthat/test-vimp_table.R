testthat::skip_on_cran()
if (!rlang::is_installed("nnet")) testthat::skip()

# Basic tests ------------------------------------------------------------------

data <- familiar:::test_create_synthetic_correlated_data(
  outcome_type = "continuous",
  n_numeric = 2,
  cluster_size = c(1, 1, 2, 3))

vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "pearson",
  outcome_type = "continuous",
  transformation_method = "none",
  normalisation_method = "none",
  cluster_method = "hclust",
  cluster_cut_method = "fixed_cut",
  cluster_similarity_metric = "mcfadden_r2",
  cluster_similarity_threshold = 0.99,
  imputation_method = "simple")

vimp_table_object <- suppressWarnings(familiar:::.vimp(vimp_object, data))

testthat::test_that("Test that the variable importance table in its initial state is correct.", {
  vimp_table <- familiar:::get_vimp_table(vimp_table_object, "initial")

  # Assert that colnames are correct.
  testthat::expect_setequal(colnames(vimp_table), c("score", "name"))

  # Assert that there are two features that end with cluster.
  testthat::expect_equal(sum(sapply(vimp_table$name, endsWith, "cluster")), 2L)

  # Assert that there are three entries for feature_1.
  testthat::expect_equal(sum(sapply(vimp_table$name, startsWith, "feature_1")), 3L)

  # Assert that there are four entries for feature_2.
  testthat::expect_equal(sum(sapply(vimp_table$name, startsWith, "feature_2")), 4L)
})


testthat::test_that("Test that the variable importance table in its encoded state is correct.", {
  vimp_table <- familiar:::get_vimp_table(vimp_table_object, "decoded")

  # Assert that colnames are correct.
  testthat::expect_setequal(colnames(vimp_table), c("score", "name"))

  # Assert that there are two features that end with cluster.
  testthat::expect_equal(sum(sapply(vimp_table$name, endsWith, "cluster")), 2L)

  # Assert that there is one entry for feature_1.
  testthat::expect_equal(sum(vimp_table$name == "feature_1"), 1L)

  # Assert that there is one entry for feature_2.
  testthat::expect_equal(sum(vimp_table$name == "feature_2"), 1L)
})


testthat::test_that("Test that the variable importance table in its declustered state is correct.", {
  vimp_table <- familiar:::get_vimp_table(vimp_table_object, "declustered")

  # Assert that colnames are correct.
  testthat::expect_setequal(colnames(vimp_table), c("score", "name"))

  # Assert that there is one entry for feature_1.
  testthat::expect_equal(sum(vimp_table$name == "feature_1"), 1L)

  # Assert that there is one entry for feature_2.
  testthat::expect_equal(sum(vimp_table$name == "feature_2"), 1L)

  # Assert that there are two entries for feature_3.
  testthat::expect_equal(sum(sapply(vimp_table$name, startsWith, "feature_3")), 2L)

  # Assert that there are three entries for feature_4.
  testthat::expect_equal(sum(sapply(vimp_table$name, startsWith, "feature_4")), 3L)
})


testthat::test_that("Test that the variable importance table in its re-clustered state is correct.", {
  # Compute reclustered vimp table.
  reclustered_vimp_table_object <- familiar:::recluster_vimp_table(vimp_table_object)
  reclustered_vimp_table <- familiar:::get_vimp_table(reclustered_vimp_table_object, "reclustered")

  # Compute decoded variable importance table. The reclustered table and the
  # decoded table should have the same contents.
  vimp_table <- familiar:::get_vimp_table(vimp_table_object, "decoded")

  # Assert that colnames are correct.
  testthat::expect_setequal(colnames(vimp_table), c("score", "name"))

  # Assert that there are two features that end with cluster.
  testthat::expect_equal(sum(sapply(vimp_table$name, endsWith, "cluster")), 2L)

  # Assert that there is one entry for feature_1.
  testthat::expect_equal(sum(vimp_table$name == "feature_1"), 1L)

  # Assert that there is one entry for feature_2.
  testthat::expect_equal(sum(vimp_table$name == "feature_2"), 1L)

  # Assert that scores match.
  testthat::expect_equal(
    reclustered_vimp_table[order(name)]$score,
    vimp_table[order(name)]$score)
})


testthat::test_that("Test that the variable importance table in its ranked state is correct.", {
  vimp_table <- familiar:::get_vimp_table(vimp_table_object, "ranked")

  # Assert that colnames are correct.
  testthat::expect_setequal(colnames(vimp_table), c("score", "name", "rank"))

  # Assert that there is one entry for feature_1.
  testthat::expect_equal(sum(vimp_table$name == "feature_1"), 1L)

  # Assert that there is one entry for feature_2.
  testthat::expect_equal(sum(vimp_table$name == "feature_2"), 1L)

  # Assert that there are two entries for feature_3.
  testthat::expect_equal(sum(sapply(vimp_table$name, startsWith, "feature_3")), 2L)

  # Assert that there are three entries for feature_4.
  testthat::expect_equal(sum(sapply(vimp_table$name, startsWith, "feature_4")), 3L)

  # Assert that there 4 ranks.
  testthat::expect_equal(sort(unique(vimp_table$rank)), seq_len(4L))

  # Assert that feature_3 has the same rank for each entry.
  testthat::expect_equal(data.table::uniqueN(vimp_table[startsWith(name, "feature_3")]$rank), 1L)

  # Assert that feature_4 has the same rank for each entry.
  testthat::expect_equal(data.table::uniqueN(vimp_table[startsWith(name, "feature_4")]$rank), 1L)
})

testthat::test_that("Test that the re-clustered variable importance table in its ranked state is correct.", {
  reclustered_vimp_table_object <- familiar:::recluster_vimp_table(vimp_table_object)
  vimp_table <- familiar:::get_vimp_table(reclustered_vimp_table_object, "ranked")

  # Assert that colnames are correct.
  testthat::expect_setequal(colnames(vimp_table), c("score", "name", "rank"))

  # Assert that there are two features that end with cluster.
  testthat::expect_equal(sum(sapply(vimp_table$name, endsWith, "cluster")), 2L)

  # Assert that there is one entry for feature_1.
  testthat::expect_equal(sum(vimp_table$name == "feature_1"), 1L)

  # Assert that there is one entry for feature_2.
  testthat::expect_equal(sum(vimp_table$name == "feature_2"), 1L)
})

# Test updating with different cluster table -----------------------------------

data_2 <- familiar:::test_create_synthetic_correlated_data(
  outcome_type = "continuous",
  n_numeric = 2,
  cluster_size = c(1, 1, 4, 2))

vimp_object_2 <- familiar:::prepare_vimp_object(
  data = data_2,
  vimp_method = "pearson",
  outcome_type = "continuous",
  transformation_method = "none",
  normalisation_method = "none",
  cluster_method = "hclust",
  cluster_cut_method = "fixed_cut",
  cluster_similarity_metric = "mcfadden_r2",
  cluster_similarity_threshold = 0.99,
  imputation_method = "simple")

reference_cluster_table <- familiar:::.create_clustering_table(
  vimp_object_2@feature_info)

# Update the variable importance object using the reference cluster table.
updated_vimp_table_object <- familiar:::update_vimp_table_to_reference(
  vimp_table_object,
  reference_cluster_table)

testthat::test_that("Updating variable importance tables functions correctly.", {
  vimp_table <- familiar:::get_vimp_table(updated_vimp_table_object, "ranked")

  # Assert that colnames are correct.
  testthat::expect_setequal(colnames(vimp_table), c("score", "name", "rank"))

  # Assert that there is one entry for feature_1.
  testthat::expect_equal(sum(vimp_table$name == "feature_1"), 1L)

  # Assert that there is one entry for feature_2.
  testthat::expect_equal(sum(vimp_table$name == "feature_2"), 1L)

  # Assert that there are two entries for feature_3.
  testthat::expect_equal(sum(sapply(vimp_table$name, startsWith, "feature_3")), 4L)

  # Assert that there are three entries for feature_4.
  testthat::expect_equal(sum(sapply(vimp_table$name, startsWith, "feature_4")), 2L)

  # Assert that there 4 ranks.
  testthat::expect_equal(sort(unique(vimp_table$rank)), seq_len(4L))

  # Assert that feature_3 has the same rank for each entry.
  testthat::expect_equal(data.table::uniqueN(vimp_table[startsWith(name, "feature_3")]$rank), 1L)

  # Assert that feature_4 has the same rank for each entry.
  testthat::expect_equal(data.table::uniqueN(vimp_table[startsWith(name, "feature_4")]$rank), 1L)
})

testthat::test_that("Updating variable importance tables with reclustering functions correctly.", {
  reclustered_vimp_table_object <- familiar:::recluster_vimp_table(
    updated_vimp_table_object)
  vimp_table <- familiar:::get_vimp_table(reclustered_vimp_table_object, "ranked")

  # Assert that colnames are correct.
  testthat::expect_setequal(colnames(vimp_table), c("score", "name", "rank"))

  # Assert that there are two features that end with cluster.
  testthat::expect_equal(sum(sapply(vimp_table$name, endsWith, "cluster")), 2L)

  # Assert that there is one entry for feature_1.
  testthat::expect_equal(sum(vimp_table$name == "feature_1"), 1L)

  # Assert that there is one entry for feature_2.
  testthat::expect_equal(sum(vimp_table$name == "feature_2"), 1L)
})

# Test updating table where all features are different -------------------------

data_3 <- familiar:::test_create_synthetic_correlated_data(
  outcome_type = "continuous",
  n_numeric = 2,
  cluster_size = c(1, 1, 1, 1))

# Rename features.
data.table::setnames(
  x = data_3@data,
  old = c("feature_1", "feature_2", "feature_3", "feature_4"),
  new = c("feature_A", "feature_B", "feature_C", "feature_D"))

vimp_object_3 <- familiar:::prepare_vimp_object(
  data = data_3,
  vimp_method = "pearson",
  outcome_type = "continuous",
  transformation_method = "none",
  normalisation_method = "none",
  cluster_method = "hclust",
  cluster_cut_method = "fixed_cut",
  cluster_similarity_metric = "mcfadden_r2",
  cluster_similarity_threshold = 0.99,
  imputation_method = "simple")

reference_cluster_table <- familiar:::.create_clustering_table(
  vimp_object_3@feature_info)

# Update the variable importance object using the reference cluster table.
updated_vimp_table_object <- familiar:::update_vimp_table_to_reference(
  vimp_table_object,
  reference_cluster_table)

testthat::test_that("Updating variable importance tables with mismatching reference cluster tables.", {
  vimp_table <- familiar:::get_vimp_table(updated_vimp_table_object, "ranked")

  testthat::expect_equal(familiar:::is_empty(updated_vimp_table_object), TRUE)

  testthat::expect_equal(familiar:::is_empty(vimp_table), TRUE)
})


testthat::test_that("Updating variable importance tables with reclustering functions correctly.", {
  reclustered_vimp_table_object <- familiar:::recluster_vimp_table(updated_vimp_table_object)
  vimp_table <- familiar:::get_vimp_table(reclustered_vimp_table_object, "ranked")

  testthat::expect_equal(familiar:::is_empty(updated_vimp_table_object), TRUE)

  testthat::expect_equal(familiar:::is_empty(vimp_table), TRUE)
})

# Test updating table where the variable importance table could not be created. -----

empty_vimp_table_object <- vimp_table_object
empty_vimp_table_object@vimp_table <- NULL

# Update the variable importance object using the reference cluster table.
updated_vimp_table_object <- familiar:::update_vimp_table_to_reference(
  empty_vimp_table_object,
  reference_cluster_table)

testthat::test_that("Updating variable importance tables with mismatching reference cluster tables.", {
  vimp_table <- familiar:::get_vimp_table(updated_vimp_table_object, "ranked")

  testthat::expect_equal(familiar:::is_empty(updated_vimp_table_object), TRUE)

  testthat::expect_equal(familiar:::is_empty(vimp_table), TRUE)
})

testthat::test_that("Updating variable importance tables with reclustering functions correctly.", {
  reclustered_vimp_table_object <- familiar:::recluster_vimp_table(updated_vimp_table_object)
  vimp_table <- familiar:::get_vimp_table(reclustered_vimp_table_object, "ranked")

  testthat::expect_equal(familiar:::is_empty(updated_vimp_table_object), TRUE)

  testthat::expect_equal(familiar:::is_empty(vimp_table), TRUE)
})

# Test with signature features -------------------------------------------------

data <- familiar:::test_create_synthetic_correlated_data(
  outcome_type = "continuous",
  n_numeric = 2,
  cluster_size = c(1, 1, 1, 1))

vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "pearson",
  outcome_type = "continuous",
  transformation_method = "none",
  normalisation_method = "none",
  cluster_method = "hclust",
  cluster_cut_method = "fixed_cut",
  cluster_similarity_metric = "mcfadden_r2",
  cluster_similarity_threshold = 0.99,
  signature = c("feature_1"),
  imputation_method = "simple")

vimp_table_object <- suppressWarnings(familiar:::.vimp(vimp_object, data))

testthat::test_that("A signature feature does not appear in the variable importance table.", {
  vimp_table <- familiar:::get_vimp_table(vimp_table_object, "ranked")

  # Assert that colnames are correct.
  testthat::expect_setequal(colnames(vimp_table), c("score", "name", "rank"))

  # Assert that there is no entry for feature_1.
  testthat::expect_equal(sum(vimp_table$name == "feature_1"), 0L)

  # Assert that there is one entry for feature_2.
  testthat::expect_equal(sum(vimp_table$name == "feature_2"), 1L)

  # Assert that there is one entry for feature_3.
  testthat::expect_equal(sum(vimp_table$name == "feature_3"), 1L)

  # Assert that there is one entry for feature_4.
  testthat::expect_equal(sum(vimp_table$name == "feature_4"), 1L)

  # Assert that there 3 ranks.
  testthat::expect_equal(sort(unique(vimp_table$rank)), seq_len(3L))
})

# Test with all signature features ---------------------------------------------

vimp_object <- familiar:::prepare_vimp_object(
  data = data,
  vimp_method = "pearson",
  outcome_type = "continuous",
  transformation_method = "none",
  normalisation_method = "none",
  cluster_method = "hclust",
  cluster_cut_method = "fixed_cut",
  cluster_similarity_metric = "mcfadden_r2",
  cluster_similarity_threshold = 0.99,
  signature = c("feature_1", "feature_2", "feature_3", "feature_4"),
  imputation_method = "simple")

vimp_table_object <- suppressWarnings(familiar:::.vimp(vimp_object, data))

testthat::test_that("A signature feature does not appear in the variable importance table.", {
  vimp_table <- familiar:::get_vimp_table(vimp_table_object, "ranked")

  # Assert that both the variable importance table object and the variable
  # importance table are empty.
  testthat::expect_equal(familiar:::is_empty(vimp_table_object), TRUE)
  testthat::expect_equal(familiar:::is_empty(vimp_table), TRUE)
})

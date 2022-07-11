data <- familiar:::test_create_synthetic_correlated_data(outcome_type="continuous",
                                                         n_numeric=2,
                                                         cluster_size=c(1, 1, 2, 3))

vimp_object <- familiar:::prepare_vimp_object(data=data,
                                              vimp_method="pearson",
                                              outcome_type="continuous",
                                              transformation_method="none",
                                              normalisation_method="none",
                                              cluster_method="hclust",
                                              cluster_cut_method="fixed_cut",
                                              cluster_similarity_metric="mcfadden_r2",
                                              cluster_similarity_threshold=0.99,
                                              imputation_method="simple")

vimp_table_object <- suppressWarnings(familiar:::.vimp(vimp_object, data))


testthat::test_that("Test that the variable importance table in its initial state is correct.",{
  
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
  testthat::expect_equal(reclustered_vimp_table[order(name)]$score,
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


# Test whether signature features are removed from the variable importance for
# multivariate methods.

# Assert that no features remain in the variable importance table when all
# features are signature features.

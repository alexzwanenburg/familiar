# Create test dataset.
data <- familiar:::test.create_good_data_set(outcome_type="binomial")

weights <- familiar:::create_instance_weights(data=data,
                                              method="inverse_number_of_samples")

# Check that weights sum to the number of classes (2)
testthat::test_that("Weights sum to the number of classes.",{
  testthat::expect_equal(sum(weights), 2)
})

weights <- familiar:::create_instance_weights(data=data,
                                              method="inverse_number_of_samples",
                                              normalisation="average_one")

# Check that the mean weight is 1.0.
testthat::test_that("Mean weight is 1.0.", {
  testthat::expect_equal(mean(weights), 1.0)
})

weights <- familiar:::create_instance_weights(data=data,
                                              method="inverse_number_of_samples",
                                              normalisation="sum_one")

# Check that weights sum to the number of classes (2)
testthat::test_that("Weights sum to 1.0.",{
  testthat::expect_equal(sum(weights), 1.0)
})

# Check that with beta equal to 0.9, weights between classes are similar.
weights <- familiar:::create_instance_weights(data=data,
                                              method="effective_number_of_samples",
                                              beta=0.9)

testthat::test_that("Weights between classes are almost the same.", {
  testthat::expect_equal(abs(diff(unique(weights))) < 10^-4, TRUE)
})

# Check that with beta equal to 0.99999, weights between classes are similar to
# inverse number of samples.
inv_weights <- familiar:::create_instance_weights(data=data,
                                                  method="inverse_number_of_samples")

weights <- familiar:::create_instance_weights(data=data,
                                              method="effective_number_of_samples",
                                              beta=0.99999)

testthat::test_that("Weights are similar to inverse number of samples.",{
  testthat::expect_equal(all(abs(inv_weights - weights) < 10^-4), TRUE)
})

weights <- familiar:::create_instance_weights(data=data,
                                              method="effective_number_of_samples",
                                              beta=0.9,
                                              normalisation="average_one")

# Check that the mean weight is 1.0.
testthat::test_that("Mean weight is 1.0.", {
  testthat::expect_equal(mean(weights), 1.0)
})

weights <- familiar:::create_instance_weights(data=data,
                                              method="effective_number_of_samples",
                                              beta=0.9,
                                              normalisation="sum_one")

# Check that weights sum to the number of classes (2)
testthat::test_that("Weights sum to 1.0.",{
  testthat::expect_equal(sum(weights), 1.0)
})


weights <- familiar:::create_instance_weights(data=data,
                                              method="effective_number_of_samples",
                                              beta=0.9)

comp_weights <- familiar:::create_instance_weights(data=data,
                                                   method="effective_number_of_samples",
                                                   beta=familiar:::..compute_effective_number_of_samples_beta(-1))

testthat::test_that("Weights are the same.", {
  testthat::expect_equal(all(weights - comp_weights == 0.0), TRUE)
})


testthat::skip("Skip hyperparameter optimisation, unless manual.")

# Check that variable importance functions correctly.
model <- familiar::train_familiar(data=data,
                                  fs_method="mrmr",
                                  learner="glm_logistic",
                                  outcome_type="binomial",
                                  outcome_column="cell_malignancy",
                                  sample_id_column="id",
                                  class_levels=c("benign", "malignant"),
                                  verbose=FALSE)

# Check that sample weighting is inverse_number_of_samples (default).
testthat::test_that("Default method is inverse_number_of_samples.", {
  testthat::expect_equal(as.character(model@hyperparameters$sample_weighting), "inverse_number_of_samples")
})

# Check that effective_number_of_samples functions.
model <- familiar::train_familiar(data=data,
                                  fs_method="mrmr",
                                  learner="glm_logistic",
                                  hyperparameter=list("sample_weighting"="effective_number_of_samples"),
                                  outcome_type="binomial",
                                  outcome_column="cell_malignancy",
                                  sample_id_column="id",
                                  class_levels=c("benign", "malignant"),
                                  verbose=FALSE)

##### binomial -----------------------------------------------------------------
# Create data.table.
data <- familiar:::test.create_good_data_set(outcome_type="binomial",
                                             to_data_object=FALSE)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(data=data,
                                  fs_method="mrmr",
                                  learner="glm_logistic",
                                  outcome_type="binomial",
                                  outcome_column="cell_malignancy",
                                  sample_id_column="id",
                                  class_levels=c("benign", "malignant"),
                                  verbose=FALSE)

testthat::test_that("Logistic model can be trained using train_familiar", {
  
  testthat::expect_s4_class(model, "familiarGLM")
  testthat::expect_equal(familiar:::model_is_trained(model), TRUE)
  testthat::expect_s3_class(summary(model), "summary.glm")
})



##### multinomial --------------------------------------------------------------
# Create data.table.
data <- familiar:::test.create_good_data_set(outcome_type="multinomial",
                                             to_data_object=FALSE)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(data=data,
                                  fs_method="none",
                                  learner="glm_multinomial",
                                  outcome_type="multinomial",
                                  outcome_column="Species",
                                  sample_id_column="sample_id",
                                  class_levels=c("setosa", "versicolor", "virginica"),
                                  verbose=FALSE)

testthat::test_that("Logistic model can be trained using train_familiar", {
  
  testthat::expect_s4_class(model, "familiarGLM")
  testthat::expect_equal(familiar:::model_is_trained(model), TRUE)
  testthat::expect_s4_class(summary(model), "summary.vglm")
})



##### count --------------------------------------------------------------------

# Create data.table.
data <- familiar:::test.create_good_data_set(outcome_type="count",
                                             to_data_object=FALSE)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(data=data,
                                  fs_method="mrmr",
                                  learner="glm_poisson",
                                  outcome_type="count",
                                  outcome_column="median_house_value",
                                  sample_id_column="sample_id",
                                  verbose=FALSE)

testthat::test_that("Poisson model can be trained using train_familiar", {
  
  testthat::expect_s4_class(model, "familiarGLM")
  testthat::expect_equal(familiar:::model_is_trained(model), TRUE)
  testthat::expect_s3_class(summary(model), "summary.glm")
})



##### continuous ---------------------------------------------------------------

# Create data.table.
data <- familiar:::test.create_good_data_set(outcome_type="continuous",
                                             to_data_object=FALSE)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(data=data,
                                  fs_method="mrmr",
                                  learner="glm_gaussian",
                                  outcome_type="continuous",
                                  outcome_column="testscr",
                                  sample_id_column="sample_id",
                                  verbose=FALSE)

testthat::test_that("Gaussian model can be trained using train_familiar", {
  
  testthat::expect_s4_class(model, "familiarGLM")
  testthat::expect_equal(familiar:::model_is_trained(model), TRUE)
  testthat::expect_s3_class(summary(model), "summary.glm")
})


##### survival -----------------------------------------------------------------

# Create data.table.
data <- familiar:::test.create_good_data_set(outcome_type="survival",
                                             to_data_object=FALSE)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(data=data,
                                  fs_method="none",
                                  learner="cox",
                                  outcome_type="survival",
                                  outcome_column=c("time", "status"),
                                  sample_id_column="id",
                                  verbose=FALSE)

testthat::test_that("Cox proportional hazards model can be trained using train_familiar", {
  
  testthat::expect_s4_class(model, "familiarCoxPH")
  testthat::expect_equal(familiar:::model_is_trained(model), TRUE)
  testthat::expect_s3_class(summary(model), "summary.coxph")
})

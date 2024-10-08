testthat::skip_on_cran()
testthat::skip_on_ci()

# binomial ---------------------------------------------------------------------
# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "binomial",
  to_data_object = FALSE)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(
  data = data,
  fs_method = "mrmr",
  learner = "glm_logistic",
  outcome_type = "binomial",
  outcome_column = "cell_malignancy",
  sample_id_column = "id",
  class_levels = c("benign", "malignant"),
  verbose = FALSE)

testthat::test_that("Logistic model can be trained using train_familiar", {
  testthat::expect_s4_class(model, "familiarGLM")
  testthat::expect_equal(familiar:::model_is_trained(model), TRUE)
  testthat::expect_s3_class(summary(model), "summary.glm")
  testthat::expect_equal(is.null(familiar::coef(model)), FALSE)
  
  # Assert that between 1 and 9 features are present, aside from the intercept.
  testthat::expect_gte(length(model@model_features), 1L)
  testthat::expect_lte(length(model@model_features), 9L)
  testthat::expect_equal(length(model@model_features) <= model@hyperparameters$sign_size, TRUE)
})

# multinomial ------------------------------------------------------------------
# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "multinomial",
  to_data_object = FALSE)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(
  data = data,
  fs_method = "none",
  learner = "glm_multinomial",
  outcome_type = "multinomial",
  outcome_column = "Species",
  sample_id_column = "sample_id",
  class_levels = c("setosa", "versicolor", "virginica"),
  verbose = FALSE)

testthat::test_that("Logistic model can be trained using train_familiar", {
  testthat::expect_s4_class(model, "familiarGLM")
  testthat::expect_equal(familiar:::model_is_trained(model), TRUE)
  testthat::expect_s3_class(summary(model), "summary.multinom")
  testthat::expect_equal(is.null(familiar::coef(model)), FALSE)
  testthat::expect_equal(is.null(familiar::vcov(model)), FALSE)
})

# count ------------------------------------------------------------------------

# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "count",
  to_data_object = FALSE)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(
  data = data,
  fs_method = "mrmr",
  learner = "glm_poisson",
  outcome_type = "count",
  outcome_column = "median_house_value",
  sample_id_column = "sample_id",
  verbose = FALSE)

testthat::test_that("Poisson model can be trained using train_familiar", {
  testthat::expect_s4_class(model, "familiarGLM")
  testthat::expect_equal(familiar:::model_is_trained(model), TRUE)
  testthat::expect_s3_class(summary(model), "summary.glm")
  testthat::expect_equal(is.null(familiar::coef(model)), FALSE)
})

# continuous -------------------------------------------------------------------

# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "continuous",
  to_data_object = FALSE)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(
  data = data,
  fs_method = "mrmr",
  learner = "glm_gaussian",
  outcome_type = "continuous",
  outcome_column = "testscr",
  sample_id_column = "sample_id",
  verbose = FALSE)

testthat::test_that("Gaussian model can be trained using train_familiar", {
  testthat::expect_s4_class(model, "familiarGLM")
  testthat::expect_equal(familiar:::model_is_trained(model), TRUE)
  testthat::expect_s3_class(summary(model), "summary.glm")
  testthat::expect_equal(is.null(familiar::coef(model)), FALSE)
})

# survival ---------------------------------------------------------------------

# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "survival",
  to_data_object = FALSE)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(
  data = data,
  fs_method = "none",
  learner = "cox",
  outcome_type = "survival",
  outcome_column = c("time", "status"),
  sample_id_column = "id",
  verbose = FALSE)

testthat::test_that("Cox proportional hazards model can be trained using train_familiar", {
  testthat::expect_s4_class(model, "familiarCoxPH")
  testthat::expect_equal(familiar:::model_is_trained(model), TRUE)
  testthat::expect_s3_class(summary(model), "summary.coxph")
  testthat::expect_equal(is.null(familiar::coef(model)), FALSE)
  testthat::expect_equal(is.null(familiar::vcov(model)), FALSE)
})

# Use experiment data ----------------------------------------------------------
# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "binomial",
  to_data_object = FALSE)

# Create data assignment.
experiment_data <- familiar::precompute_data_assignment(
  data = data,
  experimental_design = "bt(fs+mb,5)",
  outcome_type = "binomial",
  outcome_column = "cell_malignancy",
  sample_id_column = "id",
  class_levels = c("benign", "malignant"),
  verbose = FALSE)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(
  data = data,
  experiment_data = experiment_data,
  fs_method = "mrmr",
  learner = "glm_logistic",
  outcome_type = "binomial",
  outcome_column = "cell_malignancy",
  sample_id_column = "id",
  class_levels = c("benign", "malignant"),
  verbose = FALSE)

testthat::test_that("Logistic model can be trained using train_familiar", {
  # Assert that 5 models are trained.
  testthat::expect_equal(length(model), 5L)

  # Assert that the project ids match.
  testthat::expect_equal(model[[1]]@project_id, experiment_data@project_id)
})

# Check "none" variable importance method --------------------------------------

# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "binomial",
  to_data_object = FALSE)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(
  data = data,
  cluster_method = "none",
  fs_method = "none",
  learner = "glm_logistic",
  outcome_type = "binomial",
  outcome_column = "cell_malignancy",
  sample_id_column = "id",
  class_levels = c("benign", "malignant"),
  verbose = FALSE)

testthat::test_that("Assert that all features are used for \"none\" variable importance method.", {
  # Assert that all features are included in the model.
  testthat::expect_equal(length(model@model_features), 9L)
})

# Check "signature_only" variable importance method ----------------------------

# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "binomial",
  to_data_object = FALSE)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(
  data = data,
  signature = c("cell_size_uniformity", "clump_thickness"),
  fs_method = "signature_only",
  learner = "glm_logistic",
  outcome_type = "binomial",
  outcome_column = "cell_malignancy",
  sample_id_column = "id",
  class_levels = c("benign", "malignant"),
  verbose = FALSE)

testthat::test_that("Assert that all features are used for \"signature_only\" variable importance method.", {
  # Assert that only signature features are included in the model.
  testthat::expect_equal(length(model@model_features), 2L)
  testthat::expect_setequal(
    c("cell_size_uniformity", "clump_thickness"),
    model@model_features)
})

# Check interaction between signature and other features -----------------------

# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "binomial",
  to_data_object = FALSE)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(
  data = data,
  signature = c("marginal_adhesion", "bland_chromatin"),
  fs_method = "mrmr",
  learner = "glm_logistic",
  outcome_type = "binomial",
  outcome_column = "cell_malignancy",
  sample_id_column = "id",
  class_levels = c("benign", "malignant"),
  verbose = FALSE)

testthat::test_that("Assert that signature features are used in the model.", {
  # Assert that only signature features are included in the model.
  testthat::expect_gte(length(model@model_features), 2L)
  testthat::expect_lte(length(model@model_features), 9L)
  testthat::expect_equal(all(c("marginal_adhesion", "bland_chromatin") %in% model@model_features), TRUE)
  testthat::expect_equal(length(model@model_features) <= model@hyperparameters$sign_size, TRUE)
})

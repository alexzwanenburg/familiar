testthat::skip_on_cran()

# binomial ---------------------------------------------------------------------
# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "binomial",
  to_data_object = FALSE
)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(
  data = data,
  fs_method = "mrmr",
  learner = "glm_logistic",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = FALSE
)

testthat::test_that("Logistic model can be trained using train_familiar", {
  testthat::expect_s4_class(model, "familiarGLM")
  testthat::expect_true(familiar:::model_is_trained(model))
  testthat::expect_s3_class(summary(model), "summary.glm")
  testthat::expect_false(is.null(familiar::coef(model)))
  
  # Assert that between 1 and 6 features are present, aside from the intercept.
  testthat::expect_gte(length(model@model_features), 1L)
  testthat::expect_lte(length(model@model_features), 6L)
  testthat::expect_lte(length(model@model_features), model@hyperparameters$sign_size)
})

# multinomial ------------------------------------------------------------------
# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "multinomial",
  to_data_object = FALSE
)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(
  data = data,
  fs_method = "none",
  learner = "glm_multinomial",
  outcome_type = "multinomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green", "blue"),
  verbose = FALSE
)

testthat::test_that("Logistic model can be trained using train_familiar", {
  testthat::expect_s4_class(model, "familiarGLM")
  testthat::expect_true(familiar:::model_is_trained(model))
  testthat::expect_s3_class(summary(model), "summary.multinom")
  testthat::expect_false(is.null(familiar::coef(model)))
  testthat::expect_false(is.null(familiar::vcov(model)))
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
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  verbose = FALSE
)

testthat::test_that("Gaussian model can be trained using train_familiar", {
  testthat::expect_s4_class(model, "familiarGLM")
  testthat::expect_true(familiar:::model_is_trained(model))
  testthat::expect_s3_class(summary(model), "summary.glm")
  testthat::expect_false(is.null(familiar::coef(model)))
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
  outcome_column = c("outcome_time", "outcome_event"),
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  verbose = FALSE
)

testthat::test_that("Cox proportional hazards model can be trained using train_familiar", {
  testthat::expect_s4_class(model, "familiarCoxPH")
  testthat::expect_true(familiar:::model_is_trained(model))
  testthat::expect_s3_class(summary(model), "summary.coxph")
  testthat::expect_false(is.null(familiar::coef(model)))
  testthat::expect_false(is.null(familiar::vcov(model)))
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
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = FALSE
)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(
  data = data,
  experiment_data = experiment_data,
  fs_method = "mrmr",
  learner = "glm_logistic",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = FALSE
)

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
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = FALSE
)

testthat::test_that("Assert that all features are used for \"none\" variable importance method.", {
  # Assert that all features are included in the model.
  testthat::expect_equal(length(model@model_features), 6L)
})

# Check "signature_only" variable importance method ----------------------------

# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "binomial",
  to_data_object = FALSE)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(
  data = data,
  signature = c("feature_1", "feature_2a"),
  fs_method = "signature_only",
  learner = "glm_logistic",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = FALSE
)

testthat::test_that("Assert that all features are used for \"signature_only\" variable importance method.", {
  # Assert that only signature features are included in the model.
  testthat::expect_equal(length(model@model_features), 2L)
  testthat::expect_setequal(
    c("feature_1", "feature_2a"),
    model@model_features
  )
})

# Check interaction between signature and other features -----------------------

# Create data.table.
data <- familiar:::test_create_good_data(
  outcome_type = "binomial",
  to_data_object = FALSE
)

# Check that train_familiar functions correctly.
model <- familiar::train_familiar(
  data = data,
  signature = c("feature_1", "feature_2a"),
  fs_method = "mrmr",
  learner = "glm_logistic",
  outcome_type = "binomial",
  outcome_column = "outcome",
  batch_id_column = "batch_id",
  sample_id_column = "sample_id",
  series_id_column = "series_id",
  class_levels = c("red", "green"),
  verbose = FALSE
)

testthat::test_that("Assert that signature features are used in the model.", {
  # Assert that only signature features are included in the model.
  testthat::expect_gte(length(model@model_features), 2L)
  testthat::expect_lte(length(model@model_features), 6L)
  testthat::expect_true(all(c("feature_1", "feature_2a") %in% model@model_features))
  testthat::expect_lte(length(model@model_features), model@hyperparameters$sign_size)
})

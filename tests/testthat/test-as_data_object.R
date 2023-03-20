testthat::skip_on_cran()

# Train a model to transfer settings.
data <- familiar:::test_create_good_data("survival")

# Train a simple linear GLM using the good dataset.
fam_model <- familiar:::test_train(
  data = data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(data)),
  learner = "cox",
  create_novelty_detector = TRUE)

# Feature levels are correctly ordered -----------------------------------------
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Change order of features.
data$rx <- factor(
  x = data$rx,
  levels = c("Lev", "Lev+5FU", "Obs"))

testthat::test_that("Feature levels are correctly ordered", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness)

    testthat::expect_equal(
      levels(parsed_data@data$rx),
      c("Obs", "Lev", "Lev+5FU"))
  }
})


data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Keep features as characters,
data$rx <- as.character(data$rx)

testthat::test_that("Feature levels are correctly set", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness)

    testthat::expect_equal(
      levels(parsed_data@data$rx),
      c("Obs", "Lev", "Lev+5FU"))
  }
})



# Ordered features are correctly set -------------------------------------------
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Change order of features.
data$adhere <- factor(
  x = data$adhere,
  levels = c("TRUE", "FALSE"),
  ordered = TRUE)

testthat::test_that("Feature levels are correctly ordered", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness)

    testthat::expect_equal(
      levels(parsed_data@data$adhere),
      c("FALSE", "TRUE"))
    testthat::expect_equal(
      is.ordered(parsed_data@data$adhere),
      TRUE)
  }
})

data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Keep features as characters,
data$adhere <- as.character(data$adhere)

testthat::test_that("Feature levels are correctly set", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness)

    testthat::expect_equal(
      levels(parsed_data@data$adhere),
      c("FALSE", "TRUE"))
    testthat::expect_equal(
      is.ordered(parsed_data@data$adhere),
      TRUE)
  }
})



# Missing levels in categorical features ---------------------------------------

data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Remove a feature level.
data[rx == "Lev+5FU", "rx" := "Lev"]

testthat::test_that("Feature levels are correctly ordered", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness)

    testthat::expect_equal(
      levels(parsed_data@data$rx),
      c("Obs", "Lev", "Lev+5FU"))
  }
})


# Additional levels in categorical features -----------------------------------
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Add new level to categorical features.
data[c(1, 2, 3), "rx" := "Cisplatin"]

testthat::test_that("Extra levels are detected", {
  for (strictness in c("strict", "external_warn", "external")) {
    testthat::expect_error(familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness))
  }
})


# Test methods to set reference levels -----------------------------------------
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)
data$rx <- stats::relevel(data$rx, ref = "Lev")
original_true <- data$adhere == "TRUE"
data[adhere == "FALSE", "adhere" := "TRUE"]
data[original_true, "adhere" := "FALSE"]

testthat::test_that("Auto-method does not re-order existing levels.", {
  parsed_data <- familiar::as_data_object(
    data = data.table::copy(data),
    sample_id_column = "id",
    outcome_column = c("time", "status"),
    outcome_type = "survival",
    include_features = c("nodes", "rx", "adhere"),
    reference_method = "auto")

  testthat::expect_equal(
    head(levels(parsed_data@data$rx), n = 1L), "Lev")
  testthat::expect_equal(
    head(levels(parsed_data@data$adhere), n = 1L), "FALSE")
})

testthat::test_that("Always-method re-orders existing levels.", {
  parsed_data <- familiar::as_data_object(
    data = data.table::copy(data),
    sample_id_column = "id",
    outcome_column = c("time", "status"),
    outcome_type = "survival",
    include_features = c("nodes", "rx", "adhere"),
    reference_method = "always")

  testthat::expect_equal(
    head(levels(parsed_data@data$rx), n = 1L), "Obs")
  testthat::expect_equal(
    head(levels(parsed_data@data$adhere), n = 1L), "FALSE")
})

testthat::test_that("Never-method does not re-order existing levels.", {
  parsed_data <- familiar::as_data_object(
    data = data.table::copy(data),
    sample_id_column = "id",
    outcome_column = c("time", "status"),
    outcome_type = "survival",
    include_features = c("nodes", "rx", "adhere"),
    reference_method = "never")

  testthat::expect_equal(
    head(levels(parsed_data@data$rx), n = 1L), "Lev")
  testthat::expect_equal(
    head(levels(parsed_data@data$adhere), n = 1L), "FALSE")
})

# Now for automatic conversion of categorical variables.
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)
data$rx <- as.character(data$rx)
data$adhere <- as.logical(data$adhere)

testthat::test_that("Auto-method re-orders existing levels.", {
  parsed_data <- familiar::as_data_object(
    data = data.table::copy(data),
    sample_id_column = "id",
    outcome_column = c("time", "status"),
    outcome_type = "survival",
    include_features = c("nodes", "rx", "adhere"),
    reference_method = "auto")

  testthat::expect_equal(
    head(levels(parsed_data@data$rx), n = 1L), "Obs")
  testthat::expect_equal(
    head(levels(parsed_data@data$adhere), n = 1L), "FALSE")
})

testthat::test_that(
  "Always-method re-orders existing levels.", {
  parsed_data <- familiar::as_data_object(
    data = data.table::copy(data),
    sample_id_column = "id",
    outcome_column = c("time", "status"),
    outcome_type = "survival",
    include_features = c("nodes", "rx", "adhere"),
    reference_method = "always")

  testthat::expect_equal(
    head(levels(parsed_data@data$rx), n = 1L), "Obs")
  testthat::expect_equal(
    head(levels(parsed_data@data$adhere), n = 1L), "FALSE")
})

testthat::test_that("Never-method sorts existing levels.", {
  parsed_data <- familiar::as_data_object(
    data = data.table::copy(data),
    sample_id_column = "id",
    outcome_column = c("time", "status"),
    outcome_type = "survival",
    include_features = c("nodes", "rx", "adhere"),
    reference_method = "never")

  testthat::expect_equal(
    head(levels(parsed_data@data$rx), n = 1L), "Lev")
  testthat::expect_equal(
    head(levels(parsed_data@data$adhere), n = 1L), "FALSE")
})



# Censoring and event identifiers are set --------------------------------------
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

testthat::test_that("Censoring is correctly transferred", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness)

    testthat::expect_equal(
      all(parsed_data@data$outcome_event %in% c(0, 1)),
      TRUE)
  }
})



# Manual censoring and event identifiers ---------------------------------------
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)
manual_data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Update status column to specific
manual_data$status <- as.character(data$status)
manual_data[status == "0", "status" := "alive"]
manual_data[status == "1", "status" := "dead"]

testthat::test_that("Censoring and event identifiers can be manually set", {
  for (strictness in c("strict", "external_warn", "external")) {
    # With specific manual event identifiers.
    manual_parsed_data <- familiar::as_data_object(
      data = data.table::copy(manual_data),
      object = fam_model,
      event_indicator = "dead",
      censoring_indicator = "alive",
      check_stringency = strictness)

    # With original event identifiers.
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness)

    testthat::expect_equal(
      all(manual_parsed_data@data$outcome_event == parsed_data@data$outcome_event),
      TRUE)
  }
})



# Unknown censoring and event identifiers --------------------------------------
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Update status column to specific
data$status <- as.character(data$status)
data[status == "0", "status" := "alive"]
data[status == "1", "status" := "dead"]

testthat::test_that("Censoring and event identifiers are not known", {
  for (strictness in c("strict", "external_warn", "external")) {
    # With specific manual event identifiers. However, now they are not manually
    # provided.

    if (strictness == "strict") {
      testthat::expect_error(familiar::as_data_object(
        data = data.table::copy(data),
        object = fam_model,
        check_stringency = strictness))
      
    } else if (strictness == "external_warn") {
      warns <- testthat::capture_warnings(familiar::as_data_object(
        data = data.table::copy(data),
        object = fam_model,
        check_stringency = strictness))

      testthat::expect_equal(any(grepl("event indicator", warns)), TRUE)
      testthat::expect_equal(any(grepl("censoring indicator", warns)), TRUE)
      
    } else if (strictness == "external") {
      # With original event identifiers.
      parsed_data <- familiar::as_data_object(
        data = data.table::copy(data),
        object = fam_model,
        check_stringency = strictness)
    }
  }
})



# Non-standard censoring and event identifiers are correctly propagated --------
# Train a model to transfer settings.
data <- familiar:::test_create_good_data("survival")

# Train a simple linear GLM using the good dataset.
fam_model <- familiar:::test_train(
  data = data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(data)),
  learner = "cox",
  event_indicator = "dead",
  censoring_indicator = "alive",
  create_novelty_detector = TRUE)

testthat::test_that("Non-standard censoring and event identifiers are automatically transferred", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness)

    testthat::expect_equal(
      all(parsed_data@data$outcome_event %in% c(0, 1)), TRUE)
  }
})


# Class levels are correctly ordered -------------------------------------------
# Train a model to transfer settings.
data <- familiar:::test_create_good_data("binomial")

# Train a simple linear GLM using the good dataset.
fam_model <- familiar:::test_train(
  data = data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(data)),
  learner = "glm_logistic",
  create_novelty_detector = TRUE)

# Create test dataset.
data <- familiar:::test_create_good_data("binomial", to_data_object = FALSE)

testthat::test_that("Class levels are correctly ordered", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness)

    testthat::expect_equal(
      levels(parsed_data@data$outcome),
      c("benign", "malignant"))
  }
})



# Class levels are restored to expected order ----------------------------------
data <- familiar:::test_create_good_data("binomial", to_data_object = FALSE)

# Reorder class levels
data$cell_malignancy <- factor(
  x = data$cell_malignancy,
  levels = c("malignant", "benign"))

testthat::test_that("Class levels are ordered according to expectations", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness)

    testthat::expect_equal(
      levels(parsed_data@data$outcome),
      c("benign", "malignant"))
  }
})


# Missing class levels ---------------------------------------------------------
data <- familiar:::test_create_good_data("binomial", to_data_object = FALSE)
data[cell_malignancy == "benign", cell_malignancy := "malignant"]

testthat::test_that("Class levels are ordered according to expectations", {
  for (strictness in c("strict", "external_warn", "external")) {
    if (strictness == "strict") {
      testthat::expect_error(familiar::as_data_object(
        data = data.table::copy(data),
        object = fam_model,
        check_stringency = strictness))
      
    } else {
      # Class levels may be missing for external_warn and external strictness
      # levels.
      parsed_data <- familiar::as_data_object(
        data = data.table::copy(data),
        object = fam_model,
        check_stringency = strictness)

      testthat::expect_equal(
        levels(parsed_data@data$outcome),
        c("benign", "malignant"))
    }
  }
})



# Additional class levels --------------------------------------------------
data <- familiar:::test_create_good_data("binomial", to_data_object = FALSE)

# Add new level
data[c(1, 2, 3), "cell_malignancy" := "unknown"]

testthat::test_that("Additional class levels are detected", {
  for (strictness in c("strict", "external_warn", "external")) {
    if (strictness %in% c("strict", "external_warn")) {
      testthat::expect_error(familiar::as_data_object(
        data = data.table::copy(data),
        object = fam_model,
        check_stringency = strictness))
      
    } else {
      parsed_data <- familiar::as_data_object(
        data = data.table::copy(data),
        object = fam_model,
        check_stringency = strictness)
    }
  }
})

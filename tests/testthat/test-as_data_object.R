testthat::skip_on_cran()
testthat::skip_on_ci()

# Train a model to transfer settings.
data <- familiar:::test_create_good_data("survival")

# Train a simple linear GLM using the good dataset.
fam_model <- familiar:::test_train(
  data = data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(data)),
  learner = "cox",
  create_novelty_detector = TRUE
)

# Feature levels are correctly ordered -----------------------------------------
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Change order of features.
data$feature_3a <- factor(
  x = data$feature_3a,
  levels = rev(levels(data$feature_3a))
)

testthat::test_that("Feature levels are correctly ordered", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness
    )

    testthat::expect_equal(
      levels(parsed_data@data$feature_3a),
      c("round", "square")
    )
  }
})


data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Keep features as characters,
data$feature_3a <- as.character(data$feature_3a)

testthat::test_that("Feature levels are correctly set", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness
    )

    testthat::expect_equal(
      levels(parsed_data@data$feature_3a),
      c("round", "square")
    )
  }
})



# Ordered features are correctly set -------------------------------------------
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Change order of features.
data$feature_4 <- factor(
  x = data$feature_4,
  levels = c("best", "better", "good"),
  ordered = TRUE
)

testthat::test_that("Feature levels are correctly ordered", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness
    )

    testthat::expect_equal(
      levels(parsed_data@data$feature_4),
      c("good", "better", "best")
    )
    testthat::expect_true(is.ordered(parsed_data@data$feature_4))
  }
})

data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Keep features as characters,
data$feature_4 <- as.character(data$feature_4)

testthat::test_that("Feature levels are correctly set", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness)

    testthat::expect_equal(
      levels(parsed_data@data$feature_4),
      c("good", "better", "best")
    )
    testthat::expect_true(is.ordered(parsed_data@data$feature_4))
  }
})



# Missing levels in categorical features ---------------------------------------

data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Remove a feature level.
data[feature_4 == "better", "feature_4" := "best"]

testthat::test_that("Feature levels are correctly ordered", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness
    )

    testthat::expect_equal(
      levels(parsed_data@data$feature_4),
      c("good", "better", "best")
    )
  }
})


# Additional levels in categorical features -----------------------------------
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Add new level to categorical features.
data[c(1, 2, 3), "feature_3a" := "extra_square"]

testthat::test_that("Extra levels are detected", {
  for (strictness in c("strict", "external_warn", "external")) {
    testthat::expect_error(familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness
    ))
  }
})


# Test methods to set reference levels -----------------------------------------
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)
data$feature_3a <- stats::relevel(data$feature_3a, ref = "square")

testthat::test_that("Auto-method does not re-order existing levels.", {
  parsed_data <- familiar::as_data_object(
    data = data.table::copy(data),
    batch_id_column = "batch_id",
    sample_id_column = "sample_id",
    series_id_column = "series_id",
    outcome_column = c("outcome_time", "outcome_event"),
    outcome_type = "survival",
    include_features = c("feature_3a", "feature_3b"),
    reference_method = "auto"
  )

  testthat::expect_equal(
    head(levels(parsed_data@data$feature_3a), n = 1L), "square"
  )
  testthat::expect_equal(
    head(levels(parsed_data@data$feature_3b), n = 1L), "sphere"
  )
})

testthat::test_that("Always-method re-orders existing levels.", {
  parsed_data <- familiar::as_data_object(
    data = data.table::copy(data),
    batch_id_column = "batch_id",
    sample_id_column = "sample_id",
    series_id_column = "series_id",
    outcome_column = c("outcome_time", "outcome_event"),
    outcome_type = "survival",
    include_features = c("feature_3a", "feature_3b"),
    reference_method = "always"
  )

  testthat::expect_equal(
    head(levels(parsed_data@data$feature_3a), n = 1L), "round"
  )
  testthat::expect_equal(
    head(levels(parsed_data@data$feature_3b), n = 1L), "sphere"
  )
})

testthat::test_that("Never-method does not re-order existing levels.", {
  parsed_data <- familiar::as_data_object(
    data = data.table::copy(data),
    batch_id_column = "batch_id",
    sample_id_column = "sample_id",
    series_id_column = "series_id",
    outcome_column = c("outcome_time", "outcome_event"),
    outcome_type = "survival",
    include_features = c("feature_3a", "feature_3b"),
    reference_method = "never"
  )

  testthat::expect_equal(
    head(levels(parsed_data@data$feature_3a), n = 1L), "square"
  )
  testthat::expect_equal(
    head(levels(parsed_data@data$feature_3b), n = 1L), "sphere"
  )
})

# Now for automatic conversion of categorical variables.
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)
data$feature_3a <- as.character(data$feature_3a)
data[, "feature_3_extra" := feature_3a == "square"]

testthat::test_that("Auto-method re-orders existing levels.", {
  parsed_data <- familiar::as_data_object(
    data = data.table::copy(data),
    batch_id_column = "batch_id",
    sample_id_column = "sample_id",
    series_id_column = "series_id",
    outcome_column = c("outcome_time", "outcome_event"),
    outcome_type = "survival",
    include_features = c("feature_3a", "feature_3_extra"),
    reference_method = "auto"
  )
  
  testthat::expect_equal(
    head(levels(parsed_data@data$feature_3a), n = 1L), "round"
  )
  testthat::expect_equal(
    head(levels(parsed_data@data$feature_3_extra), n = 1L), "FALSE"
  )
})

testthat::test_that(
  "Always-method re-orders existing levels.", {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      batch_id_column = "batch_id",
      sample_id_column = "sample_id",
      series_id_column = "series_id",
      outcome_column = c("outcome_time", "outcome_event"),
      outcome_type = "survival",
      include_features = c("feature_3a", "feature_3_extra"),
      reference_method = "always"
    )
    
    testthat::expect_equal(
      head(levels(parsed_data@data$feature_3a), n = 1L), "round"
    )
    testthat::expect_equal(
      head(levels(parsed_data@data$feature_3_extra), n = 1L), "FALSE"
    )
})

testthat::test_that("Never-method sorts existing levels.", {
  parsed_data <- familiar::as_data_object(
    data = data.table::copy(data),
    batch_id_column = "batch_id",
    sample_id_column = "sample_id",
    series_id_column = "series_id",
    outcome_column = c("outcome_time", "outcome_event"),
    outcome_type = "survival",
    include_features = c("feature_3a", "feature_3_extra"),
    reference_method = "never"
  )
  
  testthat::expect_equal(
    head(levels(parsed_data@data$feature_3a), n = 1L), "round"
  )
  testthat::expect_equal(
    head(levels(parsed_data@data$feature_3_extra), n = 1L), "FALSE"
  )
})



# Censoring and event identifiers are set --------------------------------------
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

testthat::test_that("Censoring is correctly transferred", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness
    )

    testthat::expect_true(all(parsed_data@data$outcome_event %in% c(0, 1)))
  }
})



# Manual censoring and event identifiers ---------------------------------------
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)
manual_data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Update status column to specific
manual_data$outcome_event <- as.character(data$outcome_event)
manual_data[outcome_event == "0", "outcome_event" := "alive"]
manual_data[outcome_event == "1", "outcome_event" := "dead"]

testthat::test_that("Censoring and event identifiers can be manually set", {
  for (strictness in c("strict", "external_warn", "external")) {
    # With specific manual event identifiers.
    manual_parsed_data <- familiar::as_data_object(
      data = data.table::copy(manual_data),
      object = fam_model,
      event_indicator = "dead",
      censoring_indicator = "alive",
      check_stringency = strictness
    )

    # With original event identifiers.
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness
    )

    testthat::expect_true(
      all(manual_parsed_data@data$outcome_event == parsed_data@data$outcome_event)
    )
  }
})



# Unknown censoring and event identifiers --------------------------------------
data <- familiar:::test_create_good_data("survival", to_data_object = FALSE)

# Update status column to specific
data$outcome_event <- as.character(data$outcome_event)
data[outcome_event == "0", "outcome_event" := "alive"]
data[outcome_event == "1", "outcome_event" := "dead"]

testthat::test_that("Censoring and event identifiers are not known", {
  for (strictness in c("strict", "external_warn", "external")) {
    # With specific manual event identifiers. However, now they are not manually
    # provided.

    if (strictness == "strict") {
      testthat::expect_error(familiar::as_data_object(
        data = data.table::copy(data),
        object = fam_model,
        check_stringency = strictness
      ))
      
    } else if (strictness == "external_warn") {
      warns <- testthat::capture_warnings(familiar::as_data_object(
        data = data.table::copy(data),
        object = fam_model,
        check_stringency = strictness
      ))

      testthat::expect_true(any(grepl("event indicator", warns)))
      testthat::expect_true(any(grepl("censoring indicator", warns)))
      
    } else if (strictness == "external") {
      # With original event identifiers.
      parsed_data <- familiar::as_data_object(
        data = data.table::copy(data),
        object = fam_model,
        check_stringency = strictness
      )
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
  create_novelty_detector = TRUE
)

testthat::test_that("Non-standard censoring and event identifiers are automatically transferred", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness
    )

    testthat::expect_true(all(parsed_data@data$outcome_event %in% c(0, 1)))
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
  create_novelty_detector = TRUE
)

# Create test dataset.
data <- familiar:::test_create_good_data("binomial", to_data_object = FALSE)

testthat::test_that("Class levels are correctly ordered", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness
    )

    testthat::expect_equal(
      levels(parsed_data@data$outcome),
      c("red", "green")
    )
  }
})



# Class levels are restored to expected order ----------------------------------
data <- familiar:::test_create_good_data("binomial", to_data_object = FALSE)

# Reorder class levels
data$outcome <- factor(
  x = data$outcome,
  levels = c("green", "red")
)

testthat::test_that("Class levels are ordered according to expectations", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness
    )

    testthat::expect_equal(
      levels(parsed_data@data$outcome),
      c("red", "green")
    )
  }
})


# Missing class levels ---------------------------------------------------------
data <- familiar:::test_create_good_data("binomial", to_data_object = FALSE)
data[outcome == "red", outcome := "green"]

testthat::test_that("Class levels are ordered according to expectations", {
  for (strictness in c("strict", "external_warn", "external")) {
    if (strictness == "strict") {
      testthat::expect_error(familiar::as_data_object(
        data = data.table::copy(data),
        object = fam_model,
        check_stringency = strictness
      ))
      
    } else {
      # Class levels may be missing for external_warn and external strictness
      # levels.
      parsed_data <- familiar::as_data_object(
        data = data.table::copy(data),
        object = fam_model,
        check_stringency = strictness
      )

      testthat::expect_equal(
        levels(parsed_data@data$outcome),
        c("red", "green"))
    }
  }
})



# Additional class levels --------------------------------------------------
data <- familiar:::test_create_good_data("binomial", to_data_object = FALSE)

# Add new level
data[c(1, 2, 3), "outcome" := "blue"]

testthat::test_that("Additional class levels are detected", {
  for (strictness in c("strict", "external_warn", "external")) {
    if (strictness %in% c("strict", "external_warn")) {
      testthat::expect_error(familiar::as_data_object(
        data = data.table::copy(data),
        object = fam_model,
        check_stringency = strictness
      ))
      
    } else {
      parsed_data <- familiar::as_data_object(
        data = data.table::copy(data),
        object = fam_model,
        check_stringency = strictness
      )
    }
  }
})



# Naive model ------------------------------------------------------------------
# Train a model to transfer settings.
data <- familiar:::test_create_good_data("binomial")

# Train a naive model using the good dataset.
fam_naive_model <- familiar:::test_train(
  data = data,
  cluster_method = "none",
  imputation_method = "simple",
  hyperparameter_list = list("sign_size" = familiar:::get_n_features(data)),
  learner = "glm_logistic",
  create_novelty_detector = TRUE,
  create_naive = TRUE
)
  
data <- familiar:::test_create_good_data("binomial", to_data_object = FALSE)

testthat::test_that("Naive models can be used to convert data objects.", {
  for (strictness in c("strict", "external_warn", "external")) {
    parsed_data <- familiar::as_data_object(
      data = data.table::copy(data),
      object = fam_model,
      check_stringency = strictness
    )
    
    testthat::expect_equal(
      levels(parsed_data@data$outcome),
      c("red", "green")
    )
  }
})

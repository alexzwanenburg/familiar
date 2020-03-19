# Create feature info
feature_info_list <- list("feature_a"=methods::new("featureInfo",
                                                   name = "feature_a",
                                                   set_descriptor = NA_character_,
                                                   feature_type = "numeric",
                                                   levels = NULL,
                                                   ordered = FALSE,
                                                   data_id = NA_integer_,
                                                   run_id = NA_integer_,
                                                   in_signature = FALSE,
                                                   removed = FALSE,
                                                   removed_unknown_type = FALSE,
                                                   removed_missing_values = FALSE,
                                                   removed_no_variance = FALSE,
                                                   removed_low_variance = FALSE,
                                                   removed_low_robustness = FALSE,
                                                   removed_low_importance = FALSE,
                                                   fraction_missing = 0.0,
                                                   robustness = NULL,
                                                   univariate_importance = NULL,
                                                   transformation_parameters = list("transform_method"="none",
                                                                                    "transform_lambda"=0),
                                                   normalisation_parameters = list("norm_method"="none",
                                                                                   "norm_shift"=0,
                                                                                   "norm_scale"=1),
                                                   batch_normalisation_parameters = list("test_cohort_1"=list("norm_method"="none",
                                                                                                              "norm_shift"=0,
                                                                                                              "norm_scale"=1,
                                                                                                              "n"=10L)),
                                                   imputation_parameters = list("common_value"=5),
                                                   cluster_parameters = NULL,
                                                   required_features = "feature_a"))


# Create data
data <- data.table::data.table("subject_id"=paste0("sample_", seq_len(10)),
                               "cohort_id"="test_cohort_1",
                               "repetition_id"=rep(1, times=10),
                               "outcome"=seq_len(10),
                               "feature_a"=seq_len(10) - 1)

# Set evaluation settings
eval_settings <- familiar:::.parse_evaluation_settings(data=data,
                                                       parallel=FALSE,
                                                       outcome_type="continuous",
                                                       hpo_metric="mse",
                                                       development_batch_id="test_cohort_1",
                                                       vimp_aggregation_method="mean",
                                                       vimp_aggregation_rank_threshold=5,
                                                       prep_cluster_method="hclust",
                                                       prep_cluster_linkage_method="average",
                                                       prep_cluster_cut_method="fixed_cut",
                                                       prep_cluster_similarity_threshold=0.90,
                                                       prep_cluster_similarity_metric="spearman")

# Create familiar model
fam_model <- methods::new("familiarModel",
                          outcome_type = "continuous",
                          learner = "__test_perfect",
                          run_table = data.table::data.table("run_id"=1L, "data_id"=1L, "can_pre_process"=TRUE, "perturbation"="main", "perturb_level"=1),
                          fs_method = "none",
                          hyperparameters = list("sign_size"=1, "family"="perfect"),
                          signature = "feature_a",
                          req_feature_cols =  "feature_a",
                          important_features = "feature_a",
                          feature_info = feature_info_list,
                          class_levels = character(0),
                          project_id = 0,
                          settings = eval_settings)

# Convert to dataObject
data_obj <- familiar:::create_data_object(object=fam_model, data=data, is_pre_processed=FALSE)


# Train model
fam_model <- familiar:::train(object=fam_model, data=data_obj, get_recalibration=TRUE, get_additional_info=TRUE)

#####familiarModel-based test###################################################
testthat::test_that("Conversion of single familiarModel to familiarEnsemble works", {
  testthat::expect_s4_class(familiar::as_familiar_ensemble(object=fam_model), "familiarEnsemble")
})

testthat::test_that("Conversion of multiple familiarModel objects to familiarEnsemble works", {
  testthat::expect_s4_class(familiar::as_familiar_ensemble(object=list(fam_model, fam_model)), "familiarEnsemble")
})

testthat::test_that("Conversion of single familiarModel to familiarData works", {
  testthat::expect_s4_class(familiar::as_familiar_data(object=fam_model, data=data), "familiarData")
})

testthat::test_that("Conversion of multiple familiarModel objects to familiarData works", {
  testthat::expect_s4_class(familiar::as_familiar_data(object=list(fam_model, fam_model), data=data), "familiarData")
})

testthat::test_that("Conversion of single familiarModel to familiarCollection works", {
  testthat::expect_s4_class(familiar::as_familiar_collection(object=fam_model, data=data), "familiarCollection")
})

testthat::test_that("Conversion of multiple familiarModel objects to familiarCollection works", {
  testthat::expect_s4_class(familiar::as_familiar_collection(object=list(fam_model, fam_model), data=data), "familiarCollection")
})

#####familiarEnsemble-based test################################################

# Create familiarEnsemble from fam_model objects.
fam_ensemble <- familiar::as_familiar_ensemble(object=list(fam_model, fam_model))

testthat::test_that("Conversion of familiarEnsemble to familiarEnsemble works", {
  testthat::expect_s4_class(familiar::as_familiar_ensemble(object=fam_ensemble), "familiarEnsemble")
})

testthat::test_that("Conversion of multiple familiarEnsemble objects to familiarEnsemble fails", {
  testthat::expect_error(familiar::as_familiar_ensemble(object=list(fam_ensemble, fam_ensemble)))
})

testthat::test_that("Conversion of familiarEnsemble to familiarData works", {
  testthat::expect_s4_class(familiar::as_familiar_data(object=fam_ensemble, data=data), "familiarData")
})

testthat::test_that("Conversion of multiple familiarEnsemble objects to familiarData fails", {
  testthat::expect_error(familiar::as_familiar_data(object=list(fam_ensemble, fam_ensemble), data=data))
})

testthat::test_that("Conversion of familiarEnsemble to familiarCollection works", {
  testthat::expect_s4_class(familiar::as_familiar_collection(object=fam_ensemble, data=data), "familiarCollection")
})

testthat::test_that("Conversion of multiple familiarEnsemble objects to familiarCollection fails", {
  testthat::expect_error(familiar::as_familiar_collection(object=list(fam_ensemble, fam_ensemble), data=data))
})


#####familiarData-based test####################################################

# Create familiarData from fam_model objects.
fam_data_1 <- familiar::as_familiar_data(object=list(fam_model, fam_model), data=data)
fam_data_2 <- familiar::as_familiar_data(object=list(fam_model, fam_model), data=data)

testthat::test_that("Conversion of familiarData to familiarEnsemble fails", {
  testthat::expect_error(familiar::as_familiar_ensemble(object=fam_data_1))
})

testthat::test_that("Conversion of multiple familiarData objects to familiarEnsemble fails", {
  testthat::expect_error(familiar::as_familiar_ensemble(object=list(fam_data_1, fam_data_2)))
})

testthat::test_that("Conversion of familiarData to familiarData works", {
  testthat::expect_s4_class(familiar::as_familiar_data(object=fam_data_1), "familiarData")
})

testthat::test_that("Conversion of multiple familiarData objects to familiarData fails", {
  testthat::expect_error(familiar::as_familiar_data(object=list(fam_data_1, fam_data_2)))
})

testthat::test_that("Conversion of familiarData to familiarCollection works", {
  testthat::expect_s4_class(familiar::as_familiar_collection(object=fam_data_1), "familiarCollection")
})

testthat::test_that("Conversion of multiple familiarData objects to familiarCollection works", {
  testthat::expect_s4_class(familiar::as_familiar_collection(object=list(fam_data_1, fam_data_2)), "familiarCollection")
})

testthat::test_that("Conversion of multiple identical familiarEnsemble objects to familiarCollection fails", {
  testthat::expect_error(familiar::as_familiar_collection(object=list(fam_data_1, fam_data_1)), "familiarCollection")
})

#####familiarCollection-based test##############################################
# Create familiarCollection from fam_data objects.
fam_collection <- familiar::as_familiar_collection(object=list(fam_data_1, fam_data_2))

testthat::test_that("Conversion of familiarCollection to familiarEnsemble fails", {
  testthat::expect_error(familiar::as_familiar_ensemble(object=fam_collection))
})

testthat::test_that("Conversion of multiple familiarCollection objects to familiarEnsemble fails", {
  testthat::expect_error(familiar::as_familiar_ensemble(object=list(fam_collection, fam_collection)))
})

testthat::test_that("Conversion of familiarCollection to familiarData fails", {
  testthat::expect_error(familiar::as_familiar_data(object=fam_collection))
})

testthat::test_that("Conversion of multiple familiarCollection objects to familiarData fails", {
  testthat::expect_error(familiar::as_familiar_data(object=list(fam_collection, fam_collection)))
})

testthat::test_that("Conversion of familiarCollection to familiarCollection works", {
  testthat::expect_s4_class(familiar::as_familiar_collection(object=fam_collection), "familiarCollection")
})

testthat::test_that("Conversion of multiple familiarCollection objects to familiarCollection fails", {
  testthat::expect_error(familiar::as_familiar_collection(object=list(fam_collection, fam_collection)))
})

#####Mixed elements test########################################################
testthat::test_that("Conversion of mixed objects to familiarEnsemble fails", {
  testthat::expect_error(familiar::as_familiar_ensemble(object=list(fam_model, fam_ensemble)))
})

testthat::test_that("Conversion of mixed objects to familiarData fails", {
  testthat::expect_error(familiar::as_familiar_data(object=list(fam_model, fam_ensemble)))
})

testthat::test_that("Conversion of mixed objects to to familiarCollection fails", {
  testthat::expect_error(familiar::as_familiar_collection(object=list(fam_model, fam_ensemble)))
})


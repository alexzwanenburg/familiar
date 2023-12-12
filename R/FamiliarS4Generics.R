# Save method for model saving. The save function appears in base and is always
# explicity imported.
setGeneric("save")

setGeneric(".predict", function(object, data, ...) standardGeneric(".predict"))

setGeneric(".predict_novelty", function(object, data, ...) standardGeneric(".predict_novelty"))

setGeneric(".predict_risk_stratification", function(object, data, ...) standardGeneric(".predict_risk_stratification"))

setGeneric(".train", function(object, data, ...) standardGeneric(".train"))

setGeneric(".train_novelty_detector", function(object, data, ...) standardGeneric(".train_novelty_detector"))

setGeneric("require_package", function(x, ...) standardGeneric("require_package"))

setGeneric("set_package_version", function(object, ...) standardGeneric("set_package_version"))

setGeneric("check_package_version", function(object, ...) standardGeneric("check_package_version"))

setGeneric("get_signature", function(object, ...) standardGeneric("get_signature"))

setGeneric("set_signature", function(object, ...) standardGeneric("set_signature"))

setGeneric("model_is_trained", function(object, ...) standardGeneric("model_is_trained"))

setGeneric("complete_familiar_ensemble", function(object, ...) standardGeneric("complete_familiar_ensemble"))

setGeneric("..get_model_file_path", function(ii, object, ...) standardGeneric("..get_model_file_path"))

setGeneric("..get_model", function(ii, object, ...) standardGeneric("..get_model"))

setGeneric("..can_detach_models", function(ii, object, ...) standardGeneric("..can_detach_models"))

setGeneric("..update_model_list", function(object, ...) standardGeneric("..update_model_list"))

setGeneric("load_models", function(object, ...) standardGeneric("load_models"))

setGeneric("is_model_loaded", function(object, ...) standardGeneric("is_model_loaded"))

setGeneric("detach_models", function(object, ...) standardGeneric("detach_models"))

setGeneric("add_identifiers", function(data, object, ...) standardGeneric("add_identifiers"))

setGeneric("add_model_name", function(data, object, ...) standardGeneric("add_model_name"))

setGeneric("get_object_name", function(object, ...) standardGeneric("get_object_name"))

setGeneric("process_input_data", function(object, data, ...) standardGeneric("process_input_data"))

setGeneric("load_delayed_data", function(data, object, ...) standardGeneric("load_delayed_data"))

setGeneric("select_data_from_samples", function(data, samples, ...) standardGeneric("select_data_from_samples"))

setGeneric("filter_features", function(data, ...) standardGeneric("filter_features"))

setGeneric("filter_missing_outcome", function(data, ...) standardGeneric("filter_missing_outcome"))

setGeneric("filter_bad_samples", function(data, ...) standardGeneric("filter_bad_samples"))

setGeneric("transform_features", function(data, ...) standardGeneric("transform_features"))

setGeneric("normalise_features", function(data, ...) standardGeneric("normalise_features"))

setGeneric("batch_normalise_features", function(data, ...) standardGeneric("batch_normalise_features"))

setGeneric("remove_missing_outcomes", function(data, ...) standardGeneric("remove_missing_outcomes"))

setGeneric("impute_features", function(data, ...) standardGeneric("impute_features"))

setGeneric("cluster_features", function(data, ...) standardGeneric("cluster_features"))

setGeneric("aggregate_data", function(data, ...) standardGeneric("aggregate_data"))

setGeneric("select_features", function(data, ...) standardGeneric("select_features"))

setGeneric("preprocess_data", function(data, object, ...) standardGeneric("preprocess_data"))

setGeneric("postprocess_data", function(data, object, ...) standardGeneric("postprocess_data"))

setGeneric("update_with_replacement", function(data, ...) standardGeneric("update_with_replacement"))

setGeneric("add_package_version", function(object, ...) standardGeneric("add_package_version"))

setGeneric("add_data_column_info", function(object, ...) standardGeneric("add_data_column_info"))

setGeneric("has_calibration_info", function(object) standardGeneric("has_calibration_info"))

setGeneric("extract_settings_from_data", function(data, ...) standardGeneric("extract_settings_from_data"))

setGeneric("set_object_parameters", function(object, ...) standardGeneric("set_object_parameters"))

setGeneric("get_required_features", function(x, ...) standardGeneric("get_required_features"))

setGeneric("get_model_features", function(x, ...) standardGeneric("get_model_features"))



# clustering methods -----------------------------------------------------------
setGeneric("set_similarity_table", function(object, data, ...) standardGeneric("set_similarity_table"))

setGeneric(".set_similarity_table", function(object, data, ...) standardGeneric(".set_similarity_table"))

setGeneric("get_similarity_names", function(object, ...) standardGeneric("get_similarity_names"))

setGeneric("get_distance_table", function(object, ...) standardGeneric("get_distance_table"))

setGeneric("get_distance_matrix", function(object, ...) standardGeneric("get_distance_matrix"))

setGeneric("apply_cluster_method", function(object, ...) standardGeneric("apply_cluster_method"))

setGeneric("create_clusters", function(object, ...) standardGeneric("create_clusters"))

setGeneric(".cluster_by_silhouette", function(object, ...) standardGeneric(".cluster_by_silhouette"))

setGeneric(".cluster_by_fixed_cut", function(object, ...) standardGeneric(".cluster_by_fixed_cut"))

setGeneric(".cluster_by_dynamic_cut", function(object, ...) standardGeneric(".cluster_by_dynamic_cut"))

setGeneric(".cluster_by_generic", function(object, ...) standardGeneric(".cluster_by_generic"))



# export methods ---------------------------------------------------------------
# Additional methods are found in FamiliarCollectionExport.R
setGeneric(".export_to_file", function(data, object, dir_path, ...) standardGeneric(".export_to_file"))

setGeneric(".export", function(x, ...) standardGeneric(".export"))



# conversion & loading ---------------------------------------------------------
# Additional methods are documented in FamiliarObjectConversion.R
setGeneric("load_familiar_object", function(object, ...) standardGeneric("load_familiar_object"))


# utilities --------------------------------------------------------------------
setGeneric("is_empty", function(x, ...) standardGeneric("is_empty"))

setGeneric("get_outcome_name", function(x, ...) standardGeneric("get_outcome_name"))

setGeneric("get_outcome_class_levels", function(x, ...) standardGeneric("get_outcome_class_levels"))

setGeneric("get_outcome_columns", function(x, ...) standardGeneric("get_outcome_columns"))

setGeneric("get_non_feature_columns", function(x, ...) standardGeneric("get_non_feature_columns"))

setGeneric("get_feature_columns", function(x, ...) standardGeneric("get_feature_columns"))

setGeneric("get_n_features", function(x, ...) standardGeneric("get_n_features"))

setGeneric("get_n_samples", function(x, ...) standardGeneric("get_n_samples"))

setGeneric("has_feature_data", function(x, ...) standardGeneric("has_feature_data"))

setGeneric("get_unique_row_names", function(x, ...) standardGeneric("get_unique_row_names"))

setGeneric("get_class_probability_name", function(x, ...) standardGeneric("get_class_probability_name"))

setGeneric("encode_categorical_variables", function(object, data, ...) standardGeneric("encode_categorical_variables"))

setGeneric("get_placeholder_prediction_table", function(object, data, ...) standardGeneric("get_placeholder_prediction_table"))

setGeneric("has_bad_training_data", function(object, data, ...) standardGeneric("has_bad_training_data"))

setGeneric("has_optimised_hyperparameters", function(object, ...) standardGeneric("has_optimised_hyperparameters"))

setGeneric("fam_sample", function(x, ...) standardGeneric("fam_sample"))

setGeneric("get_bootstrap_sample", function(data, ...) standardGeneric("get_bootstrap_sample"))

setGeneric("get_subsample", function(data, ...) standardGeneric("get_subsample"))

setGeneric("create_instance_weights", function(data, ...) standardGeneric("create_instance_weights"))



# familiarModel methods --------------------------------------------------------
setGeneric("promote_learner", function(object, ...) standardGeneric("promote_learner"))

setGeneric("get_default_hyperparameters", function(object, ...) standardGeneric("get_default_hyperparameters"))

setGeneric("..train", function(object, data, ...) standardGeneric("..train"))

setGeneric("..train_naive", function(object, data, ...) standardGeneric("..train_naive"))

setGeneric("..predict", function(object, data, ...) standardGeneric("..predict"))

setGeneric("..predict_survival_probability", function(object, data, time, ...) standardGeneric("..predict_survival_probability"))

setGeneric("..set_recalibration_model", function(object, data, ...) standardGeneric("..set_recalibration_model"))

setGeneric("..vimp", function(object, ...) standardGeneric("..vimp"))

setGeneric("get_prediction_type", function(object, ...) standardGeneric("get_prediction_type"))

setGeneric("..set_calibration_info", function(object, data, ...) standardGeneric("..set_calibration_info"))

setGeneric("..set_risk_stratification_thresholds", function(object, data, ...) standardGeneric("..set_risk_stratification_thresholds"))

setGeneric("..set_vimp_parameters", function(object, ...) standardGeneric("..set_vimp_parameters"))

setGeneric("..get_distribution_family", function(object, ...) standardGeneric("..get_distribution_family"))

setGeneric("..update_outcome", function(object, data, ...) standardGeneric("..update_outcome"))

setGeneric("..update_warnings", function(object, ...) standardGeneric("..update_warnings"))

setGeneric("..update_errors", function(object, ...) standardGeneric("..update_errors"))

setGeneric("optimise_hyperparameters", function(object, data, ...) standardGeneric("optimise_hyperparameters"))

setGeneric("trim_model", function(object, ...) standardGeneric("trim_model"))

setGeneric(".trim_model", function(object, ...) standardGeneric(".trim_model"))

setGeneric("requires_naive_model", function(object, ...) standardGeneric("requires_naive_model"))



# familiarNoveltyDetector methods ----------------------------------------------
setGeneric("promote_detector", function(object, ...) standardGeneric("promote_detector"))



# familiarVimpMethod methods ---------------------------------------------------
setGeneric(".vimp", function(object, ...) standardGeneric(".vimp"))

setGeneric("promote_vimp_method", function(object, ...) standardGeneric("promote_vimp_method"))

setGeneric("prepare_vimp_object", function(data, ...) standardGeneric("prepare_vimp_object"))



# familiarMetric methods -------------------------------------------------------
setGeneric("is_higher_better", function(metric, ...) standardGeneric("is_higher_better"))

setGeneric("compute_metric_score", function(metric, ...) standardGeneric("compute_metric_score"))

setGeneric("compute_objective_score", function(metric, ...) standardGeneric("compute_objective_score"))

setGeneric("set_metric_baseline_value", function(metric, ...) standardGeneric("set_metric_baseline_value"))



# familiarCollection methods ---------------------------------------------------
setGeneric(".set_labels", function(x, ...) standardGeneric(".set_labels"))

setGeneric(".get_labels", function(x, ...) standardGeneric(".get_labels"))

setGeneric(".construct_label_table", function(x, ...) standardGeneric(".construct_label_table"))

setGeneric(".apply_labels", function(data, object, ...) standardGeneric(".apply_labels"))

setGeneric("set_object_name", function(x, ...) standardGeneric("set_object_name"))

setGeneric("set_data_set_names", function(x, ...) standardGeneric("set_data_set_names"))

setGeneric("set_learner_names", function(x, ...) standardGeneric("set_learner_names"))

setGeneric("set_fs_method_names", function(x, ...) standardGeneric("set_fs_method_names"))

setGeneric("set_feature_names", function(x, ...) standardGeneric("set_feature_names"))

setGeneric("set_risk_group_names", function(x, ...) standardGeneric("set_risk_group_names"))

setGeneric("set_class_names", function(x, ...) standardGeneric("set_class_names"))

setGeneric("get_data_set_names", function(x, ...) standardGeneric("get_data_set_names"))

setGeneric("get_learner_names", function(x, ...) standardGeneric("get_learner_names"))

setGeneric("get_fs_method_names", function(x, ...) standardGeneric("get_fs_method_names"))

setGeneric("get_feature_names", function(x, ...) standardGeneric("get_feature_names"))

setGeneric("get_risk_group_names", function(x, ...) standardGeneric("get_risk_group_names"))

setGeneric("get_class_names", function(x, ...) standardGeneric("get_class_names"))

setGeneric("get_data_set_name_levels", function(x, ...) standardGeneric("get_data_set_name_levels"))

setGeneric("get_learner_name_levels", function(x, ...) standardGeneric("get_learner_name_levels"))

setGeneric("get_fs_method_name_levels", function(x, ...) standardGeneric("get_fs_method_name_levels"))

setGeneric("get_feature_name_levels", function(x, ...) standardGeneric("get_feature_name_levels"))

setGeneric("get_risk_group_name_levels", function(x, ...) standardGeneric("get_risk_group_name_levels"))

setGeneric("get_class_name_levels", function(x, ...) standardGeneric("get_class_name_levels"))



# familiarDataElement methods --------------------------------------------------
setGeneric("extract_dispatcher", function(object, proto_data_element, ...) standardGeneric("extract_dispatcher"))

setGeneric("identify_element_sets", function(x, ...) standardGeneric("identify_element_sets"))

setGeneric("merge_data_elements", function(x, ...) standardGeneric("merge_data_elements"))

setGeneric("add_data_element_identifier", function(x, ...) standardGeneric("add_data_element_identifier"))

setGeneric("add_data_element_bootstrap", function(x, ...) standardGeneric("add_data_element_bootstrap"))

setGeneric(".add_point_estimate_from_elements", function(x, ...) standardGeneric(".add_point_estimate_from_elements"))

setGeneric(".identifier_as_data_attribute", function(x, ...) standardGeneric(".identifier_as_data_attribute"))

setGeneric(".compute_data_element_estimates", function(x, ...) standardGeneric(".compute_data_element_estimates"))

setGeneric("..compute_data_element_estimates", function(x, ...) standardGeneric("..compute_data_element_estimates"))

setGeneric(".merge_slots_into_data", function(x, ...) standardGeneric(".merge_slots_into_data"))

setGeneric(".extract_slots_from_data", function(x, ...) standardGeneric(".extract_slots_from_data"))

setGeneric(".as_data_table", function(x, ...) standardGeneric(".as_data_table"))

setGeneric("collect", function(x, ...) standardGeneric("collect"))


# vimpTable methods ------------------------------------------------------------
setGeneric("decode_vimp_table", function(x, ...) standardGeneric("decode_vimp_table"))

setGeneric("decluster_vimp_table", function(x, ...) standardGeneric("decluster_vimp_table"))

setGeneric("recluster_vimp_table", function(x, ...) standardGeneric("recluster_vimp_table"))

setGeneric("rank_vimp_table", function(x, ...) standardGeneric("rank_vimp_table"))

setGeneric("preprocess_vimp_table", function(x, ...) standardGeneric("preprocess_vimp_table"))

setGeneric("remove_signature_features", function(x, ...) standardGeneric("remove_signature_features"))

setGeneric("update_vimp_table_to_reference", function(x, ...) standardGeneric("update_vimp_table_to_reference"))

setGeneric("collect_vimp_table", function(x, ...) standardGeneric("collect_vimp_table"))



# featureInfo methods ----------------------------------------------------------
setGeneric("is_available", function(object, ...) standardGeneric("is_available"))

setGeneric("is_in_signature", function(object, ...) standardGeneric("is_in_signature"))

setGeneric("is_in_novelty", function(object, ...) standardGeneric("is_in_novelty"))

setGeneric("update_removed_status", function(object, ...) standardGeneric("update_removed_status"))

setGeneric("feature_info_complete", function(object, ...) standardGeneric("feature_info_complete"))

setGeneric("add_feature_info_parameters", function(object, data, ...) standardGeneric("add_feature_info_parameters"))

setGeneric("apply_feature_info_parameters", function(object, data, ...) standardGeneric("apply_feature_info_parameters"))

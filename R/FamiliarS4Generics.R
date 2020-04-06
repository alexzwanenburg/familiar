# Save method for model saving. The save function appears in base and is always explicity imported.
setGeneric("save")

# Predict method. This is a standard method converted to S4.
setGeneric("predict")

setGeneric("train", function(object, data, ...) {standardGeneric("train")})

setGeneric("model_is_trained", function(object, ...) {standardGeneric("model_is_trained")})

setGeneric("extract_calibration_info", function(object, ...) {standardGeneric("extract_calibration_info")})

setGeneric("assess_stratification", function(object, ...) {standardGeneric("assess_stratification")})

setGeneric("assess_calibration", function(object, ...) {standardGeneric("assess_calibration")})

setGeneric("compute_calibration_data", function(object, data, ...) {standardGeneric("compute_calibration_data")})

setGeneric("assess_performance", function(object, ...) {standardGeneric("assess_performance")})

setGeneric("assign_risk_groups", function(object, prediction_data, ...) {standardGeneric("assign_risk_groups")})

setGeneric("complete_familiar_ensemble", function(object, ...) standardGeneric("complete_familiar_ensemble"))

setGeneric("load_models", function(object, ...) {standardGeneric("load_models")})

setGeneric("is_model_loaded", function(object, ...) {standardGeneric("is_model_loaded")})

setGeneric("detach_models", function(object, ...) {standardGeneric("detach_models")})

setGeneric("add_identifiers", function(data, object, ...) {standardGeneric("add_identifiers")})

setGeneric("add_model_name", function(data, object, ...) {standardGeneric("add_model_name")})

setGeneric("get_object_name", function(object, ...) {standardGeneric("get_object_name")})

setGeneric("process_input_data", function(object, data, ...) {standardGeneric("process_input_data")})

setGeneric("create_data_object", function(object, data, ...) {standardGeneric("create_data_object")})

setGeneric("load_delayed_data", function(data, object, ...) standardGeneric("load_delayed_data"))

setGeneric("select_data_from_samples", function(data, samples, ...) {standardGeneric("select_data_from_samples")})

setGeneric("get_unique_samples", function(data, ...) {standardGeneric("get_unique_samples")})

setGeneric("filter_features", function(data, ...) {standardGeneric("filter_features")})

setGeneric("filter_missing_outcome", function(data, ...) {standardGeneric("filter_missing_outcome")})

setGeneric("filter_bad_samples", function(data, ...) {standardGeneric("filter_bad_samples")})

setGeneric("transform_features", function(data, ...) {standardGeneric("transform_features")})

setGeneric("normalise_features", function(data, ...) {standardGeneric("normalise_features")})

setGeneric("batch_normalise_features", function(data, ...) standardGeneric("batch_normalise_features"))

setGeneric("impute_features", function(data, ...) {standardGeneric("impute_features")})

setGeneric("cluster_features", function(data, ...) {standardGeneric("cluster_features")})

setGeneric("aggregate_data", function(data, ...) {standardGeneric("aggregate_data")})

setGeneric("select_features", function(data, ...) {standardGeneric("select_features")})

setGeneric("preprocess_data", function(data, object, ...) {standardGeneric("preprocess_data")})

setGeneric("update_with_replacement", function(data, ...) {standardGeneric("update_with_replacement")})

setGeneric("add_package_version", function(object, ...) {standardGeneric("add_package_version")})

##### Methods to see and update labels of data in familiarCollection objects.
setGeneric(".set_labels", function(x, ...) {standardGeneric(".set_labels")})

setGeneric(".get_labels", function(x, ...) {standardGeneric(".get_labels")})

setGeneric(".construct_label_table", function(x, ...) {standardGeneric(".construct_label_table")})

setGeneric(".apply_labels", function(data, object, ...) {standardGeneric(".apply_labels")})

setGeneric("set_data_set_names", function(x, ...) {standardGeneric("set_data_set_names")})

setGeneric("set_learner_names", function(x, ...) {standardGeneric("set_learner_names")})

setGeneric("set_fs_method_names", function(x, ...) {standardGeneric("set_fs_method_names")})

setGeneric("set_feature_names", function(x, ...) {standardGeneric("set_feature_names")})

setGeneric("set_risk_group_names", function(x, ...) {standardGeneric("set_risk_group_names")})

setGeneric("set_class_names", function(x, ...) {standardGeneric("set_class_names")})

setGeneric("get_data_set_names", function(x, ...) {standardGeneric("get_data_set_names")})

setGeneric("get_learner_names", function(x, ...) {standardGeneric("get_learner_names")})

setGeneric("get_fs_method_names", function(x, ...) {standardGeneric("get_fs_method_names")})

setGeneric("get_feature_names", function(x, ...) {standardGeneric("get_feature_names")})

setGeneric("get_risk_group_names", function(x, ...) {standardGeneric("get_risk_group_names")})

setGeneric("get_class_names", function(x, ...) {standardGeneric("get_class_names")})

setGeneric("get_data_set_name_levels", function(x, ...) {standardGeneric("get_data_set_name_levels")})

setGeneric("get_learner_name_levels", function(x, ...) {standardGeneric("get_learner_name_levels")})

setGeneric("get_fs_method_name_levels", function(x, ...) {standardGeneric("get_fs_method_name_levels")})

setGeneric("get_feature_name_levels", function(x, ...) {standardGeneric("get_feature_name_levels")})

setGeneric("get_risk_group_name_levels", function(x, ...) {standardGeneric("get_risk_group_name_levels")})

setGeneric("get_class_name_levels", function(x, ...) {standardGeneric("get_class_name_levels")})


##### Export methods #####
# Additional methods are found in FamiliarCollectionExport.R
setGeneric(".summarise_model_performance", function(object, ...) {standardGeneric(".summarise_model_performance")})

setGeneric(".export_to_file", function(data, object, dir_path, ...) {standardGeneric(".export_to_file")})

##### conversion & loading #####
# Additional methods are documented in FamiliarObjectConversion.R
setGeneric("load_familiar_object", function(object, ...) {standardGeneric("load_familiar_object")})


##### featureInfo specific methods ######
setGeneric("is_available", function(object, ...) {standardGeneric("is_available")})

setGeneric("is_in_signature", function(object, ...) {standardGeneric("is_in_signature")})

setGeneric("update_removed_status", function(object, ...) {standardGeneric("update_removed_status")})


##### plotting methods #####
setGeneric("plot_all", function(object, ...) {standardGeneric("plot_all")})


##### utilities #####
setGeneric("is_empty", function(x, ...) standardGeneric("is_empty"))

setGeneric("get_outcome_class_levels", function(x, ...) standardGeneric("get_outcome_class_levels"))

setGeneric("get_outcome_columns", function(x, ...) standardGeneric("get_outcome_columns"))

setGeneric("get_non_feature_columns", function(x, ...) standardGeneric("get_non_feature_columns"))

setGeneric("get_feature_columns", function(x, ...) standardGeneric("get_feature_columns"))

setGeneric("get_n_features", function(x, ...) standardGeneric("get_n_features"))

setGeneric("has_feature_data", function(x, ...) standardGeneric("has_feature_data"))

setGeneric("get_class_probability_name", function(x, ...) standardGeneric("get_class_probability_name"))

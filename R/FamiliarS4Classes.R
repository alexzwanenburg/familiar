#### familiarModel -------------------------------------------------------------

#' Familiar model.
#'
#' A familiarModel object is a self-contained model that can be applied to
#' generate predictions for a dataset. familiarModel objects form the parent
#' class of learner-specific child classes.
#'
#' @slot name Name of the familiarModel object.
#' @slot model The actual model trained using a specific algorithm, e.g. a
#'   random forest from the `ranger` package, or a LASSO model from `glmnet`.
#' @slot outcome_type Outcome type of the data used to create the object.
#' @slot outcome_info Outcome information object, which contains additional
#'   information concerning the outcome, such as class levels.
#' @slot feature_info List of objects containing feature information, e.g.,
#'   name, class levels, transformation, normalisation and clustering
#'   parameters.
#' @slot data_column_info Data information object containing information
#'   regarding identifier column names and outcome column names.
#' @slot hyperparameters Set of hyperparameters used to train the model.
#' @slot hyperparameter_data Information generated during hyperparameter
#'   optimisation.
#' @slot calibration_model One or more models used to recalibrate the model
#'   output. Currently only used by some models.
#' @slot novelty_detector A familiarNoveltyDetector object that can be used to
#'   detect out-of-distribution samples.
#' @slot learner Learning algorithm used to create the model.
#' @slot fs_method Feature selection method used to determine variable
#'   importance for the model.
#' @slot required_features The set of features required for complete
#'   reproduction, i.e. with imputation.
#' @slot model_features The set of features that is used to train the model,
#' @slot novelty_features The set of features that is used to train all novelty
#'   detectors in the ensemble.
#' @slot calibration_info Calibration information, e.g. baseline survival in the
#'   development cohort.
#' @slot km_info Data concerning stratification into risk groups.
#' @slot run_table Run table for the data used to train the model. Used
#'   internally.
#' @slot settings A copy of the evaluation configuration parameters used at
#'   model creation. These are used as default parameters when evaluating the
#'   model (technically, familiarEnsemble) to create a familiarData object.
#' @slot is_trimmed Flag that indicates whether the model, stored in the `model`
#'   slot, has been trimmed.
#' @slot trimmed_function List of functions whose output has been captured prior
#'   to trimming the model.
#' @slot messages List of warning and error messages generated during training.
#' @slot project_id Identifier of the project that generated the familiarModel
#'   object.
#' @slot familiar_version Version of the familiar package.
#' @slot package Name of package(s) required to executed the model itself, e.g.
#'   `ranger` or `glmnet`.
#' @slot package_version Version of the packages mentioned in the `package`
#'   attribute.
#'
#' @export

setClass("familiarModel",
         slots = list(
           # Model name.
           name = "character",
           # Model container
           model = "ANY",
           # Outcome type
           outcome_type = "character",
           # Outcome info, such as class levels, mean values etc.
           outcome_info = "ANY",
           # Data required for feature pre-processing
           feature_info = "ANY",
           # Info related to the columns in the dataset.
           data_column_info = "ANY",
           # Hyper-parameters (typically stored in the model as well)
           hyperparameters = "ANY",
           # Hyperparameter data, e.g. for visualising the hyperparameter space.
           hyperparameter_data = "ANY",
           # Models used for recalibration
           calibration_model = "ANY",
           # Model used for novelty detection
           novelty_detector = "ANY",
           # Name of learner
           learner = "character",
           # Name of feature selection method
           fs_method = "character",
           # Required features for complete reconstruction, including
           # imputation.
           required_features = "ANY",
           # Features that are required for the model.
           model_features = "ANY",
           # Features that are required for novelty detection.
           novelty_features = "ANY",
           # Run table for the current model
           run_table = "ANY",
           # Information required to assess model calibrations (e.g. baseline survival)
           calibration_info = "ANY",
           # Information required to do perform a Kaplan-Meier analysis using the model
           km_info = "ANY",
           # Evaluation settings. This allows default values for external use of
           # existing models.
           settings = "ANY",
           # Flags trimming of the model
           is_trimmed = "logical",
           # Restores functions lost due to model trimming, such as coef or
           # vcov.
           trimmed_function = "list",
           # List of warning and error messages encountered during training.
           messages = "list",
           # Project identifier for consistency tracking
           project_id = "ANY",
           # Package version for backward compatibility
           familiar_version = "ANY",
           # Name of the package required to train the learner.
           package = "ANY",
           # Version of the learner for reproducibility.
           package_version = "ANY"
         ),
         prototype = list(
           name = character(0),
           model = NULL,
           outcome_type = NA_character_,
           outcome_info = NULL,
           feature_info = NULL,
           data_column_info = NULL,
           hyperparameters = NULL,
           hyperparameter_data = NULL,
           calibration_model = NULL,
           novelty_detector = NULL,
           learner = NA_character_,
           fs_method = NA_character_,
           required_features = NULL,
           model_features = NULL,
           novelty_features = NULL,
           calibration_info = NULL,
           km_info = NULL,
           run_table = NULL,
           settings = NULL,
           is_trimmed = FALSE,
           trimmed_function = list(),
           messages = list(),
           project_id = NULL,
           familiar_version = NULL,
           package = NULL,
           package_version = NULL
         )
)


#### familiarEnsemble ----------------------------------------------------------

#' Ensemble of familiar models.
#'
#' A familiarEnsemble object contains one or more familiarModel objects.
#'
#' @slot name Name of the familiarEnsemble object.
#' @slot model_list List of attached familiarModel objects, or paths to these
#'   objects. Familiar attaches familiarModel objects when required.
#' @slot outcome_type Outcome type of the data used to create the object.
#' @slot outcome_info Outcome information object, which contains additional
#'   information concerning the outcome, such as class levels.
#' @slot data_column_info Data information object containing information
#'   regarding identifier column names and outcome column names.
#' @slot learner Learning algorithm used to create the models in the ensemble.
#' @slot fs_method Feature selection method used to determine variable
#'   importance for the models in the ensemble.
#' @slot feature_info List of objects containing feature information, e.g.,
#'   name, class levels, transformation, normalisation and clustering
#'   parameters.
#' @slot required_features The set of features required for complete
#'   reproduction, i.e. with imputation.
#' @slot model_features The combined set of features that is used to train the
#'   models in the ensemble,
#' @slot novelty_features The combined set of features that is used to train
#'   all novelty detectors in the ensemble.
#' @slot run_table Run table for the data used to train the ensemble. Used
#'   internally.
#' @slot calibration_info Calibration information, e.g. baseline survival in the
#'   development cohort.
#' @slot model_dir_path Path to folder containing the familiarModel objects. Can
#'   be updated using the `update_model_dir_path` method.
#' @slot auto_detach Flag used to determine whether models should be detached
#'   from the model after use, or not. Used internally.
#' @slot settings A copy of the evaluation configuration parameters used at
#'   model creation. These are used as default parameters when evaluating the
#'   ensemble to create a familiarData object.
#' @slot project_id Identifier of the project that generated the
#'   underlying familiarModel object(s).
#' @slot familiar_version Version of the familiar package.
#'
#' @export

setClass("familiarEnsemble",
         slots = list(
           # Ensemble name
           name = "character",
           # Model container.
           model_list = "ANY",
           # Model outcome type.
           outcome_type = "character",
           # Outcome info, such as class levels, mean values etc.
           outcome_info = "ANY",
           # Info related to the columns in the dataset.
           data_column_info = "ANY",
           # Name of learner.
           learner = "character",
           # Name of feature selection method.
           fs_method = "character",
           # Data required for feature pre-processing.
           feature_info = "ANY",
           # Required features for complete reconstruction, including
           # imputation.
           required_features = "ANY",
           # Features that are required for reconstruction, without imputation
           # (i.e. features that are in the signature directly or as part of a
           # cluster)
           model_features = "ANY",
           # Features that are required for novelty detection.
           novelty_features = "ANY",
           # Set of run tables for the current ensemble. This is only required
           # for processing internal data.
           run_table = "ANY",
           # Information required to assess model calibrations (e.g. baseline
           # survival)
           calibration_info = "ANY",
           # Path to the model directory. Required for auto-detaching.
           model_dir_path = "character",
           # Flag that signals auto-detaching. This means that models are loaded
           # and discarded one-by-one. This saves memory, but comes at the cost
           # of IO overhead. Moreover, its not possible if the models are not
           # stored on drive in the first place.
           auto_detach = "logical",
           # Evaluation settings. This allows default values for external use of
           # existing models.
           settings = "ANY",
           # Project identifier for consistency tracking.
           project_id = "ANY",
           # Package version for backward compatibility checks.
           familiar_version = "ANY"
         ),
         prototype = list(
           name = character(0),
           model_list = NULL,
           outcome_type = NA_character_,
           outcome_info = NULL,
           data_column_info = NULL,
           learner = NA_character_,
           fs_method = NA_character_,
           feature_info = NULL,
           required_features = NULL,
           model_features = NULL,
           novelty_features = NULL,
           run_table = NULL,
           calibration_info = NULL,
           model_dir_path = NA_character_,
           auto_detach = FALSE,
           settings = NULL,
           project_id = NULL,
           familiar_version = NULL
         )
)


#### familiarData --------------------------------------------------------------

#' Dataset obtained after evaluating models on a dataset.
#'
#' A familiarData object is created by evaluating familiarEnsemble or
#' familiarModel objects on a dataset. Multiple familiarData objects are
#' aggregated in a familiarCollection object.
#'
#' @slot name Name of the dataset, e.g. training or internal validation.
#' @slot outcome_type Outcome type of the data used to create the object.
#' @slot outcome_info Outcome information object, which contains additional
#'   information concerning the outcome, such as class levels.
#' @slot fs_vimp Variable importance data collected from feature selection
#'   methods.
#' @slot model_vimp Variable importance data collected from model-specific
#'   algorithms implemented by models created by familiar.
#' @slot permutation_vimp Data collected for permutation variable importance.
#' @slot hyperparameters Hyperparameters collected from created models.
#' @slot hyperparameter_data Additional data concerning hyperparameters. This is
#'   currently not used yet.
#' @slot required_features The set of features required for complete
#'   reproduction, i.e. with imputation.
#' @slot model_features The set of features that are required for using the
#'   model or ensemble of models, but without imputation.
#' @slot learner Learning algorithm used to create the model or ensemble of
#'   models.
#' @slot fs_method Feature selection method used to determine variable
#'   importance for the model or ensemble of models.
#' @slot pooling_table Run table for the data underlying the familiarData
#'   object. Used internally.
#' @slot prediction_data Model predictions for a model or ensemble of models for
#'   the underlying dataset.
#' @slot confusion_matrix Confusion matrix for a model or ensemble of models,
#'   based on the underlying dataset.
#' @slot decision_curve_data Decision curve analysis data for a model or
#'   ensemble of models, based on the underlying dataset.
#' @slot calibration_info Calibration information, e.g. baseline survival in the
#'   development cohort.
#' @slot calibration_data Calibration data for a model or ensemble of models,
#'   based on the underlying dataset.
#' @slot model_performance Model performance data for a model or ensemble of
#'   models, based on the underlying dataset.
#' @slot km_info Information concerning risk-stratification cut-off values..
#' @slot km_data Kaplan-Meier survival data for a model or ensemble of models,
#'   based on the underlying dataset.
#' @slot auc_data AUC-ROC and AUC-PR data for a model or ensemble of models,
#'   based on the underlying dataset.
#' @slot ice_data Individual conditional expectation data for features included
#'   in a model or ensemble of models, based on the underlying dataset. Partial
#'   dependence data are computed on the fly from these data.
#' @slot univariate_analysis Univariate analysis of the underlying dataset.
#' @slot feature_expressions Feature expression values of the underlying
#'   dataset.
#' @slot feature_similarity Feature similarity information of the underlying
#'   dataset.
#' @slot sample_similarity Sample similarity information of the underlying
#'   dataset.
#' @slot is_validation Signifies whether the underlying data forms a validation
#'   dataset. Used internally.
#' @slot generating_ensemble Name of the ensemble that was used to generate the
#'   familiarData object.
#' @slot project_id Identifier of the project that generated the familiarData
#'   object.
#' @slot familiar_version Version of the familiar package.
#' 
#' familiarData objects contain information obtained by evaluating a single
#' model or single ensemble of models on a dataset.
#'
#' @export

setClass("familiarData",
         slots = list(
           # Name of the familiar data set
           name = "character",
           # Model outcome type
           outcome_type = "character",
           # Outcome info, such as class levels, mean values etc.
           outcome_info = "ANY",
           # Feature selection variable importance
           fs_vimp = "ANY",
           # Model variable importance
           model_vimp = "ANY",
           # Permutation variable importance
           permutation_vimp = "ANY",
           # Hyper-parameters
           hyperparameters = "ANY",
           # Hyperparameter data, e.g. for visualising the hyperparameter space.
           hyperparameter_data = "ANY",
           # Required features to update the data
           required_features = "ANY",
           # Features that are required for reconstruction, without imputation
           # (i.e. features that are in the signature directly or as part of a
           # cluster)
           model_features = "ANY",
           # Name of learner
           learner = "character",
           # Name of feature selection method
           fs_method = "character",
           # Run table for the current data
           pooling_table = "ANY",
           # Model predictions for later reference
           prediction_data = "ANY",
           # Confusion matrix for categorical outcomes
           confusion_matrix = "ANY",
           # Data for decision curve analysis
           decision_curve_data = "ANY",
           # Calibration information, e.g. baseline survival
           calibration_info = "ANY",
           # Calibration test information
           calibration_data = "ANY",
           # Model performance metrics
           model_performance = "ANY",
           # Kaplan-Meier cut-offs
           km_info = "ANY",
           # Kaplan-Meier data
           km_data = "ANY",
           # AUC data (for plotting)
           auc_data = "ANY",
           # Information concerning the univariate importance of features
           univariate_analysis = "ANY",
           # Information concerning feature expression for individual samples
           feature_expressions = "ANY",
           # Information concerning mutual correlations between features
           feature_similarity = "ANY",
           # Information concerning similarity between samples.
           sample_similarity = "ANY",
           # Information on individual conditional expectation
           ice_data = "ANY",
           # Flag to signal whether the data concerns validation data (TRUE) or
           # development data (FALSE)
           is_validation = "logical",
           # Name of the model ensemble used to generate this data
           generating_ensemble = "character",
           # Project identifier
           project_id = "ANY",
           # Package version for backward compatibility
           familiar_version = "ANY"
         ),
         prototype = list(
           name = character(0),
           outcome_type = NA_character_,
           outcome_info = NULL,
           fs_vimp = NULL,
           model_vimp = NULL,
           permutation_vimp = NULL,
           hyperparameters = NULL,
           hyperparameter_data = NULL,
           required_features = NULL,
           model_features = NULL,
           learner = NA_character_,
           fs_method = NA_character_,
           pooling_table = NULL,
           prediction_data = NULL,
           confusion_matrix = NULL,
           decision_curve_data = NULL,
           calibration_info = NULL,
           calibration_data = NULL,
           model_performance = NULL,
           km_info = NULL,
           km_data = NULL,
           auc_data = NULL,
           univariate_analysis = NULL,
           feature_expressions = NULL,
           feature_similarity = NULL,
           sample_similarity = NULL,
           ice_data = NULL,
           is_validation = FALSE,
           generating_ensemble = character(0),
           project_id = NULL,
           familiar_version = NULL
         )
)

#### familiarCollection --------------------------------------------------------

#' Collection of familiar data.
#'
#' A familiarCollection object aggregates data from one or more familiarData
#' objects.
#' 
#' @slot name Name of the collection. 
#' @slot data_sets Name of the individual underlying datasets.
#' @slot outcome_type Outcome type for which the collection was created.
#' @slot outcome_info Outcome information object, which contains information
#'   concerning the outcome, such as class levels.
#' @slot fs_vimp Variable importance data collected by feature selection
#'   methods.
#' @slot model_vimp Variable importance data collected from model-specific
#'   algorithms implemented by models created by familiar.
#' @slot permutation_vimp Data collected for permutation variable importance.
#' @slot hyperparameters Hyperparameters collected from created models.
#' @slot hyperparameter_data Additional data concerning hyperparameters. This is
#'   currently not used yet.
#' @slot required_features The set of features required for complete
#'   reproduction, i.e. with imputation.
#' @slot model_features The set of features that are required for using the
#'   model, but without imputation.
#' @slot learner Learning algorithm(s) used for data in the collection.
#' @slot fs_method Feature selection method(s) used for data in the collection.
#' @slot prediction_data Model predictions for the data in the collection.
#' @slot confusion_matrix Confusion matrix information for the data in the
#'   collection.
#' @slot decision_curve_data Decision curve analysis data for the data in the
#'   collection.
#' @slot calibration_info Calibration information, e.g. baseline survival in the
#'   development cohort.
#' @slot calibration_data Model calibration data collected from data in the
#'   collection.
#' @slot model_performance Collection of model performance data for data in the
#'   collection.
#' @slot km_info Information concerning risk-stratification cut-off values for
#'   data in the collection.
#' @slot km_data Kaplan-Meier survival data for data in the collection.
#' @slot auc_data AUC-ROC and AUC-PR data for data in the collection.
#' @slot ice_data Individual conditional expectation data for data in the
#'   collection. Partial dependence data are computed on the fly from these
#'   data.
#' @slot univariate_analysis Univariate analysis results of data in the
#'   collection.
#' @slot feature_expressions Feature expression values for data in the
#'   collection.
#' @slot feature_similarity Feature similarity information for data in the
#'   collection.
#' @slot sample_similarity Sample similarity information for data in the
#'   collection.
#' @slot data_set_labels Labels for the different datasets in the collection.
#'   See `get_data_set_names` and `set_data_set_names`.
#' @slot learner_labels Labels for the different learning algorithms used to
#'   create the collection. See `get_learner_names` and `set_learner_names`.
#' @slot fs_method_labels Labels for the different feature selection methods
#'   used to create the collection. See `get_fs_method_names` and
#'   `set_fs_method_names`.
#' @slot feature_labels Labels for the features in this collection. See
#'   `get_feature_names` and `set_feature_names`.
#' @slot km_group_labels Labels for the risk strata in this collection. See
#'   `get_risk_group_names` and `set_risk_group_names`.
#' @slot class_labels Labels of the response variable. See `get_class_names` and
#'   `set_class_names`.
#' @slot project_id Identifier of the project that generated this collection.
#' @slot familiar_version Version of the familiar package.
#'
#' familiarCollection objects collect data from one or more familiarData
#' objects. This objects are important, as all plotting and export functions use
#' it. The fact that one can supply familiarModel, familiarEnsemble and
#' familiarData objects as arguments for these methods, is because familiar
#' internally converts these into familiarCollection objects prior to executing
#' the method.
#'
#' @export

setClass("familiarCollection",
         slots = list(
           # Name of the collection
           name = "character",
           # Name of the underlying data sets
           data_sets = "character",
           # Model outcome type
           outcome_type = "character",
           # Outcome info, such as class levels, mean values etc.
           outcome_info = "ANY",
           # Feature selection variable importance
           fs_vimp = "ANY",
           # Model variable importance
           model_vimp = "ANY",
           # Permutation variable importance
           permutation_vimp = "ANY",
           # Hyper-parameters
           hyperparameters = "ANY",
           # Hyperparameter data, e.g. for visualising the hyperparameter space.
           hyperparameter_data = "ANY",
           # Required features to update the data
           required_features = "ANY",
           # Important features, e.g. features that ended up in a signature
           # individually or as part of a cluster
           model_features = "ANY",
           # Name of learner
           learner = "character",
           # Name of feature selection method
           fs_method = "character",
           # Model predictions for later reference
           prediction_data = "ANY",
           # Confusion matrix for categorical outcomes
           confusion_matrix = "ANY",
           # Data for decision curve analysis
           decision_curve_data = "ANY",
           # Calibration information, e.g. baseline survival
           calibration_info = "ANY",
           # Calibration test information
           calibration_data = "ANY",
           # Model performance metrics
           model_performance = "ANY",
           # Kaplan-Meier cut-offs
           km_info = "ANY",
           # Kaplan-Meier data
           km_data = "ANY",
           # AUC data (for plotting)
           auc_data = "ANY",
           # Information concerning the univariate importance of features
           univariate_analysis = "ANY",
           # Information concerning feature expression for individual samples
           feature_expressions = "ANY",
           # Information concerning mutual correlations between features
           feature_similarity = "ANY",
           # Information concerning similarity between samples.
           sample_similarity = "ANY",
           # Information on individual conditional expectation
           ice_data = "ANY",
           # Label and order of data names
           data_set_labels = "ANY",
           # Label and order of learners
           learner_labels = "ANY",
           # Label and order of feature selection methods
           fs_method_labels = "ANY",
           # Label and order of features
           feature_labels = "ANY",
           # Label and order of kaplan-meier groups
           km_group_labels = "ANY",
           # Label and order of outcome classes
           class_labels = "ANY",
           # Project identifier
           project_id = "ANY",
           # Package version for backward compatibility
           familiar_version = "ANY"
         ),
         prototype = list(
           name = character(0),
           data_sets = character(0),
           outcome_type = NA_character_,
           outcome_info = NULL,
           fs_vimp = NULL,
           model_vimp = NULL,
           permutation_vimp = NULL,
           hyperparameters = NULL,
           hyperparameter_data = NULL,
           required_features = NULL,
           model_features = NULL,
           learner = NA_character_,
           fs_method = NA_character_,
           prediction_data = NULL,
           confusion_matrix = NULL,
           decision_curve_data = NULL,
           calibration_info = NULL,
           calibration_data = NULL,
           model_performance = NULL,
           km_info = NULL,
           km_data = NULL,
           auc_data = NULL,
           univariate_analysis = NULL,
           feature_expressions = NULL,
           feature_similarity = NULL,
           sample_similarity = NULL,
           ice_data = NULL,
           data_set_labels = NULL,
           learner_labels = NULL,
           fs_method_labels = NULL,
           feature_labels = NULL,
           km_group_labels = NULL,
           class_labels = NULL,
           project_id = NULL,
           familiar_version = NULL
         )
)


####dataObject#####

#' Data object
#'
#' The dataObject class is used to resolve the issue of keeping track of
#' pre-processing status and data loading inside complex workflows, e.g. nested
#' predict functions inside a calibration function.
#'
#' @slot data NULL or data table containing the data. This is the data which
#'   will be read and used.
#' @slot preprocessing_level character indicating the level of pre-processing
#'   already conducted.
#' @slot outcome_type character, determines the outcome type.
#' @slot data_column_info Object containing column information.
#' @slot delay_loading logical. Allows delayed loading data, which enables data
#'   parsing downstream without additional workflow complexity or memory
#'   utilisation.
#' @slot perturb_level numeric. This is the perturbation level for data which
#'   has not been loaded. Used for data retrieval by interacting with the run
#'   table of the accompanying model.
#' @slot load_validation logical. This determines which internal data set will
#'   be loaded. If TRUE, the validation data will be loaded, whereas FALSE loads
#'   the development data.
#' @slot aggregate_on_load logical. Determines whether data is aggregated after
#'   loading.
#' @slot sample_set_on_load NULL or vector of sample identifiers to be loaded.
#'   
setClass("dataObject",
         slots = list(
           # Data
           data = "ANY",
           # Level to which pre-processing has been conducted.
           preprocessing_level = "character",
           # Outcome type
           outcome_type = "character",
           # Outcome info, such as class levels, mean values etc.
           outcome_info = "ANY",
           # Info related to the columns in the dataset.
           data_column_info = "ANY",
           # Flag for delayed loading. This can only be meaningfully set using internal data.
           delay_loading = "logical",
           # Perturbation level for data which has not been loaded. Used for data retrieval in combination with the run table of the accompanying model.
           perturb_level = "numeric",
           # Determines which data should be loaded.
           load_validation = "logical",
           # Flag for aggregation after loading and pre-processing
           aggregate_on_load = "logical",
           # Samples to be loaded
           sample_set_on_load = "ANY"
         ),
         prototype = list(
           data = NULL,
           preprocessing_level = "none",
           outcome_type = NA_character_,
           outcome_info = NULL,
           delay_loading = FALSE,
           perturb_level = NA_integer_,
           load_validation = TRUE,
           aggregate_on_load = FALSE,
           sample_set_on_load = NULL
         )
)

#### featureInfo ---------------------------------------------------------------

#' Feature information object.
#'
#' A featureInfo object contains information for a single feature. This
#' information is used to check data prospectively for consistency and for data
#' preparation. These objects are, for instance, attached to a familiarModel
#' object so that data can be pre-processed in the same way as the development
#' data.
#'
#' @slot name Name of the feature, which by default is the column name of the
#'   feature.
#' @slot set_descriptor Character string describing the set to which the feature
#'   belongs. Currently not used.
#' @slot feature_type Describes the feature type, i.e. `factor` or `numeric`.
#' @slot levels The class levels of categorical features. This is used to check
#'   prospective datasets.
#' @slot ordered Specifies whether the
#' @slot distribution Five-number summary (numeric) or class frequency
#'   (categorical).
#' @slot data_id Internal identifier for the dataset used to derive the feature
#'   information.
#' @slot run_id Internal identifier for the specific subset of the dataset used
#'   to derive the feature information.
#' @slot in_signature Specifies whether the feature is included in the model
#'   signature.
#' @slot in_novelty Specifies whether the feature is included in the novelty
#'   detector.
#' @slot removed Specifies whether the feature was removed during
#'   pre-processing.
#' @slot removed_unknown_type Specifies whether the feature was removed during
#'   pre-processing because the type was neither factor nor numeric..
#' @slot removed_missing_values Specifies whether the feature was removed during
#'   pre-processing because it contained too many missing values.
#' @slot removed_no_variance Specifies whether the feature was removed during
#'   pre-processing because it did not contain more than 1 unique value.
#' @slot removed_low_variance Specifies whether the feature was removed during
#'   pre-processing because the variance was too low. Requires applying
#'   `low_variance` as a `filter_method`.
#' @slot removed_low_robustness Specifies whether the feature was removed during
#'   pre-processing because it lacks robustness. Requires applying
#'   `robustness` as a `filter_method`, as well as repeated measurement.
#' @slot removed_low_importance Specifies whether the feature was removed during
#'   pre-processing because it lacks relevance. Requires applying
#'   `univariate_test` as a `filter_method`.
#' @slot fraction_missing Specifies the fraction of missing values.
#' @slot robustness Specifies robustness of the feature, if measured.
#' @slot univariate_importance Specifies the univariate p-value of the feature, if measured.
#' @slot transformation_parameters Details parameters for power transformation of numeric features.
#' @slot normalisation_parameters Details parameters for (global) normalisation of numeric features.
#' @slot batch_normalisation_parameters Details parameters for batch normalisation of numeric features.
#' @slot imputation_parameters Details parameters or models for imputation of missing values.
#' @slot cluster_parameters Details parameters for forming clusters with other features.
#' @slot required_features Details features required for clustering or imputation.
#' @slot familiar_version Version of the familiar package.
#'
#' @export

setClass("featureInfo",
         slots = list(
           name = "character",
           set_descriptor = "character",
           feature_type = "character",
           levels = "ANY",
           ordered = "logical",
           distribution = "ANY",
           data_id = "integer",
           run_id = "integer",
           in_signature = "logical",
           in_novelty = "logical",
           removed = "logical",
           removed_unknown_type = "logical",
           removed_missing_values = "logical",
           removed_no_variance = "logical",
           removed_low_variance = "logical",
           removed_low_robustness = "logical",
           removed_low_importance = "logical",
           fraction_missing = "numeric",
           robustness = "ANY",
           univariate_importance = "ANY",
           transformation_parameters = "ANY",
           normalisation_parameters = "ANY",
           batch_normalisation_parameters = "ANY",
           imputation_parameters = "ANY",
           cluster_parameters = "ANY",
           required_features = "ANY",
           familiar_version = "ANY"
         ),
         prototype = list(
           name = NA_character_,
           set_descriptor = NA_character_,
           feature_type = NA_character_,
           levels = NULL,
           ordered = FALSE,
           distribution = NULL,
           data_id = NA_integer_,
           run_id = NA_integer_,
           in_signature = FALSE,
           in_novelty = FALSE,
           removed = FALSE,
           removed_unknown_type = FALSE,
           removed_missing_values = FALSE,
           removed_no_variance = FALSE,
           removed_low_variance = FALSE,
           removed_low_robustness = FALSE,
           removed_low_importance = FALSE,
           fraction_missing = NA_real_,
           robustness = NULL,
           univariate_importance = NULL,
           transformation_parameters = NULL,
           normalisation_parameters = NULL,
           batch_normalisation_parameters = NULL,
           imputation_parameters = NULL,
           cluster_parameters = NULL,
           required_features = NULL,
           familiar_version = NULL
         )
)


#### featureInfoParameters -----------------------------------------------------

#' Feature information parameters object.
#'
#' A featureInfo object contains information for a single feature. Some
#' information, for example concerning clustering and transformation contains
#' various parameters that allow for applying the data transformation correctly.
#' These are stored in featureInfoParameters objects.
#'
#' @slot name Name of the feature, which by default is the column name of the
#'   feature. Typically used to correctly assign the data.
#' @slot complete Flags whether the parameters have been completely set.
#' @slot familiar_version Version of the familiar package.
#'
#' @details featureInfoParameters is normally a parent class for specific
#'   classes, such as featureInfoParametersTransformation.
#'
#' @export
setClass("featureInfoParameters",
         slots = list(
           name = "character",
           complete = "logical",
           familiar_version = "ANY"
         ),
         prototype = list(
           name = NA_character_,
           complete = FALSE,
           familiar_version = NULL
         )
)



#### vimpTable -----------------------------------------------------------------

#' Variable importance table
#'
#' A vimpTable object contains information concerning variable importance of one
#' or more features. These objects are created during feature selection.
#'
#' @slot vimp_table Table containing features with corresponding scores.
#' @slot vimp_method Method used to compute variable importance scores for each
#'   feature.
#' @slot run_table Run table for the data used to compute variable importances
#'   from. Used internally.
#' @slot score_aggregation Method used to aggregate the score of contrasts for
#'   each categorical feature, if any,
#' @slot encoding_table Table used to relate categorical features to their
#'   contrasts, if any. Not used for all variable importance methods.
#' @slot cluster_table Table used to relate original features with features
#'   after clustering. Variable importance is determined after feature
#'   processing, which includes clustering.
#' @slot invert Determines whether increasing score corresponds to increasing
#'   (`FALSE`) or decreasing rank (`TRUE`). Used internally to determine how
#'   ranks should be formed.
#' @slot project_id Identifier of the project that generated the vimpTable
#'   object.
#' @slot familiar_version Version of the familiar package used to create this
#'   table.
#' @slot state State of the variable importance table. The object can have the
#'   following states:
#'
#'   * `initial`: initial state, directly after the variable importance table is
#'   filled.
#'
#'   * `decoded`: depending on the variable importance method, the initial
#'   variable importance table may contain the scores of individual contrasts
#'   for categorical variables. When decoded, data in the `encoding_table`
#'   attribute has been used to aggregate scores from all contrasts into a
#'   single score for each feature.
#'
#'   * `declustered`: variable importance is determined from fully processed
#'   features, which includes clustering. This means that a single feature in
#'   the variable importance table may represent multiple original features.
#'   When a variable importance table has been declustered, all clusters have
#'   been turned into their constituent features.
#'
#'   * `reclustered`: When the table is reclustered, features are replaced by
#'   their respective clusters. This is actually used when updating the cluster
#'   table to ensure it fits to a local context. This prevents issues when
#'   attempting to aggregate or apply variable importance tables in data with
#'   different feature preprocessing, and as a result, different clusters.
#'
#'   * `ranked`: The scores have been used to create ranks, with lower ranks
#'   indicating better features.
#'
#'   * `aggregated`: Score and ranks from multiple variable importance tables
#'   were aggregated.
#'
#' @details vimpTable objects exists in various states. These states are
#'   generally incremental, i.e. one cannot turn a declustered table into the
#'   initial version. Some methods such as aggregation internally do some state
#'   reshuffling.
#'
#'   This object replaces the ad-hoc lists with information that were used in
#'   versions prior to familiar 1.2.0.
#' @seealso get_vimp_table
#' @export

setClass("vimpTable",
         slots = list(
           # Variable importance table.
           vimp_table = "ANY",
           # Variable importance method that generated the current variable
           # importance table.
           vimp_method = "character",
           # Run table for the current model
           run_table = "ANY",
           # Set how scores from encoded features should be aggregated.
           score_aggregation = "character",
           # Table that can be used to merge encoded features back into
           # singleton features, if necessary.
           encoding_table = "ANY",
           # Table that can be used to decluster the current table.
           cluster_table = "ANY",
           # Whether scores should be inverted for ranking.
           invert = "logical",
           # Project identifier.
           project_id = "ANY",
           # Version of familiar used to create the object.
           familiar_version = "ANY",
           # State of the object.
           state="character"
         ),
         prototype = list(
           vimp_table = NULL,
           vimp_method = NA_character_,
           run_table = NULL,
           score_aggregation = NA_character_,
           encoding_table = NULL,
           cluster_table = NULL,
           invert = FALSE,
           project_id = NULL,
           familiar_version = NULL,
           state="initial"
         )
)



#### outcomeInfo ---------------------------------------------------------------

#' Outcome information object.
#'
#' An outcome information object stores data concerning an outcome. This is used
#' to prospectively check data.
#'
#' @slot name Name of the outcome, inherited from the original column name by
#'   default.
#' @slot outcome_type Type of outcome.
#' @slot outcome_column Name of the outcome column in data.
#' @slot levels Specifies class levels of categorical outcomes.
#' @slot ordered Specifies whether categorical outcomes are ordered.
#' @slot reference Class level used as reference.
#' @slot time Maximum time, as set by the `time_max` configuration parameter.
#' @slot censored Censoring indicators for survival outcomes.
#' @slot event Event indicators for survival outcomes.
#' @slot competing_risk Indicators for competing risks in survival outcomes.
#' @slot distribution Five-number summary (numeric outcomes), class frequency
#'   (categorical outcomes), or survival distributions.
#' @slot data_id Internal identifier for the dataset used to derive the outcome
#'   information.
#' @slot run_id Internal identifier for the specific subset of the dataset used
#'   to derive the outcome information.
#' @slot transformation_parameters Parameters used for transforming a numeric
#'   outcomes. Currently unused.
#' @slot normalisation_parameters Parameters used for normalising numeric
#'   outcomes. Currently unused.
#'
#' @export

setClass("outcomeInfo",
         slots = list(
           # Name of the outcome
           name = "character",
           # Outcome type
           outcome_type = "character",
           # Outcome column
           outcome_column = "ANY",
           # Class levels of categorical outcomes.
           levels = "ANY",
           # Flag for ordinal categorical outcomes.
           ordered = "logical",
           # Reference class of categorical outcomes.
           reference = "ANY",
           # Max time for the outcome.
           time = "numeric",
           # Censor indicator for survival outcomes, e.g. alive.
           censored = "character",
           # Event indicator for survival outcomes, e.g. recurrent disease.
           event = "character",
           # Competing risk indicator(s) for survival outcomes, e.g. dead.
           competing_risk = "character",
           # Distribution information of outcome variables.
           distribution = "ANY",
           # Data id to which this outcome data belongs.
           data_id = "integer",
           # Run id to which this outcome data belongs.
           run_id = "integer",
           # Transformation parameters for the outcome data.
           transformation_parameters = "ANY",
           # Normalisation parameters for the outcome data.
           normalisation_parameters = "ANY"
         ),
         prototype = list(
           name = NA_character_,
           outcome_type = NA_character_,
           outcome_column = NULL,
           levels = NULL,
           ordered = FALSE,
           reference = NA_character_,
           time = Inf,
           censored = NA_character_,
           event = NA_character_,
           competing_risk = NA_character_,
           distribution = NULL,
           data_id = NA_integer_,
           run_id = NA_integer_,
           transformation_parameters = NULL,
           normalisation_parameters = NULL
         )
)


#### familiarVimpMethod --------------------------------------------------------

#' Variable importance method object.
#'
#' The familiarVimpMethod class is the parent class for all variable importance
#' methods in familiar.
#'
#' @slot outcome_type Outcome type of the data to be evaluated using the object.
#' @slot hyperparameters Set of hyperparameters for the variable importance
#'   method.
#' @slot vimp_method The character string indicating the variable importance
#'   method.
#' @slot multivariate Flags whether the variable importance method is
#'   multivariate vs. univariate.
#' @slot outcome_info Outcome information object, which contains additional
#'   information concerning the outcome, such as class levels.
#' @slot feature_info List of objects containing feature information, e.g.,
#'   name, class levels, transformation, normalisation and clustering
#'   parameters.
#' @slot required_features The set of features to be assessed by the variable
#'   importance method.
#' @slot package Name of the package(s) required to execute the variable
#'   importance method.
#' @slot run_table Run table for the data to be assessed by the variable
#'   importance method. Used internally.
#' @slot project_id Identifier of the project that generated the
#'   familiarVimpMethod object.
#'
#' @export
setClass("familiarVimpMethod",
         slots = list(
           # Outcome type
           outcome_type = "character",
           # Hyper-parameters (typically stored in the model as well)
           hyperparameters = "ANY",
           # Name of variable importance method
           vimp_method = "character",
           # Indicates whether the method is a univariate or multivariate
           # method.
           multivariate = "logical",
           # Outcome info, such as class levels, mean values etc.
           outcome_info = "ANY",
           # Data required for feature pre-processing
           feature_info = "ANY",
           # Required features for complete reconstruction, including imputation
           required_features = "ANY",
           # Name of the package required to perform variable importance.
           package = "ANY",
           # Run table for the current vimp method
           run_table = "ANY",
           # Project identifier for consistency tracking
           project_id = "ANY"
         ),
         prototype = list(
           outcome_type = NA_character_,
           hyperparameters = NULL,
           vimp_method = NA_character_,
           multivariate = FALSE,
           outcome_info = NULL,
           feature_info = NULL,
           required_features = NULL,
           package = NULL,
           run_table = NULL,
           project_id = NULL
         )
)



#### familiarNoveltyDetector ---------------------------------------------------

#' Novelty detector.
#'
#' A familiarNoveltyDetector object is a self-contained model that can be
#' applied to generate out-of-distribution predictions for instances in a
#' dataset.
#'
#' @slot name Name of the familiarNoveltyDetector object.
#' @slot learner Learning algorithm used to create the novelty detector.
#' @slot model The actual novelty detector trained using a specific algorithm,
#'   e.g. a isolation forest from the `isotree` package.
#' @slot feature_info List of objects containing feature information, e.g.,
#'   name, class levels, transformation, normalisation and clustering
#'   parameters.
#' @slot data_column_info Data information object containing information
#'   regarding identifier column names.
#' @slot conversion_parameters Parameters used to convert raw output to
#'   statistical probability of being out-of-distribution. Currently unused.
#' @slot hyperparameters Set of hyperparameters used to train the detector.
#' @slot required_features The set of features required for complete
#'   reproduction, i.e. with imputation.
#' @slot model_features The set of features that is used to train the detector.
#' @slot run_table Run table for the data used to train the detector. Used
#'   internally.
#' @slot is_trimmed Flag that indicates whether the detector, stored in the
#'   `model` slot, has been trimmed.
#' @slot trimmed_function List of functions whose output has been captured prior
#'   to trimming the model.
#' @slot project_id Identifier of the project that generated the
#'   familiarNoveltyDetector object.
#' @slot familiar_version Version of the familiar package.
#' @slot package Name of package(s) required to executed the detector itself,
#'   e.g. `isotree`.
#' @slot package_version Version of the packages mentioned in the `package`
#'   attribute.
#'
#' Note that these objects do not contain any data concerning outcome, as this
#' not relevant for (prospective) out-of-distribution detection.
#'
#' @export

setClass("familiarNoveltyDetector",
         slots = list(
           # Model name.
           name = "character",
           # Detector
           learner = "character",
           # Model container
           model = "ANY",
           # Info related to the columns in the dataset.
           data_column_info = "ANY",
           # Parameters needed to convert raw novelty scores into p-values.
           conversion_parameters = "ANY",
           # Hyperparameters used to create the novelty detector.
           hyperparameters = "ANY",
           # Data required for feature pre-processing
           feature_info = "ANY",
           # Required features for complete reconstruction, including
           # imputation.
           required_features = "ANY",
           # Features that are required for novelty detection.
           model_features = "ANY",
           # Run table for the current model
           run_table = "ANY",
           # Flags trimming of the novelty detector.
           is_trimmed = "logical",
           # Restores functions lost due to model trimming, such as coef or
           # vcov.
           trimmed_function = "list",
           # Project identifier for consistency tracking
           project_id = "ANY",
           # Package version for backward compatibility.
           familiar_version = "ANY",
           # Name of the package required to train the learner.
           package = "ANY",
           # Version of the learner for reproducibility.
           package_version = "ANY"
         ),
         prototype = list(
           name = character(0),
           learner = NA_character_,
           model = NULL,
           data_column_info = NULL,
           conversion_parameters = NULL,
           hyperparameters = NULL,
           feature_info = NULL,
           required_features = NULL,
           model_features = NULL,
           run_table = NULL,
           is_trimmed = FALSE,
           trimmed_function = list(),
           project_id = NULL,
           familiar_version = NULL,
           package = NULL,
           package_version = NULL
         )
)



#### familiarHyperparameterLearner ---------------------------------------------

#' Hyperparameter learner.
#'
#' A familiarHyperparameterLearner object is a self-contained model that can be
#' applied to predict optimisation scores for a set of hyperparameters.
#'
#' @slot name Name of the familiarHyperparameterLearner object.
#' @slot learner Algorithm used to create the hyperparameter learner.
#' @slot target_learner Algorithm for which the hyperparameters are being
#'   learned.
#' @slot target_outcome_type Outcome type of the learner for which
#'   hyperparameters are being modeled. Used to determine the target
#'   hyperparameters.
#' @slot optimisation_metric One or metrics used to generate the optimisation
#'   score.
#' @slot optimisation_function Function used to generate the optimisation score.
#' @slot model The actual model trained using the specific algorithm, e.g. a
#'   isolation forest from the `isotree` package.
#' @slot target_hyperparameters The names of the hyperparameters that are used
#'   to train the hyperparameter learner.
#' @slot project_id Identifier of the project that generated the
#'   familiarHyperparameterLearner object.
#' @slot familiar_version Version of the familiar package.
#' @slot package Name of package(s) required to executed the hyperparameter
#'   learner itself, e.g. `laGP`.
#' @slot package_version Version of the packages mentioned in the `package`
#'   attribute.
#'
#' @details Hyperparameter learners are used to infer the optimisation score for
#'   sets of hyperparameters. These are then used to either infer utility using
#'   acquisition functions or to generate summary scores to identify the optimal
#'   model.
#'
#' @export

setClass("familiarHyperparameterLearner",
         slots = list(
           # Model name.
           name = "character",
           # Hyperparameter learner
           learner = "character",
           # Learner for which the hyperparameters are being learned.
           target_learner = "character",
           # Outcome type for the above learner.
           target_outcome_type = "character",
           # Metric(s) used to generate the input data for optimisation score
           # that is being learned.
           optimisation_metric = "character",
           # Function used to generate the optimisation score.
           optimisation_function = "character",
           # Model container
           model = "ANY",
           # Names of the hyperparameters that are being learned.
           target_hyperparameters = "ANY",
           # Project identifier for consistency tracking
           project_id = "ANY",
           # Package version for backward compatibility.
           familiar_version = "ANY",
           # Name of the package required to train the learner.
           package = "ANY",
           # Version of the learner for reproducibility.
           package_version = "ANY"
         ),
         prototype = list(
           name = character(0),
           learner = NA_character_,
           target_learner = NA_character_,
           target_outcome_type = NA_character_,
           optimisation_metric = NA_character_,
           optimisation_function = NA_character_,
           model = NULL,
           target_hyperparameters = NULL,
           project_id = NULL,
           familiar_version = NULL,
           package = NULL,
           package_version = NULL
         )
)



#### familiarMetric ------------------------------------------------------------

#' Model performance metric.
#'
#' Superclass for model performance objects.
#'
#' @slot metric Performance metric.
#' @slot outcome_type Type of outcome being predicted.
#' @slot name Name of the performance metric.
#' @slot value_range Range of the performance metric. Can be half-open.
#' @slot baseline_value Value of the metric for trivial models, e.g. models that
#'   always predict the median value, the majority class, or the mean hazard,
#'   etc.
#' @slot higher_better States whether higher metric values correspond to better
#'   predictive model performance (e.g. accuracy) or not (e.g. root mean squared
#'   error).
#'
#' @export

setClass("familiarMetric",
         slots = list(
           # The metric itself.
           metric = "character",
           # The outcome type associated with the metric.
           outcome_type = "character",
           # The name of the metric, e.g. for plotting.
           name = "character",
           # The potential value range of the metric.
           value_range = "numeric",
           # The baseline value of the metric, e.g. to derive an objective
           # function from.
           baseline_value = "ANY",
           # Flag that sets whether higher values denote better performance.
           higher_better = "logical"
         ),
         prototype = list(
           metric = NA_character_,
           outcome_type = NA_character_,
           name = NA_character_,
           value_range = c(NA_real_, NA_real_),
           baseline_value = NULL,
           higher_better = TRUE)
)



#### familiarDataElement -------------------------------------------------------

#'  Data container for evaluation data.
#'
#'  Most attributes of the familiarData object are objects of the
#'  familiarDataElement class. This (super-)class is used to allow for
#'  standardised aggregation and processing of evaluation data.
#'
#'@slot data Evaluation data, typically a data.table or list.
#'@slot identifiers Identifiers of the data, e.g. the generating model name,
#'  learner, etc.
#'@slot detail_level Sets the level at which results are computed and
#'  aggregated.
#'
#'  * `ensemble`: Results are computed at the ensemble level, i.e. over all
#'  models in the ensemble. This means that, for example, bias-corrected
#'  estimates of model performance are assessed by creating (at least) 20
#'  bootstraps and computing the model performance of the ensemble model for
#'  each bootstrap.
#'
#'  * `hybrid` (default): Results are computed at the level of models in an
#'  ensemble. This means that, for example, bias-corrected estimates of model
#'  performance are directly computed using the models in the ensemble. If there
#'  are at least 20 trained models in the ensemble, performance is computed for
#'  each model, in contrast to `ensemble` where performance is computed for the
#'  ensemble of models. If there are less than 20 trained models in the
#'  ensemble, bootstraps are created so that at least 20 point estimates can be
#'  made.
#'
#'  * `model`: Results are computed at the model level. This means that, for
#'  example, bias-corrected estimates of model performance are assessed by
#'  creating (at least) 20 bootstraps and computing the performance of the model
#'  for each bootstrap.
#'
#'  Note that each level of detail has a different interpretation for bootstrap
#'  confidence intervals. For `ensemble` and `model` these are the confidence
#'  intervals for the ensemble and an individual model, respectively. That is,
#'  the confidence interval describes the range where an estimate produced by a
#'  respective ensemble or model trained on a repeat of the experiment may be
#'  found with the probability of the confidence level. For `hybrid`, it
#'  represents the range where any single model trained on a repeat of the
#'  experiment may be found with the probability of the confidence level. By
#'  definition, confidence intervals obtained using `hybrid` are at least as
#'  wide as those for `ensemble`. `hybrid` offers the correct interpretation if
#'  the goal of the analysis is to assess the result of a single, unspecified,
#'  model.
#'
#'  Some child classes do not use this parameter.
#'@slot estimation_type Sets the type of estimation that should be possible.
#'  This has the following options:
#'
#'  * `point`: Point estimates.
#'
#'  * `bias_correction` or `bc`: Bias-corrected estimates. A bias-corrected
#'  estimate is computed from (at least) 20 point estimates, and `familiar` may
#'  bootstrap the data to create them.
#'
#'  * `bootstrap_confidence_interval` or `bci` (default): Bias-corrected
#'  estimates with bootstrap confidence intervals (Efron and Hastie, 2016). The
#'  number of point estimates required depends on the `confidence_level`
#'  parameter, and `familiar` may bootstrap the data to create them.
#'
#'  Some child classes do not use this parameter.
#'@slot confidence_level (*optional*) Numeric value for the level at which
#'  confidence intervals are determined. In the case bootstraps are used to
#'  determine the confidence intervals bootstrap estimation, `familiar` uses the
#'  rule of thumb \eqn{n = 20 / ci.level} to determine the number of required
#'  bootstraps.
#'@slot bootstrap_ci_method Method used to determine bootstrap confidence
#'  intervals (Efron and Hastie, 2016). The following methods are implemented:
#'
#'  * `percentile` (default): Confidence intervals obtained using the percentile
#'  method.
#'
#'  * `bc`: Bias-corrected confidence intervals.
#'
#'  Note that the standard method is not implemented because this method is
#'  often not suitable due to non-normal distributions. The bias-corrected and
#'  accelerated (BCa) method is not implemented yet.
#'@slot value_column Identifies column(s) in the `data` attribute presenting
#'  values.
#'@slot grouping_column Identifies column(s) in the `data` attribute presenting
#'  identifier columns for grouping during aggregation. Familiar will
#'  automatically assign items from the `identifiers` attribute to the data and
#'  this attribute when combining multiple familiarDataElements of the same
#'  (child) class.
#'@slot is_aggregated Defines whether the object was aggregated.
#'
#'@references 1. Efron, B. & Hastie, T. Computer Age Statistical Inference.
#'  (Cambridge University Press, 2016).
#'
#'@export

setClass("familiarDataElement",
         slots = list(
           # The primary results.
           data = "ANY",
           # Identifiers of the data, e.g. the generating model name, the
           # feature-selection method and learner.
           identifiers = "ANY",
           # The level of detail at which the data was computed.
           detail_level = "character",
           # The kind of estimation for which the data was computed, e.g.
           # bias-corrected estimates.
           estimation_type = "character",
           # The confidence level for which data was computed. Only set if the
           # correct estimation type is set.
           confidence_level = "ANY",
           # The method used to compute the bootstrap confidence intervals from
           # the data.
           bootstrap_ci_method = "character",
           # The column that contains the relevant data. Useful for merging and
           # identifying bootstraps.
           value_column = "character",
           # The column(s) required for grouping the data. Useful for determining confidence intervals.
           grouping_column = "ANY",
           # Flag that signals whether the data is aggregated, e.g. by computing
           # confidence intervals and a bias-corrected value.
           is_aggregated = "logical"
         ),
         prototype = list(
           data = NULL,
           identifiers = NULL,
           detail_level = NA_character_,
           estimation_type = NA_character_,
           confidence_level = NULL,
           bootstrap_ci_method = NA_character_,
           value_column = NA_character_,
           grouping_column = NULL,
           is_aggregated = FALSE)
)

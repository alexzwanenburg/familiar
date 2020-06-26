####familiarModel####
setClass("familiarModel",
         slots = list(
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
           # Features included in the model
           signature = "ANY",
           # Required features for complete reconstruction, including imputation
           req_feature_cols = "ANY",
           # Features that are required for reconstruction, without imputation (i.e. features that are in the signature directly or as part of a cluster)
           important_features = "ANY",
           # Run table for the current model
           run_table = "ANY",
           # Information required to assess model calibrations (e.g. baseline survival)
           calibration_info = "ANY",
           # Information required to do perform a Kaplan-Meier analysis using the model
           km_info = "ANY",
           # Evaluation settings. This allows default values for external use of existing models.
           settings = "ANY",
           # Flags anonymisation of the model
           is_anonymised = "logical",
           # Project identifier for consistency tracking
           project_id = "ANY",
           # Package version for backward compatibility
           familiar_version = "ANY"
         ),
         prototype = list(
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
           signature = NULL,
           req_feature_cols = NULL,
           important_features = NULL,
           calibration_info = NULL,
           km_info = NULL,
           run_table = NULL,
           settings = NULL,
           is_anonymised = FALSE,
           project_id = NULL,
           familiar_version = NULL
         )
)

#####familiarEnsemble#####
setClass("familiarEnsemble",
         slots = list(
           # Model container
           model_list = "ANY",
           # Model outcome type
           outcome_type = "character",
           # Outcome info, such as class levels, mean values etc.
           outcome_info = "ANY",
           # Info related to the columns in the dataset.
           data_column_info = "ANY",
           # Name of learner
           learner = "character",
           # Name of feature selection method
           fs_method = "character",
           # Data required for feature pre-processing
           feature_info = "ANY",
           # Required features for complete reconstruction, including imputation
           req_feature_cols = "ANY",
           # Features that are required for reconstruction, without imputation (i.e. features that are in the signature directly or as part of a cluster)
           important_features = "ANY",
           # Set of run tables for the current ensemble. This is only required for processing internal data.
           run_table = "ANY",
           # Information required to assess model calibrations (e.g. baseline survival)
           calibration_info = "ANY",
           # Evaluation settings. This allows default values for external use of existing models.
           settings = "ANY",
           # Flags anonymisation of the model
           is_anonymised = "logical",
           # Project identifier for consistency tracking
           project_id = "ANY",
           # Package version for backward compatibility
           familiar_version = "ANY"
         ),
         prototype = list(
           model_list = NULL,
           outcome_type = NA_character_,
           outcome_info = NULL,
           data_column_info = NULL,
           learner = NA_character_,
           fs_method = NA_character_,
           feature_info = NULL,
           req_feature_cols = NULL,
           important_features = NULL,
           run_table = NULL,
           calibration_info = NULL,
           settings = NULL,
           is_anonymised = FALSE,
           project_id = NULL,
           familiar_version = NULL
         )
)

#####familiarData#####
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
           req_feature_cols = "ANY",
           # Features that are required for reconstruction, without imputation
           # (i.e. features that are in the signature directly or as part of a
           # cluster)
           important_features = "ANY",
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
           mutual_correlation = "ANY",
           # Information on individual conditional expectation
           ice_data = "ANY",
           # Flag to signal whether the data concerns validation data (TRUE) or
           # development data (FALSE)
           is_validation = "logical",
           # Flag to signal whether the data is anonymised
           is_anonymised = "logical",
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
           req_feature_cols = NULL,
           important_features = NULL,
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
           mutual_correlation = NULL,
           ice_data = NULL,
           is_validation = FALSE,
           is_anonymised = FALSE,
           generating_ensemble = character(0),
           project_id = NULL,
           familiar_version = NULL
         )
)

#####familiarCollection#####
#' Collection of familiar data.
#'
#' A familiarCollection object aggregates data from one or more familiarData objects.
#' @slot collection_name character. 
#' @slot data_sets character. 
#' @slot outcome_type character. 
#' @slot outcome_info ANY. 
#' @slot fs_vimp ANY. 
#' @slot model_vimp ANY.
#' @slot permutation_vimp ANY.
#' @slot hyperparameters ANY.
#' @slot hyperparameter_data ANY.
#' @slot req_feature_cols ANY.
#' @slot important_features ANY. 
#' @slot learner character. 
#' @slot fs_method character. 
#' @slot prediction_data ANY.
#' @slot confusion_matrix ANY.
#' @slot decision_curve_data ANY.
#' @slot calibration_info ANY. 
#' @slot calibration_data ANY. 
#' @slot model_performance ANY. 
#' @slot km_info ANY. 
#' @slot km_data ANY. 
#' @slot auc_data ANY. 
#' @slot univariate_analysis ANY. 
#' @slot feature_expressions ANY. 
#' @slot mutual_correlation ANY. 
#' @slot data_set_labels ANY.
#' @slot ice_data ANY,
#' @slot learner_labels ANY. 
#' @slot fs_method_labels ANY. 
#' @slot feature_labels ANY. 
#' @slot km_group_labels ANY.
#' @slot class_labels ANY. 
#' @slot is_anonymised logical. 
#' @slot project_id ANY. 
#' @slot familiar_version ANY. 
#'
#' @export

setClass("familiarCollection",
         slots = list(
           # Name of the collection
           collection_name = "character",
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
           req_feature_cols = "ANY",
           # Important features, e.g. features that ended up in a signature
           # individually or as part of a cluster
           important_features = "ANY",
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
           mutual_correlation = "ANY",
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
           # Flag to signal whether the data is anonymised
           is_anonymised = "logical",
           # Project identifier
           project_id = "ANY",
           # Package version for backward compatibility
           familiar_version = "ANY"
         ),
         prototype = list(
           collection_name = NA_character_,
           data_sets = character(0),
           outcome_type = NA_character_,
           outcome_info = NULL,
           fs_vimp = NULL,
           model_vimp = NULL,
           permutation_vimp = NULL,
           hyperparameters = NULL,
           hyperparameter_data = NULL,
           req_feature_cols = NULL,
           important_features = NULL,
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
           mutual_correlation = NULL,
           ice_data = NULL,
           data_set_labels = NULL,
           learner_labels = NULL,
           fs_method_labels = NULL,
           feature_labels = NULL,
           km_group_labels = NULL,
           class_labels = NULL,
           is_anonymised = FALSE,
           project_id = NULL,
           familiar_version = NULL
         )
)


#####dataObject#####
#' Data object
#'
#' The dataObject class is used to resolve the issue of keeping track of pre-processing status and data loading inside complex workflows, e.g.
#' nested predict functions inside a calibration function.
#' @slot data NULL or data table containing the data. This is the data which will be read and used.
#' @slot is_pre_processed logical, signifies whether the data has been pre-processed.
#' @slot outcome_type character, determines the outcome type.
#' @slot delay_loading logical. Allows delayed loading data, which enables data parsing downstream without additional workflow complexity or memory utilisation.
#' @slot perturb_level numeric. This is the perturbation level for data which has not been loaded. Used for data retrieval by interacting with the run table of the accompanying model.
#' @slot load_validation logical. This determines which internal data set will be loaded. If TRUE, the validation data will be loaded, whereas FALSE loads the development data.
#' @slot aggregate_on_load logical. Determines whether data is aggregated after loading.
#' @slot sample_set_on_load NULL or vector of sample identifiers to be loaded.
#'
setClass("dataObject",
         slots = list(
           # Data
           data = "ANY",
           # Whether the data has been pre-processed
           is_pre_processed = "logical",
           # Outcome type
           outcome_type = "character",
           # Outcome info, such as class levels, mean values etc.
           outcome_info = "ANY",
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
           is_pre_processed = FALSE,
           outcome_type = NA_character_,
           outcome_info = NULL,
           delay_loading = FALSE,
           perturb_level = NA_integer_,
           load_validation = TRUE,
           aggregate_on_load = FALSE,
           sample_set_on_load = NULL
         )
)

#####featureInfo#####
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
           expression_parameters = "ANY",
           imputation_parameters = "ANY",
           cluster_parameters = "ANY",
           required_features = "ANY"
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
           expression_parameters = NULL,
           imputation_parameters = NULL,
           cluster_parameters = NULL,
           required_features = NULL
         )
)

#####outcomeInfo#####
setClass("outcomeInfo",
         slots = list(
           # Name of the outcome
           name = "character",
           # Outcome type
           outcome_type = "character",
           # Outcome column
           outcome_column = "character",
           # Class levels of categorical outcomes.
           levels = "ANY",
           # Flag for ordinal categorical outcomes.
           ordered = "logical",
           # Reference class of categorical outcomes.
           reference = "ANY",
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
           outcome_column = NA_character_,
           levels = NULL,
           ordered = FALSE,
           reference = NA_character_,
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


####familiarModel####
setClass("familiarVimpMethod",
         slots = list(
           # Outcome type
           outcome_type = "character",
           # Hyper-parameters (typically stored in the model as well)
           hyperparameters = "ANY",
           # Name of variable importance method
           vimp_method = "character",
           # Outcome info, such as class levels, mean values etc.
           outcome_info = "ANY",
           # Data required for feature pre-processing
           feature_info = "ANY",
           # Required features for complete reconstruction, including imputation
           req_feature_cols = "ANY",
           # Run table for the current vimp method
           run_table = "ANY"
         ),
         prototype = list(
           outcome_type = NA_character_,
           hyperparameters = NULL,
           vimp_method = NA_character_,
           outcome_info = NULL,
           feature_info = NULL,
           req_feature_cols = NULL,
           run_table = NULL
         )
)

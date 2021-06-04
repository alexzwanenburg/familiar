#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


.get_available_data_elements <- function(check_has_estimation_type=FALSE,
                                         check_has_detail_level=FALSE, 
                                         check_has_sample_limit=FALSE){
  
  # All data elements.
  all_data_elements <- c("auc_data", "calibration_data", "calibration_info", "confusion_matrix",
                         "decision_curve_analyis", "feature_expressions",
                         "fs_vimp", "hyperparameters", "model_performance",
                         "model_vimp", "permutation_vimp", "prediction_data",
                         "risk_stratification_data", "risk_stratification_info",
                         "univariate_analysis", "feature_similarity", "sample_similarity")
  
  # Data elements that allow setting an estimation type.
  can_set_estimation_type <- c("auc_data", "calibration_data", "decision_curve_analyis",
                               "model_performance", "permutation_vimp",  "prediction_data")
  
  # Data elements that allow setting a detail level.
  can_set_detail_level <- c(can_set_estimation_type, "calibration_info", "confusion_matrix", "risk_stratification_data", "risk_stratification_info")
  
  # Data elements that allow for setting an estimation type but not detail
  # level.
  can_set_estimation_type <- c(can_set_estimation_type, "feature_similarity")
  
  # Data elements that allow for setting a sample limit.
  can_set_sample_limit <- c("sample_similarity")
  
  if(check_has_sample_limit){
    all_data_elements <- intersect(all_data_elements, can_set_sample_limit)
  }
  
  if(check_has_estimation_type){
    all_data_elements <- intersect(all_data_elements, can_set_estimation_type)
  } 
  
  if(check_has_detail_level){
    all_data_elements <- intersect(all_data_elements, can_set_detail_level)
  }
  
  return(all_data_elements)
}



.parse_detail_level <- function(x, default, data_element){
  
  if(is.null(x) | is.waive(x)) return(default)
  
  # detail level is stored in a list, by data_element.
  if(is.list(x)) x <- x[[data_element]]
  
  if(is.null(x)) return(default)
  
  .check_parameter_value_is_valid(x=x, var_name="detail_level",
                                  values=c("ensemble", "hybrid", "model"))
  
  return(x)
}



.parse_estimation_type <- function(x, default, data_element, detail_level, has_internal_bootstrap){
  
  # Change to default to point if the detail_level is model.
  if(detail_level == "model") default <- "point"
  
  # In case there is no internal bootstrap, we can only determine point
  # estimates for ensemble and model detail levels (but potentially more for
  # hybrid).
  if(!has_internal_bootstrap & detail_level %in% c("ensemble", "model") & default != "point") default <- "point"
  
  if(is.null(x) | is.waive(x)) return(default)
  
  # detail level is stored in a list, by data_element.
  if(is.list(x)) x <- x[[data_element]]
  
  if(is.null(x)) return(default)
  
  .check_parameter_value_is_valid(x=x, var_name="estimation_type",
                                  values=c("point", "bias_correction", "bc", "bootstrap_confidence_interval", "bci"))
  
  return(x)
}



.parse_aggregate_results <- function(x, default, data_element){
  
  if(is.null(x) | is.waive(x)) return(default)
  
  # detail level is stored in a list, by data_element.
  if(is.list(x)) x <- x[[data_element]]
  
  if(is.null(x)) return(default)
  
  x <- tolower(x)
  .check_parameter_value_is_valid(x=x, var_name="aggregate_results",
                                  values=c("true", "false", "none", "all", "default"))
  
  if(x == "default") return(default)
  if(x %in% c("true", "all")) return(TRUE)
  
  return(FALSE)
}



.parse_sample_limit <- function(x, default, data_element){
  
  if(is.null(x) | is.waive(x)) return(default)
  
  # detail level is stored in a list, by data_element.
  if(is.list(x)) x <- x[[data_element]]
  
  if(is.null(x)) return(default)
  
  if(x == "default") return(default)
  
  .check_number_in_valid_range(x=x, var_name="sample_limit",
                               range=c(20L, Inf))
  
  return(x)
}



#'@title Internal function to create a familiarData object.
#'
#'@description Compute various data related to model performance and calibration
#'  from the provided dataset and `familiarEnsemble` object and store it as a
#'  `familiarData` object.
#'
#'@param object A `familiarEnsemble` object, which is an ensemble of one or more
#'  `familiarModel` objects.
#'@param data A `dataObject` object, `data.table` or `data.frame` that
#'  constitutes the data that are used to compute
#'@param is_pre_processed Flag that indicates whether the data was already
#'  pre-processed externally, e.g. normalised and clustered. Only used if the
#'  `data` argument is a `data.table` or `data.frame`.
#'@param cl Cluster created using the `parallel` package. This cluster is then
#'  used to speed up computation through parallellisation.
#'@param time_max Time point which is used as the benchmark for e.g. cumulative
#'  risks generated by random forest, or the cut-off value for Uno's concordance
#'  index. If not provided explicitly, this parameter is read from settings used
#'  at creation of the underlying `familiarModel` objects. Only used for
#'  `survival` outcomes.
#'@param eval_times One or more time points that are used for in analysis of
#'  survival problems when data has to be assessed at a set time, e.g.
#'  calibration. If not provided explicitly, this parameter is read from
#'  settings used at creation of the underlying `familiarModel` objects. Only
#'  used for `survival` outcomes.
#'@param aggregation_method Method for aggregating variable importances for the
#'  purpose of evaluation. Variable importances are determined during feature
#'  selection steps and after training the model. Both types are evaluated, but
#'  feature selection variable importance is only evaluated at run-time.
#'
#'  See the documentation for the `vimp_aggregation_method` argument in
#'  `summon_familiar` for information concerning the different available
#'  methods.
#'
#'  If not provided explicitly, this parameter is read from settings used at
#'  creation of the underlying `familiarModel` objects.
#'@param rank_threshold The threshold used to  define the subset of highly
#'  important features during evaluation.
#'
#'  See the documentation for the `vimp_aggregation_rank_threshold` argument in
#'  `summon_familiar` for more information.
#'
#'  If not provided explicitly, this parameter is read from settings used at
#'  creation of the underlying `familiarModel` objects.
#'@param ensemble_method Method for ensembling predictions from models for the
#'  same sample. Available methods are:
#'
#'  * `mean`: Use the mean of the predicted values as the ensemble value for a
#'  sample.
#'@param metric One or more metrics for assessing model performance. See the
#'  vignette on performance metrics for the available metrics. If not provided
#'  explicitly, this parameter is read from settings used at creation of the
#'  underlying `familiarModel` objects.
#'@param feature_cluster_method The method used to perform clustering. These are
#'  the same methods as for the `cluster_method` configuration parameter:
#'  `none`, `hclust`, `agnes`, `diana` and `pam`.
#'
#'  `none` cannot be used when extracting data regarding mutual correlation or
#'  feature expressions.
#'
#'  If not provided explicitly, this parameter is read from settings used at
#'  creation of the underlying `familiarModel` objects.
#'
#'@param feature_linkage_method The method used for agglomerative clustering in
#'  `hclust` and `agnes`. These are the same methods as for the
#'  `cluster_linkage_method` configuration parameter: `average`, `single`,
#'  `complete`, `weighted`, and `ward`.
#'
#'  If not provided explicitly, this parameter is read from settings used at
#'  creation of the underlying `familiarModel` objects.
#'
#'@param feature_cluster_cut_method The method used to divide features into
#'  separate clusters. The available methods are the same as for the
#'  `cluster_cut_method` configuration parameter: `silhouette`, `fixed_cut` and
#'  `dynamic_cut`.
#'
#'  `silhouette` is available for all cluster methods, but `fixed_cut` only
#'  applies to methods that create hierarchical trees (`hclust`, `agnes` and
#'  `diana`). `dynamic_cut` requires the `dynamicTreeCut` package and can only
#'  be used with `agnes` and `hclust`.
#'
#'  If not provided explicitly, this parameter is read from settings used at
#'  creation of the underlying `familiarModel` objects.
#'
#'@param feature_similarity_threshold The threshold level for pair-wise
#'  similarity that is required to form feature clusters with the `fixed_cut`
#'  method.
#'
#'  If not provided explicitly, this parameter is read from settings used at
#'  creation of the underlying `familiarModel` objects.
#'
#'@param feature_similarity_metric Metric to determine pairwise similarity
#'  between features. Similarity is computed in the same manner as for
#'  clustering, and `feature_similarity_metric` therefore has the same options
#'  as `cluster_similarity_metric`: `mcfadden_r2`, `cox_snell_r2`,
#'  `nagelkerke_r2`, `spearman`, `kendall` and `pearson`.
#'
#'  If not provided explicitly, this parameter is read from settings used at
#'  creation of the underlying `familiarModel` objects.
#'
#'@param sample_cluster_method The method used to perform clustering based on
#'  distance between samples. These are the same methods as for the
#'  `cluster_method` configuration parameter: `hclust`, `agnes`, `diana` and
#'  `pam`.
#'
#'  `none` cannot be used when extracting data for feature expressions.
#'
#'  If not provided explicitly, this parameter is read from settings used at
#'  creation of the underlying `familiarModel` objects.
#'
#'@param sample_linkage_method The method used for agglomerative clustering in
#'  `hclust` and `agnes`. These are the same methods as for the
#'  `cluster_linkage_method` configuration parameter: `average`, `single`,
#'  `complete`, `weighted`, and `ward`.
#'
#'  If not provided explicitly, this parameter is read from settings used at
#'  creation of the underlying `familiarModel` objects.
#'
#'@param sample_similarity_metric Metric to determine pairwise similarity
#'  between samples. Similarity is computed in the same manner as for
#'  clustering, but `sample_similarity_metric` has different options that are
#'  better suited to computing distance between samples instead of between
#'  features: `gower`, `euclidean`.
#'
#'  The underlying feature data is scaled to the \eqn{[0, 1]} range (for
#'  numerical features) using the feature values across the samples. The
#'  normalisation parameters required can optionally be computed from feature
#'  data with the outer 5% (on both sides) of feature values trimmed or
#'  winsorised. To do so append `_trim` (trimming) or `_winsor` (winsorising) to
#'  the metric name. This reduces the effect of outliers somewhat.
#'
#'  If not provided explicitly, this parameter is read from settings used at
#'  creation of the underlying `familiarModel` objects.
#'
#'@param icc_type String indicating the type of intraclass correlation
#'  coefficient (`1`, `2` or `3`) that should be used to compute robustness for
#'  features in repeated measurements during the evaluation of univariate
#'  importance. These types correspond to the types in Shrout and Fleiss (1979).
#'  If not provided explicitly, this parameter is read from settings used at
#'  creation of the underlying `familiarModel` objects.
#'@param verbose Flag to indicate whether feedback should be provided on the
#'  computation and extraction of various data elements.
#'@param message_indent Number of indentation steps for messages shown during
#'  computation and extraction of various data elements.
#'@param data_element String indicating which data elements are to be extracted.
#'  Default is `all`, but specific elements can be specified to speed up
#'  computations if not all elements are to be computed. This is an internal
#'  parameter that is set by, e.g. the `export_model_vimp` method.
#'@param ... Unused arguments.
#'
#'@inheritParams .parse_evaluation_settings
#'
#'@return A `familiarData` object.
#'@references 1. Shrout, P. E. & Fleiss, J. L. Intraclass correlations: uses in
#'  assessing rater reliability. Psychol. Bull. 86, 420–428 (1979).
#'@md
#'@keywords internal
setGeneric("extract_data", function(object,
                                    data,
                                    data_element=waiver(),
                                    is_pre_processed=FALSE,
                                    cl=NULL,
                                    time_max=waiver(),
                                    aggregation_method=waiver(),
                                    rank_threshold=waiver(),
                                    ensemble_method=waiver(),
                                    stratification_method=waiver(),
                                    eval_times=waiver(),
                                    metric=waiver(),
                                    feature_cluster_method=waiver(),
                                    feature_cluster_cut_method=waiver(),
                                    feature_linkage_method=waiver(),
                                    feature_similarity_metric=waiver(),
                                    feature_similarity_threshold=waiver(),
                                    sample_cluster_method=waiver(),
                                    sample_linkage_method=waiver(),
                                    sample_similarity_metric=waiver(),
                                    sample_limit=waiver(),
                                    detail_level=waiver(),
                                    estimation_type=waiver(),
                                    aggregate_results=waiver(),
                                    confidence_level=waiver(),
                                    bootstrap_ci_method=waiver(),
                                    icc_type=waiver(),
                                    dynamic_model_loading=FALSE,
                                    message_indent=0L,
                                    verbose=FALSE, ...) standardGeneric("extract_data"))

#####extract_data#####
setMethod("extract_data", signature(object="familiarEnsemble"),
          function(object,
                   data,
                   data_element=waiver(),
                   is_pre_processed=FALSE,
                   cl=NULL,
                   time_max=waiver(),
                   aggregation_method=waiver(),
                   rank_threshold=waiver(),
                   ensemble_method=waiver(),
                   stratification_method=waiver(),
                   eval_times=waiver(),
                   metric=waiver(),
                   feature_cluster_method=waiver(),
                   feature_cluster_cut_method=waiver(),
                   feature_linkage_method=waiver(),
                   feature_similarity_metric=waiver(),
                   feature_similarity_threshold=waiver(),
                   sample_cluster_method=waiver(),
                   sample_linkage_method=waiver(),
                   sample_similarity_metric=waiver(),
                   sample_limit=waiver(),
                   detail_level=waiver(),
                   estimation_type=waiver(),
                   aggregate_results=waiver(),
                   confidence_level=waiver(),
                   bootstrap_ci_method=waiver(),
                   icc_type=waiver(),
                   dynamic_model_loading=FALSE,
                   message_indent=0L,
                   verbose=FALSE,
                   ...){
            # Generates a familiarData object from the ensemble.
            
            if(is.waive(data_element)) data_element <- .get_available_data_elements()
            
            # Check the data_element argument.
            if(length(data_element) > 0){
              .check_parameter_value_is_valid(x=data_element, var_name="data_element",
                                              values=.get_available_data_elements())
            }
            
            # Check the dynamic_model_loading argument because it is used here.
            .check_parameter_value_is_valid(x=dynamic_model_loading, var_name="dynamic_model_loading",
                                            values=c(FALSE, TRUE))
            
            # Set auto-detach here. Note that, if TRUE, load_models may reset it
            # to FALSE if models cannot be detached.
            object@auto_detach <- dynamic_model_loading
            
            # Check whether data is a dataObject, and create one otherwise.
            if(!is(data, "dataObject")){
              data <- as_data_object(data=data,
                                     object=object)
              
              # Set pre-processing level.
              data@preprocessing_level=ifelse(is_pre_processed, "clustering", "none")
            }
            
            # Load models, and drop any models that were not trained.
            object <- load_models(object=object, drop_untrained=TRUE)
            
            
            # Extract feature distance tables,
            if(any(c("feature_similarity", "univariate_analysis", "feature_expressions", "permutation_vimp") %in% data_element)){
              # Not for the fs_vimp and model_vimp data elements. This is
              # because these derive cluster information from consensus
              # clustering.

              # Compute a table containg the pairwise distance between features.
              feature_similarity <- extract_feature_similarity(object=object,
                                                               data=data,
                                                               cl=cl,
                                                               estimation_type=estimation_type,
                                                               aggregate_results=aggregate_results,
                                                               confidence_level=confidence_level,
                                                               bootstrap_ci_method=bootstrap_ci_method,
                                                               is_pre_processed=is_pre_processed,
                                                               feature_cluster_method=feature_cluster_method,
                                                               feature_linkage_method=feature_linkage_method,
                                                               feature_cluster_cut_method=feature_cluster_cut_method,
                                                               feature_similarity_threshold=feature_similarity_threshold,
                                                               feature_similarity_metric=feature_similarity_metric,
                                                               verbose=verbose,
                                                               message_indent=message_indent)
            } else {
              feature_similarity <- NULL
            }
            
            if(any(c("sample_similarity", "feature_expressions") %in% data_element)){
              # Compute a table containing the pairwise distance between samples.
              sample_similarity <- extract_sample_similarity(object=object,
                                                             data=data,
                                                             cl=cl,
                                                             is_pre_processed=is_pre_processed,
                                                             sample_limit=sample_limit,
                                                             sample_similarity_metric=sample_similarity_metric,
                                                             sample_cluster_method=sample_cluster_method,
                                                             sample_linkage_method=sample_linkage_method,
                                                             verbose=verbose,
                                                             message_indent=message_indent)
            } else {
              sample_similarity <- NULL
            }

            # Extract feature variable importance
            if(any(c("fs_vimp") %in% data_element)){
              fs_vimp_info <- extract_fs_vimp(object=object,
                                              aggregation_method=aggregation_method,
                                              rank_threshold=rank_threshold,
                                              message_indent=message_indent,
                                              verbose=verbose)
            } else {
              fs_vimp_info <- NULL
            }


            # Extract model-based variable importance
            if(any(c("model_vimp") %in% data_element)){
              model_vimp_info <- extract_model_vimp(object=object,
                                                    data=data,
                                                    aggregation_method=aggregation_method,
                                                    rank_threshold=rank_threshold,
                                                    message_indent=message_indent,
                                                    verbose=verbose)
            } else {
              model_vimp_info <- NULL
            }
            
            
            # Assess permutation variable importance
            if(any(c("permutation_vimp") %in% data_element)){
              permutation_vimp <- extract_permutation_vimp(object=object,
                                                           data=data,
                                                           cl=cl,
                                                           feature_similarity=feature_similarity,
                                                           metric=metric,
                                                           ensemble_method=ensemble_method,
                                                           eval_times=eval_times,
                                                           detail_level=detail_level,
                                                           estimation_type=estimation_type,
                                                           aggregate_results=aggregate_results,
                                                           confidence_level=confidence_level,
                                                           bootstrap_ci_method=bootstrap_ci_method,
                                                           message_indent=message_indent,
                                                           verbose=verbose)
            } else {
              permutation_vimp <- NULL
            }
            
            

            # Expression heatmap data
            if(any(c("feature_expressions") %in% data_element)){
              expression_info <- extract_feature_expression(object=object,
                                                            data=data,
                                                            feature_similarity=feature_similarity,
                                                            sample_similarity=sample_similarity,
                                                            feature_cluster_method=feature_cluster_method,
                                                            feature_linkage_method=feature_linkage_method,
                                                            feature_similarity_metric=feature_similarity_metric,
                                                            sample_cluster_method=sample_cluster_method,
                                                            sample_linkage_method=sample_linkage_method,
                                                            sample_similarity_metric=sample_similarity_metric,
                                                            eval_times=eval_times,
                                                            message_indent=message_indent,
                                                            verbose=verbose)
            } else {
              expression_info <- NULL
            }


            # Univariate analysis
            if(any(c("univariate_analysis") %in% data_element)){
              univar_info <- extract_univariate_analysis(object=object,
                                                         data=data,
                                                         cl=cl,
                                                         icc_type=icc_type,
                                                         feature_similarity=feature_similarity,
                                                         feature_cluster_method=feature_cluster_method,
                                                         feature_cluster_cut_method=feature_cluster_cut_method,
                                                         feature_linkage_method=feature_linkage_method,
                                                         feature_similarity_threshold=feature_similarity_threshold,
                                                         feature_similarity_metric=feature_similarity_metric,
                                                         message_indent=message_indent,
                                                         verbose=verbose)
            } else {
              univar_info <- NULL
            }


            # Extract hyper-parameters
            if(any(c("hyperparameters") %in% data_element)){
              hyperparameter_info <- extract_hyperparameters(object=object,
                                                             message_indent=message_indent,
                                                             verbose=verbose)
            } else {
              hyperparameter_info <- NULL
            }
            
            
            # Create predictions.
            if(any(c("prediction_data") %in% data_element)){
              prediction_data <- extract_predictions(object=object,
                                                     data=data,
                                                     cl=cl,
                                                     ensemble_method=ensemble_method,
                                                     detail_level=detail_level,
                                                     estimation_type=estimation_type,
                                                     aggregate_results=aggregate_results,
                                                     confidence_level=confidence_level,
                                                     eval_times=eval_times,
                                                     message_indent=message_indent,
                                                     verbose=verbose)
            } else {
              prediction_data <- NULL
            }
            
            # Compute model performance based on the prediction_data
            if(any(c("model_performance") %in% data_element)){
              model_performance_data <- extract_performance(object=object,
                                                            data=data,
                                                            cl=cl,
                                                            metric=metric,
                                                            ensemble_method=ensemble_method,
                                                            eval_times=eval_times,
                                                            detail_level=detail_level,
                                                            estimation_type=estimation_type,
                                                            aggregate_results=aggregate_results,
                                                            confidence_level=confidence_level,
                                                            bootstrap_ci_method=bootstrap_ci_method,
                                                            message_indent=message_indent,
                                                            verbose=verbose)
              
            } else {
              model_performance_data <- NULL
            }
            
            # Compute the decision curve analysis data.
            if(any(c("decision_curve_analyis") %in% data_element)){
              decision_curve_data <- extract_decision_curve_data(object=object,
                                                                 data=data,
                                                                 cl=cl,
                                                                 ensemble_method=ensemble_method,
                                                                 eval_times=eval_times,
                                                                 detail_level=detail_level,
                                                                 estimation_type=estimation_type,
                                                                 aggregate_results=aggregate_results,
                                                                 confidence_level=confidence_level,
                                                                 bootstrap_ci_method=bootstrap_ci_method,
                                                                 message_indent=message_indent,
                                                                 verbose=verbose)
              
            } else {
              decision_curve_data <- NULL
            }
            
            
            # Extract information regarding stratification
            if(any(c("risk_stratification_info") %in% data_element)){
              stratification_info <- extract_risk_stratification_info(object=object,
                                                                      detail_level=detail_level,
                                                                      message_indent=message_indent,
                                                                      verbose=verbose)
            } else {
              stratification_info <- NULL
            }
            
            # Compute stratification tests
            if(any(c("risk_stratification_data") %in% data_element)){
              stratification_data <- extract_risk_stratification_data(object=object,
                                                                      data=data,
                                                                      cl=cl,
                                                                      ensemble_method=ensemble_method,
                                                                      stratification_method=stratification_method,
                                                                      detail_level=detail_level,
                                                                      confidence_level=confidence_level,
                                                                      message_indent=message_indent,
                                                                      verbose=verbose)
              
            } else {
              stratification_data <- NULL
            }
            
            if(any(c("calibration_info") %in% data_element)){
              calibration_info <- extract_calibration_info(object=object,
                                                           detail_level=detail_level,
                                                           message_indent=message_indent,
                                                           verbose=verbose)
              
            } else {
              calibration_info <- NULL
            }
            
            # Extract calibration data
            if(any(c("calibration_data") %in% data_element)){
              calibration_data <- extract_calibration_data(object=object,
                                                           data=data,
                                                           cl=cl,
                                                           ensemble_method=ensemble_method,
                                                           eval_times=eval_times,
                                                           detail_level=detail_level,
                                                           estimation_type=estimation_type,
                                                           aggregate_results=aggregate_results,
                                                           confidence_level=confidence_level,
                                                           bootstrap_ci_method=bootstrap_ci_method,
                                                           message_indent=message_indent,
                                                           verbose=verbose)
            } else {
              calibration_data <- NULL
            }
            
            # Extract AUC data
            if(any(c("auc_data") %in% data_element)){
              auc_data <- extract_auc_data(object=object,
                                           data=data,
                                           cl=cl,
                                           ensemble_method=ensemble_method,
                                           detail_level=detail_level,
                                           estimation_type=estimation_type,
                                           aggregate_results=aggregate_results,
                                           bootstrap_ci_method=bootstrap_ci_method,
                                           confidence_level=confidence_level,
                                           message_indent=message_indent,
                                           verbose=verbose)
            } else {
              auc_data <- NULL
            }
            
            # Extract confusion matrix data.
            if(any(c("confusion_matrix") %in% data_element)){
              confusion_matrix_info <- extract_confusion_matrix(object=object,
                                                                data=data,
                                                                cl=cl,
                                                                ensemble_method=ensemble_method,
                                                                detail_level=detail_level,
                                                                message_indent=message_indent,
                                                                verbose=verbose)
            } else {
              confusion_matrix_info <- NULL
            }

            # Set up a placehold pooling table. This may need to be adepted.
            pooling_table <- data.table::data.table("ensemble_data_id"=object@run_table$ensemble_data_id,
                                                    "ensemble_run_id"=object@run_table$ensemble_run_id,
                                                    "data_perturb_level"=ifelse(is.na(data@perturb_level), 0, data@perturb_level),
                                                    "pool_data_id"=0L,
                                                    "pool_run_id"=0,
                                                    "pool_perturb_level"=0)
            
            # Create a familiarData object
            fam_data <- methods::new("familiarData",
                                     outcome_type = object@outcome_type,
                                     outcome_info = object@outcome_info,
                                     fs_vimp = fs_vimp_info,
                                     model_vimp = model_vimp_info,
                                     permutation_vimp = permutation_vimp,
                                     hyperparameters = hyperparameter_info,
                                     hyperparameter_data = NULL,
                                     required_features = object@required_features,
                                     model_features = object@model_features,
                                     learner = object@learner,
                                     fs_method = object@fs_method,
                                     pooling_table = pooling_table,
                                     prediction_data = prediction_data,
                                     confusion_matrix = confusion_matrix_info,
                                     decision_curve_data = decision_curve_data,
                                     calibration_info = calibration_info,
                                     calibration_data = calibration_data,
                                     model_performance = model_performance_data,
                                     km_info = stratification_info,
                                     km_data = stratification_data,
                                     auc_data = auc_data,
                                     univariate_analysis = univar_info,
                                     feature_expressions = expression_info,
                                     feature_similarity = feature_similarity,
                                     sample_similarity = sample_similarity,
                                     ice_data = NULL,
                                     is_validation = data@load_validation,
                                     generating_ensemble = get_object_name(object=object, abbreviated=FALSE),
                                     project_id = object@project_id)
            
            # Add package version to the data set 
            fam_data <- add_package_version(object=fam_data)
            
            # Return data
            return(fam_data)
          })


# metric.main <- function(metric, purpose, object=NULL, dt=NULL, outcome_type=NULL, metric_score=NULL, score_train=NULL, score_mean=NULL, na.rm=NULL){
#   # This is a convenience function so that all relevant access to metric-specific functions can be implemented and called from here.
#   # Do not call this function directly, but use interface functions metric.assess_model_performance, checkOutcomeType and getMetricObjectiveScoreRange
# 
#   # Area under the receiver-operating curve
#   if(metric %in% c("auc", "auc_roc")){
#     if(purpose=="score"){
#       score           <- metric.auc.calc(dt=dt, metric=metric, outcome_type=outcome_type, na.rm=na.rm)
#     } else if(purpose=="objective_score"){
#       obj_score       <- metric.auc.to_objective(score=metric_score)
#     } else if(purpose=="outcome"){
#       type_is_valid   <- metric.auc.outcome(metric=metric, outcome_type=outcome_type)
#     } else if(purpose=="default_range"){
#       default_range   <- metric.auc.default_range()
#     } else if(purpose=="objective_range"){
#       obj_score_range <- metric.auc.objective_range()
#     } else if(purpose=="higher_score_better"){
#       higher_better   <- metric.auc.higher_better(metric=metric)
#     }
#   }
# 
#   # Brier score
#   if(metric=="brier"){
#     if(purpose=="score"){
#       score           <- metric.brier.calc(dt=dt, metric=metric, outcome_type=outcome_type)
#     } else if(purpose=="objective_score"){
#       obj_score       <- metric.brier.to_objective(score=metric_score)
#     } else if(purpose=="outcome"){
#       type_is_valid   <- metric.brier.outcome(metric=metric, outcome_type=outcome_type)
#     } else if(purpose=="default_range"){
#       default_range   <- metric.brier.default_range()
#     } else if(purpose=="objective_range"){
#       obj_score_range <- metric.brier.objective_range()
#     } else if(purpose=="higher_score_better"){
#       higher_better   <- metric.brier.higher_better(metric=metric)
#     }
#   }
# 
#   # Concordance index
#   if(metric %in% c("concordance_index", "global_concordance_index")){
#     if(purpose=="score"){
#       score           <- metric.concordance_index.calc(dt=dt, metric=metric, outcome_type=outcome_type, object=object, na.rm=na.rm)
#     } else if(purpose=="objective_score"){
#       obj_score       <- metric.concordance_index.to_objective(score=metric_score)
#     } else if(purpose=="outcome"){
#       type_is_valid   <- metric.concordance_index.outcome(metric=metric, outcome_type=outcome_type)
#     } else if(purpose=="default_range"){
#       default_range   <- metric.concordance_index.default_range()
#     } else if(purpose=="objective_range"){
#       obj_score_range <- metric.concordance_index.objective_range()
#     } else if(purpose=="higher_score_better"){
#       higher_better   <- metric.concordance_index.higher_better(metric=metric)
#     }
#   }
# 
#   # Accuracy and confusion matrix-based metrics
#   cm_metrics <- c("accuracy", "balanced_accuracy", "bac", "balanced_error_rate", "ber", "sensitivity", "recall", "true_positive_rate", "tpr",
#                   "specificity", "true_negative_rate", "tnr", "precision", "ppv", "npv", "false_discovery_rate", "fdr", "f1_score",  "kappa",
#                   "mcc", "matthews_correlation_coefficient", "informedness", "youden_j", "youden_index", "markedness")
#   if(any(startsWith(metric, cm_metrics))) {
#     if(purpose=="score") {
#       # Determine averaging method
#       averaging_method <- c("macro", "weighted", "micro")[endsWith(x=metric, suffix=c("_macro", "_weighted", "_micro"))]
#       if(length(averaging_method)==0){
#         averaging_method <- "macro"
#       }
# 
#       # Calculate score
#       score           <- metric.confusion_matrix.calc(data=dt, metric=metric, outcome_type=outcome_type, averaging=averaging_method)
#     } else if(purpose=="objective_score"){
#       obj_score       <- metric.confusion_matrix.to_objective(metric=metric, score=metric_score, outcome_type=outcome_type)
#     } else if(purpose=="outcome"){
#       type_is_valid   <- metric.confusion_matrix.outcome(metric=metric, outcome_type=outcome_type)
#     } else if(purpose=="default_range"){
#       default_range   <- metric.confusion_matrix.default_range(metric=metric)
#     } else if(purpose=="objective_range"){
#       obj_score_range <- metric.confusion_matrix.objective_range()
#     } else if(purpose=="higher_score_better"){
#       higher_better   <- metric.confusion_matrix.higher_better(metric=metric)
#     }
#   }
# 
#   # Regression metrics
#   if(metric %in% .get_available_regression_metrics()) {
#     if(purpose=="score"){
#       score           <- metric.regression.calc(dt=dt, metric=metric, outcome_type=outcome_type)
#     } else if(purpose=="objective_score"){
#       obj_score       <- metric.regression.to_objective(metric=metric, score=metric_score, score_mean=score_mean, outcome_type=outcome_type)
#     } else if(purpose=="outcome") {
#       type_is_valid   <- metric.regression.outcome(metric=metric, outcome_type=outcome_type)
#     } else if(purpose=="default_range"){
#       default_range   <- metric.regression.default_range(metric=metric)
#     } else if(purpose=="objective_range") {
#       obj_score_range <- metric.regression.objective_range()
#     } else if(purpose=="higher_score_better"){
#       higher_better   <- metric.regression.higher_better(metric=metric)
#     }
#   }
# 
#   # Return objects
#   if(purpose=="score"){
#     # Return metric score.
#     return(score)
# 
#   } else if(purpose=="objective_score"){
#     # Return objective score.
#     return(obj_score)
# 
#   } else if (purpose=="outcome") {
#     # Return whether the outcome is valid for the selected learner This may be
#     # unset if the learner is not specified correctly.
#     if(exists("type_is_valid", where=environment())) {
#       return(type_is_valid)
#       
#     } else {
#       return(NULL)
#     }
#     
#   } else if(purpose=="default_range"){
#     # Return default range of metric scores. These are two or three element
#     # vectors denoting the range, or range with center (e.g. 0.5 for AUC).
#     return(default_range)
#     
#   } else if(purpose=="objective_range"){
#     # Return range of objective scores.
#     return(obj_score_range)
#     
#   } else if(purpose=="higher_score_better"){
#     # Return flag that describes whether a higher score is indeed better.
#     return(higher_better)
#   }
# }







metric.get_metric_objective_range <- function(metric, outcome_type){

  # Get range of objective scores, e.g. for normalising weighted ensemble risks.
  # obj_score_range <- metric.main(metric=metric, purpose="objective_range")
  
  return(c(-1.0, 1.0))
}









metric.confusion_matrix.calc <- function(data, metric, outcome_type, averaging="macro"){
  # "averaging" determines how metrics are determined for multiclass outcomes:
  # - macro: averaged for each class
  # - weighted: weighted average over classes
  # - micro: summation of confidence matrices for classes

  # Obtain the confusion matrix
  cm <- metric.confusion_matrix.get_confusion_matrix(data=data, outcome_type=outcome_type)
  
  if(is.null(cm)){
    return(as.double(NA))
  }
  
  if(metric == "accuracy"){
    # Accuracy
    score <- metric.confusion_matrix.accuracy(cm=cm)
    
  } else if(metric %in% c("balanced_accuracy", "bac")) {
    # Balanced accuracy
    score <- metric.confusion_matrix.balanced_accuracy(cm=cm)
    
  } else if(metric %in% c("balanced_error_rate", "ber")) {
    # Balanced error rate
    score <- metric.confusion_matrix.balanced_error_rate(cm=cm)
    
  } else if(metric == "f1_score"){
    score <- metric.confusion_matrix.f1_score(cm=cm, averaging=averaging, outcome_type=outcome_type)
    
  } else if(metric %in% c("false_discovery_rate", "fdr")){
    # False discovery rate
    score <- metric.confusion_matrix.fdr(cm=cm, averaging=averaging, outcome_type=outcome_type)
    
  } else if(metric == "kappa"){
    # Cohen's kappa
    score <- metric.confusion_matrix.cohens_kappa(cm=cm)
  
  } else if(metric == "informedness"){
    # Informedness
    score <- metric.confusion_matrix.informedness(cm=cm)
    
  } else if(metric == "markedness"){
    # Markedness
    score <- metric.confusion_matrix.markedness(cm=cm)
    
  } else if(metric %in% c("mcc", "matthews_correlation_coefficient")){
    # Matthews' correlation coefficient
    score <- metric.confusion_matrix.mcc(cm=cm)
    
  } else if(metric %in% "npv"){
    # Negative predictive value
    score <- metric.confusion_matrix.npv(cm=cm, averaging=averaging, outcome_type=outcome_type)
    
  } else if(metric %in% c("precision", "ppv")) {
    # Positive predictive value
    score <- metric.confusion_matrix.ppv(cm=cm, averaging=averaging, outcome_type=outcome_type)
    
  } else if(metric %in% c("sensitivity", "recall", "true_positive_rate", "tpr")) {
    # Recall
    score <- metric.confusion_matrix.recall(cm=cm, averaging=averaging, outcome_type=outcome_type)
    
  } else if(metric %in% c("specificity", "true_negative_rate", "tnr")){
    # Specificity
    score <- metric.confusion_matrix.specificity(cm=cm, averaging=averaging, outcome_type=outcome_type)
    
  } else if(metric %in% c("youden_j", "youden_index")){
    # Youden J index
    score <- metric.confusion_matrix.youden_index(cm=cm, averaging=averaging, outcome_type=outcome_type)
    
  } else {
    ..error_reached_unreachable_code("metric.confusion_matrix.calc_no_known_metric")
  }
  
  return(score)
}



metric.confusion_matrix.default_range <- function(metric){
  # Default range of metric values.
  
  if(metric %in% c("balanced_error_rate", "ber", "false_discovery_rate", "fdr")) {
    # These metrics range from 0 (best) to 1 (worst).
    default_range <- c(1.0, 0.0)
    
  } else if(metric %in% c("kappa", "mcc", "matthews_correlation_coefficient", "informedness",
                          "youden_j", "youden_index", "markedness")) {
    # These metrics range from -1 (worst) to 1 (best).
    default_range <- c(-1.0, 1.0)
    
  } else {
    # Other metrics range from 0 (worst) to 1 (best).
    default_range <- c(0.0, 1.0)
  }
  
  return(default_range)
}



metric.confusion_matrix.objective_range <- function() {
  # Range of values for objective scores
  return(c(0, 1))
}



metric.confusion_matrix.outcome <- function(metric, outcome_type){
  if(outcome_type %in% c("binomial", "multinomial")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



metric.confusion_matrix.to_objective <- function(metric, score, outcome_type){
  # For most confusion matrix-based metrics 0 is worst and 1 is best.

  # Set NA predictions to random
  score[!is.finite(score)] <- as.double(NA)

  # Most metrics are scaled from 0 (worst) to 1 (best). Several scores require inversion or further processing.
  if(metric %in% c("balanced_error_rate", "ber", "false_discovery_rate", "fdr")) {
    # These metrics range from 0 (best) to 1 (worst), and should be inverted.
    obj_score <- 1 - score
    
  } else if(metric %in% c("kappa", "mcc", "matthews_correlation_coefficient", "informedness",
                          "youden_j", "youden_index", "markedness")) {
    # This score ranges from -1 (worst) to 1 (best), and should be normalised.
    obj_score <- (score + 1) / 2
    
  } else {
    # Scores that range from 0 to 1 (best).
    obj_score <- score
  }

  return(obj_score)
}


metric.confusion_matrix.higher_better <- function(metric){
  if(metric %in% c("balanced_error_rate", "ber", "false_discovery_rate", "fdr")){
    return(FALSE)
    
  } else {
    return(TRUE)
  }
}



metric.confusion_matrix.get_confusion_matrix <- function(data, outcome_type){
  
  # Get the classes and number of classes in data
  classes <- levels(droplevels(data$outcome))
  n_classes <- length(classes)
  
  # Skip calculation if only one class is present (this should never happen)
  if(n_classes <= 1) return(NULL)
  
  # Get the column with predicted outcomes
  p_outc_col <- getPredictedOutcomeColumn(outcome_type=outcome_type)
  
  # Identify the real outcome columns
  outc_col <- get_outcome_columns(x=outcome_type)
  
  # Remove missing outcome and missing probability values
  valid_entries <- is.finite(data[[p_outc_col]])
  
  # Maintain only valid entries
  data <- data[valid_entries, ]
  
  # Skip further processing if no valid entries exist
  if(is_empty(data)) return(NULL)
  
  # Create empty scalars
  tp <- tn <- fp <- fn <- prevalence <- bias <- numeric(n_classes)
  
  # Total number of samples
  n_samples <- nrow(data)
  
  # Iterate over positive classes
  for(ii in seq_len(n_classes)){
    
    # Determine true positives, true negatives, false positives and false negatives
    tp[ii] <- nrow(data[get(p_outc_col)==classes[ii] & get(outc_col)==classes[ii], ])
    tn[ii] <- nrow(data[get(p_outc_col)!=classes[ii] & get(outc_col)!=classes[ii], ])
    fp[ii] <- nrow(data[get(p_outc_col)==classes[ii] & get(outc_col)!=classes[ii], ])
    fn[ii] <- nrow(data[get(p_outc_col)!=classes[ii] & get(outc_col)==classes[ii], ])
    
    # Prevalence (fraction of observed positive class)
    prevalence[ii] <- nrow(data[get(outc_col)==classes[ii]]) / n_samples
    
    # Bias (fraction of predicted positive class)
    bias[ii] <- nrow(data[get(p_outc_col)==classes[ii]]) / n_samples
  }
  
  return(list("tp"=tp, "tn"=tn, "fp"=fp, "fn"=fn, "prevalence"=prevalence, "bias"=bias,
              "n_samples"=n_samples, "n_classes"=n_classes))
}



metric.confusion_matrix.aggregate_matrix <- function(cm, averaging, outcome_type){
  
  if(outcome_type == "binomial"){
    # Use second class as positive class
    return(list("tp" = cm$tp[2],
                "tn" = cm$tn[2],
                "fp" = cm$fp[2],
                "fn" = cm$fn[2],
                "prevalence" = 1.0,
                "bias" = 1.0,
                "n_samples" = cm$n_samples,
                "n_classes" = 1.0))
    
  } else if(outcome_type == "multinomial"){
    
    if(averaging == "micro"){
      # Merge confusion matrix for micro-averaging.
      return(list("tp" = sum(cm$tp),
                  "tn" = sum(cm$tn),
                  "fp" = sum(cm$fp),
                  "fn" = sum(cm$fn),
                  "prevalence" = 1.0,
                  "bias" = 1.0,
                  "n_samples" = cm$n_classes * cm$n_samples,
                  "n_classes" = 1.0))
      
    } else {
      # No changes are needed.
      return(cm)
    }
    
  } else {
    ..error_reached_unreachable_code("metric.confusion_matrix.average_score_unknown_outcome_type")
  }
}



metric.confusion_matrix.average_score <- function(score, cm, averaging, outcome_type){
  
  if(outcome_type == "binomial"){
    return(score)
  
  } else if(outcome_type == "multinomial"){
    
    if(averaging == "weighted"){
      # Weighted averaging
      return(sum(cm$prevalence * score))
      
    } else if(averaging == "macro"){
      # Macro averaging
      return(sum(score) / cm$n_classes)
      
    } else if(averaging == "micro"){
      # Micro averaging
      return(score)
      
    } else {
      ..error_reached_unreachable_code("metric.confusion_matrix.average_score_unknown_averaging_method")
    }
    
  } else {
    ..error_reached_unreachable_code("metric.confusion_matrix.average_score_unknown_outcome_type")
  }
}



metric.confusion_matrix.accuracy <- function(cm){
  # Accuracy
  return(sum(cm$tp) / cm$n_samples)
}



metric.confusion_matrix.balanced_accuracy <- function(cm){
  # Balanced accuracy [Brodersen, K. H., Ong, C. S., Stephan, K. E. & Buhmann,
  # J. M. The Balanced Accuracy and Its Posterior Distribution. in 2010 20th
  # International Conference on Pattern Recognition 3121–3124 (2010).]
  return(sum(cm$tp / (cm$tp + cm$fn)) / cm$n_classes)
}



metric.confusion_matrix.balanced_error_rate <- function(cm){
  # Balanced error rate
  return(sum(cm$fn / (cm$tp + cm$fn)) / cm$n_classes)
}



metric.confusion_matrix.cohens_kappa <- function(cm){
  # Cohen's kappa [Cohen, J. A Coefficient of Agreement for Nominal Scales.
  # Educ. Psychol. Meas. 20, 37–46 (1960).]
  #
  # Implementation after
  # https://stats.stackexchange.com/questions/251165/cohens-kappa-with-three-categories-of-variable
  
  # Observed agreement
  obs_agreement <- sum(cm$tp) / cm$n_samples
  
  # Agreement expected at random
  rand_agreement <- sum((cm$tp + cm$fp) * (cm$tp + cm$fn)) / cm$n_samples^2
  
  if(rand_agreement == 1.0 & obs_agreement == 1.0){
    # Prevent division by zero for perfect predictions.
    return(1.0)
    
  } else {
    # Compute kappa
    return((obs_agreement - rand_agreement) / (1.0 - rand_agreement))
  }
}



metric.confusion_matrix.f1_score <- function(cm, averaging, outcome_type){
  # F1 score
  
  # Aggregate matrix
  cm <- metric.confusion_matrix.aggregate_matrix(cm=cm, averaging=averaging, outcome_type=outcome_type)
  
  # Compute score
  score <- 2.0 * cm$tp / (2.0 * cm$tp + cm$fp + cm$fn)

  # Averaging
  return(metric.confusion_matrix.average_score(score=score, cm=cm, averaging=averaging, outcome_type=outcome_type))
}



metric.confusion_matrix.fdr <- function(cm, averaging, outcome_type){
  # False detection rate
  
  # Aggregate matrix
  cm <- metric.confusion_matrix.aggregate_matrix(cm=cm, averaging=averaging, outcome_type=outcome_type)
  
  # Compute score
  score <- cm$fp / (cm$tp + cm$fp)
  
  # Averaging
  return(metric.confusion_matrix.average_score(score=score, cm=cm, averaging=averaging, outcome_type=outcome_type))
}


metric.confusion_matrix.informedness <- function(cm){
  # Informedness [Powers, D. M. Evaluation: from precision, recall and F-measure
  # to ROC, informedness, markedness and correlation. International Journal of
  # Machine Learning Technology 2, 37–63 (2011).]
  #
  # See equations 28, 42.
  
  return(sum(cm$prevalence / (1.0 - cm$prevalence) * (cm$tp / (cm$tp + cm$fn) - cm$bias)))
}



metric.confusion_matrix.markedness <- function(cm){
  # Markedness [Powers, D. M. Evaluation: from precision, recall and F-measure
  # to ROC, informedness, markedness and correlation. International Journal of
  # Machine Learning Technology 2, 37–63 (2011).]
  #
  # See equations 29, 43.
  
  return(sum(cm$bias / (1.0 - cm$bias) * (cm$tp / (cm$tp + cm$fp) - cm$prevalence)))
}



metric.confusion_matrix.mcc <- function(cm){
  # Matthews' correlation coefficient.
  #
  # See [Jurman, G., Riccadonna, S. & Furlanello, C. A comparison of MCC and CEN
  # error measures in multi-class prediction. PLoS One 7, e41882 (2012);
  # Gorodkin, J. Comparing two K-category assignments by a K-category
  # correlation coefficient. Comput. Biol. Chem. 28, 367–374 (2004).]
  # 
  # Mirrors scikit-learn implementation.

  cov_obs_pred  <- sum(cm$tp) / cm$n_samples - sum(cm$prevalence * cm$bias)
  cov_obs_obs   <- 1.0 - sum(cm$prevalence * cm$prevalence)
  cov_pred_pred <- 1.0 - sum(cm$bias * cm$bias)
  
  if(cov_obs_obs == 0.0 | cov_pred_pred == 0.0){
    return(0.0)
    
  } else {
    return(cov_obs_pred / sqrt(cov_obs_obs * cov_pred_pred))
  }
}



metric.confusion_matrix.npv <- function(cm, averaging, outcome_type){
  # Negative predictive value
  
  # Aggregate matrix
  cm <- metric.confusion_matrix.aggregate_matrix(cm=cm, averaging=averaging, outcome_type=outcome_type)
  
  # Compute NPV
  score <- cm$tn / (cm$tn + cm$fn)
  
  # Averaging
  return(metric.confusion_matrix.average_score(score=score, cm=cm, averaging=averaging, outcome_type=outcome_type))
}



metric.confusion_matrix.ppv <- function(cm, averaging, outcome_type){
  # Positive predictive value
  #
  # PPV is also called precision.
  
  # Aggregate matrix
  cm <- metric.confusion_matrix.aggregate_matrix(cm=cm, averaging=averaging, outcome_type=outcome_type)
  
  # Compute PPV.
  score <- cm$tp / (cm$tp + cm$fp)
  
  # Averaging
  return(metric.confusion_matrix.average_score(score=score, cm=cm, averaging=averaging, outcome_type=outcome_type))
}



metric.confusion_matrix.recall <- function(cm, averaging, outcome_type){
  # Recall
  #
  # Recall is also called sensitivity and true positive rate.
  
  # Aggregate matrix
  cm <- metric.confusion_matrix.aggregate_matrix(cm=cm, averaging=averaging, outcome_type=outcome_type)
  
  # Compute recall.
  score <- cm$tp / (cm$tp + cm$fn)
  
  # Averaging
  return(metric.confusion_matrix.average_score(score=score, cm=cm, averaging=averaging, outcome_type=outcome_type))
}



metric.confusion_matrix.specificity <- function(cm, averaging, outcome_type){
  # Specificity
  #
  # Specifity is also called the true negative rate.
  
  # Aggregate matrix
  cm <- metric.confusion_matrix.aggregate_matrix(cm=cm, averaging=averaging, outcome_type=outcome_type)
  
  # Compute specificity.
  score <- cm$tn / (cm$tn + cm$fp)
  
  # Averaging
  return(metric.confusion_matrix.average_score(score=score, cm=cm, averaging=averaging, outcome_type=outcome_type))
}



metric.confusion_matrix.youden_index <- function(cm, averaging, outcome_type){
  # Youden's J index [Youden, W. J. Index for rating diagnostic tests. Cancer 3,
  # 32–35 (1950).]
  
  # Aggregate matrix
  cm <- metric.confusion_matrix.aggregate_matrix(cm=cm, averaging=averaging, outcome_type=outcome_type)
  
  # Compute Youden index.
  score <- cm$tp / (cm$tp + cm$fn) + cm$tn / (cm$tn + cm$fp) - 1.0
  
  # Averaging
  return(metric.confusion_matrix.average_score(score=score, cm=cm, averaging=averaging, outcome_type=outcome_type))
}

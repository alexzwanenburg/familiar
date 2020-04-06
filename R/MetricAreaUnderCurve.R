metric.auc.calc <- function(dt, metric, outcome_type, na.rm=FALSE){

  # Suppress NOTES due to non-standard evaluation in data.table
  outcome <- NULL

  # Get the classes and number of classes in dt
  classes   <- levels(droplevels(dt$outcome))
  n_classes <- length(classes)

  # Skip calculation if only one class is present (this should never happen)
  if(n_classes==1){ return(as.double(NA)) }

  # Remove missing values
  if(na.rm==TRUE){
    # Get names of predicted class probabilities
    pred_prob_cols <- get_class_probability_name(x=classes)

    # Determine valid entries
    valid_entries  <- apply(dt[, pred_prob_cols, with=FALSE], 1, function(x) (all(is.finite(x))))

    # Maintain only valid entries
    dt             <- dt[valid_entries, ]

    # Skip further processing if no valid entries exist
    if(nrow(dt)==0) { return(as.double(NA)) }
  }

  # Define class combinations (>1 in case of multinomial outcomes)
  class_comb  <- utils::combn(classes, 2)
  n_class_comb <- ncol(class_comb)

  # Generate empty auc vector
  # AUC of the ROC is calculated according to Hand, D.J, and Till, R.J. A simple generalisation of the area under
  # the ROC curve for multiple class classification problems, Machine Learning 45 171-186 (2001)
  auc_score   <- vector(mode="numeric", length=n_class_comb)

  # Iterate over combinations
  for(ii in seq_len(n_class_comb)){
    # Find the current positive and negative classes
    pos_class <- class_comb[1,ii]
    neg_class <- class_comb[2,ii]

    # Get the probability column name for the positive class
    pos_class_prob_col <- get_class_probability_name(x=pos_class)

    # Get the probabilities that correspond to the positive and negative class in outcome (g and f in Hand et al.)
    prob_pos  <- dt[outcome==pos_class, ][[pos_class_prob_col]]
    prob_neg  <- dt[outcome==neg_class, ][[pos_class_prob_col]]

    # Get number of positive and negative class entries (n0 and n1 in Hand et al.)
    n_pos     <- length(prob_pos)
    n_neg     <- length(prob_neg)

    # Calculate auc only when positive and negative classes are present (this should always be the case as we drop unused class levels at the start)
    if(n_pos > 0 & n_neg >0){
      # Determine probability ranks
      prob_rank <- data.table::frank(x=c(prob_pos, prob_neg), ties.method="average")

      # Calculate auc
      auc_score[ii] <- (sum(prob_rank[seq_len(n_pos)]) - n_pos * (n_pos+1)/2) / (n_pos * n_neg)
    }
  }

  # Calculate mean AUC (eq. 7 from Hand et al.). This has no effect for binomial AUC.
  auc_score <- 2/(n_classes * (n_classes-1)) * sum(auc_score, na.rm=TRUE)

  return(auc_score)
}



metric.auc.objective_range <- function() {
  # Range of values for objective scores
  return(c(-1, 1))
}


metric.auc.default_range <- function(){
  # Default range. 0.5 is random.
  return(c(0.0, 0.5, 1.0))
}


metric.auc.outcome <- function(metric, outcome_type){
  if(metric %in% c("auc", "auc_roc") & outcome_type %in% c("binomial", "multinomial")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}



metric.auc.to_objective <- function(score){
  # The AUC is better evaluated at normal scales than at logarithmic scales

  # Set NA predictions to random
  score[!is.finite(score)] <- as.double(NA)

  # Return on (-1,1) range
  score <- 2*(score-0.5)

  return(score)
}



metric.auc.higher_better <- function(metric){
  return(TRUE)
}

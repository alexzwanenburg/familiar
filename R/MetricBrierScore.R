metric.brier.calc <- function(dt, metric, outcome_type){

  # Get the classes and number of classes in dt
  classes        <- levels(droplevels(dt$outcome))
  n_classes      <- length(classes)

  # Skip calculation if only one class is present (this should never happen)
  if(n_classes<=1){ return(as.double(NA)) }

  # Get names of columns with predicted class probabilities
  pred_prob_cols <- getClassProbabilityColumns(outcome_type=outcome_type, class_levels=classes)

  # Identify the real outcome columns
  outc_col       <- get_outcome_columns(x=outcome_type)

  # Remove missing outcome and missing probability values
  valid_entries  <- apply(dt[, pred_prob_cols, with=FALSE], 1, function(x) (all(is.finite(x))))

  # Maintain only valid entries
  dt             <- dt[valid_entries, ]

  # Skip further processing if no valid entries exist
  if(nrow(dt)==0) { return(as.double(NA)) }

  # Create empty brier score
  brier_score    <- vector(mode="numeric", length=n_classes)

  # Iterate over classes
  for(ii in seq_len(n_classes)){
    # Find the current positive and negative classes
    pos_class    <- classes[ii]

    # Get the probability column name for the positive class
    pos_class_prob_col <- getClassProbabilityColumns(outcome_type=outcome_type, class_levels=pos_class)

    # Copy data table
    dt_brier     <- data.table::copy(dt[,c(outc_col, pos_class_prob_col), with=FALSE])

    # Mark positive class as 1
    dt_brier[, "pos_outcome":=0]
    dt_brier[get(outc_col)==pos_class, "pos_outcome":=1]

    # Calculate uncorrected brier score for the current class
    brier_score[ii] <- sum((dt_brier[[pos_class_prob_col]] - dt_brier$pos_outcome)^2)
  }

  # Calculate overall brier score
  brier_score <- sum(brier_score) / nrow(dt)

  # Correct score for binomial and multinomial outcomes so that it falls in the (0, 1) range
  brier_score <- brier_score / 2.0
  
  return(brier_score)
}


metric.brier.default_range <- function(){
  # Range of default metric values. Note that 1 is worst, and 0 is best.
  return(c(1.0, 0.0))
}


metric.brier.objective_range <- function() {
  # Range of possible objective values.
  return(c(0, 1))
}



metric.brier.outcome <- function(metric, outcome_type){
  if(metric %in% c("brier") & outcome_type %in% c("binomial", "multinomial")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}



metric.brier.to_objective <- function(score){
  # For the brier score 0 is the best, and 1 is the worst. Since we maximise the
  # score, the brier objective score is inverted in direction, and rescaled.

  # Set NA predictions to random
  score[!is.finite(score)] <- as.double(NA)

  # Invert the original score (-1 worst, 0 best)
  obj_score <- -score

  # Normalise to (0, 1) range
  obj_score <- obj_score + 1.0

  return(obj_score)
}



metric.brier.higher_better <- function(metric){
  return(FALSE)
}

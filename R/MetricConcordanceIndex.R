metric.concordance_index.calc <- function(dt, metric="concordance_index", learner=NULL, outcome_type, na.rm=FALSE){
  # Wraps the metric.concordance_index.cindex_ function

  # Suppress NOTES due to non-standard evaluation in data.table
  outcome_pred <- NULL

  # Remove missing values
  if(na.rm==TRUE){

    # Maintain only valid entries
    dt <- dt[is.finite(outcome_pred), ]

    # Skip further processing if no valid entries exist
    if(nrow(dt)==0) { return(as.double(NA)) }
  }

  # Get outcome type
  outcome_cols <- get_outcome_columns(x=outcome_type)

  if(metric=="concordance_index"){
    # Calculate concordance index
    score <- metric.concordance_index.cindex_(x=dt$outcome_pred, y=as.matrix(dt[, outcome_cols, with=FALSE]))
  } else if(metric=="global_concordance_index"){
    # Calculate global concordance index
    score <- metric.concordance_index.cindex_(x=dt$outcome_pred, y=as.matrix(dt[, outcome_cols, with=FALSE]), weight="sqrt")
  }

  # Invert the concordance index for risks (which are inversely related to survival times)
  if(learner.check_model_prediction_type(learner=learner, outcome_type=outcome_type) %in% c("hazard_ratio", "sum_cumulative_hazard")){
    score <- 1 - score
  }

  return(score)
}



metric.concordance_index.default_range <- function(){
  # Default range.
  return(c(0.0, 0.5, 1.0))
}



metric.concordance_index.objective_range <- function() {
  # Range of values for objective scores.
  return(c(-1, 1))
}



metric.concordance_index.outcome <- function(metric, outcome_type){

  if(outcome_type=="survival"){
    return(TRUE)
  } else {
    return(FALSE)
  }
}



metric.concordance_index.to_objective <- function(score){
  # The concordance index is better evaluated at normal scales.

  # Set NA predictions to random
  score[!is.finite(score)] <- as.double(NA)

  # Return on (-1,1) range. Note that metric.concordance_index.calc
  # automatically converts values so that the best concordance is found at 1.0.
  # This behaviour is driven by the output of the learner, i.e. risk-like scores
  # or time-like scores.
  score <- 2*(score-0.5)

  return(score)
}



metric.concordance_index.cindex_ <- function(x, y, weight=NULL) {
  #Based on Pencina et al. 2004; doi:10.1002/sim.1802

  # Suppress NOTES due to non-standard evaluation in data.table
  id.x <- id.y <- event.x <- event.y <- time.x <- time.y <- pred.x <- pred.y <- NULL

  # x is vector with predictor values
  # y is the vector with censored outcomes

  # Generate a combinatorial data set
  dt <- data.table("id_join"=1, "id"=seq_len(length(x)), "pred"=x, "time"=y[,1], "event"=y[,2])
  dt <- merge(dt, dt, by="id_join", allow.cartesian=TRUE)[id.x < id.y, ][, ":="(id_join=NULL, id.x=NULL, id.y=NULL)]

  # Get only useful pairs (event-event with non-tied times; event-non-event with non-event surviving past event)
  dt <- dt[(event.x==1 & event.y==1 & time.x != time.y) | (event.x==1 & time.x < time.y) | (event.y==1 & time.y < time.x), ]

  if(is.null(weight)){
    dt[, "weight":=1.0]
  } else if(weight=="sqrt"){
    dt[, "weight":=sqrt(abs(time.x-time.y))]
  }

  # Calculate concordance index using Noethers method.
  n_concord <- sum(dt[(pred.x > pred.y & time.x > time.y) | (pred.x < pred.y & time.x < time.y)]$weight)
  n_discord <- sum(dt[(pred.x > pred.y & time.x < time.y) | (pred.x < pred.y & time.x > time.y)]$weight)
  n_ties <-    sum(dt[pred.x == pred.y]$weight)

  # Calculate concordance index
  ci <- (n_concord + 0.5 * n_ties) / (n_concord + n_discord + n_ties)

  # Check if the concordance index is valid
  if(!is.finite(ci)) { ci <- as.double(NA) }

  return(ci)
}


metric.concordance_index.higher_better <- function(metric){
  return(TRUE)
}

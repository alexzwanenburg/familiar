vimp.corelearn.learner <- function(method, outcome_type){
  return(NULL)
}

vimp.corelearn.outcome <- function(method, outcome_type){

  if(outcome_type %in% c("binomial", "multinomial") & method %in% c("gini", "mdl", "relieff_exp_rank", "gain_ratio")){
    return(TRUE)
  } else if(outcome_type %in% c("continuous", "count") & method %in% c("relieff_exp_rank")) {
    return(TRUE)
  } else {
    return(FALSE)
  }

}



vimp.corelearn.param <- function(data_obj, method){

  return(list())
}



vimp.corelearn.vimp <- function(data_obj, method){
  # Variable importance using learners implemented in the CORElearn package

  # Internal variable
  outcome_type <-  data_obj@outcome_type

  # Select all feature columns
  feature_cols <- get_feature_columns(x=data_obj)

  # Select feature columns which lack variance
  feature_no_var_cols <- feature_cols[sapply(feature_cols, function(ii, dt) (is_singular_data(dt[[ii]])), dt=data_obj@data)]
  valid_feature_cols  <- feature_cols[!feature_cols %in% feature_no_var_cols]

  # Check if there are valid (non-singular) features left
  if(length(valid_feature_cols) > 0){

    # Generate a formula with only feature columns that have some variance
    formula             <- stats::reformulate(valid_feature_cols, response=quote(outcome))

    # Calculate gini index (Kononenko1995 - On biases in estimating multi-valued attributes)
    if(method=="gini"){
      score             <- CORElearn::attrEval(formula, data=data_obj@data, estimator="Gini")
    } else if(method=="mdl") {
      score             <- CORElearn::attrEval(formula, data=data_obj@data, estimator="MDL")
    } else if(method=="relieff_exp_rank"){
      if(outcome_type %in% c("continuous", "count")) {
        score           <- CORElearn::attrEval(formula, data=data_obj@data, estimator="RReliefFexpRank")
      } else if(outcome_type %in% c("binomial", "multinomial")){
        score           <- CORElearn::attrEval(formula, data=data_obj@data, estimator="ReliefFexpRank")
      }
    } else if(method=="gain_ratio"){
      score             <- CORElearn::attrEval(formula, data=data_obj@data, estimator="GainRatio")
    }

    # Generate variable importance data table
    dt_vimp             <- data.table::data.table("score"=score, "name"=names(score))
    dt_vimp$rank        <- data.table::frank(-(dt_vimp$score), ties.method="min")
    dt_vimp$multi_var   <- FALSE

  } else {
    dt_vimp <- getEmptyVimp()
  }
  return(dt_vimp)
}

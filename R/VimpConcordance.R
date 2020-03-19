vimp.concordance.learner <- function(method, outcome_type){
  return(NULL)
}

vimp.concordance.outcome <- function(method, outcome_type){

  if(outcome_type %in% c("binomial", "multinomial", "continuous", "count", "survival")){
    return(TRUE)
  } else {
    return(FALSE)
  }

}



vimp.concordance.param <- function(data_obj, method){

  return(list())
}



vimp.concordance.vimp <- function(data_obj){
  # Use concordance-based measures for variable importance:
  # - Gini index for binomial and multinomial outcomes
  # - Kendall's Tau for continuous and counts outcomes
  # - Concordance index for surival features

  # Internal
  outcome_type <- data_obj@outcome_type


  # Calculate gini index for categorical outcomes
  if(outcome_type %in% c("binomial", "multinomial")){
    dt_vimp <- vimp.corelearn.vimp(data_obj=data_obj, method="gini")

    return(dt_vimp)
  }

  # For continuous outcomes use kendall's tau from the correlation.vimp function
  if(outcome_type %in% c("continuous", "count")) {
    dt_vimp <- vimp.correlation.vimp(data_obj=data_obj, method="kendall")

    return(dt_vimp)
  }

  if(outcome_type == "survival") {

    # Use effect coding to convert categorical data into encoded data
    contrast_list <- getContrasts(dt=data_obj@data, method="effect", drop_levels=TRUE, outcome_type=outcome_type)

    # Extract data table with contrasts
    data <- contrast_list$dt_contrast

    # Find feature columns in data table
    feature_cols <- get_feature_columns(x=data, outcome_type=outcome_type)

    # Calculate concordance index
    ci <- sapply(feature_cols, function(ii, data){
      metric.concordance_index.cindex_(x=data[[ii]], y=survival::Surv(data$outcome_time, data$outcome_event))
    }, data=data)

    # Generate variable importance data table
    dt_vimp           <- data.table::data.table("score"=(ci-0.5)*2, "name"=feature_cols)
    dt_vimp           <- applyContrastReference(dt=dt_vimp, dt_ref=contrast_list$dt_ref, method="abs_max")
    dt_vimp$rank      <- data.table::frank(1-abs(dt_vimp$score), ties.method="min")
    dt_vimp$score     <- dt_vimp$score/2 + 0.5
    dt_vimp$multi_var <- FALSE

    return(dt_vimp)
  }
}

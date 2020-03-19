vimp.correlation.learner <- function(method, outcome_type){
  return(NULL)
}


vimp.correlation.outcome <- function(method, outcome_type){

  if(outcome_type %in% c("continuous", "count", "survival")){
    return(TRUE)
  } else {
    return(FALSE)
  }

}



vimp.correlation.param <- function(data_obj, method){

  return(list())
}



vimp.correlation.vimp <- function(data_obj, method){
  # Correlation based variable importance, using spearman, pearson and kendall correlation

  # Suppress NOTES due to non-standard evaluation in data.table
  outcome_event <- NULL

  # Set outcome type
  outcome_type <- data_obj@outcome_type

  # Drop non-event data for censored data analysis for calculating correlation and set outcome column
  if(outcome_type=="survival"){
    data <- data_obj@data[outcome_event==1, ]
    outcome_col <- "outcome_time"
  } else {
    data <- data_obj@data
    outcome_col <- "outcome"
  }

  # Use effect coding to convert categorical data into encoded data
  contrast_list <- getContrasts(dt=data, method="effect", drop_levels=TRUE, outcome_type=outcome_type)

  # Extract data table with contrasts
  data <- contrast_list$dt_contrast

  # Find feature columns in data table
  feature_cols <- get_feature_columns(x=data, outcome_type=outcome_type)

  # Calculate correlation coefficient
  corr_coef <- sapply(feature_cols, function(ii, data, outcome_col, corr_method){
    stats::cor(x=data[[ii]], y=data[[outcome_col]], method=corr_method)
  }, data=data, outcome_col=outcome_col, corr_method=method)

  # Generate variable importance data table
  dt_vimp           <- data.table::data.table("score"=corr_coef, "name"=feature_cols)
  dt_vimp           <- applyContrastReference(dt=dt_vimp, dt_ref=contrast_list$dt_ref, method="abs_max")
  dt_vimp$rank      <- data.table::frank(1-abs(dt_vimp$score), ties.method="min")
  dt_vimp$multi_var <- FALSE

  return(dt_vimp)
}

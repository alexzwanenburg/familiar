metric.compute_optimisation_score <- function(score_table,
                                              optimisation_objective){
  browser()
  # Select the correct optimisation function.
  optimisation_function <- switch(optimisation_objective,
                                  "max_validation" = metric.optim_score.max_validation,
                                  "balanced" = metric.optim_score.balanced,
                                  "stronger_balance" = metric.optim_score.stronger_balance)
  
  # Find identifier columns.
  id_columns <- intersect(colnames(score_table),
                          c("param_id", "run_id"))
  
  # Cast objective score wide by data_set.
  optimisation_table <- dcast(data=score_table[, c(id_columns, "metric", "data_set", "objective_score")],
                              param_id + run_id + metric ~ data_set,
                              value.var="objective_score")
  
  # Compute optimisation score based on objective scores.
  optimisation_table[, list("optimisation_score"=optimisation_function(training=training,
                                                                       validation=validation)),
                     by=c(id_columns, "metric")]
  
  # Average optimisation score over metrics.
  optimisation_table[, list("optimisation_score"=mean(optimisation_score, na.rm=TRUE)),
                     by=id_columns]
  
  return(optimisation_table)
}



metric.optim_score.max_validation <- function(training=NULL, validation) return(validation)

metric.optim_score.balanced <- function(training, validation) return(validation - abs(validation - training))

metric.optim_score.stronger_balance <- function(training, validation) return(validation - 2.0 * abs(validation - training))


metric.summarise_optimisation_score <- function(score_table, method){
  # Calculates a summary objective score
  
  # Suppress NOTES due to non-standard evaluation in data.table
  optimisation_score <- NULL
  
  # Find identifier columns.
  id_columns <- intersect(colnames(score_table),
                          "param_id")

  # Obtain the aggregation method.
  aggregation_method <- switch(method,
                               "percentile"=stats::median,
                               "z_statistic"=mean,
                               "median"=stats::median,
                               "mean"=mean,
                               "max"=max,
                               "min"=min)
  
  # Compute the mean optimisation score, overall, or per parameter id.
  score_table <- score_table[, list("optimisation_score"=aggregation_method(optimisation_score, na.rm=TRUE)),
                             by=id_columns]
  
  return(score_table)
}



metric.get_objective_score <- function(dt, metric, objective, outcome_type, as_vector=FALSE){
  # Calculates objective score  

  # Suppress NOTES due to non-standard evaluation in data.table
  obj_score_train <- obj_score_valid <- NULL

  # Get score range
  score_range <- metric.get_metric_objective_range(metric=metric, outcome_type=outcome_type)
  min_score   <- min(score_range)
  max_score   <- max(score_range)

  # Replace NAs by min_score and max_score
  dt[is.na(obj_score_train)|is.na(obj_score_valid), ":="("obj_score_train"=max_score, "obj_score_valid"=min_score)]

  # Calculate objective score
  dt$obj_score <- metric.obj_score.calculate(objective=objective, obj_score_train=dt$obj_score_train, obj_score_valid=dt$obj_score_valid)

  if(as_vector==FALSE){
    return(dt)
  } else {
    return(dt$obj_score)
  }
}



metric.get_objective_score_range <- function(metric, objective, outcome_type=outcome_type){
  # Return range for objective scores

  # Get log score range
  score_range <- metric.get_metric_objective_range(metric=metric, outcome_type=outcome_type)
  min_score   <- min(score_range)
  max_score   <- max(score_range)

  # Calculate minimum and maximum objective scores
  max_obj_score <- metric.obj_score.calculate(objective=objective, obj_score_train=max_score, obj_score_valid=max_score)
  min_obj_score <- metric.obj_score.calculate(objective=objective, obj_score_train=max_score, obj_score_valid=min_score)

  return(c(min_obj_score, max_obj_score))
}



metric.summarise_objective_scores <- function(dt, objective, as_vector=FALSE){
  # Calculates a summary objective score

  # Suppress NOTES due to non-standard evaluation in data.table
  obj_score <- param_id <- NULL

  # Check whether aggregation columns are present
  # if(all(c("param_id") %in% colnames(dt))){
  #   # Calculate summary objective score over aggregation columns
  #   dt <- dt[, list(summ_obj_score=metric.obj_score.calculate(objective=objective, obj_score_train=mean(obj_score_train), obj_score_valid=mean(obj_score_valid))),
  #            by=list(param_id)]
  # } else {
  #   # Calculate summary objective over available data
  #   dt <- dt[, list(summ_obj_score=metric.obj_score.calculate(objective=objective, obj_score_train=mean(obj_score_train), obj_score_valid=mean(obj_score_valid)))]
  # }

  if(all(c("param_id") %in% colnames(dt))){
    # Calculate summary objective score over aggregation columns
    dt <- dt[, list(summ_obj_score=mean(obj_score)), by=list(param_id)]
  } else {
    # Calculate summary objective over available data
    dt <- dt[, list(summ_obj_score=mean(obj_score))]
  }

  # Determine whether a data table or a vector should be returned
  if(as_vector==FALSE){
    return(dt)
  } else {
    return(dt$summ_obj_score)
  }
}



metric.obj_score.calculate <- function(objective, obj_score_train, obj_score_valid){
  if(objective=="max_validation"){
    return(metric.obj_score.max_validation(obj_score_valid=obj_score_valid))
  } else if(objective=="balanced") {
    return(metric.obj_score.balanced(obj_score_train=obj_score_train, obj_score_valid=obj_score_valid))
  } else if(objective=="stronger_balance") {
    return(metric.obj_score.stronger_balance(obj_score_train=obj_score_train, obj_score_valid=obj_score_valid))
  }
}



metric.obj_score.max_validation <- function(obj_score_train=NULL, obj_score_valid){
  # Objective score for optimising validation performance
  return(obj_score_valid)
}



metric.obj_score.balanced <- function(obj_score_train, obj_score_valid){
  # Objective score for optimising balance between training and validation sets
  return(obj_score_valid - abs(obj_score_valid - obj_score_train))
}



metric.obj_score.stronger_balance <- function(obj_score_train, obj_score_valid){
  # Objective score for optimising balance with stronger unbalance penalty
  return(obj_score_valid - 2 * abs(obj_score_valid - obj_score_train))
}

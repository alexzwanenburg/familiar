metric.compute_optimisation_score <- function(score_table,
                                              optimisation_objective){
  
  # Select the correct optimisation function.
  optimisation_function <- switch(optimisation_objective,
                                  "max_validation" = metric.optim_score.max_validation,
                                  "balanced" = metric.optim_score.balanced,
                                  "stronger_balance" = metric.optim_score.stronger_balance)
  
  # Find identifier columns.
  id_columns <- intersect(colnames(score_table),
                          c("param_id", "run_id"))
  
  # Create formula
  formula <- stats::reformulate(termlabels="data_set",
                                response=paste0(c(id_columns, "metric"), collapse=" + "))
  
  # Cast objective score wide by data_set.
  optimisation_table <- dcast(data=score_table[, mget(c(id_columns, "metric", "data_set", "objective_score"))],
                              formula,
                              value.var="objective_score")
  
  # Compute optimisation score based on objective scores.
  optimisation_table <- optimisation_table[, list("optimisation_score"=optimisation_function(training=training,
                                                                                             validation=validation)),
                                           by=c(id_columns, "metric")]
  
  # Average optimisation score over metrics.
  optimisation_table <- optimisation_table[, list("optimisation_score"=mean(optimisation_score, na.rm=TRUE)),
                                           by=id_columns]
  
  return(optimisation_table)
}



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



metric.optim_score.max_validation <- function(training=NULL, validation) return(validation)

metric.optim_score.balanced <- function(training, validation) return(validation - abs(validation - training))

metric.optim_score.stronger_balance <- function(training, validation) return(validation - 2.0 * abs(validation - training))

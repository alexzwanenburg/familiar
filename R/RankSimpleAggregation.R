# Simple ensemble aggregation, as in Wald R, Khoshgoftaar TM, Dittman D, Awada W, Napolitano A. An extensive comparison of feature ranking
# aggregation techniques in bioinformatics. 2012 IEEE 13th Int. Conf. Inf. Reuse Integr., IEEE; 2012, p. 377-84.

rank.mean <- function(dt){
  # Aggregate using mean rank

  # Suppress NOTES due to non-standard evaluation in data.table
  aggr_score <- NULL

  # Calculate mean rank
  dt_rank <- dt[, list(aggr_score=mean(rank)), by=c("name")][, "aggr_rank":=frank(aggr_score, ties.method="min")]

  # # Drop aggr_score column
  # dt_rank <- dt_rank[, "aggr_score":=NULL]

  return(dt_rank)
}


rank.median <- function(dt){
  # Aggregate using median rank

  # Suppress NOTES due to non-standard evaluation in data.table
  aggr_score <- NULL

  # Calculate median rank
  dt_rank <- dt[, list(aggr_score=stats::median(rank)), by=c("name")][, "aggr_rank":=data.table::frank(aggr_score, ties.method="min")]

  # # Drop aggr_score column
  # dt_rank <- dt_rank[, "aggr_score":=NULL]

  return(dt_rank)
}


rank.best_rank <- function(dt){
  # Aggregate using the best rank achieved

  # Suppress NOTES due to non-standard evaluation in data.table
  aggr_score <- NULL

  # Calculate best rank (i.e. lowest rank)
  dt_rank <- dt[, list(aggr_score=min(rank)), by=c("name")][, "aggr_rank":=data.table::frank(aggr_score, ties.method="min")]

  # # Drop aggr_score column
  # dt_rank <- dt_rank[, "aggr_score":=NULL]

  return(dt_rank)
}


rank.worst_rank <- function(dt){
  # Aggregate using the worst rank achieved

  # Suppress NOTES due to non-standard evaluation in data.table
  aggr_score <- NULL

  # Calculate worst rank (i.e. highest rank)
  dt_rank <- dt[, list(aggr_score=max(rank)), by=c("name")][, "aggr_rank":=data.table::frank(aggr_score, ties.method="min")]

  # # Drop aggr_score column
  # dt_rank <- dt_rank[, "aggr_score":=NULL]

  return(dt_rank)
}

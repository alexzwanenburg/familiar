# Simple ensemble aggregation, as in Wald R, Khoshgoftaar TM, Dittman D, Awada
# W, Napolitano A. An extensive comparison of feature ranking aggregation
# techniques in bioinformatics. 2012 IEEE 13th Int. Conf. Inf. Reuse Integr.,
# IEEE; 2012, p. 377-84.

rank.none <- function(vimp_table){
  # Aggregate using mean rank
  
  # Suppress NOTES due to non-standard evaluation in data.table
  aggr_score <- score <- NULL
  
  # Determine if higher scores are better
  higher_better <- sapply(split(vimp_table, by=c("data_id", "run_id")), function(x) (stats::cor(x=x$score, y=x$rank, method="spearman") < 0.0))
  if(all(is.na(higher_better))){
    higher_better <- FALSE
    
  } else {
    higher_better <- any(higher_better)
  }
  
  # Compute mean score
  rank_table <- vimp_table[, list(aggr_score=mean(score)), by=c("name")]
  
  if(higher_better){
    rank_table[, "aggr_rank":=data.table::frank(-aggr_score, ties.method="min")]
    
  } else {
    rank_table[, "aggr_rank":=data.table::frank(aggr_score, ties.method="min")]
  }
  
  return(rank_table)
}



rank.mean <- function(vimp_table){
  # Aggregate using mean rank

  # Suppress NOTES due to non-standard evaluation in data.table
  aggr_score <- NULL

  # Calculate mean rank
  rank_table <- vimp_table[, list(aggr_score=mean(rank)), by=c("name")][, "aggr_rank":=data.table::frank(aggr_score, ties.method="min")]

  return(rank_table)
}



rank.median <- function(vimp_table){
  # Aggregate using median rank

  # Suppress NOTES due to non-standard evaluation in data.table
  aggr_score <- NULL

  # Calculate median rank
  rank_table <- vimp_table[, list(aggr_score=stats::median(rank)), by=c("name")][, "aggr_rank":=data.table::frank(aggr_score, ties.method="min")]

  # # Drop aggr_score column
  # rank_table <- rank_table[, "aggr_score":=NULL]

  return(rank_table)
}


rank.best_rank <- function(vimp_table){
  # Aggregate using the best rank achieved

  # Suppress NOTES due to non-standard evaluation in data.table
  aggr_score <- NULL

  # Calculate best rank (i.e. lowest rank)
  rank_table <- vimp_table[, list(aggr_score=min(rank)), by=c("name")][, "aggr_rank":=data.table::frank(aggr_score, ties.method="min")]

  # # Drop aggr_score column
  # rank_table <- rank_table[, "aggr_score":=NULL]

  return(rank_table)
}


rank.worst_rank <- function(vimp_table){
  # Aggregate using the worst rank achieved

  # Suppress NOTES due to non-standard evaluation in data.table
  aggr_score <- NULL

  # Calculate worst rank (i.e. highest rank)
  rank_table <- vimp_table[, list(aggr_score=max(rank)), by=c("name")][, "aggr_rank":=data.table::frank(aggr_score, ties.method="min")]

  # # Drop aggr_score column
  # rank_table <- rank_table[, "aggr_score":=NULL]

  return(rank_table)
}

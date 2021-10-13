# Stability selection methods, as in Wald R, Khoshgoftaar TM, Dittman D, Awada
# W, Napolitano A. An extensive comparison of feature ranking aggregation
# techniques in bioinformatics. 2012 IEEE 13th Int. Conf. Inf. Reuse Integr.,
# IEEE; 2012, p. 377-84. These methods are based on Haury AC, Gestraud P, Vert
# JP. The influence of feature selection methods on accuracy, stability and
# interpretability of molecular signatures. PLoS One 2011;6:1-12.

rank.stability <- function(vimp_table, rank_threshold){
  # Aggregate using stability Meinshausen N, Bühlmann P. Stability selection. J
  # R Stat Soc Ser B (Statistical Methodol. 2010 Jul 5;72(4):417-73.

  # Suppress NOTES due to non-standard evaluation in data.table
  occurrence <- NULL

  # Calculate occurrence with a rank l.eq rank_threshold
  rank_table <- rank.get_feature_occurrence(vimp_table=vimp_table, threshold=rank_threshold, n_runs=data.table::uniqueN(vimp_table, by="run_id"))
  rank_table[, "aggr_rank":=data.table::frank(-occurrence, ties.method="min")]

  # Rename "occurrence column for consistency
  data.table::setnames(rank_table, "occurrence", "aggr_score")
  
  return(rank_table)
}



rank.exponential <- function(vimp_table, rank_threshold){
  # Aggregate using exponential weighted stability

  # Suppress NOTES due to non-standard evaluation in data.table
  aggr_score <- NULL

  # Calculate occurrence with a rank l.eq rank_threshold and weight by
  # exponential rank: better ranks receive higher scores
  rank_table <- vimp_table[, list(aggr_score=sum(exp(-rank/rank_threshold) * (rank <= rank_threshold))), by=c("name")]
  rank_table[, "aggr_rank":=data.table::frank(-aggr_score, ties.method="min")]

  return(rank_table)
}

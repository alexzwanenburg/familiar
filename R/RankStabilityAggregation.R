# Stability selection methods, as in Wald R, Khoshgoftaar TM, Dittman D, Awada
# W, Napolitano A. An extensive comparison of feature ranking aggregation
# techniques in bioinformatics. 2012 IEEE 13th Int. Conf. Inf. Reuse Integr.,
# IEEE; 2012, p. 377-84. These methods are based on Haury AC, Gestraud P, Vert
# JP. The influence of feature selection methods on accuracy, stability and
# interpretability of molecular signatures. PLoS One 2011;6:1-12.

.compute_rank_stability <- function(x, rank_threshold) {
  # Aggregate using stability Meinshausen N, Bühlmann P. Stability selection. J
  # R Stat Soc Ser B (Statistical Methodol. 2010 Jul 5;72(4):417-73.

  # Extract the variable importance table.
  vimp_table <- x@vimp_table

  # Compute the number of runs.
  n_runs <- data.table::uniqueN(vimp_table, by = "run_id")

  # Calculate occurrence with a rank l.eq rank_threshold
  vimp_table <- .compute_feature_occurrence(
    vimp_table = vimp_table,
    threshold = rank_threshold,
    n_runs = n_runs
  )

  # Rename "occurrence" column for consistency
  data.table::setnames(
    x = vimp_table,
    old = "occurrence",
    new = "score"
  )

  # Attach to vimpTable object.
  x@vimp_table <- vimp_table

  # Set correct invert value.
  x@invert <- TRUE

  return(x)
}



.compute_rank_exponential <- function(x, rank_threshold) {
  # Aggregate using exponential weighted stability

  # Suppress NOTES due to non-standard evaluation in data.table
  rank <- NULL

  # Calculate occurrence with a rank l.eq rank_threshold and weight by
  # exponential rank: better ranks receive higher scores
  x@vimp_table <- x@vimp_table[
    ,
    list("score" = sum(exp(-rank / rank_threshold) * (rank <= rank_threshold))),
    by = c("name")
  ]

  # Set correct invert value.
  x@invert <- TRUE

  return(x)
}

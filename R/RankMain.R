.get_available_rank_aggregation_methods <- function(){
  return(c("none",
           "mean",
           "median",
           "best",
           "worst",
           "stability",
           "exponential",
           "borda",
           "enhanced_borda",
           "truncated_borda",
           "enhanced_truncated_borda"))
}

rank.optimise_occurrence_threshold <- function(vimp_table){

  if(is(vimp_table, "vimpTable")) vimp_table <- vimp_table@vimp_table
  
  # Determine number of features, runs and the maximum rank
  n_features <- data.table::uniqueN(vimp_table, by="name")
  n_runs     <- data.table::uniqueN(vimp_table, by="run_id")
  max_rank   <- max(vimp_table$rank)

  # Check if there is only one feature or only one run
  if(n_features==1){ return(1) }
  if(n_runs==1){ return(max_rank) }

  # Set max threshold to either 50 or max_rank, if lower.
  max_threshold <- min(c(max_rank, 50))

  # We need to calculate for all possible cut-off thresholds which threshold maximises variance in occurrence
  threshold_var <- sapply(seq_len(max_threshold), function(threshold, n_runs, vimp_table) (stats::var(rank.get_feature_occurrence(vimp_table=vimp_table, n_runs=n_runs, threshold=threshold)$occurrence)),
                          vimp_table=vimp_table, n_runs=n_runs)

  # Return optimal threshold
  return(which.max(threshold_var))
}



rank.get_feature_occurrence <- function(vimp_table, n_runs, threshold){

  # Suppress NOTES due to non-standard evaluation in data.table
  name <- NULL

  # Make a local copy
  vimp_table <- data.table::copy(vimp_table)

  # Determine the occurrence for features with rank less or equal to the given threshold.
  vimp_table <- vimp_table[, list(occurrence=sum(rank <= threshold)/n_runs), by=name]

  return(vimp_table)
}

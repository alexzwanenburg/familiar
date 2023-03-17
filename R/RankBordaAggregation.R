.compute_rank_borda <- function(x,
                                rank_threshold,
                                truncated = FALSE,
                                enhanced = FALSE) {
  # Borda selection methods, as in Wald R, Khoshgoftaar TM, Dittman D, Awada W,
  # Napolitano A. An extensive comparison of feature ranking aggregation
  # techniques in bioinformatics. 2012 IEEE 13th Int. Conf. Inf. Reuse Integr.,
  # IEEE; 2012, p. 377-84.
  #
  # The Borda score is defined sum(N-r_l), with r the rank in list l (of length
  # N). If all lists have the same length, this devolves into a mean scoring.
  # The enhanced borda method resolves this issue by multiplying the borda score
  # for each feature by the feature occurrence. The truncated borda method only
  # assigns a non-zero value if the feature is within the top features as
  # indicated by rank_threshold. Higher scores signify better features.

  # Suppress NOTES due to non-standard evaluation in data.table
  score <- occurrence <- rank <- max_rank <- borda_score <- sum_score <- NULL

  # Extract the variable importance table.
  vimp_table <- x@vimp_table

  # Determine the number of runs
  n_runs <- data.table::uniqueN(vimp_table, by = "run_id")

  # Make a local copy to avoid changing the concatenated input rank table by
  # reference.
  borda_count_table <- data.table::copy(vimp_table)

  if (truncated) {
    # Keep only those features with a rank equal or lower than rank_threshold.

    # Compute a normalised Borda score, with maximum 1 and minimum
    # 1/rank_threshold. In effect, the best feature always receives a score of
    # 1, down to the feature with rank equal to rank_threshold 1/rank_threshold.
    # Any remaining features receive a score of 0.
    borda_count_table[
      rank <= rank_threshold,
      "borda_score" := (rank_threshold - rank + 1) / rank_threshold]
    borda_count_table[is.na(borda_score), "borda_score" := 0.0]
    
  } else {
    # If not truncated, compute the standard borda score according to Wald et
    # al. Find the maximum rank within each ranking list subset (identified by
    # run_id).
    borda_count_table[, "max_rank" := max(rank), by = "run_id"]

    # Compute a normalised Borda score, with maximum 1 and minimum 1/max_rank.
    borda_count_table[, "borda_score" := (max_rank - rank + 1) / max_rank]
  }

  # Sum all scores over the different lists
  borda_count_table <- borda_count_table[, list(
    sum_score = sum(borda_score)),
    by = "name"]

  if (enhanced) {
    # Enhanced borda methods use the occurrence (as in stability selection) for
    # weighting the borda score.
    occurrence_table <- rank.get_feature_occurrence(
      vimp_table = vimp_table,
      threshold = rank_threshold,
      n_runs = n_runs)

    # Merge tables by name
    vimp_table <- merge(
      x = borda_count_table,
      y = occurrence_table,
      all.x = FALSE,
      all.y = TRUE,
      by = c("name"))

    # Compute the enhanced borda score.
    vimp_table[, "score" := sum_score * occurrence]
    vimp_table[!is.finite(score), "score" := 0.0]
    
  } else {
    # No merging or further aggregation is required.
    vimp_table <- borda_count_table
    data.table::setnames(
      x = vimp_table, 
      old = "sum_score",
      new = "score")
  }

  # Attach to vimpTable object.
  x@vimp_table <- vimp_table[, mget(c("name", "score"))]

  # Set correct invert value.
  x@invert <- TRUE

  return(x)
}

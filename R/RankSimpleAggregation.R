# Simple ensemble aggregation, as in Wald R, Khoshgoftaar TM, Dittman D, Awada
# W, Napolitano A. An extensive comparison of feature ranking aggregation
# techniques in bioinformatics. 2012 IEEE 13th Int. Conf. Inf. Reuse Integr.,
# IEEE; 2012, p. 377-84.

.compute_rank_mean_score <- function(x) {
  # Aggregate using mean score.

  # Suppress NOTES due to non-standard evaluation in data.table
  score <- NULL

  # Compute mean score
  x@vimp_table <- x@vimp_table[, list("score" = mean(score)), by = c("name")]

  return(x)
}



.compute_rank_mean_rank <- function(x) {
  # Aggregate using mean rank

  # Suppress NOTES due to non-standard evaluation in data.table
  rank <- NULL

  # Calculate mean rank
  x@vimp_table <- x@vimp_table[, list("score" = mean(rank)), by = c("name")]

  # Set correct invert value.
  x@invert <- FALSE

  return(x)
}



.compute_rank_median_rank <- function(x) {
  # Aggregate using median rank

  # Suppress NOTES due to non-standard evaluation in data.table
  rank <- NULL

  # Calculate median rank
  x@vimp_table <- x@vimp_table[, list("score" = as.double(stats::median(rank))), by = c("name")]

  # Set correct invert value.
  x@invert <- FALSE

  return(x)
}



.compute_rank_best_rank <- function(x) {
  # Aggregate using the best rank achieved.

  # Suppress NOTES due to non-standard evaluation in data.table
  rank <- NULL

  # Calculate best rank (i.e. lowest rank)
  x@vimp_table <- x@vimp_table[, list("score" = min(rank)), by = c("name")]

  # Set correct invert value.
  x@invert <- FALSE

  return(x)
}



.compute_rank_worst_rank <- function(x) {
  # Aggregate using the worst rank achieved

  # Suppress NOTES due to non-standard evaluation in data.table
  rank <- NULL

  # Calculate worst rank (i.e. highest rank)
  x@vimp_table <- x@vimp_table[, list("score" = max(rank)), by = c("name")]

  # Set correct invert value.
  x@invert <- FALSE

  return(x)
}

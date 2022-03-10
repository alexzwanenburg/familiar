rank.get_vimp_table <- function(run, fs_method, proj_list, file_paths, decluster=FALSE){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  data_id <- run_id <- NULL
  
  # Find and load vimp list.
  vimp_list <- tryCatch(readRDS(.get_feature_selection_data_filename(proj_list=proj_list,
                                                                     fs_method=fs_method,
                                                                     file_paths=file_paths)),
                        error=identity)
  
  if(inherits(vimp_list, "error")) return(NULL)
  
  # Try and identify matching data and run ids between vimp_list entries and run$run_table
  # Start at the bottom and try to match.
  for(ii in rev(run$run_table$perturb_level)){
    run_id_list <- .get_iteration_identifiers(run=run, perturb_level=ii)
    
    # Check whether there are any matching data and run ids by determining the number of rows in the table after matching
    match_vimp <- sapply(vimp_list, function(iter_vimp, run_id_list) (nrow(iter_vimp$run_table[data_id==run_id_list$data & run_id==run_id_list$run])>0),
                         run_id_list=run_id_list)
    
    # If there is any match, we break from the loop
    if(any(match_vimp)){
      break()
    }
  }
  
  # Shrink vimp list to only those matching
  vimp_list <- vimp_list[match_vimp]
  
  # Combine all vimp tables in the vimp list
  if(decluster){
    # Perform declustering if required (this is required when creating ensemble model lists)
    
    # Decluster features before processing
    vimp_list <- lapply(vimp_list, function(list_elem) {
      
      # Obtain the list with featureInfo objects
      feature_info_list <- get_feature_info_list(run=list_elem)
      
      # Decluster the vimp table
      vimp_table <- rank.decluster_vimp_table(vimp_table=list_elem$vimp,
                                              translation_table=list_elem$translation_table,
                                              feature_info_list=feature_info_list)
      
      return(vimp_table)
    })
    
    # Combine into single list
    vimp_table <- data.table::rbindlist(vimp_list, use.names=TRUE)
    
  } else {
    # Combine into a single list.
    vimp_table <- data.table::rbindlist(lapply(vimp_list, function(iter_vimp) (iter_vimp$vimp)), use.names=TRUE)
  }
  
  return(vimp_table)
}


rank.get_feature_ranks <- function(run=NULL,
                                   fs_method=NULL,
                                   settings=NULL,
                                   proj_list=NULL,
                                   file_paths=NULL,
                                   decluster=FALSE,
                                   rank_threshold=NULL,
                                   aggregation_method=NULL,
                                   translation_table=NULL,
                                   vimp_table=NULL){
  
  if(is.null(settings)) settings <- get_settings()
  if(is.null(proj_list)) proj_list <- get_project_list()
  if(is.null(file_paths)) file_paths <- get_file_paths()
  
  # Obtain the variable importance table for the current run from the file
  # system.
  if(is.null(vimp_table)){
    vimp_table <- rank.get_vimp_table(run=run,
                                      fs_method=fs_method,
                                      proj_list=proj_list,
                                      file_paths=file_paths,
                                      decluster=decluster)
  }
  
  # Combine all vimp tables in the vimp list
  if(decluster){
    if(is.null(translation_table)){
      # Perform consensus clustering on the table. This results into a
      # translation table. This translation table enables us to translate
      # between likely clusters and individual features over the entire dataset
      # under evaluation here.
      translation_table <- rank.consensus_clustering(vimp_table=vimp_table)
    }
    
    # Re-cluster variables so that each cluster of features will only be counted
    # once. This is particularly helpful for features that incorporate
    # occurrence or use a threshold for the important set of features.
    vimp_table <- rank.recluster_vimp_table(vimp_table=vimp_table,
                                            translation_table=translation_table)
    
  } else {
    # Create an empty translation table
    translation_table <- rank.consensus_clustering(vimp_table=NULL)
  }
  
  if(is_empty(vimp_table)){
    # Create an empty ranking table
    empty_ranking_table <- data.table::data.table("name"=character(0),
                                                  "aggr_score"=numeric(0),
                                                  "aggr_rank"=integer(0))
    
    # Add cluster identifier column, if required.
    if(decluster) {
      empty_ranking_table$cluster_id <- integer(0)
    }
    
    return(empty_ranking_table)
    
  } else {
    # Read aggregation_method and threshold from settings
    if(!is.null(settings)){
      aggregation_method <- settings$fs$aggregation
      rank_threshold <- settings$fs$aggr_rank_threshold
    }
    
    # Do not aggregate data in case only one dataset is assessed.
    if(data.table::uniqueN(vimp_table, by=c("data_id", "run_id")) == 1){
      aggregation_method <- "none"
    }
    
    # Compute aggregated ranks.
    ranking_table <- rank.aggregate_feature_ranks(vimp_table=vimp_table,
                                                  rank_threshold=rank_threshold,
                                                  aggregation_method=aggregation_method)
    
    if(decluster){
      # Turn the clustered features into individual features.
      ranking_table <- rank.decluster_vimp_table(vimp_table=ranking_table,
                                                 translation_table=translation_table)
    }
    
    return(ranking_table)
  }
}



rank.check_aggregation_method <- function(method){
  .check_parameter_value_is_valid(x=method, var_name="aggregation method",
                                  values=c("none",
                                           "mean",
                                           "median",
                                           "best",
                                           "worst",
                                           "stability",
                                           "exponential",
                                           "borda",
                                           "enhanced_borda",
                                           "truncated_borda",
                                           "enhanced_truncated_borda")
  )
}



rank.optimise_occurrence_threshold <- function(vimp_table){
  
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



rank.aggregate_feature_ranks <- function(vimp_table, rank_threshold=NULL, aggregation_method=NULL){
  
  # Make a local copy of vimp_table
  vimp_table <- data.table::copy(vimp_table)
  
  # Convert the rank column to double
  vimp_table$rank <- as.numeric(vimp_table$rank)
  
  if(aggregation_method == "none"){
    rank_table <- rank.none(vimp_table=vimp_table)
  }
  
  # Perform aggregation using simple ensemble methods
  if(aggregation_method %in% c("mean", "median", "best", "worst")){
    
    if(aggregation_method == "mean"){
      rank_table <- rank.mean(vimp_table=vimp_table)
      
    } else if(aggregation_method == "median"){
      rank_table <- rank.median(vimp_table=vimp_table)
      
    } else if(aggregation_method == "best"){
      rank_table <- rank.best_rank(vimp_table=vimp_table)
      
    } else if(aggregation_method == "worst"){
      rank_table <- rank.worst_rank(vimp_table=vimp_table)
    } 
  }
  
  # Perform aggregation using stability-based methods
  if(aggregation_method %in% c("stability", "exponential")){
    
    if(is.null(rank_threshold)) {
      rank_threshold <- rank.optimise_occurrence_threshold(vimp_table=vimp_table)
    }
    
    if(aggregation_method == "stability"){
      rank_table <- rank.stability(vimp_table=vimp_table,
                                   rank_threshold=rank_threshold)
      
    } else if(aggregation_method == "exponential"){
      rank_table <- rank.exponential(vimp_table=vimp_table,
                                     rank_threshold=rank_threshold)
    } 
  }
  
  # Perform aggregation using Borda-count based methods
  if(aggregation_method %in% c("borda", "enhanced_borda", "truncated_borda", "enhanced_truncated_borda")){
    
    if(is.null(rank_threshold)) {
      rank_threshold <- rank.optimise_occurrence_threshold(vimp_table=vimp_table)
    }
    
    if(aggregation_method == "borda") {
      rank_table <- rank.borda_aggregation(vimp_table=vimp_table,
                                           rank_threshold=rank_threshold,
                                           truncated=FALSE,
                                           enhanced=FALSE)
      
    } else if(aggregation_method == "enhanced_borda") {
      rank_table <- rank.borda_aggregation(vimp_table=vimp_table,
                                           rank_threshold=rank_threshold,
                                           truncated=FALSE,
                                           enhanced=TRUE)
      
    } else if(aggregation_method == "truncated_borda") {
      rank_table <- rank.borda_aggregation(vimp_table=vimp_table,
                                           rank_threshold=rank_threshold,
                                           truncated=TRUE,
                                           enhanced=FALSE)
      
    } else if(aggregation_method == "enhanced_truncated_borda") {
      rank_table <- rank.borda_aggregation(vimp_table=vimp_table,
                                           rank_threshold=rank_threshold,
                                           truncated=TRUE,
                                           enhanced=TRUE)
      
    } else {
      ..error_reached_unreachable_code("rank.aggregate_feature_ranks_unknown_borda_aggregation_method")
    }
  }
  
  return(rank_table)
}


rank.decluster_vimp_table <- function(vimp_table, feature_info_list=NULL, translation_table=NULL){
  
  if(is_empty(vimp_table)){
    # An empty table exists for "random" or "none" feature selection methods,
    # and may also occur under other circumstances such as models that failed to
    # train.
    
    # Add an (empty) cluster_id column
    vimp_table[, "cluster_id":=.I]
    
    data.table::setcolorder(vimp_table, "name")
    return(vimp_table)
  }
  
  # Obtain a translation table
  if(is.null(translation_table) & is.null(feature_info_list)){
    ..error_reached_unreachable_code("rank.decluster_vimp_table_no_feature_info_list")
    
  } else if(is.null(translation_table)){
    translation_table <- rank.get_decluster_translation_table(features=vimp_table$name,
                                                              feature_info_list=feature_info_list)
  }
  
  # Local copy of the translation table.
  translation_table <- data.table::copy(translation_table)
  
  # Add cluster identifier, if not present.
  if(!"cluster_id" %in% colnames(translation_table)){
    translation_table[, "cluster_id":=.GRP, by="cluster_name"]
  }
  
  # Merge with the vimp table
  vimp_table <- merge(x=vimp_table, y=translation_table, by.x="name", by.y="cluster_name")
  
  # Drop "name" column and rename the "feature_name" column to "name"
  vimp_table[, "name":=NULL]
  data.table::setnames(vimp_table, "feature_name", "name")
  data.table::setcolorder(vimp_table, "name")
  
  return(vimp_table)
}


rank.recluster_vimp_table <- function(vimp_table, translation_table=NULL){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  score <- rank <- multi_var <- NULL
  
  # Remove (empty) cluster_id column, if present.
  if("cluster_id" %in% colnames(vimp_table)){
    vimp_table[, "cluster_id":=NULL]
  }
  
  if(is_empty(vimp_table)){
    # An empty table exists for "random" or "none" feature selection methods,
    # and may also occur under other circumstances such as models that failed to
    # train.
    return(vimp_table)
  }
  
  # If there is no translation_table, assume that all 
  if(is_empty(translation_table)){
    return(vimp_table)
  }
  
  # Local copy of the translation table.
  translation_table <- data.table::copy(translation_table)
  
  # Merge with the vimp table
  vimp_table <- merge(x=vimp_table,
                      y=translation_table,
                      by.x="name",
                      by.y="feature_name")
  
  # Concatenate by cluster_name, 
  vimp_table <- vimp_table[, list("score"=mean(score),
                                  "rank"=min(rank),
                                  "multi_var"=max(multi_var)),
                           by=c("cluster_name", "data_id", "run_id")]
  
  # Drop "name" column and rename the "new_name" column to "name"
  data.table::setnames(vimp_table, "cluster_name", "name")
  data.table::setcolorder(vimp_table, "name")
  
  return(vimp_table)
}



rank.get_decluster_translation_table <- function(features, feature_info_list){
  
  # Check if there are any features that should be incorporated in the
  # translation table.
  if(length(features) == 0){
    return(data.table::data.table("cluster_name"=character(0), "feature_name"=character(0)))
  }
  
  # Find the cluster_table that can be used to decluster the values
  cluster_table <- get_cluster_table(feature_info_list=feature_info_list)
  
  # Iterate over feature names in the vimp table
  translation_table <- data.table::rbindlist(lapply(features, function(feature, cluster_table){
    
    # Find name of underlying features
    translation_table <- data.table::data.table("cluster_name" = feature,
                                                "feature_name" = features_before_clustering(features=feature, cluster_table=cluster_table))
    
    return(translation_table)
  }, cluster_table=cluster_table))
  
  return(translation_table)
}



#' @title Consensus clustering for (co-clustered) features in different runs
#'
#' @param vimp_table A data.table with variable importance information over all
#'   the runs and cluster identifiers (`cluster_id`).
#'
#' @return A table to translate between features and clustered features. This
#'   table contains three columns: `feature_name` (name of the feature), `cluster_name`
#'   (name of the cluster), and `cluster_id` (cluster identifier).
#' @noRd
rank.consensus_clustering <- function(vimp_table){
  # Performs consensus clustering and return a table that can be 
  
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- is_co_clustered <- n_co_clustered <- n_total <- cluster_size <- cluster_repr_feature <- NULL
  
  # Check if the vimp_table is empty.
  if(is_empty(vimp_table)){
    return(data.table::data.table("feature_name"=character(0),
                                  "cluster_name"=character(0),
                                  "cluster_id"=character(0)))
  }
  
  # Find unique features
  features <- unique(vimp_table$name)
  
  if(length(features) == 1){
    # Generate cluster_table with only singular clusters
    cluster_table <- data.table::data.table("feature_name"=features,
                                            "cluster_name"=features)
    
    # Add in cluster identifiers
    cluster_table[, "cluster_id":=.I]
    
    return(cluster_table)
  }
  
  # Get grouping columns
  grouping_columns <- c("data_id", "run_id", "model_name")
  grouping_columns <- intersect(colnames(vimp_table),
                                grouping_columns)
  
  # Construct list of tables with co-clustered pairs.
  co_cluster_list <- lapply(split(vimp_table, by=c(grouping_columns)), function(vimp_table){
    
    # Find features in the current table
    features <- vimp_table$name
    
    # Check if there are any feature pairs present in the data.
    if(length(features) < 2){
      return(data.table::data.table("feature_name_1"=character(0),
                                    "feature_name_2"=character(0),
                                    "is_co_clustered"=logical(0)))
    }
    
    # Create all combinations of features.
    combinations <- utils::combn(seq_along(features), 2)
    
    # Find cluster identifiers that are the same, and thus indicate co-clustered
    # features.
    is_co_clustered <- sapply(seq_len(ncol(combinations)), function(ii, combinations, x){
      return(x[combinations[1, ii]] == x[combinations[2, ii]])
    }, combinations=combinations, x=vimp_table$cluster_id)
    
    co_cluster_table <- data.table::data.table("feature_name_1"=features[combinations[1, ]],
                                               "feature_name_2"=features[combinations[2, ]],
                                               "is_co_clustered"=is_co_clustered)
    
    return(co_cluster_table)
  })
  
  # Combine list of tables into a single table.
  co_cluster_table <- data.table::rbindlist(co_cluster_list, use.names=TRUE)
  
  # Check if any pairs have been formed.
  if(is_empty(co_cluster_table)){
    # Generate cluster_table with only singular clusters
    cluster_table <- data.table::data.table("feature_name"=features,
                                            "cluster_name"=features)
    
    # Add in cluster identifiers
    cluster_table[, "cluster_id":=.I]
    
    return(cluster_table)
    
  }
  
  # Summarise data.
  co_cluster_table <- co_cluster_table[, list("n_co_clustered"=sum(is_co_clustered),
                                              "n_total"=.N),
                                       by=c("feature_name_1", "feature_name_2")]
  
  # Bind with the same table, but with columns inverted. This is done because
  # pairs A and B and B and A are actually the same, but this is not accounted
  # for in the previous table.
  co_cluster_table <- rbind(co_cluster_table,
                            data.table::data.table("feature_name_1"=co_cluster_table$feature_name_2,
                                                   "feature_name_2"=co_cluster_table$feature_name_1,
                                                   "n_co_clustered"=co_cluster_table$n_co_clustered,
                                                   "n_total"=co_cluster_table$n_total))
  
  # Summarise data again.
  co_cluster_table <- co_cluster_table[, list("n_co_clustered"=sum(n_co_clustered),
                                              "n_total"=sum(n_total)),
                                       by=c("feature_name_1", "feature_name_2")]
  
  # Find distance as 1 minus the fraction of co-cluster occurrence. This means
  # that features with pairwise distance 0.0 are always found together, whereas
  # features with pairwise distance 1.0 are never in the same cluster.
  co_cluster_table[, "value":=1 - n_co_clustered/n_total]
  
  # Drop columns which are now longer used: n_co_clustered, n_total
  co_cluster_table[, ":="("n_co_clustered"=NULL, "n_total"=NULL)]
  
  # Add in diagonal (feature_name_1 == feature_name_2). These always have distance 0.0.
  co_cluster_table <- rbind(co_cluster_table,
                            data.table::data.table("feature_name_1"=features,
                                                   "feature_name_2"=features,
                                                   "value"=0.0))
  
  # Remove any duplicate entries introduced by adding the diagonal.
  co_cluster_table <- co_cluster_table[,
                                       list("value"=min(value)),
                                       by=c("feature_name_1", "feature_name_2")]
  
  # Create n x n table
  co_cluster_table  <- data.table::dcast(co_cluster_table,
                                         feature_name_1 ~ feature_name_2,
                                         value.var="value")
  
  rownames(co_cluster_table) <- co_cluster_table$feature_name_1
  co_cluster_table[, "feature_name_1":=NULL]
  
  # Create a distance matrix, and set missing pairs to 1.0.
  co_cluster_table[is.na(co_cluster_table)] <- 1.0
  
  if(length(features) >= 2){
    # Obtain a cluster table using hierarchical clustering. We will consider
    # features co-clustered if they are found together 8/10 times.
    cluster_table <- cluster.get_cluster_table(require_representation=TRUE,
                                               distance_matrix=stats::as.dist(co_cluster_table),
                                               cluster_method="hclust",
                                               cluster_linkage="average",
                                               cluster_cut_method="fixed_cut",
                                               cluster_similarity_threshold=0.8,
                                               cluster_similarity_metric="pearson",
                                               cluster_representation_method="first")
    
    # Set cluster names for clustered features
    cluster_table[cluster_size == 1, "cluster_name":=cluster_repr_feature]
    cluster_table[cluster_size > 1, "cluster_name":=paste0(cluster_repr_feature, "_cluster")]
    
    # Only keep relevant columns.
    cluster_table <- cluster_table[, c("name", "cluster_name", "cluster_id"), with=FALSE]
    
  }  else {
    ..error_reached_unreachable_code("rank.consensus_clustering: fewer than 2 features found.")
  }
  
  data.table::setnames(cluster_table, old="name", new="feature_name")
  
  return(cluster_table)
}

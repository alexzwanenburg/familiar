
#'@title Internal function to extract permutation variable importance.
#'
#'@description Computes and collects permutation variable importance from a
#'  `familiarEnsemble`.
#'
#'@inheritParams extract_data
#'
#'@details This function also computes credibility intervals for the ensemble
#'  model, at the level of `confidence_level`.
#'
#'@return A list with data.tables for single and ensemble model assessments.
#'@md
#'@keywords internal
setGeneric("extract_permutation_vimp",
           function(object,
                    data,
                    cl=NULL,
                    ensemble_method=waiver(),
                    feature_similarity_table,
                    feature_cluster_method=waiver(),
                    feature_linkage_method=waiver(),
                    feature_cluster_cut_method=waiver(),
                    feature_similarity_metric=waiver(),
                    feature_similarity_threshold=waiver(),
                    metric=waiver(),
                    eval_times=waiver(),
                    confidence_level=waiver(),
                    bootstrap_ci_method=waiver(),
                    compute_model_data=waiver(),
                    compute_model_ci=waiver(),
                    compute_ensemble_ci=waiver(),
                    aggregate_ci=waiver(),
                    is_pre_processed=FALSE,
                    message_indent=0L,
                    verbose=FALSE,
                    ...) standardGeneric("extract_permutation_vimp"))

#####extract_permutation_vimp#####
setMethod("extract_permutation_vimp", signature(object="familiarEnsemble"),
          function(object,
                   data,
                   cl=NULL,
                   ensemble_method=waiver(),
                   feature_similarity_table,
                   feature_cluster_method=waiver(),
                   feature_linkage_method=waiver(),
                   feature_cluster_cut_method=waiver(),
                   feature_similarity_metric=waiver(),
                   feature_similarity_threshold=waiver(),
                   metric=waiver(),
                   eval_times=waiver(),
                   confidence_level=waiver(),
                   bootstrap_ci_method=waiver(),
                   compute_model_data=waiver(),
                   compute_model_ci=waiver(),
                   compute_ensemble_ci=waiver(),
                   aggregate_ci=waiver(),
                   is_pre_processed=FALSE,
                   message_indent=0L,
                   verbose=FALSE,
                   ...){

            # Message extraction start
            if(verbose){
              logger.message(paste0("Computing permutation variable importance for models in the dataset."),
                             indent=message_indent)
            }
            
            # Load eval_times from the object settings attribute, if it is not provided.
            if(is.waive(eval_times)) eval_times <- object@settings$eval_times
            
            # Check eval_times argument
            if(object@outcome_type %in% c("survival")){
              sapply(eval_times, .check_number_in_valid_range, var_name="eval_times", range=c(0.0, Inf), closed=c(FALSE, TRUE))
            }
            
            # Obtain ensemble method from stored settings, if required.
            if(is.waive(ensemble_method)) ensemble_method <- object@settings$ensemble_method
            
            # Load confidence alpha from object settings attribute if not
            # provided externally.
            if(is.waive(confidence_level)) confidence_level <- object@settings$confidence_level
            
            # Check confidence_level input argument
            .check_number_in_valid_range(x=confidence_level, var_name="confidence_level",
                                         range=c(0.0, 1.0), closed=c(FALSE, FALSE))
            
            # Check ensemble_method argument
            .check_parameter_value_is_valid(x=ensemble_method, var_name="ensemble_method",
                                            values=.get_available_ensemble_prediction_methods())
            
            # Load the bootstrap method
            if(is.waive(bootstrap_ci_method)) bootstrap_ci_method <- object@settings$bootstrap_ci_method
            
            .check_parameter_value_is_valid(x=bootstrap_ci_method, var_name="bootstrap_ci_methpd",
                                            values=.get_available_bootstrap_confidence_interval_methods())
            
            # By default, compute confidence intervals for ensembles, but not
            # for models.
            if(is.waive(compute_model_data)) compute_model_data <- "none"
            if(is.waive(compute_model_ci)) compute_model_ci <- "none"
            if(is.waive(compute_ensemble_ci)) compute_ensemble_ci <- "all"
            if(is.waive(aggregate_ci)) aggregate_ci <- "all"
            
            # Load metric(s) from the object settings attribute if not provided
            # externally.
            if(is.waive(metric)) metric <- object@settings$metric

            # Check metric input argument
            sapply(metric, metric.check_outcome_type, object=object)
            
            # Obtain cluster method from stored settings, if required.
            if(is.waive(feature_cluster_method)) feature_cluster_method <- object@settings$feature_cluster_method
            
            # Obtain linkage function from stored settings, if required.
            if(is.waive(feature_linkage_method)) feature_linkage_method <- object@settings$feature_linkage_method

            # Obtain feature cluster cut method from stored settings, if required.
            if(is.waive(feature_cluster_cut_method)) feature_cluster_cut_method <- object@settings$feature_cluster_cut_method
            
            # Obtain cluster similarity threshold from stored settings, if required.
            if(is.waive(feature_similarity_threshold)) feature_similarity_threshold <- object@settings$feature_similarity_threshold

            # Obtain similarity metric from stored settings, if required.
            if(is.waive(feature_similarity_metric)) feature_similarity_metric <- object@settings$feature_similarity_metric

            # Replace feature cluster method == "none" with "hclust"
            if(feature_cluster_method == "none") feature_cluster_method <- "hclust"
            
            .check_cluster_parameters(cluster_method=feature_cluster_method,
                                      cluster_linkage=feature_linkage_method,
                                      cluster_cut_method=feature_cluster_cut_method,
                                      cluster_similarity_threshold=feature_similarity_threshold,
                                      cluster_similarity_metric=feature_similarity_metric,
                                      var_type="feature")
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)) ..error_ensemble_models_not_loaded()
            
            # Extract data for the individual models and the ensemble.
            vimp_data <- universal_extractor(object=object,
                                             cl=cl,
                                             FUN=.extract_permutation_vimp,
                                             data=data,
                                             is_pre_processed=is_pre_processed,
                                             ensemble_method=ensemble_method,
                                             metric=metric,
                                             eval_times=eval_times,
                                             confidence_level=confidence_level,
                                             compute_model_data=any(c("all", "permutation_vimp", "TRUE") %in% compute_model_data),
                                             compute_model_ci=any(c("all", "permutation_vimp", "TRUE") %in% compute_model_ci),
                                             compute_ensemble_ci=any(c("all", "permutation_vimp", "TRUE") %in% compute_ensemble_ci),
                                             aggregate_ci=any(c("all", "permutation_vimp", "TRUE") %in% aggregate_ci),
                                             bootstrap_ci_method=bootstrap_ci_method,
                                             similarity_table=feature_similarity_table,
                                             cluster_method=feature_cluster_method,
                                             cluster_linkage=feature_linkage_method,
                                             cluster_cut_method=feature_cluster_cut_method,
                                             cluster_similarity_threshold=feature_similarity_threshold,
                                             cluster_similarity_metric=feature_similarity_metric,
                                             message_indent=message_indent + 1L,
                                             verbose=verbose)
            
            return(vimp_data)
          })



.extract_permutation_vimp <- function(cl,
                                      object,
                                      data,
                                      is_pre_processed,
                                      eval_times,
                                      ensemble_method,
                                      metric,
                                      determine_ci,
                                      bootstrap_ci_method,
                                      aggregate_ci,
                                      confidence_level,
                                      similarity_table,
                                      cluster_method,
                                      cluster_linkage,
                                      cluster_cut_method,
                                      cluster_similarity_threshold,
                                      cluster_similarity_metric,
                                      verbose,
                                      message_indent){
  
  if(object@outcome_type %in% c("survival")){
    # Compute permutation variable importance data at each of the evaluation time points.
    vimp_data <- lapply(eval_times,
                        ..extract_permutation_vimp,
                        object=object,
                        data=data,
                        cl=cl,
                        metric=metric,
                        is_pre_processed=is_pre_processed,
                        ensemble_method=ensemble_method,
                        determine_ci=determine_ci,
                        confidence_level=confidence_level,
                        similarity_table=similarity_table,
                        cluster_method=cluster_method,
                        cluster_linkage=cluster_linkage,
                        cluster_cut_method=cluster_cut_method,
                        cluster_similarity_threshold=cluster_similarity_threshold,
                        cluster_similarity_metric=cluster_similarity_metric,
                        verbose=verbose,
                        message_indent=message_indent)
    
    # Concatenate lists.
    vimp_data <- list("model_data"=rbind_list_list(vimp_data, "model_data"),
                      "bootstrap_data"=rbind_list_list(vimp_data, "bootstrap_data"),
                      "confidence_level"=confidence_level)
    
  } else {
    # Compute permutation variable importance data.
    vimp_data <- ..extract_permutation_vimp(object=object,
                                            data=data,
                                            is_pre_processed=is_pre_processed,
                                            cl=cl,
                                            metric=metric,
                                            ensemble_method=ensemble_method,
                                            determine_ci=determine_ci,
                                            confidence_level=confidence_level,
                                            similarity_table=similarity_table,
                                            cluster_method=cluster_method,
                                            cluster_linkage=cluster_linkage,
                                            cluster_cut_method=cluster_cut_method,
                                            cluster_similarity_threshold=cluster_similarity_threshold,
                                            cluster_similarity_metric=cluster_similarity_metric,
                                            verbose=verbose,
                                            message_indent=message_indent)
  }
  
  if(determine_ci & aggregate_ci){
    
    # Aggregate the data by computing the bootstrap confidence intervals.
    vimp_data$model_data <- .compute_bootstrap_ci(x0=vimp_data$model_data,
                                                  xb=vimp_data$bootstrap_data,
                                                  target_column="value",
                                                  bootstrap_ci_method=bootstrap_ci_method,
                                                  additional_splitting_variable=c("metric", "feature", "similarity_threshold"),
                                                  confidence_level=confidence_level,
                                                  cl=cl,
                                                  verbose=verbose,
                                                  message_indent=message_indent)
    
    # Set the bootstrap_data to NULL.
    vimp_data$bootstrap_data <- NULL
    
  } else if(determine_ci){
    # Add the bootstrap confidence interval method
    vimp_data$bootstrap_ci_method <- bootstrap_ci_method
  }
  
  # Add the similarity metric, if appropriate.
  if(cluster_method %in% c("agnes", "diana", "hclust") & cluster_cut_method == "fixed_cut"){
    vimp_data$similarity_metric=cluster_similarity_metric
  }
  
  return(vimp_data)
}



..extract_permutation_vimp <- function(eval_times=NULL,
                                       object,
                                       data,
                                       is_pre_processed,
                                       cl,
                                       metric,
                                       n_reshuffle=21,
                                       ensemble_method,
                                       determine_ci,
                                       confidence_level,
                                       similarity_table,
                                       cluster_method,
                                       cluster_linkage,
                                       cluster_cut_method,
                                       cluster_similarity_threshold,
                                       cluster_similarity_metric,
                                       verbose,
                                       message_indent){

  # Perform a safety check.
  if(length(eval_times) > 1) ..error_reached_unreachable_code("..extract_permutation_vimp: more than one value for eval_times")
  
  # Load input data. We stop at the signature step because we want to work with
  # the unclustered input features, but may need to apply model-specific
  # preprocessing steps later on.
  data <- process_input_data(object=object,
                             data=data,
                             is_pre_processed=is_pre_processed,
                             stop_at="signature")
  
  # Perform some checks to see if there is sufficient data to perform a half-way
  # meaningful analysis.
  if(is_empty(data)) return(NULL)
  if(nrow(data@data) < 5) return(NULL)
  
  # Maintain only important features. The current set is based on the
  # required features.
  data <- filter_features(data=data,
                          available_features=object@model_features)
  
  # Derive feature information
  feature_cluster_info <- .select_feature_clusters(available_features=get_feature_columns(data),
                                                   similarity_table=similarity_table,
                                                   cluster_method=cluster_method,
                                                   cluster_linkage=cluster_linkage,
                                                   cluster_cut_method=cluster_cut_method,
                                                   cluster_similarity_threshold=cluster_similarity_threshold,
                                                   cluster_similarity_metric=cluster_similarity_metric)
  
  # Iterate over features.
  vimp_data <-lapply(feature_cluster_info$feature_clusters, function(shuffled_features,
                                                                     object,
                                                                     data,
                                                                     cl,
                                                                     confidence_level,
                                                                     determine_ci,
                                                                     time,
                                                                     metric,
                                                                     n_reshuffle,
                                                                     ensemble_method,
                                                                     similarity_threshold,
                                                                     message_indent,
                                                                     verbose){
    
    # Identify the similarity threshold for which the current feature sets form
    # a cluster.
    n <- shuffled_features$n_thresholds_same_cluster[1]
    similarity_threshold_used <- head(similarity_threshold[similarity_threshold <= shuffled_features$similarity_threshold[1]], n=n)
    
    # Compute the point estimate for permutation vimp.
    vimp_data <- .compute_permutation_vimp(shuffled_features=shuffled_features$name,
                                           object=object,
                                           data=data,
                                           time=time,
                                           metric=metric,
                                           n_reshuffle=n_reshuffle,
                                           similarity_threshold=similarity_threshold_used,
                                           ensemble_method=ensemble_method)
    
    if(determine_ci){
      if(verbose & !is.null(time)) logger.message(paste0("Computing bootstrap confidence interval data for variable importance of the ",
                                                         paste_s(shuffled_features$name), ifelse(length(shuffled_features$name) == 1, " feature", " features"),
                                                         " at time ", time, "."),
                                                  indent=message_indent)
      if(verbose & is.null(time)) logger.message(paste0("Computing bootstrap confidence interval data for variable importance of the ",
                                                        paste_s(shuffled_features$name), ifelse(length(shuffled_features$name) == 1, " feature", " features"),
                                                        "."),
                                                 indent=message_indent)
      
      # Bootstrap the data.
      bootstrap_data <- bootstrapper(data=data,
                                     alpha= 1.0 - confidence_level,
                                     FUN=.compute_permutation_vimp,
                                     shuffled_features=shuffled_features$name,
                                     object=object,
                                     time=time,
                                     metric=metric,
                                     n_reshuffle=1L,
                                     similarity_threshold=similarity_threshold_used,
                                     ensemble_method=ensemble_method,
                                     cl=cl,
                                     verbose=verbose)
      
    } else {
      bootstrap_data <- NULL
    }
    
    return(list("model_data"=vimp_data,
                "bootstrap_data"=bootstrap_data))
  },
  object=object,
  data=data,
  cl=cl,
  confidence_level=confidence_level,
  determine_ci=determine_ci,
  time=eval_times,
  metric=metric,
  n_reshuffle=n_reshuffle,
  ensemble_method=ensemble_method,
  similarity_threshold=feature_cluster_info$similarity_threshold,
  message_indent=message_indent,
  verbose=verbose)
  
  # Concatenate lists.
  vimp_data <- list("model_data"=rbind_list_list(vimp_data, "model_data"),
                    "bootstrap_data"=rbind_list_list(vimp_data, "bootstrap_data"),
                    "confidence_level"=confidence_level)
  
  return(vimp_data)
}



.compute_permutation_vimp <- function(shuffled_features,
                                      object,
                                      data,
                                      time,
                                      metric,
                                      n_reshuffle,
                                      similarity_threshold,
                                      ensemble_method){
 
  # Make the unshuffled prediction.
  unshuffled_prediction <- .predict(object=object,
                                    data=data,
                                    time=time,
                                    ensemble_method=ensemble_method)
  
  # Check if any predictions are valid.
  if(!any_predictions_valid(unshuffled_prediction, outcome_type=object@outcome_type)) return(NULL)
  
  # Compute the unshuffled metrics.
  unshuffled_metrics <- lapply(metric,
                               ..compute_model_performance,
                               data=unshuffled_prediction,
                               time=time,
                               object=object)
  
  # Concatenate lists
  unshuffled_metrics <- data.table::rbindlist(unshuffled_metrics)
  
  # Determine the reshuffles.
  shuffle_ids <- lapply(seq_len(n_reshuffle), function(ii, n) sample.int(n=n), n=nrow(data@data))
  
  # Compute data from shuffled datasets.
  shuffled_metrics <- lapply(shuffle_ids,
                             .compute_shuffle_permutation_vimp,
                             shuffled_features=shuffled_features,
                             object=object,
                             data=data,
                             time=time,
                             metric=metric,
                             ensemble_method=ensemble_method)
  
  # Concatenate lists
  shuffled_metrics <- data.table::rbindlist(shuffled_metrics)
  
  # Check if any data was returned.
  if(is_empty(shuffled_metrics)) return(NULL)
  
  permutation_data <- lapply(metric, function(selected_metric,
                                              unshuffled_metrics,
                                              shuffled_metrics,
                                              time,
                                              similarity_threshold,
                                              shuffled_features){
    
    # Obtain the unshuffled values.
    unshuffled_value <- unshuffled_metrics[metric==selected_metric]$value
    
    # Obtain the shuffled values.
    shuffled_value <- shuffled_metrics[metric==selected_metric]$value
    
    if(length(shuffled_value) == 0) return(NULL)
    
    # Compute the median value.
    shuffled_value <- stats::median(shuffled_value, na.rm=TRUE)
    
    # Check that the median value is finite.
    if(!is.finite(shuffled_value)) return(NULL)
    
    # Return data.
    if(!is.null(time)){
      return(data.table::data.table("feature"=rep(shuffled_features, times=length(similarity_threshold)),
                                    "evaluation_time"=time,
                                    "similarity_threshold"=rep(similarity_threshold, each=length(shuffled_features)),
                                    "metric"=selected_metric,
                                    "value"=unshuffled_value - shuffled_value))
      
    } else {
      return(data.table::data.table("feature"=rep(shuffled_features, times=length(similarity_threshold)),
                                    "similarity_threshold"=rep(similarity_threshold, each=length(shuffled_features)),
                                    "metric"=selected_metric,
                                    "value"=unshuffled_value - shuffled_value))
    }
  },
  unshuffled_metrics=unshuffled_metrics,
  shuffled_metrics=shuffled_metrics,
  time=time,
  similarity_threshold=similarity_threshold,
  shuffled_features=shuffled_features)
  
  return(data.table::rbindlist(permutation_data))
}



.compute_shuffle_permutation_vimp <- function(shuffle_id,
                                              shuffled_features,
                                              object,
                                              data,
                                              time,
                                              metric,
                                              ensemble_method){
  
  # Make a local copy of data
  data@data <- data.table::copy(data@data)
  
  # Iterate over features and shuffle them.
  for(ii in shuffled_features) data.table::set(x=data@data, j=ii, value=data@data[[ii]][shuffle_id])
  
  # Predict for the shuffled data
  shuffled_prediction <- .predict(object=object,
                                  data=data,
                                  time=time,
                                  ensemble_method=ensemble_method)
  
  # Check if any predictions are valid.
  if(!any_predictions_valid(shuffled_prediction, outcome_type=object@outcome_type)) return(NULL)
  
  # Compute the unshuffled metrics.
  shuffled_metrics <- lapply(metric,
                             ..compute_model_performance,
                             data=shuffled_prediction,
                             time=time,
                             object=object)
  
  return(data.table::rbindlist(shuffled_metrics))
}



.select_feature_clusters <- function(available_features,
                                     similarity_table,
                                     cluster_method,
                                     cluster_linkage,
                                     cluster_cut_method,
                                     cluster_similarity_threshold,
                                     cluster_similarity_metric){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  name <- cluster_id <- similarity_threshold <- NULL
  
  if(is_empty(similarity_table)) {
    # Set the placeholder similarity threshold
    cluster_similarity_threshold <- ifelse(cluster_method %in% c("agnes", "diana", "hclust") & cluster_cut_method == "fixed_cut",
                                           1.0,
                                           Inf)
    
    # Only use top level features.
    cluster_table <- data.table::data.table("name"=available_features,
                                            "similarity_threshold"=cluster_similarity_threshold,
                                            "cluster_id"=seq_along(available_features),
                                            "cluster_size"=1L)
    
  } else if(cluster_method %in% c("agnes", "diana", "hclust") & cluster_cut_method == "fixed_cut"){
    # Solution for methods where multiple cuts are possible.
    
    # Compute the distance matrix
    distance_matrix <- cluster.get_distance_matrix(similarity_table=similarity_table,
                                                   similarity_metric=cluster_similarity_metric)
    
    # Insert 1.0 if necessary.
    if(!1.0 %in% cluster_similarity_threshold) cluster_similarity_threshold <- c(1.0, cluster_similarity_threshold)
    
    # Sort descending.
    cluster_similarity_threshold <- sort(cluster_similarity_threshold,
                                         decreasing=TRUE)
    
    # Obtain cluster tables for each threshold.
    cluster_table <- lapply(cluster_similarity_threshold, function(cluster_similarity_threshold, ...){
      
      # Identify clusters
      cluster_table <- cluster.get_cluster_table(require_representation=FALSE,
                                                 cluster_similarity_threshold=cluster_similarity_threshold,
                                                 ...)
      
      # Add the similarity threshold.
      cluster_table[, "similarity_threshold":=cluster_similarity_threshold]
      
      # Reorder columns
      data.table::setcolorder(cluster_table, c("name", "similarity_threshold", "cluster_id", "cluster_size"))
      
      return(cluster_table)
    },
    distance_matrix=distance_matrix,
    cluster_method=cluster_method,
    cluster_linkage=cluster_linkage,
    cluster_cut_method=cluster_cut_method,
    cluster_similarity_metric=cluster_similarity_metric)
    
    # Concatenate to single table
    cluster_table <- data.table::rbindlist(cluster_table, use.names=TRUE)
    
    # Remove features that are not available.
    cluster_table <- cluster_table[name %in% available_features]
    
  } else {
    # Solution for methods that do not allow for multiple cuts.
    
    # Identify clusters
    cluster_table <- cluster.get_cluster_table(require_representation=FALSE,
                                               distance_matrix=cluster.get_distance_matrix(similarity_table=similarity_table,
                                                                                           similarity_metric=cluster_similarity_metric),
                                               cluster_method=cluster_method,
                                               cluster_linkage=cluster_linkage,
                                               cluster_cut_method=cluster_cut_method,
                                               cluster_similarity_metric=cluster_similarity_metric,
                                               cluster_similarity_threshold=cluster_similarity_threshold)
    
    # Add the similarity threshold.
    cluster_table[, "similarity_threshold":=-Inf]
    
    # Reorder columns
    data.table::setcolorder(cluster_table, c("name", "similarity_threshold", "cluster_id", "cluster_size"))
    
    # Combine with all singleton features.
    cluster_table <- rbind(data.table::data.table("name"=available_features,
                                                  "similarity_threshold"=Inf,
                                                  "cluster_id"=seq_along(available_features),
                                                  "cluster_size"=1L),
                           cluster_table)
    
    # Remove features that are not available.
    cluster_table <- cluster_table[name %in% available_features]
  }
  
  # Identify the unique thresholds that were used.
  similarity_thresholds_used <- sort(unique(cluster_table$similarity_threshold),
                                     decreasing=TRUE)
  
  # Identify the unique clusters of features that should be computed.
  cluster_table <- cluster_table[, list("similarity_threshold"=max(similarity_threshold),
                                        "n_thresholds_same_cluster"=.N,
                                        "cluster_id"=min(cluster_id)),
                                 by=c("name", "cluster_size")]
  
  # Split the cluster table into clusters.
  cluster_list <- split(cluster_table, by=c("similarity_threshold",
                                           "n_thresholds_same_cluster",
                                           "cluster_id",
                                           "cluster_size"))
  
  return(list("similarity_threshold"=similarity_thresholds_used,
              "feature_clusters"=cluster_list))
}

#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarDataElementPermutationVimp",
         contains="familiarDataElement",
         slots=list("similarity_metric"="character"),
         prototype = methods::prototype(value_column="value",
                                        grouping_column=c("feature", "metric"),
                                        similarity_metric=NA_character_))


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
                    feature_similarity,
                    feature_cluster_method=waiver(),
                    feature_linkage_method=waiver(),
                    feature_cluster_cut_method=waiver(),
                    feature_similarity_metric=waiver(),
                    feature_similarity_threshold=waiver(),
                    metric=waiver(),
                    evaluation_times=waiver(),
                    detail_level=waiver(),
                    estimation_type=waiver(),
                    aggregate_results=waiver(),
                    confidence_level=waiver(),
                    bootstrap_ci_method=waiver(),
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
                   feature_similarity,
                   feature_cluster_method=waiver(),
                   feature_linkage_method=waiver(),
                   feature_cluster_cut_method=waiver(),
                   feature_similarity_metric=waiver(),
                   feature_similarity_threshold=waiver(),
                   metric=waiver(),
                   evaluation_times=waiver(),
                   detail_level=waiver(),
                   estimation_type=waiver(),
                   aggregate_results=waiver(),
                   confidence_level=waiver(),
                   bootstrap_ci_method=waiver(),
                   is_pre_processed=FALSE,
                   message_indent=0L,
                   verbose=FALSE,
                   ...){
            
            # Message extraction start
            logger.message(paste0("Computing permutation variable importance for models in the dataset."),
                           indent=message_indent,
                           verbose=verbose)
            
            # Load evaluation_times from the object settings attribute, if it is not provided.
            if(is.waive(evaluation_times)) evaluation_times <- object@settings$eval_times
            
            # Check evaluation_times argument
            if(object@outcome_type %in% c("survival")){
              sapply(evaluation_times, .check_number_in_valid_range, var_name="evaluation_times", range=c(0.0, Inf), closed=c(FALSE, TRUE))
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
            
            .check_parameter_value_is_valid(x=bootstrap_ci_method, var_name="bootstrap_ci_method",
                                            values=.get_available_bootstrap_confidence_interval_methods())
            
            # Check the level detail.
            detail_level <- .parse_detail_level(x = detail_level,
                                                object = object,
                                                default = "hybrid",
                                                data_element = "permutation_vimp")
            
            # Check the estimation type.
            estimation_type <- .parse_estimation_type(x = estimation_type,
                                                      object = object,
                                                      default = "bootstrap_confidence_interval",
                                                      data_element = "permutation_vimp",
                                                      detail_level = detail_level,
                                                      has_internal_bootstrap = TRUE)
            
            # Check whether results should be aggregated.
            aggregate_results <- .parse_aggregate_results(x = aggregate_results,
                                                          object = object,
                                                          default = TRUE,
                                                          data_element = "permutation_vimp")
            
            # Load metric(s) from the object settings attribute if not provided
            # externally.
            if(is.waive(metric)) metric <- object@settings$metric

            # Check metric input argument
            sapply(metric, metric.check_outcome_type, object=object)
            
            # Aggregate feature similarity data elements.
            feature_similarity <- .compute_data_element_estimates(feature_similarity)
            feature_similarity <- feature_similarity[[1]]
            
            # Obtain cluster method from stored settings, if required.
            if(is.waive(feature_cluster_method)){
              if(is.null(feature_similarity)){
                feature_cluster_method <- object@settings$feature_cluster_method
              } else {
                feature_cluster_method <- feature_similarity@cluster_method
              }
            } 
            
            # Obtain linkage function from stored settings, if required.
            if(is.waive(feature_linkage_method)){
              if(is.null(feature_similarity)){
                feature_linkage_method <- object@settings$feature_linkage_method
              } else {
                feature_linkage_method <- feature_similarity@linkage_method
              }
            } 

            # Obtain feature cluster cut method from stored settings, if
            # required.
            if(is.waive(feature_cluster_cut_method)){
              if(is.null(feature_similarity)){
                feature_cluster_cut_method <- object@settings$feature_cluster_cut_method
              } else {
                feature_cluster_cut_method <- feature_similarity@cluster_cut_method
              }
            } 
            
            # Obtain cluster similarity threshold from stored settings, if
            # required.
            if(is.waive(feature_similarity_threshold)){
              if(is.null(feature_similarity)){
                feature_similarity_threshold <- object@settings$feature_similarity_threshold
              } else {
                feature_similarity_threshold <- feature_similarity@similarity_threshold
              }
            } 

            # Obtain similarity metric from stored settings, if required.
            if(is.waive(feature_similarity_metric)){
              if(is.null(feature_similarity)){
                feature_similarity_metric <- object@settings$feature_similarity_metric
              } else {
                feature_similarity_metric <- feature_similarity@similarity_metric
              }
            } 

            # Replace feature cluster method == "none" with "hclust"
            if(feature_cluster_method == "none") feature_cluster_method <- "hclust"
            
            .check_cluster_parameters(cluster_method=feature_cluster_method,
                                      cluster_linkage=feature_linkage_method,
                                      cluster_cut_method=feature_cluster_cut_method,
                                      cluster_similarity_threshold=feature_similarity_threshold,
                                      cluster_similarity_metric=feature_similarity_metric,
                                      data_type="feature")
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)) ..error_ensemble_models_not_loaded()
            
            # Generate a prototype data element.
            proto_data_element <- new("familiarDataElementPermutationVimp",
                                      detail_level = detail_level,
                                      estimation_type = estimation_type,
                                      confidence_level = confidence_level,
                                      bootstrap_ci_method = bootstrap_ci_method,
                                      similarity_metric = feature_similarity_metric)
            
            # Generate elements and dispatch.
            vimp_data <- extract_dispatcher(FUN=.extract_permutation_vimp,
                                            has_internal_bootstrap=TRUE,
                                            cl=cl,
                                            object=object,
                                            data=data,
                                            proto_data_element=proto_data_element,
                                            is_pre_processed=is_pre_processed,
                                            ensemble_method=ensemble_method,
                                            metric=metric,
                                            evaluation_times=evaluation_times,
                                            aggregate_results=aggregate_results,
                                            similarity_table=feature_similarity,
                                            cluster_method=feature_cluster_method,
                                            cluster_linkage=feature_linkage_method,
                                            cluster_cut_method=feature_cluster_cut_method,
                                            cluster_similarity_threshold=feature_similarity_threshold,
                                            cluster_similarity_metric=feature_similarity_metric,
                                            message_indent=message_indent + 1L,
                                            verbose=verbose)
            
            return(vimp_data)
          })



.extract_permutation_vimp <- function(object,
                                      proto_data_element,
                                      evaluation_times=NULL,
                                      aggregate_results,
                                      cl,
                                      ...){
  # Ensure that the object is loaded
  object <- load_familiar_object(object)
  
  # Add model name.
  proto_data_element <- add_model_name(proto_data_element, object=object)
  
  # Add evaluation time as a identifier to the data element.
  if(length(evaluation_times) > 0 & object@outcome_type == "survival"){
    data_elements <- add_data_element_identifier(x=proto_data_element, evaluation_time=evaluation_times)
    
  } else {
    data_elements <- list(proto_data_element)
  }
  
  # Iterate over data elements
  vimp_data <- lapply(data_elements,
                      ..extract_permutation_vimp,
                      object=object,
                      aggregate_results=aggregate_results,
                      cl=cl,
                      ...)
  
  return(vimp_data)
}



..extract_permutation_vimp <- function(data_element,
                                       object,
                                       data,
                                       is_pre_processed,
                                       cl,
                                       ensemble_method,
                                       similarity_table,
                                       cluster_method,
                                       cluster_linkage,
                                       cluster_cut_method,
                                       cluster_similarity_threshold,
                                       cluster_similarity_metric,
                                       aggregate_results,
                                       progress_bar=FALSE,
                                       verbose=FALSE,
                                       message_indent,
                                       ...){
  
  # Compute prediction data.
  prediction_data <- .predict(object=object,
                              data=data,
                              time=data_element@identifiers$evaluation_time,
                              ensemble_method=ensemble_method,
                              is_pre_processed=is_pre_processed)
  
  # Check if any predictions are valid.
  if(!any_predictions_valid(prediction_data, outcome_type=object@outcome_type)) return(NULL)
  
  # Remove data with missing predictions.
  prediction_data <- remove_nonvalid_predictions(prediction_data,
                                                 outcome_type=object@outcome_type)
  
  # Remove data with missing outcomes.
  prediction_data <- remove_missing_outcomes(data=prediction_data,
                                             outcome_type=object@outcome_type)
  
  # Check that any prediction data remain.
  if(is_empty(prediction_data)) return(NULL)
  if(data.table::uniqueN(prediction_data, by=get_id_columns(id_depth="sample")) < 5) return(NULL)
  
  # Explicitly load input data. We stop at the signature step because we want to
  # work with the unclustered input features, but may need to apply
  # model-specific preprocessing steps later on.
  data <- process_input_data(object=object,
                             data=data,
                             is_pre_processed=is_pre_processed,
                             stop_at="signature")
  
  # Remove instances with missing outcomes.
  data <- remove_missing_outcomes(data=data,
                                  outcome_type=object@outcome_type)
  
  # Perform some checks to see if there is sufficient data to perform a half-way
  # meaningful analysis.
  if(is_empty(data)) return(NULL)
  if(data.table::uniqueN(data@data, by=get_id_columns(id_depth="sample")) < 5) return(NULL)
  
  # Message the user concerning the time at which metrics are computed. This is
  # only relevant for survival analysis.
  if(length(data_element@identifiers$evaluation_time) > 0 & progress_bar){
    logger.message(paste0("Computing permutation variable importance at time ", data_element@identifiers$evaluation_time, "."),
                   indent=message_indent,
                   verbose=verbose)
  }
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

  # Create full list of all instances that should be evaluated.
  bootstrap_data <- lapply(feature_cluster_info$feature_clusters,
                           .create_permutation_data_elements,
                           data_element = data_element,
                           similarity_threshold = feature_cluster_info$similarity_threshold,
                           ...)
  
  # Flatten the instance list.
  bootstrap_data <- .flatten_nested_list(bootstrap_data, flatten=TRUE)
  
  # Iterate over elements.
  data_elements <- fam_mapply(cl=cl,
                              assign=NULL,
                              FUN = .compute_permutation_vimp,
                              data_element = bootstrap_data$data_element,
                              bootstrap = bootstrap_data$bootstrap,
                              bootstrap_seed = bootstrap_data$seed,
                              shuffled_features = bootstrap_data$feature,
                              similarity_threshold = bootstrap_data$similarity_threshold,
                              n_shuffles = bootstrap_data$n_shuffle,
                              MoreArgs=c(list("object"=object,
                                              "data" = data,
                                              "prediction_data" = prediction_data,
                                              "ensemble_method" = ensemble_method),
                                         list(...)),
                              progress_bar = progress_bar,
                              chopchop=TRUE)
  
  # Merge data elements
  data_elements <- merge_data_elements(data_elements)
  
  # Aggregate results, if required.
  if(aggregate_results) data_elements <- .compute_data_element_estimates(x=data_elements)
  
  return(data_elements)
}



.compute_permutation_vimp <- function(data_element,
                                      bootstrap,
                                      bootstrap_seed,
                                      shuffled_features,
                                      similarity_threshold,
                                      n_shuffles,
                                      object,
                                      data,
                                      prediction_data,
                                      ...){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- unpermuted <- permuted <- NULL
  
  # Bootstrap the data.
  if(bootstrap) data <- get_bootstrap_sample(data=data,
                                             seed=bootstrap_seed)
  
  # Bootstrap the prediction data.
  if(bootstrap) prediction_data <- get_bootstrap_sample(data=prediction_data,
                                                        seed=bootstrap_seed)
  
  # Check if any predictions are valid.
  if(!any_predictions_valid(prediction_data, outcome_type=object@outcome_type)) return(NULL)
  
  # Determine metric scores for the real (unshuffled) data
  scores <- ..compute_permutation_vimp(data_element = data_element,
                                       object=object,
                                       data = NULL,
                                       prediction_data = prediction_data,
                                       shuffled_features = NULL,
                                       ...)
  
  # Determine metric scores for the shuffled data.
  for(ii in seq_len(n_shuffles)){
    scores <- c(scores,
                ..compute_permutation_vimp(data_element = data_element,
                                           object=object,
                                           data = data,
                                           prediction_data = NULL,
                                           shuffled_features = shuffled_features,
                                           ...))
  }
  
  # Combine to single data.table
  scores <- data.table::rbindlist(scores, use.names=TRUE)
  
  # Determine median score.
  scores <- scores[, list("value"=stats::median(value, na.rm=TRUE)), by=c("metric", "is_shuffled")]
 
  # Cast wide and compute difference between unshuffled and shuffled values.
  scores <- data.table::dcast(data=scores,
                              metric ~ is_shuffled,
                              value.var = "value")
  
  # Rename FALSE and TRUE columns to something that will not cause issues.
  data.table::setnames(scores,
                       old=c("FALSE", "TRUE"),
                       new=c("unpermuted", "permuted"))
  
  # Compute the difference between permuted and unpermuted values.
  scores[, "value":=unpermuted - permuted]
  
  # Determine the number of metrics.
  n_metrics <- nrow(scores)
  
  # Add features.
  scores <- scores[rep(seq_len(n_metrics), each=length(shuffled_features))]
  scores[, ":="("feature"=rep(shuffled_features, times=n_metrics),
                "permuted"=NULL,
                "unpermuted"=NULL)]
  
  # Store to data element.
  data_element@data <- scores
  
  # Add similarity threshold as an identifier. This ensures that data are
  # unchanged are multiplied.
  data_elements <- add_data_element_identifier(x=data_element,
                                               similarity_threshold=similarity_threshold)
  
  return(data_elements)
}


..compute_permutation_vimp <- function(data_element,
                                       object,
                                       metric,
                                       data=NULL,
                                       prediction_data=NULL,
                                       shuffled_features=NULL,
                                       ensemble_method,
                                       is_pre_processed,
                                       ...){
  
  # Generate prediction data if it does not exist.
  if(is.null(prediction_data)){
    
    if(!is.null(shuffled_features)){
      
      # Make local copy of data to avoid updating by reference.
      data@data <- data.table::copy(data@data)
      
      # Shuffle row entries.
      shuffle_id <-  sample.int(n=nrow(data@data))
      
      # Iterate over features and shuffle them. Check that the feature appears
      # in the data.
      for(ii in shuffled_features){
        if(ii %in% colnames(data@data)) data.table::set(x=data@data, j=ii, value=data@data[[ii]][shuffle_id])
      }
    }
    
    # Create prediction based on data.
    prediction_data <- .predict(object=object,
                                data=data,
                                time=data_element@identifiers$evaluation_time,
                                ensemble_method=ensemble_method)
  }
  
  # Compute score.
  score <- sapply(metric,
                  compute_metric_score,
                  object=object,
                  data=prediction_data,
                  time=data_element@identifiers$evaluation_time)
  
  return(list(data.table::data.table("metric"=metric,
                                     "value"=score,
                                     "is_shuffled"=!is.null(shuffled_features))))
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
    
    # Create method object.
    cluster_method_object <- create_cluster_method_object(cluster_method="none",
                                                          data_type = "feature",
                                                          cluster_representation_method="none")
    
    # Update (no) similarity table.
    cluster_method_object@similarity_table <- methods::new("noSimilarityTable",
                                                           data=available_features,
                                                           data_type=cluster_method_object@data_type)
    
    # Generate the clustering table.
    cluster_table <- create_clusters(object=cluster_method_object,
                                     as_cluster_object=FALSE)
    
    # Add in the similarity threshold.
    cluster_table[, "similarity_threshold":=cluster_similarity_threshold]
    cluster_table[, "cluster_size":=.N, by="cluster_id"]

  } else if(cluster_method %in% c("agnes", "diana", "hclust") & cluster_cut_method == "fixed_cut"){
    # Solution for methods where multiple cuts are possible.
    
    # Create method object.
    cluster_method_object <- create_cluster_method_object(cluster_method=cluster_method,
                                                          data_type = "feature",
                                                          cluster_linkage=cluster_linkage,
                                                          cluster_cut_method=cluster_cut_method,
                                                          cluster_similarity_threshold=cluster_similarity_threshold,
                                                          cluster_similarity_metric=cluster_similarity_metric,
                                                          cluster_representation_method="none")
    
    # Update the similarity table and attach.
    cluster_method_object@similarity_table <- methods::new("similarityTable",
                                                           data=similarity_table@data[, mget(c("feature_name_1", "feature_name_2", "value"))],
                                                           similarity_metric=cluster_similarity_metric,
                                                           data_type=cluster_method_object@data_type)
    
    # Insert 1.0 if necessary.
    if(!1.0 %in% cluster_similarity_threshold) cluster_similarity_threshold <- c(1.0, cluster_similarity_threshold)
    
    # Sort descending.
    cluster_similarity_threshold <- sort(cluster_similarity_threshold,
                                         decreasing=TRUE)
    
    # Obtain cluster tables for each threshold.
    cluster_table <- lapply(cluster_similarity_threshold, function(cluster_similarity_threshold, cluster_method_object){
      
      # Update the similarity threshold in the object.
      cluster_method_object@similarity_threshold <- cluster_similarity_threshold
      
      # Generate the clustering table.
      cluster_table <- create_clusters(object=cluster_method_object,
                                       as_cluster_object=FALSE)
      
      # Add in the similarity threshold and cluster size.
      cluster_table[, "similarity_threshold":=cluster_similarity_threshold]
      cluster_table[, "cluster_size":=.N, by="cluster_id"]
      
      return(cluster_table)
    },
    cluster_method_object=cluster_method_object)
    
    # Concatenate to single table
    cluster_table <- data.table::rbindlist(cluster_table, use.names=TRUE)
    
    # Remove features that are not available.
    cluster_table <- cluster_table[name %in% available_features]
    
  } else {
    # Solution for methods that do not allow for multiple cuts, such as PAM.
    
    # Create method object.
    cluster_method_object <- create_cluster_method_object(cluster_method=cluster_method,
                                                          data_type = "feature",
                                                          cluster_linkage=cluster_linkage,
                                                          cluster_cut_method=cluster_cut_method,
                                                          cluster_similarity_threshold=cluster_similarity_threshold,
                                                          cluster_similarity_metric=cluster_similarity_metric,
                                                          cluster_representation_method="none")
    
    # Update the similarity table and attach.
    cluster_method_object@similarity_table <- methods::new("similarityTable",
                                                           data=similarity_table@data[, mget(c("feature_name_1", "feature_name_2", "value"))],
                                                           similarity_metric=cluster_similarity_metric,
                                                           data_type=cluster_method_object@data_type)
    
    # Generate the clustering table.
    cluster_table <- create_clusters(object=cluster_method_object,
                                     as_cluster_object=FALSE)

    # Add the similarity threshold.
    cluster_table[, "similarity_threshold":=-Inf]
    cluster_table[, "cluster_size":=.N, by="cluster_id"]
    
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



.create_permutation_data_elements <- function(feature_cluster,
                                              data_element,
                                              similarity_threshold,
                                              ...){

  # Add bootstraps (if required).
  bootstrap_data <- add_data_element_bootstrap(x=data_element,
                                               ...)
  
  # Add features.
  bootstrap_data$features <- lapply(seq_along(bootstrap_data$data_element),
                                    function(ii, feature) (feature),
                                    feature=feature_cluster$name)
  
  # Identify the similarity threshold for which the current feature sets form
  # a cluster.
  n <- feature_cluster$n_thresholds_same_cluster[1]
  used_similarity_threshold <- head(similarity_threshold[similarity_threshold <= feature_cluster$similarity_threshold[1]], n=n)
  
  # Add similarity threshold.
  bootstrap_data$similarity_threshold <- lapply(seq_along(bootstrap_data$data_element),
                                                function(ii, similarity_threshold) (similarity_threshold),
                                                similarity_threshold = used_similarity_threshold)
  
  # Add number of shuffles. We don't introduce additional shuffles except for
  # point estimates.
  n_shuffles <- ifelse(data_element@estimation_type == "point", 20, 1)
  
  # Add shuffles.
  bootstrap_data$n_shuffles <- rep(n_shuffles, times=length(bootstrap_data$data_element))
  
  return(bootstrap_data)
}



#####export_permutation_vimp#####

#'@title Extract and export permutation variable importance.
#'
#'@description Extract and export model-based variable importance from a
#'  familiarCollection.
#'
#'@inheritParams export_all
#'@inheritParams export_univariate_analysis_data
#'
#'@inheritDotParams extract_permutation_vimp
#'@inheritDotParams as_familiar_collection
#'
#'@details Data, such as permutation variable importance and calibration
#'  information, is usually collected from a `familiarCollection` object.
#'  However, you can also provide one or more `familiarData` objects, that will
#'  be internally converted to a `familiarCollection` object. It is also
#'  possible to provide a `familiarEnsemble` or one or more `familiarModel`
#'  objects together with the data from which data is computed prior to export.
#'  Paths to the previously mentioned files can also be provided.
#'
#'  All parameters aside from `object` and `dir_path` are only used if `object`
#'  is not a `familiarCollection` object, or a path to one.
#'
#'  Permutation Variable importance assesses the improvement in model
#'  performance due to a feature. For this purpose, the performance of the model
#'  is measured as normal, and is measured again with a dataset where the values
#'  of the feature in question have been randomly permuted. The difference
#'  between both performance measurements is the permutation variable
#'  importance.
#'
#'  In familiar, this basic concept is extended in several ways:
#'
#'  * Point estimates of variable importance are based on multiple (21) random
#'  permutations. The difference between model performance on the normal dataset
#'  and the median performance measurement of the randomly permuted datasets is
#'  used as permutation variable importance.
#'
#'  * Confidence intervals for the ensemble model are determined using bootstrap
#'  methods.
#'
#'  * Permutation variable importance is assessed for any metric specified using
#'  the `metric` argument.
#'
#'  * Permutation variable importance can take into account similarity between
#'  features and permute similar features simultaneously.
#'
#'@return A data.table (if `dir_path` is not provided), or nothing, as all data
#'  is exported to `csv` files.
#'@exportMethod export_permutation_vimp
#'@md
#'@rdname export_permutation_vimp-methods
setGeneric("export_permutation_vimp",
           function(object,
                    dir_path=NULL,
                    aggregate_results=TRUE,
                    export_collection=FALSE,
                    ...) standardGeneric("export_permutation_vimp"))

#####export_permutation_vimp (collection)#####

#'@rdname export_permutation_vimp-methods
setMethod("export_permutation_vimp", signature(object="familiarCollection"),
          function(object,
                   dir_path=NULL,
                   aggregate_results=TRUE,
                   export_collection=FALSE,
                   ...){
            
            # Make sure the collection object is updated.
            object <- update_object(object=object)
            
            return(.export(x=object,
                           data_slot="permutation_vimp",
                           dir_path=dir_path,
                           aggregate_results=aggregate_results,
                           type="variable_importance",
                           subtype="permutation",
                           export_collection=export_collection))
          })

#####export_permutation_vimp (generic)#####

#'@rdname export_permutation_vimp-methods
setMethod("export_permutation_vimp", signature(object="ANY"),
          function(object,
                   dir_path=NULL,
                   aggregate_results=TRUE,
                   export_collection=FALSE,
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object,
                                          "data_element"="permutation_vimp",
                                          "aggregate_results"=aggregate_results),
                                     list(...)))
            
            return(do.call(export_permutation_vimp,
                           args=c(list("object"=object,
                                       "dir_path"=dir_path,
                                       "aggregate_results"=aggregate_results,
                                       "export_collection"=export_collection),
                                  list(...))))
          })

#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("featureInfoCluster",
         contains="featureInfoParameters",
         slots=list("method" = "ANY",
                    "weight" = "numeric",
                    "invert" = "logical",
                    "cluster_name" = "character",
                    "cluster_size" = "integer",
                    "cluster_features" = "character",
                    "required_features"="ANY"),
         prototype=list("method" = NULL,
                        "weight" = 1.0,
                        "invert" = FALSE,
                        "cluster_name" = NA_character_,
                        "cluster_size" = NA_integer_,
                        "cluster_features" = NA_character_,
                        "required_features"=NULL))


create_cluster_parameter_skeleton <- function(feature_info_list,
                                              feature_names=NULL,
                                              cluster_method,
                                              cluster_linkage=NULL,
                                              cluster_cut_method=NULL,
                                              cluster_similarity_threshold=NULL,
                                              cluster_similarity_metric=NULL,
                                              cluster_representation_method=NULL,
                                              .override_existing=FALSE){
  # Creates a skeleton for the provided cluster method.
  
  # Determine feature names from the feature info list, if provided.
  if(is.null(feature_names)) feature_names <- names(feature_info_list)
  
  # Select only features that appear in the feature info list.
  feature_names <- intersect(names(feature_info_list),
                             feature_names)
  
  # Skip step if no feature info objects are updated.
  if(is_empty(feature_names)) return(feature_info_list)
  
  # Check that method is applicable.
  .check_parameter_value_is_valid(x=cluster_method,
                                  var_name="cluster_method",
                                  values=.get_available_cluster_methods())
  
  # Update familiar info objects with a feature clustering skeleton.
  updated_feature_info <- fam_lapply(X=feature_info_list[feature_names],
                                     FUN=.create_cluster_parameter_skeleton,
                                     method=cluster_method,
                                     .override_existing=.override_existing)
  
  # Provide names for the updated feature info objects.
  names(updated_feature_info) <- feature_names
  
  # Replace list elements.
  feature_info_list[feature_names] <- updated_feature_info
  
  return(feature_info_list)
}



.create_cluster_parameter_skeleton <- function(feature_info,
                                               method,
                                               ...,
                                               .override_existing=FALSE){
  
  # Check if clustering data was already completed, and does not require being
  # determined anew.
  if(feature_info_complete(feature_info@cluster_parameters) & !.override_existing) return(feature_info)
  
  # Create cluster parameter object.
  object <- methods::new("featureInfoCluster",
                         method=method)
  
  # Set feature name
  object@name <- feature_name
  
  # Update the familiar version.
  object <- add_package_version(object=object)
  
  # Check that the feature was not removed and is not set as a signature.
  if(!available(feature_info) || is_in_signature(feature_info)) method <- "none"
  
  # Create the cluster method object.
  object@method <- create_cluster_method_object(cluster_method=method,
                                                data_type="cluster",
                                                ...)
  
  # Update imputation_parameters slot.
  feature_info@cluster_parameters <- object
  
  return(feature_info)
}




add_cluster_info <- function(cl=NULL,
                             feature_info_list,
                             data,
                             message_indent=0L,
                             verbose=FALSE){

  # Suppress NOTES due to non-standard evaluation in data.table
  name <- cluster_id <- weight <- cluster_size <-  NULL
  
  
  # Find feature columns.
  feature_names <- get_feature_columns(x=data)
  
  # Sanity check.
  if(!(setequal(feature_names, get_available_features(feature_info_list=feature_info_list)))){
    ..error_reached_unreachable_code("add_cluster_info: features in data and the feature info list are expect to be the same, but were not.")
  }
  
  # Skeletons should be present. Now we need to identify which features can be
  # grouped by their method slot. This is done by iterating, and identifying
  # which features form groups. Though this may seem to a bit convoluted, we do
  # this to allow for proper processing of features that may have different
  # parameters assigned on purpose, e.g. signature features or externally
  # provided features.
  
  # List of cluster groups that use the same method.
  cluster_group_lists <- NULL
  
  # Eliminate features that are already complete.
  feature_names <- feature_names[!sapply(feature_info_list[feature_names],
                                         function(x) (feature_info_complete(x@cluster_parameters)))]
  
  # Skip any further processing if all parameter information has already been
  # completed.
  if(length(feature_names) == 0) return(feature_info_list)  
  
  # Identify all features that are not to be clustered.
  none_features <- feature_names[sapply(feature_info_list[feature_names],
                                                          function(x) (is(x@cluster_parameters@method, "clusterMethodNone")))]
  
  # Identify features that should still be sorted.
  feature_names <- setdiff(feature_names, none_features)
  browser()
  # Iterate to create feature groups.
  while(TRUE){
    # Break once all features have been assigned.
    if(length(feature_names) == 0) break()
    
    # Get the cluster method object of the first feature that still needs to be
    # sorted.
    cluster_method_object <- feature_info_list[[feature_names[1L]]]@cluster_parameters@method
    
    # Find other unsorted features that have the same method.
    same_method_features <- feature_names[sapply(feature_info_list[feature_names],
                                                 function(x, y) (identical(x@cluster_parameters@method, y)),
                                                 y=cluster_method_object)]
    
    # Append to the cluster group list. Note that the list is nested to avoid
    # concatenating list elements.
    cluster_group_lists <- c(cluster_group_lists,
                             list(list("features"=same_method_features,
                                       "cluster_method_object"=cluster_method_object)))
    
    # Remove features from feature names.
    feature_names <- setdiff(feature_names, same_method_features)
  }
  
  # Now process data that use clustering.
  if(length(cluster_group_lists) > 0){
    # Iterate over lists of features that have unique clustering parameters.
    for(cluster_group in cluster_group_lists){
      
      # Check that at least 3 features are present. Otherwise no sensible
      # clusters may be formed.
      if(length(cluster_group$features) <= 2){
        
        # Update feature info so that these features now have "none" cluster objects.
        feature_info_list <- create_cluster_parameter_skeleton(feature_info_list,
                                                               feature_names=cluster_group$features,
                                                               cluster_method="none")
        # Add features to none_features
        none_features <- c(none_features,
                           cluster_group$features)
        
        # And skip to the next group.
        next()
      }
      
      # Compute distance matrix from data.
      distance_matrix <- get_distance_matrix(object=cluster_group$cluster_method_object,
                                             data=filter_features(data=data,
                                                                  available_features=cluster_group$features),
                                             feature_info_list=feature_info_list[cluster_group$features])
      
    }
  }
  
  # Determine feature columns. Novelty features are clustered.
  feature_columns <- get_available_features(feature_info_list=feature_info_list,
                                            data_obj=data_obj,
                                            exclude_signature=TRUE,
                                            exclude_novelty=FALSE)
  
  if(length(feature_columns) <= 2 | settings$prep$cluster_method=="none"){
    # Message that no clustering was performed
    logger.message(paste0("Pre-processing: No feature clustering was performed."),
                   indent=message_indent,
                   verbose=verbose)
    
    return(feature_info_list)
  }
  
  # Find distances
  dist_mat <- cluster.get_distance_matrix(cl=cl,
                                          data_obj=data_obj,
                                          feature_columns=feature_columns,
                                          settings=settings,
                                          message_indent=message_indent,
                                          verbose=verbose)

  # Determine if there are any reasonable cluster candidates
  if(all(dist_mat > similarity.to_distance(x=settings$prep$cluster_sim_thresh,
                                           similarity_metric=settings$prep$cluster_similarity_metric))){
    
    # Message that no clustering was performed due to distances
    logger.message(paste0("Pre-processing: No feature clustering was performed as no feature pairs were within the distance to form a cluster."),
                   indent=message_indent,
                   verbose=verbose)
    
    return(feature_info_list)
  }
  
  # Identify clusters
  cluster_table <- cluster.get_cluster_table(cl=cl,
                                             require_representation=TRUE,
                                             distance_matrix=dist_mat,
                                             data_obj=data_obj,
                                             feature_info_list=feature_info_list,
                                             settings=settings,
                                             message_indent=message_indent,
                                             verbose=verbose)

  # Generate the update list
  upd_list <- lapply(cluster_table$name, function(ii, feature_info_list, cluster_table, cluster_repr_method){
    
    # Find featureInfo object
    object <- feature_info_list[[ii]]
    
    # Select the table entry
    table_entry <- cluster_table[name==ii]
    
    # Add cluster parameters
    object@cluster_parameters <- list("method" = cluster_repr_method,
                                      "weight" = table_entry$weight,
                                      "invert" = table_entry$inversion,
                                      "required_features" = cluster_table[cluster_id==table_entry$cluster_id & weight > 0.0]$name,
                                      "cluster_name" = paste0(table_entry$cluster_repr_feature, "_cluster"),
                                      "cluster_size" = table_entry$cluster_size)
    
    return(object)
  }, feature_info_list=feature_info_list, cluster_table=cluster_table, cluster_repr_method=settings$prep$cluster_repr_method)
  
  # Set names of the update object list
  names(upd_list) <- cluster_table$name
  
  # Update feature_info_list
  feature_info_list[cluster_table$name] <- upd_list
  
  # Message results of clustering
  n_clusters          <- data.table::uniqueN(cluster_table[cluster_size > 1]$cluster_id)
  n_singular_clusters <- data.table::uniqueN(cluster_table[cluster_size == 1]$cluster_id)
  
  logger.message(paste0("Pre-processing: ", n_clusters, " non-singular ",
                        ifelse(n_clusters==1, "cluster was", "clusters were"), " formed. ",
                        n_clusters + n_singular_clusters, " features remain."),
                 indent=message_indent,
                 verbose=verbose)
  
  return(feature_info_list)
}



# cluster.get_featurewise_similarity_table <- function(cl=NULL, data_obj,
#                                                      feature_columns=NULL,
#                                                      settings=NULL,
#                                                      similarity_metric=NULL,
#                                                      message_indent=0L,
#                                                      verbose=FALSE){
#   # Create a pairwise similarity table
#   
#   # Internal function for computing pair-wise similarity
#   ..compute_similarity <- function(ii, combinations, data, similarity_metric, categorical_mask){
#     
#     # Identify features that are being compared.
#     feature_1 <- combinations[1, ii]
#     feature_2 <- combinations[2, ii]
#     
#     # Compute pairwise similarity
#     similarity <- similarity.compute_similarity(x=data[[feature_1]],
#                                                 y=data[[feature_2]],
#                                                 x_categorical=categorical_mask[feature_1],
#                                                 y_categorical=categorical_mask[feature_2],
#                                                 similarity_metric=similarity_metric)
#     
#     return(similarity)
#   }
#   
#   
#   if(is.null(data_obj)){
#     ..error_reached_unreachable_code("cluster.get_featurewise_similarity_table: data_obj argument cannot be missing.")
#   }
#   
#   if(!is.null(settings)){
#     similarity_metric <- settings$prep$cluster_similarity_metric
#   } else if(is.null(similarity_metric)){
#     ..error_reached_unreachable_code("cluster.get_featurewise_similarity_table: settings and similarity_metric arguments are both missing.")
#   }
#   
#   # Message similarity metric
#   logger.message(paste0("Pair-wise distance between features is quantified by ",
#                         similarity.message_similarity_metric(similarity_metric=similarity_metric), "."),
#                  indent=message_indent,
#                  verbose=verbose)
#   
#   # Get feature columns if this was not provided
#   if(is.null(feature_columns)){
#     feature_columns <- get_feature_columns(x=data_obj)
#   }
#   
#   # Generate all combinations of features
#   combinations <- utils::combn(sort(feature_columns), 2)
#   
#   # Determine which features are categorical.
#   column_class <- lapply(feature_columns, function(ii, data) (class(data[[ii]])), data=data_obj@data)
#   categorical_mask <- sapply(column_class, function(selected_column_class) (any(selected_column_class %in% c("logical", "character", "factor"))))
#   
#   # Add names to the mask to allow indexing by feature name.
#   names(categorical_mask) <- feature_columns
#   
#   # Determine similarity measures for each feature pair.
#   similarity <- fam_sapply(cl=cl,
#                            assign=NULL,
#                            X=seq_len(ncol(combinations)),
#                            FUN=..compute_similarity,
#                            progress_bar=verbose,
#                            combinations=combinations,
#                            data=droplevels(data_obj@data),
#                            similarity_metric=similarity_metric,
#                            categorical_mask=categorical_mask,
#                            chopchop=TRUE)
#   
#   # Transform similarity scores into a data.table.
#   similarity_table  <- data.table::data.table("feature_name_1"=combinations[1,],
#                                               "feature_name_2"=combinations[2,],
#                                               "value"=similarity)
# 
#   return(similarity_table)
# }
# 
# 
# 
# cluster.get_samplewise_similarity_table <- function(cl=NULL,
#                                                     data_obj,
#                                                     similarity_metric,
#                                                     message_indent=0L,
#                                                     verbose=FALSE){
#   
#   # Internal function for computing pair-wise similarity
#   ..compute_similarity <- function(ii, combinations, data, similarity_metric, categorical_mask){
# 
#     # Identify features that are being compared.
#     row_1 <- combinations[1, ii]
#     row_2 <- combinations[2, ii]
#     
#     # Compute pairwise similarity
#     similarity <- similarity.compute_similarity(x=as.numeric(data[row_1, ]),
#                                                 y=as.numeric(data[row_2, ]),
#                                                 x_categorical=categorical_mask,
#                                                 y_categorical=categorical_mask,
#                                                 similarity_metric=similarity_metric)
#     
#     return(similarity)
#   }
#   
#   
#   # Get similarity between samples.
#   if(is.null(data_obj)){
#     ..error_reached_unreachable_code("cluster.get_samplewise_similarity_table: data_obj argument cannot be missing.")
#   }
#   
#   # Check if similarity metric is provided.
#   if(is.null(similarity_metric)){
#     ..error_reached_unreachable_code("cluster.get_samplewise_similarity_table: similarity_metric argument is missing.")
#   }
#   
#   # Message similarity metric
#   logger.message(paste0("Pair-wise distance between samples is quantified by ",
#                         similarity.message_similarity_metric(similarity_metric=similarity_metric), "."),
#                  indent=message_indent,
#                  verbose=verbose)
#   
#   # Get feature columns.
#   feature_columns <- get_feature_columns(x=data_obj)
#   
#   # Determine which features are categorical.
#   column_class <- lapply(feature_columns, function(ii, data) (class(data[[ii]])), data=data_obj@data)
#   categorical_mask <- sapply(column_class, function(selected_column_class) (any(selected_column_class %in% c("logical", "character", "factor"))))
#   
#   # Determine if data requires normalisation
#   if(similarity.requires_normalisation(similarity_metric=similarity_metric)){
#     # Identify numerical features
#     numerical_features <- feature_columns[!categorical_mask]
#     
#     if(grepl(pattern="_trim", x=similarity_metric, fixed=TRUE)){
#       norm_method <- "normalisation_trim"
#       
#     } else if(grepl(pattern="_winsor", x=similarity_metric, fixed=TRUE)){
#       norm_method <- "normalisation_winsor"
#       
#     } else {
#       norm_method <- "normalisation"
#     }
#     
#     # Perform normalisation.
#     for(ii in numerical_features){
#       data.table::set(data_obj@data,
#                       j=ii,
#                       value=.normalise(x=data_obj@data[[ii]],
#                                        normalisation_method=norm_method,
#                                        range=c(0, 1)))
#     }
#   }
#   
#   # Generate all combinations of samples
#   combinations <- utils::combn(seq_len(nrow(data_obj@data)), 2)
#   
#   # Determine similarity measures for each sample pair.
#   similarity <- fam_sapply(cl=cl,
#                            assign=NULL,
#                            X=seq_len(ncol(combinations)),
#                            FUN=..compute_similarity,
#                            progress_bar=verbose,
#                            combinations=combinations,
#                            data=data_obj@data[, mget(feature_columns)],
#                            similarity_metric=similarity_metric,
#                            categorical_mask=categorical_mask,
#                            chopchop=TRUE)
#   
#   # Create unique row names.
#   row_names <- get_unique_row_names(x=data_obj)
#   
#   # Transform similarity scores into a data.table.
#   similarity_table  <- data.table::data.table("sample_1"=row_names[combinations[1, ]],
#                                               "sample_2"=row_names[combinations[2, ]],
#                                               "value"=similarity)
#   
#   return(similarity_table)
# }
# 
# 
# cluster.get_distance_matrix <- function(cl=NULL,
#                                         data_obj=NULL,
#                                         similarity_table=NULL,
#                                         feature_columns=NULL,
#                                         settings=NULL,
#                                         similarity_metric=NULL,
#                                         message_indent=0L,
#                                         verbose=FALSE){
#   # Compute distance matrix
# 
#   # Suppress NOTES due to non-standard evaluation in data.table
#   value <- NULL
#   
#   # Check if similarity metric is provided
#   if(!is.null(settings)){
#     similarity_metric <- settings$prep$cluster_similarity_metric
#   } else if(is.null(similarity_metric)){
#     ..error_reached_unreachable_code("cluster.get_distance_matrix: settings and similarity_metric arguments are both missing.")
#   }
#   
#   # Compute the similarity_table first. This is the distance matrix in tabular
#   # format. This table is then processed further to derive the distance matrix.
#   if(is.null(similarity_table)){
#     lower_triangle <- cluster.get_featurewise_similarity_table(cl=cl,
#                                                                data_obj=data_obj,
#                                                                feature_columns=feature_columns,
#                                                                settings=settings,
#                                                                similarity_metric=similarity_metric,
#                                                                message_indent=message_indent,
#                                                                verbose=verbose)
#     
#   } else {
#     lower_triangle <- data.table::copy(similarity_table)
#   }
#   
#   # Determine whether the similarity table is for features (columns) or samples
#   # (rows).
#   table_type <- ifelse("feature_name_1" %in% colnames(lower_triangle), "feature", "sample")
#   
#   element_1 <- ifelse(table_type == "feature", "feature_name_1", "sample_1")
#   element_2 <- ifelse(table_type == "feature", "feature_name_2", "sample_2")
#   
#   # Find elements from the distance table.
#   elements <- union(lower_triangle[[element_1]], lower_triangle[[element_2]])
#   
#   # Convert similarity to distance.
#   lower_triangle[, "value":=similarity.to_distance(x=value, similarity_metric=similarity_metric)]
#   
#   # Add in other triangle of the table by switching around the columns.
#   upper_triangle <- data.table::copy(lower_triangle)
#   data.table::setnames(upper_triangle, old=c(element_1, element_2), new=c(element_2, element_1))
#   
#   # Combine with data.table that includes diagonals
#   diagonal_table <- data.table::data.table("element_1"=elements, "element_2"=elements, "value"=as.double(0))
#   data.table::setnames(diagonal_table, old=c("element_1", "element_2"), new=c(element_1, element_2))
#   distance_table  <- rbind(lower_triangle, diagonal_table, upper_triangle)
#   
#   # Create n x n table
#   distance_table  <- data.table::dcast(distance_table,
#                                        stats::as.formula(paste(element_1, "~", element_2)),
#                                        value.var="value")
#   
#   rownames(distance_table) <- distance_table[[element_1]]
#   distance_table[, (element_1):=NULL]
#   
#   # Create dissimilarity matrix
#   distance_matrix <- stats::as.dist(distance_table)
#   
#   return(distance_matrix)
# }



cluster.get_cluster_object <- function(cl=NULL,
                                       distance_matrix,
                                       settings=NULL,
                                       cluster_method=NULL,
                                       cluster_linkage=NULL,
                                       message_indent=0L,
                                       verbose=FALSE){

  # Read and check settings required to form a cluster object.
  if(!is.null(settings)){
    cluster_method <- settings$prep$cluster_method
    cluster_linkage <- settings$prep$cluster_linkage
    
  } else if(is.null(cluster_method)){
    ..error_reached_unreachable_code("cluster.get_cluster_object: no cluster_method argument was provided.")
  }
  
  if(cluster_method %in% c("agnes", "diana", "hclust") & is.null(cluster_linkage)){
    ..error_reached_unreachable_code("cluster.get_cluster_object: no cluster_linkage argument was provided.")
  }
  
  if(cluster_method %in% c("pam")){
    ##### Partitioning methods #############################################################
    
    # Message the algorithm
    logger.message(paste0("Pre-processing: Clustering using partitioning around medioids (PAM)."),
                   indent=message_indent,
                   verbose=verbose)
    
    # Determine optimal numbers of clusters based on silhouette
    n_clusters <- cluster.optimise_silhoutte(n_features=attr(distance_matrix, "Size"),
                                             dist_mat=distance_matrix,
                                             cluster_method=cluster_method)
    
    # Message the number of clusters
    logger.message(paste0("Pre-processing: Best average cluster silhouette was achieved by creating ", n_clusters, " clusters."),
                   indent=message_indent,
                   verbose=verbose)
    
    # Message representation
    logger.message(paste0("Pre-processing: A cluster is represented by its medioid feature."),
                   indent=message_indent,
                   verbose=verbose)
    
    # Create clusters
    cluster_object <- cluster.pam(dist_mat=distance_matrix, n_clusters=n_clusters)
    
  } else if(cluster_method %in% c("agnes", "diana", "hclust")){
    
    ##### Hierarchical methods #############################################################
    
    # Create clusters
    if(cluster_method=="agnes"){
      # Message the algorithm
      logger.message(paste0("Pre-processing: Clustering using agglomerative hierarchical clustering (AGNES) with ", cluster_linkage, " linkage."),
                     indent=message_indent,
                     verbose=verbose)
      
      # Create dendrogram
      cluster_object <- cluster.agnes(dist_mat=distance_matrix,
                                      linkage=cluster_linkage)
      
    } else if(cluster_method=="hclust") {
      # Message the algorithm
      logger.message(paste0("Pre-processing: Clustering using hierarchical clustering with ", cluster_linkage, " linkage."),
                     indent=message_indent,
                     verbose=verbose)
      
      # Create dendrogram
      cluster_object <- cluster.hclust(dist_mat=distance_matrix,
                                       linkage=cluster_linkage)
      
    } else if(cluster_method == "diana") {
      # Message the algorithm
      logger.message(paste0("Pre-processing: Clustering using divisive analysis hierarchical clustering (DIANA)."),
                     indent=message_indent,
                     verbose=verbose)
      
      # Create dendrogram
      cluster_object <- cluster.diana(dist_mat=distance_matrix)
    }
    
    # Convert to hierarchical cluster object
    cluster_object <- stats::as.hclust(cluster_object)
    
  } else {
    ..error_reached_unreachable_code("cluster.get_cluster_object: unknown cluster_method.")
  }
  
  return(cluster_object)
}



cluster.get_cluster_table <- function(cl=NULL,
                                      require_representation=TRUE,
                                      cluster_object=NULL, 
                                      distance_matrix=NULL,
                                      data_obj=NULL,
                                      feature_info_list=NULL,
                                      settings=NULL,
                                      cluster_method=NULL, 
                                      cluster_linkage=NULL,
                                      cluster_cut_method=NULL,
                                      cluster_similarity_threshold=NULL,
                                      cluster_similarity_metric=NULL,
                                      cluster_representation_method=NULL,
                                      message_indent=0L,
                                      verbose=FALSE){

  if(is.null(distance_matrix)){
    if(is.null(cluster_object)) ..error_reached_unreachable_code("cluster.get_cluster_table: distance_matrix is required to create a cluster_object.")
    if(require_representation) ..error_reached_unreachable_code("cluster.get_cluster_table: distance_matrix is required to find representative features.")
  }
  
  # Create a cluster object if required.
  if(is.null(cluster_object)){
    cluster_object <- cluster.get_cluster_object(cl=cl, distance_matrix=distance_matrix, settings=settings, cluster_method=cluster_method,
                                                 cluster_linkage=cluster_linkage, verbose=verbose)
  }
  
  # Read and check settings required to form a cluster table
  if(!is.null(settings)){
    cluster_method <- settings$prep$cluster_method

  } else if(is.null(cluster_method)){
    ..error_reached_unreachable_code("cluster.get_cluster_table: no cluster_method argument was provided.")
  }
  
  if(!is.null(settings)){
    cluster_cut_method <- settings$prep$cluster_cut_method
    cluster_similarity_threshold <- settings$prep$cluster_sim_thresh
    cluster_similarity_metric <- settings$prep$cluster_similarity_metric
    
  } else if(is.null(cluster_cut_method) & cluster_method %in% c("agnes", "diana", "hclust")){
    ..error_reached_unreachable_code("cluster.get_cluster_table: no cluster_cut_method argument was provided.")
    
  } else if(is.null(cluster_similarity_threshold) & cluster_method %in% c("agnes", "diana", "hclust")){
    ..error_reached_unreachable_code("cluster.get_cluster_table: no cluster_similarity_threshold argument was provided.")
    
  } else if(is.null(cluster_similarity_metric) & cluster_method %in% c("agnes", "diana", "hclust")){
    ..error_reached_unreachable_code("cluster.get_cluster_table: no cluster_similarity_metric argument was provided.")
  }
  
  # Check whether a distance matrix is present to optimise n_clusters.
  if(is.null(distance_matrix) & cluster_method %in% c("agnes", "diana", "hclust")){
    if(cluster_cut_method == "silhouette"){
      ..error_reached_unreachable_code(paste0("cluster.get_cluster_table: no distance_matrix argument",
                                       " was provided to derive an optimal number of clusters using silhouette for hierarchical clustering."))
    }
  }
  
  # cluster_representation_method
  if(is.null(cluster_representation_method) & require_representation & cluster_method != "pam"){
    cluster_representation_method <- settings$prep$cluster_repr_method
  }
  
  if(is.null(data_obj)){
    if(require_representation){
      if(!cluster_representation_method %in% c("first") & !cluster_method %in% c("pam")){
      ..error_reached_unreachable_code("cluster.get_cluster_table: data_obj is required to find representative features.")
      }
    } 
  }
  
  if(is.null(feature_info_list)){
    if(require_representation){
      if(!cluster_representation_method %in% c("first") & !cluster_method %in% c("pam")){
        ..error_reached_unreachable_code("cluster.get_cluster_table: feature_info_list is required to find representative features.")
      }
    }
  }
  
  # Suppress NOTES due to non-standard evaluation in data.table
  cluster_repr_feature <- name <- NULL
  
  if(cluster_method %in% c("pam")){
    ##### Partitioning methods #############################################################
    
    # Get cluster medoids and the corresponding cluster id.
    temp_cluster_table <- data.table::data.table("cluster_id"=seq_len(length(cluster_object$id.med)),
                                                 "cluster_repr_feature"=names(cluster_object$clustering)[cluster_object$id.med])
    
    # Combine with all cluster features based on the cluster id.
    cluster_table <- data.table::data.table("name"=names(cluster_object$clustering),
                                                   "cluster_id"=cluster_object$clustering)
    cluster_table <- merge(cluster_table, temp_cluster_table, by="cluster_id", all.x=TRUE)
    
    # Add inversion and weight columns
    cluster_table[, ":="("inversion"=FALSE, "weight"=0.0)]
    cluster_table[name==cluster_repr_feature, "weight":=1.0]
    
  } else if(cluster_method %in% c("agnes", "diana", "hclust")){
    ##### Hierarchical methods #############################################################
  
    # Compute the cut-height
    cut_height <- similarity.to_distance(x=cluster_similarity_threshold, similarity_metric=cluster_similarity_metric)
    
    # Determine cuts
    if(cluster_cut_method=="fixed_cut"){
      # Cut the tree at the given height
      cluster_id <- stats::cutree(tree=cluster_object, h=cut_height)
      
      # Define clusters
      cluster_table <- data.table::data.table("name"=names(cluster_id), "cluster_id"=cluster_id)
      
      # Message the number of clusters
      logger.message(paste0("Pre-processing: Clusters were created by cutting the dendrogram below ", cut_height, ", which corresponds to ",
                            similarity.message_similarity_metric(similarity_metric=cluster_similarity_metric)," above ", cluster_similarity_threshold, "."),
                     indent=message_indent,
                     verbose=verbose)
      
      
    } else if(cluster_cut_method=="dynamic_cut"){
      
      if(!is_package_installed("dynamicTreeCut")){
        logger.stop("Please install the dynamicTreeCut package.")
      }
      
      # From Langfelder P, Zhang B, Horvath S (2007) Defining clusters from a hierarchical cluster tree: the Dynamic Tree Cut package for R. Bioinformatics 2008 24(5):719-720
      cluster_id <- dynamicTreeCut::cutreeDynamicTree(dendro=cluster_object, maxTreeHeight=cut_height, deepSplit=TRUE, minModuleSize=1)
      
      # Define clusters
      cluster_table <- data.table::data.table("name"=cluster_object$labels, "cluster_id"=cluster_id)
      
      # Message the number of clusters
      logger.message(paste0("Pre-processing: Clusters were determined using the dynamic tree cut algorithm with maximum height of ", cut_height, ", which corresponds to ",
                            similarity.message_similarity_metric(similarity_metric=cluster_similarity_metric)," above ", cluster_similarity_threshold, "."),
                     indent=message_indent,
                     verbose=verbose)
      
      
    } else if(cluster_cut_method=="silhouette"){
      
      # Determine the optimal number of clusters based on the average cluster silhouette
      n_clusters <- cluster.optimise_silhoutte(tree=cluster_object, n_features=attr(distance_matrix, "Size"),
                                               dist_mat=distance_matrix, cluster_method=cluster_method)
      
      # Cut the tree for the optimal number of clusters
      cluster_id <- stats::cutree(tree=cluster_object, k=n_clusters)
      
      # Define clusters
      cluster_table <- data.table::data.table("name"=names(cluster_id), "cluster_id"=cluster_id)
      
      # Message the number of clusters
      logger.message(paste0("Pre-processing: Best average cluster silhouette was achieved by creating ", n_clusters, " clusters."),
                     indent=message_indent,
                     verbose=verbose)
    }
    
    if(require_representation){
      
      # Find representative features
      cluster_table <- fam_lapply_lb(cl=cl,
                                     assign=NULL,
                                     X=split(cluster_table, by="cluster_id"),
                                     FUN=cluster.find_representation,
                                     progress_bar=FALSE,
                                     data_obj=data_obj,
                                     feature_info_list=feature_info_list,
                                     dist_mat=distance_matrix,
                                     method=cluster_representation_method)
      
      # Combine into single table
      cluster_table <- data.table::rbindlist(cluster_table)
    }
  }

  # Determine cluster size
  cluster_table[, "cluster_size":=.N, by="cluster_id"]
  
  return(cluster_table)
}



cluster.extract_label_order <- function(cluster_object, cluster_method){
  
  if(cluster_method %in% c("hclust", "diana", "agnes")){

    # Extract order from hierarchical clusters.
    return(data.table::data.table("name"=cluster_object$labels[cluster_object$order],
                                  "label_order"=seq_along(cluster_object$labels)))
    
  } else if(cluster_method %in% c("pam")){
    
    # Use the silhouette info matrix to determine label order.
    silhouette_matrix <- cluster_object$silinfo$widths
    order_table <- data.table::data.table("name"=rownames(silhouette_matrix))
    order_table[,  "label_order":=.I]
    
    return(order_table)

  } else {
    ..error_reached_unreachable_code(paste0("cluster.extract_label_order: label order cannot be extracted"
                                            ," because the cluster_method was not recognized."))
  }
}



cluster.get_silhouette <- function(n_clusters, tree=NULL, dist_mat, cluster_method){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  sil_width <- cluster_size <- NULL
  
  if(cluster_method == "pam"){
    # Generate partioning around medoids cluster
    clust_obj <- cluster.pam(dist_mat=dist_mat, n_clusters=n_clusters)
    
    # Extract silhoutte table
    silhouette_table <- data.table::as.data.table(clust_obj$silinfo$widths, keep.rownames=FALSE)
    
    # Compute the size and average silhouette in each cluster
    silhouette_table <- silhouette_table[, list("avg_sil"=mean(sil_width), "cluster_size"=.N), by="cluster"]
    
    # Maintain only non-singular clusters
    silhouette_table <- silhouette_table[cluster_size > 1]
    
    # Return average silhouette in the formed non-singular clusters.
    if(!is_empty(silhouette_table)){
      return(mean(silhouette_table$avg_sil))
      
    } else {
      return(0.0)
    }
    
  } else if(cluster_method %in% c("agnes", "diana", "hclust")){
    
    require_package(x="cluster",
                    purpose="to cluster similar features together")
    
    # Compute silhouette.
    silhouette_matrix <- cluster::silhouette(x=stats::cutree(tree=tree, k=n_clusters), dist=dist_mat)
    
    # Parse to matrix by changing the class. 
    class(silhouette_matrix) <- "matrix"

    # Extract silhoutte table
    silhouette_table <- data.table::as.data.table(silhouette_matrix, keep.rownames=FALSE)
    
    # Compute the size and average silhouette in each cluster
    silhouette_table <- silhouette_table[, list("avg_sil"=mean(sil_width), "cluster_size"=.N), by="cluster"]
    
    # Maintain only non-singular clusters
    silhouette_table <- silhouette_table[cluster_size > 1]
    
    # Return average silhouette in the formed non-singular clusters.
    if(!is_empty(silhouette_table)){
      return(mean(silhouette_table$avg_sil))
      
    } else {
      return(0.0)
    }
    
  } else {
    ..error_reached_unreachable_code("cluster.get_silhouette: unknown cluster method encountered.")
  }
}



cluster.optimise_silhoutte <- function(n_features, tree=NULL, dist_mat, cluster_method){

  # Check problematic values
  if(n_features == 1){
    return(1)
    
  } else if(n_features == 2){
    
    if(dist_mat[1] == 0){
      # Zero distance can be safely imputed as being identical.
      return(1)
      
    } else {
      warning("Optimal number of clusters for silhouette could not be determined as only 2 features were available.")
      
      return(2)
    }
  }
  
  # Get cluster silhouettes
  clust_k         <- seq(from=2, to=n_features-1, by=1)
  clust_sil       <- sapply(clust_k, cluster.get_silhouette, tree=tree, dist_mat=dist_mat, cluster_method=cluster_method)

  # Determine width of moving average filter as well as padding
  n_pad_width     <- floor((0.5 * sqrt(n_features) - 1))
  n_pad_width     <- max(c(0, n_pad_width))

  # Set filter width
  n_filt_width    <- 2 * n_pad_width + 1

  # Pad silhouettes before filtering
  if(n_pad_width > 0){
    clust_sil <- c(rep(head(clust_sil, n=1), times=n_pad_width),
                   clust_sil,
                   rep(tail(clust_sil, n=1), times=n_pad_width))
  }

  # Create moving average filter
  mov_avg_filter <- rep(1/n_filt_width, times=n_filt_width)

  # Apply filter
  clust_sil_avg  <- as.numeric(stats::filter(x=clust_sil, filter=mov_avg_filter, sides=2))

  # Drop edge values
  clust_sil_avg  <- clust_sil_avg[!is.na(clust_sil_avg)]

  # Find maximum silhouette value.
  max_sil_avg    <- max(clust_sil_avg)
  
  # Determine if the silhoutte indicates reasonable structure (> 0.50), see
  # Kaufman and Rousseeuw: Finding groups in data.
  if(max_sil_avg > 0.50){
    optim_k      <- clust_k[which.max(clust_sil_avg)]
    
  } else {
    optim_k      <- n_features
  }

  return(optim_k)
}



cluster.pam <- function(dist_mat, n_clusters){
  # Create partioning-around-medoids cluster Kaufman, L. and Rousseeuw, P.J.
  # (1987), Clustering by means of Medoids, in Statistical Data Analysis Based
  # on the L 1 {\displaystyle L_{1}} L_{1}–Norm and Related Methods, edited by
  # Y. Dodge, North-Holland, 405–416.)
  
  if(n_clusters == length(attr(dist_mat, "Labels"))){
    # PAM clustering doesn't like it when you n_clusters is equal to the number
    # of features.
    
    # Obtain features
    features <- attr(dist_mat, "Labels")
    
    # Create fake pam object
    h <- list("medoids"=features,
              "id.med"=seq_along(features),
              "clustering"=seq_along(features),
              "isolation"=factor(rep_len("no", length(features)), levels=c("no", "L", "L*")),
              "silinfo"=list("widths"=matrix(data=0.0,
                                             nrow=length(features),
                                             ncol=3L,
                                             dimnames=list(features, c("cluster", "neighbor", "sil_width"))),
                             "clus.avg.widths"=rep_len(0.0, length(features)),
                             "avg.width"=0.0))
    
    # Name cluster indices
    names(h$clustering) <- features
    
    # Name isolation for each cluster.
    names(h$isolation) <- as.character(seq_along(features))
    
    # Adept silhouette info.
    h$silinfo$widths[, 1] <- as.numeric(seq_along(features))
    h$silinfo$widths[, 2] <- as.numeric(data.table::shift(seq_along(features), n=1L,type="lead", fill=1L))
    
    # Set class
    class(h) <- c("pam", "partition")
    
    return(h)
     
  } else {
    
    require_package(x="cluster",
                    purpose="to cluster similar features together")
    
    return(cluster::pam(x=dist_mat, k=n_clusters, keep.diss=FALSE, keep.data=FALSE))
  }
}



cluster.agnes <- function(dist_mat, linkage){
  
  require_package(x="cluster",
                  purpose="to cluster similar features together")
  
  # Compute agglomerative hierarchical clustering of the data set
  return(cluster::agnes(x=dist_mat, method=linkage, keep.diss=FALSE, keep.data=FALSE))
}



cluster.diana <- function(dist_mat){
  
  require_package(x="cluster",
                  purpose="to cluster similar features together")
  
  # Compute DIvisive ANAlysis hierarchical clustering of the data set
  return(cluster::diana(x=dist_mat, keep.diss=FALSE, keep.data=FALSE))
}



cluster.hclust <- function(dist_mat, linkage){
  # Hierarchical clustering

  # Convert general linkage names to stats::hclust linkage names.
  if(linkage == "ward")          {
    linkage <- "ward.D2"
    
  } else if(linkage == "weighted") {
    linkage <- "mcquitty"
  }
  
  if(is_package_installed("fastcluster")){
    return(fastcluster::hclust(d=dist_mat, method=linkage))
    
  } else {
    return(stats::hclust(d=dist_mat, method=linkage))
  }
  
}



cluster.find_representation <- function(dt, data_obj, dist_mat, feature_info_list, method){
  # Find representative feature(s) for the current cluster
  
  # Check if there are more than one feature in the cluster
  if(nrow(dt)==1){
    dt_clust <- data.table::copy(dt)
    return(dt_clust[, ":="("cluster_repr_feature"=dt$name[1], "inversion"=FALSE, "weight"=1.0)])
  }
  
  if(method == "first"){
    # Representation by the first feature in the cluster.
    dt_clust <- cluster.first_representation(dt=dt)
    
  } else if(method == "best_predictor"){
    # Representation by the best predictor
    dt_clust <- cluster.best_predictor_representation(dt=dt, data_obj=data_obj)
    
  } else if(method == "medioid"){
    # Representation by the cluster medioid
    dt_clust <- cluster.medioid_representation(dt=dt, dist_mat=dist_mat)
    
  } else if(method == "mean"){
    
    # Determine type of features in the current set.
    feature_types <- sapply(dt$name, function(feature) (feature_info_list[[feature]]@feature_type))
    
    if(any(feature_types == "factor")){
      # If any of the features is a factor, the "best_predictor" or "mediods" method should be used.
      # "mediods" corresponds a bit more with the intention of mean.
      dt_clust <- cluster.medioid_representation(dt=dt, dist_mat=dist_mat)
      
    } else {
      # Representation by the cluster mean
      dt_clust <- cluster.mean_representation(dt=dt, data_obj=data_obj)
    }
    
  } else {
    ..error_reached_unreachable_code(paste0("cluster.find_representation", "The cluster representation method was not recognised. Found: ", method))
  }
  
  return(dt_clust)
}



cluster.best_predictor_representation <- function(dt, data_obj){
  # Algorithm to find the best predictor in a cluster
  
  # Suppress NOTES due to non-standard evaluation in data.table
  name <- p_value <- cluster_repr_feature <-  NULL
  
  # Extract feature names
  features      <- dt$name
  
  # Select features
  data_obj      <- select_features(data=data_obj, features=features)
  
  # Aggregate data
  data_obj      <- aggregate_data(data=data_obj)
  
  # Calculate p-values of the features in the clusters
  regr_pval     <- compute_univariable_p_values(data_obj=data_obj, feature_columns=features)
  dt_regr       <- data.table::data.table("name"=names(regr_pval), "p_value"=regr_pval)
  
  # Replace non-finite p-values.
  dt_regr[!is.finite(p_value), "p_value":=1.0]
  
  # Combine by name to add cluster ids to the p-values
  dt_clust      <- merge(x=dt, y=dt_regr, by="name", all=TRUE)
  
  # Select the most predictive feature
  dt_clust[, "cluster_repr_feature":=dt_clust[p_value==min(p_value)]$name[1]]
  
  # Clean up and reorder dt_clust
  dt_clust[, ":="("p_value"=NULL)]
  
  # Add inversion and weight columns
  dt_clust[, ":="("inversion"=FALSE, "weight"=0.0)]
  dt_clust[name==cluster_repr_feature, "weight":=1.0]

  return(dt_clust)
}



cluster.first_representation <- function(dt){
  
  # Make a local copy of dt
  dt_clust <- data.table::copy(dt)
  
  # Add cluster_repr_feature column
  dt_clust[, ":="("cluster_repr_feature"=dt$name[1],
                  "inversion"=FALSE,
                  "weight"=0.0)]
  
  dt_clust[1, "weight":=1.0]
  
  return(dt_clust)
}



cluster.medioid_representation <- function(dt, dist_mat){
  # Suppress NOTES due to non-standard evaluation in data.table
  name_1 <- name_2 <- name <- cluster_repr_feature <- NULL
  
  # Convert the distance matrix to a long representation
  distance_table <- as.matrix(dist_mat)
  distance_table <- data.table::data.table(distance_table, keep.rownames=TRUE)
  data.table::setnames(distance_table, "rn", "name_1")
  distance_table <- melt(data=distance_table, id.vars="name_1", variable.name="name_2", value.name="distance")
  
  # Remove pairs were the same feature is compared
  distance_table <- distance_table[name_1 != name_2]
  
  # Find cluster medoids (which have minimal pairwise distance with the other features in the cluster)
  dt_clust <- dt[, cluster_repr_feature:=cluster.find_medoid(dt=dt, dist_table=distance_table)]

  # Add inversion and weight columns
  dt_clust[, ":="("inversion"=FALSE, "weight"=0.0)]
  dt_clust[name==cluster_repr_feature, "weight":=1.0]

  return(dt_clust)
}



cluster.mean_representation <- function(dt, data_obj){

  # Make a local copy of dt
  dt_clust <- data.table::copy(dt)
  
  # Now determine correlation between the selected representative feature and the other features
  feature_correlation <- sapply(dt$name, function(feature, repr_feature, data){
    return(stats::cor(x=data@data[[repr_feature]], y=data@data[[feature]], use="na.or.complete", method="spearman"))
  }, repr_feature=dt$name[1], data=data_obj)
  
  # Add cluster_repr_feature column
  dt_clust[, "cluster_repr_feature":=dt$name[1]]
  
  # Find required inversion (correlation < 0)
  dt_clust[, "inversion":=feature_correlation < 0.0]
  
  # Feature weight
  n_features <- nrow(dt_clust)
  dt_clust[, "weight":=1.0/n_features]
  
  return(dt_clust)
}



cluster.find_medoid <- function(dt, dist_table){

  # Suppress NOTES due to non-standard evaluation in data.table
  name_1 <- name_2 <- NULL

  get_average_pairwise_distance <- function(feat, pair_feat, dist_table){
    # Function for finding the pairwise distance between all features and feature "feat" in a cluster.

    # Extract all distances from combinations where pairs of with feature "feat" with features in the cluster "pair_feat" occur.
    # Since dt_dist only contains unique combinations, we have to switch around name and name_col columns for comparison.
    feat_dist <- c(dist_table[name_1==feat & name_2 %in% pair_feat]$distance)

    # Return the average pairwise distance
    return(mean(feat_dist, na.rm=TRUE))
  }

  # Return feature if it is alone in its cluster
  if(nrow(dt)==1){ return(dt$name[1]) }

  # Calculate mean pairwise distance for all features in a cluster
  mean_distance <- sapply(dt$name, get_average_pairwise_distance, pair_feat=dt$name, dist_table=dist_table)

  # Return name of the feature which has the minimal average pairwise distance to the other features
  return(names(which.min(mean_distance)))
}



cluster.compute_cluster <- function(cluster_table, data_obj){
  # data.table::data.table("name"=character(0), "cluster_name"=character(0), "invert"=logical(0), "weight"=logical(0)))
  
  # Suppress NOTES due to non-standard evaluation in data.table
  name <- weight <- NULL
  
  # Remove all entries with 0 weight
  cluster_table <- cluster_table[weight > 0.0]
  
  # Create a local copy of relevant data
  local_data <- data.table::copy(data_obj@data[, cluster_table$name, with=FALSE])

  # Numeric features
  if(all(cluster_table$type == "numeric")){
  
    # Determine the total weight
    total_weight <- sum(cluster_table$weight)
  
    for(ii in cluster_table$name){
      
      # Get entry for the table
      table_entry <- cluster_table[name==ii]
      
      # Set the weight factor
      weight_factor <- table_entry$weight / total_weight
      
      # Determine inversion 
      if(table_entry$invert){
        weight_factor <- -1.0 * weight_factor
      }
      
      # Update the value in the column
      data.table::set(local_data, j=ii, value=weight_factor * local_data[[ii]])
    }
    
    # Sum over columns
    cluster_data <- data.table::data.table("meta_feature"=rowSums(local_data))
    
    # Rename the column in cluster_data
    data.table::setnames(cluster_data, "meta_feature", cluster_table$cluster_name[1])
    
  } else if(all(cluster_table$type == "factor")){
    # Categorical features

    if(nrow(cluster_table) > 1){
      stop("Attempting to form cluster with more than one categorical features. This is not possible.")
    }
    
    # Copy cluster_data
    cluster_data <- local_data
    
    # Rename the column in cluster_data
    data.table::setnames(cluster_data, cluster_table$cluster_name[1])
    
  } else {
    # Mixed type: this should not occur.
    stop("Attempting to form cluster with mixed data types. This is not possible.")
  }
  
  return(cluster_data)
  
}







.get_available_cluster_methods <- function(){
  return(c("none", "pam", "agnes", "diana", "hclust"))
}



.get_available_linkage_methods <- function(){
  return(c("average", "single", "complete", "weighted", "ward"))
}



.get_available_cluster_cut_methods <- function(cluster_method){
  # Note that "none" is used to prevent clustering. It's not a valid choice per
  # se.
  
  if(cluster_method == "none"){
    cut_methods <- "none"
    
  } else if(cluster_method == "pam"){
    cut_methods <- c("none", "silhouette")
    
  } else if(cluster_method %in% c("agnes", "diana")){
    cut_methods <- c("none", "fixed_cut", "silhouette")
    
  } else if(cluster_method %in% c("hclust")){
    cut_methods <- c("none", "fixed_cut", "silhouette", "dynamic_cut")
    
  } else {
    ..error_reached_unreachable_code(paste0(".get_available_cluster_cut_methods: ",
                                            "encountered unknown cluster method: ",
                                            cluster_method))
  }
  
  return(cut_methods)
  
}



.get_available_cluster_representation_methods <- function(cluster_method){
  # Note that "none" is used to prevent forming cluster representations. It's
  # not a valid choice per se.
  
  if(cluster_method == "none"){
    representation_methods <- "none"
    
  } else if(cluster_method == "pam"){
    representation_methods <- c("none", "medioid")
    
  } else if(cluster_method %in% c("agnes", "diana", "hclust")){
    representation_methods <- c("none", "medioid", "best_predictor", "mean")
    
  } else {
    ..error_reached_unreachable_code(paste0(".get_available_cluster_representation_methods: ",
                                            "encountered unknown cluster method: ",
                                            cluster_method))
  }
  
  return(representation_methods)
}

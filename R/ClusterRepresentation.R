#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
#' @include Clustering.R
#' @include ClusteringMethod.R
NULL



#### add_feature_info_parameters (singularClusteringObject, dataObject) --------
setMethod("add_feature_info_parameters", signature(object="singularClusteringObject", data="dataObject"),
          function(object,
                   data,
                   ...){
            
            # Sanity check: the cluster is singular.
            if(length(object@cluster_features) != 1){
              ..error_reached_unreachable_code(paste0("add_feature_info_parameters,singularClusteringObject,dataObject: ",
                                                      "the number of features is expected to be 1. Found: ", length(object@cluster_features)))
            }
            
            # Create representation object for a singular cluster.
            representation_object <- methods::new("clusterRepresentationObject",
                                                  "name" = object@cluster_features,
                                                  "weight" = 1.0,
                                                  "invert" = FALSE,
                                                  "cluster_name" = object@cluster_features,
                                                  "cluster_size" = 1L,
                                                  "cluster_features" = object@cluster_features,
                                                  "required_features"= object@cluster_features)
            
            # Set as list.
            representation_object_list <- list(representation_object)
            
            # Add name to list elements.
            names(representation_object_list) <- sapply(representation_object_list, function(x) (x@name))
            
            return(representation_object_list)
          })



#### add_feature_info_parameters (clusteringObject, dataObject) ----------------
setMethod("add_feature_info_parameters", signature(object="clusteringObject", data="dataObject"),
          function(object,
                   data,
                   feature_info_list,
                   cluster_object,
                   ...){
            
            if(object@representation_method == "none"){
              # Form singular clusters
              representation_object_list <- .cluster_representation_by_singular_features(object=object)
              
            } else if(object@representation_method == "medioid"){
              # Represent clusters by the most central feature.
              representation_object_list <- .cluster_representation_by_medioid(object=object,
                                                                               cluster_object=cluster_object)
              
            } else if(object@representation_method == "best_predictor"){
              
            } else if(object@representation_method == "mean"){
              
            } else if(object@representation_method == "concordance"){
              # TODO implement.
            } else {
              ..error_reached_unreachable_code(paste0("add_feature_info_parameters,clusteringObject,dataObject: encountered an unknown representation method: ",
                                                      object@representation_method))
            }
            
            # Add name to list elements.
            names(representation_object_list) <- sapply(representation_object_list, function(x) (x@name))
            
            return(representation_object_list)
          })



.cluster_representation_by_singular_features <- function(object, ...){
  # This forms singular clusters.
  
  # Iterate over features to form singular clusters.
  representation_object_list <- lapply(object@cluster_features, function(current_feature){
    
    # Create representation object for a singular cluster.
    representation_object <- methods::new("clusterRepresentationObject",
                                          "name" = current_feature,
                                          "weight" = 1.0,
                                          "invert" = FALSE,
                                          "cluster_name" = current_feature,
                                          "cluster_size" = 1L,
                                          "cluster_features" = current_feature,
                                          "required_features"= current_feature)
    
    return(representation_object)
  })
  
  return(representation_object_list)
}



.cluster_representation_by_medioid <- function(object,
                                               cluster_object,
                                               ...){
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

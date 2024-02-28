#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
#' @include Clustering.R
#' @include ClusteringMethod.R
NULL



# add_feature_info_parameters (singularClusteringObject, dataObject) -----------
setMethod(
  "add_feature_info_parameters",
  signature(
    object = "singularClusteringObject",
    data = "dataObject"
  ),
  function(
    object,
    data,
    ...
  ) {
    
    # Sanity check: the cluster is singular.
    if (length(object@cluster_features) != 1L) {
      ..error_reached_unreachable_code(paste0(
        "add_feature_info_parameters,singularClusteringObject,dataObject: ",
        "the number of features is expected to be 1. Found: ", length(object@cluster_features)
      ))
    }
    
    # Create representation object for a singular cluster.
    representation_object <- methods::new(
      "clusterRepresentationObject",
      "name" = object@cluster_features,
      "weight" = 1.0,
      "invert" = FALSE,
      "cluster_name" = object@cluster_features,
      "cluster_size" = 1L,
      "cluster_features" = object@cluster_features,
      "required_features" = object@cluster_features
    )
    
    # Set as list.
    representation_object_list <- list(representation_object)
    
    # Add name to list elements.
    names(representation_object_list) <- sapply(
      representation_object_list,
      function(x) (x@name)
    )
    
    return(representation_object_list)
  }
)



# add_feature_info_parameters (clusteringObject, dataObject) -------------------
setMethod(
  "add_feature_info_parameters",
  signature(
    object = "clusteringObject",
    data = "dataObject"
  ),
  function(
    object,
    data,
    feature_info_list,
    cluster_method_object,
    ...
  ) {
    
    if (object@representation_method == "none") {
      # Form singular clusters
      representation_object_list <- .cluster_representation_by_singular_features(object = object)
      
    } else if (object@representation_method == "medioid") {
      # Represent clusters by the most central feature.
      representation_object_list <- .cluster_representation_by_medioid(
        object = object,
        cluster_object = cluster_method_object
      )
      
    } else if (object@representation_method == "best_predictor") {
      # Represent clusters by the best predictor.
      representation_object_list <- .cluster_representation_by_best_predictor(
        object = object,
        data = data
      )
      
    } else if (object@representation_method == "mean") {
      # Represent cluster as a meta-feature.
      representation_object_list <- .cluster_representation_as_mean_feature(
        object = object,
        data = data,
        feature_info_list = feature_info_list,
        cluster_object = cluster_method_object
      )
      
    } else if (object@representation_method == "concordance") {
      # TODO implement.
      
    } else {
      ..error_reached_unreachable_code(paste0(
        "add_feature_info_parameters,clusteringObject,dataObject: ",
        "encountered an unknown representation method: ", object@representation_method
      ))
    }
    
    # Add name to list elements.
    names(representation_object_list) <- sapply(
      representation_object_list,
      function(x) (x@name)
    )
    
    return(representation_object_list)
  }
)



.cluster_representation_by_singular_features <- function(object, ...) {
  # This forms singular clusters.
  
  # Iterate over features to form singular clusters.
  representation_object_list <- lapply(
    object@cluster_features,
    function(current_feature) {
      # Create representation object for a singular cluster.
      representation_object <- methods::new(
        "clusterRepresentationObject",
        "name" = current_feature,
        "weight" = 1.0,
        "invert" = FALSE,
        "cluster_name" = current_feature,
        "cluster_size" = 1L,
        "cluster_features" = current_feature,
        "required_features" = current_feature
      )
      
      return(representation_object)
    }
  )
  
  return(representation_object_list)
}



.cluster_representation_by_medioid <- function(
    object,
    cluster_object,
    ...
) {
  # Represent clusters by their medioid feature.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  element_1 <- element_2 <- value <- average_distance <- NULL
  
  # Get distance table.
  distance_table <- get_distance_table(
    object = cluster_object@similarity_table,
    include_diagonal = FALSE
  )
  
  # Determine whether the similarity table is for features (columns) or samples
  # (rows).
  element_names <- .get_cluster_data_type_element_name(
    data_type = cluster_object@data_type
  )
  
  # Change names in similarity table to a generic name.
  data.table::setnames(
    x = distance_table,
    old = element_names,
    new = c("element_1", "element_2")
  )
  
  # Limit distance table to only features in the cluster.
  distance_table <- distance_table[
    element_1 %in% object@cluster_features & element_2 %in% object@cluster_features
  ]
  
  # Find mean distance for each feature.
  distance_table <- distance_table[
    ,
    list("average_distance" = mean(value)),
    by = "element_1"
  ]
  
  # Find the feature to be used as the representative feature.
  representative_feature <- distance_table[
    average_distance == min(average_distance)
  ]$element_1[1L]
  
  # Iterate over features to form representation objects.
  representation_object_list <- lapply(
    object@cluster_features,
    function(current_feature, cluster_features, representative_feature) {
      # Create representation object for a singular cluster.
      representation_object <- methods::new(
        "clusterRepresentationObject",
        "name" = current_feature,
        "weight" = ifelse(current_feature == representative_feature, 1.0, 0.0),
        "invert" = FALSE,
        "cluster_name" = paste0(representative_feature, "_cluster"),
        "cluster_size" = length(cluster_features),
        "cluster_features" = cluster_features,
        "required_features" = representative_feature
      )
      
      return(representation_object)
    },
    cluster_features = object@cluster_features,
    representative_feature = representative_feature
  )
  
  return(representation_object_list)
}



.cluster_representation_by_best_predictor <- function(
    object,
    data,
    ...
) {
  # Represent clusters by the most predictive feature.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  p_value <-  NULL
  
  # Select features
  data <- select_features(
    data = data,
    features = object@cluster_features
  )
  
  # Aggregate data
  data <- aggregate_data(data = data)
  
  # Calculate p-values of the features in the clusters
  feature_p_values <- compute_univariable_p_values(
    data_obj = data,
    feature_columns = object@cluster_features
  )
  
  # Integrate in a data.table
  predictor_data <- data.table::data.table(
    "name" = names(feature_p_values),
    "p_value" = feature_p_values
  )
  
  # Replace non-finite p-values.
  predictor_data[!is.finite(p_value), "p_value" := 1.0]
  
  # Find the feature to be used as the representative feature.
  representative_feature <- predictor_data[p_value == min(p_value)]$name[1L]
  
  # Iterate over features to form representation objects.
  representation_object_list <- lapply(
    object@cluster_features,
    function(
      current_feature,
      cluster_features,
      representative_feature
    ) {
      # Create representation object for a singular cluster.
      representation_object <- methods::new(
        "clusterRepresentationObject",
        "name" = current_feature,
        "weight" = ifelse(current_feature == representative_feature, 1.0, 0.0),
        "invert" = FALSE,
        "cluster_name" = paste0(representative_feature, "_cluster"),
        "cluster_size" = length(cluster_features),
        "cluster_features" = cluster_features,
        "required_features" = representative_feature
      )
      
      return(representation_object)
    },
    cluster_features = object@cluster_features,
    representative_feature = representative_feature
  )
  
  return(representation_object_list)
}



.cluster_representation_as_mean_feature <- function(
    object,
    data,
    feature_info_list,
    ...
) {
  # Represent clusters by the mean
  
  # Check if any of the features is categorical - for these "mean" clusters can
  # not be realistically created.
  if (
    any(sapply(
      feature_info_list[object@cluster_features],
      function(x) (x@feature_type)
    ) == "factor")
  ) {
    return(.cluster_representation_by_medioid(
      object = object,
      ...
    ))
  }
  
  ..cluster_representation_as_mean_feature <- function(
    current_feature,
    feature_correlation,
    cluster_features
  ) {
    
    # Create representation object for a cluster meta-feature.
    representation_object <- methods::new(
      "clusterRepresentationObject",
      "name" = current_feature,
      "weight" = 1.0 / length(cluster_features),
      "invert" = feature_correlation < 0.0,
      "cluster_name" = paste0(cluster_features[1L], "_meta_cluster"),
      "cluster_size" = length(cluster_features),
      "cluster_features" = cluster_features,
      "required_features" = cluster_features
    )
    
    return(representation_object)
  }
  
  # Select features
  data <- select_features(data = data, features = object@cluster_features)
  
  # Compute Spearman correlation between the first feature and the other
  # features.
  feature_correlation <- sapply(
    data@data[, mget(object@cluster_features)],
    stats::cor,
    y = data@data[[object@cluster_features[1L]]],
    use = "na.or.complete",
    method = "spearman"
  )
  
  # Iterate over features to form representation objects.
  representation_object_list <- mapply(
    ..cluster_representation_as_mean_feature,
    current_feature = object@cluster_features,
    feature_correlation = feature_correlation,
    MoreArgs = list("cluster_features" = object@cluster_features),
    SIMPLIFY = FALSE
  )
  
  return(representation_object_list)
}

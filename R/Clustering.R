#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

# Cluster parameter object -----------------------------------------------------
setClass(
  "featureInfoParametersCluster",
  contains = "featureInfoParameters",
  slots = list(
    "type" = "character",
    "method" = "ANY",
    "weight" = "numeric",
    "invert" = "logical",
    "cluster_name" = "character",
    "cluster_size" = "integer",
    "cluster_features" = "character",
    "required_features" = "ANY"
  ),
  prototype = list(
    "type" = NA_character_,
    "method" = NULL,
    "weight" = 1.0,
    "invert" = FALSE,
    "cluster_name" = NA_character_,
    "cluster_size" = NA_integer_,
    "cluster_features" = NA_character_,
    "required_features" = NULL
  )
)


# Representation object --------------------------------------------------------
setClass(
  "clusterRepresentationObject",
  slots = list(
    "name" = "character",
    "weight" = "numeric",
    "invert" = "logical",
    "cluster_name" = "character",
    "cluster_size" = "integer",
    "cluster_features" = "character",
    "required_features" = "ANY"
  ),
  prototype = list(
    "weight" = NA_real_,
    "invert" = NA,
    "cluster_name" = NA_character_,
    "cluster_size" = NA_integer_,
    "cluster_features" = NA_character_,
    "required_features" = NULL
  )
)



create_cluster_parameter_skeleton <- function(
    feature_info_list,
    feature_names = NULL,
    cluster_method,
    cluster_linkage = NULL,
    cluster_cut_method = NULL,
    cluster_similarity_threshold = NULL,
    cluster_similarity_metric = NULL,
    cluster_representation_method = NULL,
    .override_existing = FALSE
) {
  # Creates a skeleton for the provided cluster method.
  
  # Determine feature names from the feature info list, if provided.
  if (is.null(feature_names)) feature_names <- names(feature_info_list)
  
  # Select only features that appear in the feature info list.
  feature_names <- intersect(names(feature_info_list), feature_names)
  
  # Skip step if no feature info objects are updated.
  if (is_empty(feature_names)) return(feature_info_list)
  
  # Check that method is applicable.
  .check_parameter_value_is_valid(
    x = cluster_method,
    var_name = "cluster_method",
    values = .get_available_cluster_methods()
  )
  
  # Update familiar info objects with a feature clustering skeleton.
  updated_feature_info <- fam_lapply(
    X = feature_info_list[feature_names],
    FUN = .create_cluster_parameter_skeleton,
    method = cluster_method,
    cluster_linkage = cluster_linkage,
    cluster_cut_method = cluster_cut_method,
    cluster_similarity_threshold = cluster_similarity_threshold,
    cluster_similarity_metric = cluster_similarity_metric,
    cluster_representation_method = cluster_representation_method,
    .override_existing = .override_existing
  )
  
  # Provide names for the updated feature info objects.
  names(updated_feature_info) <- feature_names
  
  # Replace list elements.
  feature_info_list[feature_names] <- updated_feature_info
  
  return(feature_info_list)
}



.create_cluster_parameter_skeleton <- function(
    feature_info,
    method,
    ...,
    .override_existing = FALSE
) {
  
  # Check if clustering data was already completed, and does not require being
  # determined anew.
  if (feature_info_complete(feature_info@cluster_parameters) && !.override_existing) {
    return(feature_info)
  }
  
  # Create cluster parameter object.
  object <- methods::new("featureInfoParametersCluster", method = method)
  
  # Set feature name
  object@name <- feature_info@name
  
  # Set feature type
  object@type <- feature_info@feature_type
  
  # Update the familiar version.
  object <- add_package_version(object = object)
  
  # Check that the feature was not removed and is not set as a signature.
  if (!is_available(feature_info) || is_in_signature(feature_info)) method <- "none"
  
  # Create the cluster method object.
  object@method <- create_cluster_method_object(
    cluster_method = method,
    data_type = "cluster",
    ...
  )
  
  # Update imputation_parameters slot.
  feature_info@cluster_parameters <- object
  
  return(feature_info)
}



add_cluster_info <- function(
    cl = NULL,
    feature_info_list,
    data,
    message_indent = 0L,
    verbose = FALSE
) {
  
  # Find feature columns.
  feature_names <- get_feature_columns(x = data)
  
  # Sanity check.
  if (!(setequal(feature_names, get_available_features(feature_info_list = feature_info_list)))) {
    ..error_reached_unreachable_code(paste0(
      "add_cluster_info: features in data and the feature info list are expected ",
      "to be the same, but were not."
    ))
  }
  
  # Skeletons should be present. Now we need to identify which features can be
  # grouped by their method slot. This is done by iterating, and identifying
  # which features form groups. Though this may seem to a bit convoluted, we do
  # this to allow for proper processing of features that may have different
  # parameters assigned on purpose, e.g. signature features or externally
  # provided features.
  
  # Eliminate features that are already complete.
  feature_names <- feature_names[!sapply(
    feature_info_list[feature_names],
    function(x) (feature_info_complete(x@cluster_parameters))
  )]
  
  # Skip any further processing if all parameter information has already been
  # completed.
  if (length(feature_names) == 0L) return(feature_info_list)  
  
  # Set unassigned features.
  unassigned_features <- feature_names
  
  # Iterate to eliminate any groups that would be smaller than 2.
  while (TRUE) {
    # Break once all features have been assigned.
    if (length(unassigned_features) == 0L) break
    
    # Get the cluster method object of the first feature that still needs to be
    # sorted.
    cluster_method_object <- feature_info_list[[unassigned_features[1L]]]@cluster_parameters@method
    
    # Find other unsorted features that have the same method.
    same_method_features <- unassigned_features[sapply(
      feature_info_list[unassigned_features],
      function(x, y) {
        return(identical(x@cluster_parameters@method, y))
      },
      y = cluster_method_object
    )]
    
    # Perform 2 checks:
    #
    # 1, Check that at least 2 features are present. Otherwise no sensible
    # clusters may be formed.
    #
    # 2, Check that there are at least 5 instances present. We cannot form
    # clusters when similarity is difficult to assess.
    if (length(same_method_features) < 2L || get_n_samples(data, id_depth = "repetition") < 5L) {
      feature_info_list[same_method_features] <- create_cluster_parameter_skeleton(
        feature_info_list[same_method_features],
        cluster_method = "none"
      )
    }
    
    # Update the number of features that have not been assigned.
    unassigned_features <- setdiff(unassigned_features, same_method_features)
  }
  
  # Set unassigned features and the initial grouping list
  unassigned_features <- feature_names
  
  # Iterate to create feature groups and add feature information.
  while (TRUE) {
    # Break once all features have been assigned.
    if (length(unassigned_features) == 0L) break()
    
    # Get the cluster method object of the first feature that still needs to be
    # sorted.
    cluster_method_object <- feature_info_list[[unassigned_features[1L]]]@cluster_parameters@method
    
    # Find other unsorted features that have the same method.
    same_method_features <- unassigned_features[sapply(
      feature_info_list[unassigned_features],
      function(x, y) {
        return(identical(x@cluster_parameters@method, y))
      },
      y = cluster_method_object
    )]
    
    # Message that computations are starting.
    if (cluster_method_object@method != "none") {
      logger_message(
        paste0(
          "Computing similarity between ", length(same_method_features), " features ",
          "using the ", cluster_method_object@similarity_metric, " metric ",
          "for clustering using the ", cluster_method_object@method, " method."
        ),
        indent = message_indent,
        verbose = verbose
      )
    }
    
    # Compute similarity.
    cluster_method_object <- set_similarity_table(
      object = cluster_method_object,
      data = filter_features(
        data = data,
        available_features = same_method_features
      ),
      feature_info_list = feature_info_list[same_method_features],
      cl = cl,
      verbose = verbose
    )
    
    # Create clustering objects. These are used to update the
    # feature_info_lists.
    clustering_objects <- unname(create_clusters(object = cluster_method_object))
    
    # Update feature info lists.
    updated_feature_info <- fam_lapply(
      cl = cl,
      X = clustering_objects,
      FUN = .add_cluster_info,
      cluster_method_object = cluster_method_object,
      feature_info_list = feature_info_list,
      data = data,
      progress_bar = FALSE
    )
    
    # Flatten lists with feature info.
    updated_feature_info <- unlist(updated_feature_info, recursive = FALSE)
    
    # Replace list elements without reordering.
    feature_info_list[same_method_features] <- updated_feature_info[same_method_features]
    
    # Remove features from the list of unassigned features.
    unassigned_features <- setdiff(unassigned_features, same_method_features)
  }
  
  return(feature_info_list)
}



.add_cluster_info <- function(
    clustering_object,
    cluster_method_object,
    feature_info_list,
    data
) {
  
  # Limit feature info list and data to the features in the cluster
  data <- filter_features(
    data = data,
    available_features = clustering_object@cluster_features
  )
  
  # Find representation.
  representation_objects <- add_feature_info_parameters(
    object = clustering_object,
    data = data,
    feature_info = feature_info_list[clustering_object@cluster_features],
    cluster_method_object = cluster_method_object
  )
  
  # Update cluster_parameters attribute in the feature info objects.
  updated_feature_info <- fam_mapply(
    FUN = ..add_cluster_info,
    feature_info = feature_info_list[clustering_object@cluster_features],
    representation_object = representation_objects[clustering_object@cluster_features]
  )
  
  # Update names.
  names(updated_feature_info) <- sapply(updated_feature_info, function(x) (x@name))
  
  return(updated_feature_info)
}



..add_cluster_info <- function(
    feature_info,
    representation_object) {
  
  # Use the representation object to update the featureInfoParametersCluster
  # object in cluster_parameters.
  object <- add_feature_info_parameters(
    object = feature_info@cluster_parameters,
    data = representation_object
  )
  
  # Attach updated information object.
  feature_info@cluster_parameters <- object
  
  return(feature_info)
}



# add_feature_info_parameters (cluster info, representation object) ------------
setMethod(
  "add_feature_info_parameters",
  signature(
    object = "featureInfoParametersCluster",
    data = "clusterRepresentationObject"
  ),
  function(object, data, ...) {
    
    # Sanity check: check that names are correct.
    if (object@name != data@name) {
      ..error_reached_unreachable_code(paste0(
        "add_feature_info_parameters,featureInfoParametersCluster,clusterRepresentationObject: ",
        "the cluster information object and representation object were not specified for ",
        "the same feature: ", object@name, " (info) and ", data@name, " (representation)"
      ))
    }
    
    # Copy contents
    object@weight <- data@weight
    object@invert <- data@invert
    object@cluster_name <- data@cluster_name
    object@cluster_size <- data@cluster_size
    object@cluster_features <- data@cluster_features
    object@required_features <- data@required_features
    
    # Set complete.
    object@complete <- TRUE
    
    return(object)
  }
)



# apply_feature_info_parameters (cluster info, dataObject) ---------------------
setMethod(
  "apply_feature_info_parameters",
  signature(
    object = "featureInfoParametersCluster",
    data = "dataObject"
  ),
  function(object, data, ...) {
    
    # Determine if the current feature is required, and skip if not.
    if (!object@name %in% object@required_features) return(NULL)
    
    # Determine if weight is not 0.0, and skip if it is.
    weight <- object@weight
    if (weight == 0.0) return(NULL)
    
    # Invert weights
    if (object@invert) weight <- -1.0 * weight
    
    # Only apply weights to numeric features.
    if (object@type == "numeric") {
      return(data@data[[object@name]] * weight)
      
    } else {
      return(data@data[[object@name]])
    }
  }
)



.create_clustering_table <- function(
    feature_info_list,
    selected_features = NULL,
    show_weights = FALSE) {
  
  # Select only requested features.
  if (!is.null(selected_features)) {
    feature_info_list <- feature_info_list[selected_features]
  }
  
  cluster_table <- lapply(
    feature_info_list,
    function(x, show_weights) {
      # If cluster parameters are not set, skip.
      if (is.null(x@cluster_parameters)) return(NULL)
      
      if (show_weights) {
        return(data.table::data.table(
          "cluster_name" = x@cluster_parameters@cluster_name,
          "feature_name" = x@cluster_parameters@cluster_features,
          "feature_required" = x@cluster_parameters@cluster_features %in% x@cluster_parameters@required_features,
          "weight" = x@cluster_parameters@weight
        ))
        
      } else {
        return(data.table::data.table(
          "cluster_name" = x@cluster_parameters@cluster_name,
          "feature_name" = x@cluster_parameters@cluster_features,
          "feature_required" = x@cluster_parameters@cluster_features %in% x@cluster_parameters@required_features
        ))
      }
    },
    show_weights = show_weights
  )
  
  # Remove duplicate entries.
  cluster_table <- unique(data.table::rbindlist(
    cluster_table,
    use.names = TRUE
  ))
  
  return(cluster_table)
}



features_before_clustering <- function(
    features,
    cluster_table = NULL,
    feature_info_list = NULL,
    representative_only = FALSE
) {
  # Convert input features to original features
  
  # Suppress NOTES due to non-standard evaluation in data.table
  cluster_name <- feature_required <- NULL
  
  # Create a cluster table if it is not provided
  if (is.null(cluster_table) && is.null(feature_info_list)) {
    ..error_reached_unreachable_code(paste0(
      "features_before_clustering: if no cluster_table is provided, ",
      "feature_info_list cannot be empty."
    ))
    
  } else if (is.null(cluster_table)) {
    cluster_table <- .create_clustering_table(
      feature_info_list = feature_info_list,
      selected_features = features
    )
  }
  
  # Find original features.
  if (representative_only) {
    original_features <- unique(
      cluster_table[cluster_name %in% features & feature_required == TRUE]$feature_name
    )
    
  } else {
    original_features <- unique(
      cluster_table[cluster_name %in% features]$feature_name
    )
  }
  
  # Return original features.
  return(original_features)
}



features_after_clustering <- function(
    features,
    cluster_table = NULL,
    feature_info_list = NULL
) {
  # Convert input features to features after clustering
  
  # Suppress NOTES due to non-standard evaluation in data.table
  feature_name <- NULL
  
  # Check if features are set.
  if (is_empty(features)) return(NULL)
  
  # Create a cluster table if it is not provided
  if (is.null(cluster_table) && is.null(feature_info_list)) {
    ..error_reached_unreachable_code(paste0(
      "features_after_clustering: if no cluster_table is provided, ",
      "feature_info_list may be empty."
    ))
    
  } else if (is.null(cluster_table)) {
    cluster_table <- .create_clustering_table(
      feature_info_list = feature_info_list,
      selected_features = features
    )
  }
  
  # Find and return feature names after clustering.
  return(unique(cluster_table[feature_name %in% features]$cluster_name))
}



set_clustered_data <- function(
    cluster_table,
    data,
    feature_info_list
) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  feature_required <- NULL
  
  # Only use required data.
  cluster_table <- cluster_table[feature_required == TRUE]
  
  # For singular clusters or clusters represented by a single feature, simply
  # return the data in the respective column.
  if (nrow(cluster_table) == 1L) return(data@data[[cluster_table$feature_name]])
  
  # Add weighted. Instantiate with 0s.
  feature_values <- numeric(nrow(data@data))
  
  # Iterate over features.
  for (current_feature in cluster_table$feature_name) {
    feature_values <- feature_values + apply_feature_info_parameters(
      object = feature_info_list[[current_feature]]@cluster_parameters,
      data = data
    )
  }
  
  return(feature_values)
}



.get_available_cluster_methods <- function() {
  return(c("none", "pam", "agnes", "diana", "hclust"))
}



.get_available_linkage_methods <- function(cluster_method) {
  if (cluster_method %in% c("agnes", "diana", "hclust")) {
    return(c("average", "single", "complete", "weighted", "ward"))
    
  } else {
    return("none")
  }
}



.get_available_cluster_cut_methods <- function(cluster_method) {
  # Note that "none" is used to prevent clustering. It's not a valid choice per
  # se.
  
  if (cluster_method == "none") {
    cut_methods <- "none"
    
  } else if (cluster_method == "pam") {
    cut_methods <- c("none", "silhouette")
    
  } else if (cluster_method %in% c("agnes", "diana")) {
    cut_methods <- c("none", "fixed_cut", "silhouette")
    
  } else if (cluster_method %in% c("hclust")) {
    cut_methods <- c("none", "fixed_cut", "silhouette", "dynamic_cut")
    
  } else {
    ..error_reached_unreachable_code(paste0(
      ".get_available_cluster_cut_methods: encountered unknown cluster method: ",
      cluster_method
    ))
  }
  
  return(cut_methods)
  
}



.get_available_cluster_representation_methods <- function(cluster_method) {
  # Note that "none" is used to prevent forming cluster representations. It's
  # not a valid choice per se.
  
  if (cluster_method == "none") {
    representation_methods <- "none"
    
  } else if (cluster_method == "pam") {
    representation_methods <- c("none", "medioid")
    
  } else if (cluster_method %in% c("agnes", "diana", "hclust")) {
    representation_methods <- c("none", "medioid", "best_predictor", "mean")
    
  } else {
    ..error_reached_unreachable_code(paste0(
      ".get_available_cluster_representation_methods: ",
      "encountered unknown cluster representation method: ",
      cluster_method
    ))
  }
  
  return(representation_methods)
}

#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

# Cluster method objects -------------------------------------------------------
setClass(
  "clusterMethod",
  slots = list(
    "method" = "character",
    "data_type" = "character",
    "cluster_cut_method" = "character",
    "representation_method" = "character",
    "similarity_table" = "ANY",
    "object" = "ANY"),
  prototype = list(
    "method" = NA_character_,
    "data_type" = NA_character_,
    "cluster_cut_method" = "none",
    "representation_method" = "none",
    "similarity_table" = NULL,
    "object" = NULL))

## clusterMethodHierarchical ---------------------------------------------------
setClass(
  "clusterMethodHierarchical",
  contains = "clusterMethod",
  slots = list(
    "similarity_metric" = "character",
    "similarity_threshold" = "numeric"),
  prototype = list(
    "similarity_metric" = NA_character_,
    "similarity_threshold" = NA_real_))

## clusterMethodHClust ---------------------------------------------------------
setClass(
  "clusterMethodHClust",
  contains = "clusterMethodHierarchical",
  slots = list("linkage_method" = "character"),
  prototype = list("linkage_method" = NA_character_))

## clusterMethodAgnes ----------------------------------------------------------
setClass(
  "clusterMethodAgnes",
  contains = "clusterMethodHierarchical",
  slots = list("linkage_method" = "character"),
  prototype = list("linkage_method" = NA_character_))

## clusterMethodDiana ----------------------------------------------------------
setClass(
  "clusterMethodDiana",
  contains = "clusterMethodHierarchical")

## clusterMethodPAM ------------------------------------------------------------
setClass(
  "clusterMethodPAM",
  contains = "clusterMethod",
  slots = list("similarity_metric" = "character"),
  prototype = list("similarity_metric" = "character"))

## clusterMethodNone -----------------------------------------------------------
setClass(
  "clusterMethodNone",
  contains = "clusterMethod")



# Similarity table object ------------------------------------------------------

## similarityTable -------------------------------------------------------------
setClass(
  "similarityTable",
  slots = list(
    "data" = "ANY",
    "similarity_metric" = "character",
    "data_type" = "character"),
  prototype = list(
    "data" = NULL,
    "similarity_metric" = NA_character_,
    "data_type" = NA_character_))

## noSimilarityTable -----------------------------------------------------------
setClass(
  "noSimilarityTable",
  slots = list(
    "data" = "ANY",
    "data_type" = "character"),
  prototype = list(
    "data" = NULL,
    "data_type" = NA_character_))



# clustering objects -----------------------------------------------------------

## clusteringObject ------------------------------------------------------------
setClass(
  "clusteringObject",
  slots = list(
    "representation_method" = "character",
    "cluster_features" = "character"),
  prototype = list(
    "representation_method" = "none",
    "cluster_features" = NA_character_))

## singularClusteringObject ----------------------------------------------------
setClass(
  "singularClusteringObject",
  slots = list("cluster_features" = "character"),
  prototype = list("cluster_features" = NA_character_))



create_cluster_method_object <- function(
    cluster_method,
    data_type,
    cluster_linkage = NULL,
    cluster_cut_method = NULL,
    cluster_similarity_threshold = NULL,
    cluster_similarity_metric = NULL,
    cluster_representation_method = NULL) {
  
  # Check that method is applicable.
  .check_parameter_value_is_valid(
    x = cluster_method,
    var_name = ifelse(
      data_type == "cluster",
      "cluster_method",
      paste0(data_type, "_cluster_method")),
    values = .get_available_cluster_methods())
  
  # Check that data_type is valid.
  .check_parameter_value_is_valid(
    x = data_type,
    var_name = "data_type",
    values = c("feature", "cluster", "sample"))
  
  if (cluster_method == "none") {
    object <- methods::new("clusterMethodNone")
    
  } else if (cluster_method == "pam") {
    object <- methods::new("clusterMethodPAM")
    
  } else if (cluster_method == "hclust") {
    object <- methods::new("clusterMethodHClust")
    
  } else if (cluster_method == "agnes") {
    object <- methods::new("clusterMethodAgnes")
    
  } else if (cluster_method == "diana") {
    object <- methods::new("clusterMethodDiana")
    
  } else {
    ..error_reached_unreachable_code(paste0(
      "create_cluster_method_object: encountered an unknown cluster method: ",
      cluster_method))
  }
  
  # Cluster method and data type are always set.
  object@method <- cluster_method
  object@data_type <- data_type
  
  # Set cluster object method parameters as required.
  object <- set_object_parameters(
    object = object,
    cluster_linkage = cluster_linkage,
    cluster_cut_method = cluster_cut_method,
    cluster_similarity_threshold = cluster_similarity_threshold,
    cluster_similarity_metric = cluster_similarity_metric,
    cluster_representation_method = cluster_representation_method)
  
  return(object)
}


# set_object_parameters methods ------------------------------------------------

## set_object_parameters (none) ------------------------------------------------
setMethod(
  "set_object_parameters",
  signature(object = "clusterMethodNone"),
  function(object, ...) {
    
    return(object)
  }
)



## set_object_parameters (general) ---------------------------------------------
setMethod(
  "set_object_parameters",
  signature(object = "clusterMethod"),
  function(
    object,
    cluster_cut_method = NULL,
    cluster_representation_method = NULL,
    ...) {
    
    # Check that data type is set correctly. This is used for many
    # checks.
    if (!object@data_type %in% c("feature", "sample", "cluster")) {
      ..error_reached_unreachable_code(paste0(
        "set_object_parameters,clusterMethod: the data_type attribute was not correctly set."))
    }
    
    # Cut methods are optional, and default to "none".
    if (!is.null(cluster_cut_method)) {
      
      # Check cluster cut method.
      .check_parameter_value_is_valid(
        x = cluster_cut_method,
        var_name = ifelse(
          object@data_type == "cluster",
          "cluster_cut_method",
          paste0(object@data_type, "_cluster_cut_method")),
        values = .get_available_cluster_cut_methods(object@method))
      
      # Set cluster cut method.
      object@cluster_cut_method <- cluster_cut_method
    }
    
    # Check representation method.
    if (!is.null(cluster_representation_method) && object@cluster_cut_method != "none") {
      
      # Check representation method.
      .check_parameter_value_is_valid(
        x = cluster_representation_method,
        var_name = ifelse(
          object@data_type == "cluster",
          "cluster_representation_method",
          paste0(object@data_type, "_cluster_representation_method")),
        values = .get_available_cluster_representation_methods(object@method))
      
      # Set cluster representation method.
      object@representation_method <- cluster_representation_method
    }
    
    return(object)
  }
)



## set_object_parameters (PAM) -------------------------------------------------
setMethod(
  "set_object_parameters",
  signature(object = "clusterMethodPAM"),
  function(
    object,
    cluster_similarity_metric,
    ...) {
    
    # Call the method for the parent class (clusterMethod) first.
    object <- methods::callNextMethod()
    
    # Check that similarity metric is valid.
    .check_parameter_value_is_valid(
      x = cluster_similarity_metric,
      var_name = paste0(object@data_type, "_similarity_metric"),
      values = .get_available_similarity_metrics(data_type = object@data_type))
    
    # Set similarity metric.
    object@similarity_metric <- cluster_similarity_metric
    
    return(object)
  }
)



## set_object_parameters (general hierarchical) --------------------------------
setMethod(
  "set_object_parameters",
  signature(object = "clusterMethodHierarchical"),
  function(
    object,
    cluster_similarity_metric,
    cluster_similarity_threshold = NULL,
    ...) {
    
    # Call the method for the parent class (clusterMethod) first.
    object <- methods::callNextMethod()
    
    # Check that similarity metric is valid.
    .check_parameter_value_is_valid(
      x = cluster_similarity_metric,
      var_name = paste0(object@data_type, "_similarity_metric"),
      values = .get_available_similarity_metrics(data_type = object@data_type))
    
    # Set similarity metric.
    object@similarity_metric <- cluster_similarity_metric
    
    if (object@cluster_cut_method %in% c("fixed_cut", "dynamic_cut")) {
      # Check cutting height for fixed cut. Multiple cut heights are
      # possible. Use as_distance to get two-value ranges, but note that
      # these values are similarity otherwise.
      sapply(
        cluster_similarity_threshold,
        .check_number_in_valid_range,
        var_name = paste0(object@data_type, "_similarity_threshold"),
        range = similarity.metric_range(
          similarity_metric = object@similarity_metric,
          as_distance = TRUE))
      
      # Attach to object.
      object@similarity_threshold <- cluster_similarity_threshold
    }
    
    return(object)
  }
)



## set_object_parameters (hclust) ----------------------------------------------
setMethod(
  "set_object_parameters",
  signature(object = "clusterMethodHClust"),
  function(
    object,
    cluster_linkage_method,
    ...) {
    
    # Call next method (clusterMethodHierarchical). This will also call the
    # method for its parent method (clusterMethod).
    object <- methods::callNextMethod()
    
    # Check that linkage method is valid.
    .check_parameter_value_is_valid(
      x = cluster_linkage_method,
      var_name = paste0(object@data_type, "_linkage_method"),
      values = .get_available_linkage_methods(cluster_method = object@method))
    
    # attach to object.
    object@linkage_method <- cluster_linkage_method
    
    return(object)
  }
)



## set_object_parameters (agnes) -----------------------------------------------
setMethod(
  "set_object_parameters",
  signature(object = "clusterMethodAgnes"),
  function(
    object,
    cluster_linkage_method,
    ...) {
    
    # Call next method (clusterMethodHierarchical). This will also call the
    # method for its parent method (clusterMethod).
    object <- methods::callNextMethod()
    
    # Check that linkage method is valid.
    .check_parameter_value_is_valid(
      x = cluster_linkage_method,
      var_name = paste0(object@data_type, "_linkage_method"),
      values = .get_available_linkage_methods(cluster_method = object@method))
    
    # attach to object.
    object@linkage_method <- cluster_linkage_method
    
    return(object)
  }
)



## set_object_parameters (diana) -----------------------------------------------
setMethod(
  "set_object_parameters",
  signature(object = "clusterMethodDiana"),
  function(object, ...) {
    
    # Call next method (clusterMethodHierarchical). This will also call the
    # method for its parent method (clusterMethod).
    object <- methods::callNextMethod()
    
    return(object)
  }
)


# set_similarity_table methods -------------------------------------------------

## set_similarity_table (missing, dataObject -----------------------------------
setMethod(
  "set_similarity_table",
  signature(
    object = "missing",
    data = "dataObject"),
  function(
    object,
    data,
    feature_info_list,
    similarity_metric,
    data_type,
    ...) {
    
    # For calls where we just want to create the similarity table, e.g.
    # in ..extract_feature_similarity. Here we create a generic
    # clusterMethodPAM object just to create the similarity table.
    object <- methods::new(
      "clusterMethodPAM",
      data_type = data_type,
      similarity_metric = similarity_metric)
    
    # Pass to set_similarity_table.
    object <- set_similarity_table(
      object = object,
      data = data,
      feature_info_list = feature_info_list,
      ...)
    
    # Return the table itself.
    return(object@similarity_table@data)
  }
)



## set_similarity_table (none, dataObject) -------------------------------------
setMethod(
  "set_similarity_table",
  signature(
    object = "clusterMethodNone",
    data = "dataObject"),
  function(
    object,
    data,
    feature_info_list,
    ...) {
    
    # Try to get similarity table
    similarity_table <- .set_similarity_table(
      object = object,
      data = data,
      feature_info_list = feature_info_list,
      ...)
    
    # Set the similarity_table attribute.
    object@similarity_table <- similarity_table
    
    return(object)
  }
)



## set_similarity_table (clusterMethod, dataObject) ----------------------------
setMethod(
  "set_similarity_table",
  signature(
    object = "clusterMethod",
    data = "dataObject"),
  function(
    object,
    data,
    feature_info_list,
    ...) {
    
    # Try to get similarity table
    similarity_table <- .set_similarity_table(
      object = object,
      data = data,
      feature_info_list = feature_info_list,
      ...)
    
    # If setting the similarity table does not work, we cannot create clusters.
    # Switch to "none" method instead.
    if (is.null(similarity_table)) {
      object <- create_cluster_method_object(
        cluster_method = "none",
        data_type = object@data_type)
      
      return(set_similarity_table(
        object = object,
        data = data,
        feature_info_list = feature_info_list))
    }
    
    # Set the similarity_table attribute.
    object@similarity_table <- similarity_table
    
    return(object)
  }
)


# .set_similarity_table methods ------------------------------------------------

## .set_similarity_table (none, dataObject) ------------------------------------
setMethod(
  ".set_similarity_table",
  signature(
    object = "clusterMethodNone",
    data = "dataObject"),
  function(
    object,
    data,
    feature_info_list,
    ...) {
    # Specific method for objects that indicate that no clustering should be
    # performed.
    
    # Check that the data are not empty.
    if (is_empty(data)) return(NULL)
    
    # Get feature columns.
    feature_columns <- get_feature_columns(data)
    
    # Sanity check.
    if (!(setequal(feature_columns, get_available_features(feature_info_list = feature_info_list)))) {
      ..error_reached_unreachable_code(paste0(
        ".set_similarity_table,clusterMethodNone,dataObject: features in data and the ",
        "feature info list are expect to be the same, but were not."))
    }
    
    if (object@data_type %in% c("cluster", "feature")) {
      similarity_data <- feature_columns
      
    } else if (object@data_type == "sample") {
      # Create unique row names.
      similarity_data <- get_unique_row_names(x = data)
      
    } else {
      ..error_reached_unreachable_code(paste0(
        ".set_similarity_table,clusterMethodNone,dataObject: ",
        "encountered an unknown data_type: ",
        object@data_type))
    }
    
    # Create (no) similarity table.
    similarity_table <- methods::new(
      "noSimilarityTable",
      "data" = similarity_data,
      "data_type" = object@data_type)
    
    
    return(similarity_table)
  }
)



## .set_similarity_table (clusterMethod, dataObject) ---------------------------
setMethod(
  ".set_similarity_table",
  signature(
    object = "clusterMethod",
    data = "dataObject"),
  function(
    object,
    data,
    feature_info_list,
    cl = NULL,
    verbose = FALSE,
    ...) {
    
    # Check that the data are not empty.
    if (is_empty(data)) return(NULL)
    
    # Get feature columns.
    feature_columns <- get_feature_columns(data)
    
    # Set the categorical mask.
    categorical_mask <- sapply(
      feature_info_list[feature_columns],
      function(x) (x@feature_type == "factor"))
    
    # Sanity check.
    if (!(setequal(feature_columns, get_available_features(feature_info_list = feature_info_list)))) {
      ..error_reached_unreachable_code(paste0(
        ".set_similarity_table,clusterMethod,dataObject: features in data and ",
        "the feature info list are expect to be the same, but were not."))
    }
    
    if (object@data_type %in% c("cluster", "feature")) {
      
      # Internal function for computing pair-wise similarity between
      # features.
      ..compute_similarity <- function(
        ii,
        combinations,
        data,
        similarity_metric,
        categorical_mask) {
        
        # Identify features that are being compared.
        feature_1 <- combinations[1, ii]
        feature_2 <- combinations[2, ii]
        
        # Compute pairwise similarity
        similarity <- similarity.compute_similarity(
          x = data[[feature_1]],
          y = data[[feature_2]],
          x_categorical = categorical_mask[feature_1],
          y_categorical = categorical_mask[feature_2],
          similarity_metric = similarity_metric)
        
        return(similarity)
      }
      
      # Check that the number of features is at least two. This is more
      # of technical requirement than anything else.
      if (length(feature_columns) < 2) return(NULL)
      
      # Generate all combinations of features
      combinations <- utils::combn(sort(feature_columns), 2)
      
      # Determine similarity measures for each feature pair.
      similarity <- fam_sapply(
        cl = cl,
        assign = NULL,
        X = seq_len(ncol(combinations)),
        FUN = ..compute_similarity,
        progress_bar = verbose,
        combinations = combinations,
        data = droplevels(data@data),
        similarity_metric = object@similarity_metric,
        categorical_mask = categorical_mask,
        chopchop = TRUE)
      
      # Transform similarity scores into a data.table.
      similarity_data  <- data.table::data.table(
        "feature_name_1" = combinations[1, ],
        "feature_name_2" = combinations[2, ],
        "value" = similarity)
      
    } else if (object@data_type == "sample") {
      
      # Internal function for computing pair-wise similarity between
      # instances.
      ..compute_similarity <- function(
        ii,
        combinations,
        data,
        similarity_metric,
        categorical_mask) {
        
        # Identify features that are being compared.
        row_1 <- combinations[1, ii]
        row_2 <- combinations[2, ii]
        
        # Compute pairwise similarity
        similarity <- similarity.compute_similarity(
          x = as.numeric(data[row_1, ]),
          y = as.numeric(data[row_2, ]),
          x_categorical = categorical_mask,
          y_categorical = categorical_mask,
          similarity_metric = similarity_metric)
        
        return(similarity)
      }
      
      # Determine if data requires normalisation
      if (similarity.requires_normalisation(similarity_metric = object@similarity_metric)) {
        # Identify numerical features
        numerical_features <- feature_columns[!categorical_mask]
        
        # Create a local copy of data.
        data@data <- data.table::copy(data@data)
        
        # Find the normalisation method.
        if (grepl(pattern = "_trim", x = object@similarity_metric, fixed = TRUE)) {
          norm_method <- "normalisation_trim"
          
        } else if (grepl(pattern = "_winsor", x = object@similarity_metric, fixed = TRUE)) {
          norm_method <- "normalisation_winsor"
          
        } else {
          norm_method <- "normalisation"
        }
        
        # Perform normalisation.
        for (ii in numerical_features) {
          data.table::set(
            data@data,
            j = ii,
            value = .normalise(
              x = data@data[[ii]],
              normalisation_method = norm_method,
              range = c(0, 1)))
        }
      }
      
      # Check that the number of rows is at least two. This is more
      # of technical requirement than anything else.
      if (nrow(data@data) < 2) return(NULL)
      
      # Generate all combinations of samples
      combinations <- utils::combn(seq_len(nrow(data@data)), 2)
      
      # Determine similarity measures for each sample pair.
      similarity <- fam_sapply(
        cl = cl,
        assign = NULL,
        X = seq_len(ncol(combinations)),
        FUN = ..compute_similarity,
        progress_bar = verbose,
        combinations = combinations,
        data = data@data[, mget(feature_columns)],
        similarity_metric = object@similarity_metric,
        categorical_mask = categorical_mask,
        chopchop = TRUE)
      
      # Create unique row names.
      row_names <- get_unique_row_names(x = data)
      
      # Transform similarity scores into a data.table.
      similarity_data  <- data.table::data.table(
        "sample_1" = row_names[combinations[1, ]],
        "sample_2" = row_names[combinations[2, ]],
        "value" = similarity)
      
    } else {
      ..error_reached_unreachable_code(paste0(
        ".set_similarity_table,clusterMethod,dataObject: encountered an unknown data_type: ",
        object@data_type))
    }
    
    # Create similarity table.
    similarity_table <- methods::new(
      "similarityTable",
      "data" = similarity_data,
      "similarity_metric" = object@similarity_metric,
      "data_type" = object@data_type)
    
    # Set similarity table,
    return(similarity_table)
  }
)


# get_similarity_names methods -------------------------------------------------

## get_similarity_names (NULL) -------------------------------------------------
setMethod(
  "get_similarity_names",
  signature(object = "NULL"),
  function(object, ...) {
    return(NULL)
  }
)



## get_similarity_names (similarityTable) --------------------------------------
setMethod(
  "get_similarity_names",
  signature(object = "similarityTable"),
  function(object, ...) {
    
    element_names <- NULL
    if (object@data_type %in% c("cluster", "feature")) {
      if (!is.null(object@data)) {
        element_names <- unique(c(
          object@data$feature_name_1,
          object@data$feature_name_2))
      }
      
    } else if (object@data_type == "sample") {
      if (!is.null(object@data)) {
        element_names <- unique(c(
          object@data$sample_1,
          object@data$sample_2))
      }
      
    } else {
      ..error_reached_unreachable_code(paste0(
        "get_similarity_names,similarityTable: encountered an unknown data_type: ",
        object@data_type))
    }
    
    return(element_names)
  }
)



## get_similarity_names (noSimilarityTable) ------------------------------------
setMethod(
  "get_similarity_names",
  signature(object = "noSimilarityTable"),
  function(object, ...) {
    
    element_names <- NULL
    if (object@data_type %in% c("cluster", "feature", "sample")) {
      if (!is.null(object@data)) element_names <- object@data
      
    } else {
      ..error_reached_unreachable_code(paste0(
        "get_similarity_names,noSimilarityTable: encountered an unknown data_type: ",
        object@data_type))
    }
    
    return(element_names)
  }
)



# get_distance_table methods ---------------------------------------------------

## get_distance_table (clusterMethod) ------------------------------------------
setMethod(
  "get_distance_table",
  signature(object = "noSimilarityTable"),
  function(object, ...) {
    
    # Check that similarity table is set.
    if (is.null(object@similarity_table)) {
      # Create similarity table first.
      object <- do.call(
        set_similarity_table,
        args = c(
          list("object" = object),
          list(...)))
    }
    
    # Push to get_distance_matrix for similarity tables.
    return(get_distance_table(
      object = object@similarity_table,
      ...))
  }
)



## get_distance_table (similarityTable) ----------------------------------------
setMethod(
  "get_distance_table",
  signature(object = "similarityTable"),
  function(
    object,
    include_diagonal = TRUE,
    ...) {
    
    # Suppress NOTES due to non-standard evaluation in data.table
    value <- NULL
    
    # Extract similarity table.
    similarity_metric <- object@similarity_metric
    
    # Copy data from the similarity table.
    lower_triangle <- data.table::copy(object@data)
    
    # Determine whether the similarity table is for features (columns)
    # or samples (rows).
    element_names <- .get_cluster_data_type_element_name(data_type = object@data_type)
    
    # Find elements from the distance table.
    elements <- union(
      lower_triangle[[element_names[1]]],
      lower_triangle[[element_names[2]]])
    
    # Convert similarity to distance.
    lower_triangle[, "value" := similarity.to_distance(
      x = value,
      similarity_metric = similarity_metric)]
    
    # Add in other triangle of the table by switching around the columns.
    upper_triangle <- data.table::copy(lower_triangle)
    data.table::setnames(
      upper_triangle,
      old = element_names,
      new = rev(element_names))
    
    if (include_diagonal) {
      # Create diagonals that always have distance 0.
      diagonal_table <- data.table::data.table(
        "element_1" = elements,
        "element_2" = elements,
        "value" = as.double(0))
      
      # Add names to the diagonal table.
      data.table::setnames(
        diagonal_table,
        old = c("element_1", "element_2"),
        new = element_names)
      
      # Combine to single, long, table
      distance_table <- rbind(
        lower_triangle,
        diagonal_table,
        upper_triangle)
      
    } else {
      # Combine upper and lower triangles.
      distance_table <- rbind(
        lower_triangle,
        upper_triangle)
    }
    
    return(distance_table)
  }
)



## get_distance_table (noSimilarityTable) --------------------------------------
setMethod(
  "get_distance_table",
  signature(object = "noSimilarityTable"),
  function(object, ...) {
    # For noSimilarityTable objects, return NULL.
    
    return(NULL)
  }
)


# get_distance_matrix methods --------------------------------------------------

## get_distance_matrix (clusterMethod) -----------------------------------------
setMethod(
  "get_distance_matrix",
  signature(object = "clusterMethod"),
  function(object, ...) {
    
    # Check that similarity table is set.
    if (is.null(object@similarity_table)) {
      # Create similarity table first.
      object <- do.call(
        set_similarity_table,
        args = c(
          list("object" = object),
          list(...)))
    }
    
    return(get_distance_matrix(object = object@similarity_table))
  }
)



## get_distance_matrix (similarityTable) ---------------------------------------
setMethod(
  "get_distance_matrix",
  signature(object = "similarityTable"),
  function(object, ...) {
    # Converts a similarity table into a distance matrix.
    
    # Determine whether the similarity table is for features (columns) or
    # samples (rows).
    element_names <- .get_cluster_data_type_element_name(data_type = object@data_type)
    
    # Convert a similarity table to a full distance table first.
    distance_table <- get_distance_table(object = object)
    
    # Create n x n table
    distance_table  <- data.table::dcast(
      distance_table,
      stats::as.formula(paste(element_names[1], "~", element_names[2])),
      value.var = "value")
    
    # Ensure that the diagonal is formed by the pairwise distance of the same
    # feature, i.e. 0.0.
    data.table::setcolorder(
      distance_table,
      neworder = c(element_names[1], as.character(distance_table[[element_names[1]]])))
    
    # Add rownames into the distance table -- I know. Blasphemy. Otherwise
    # as.dist doesn't function.
    rownames(distance_table) <- distance_table[[element_names[1]]]
    distance_table[, (element_names[1]) := NULL]
    
    # Create dissimilarity matrix
    distance_matrix <- stats::as.dist(distance_table)
    
    return(distance_matrix)
  }
)



## get_distance_matrix (noSimilarityTable) -------------------------------------
setMethod(
  "get_distance_matrix",
  signature(object = "noSimilarityTable"),
  function(object, ...) {
    
    return(NULL)
  }
)


# apply_cluster_method methods -------------------------------------------------

## apply_cluster_method (clusterMethod) ----------------------------------------
setMethod(
  "apply_cluster_method",
  signature(object = "clusterMethod"),
  function(object, ...) {
    # Generic method where no clustering object is generated.
    return(object)
  }
)




## apply_cluster_method (hclust) -----------------------------------------------
setMethod(
  "apply_cluster_method",
  signature(object = "clusterMethodHClust"),
  function(object, ...) {
    
    # Get distance matrix.
    distance_matrix <- do.call(
      get_distance_matrix,
      args = c(
        list("object" = object),
        list(...)))
    
    # Skip if the distance matrix is NULL.
    if (is.null(distance_matrix)) return(object)
    
    # Convert general linkage names to stats::hclust linkage names.
    linkage_method <- object@linkage_method
    if (object@linkage_method == "ward")          {
      linkage_method <- "ward.D2"
      
    } else if (linkage_method == "weighted") {
      linkage_method <- "mcquitty"
    }
    
    if (is_package_installed("fastcluster")) {
      object@object <- fastcluster::hclust(
        d = distance_matrix,
        method = linkage_method)
      
    } else {
      object@object <- stats::hclust(
        d = distance_matrix,
        method = linkage_method)
    }
    
    return(object)
  }
)



## apply_cluster_method (agnes) ------------------------------------------------
setMethod(
  "apply_cluster_method",
  signature(object = "clusterMethodAgnes"),
  function(object, ...) {
    
    # Get distance matrix.
    distance_matrix <- do.call(
      get_distance_matrix,
      args = c(
        list("object" = object),
        list(...)))
    
    # Skip if the distance matrix is NULL.
    if (is.null(distance_matrix)) return(object)
    
    require_package(
      x = "cluster",
      purpose = "to cluster similar features together")
    
    # Compute agglomerative hierarchical clustering of the data set
    object@object <- stats::as.hclust(
      cluster::agnes(
        x = distance_matrix,
        method = object@linkage_method,
        keep.diss = FALSE,
        keep.data = FALSE)
    )
    
    return(object)
  }
)



## apply_cluster_method (diana) ------------------------------------------------
setMethod(
  "apply_cluster_method",
  signature(object = "clusterMethodDiana"),
  function(object,  ...) {
    
    # Get distance matrix.
    distance_matrix <- do.call(
      get_distance_matrix,
      args = c(
        list("object" = object),
        list(...)))
    
    # Skip if the distance matrix is NULL.
    if (is.null(distance_matrix)) return(object)
    
    require_package(
      x = "cluster",
      purpose = "to cluster similar features together")
    
    # Compute DIvisive ANAlysis hierarchical clustering of the data set
    object@object <- stats::as.hclust(
      cluster::diana(
        x = distance_matrix,
        keep.diss = FALSE,
        keep.data = FALSE)
    )
    
    return(object)
  }
)



# create_clusters methods ------------------------------------------------------

## create_clusters (generic hierarchical) --------------------------------------
setMethod(
  "create_clusters",
  signature(object = "clusterMethodHierarchical"),
  function(
    object,
    as_cluster_objects = TRUE,
    ...) {
    
    cluster_table <- NULL
    if (object@cluster_cut_method == "silhouette") {
      # Cluster by silhouette.
      cluster_table <- .cluster_by_silhouette(object = object)
      
      # Attempt to create a cluster table if the cluster table returned empty.
      # Attempt to set singular clusters instead.
      if (is_empty(cluster_table)) {
        # Add names. Each cluster is singular.
        cluster_table <- .cluster_by_generic(object = object)
      }
      
    } else if (object@cluster_cut_method == "fixed_cut") {
      # Cluster by cutting the tree at a fixed height.
      cluster_table <- .cluster_by_fixed_cut(object = object)
      
      # Attempt to create a cluster table if the cluster table returned empty.
      # Attempt to set singular clusters instead.
      if (is_empty(cluster_table)) {
        # Add names. Each cluster is singular.
        cluster_table <- .cluster_by_generic(object = object)
      }
      
    } else if (object@cluster_cut_method == "none") {
      # Add names. Each cluster is singular.
      cluster_table <- .cluster_by_generic(object = object)
      
    } else {
      ..error_reached_unreachable_code(paste0(
        "create_clusters,clusterMethodHierarchical: encountered an unknown cluster cut method: ",
        object@cluster_method))
    }
    
    # Determine how to return the results.
    if (as_cluster_objects) {
      return(.convert_cluster_table_to_cluster_objects(
        cluster_table = cluster_table,
        representation_method = object@representation_method))
      
    } else {
      return(cluster_table)
    }
  }
)



## create_clusters (hclust) ----------------------------------------------------
setMethod(
  "create_clusters",
  signature(object = "clusterMethodHClust"),
  function(
    object,
    as_cluster_objects = TRUE,
    ...) {
    
    if (object@cluster_cut_method %in% c("silhouette", "fixed_cut")) {
      # Silhouette and fixed cut are implemented for the parent class
      # (clusterMethodHierarchical).
      return(callNextMethod())
      
    } else if (object@cluster_cut_method == "dynamic_cut") {
      # Cluster by cutting the tree at a fixed height.
      cluster_table <- .cluster_by_dynamic_cut(object = object)
      
      # Attempt to create a cluster table if the cluster table returned empty.
      # Attempt to set singular clusters instead.
      if (is_empty(cluster_table)) {
        # Add names. Each cluster is singular.
        cluster_table <- .cluster_by_generic(object = object)
      }
      
    } else if (object@cluster_cut_method == "none") {
      # Add names. Each cluster is singular.
      cluster_table <- .cluster_by_generic(object = object)
      
    } else {
      ..error_reached_unreachable_code(paste0(
        "create_clusters,clusterMethodHClust: encountered an unknown cluster cut method: ",
        object@cluster_method))
    }
    
    # Determine how to return the results.
    if (as_cluster_objects) {
      return(.convert_cluster_table_to_cluster_objects(
        cluster_table = cluster_table,
        representation_method = object@representation_method))
      
    } else {
      return(cluster_table)
    }
  }
)



## create_clusters (PAM) -------------------------------------------------------
setMethod(
  "create_clusters",
  signature(object = "clusterMethodPAM"),
  function(
    object,
    as_cluster_objects = TRUE,
    ...) {
    
    cluster_table <- NULL
    if (object@cluster_cut_method == "silhouette") {
      # Cluster by silhouette.
      cluster_table <- .cluster_by_silhouette(object = object)
      
      # Attempt to create a cluster table if the cluster table returned empty.
      # Attempt to set singular clusters instead.
      if (is_empty(cluster_table)) {
        # Add names. Each cluster is singular.
        cluster_table <- .cluster_by_generic(object = object)
      }
      
    } else if (object@cluster_cut_method == "none") {
      # Add names. Each cluster is singular.
      cluster_table <- .cluster_by_generic(object = object)
      
    } else {
      ..error_reached_unreachable_code(paste0(
        "create_clusters,clusterMethodPAM: encountered an unknown cluster cut method: ",
        object@cluster_method))
    }
    
    # Determine how to return the results.
    if (as_cluster_objects) {
      return(.convert_cluster_table_to_cluster_objects(
        cluster_table = cluster_table,
        representation_method = object@representation_method))
      
    } else {
      return(cluster_table)
    }
  }
)



## create_clusters (none) ------------------------------------------------------
setMethod(
  "create_clusters",
  signature(object = "clusterMethodNone"),
  function(
    object,
    as_cluster_objects = TRUE,
    ...) {
    
    # Add names. Each cluster is singular.
    cluster_table <- .cluster_by_generic(object = object)
    
    # Determine how to return the results.
    if (as_cluster_objects) {
      return(.convert_cluster_table_to_cluster_objects(
        cluster_table = cluster_table,
        representation_method = "none"))
      
    } else {
      return(cluster_table)
    }
  }
)



# .cluster_by_generic methods --------------------------------------------------

## .cluster_by_generic (generic hierarchical) -----------------------------------
setMethod(
  ".cluster_by_generic",
  signature(object = "clusterMethodHierarchical"),
  function(object, ...) {
    
    # Attempt to create the dendrogram.
    if (is.null(object@object)) object <- apply_cluster_method(object)
    
    # Find element names.
    element_names <- get_similarity_names(object@similarity_table)
    
    if (is.null(object@object)) {
      # A dendrogram wasn't formed, and we use the element names
      # instead.
      if (is.null(element_names)) {
        return(NULL)
        
      } else {
        # Set 
        return(data.table::data.table(
          "name" = element_names,
          "cluster_id" = seq_along(element_names),
          "label_order" = seq_along(element_names)))
      }
      
    } else {
      return(data.table::data.table(
        "name" = object@object$labels[object@object$order],
        "cluster_id" = seq_along(object@object$labels),
        "label_order" = seq_along(object@object$labels)))
    }
  }
)



## .cluster_by_generic (generic) -----------------------------------------------
setMethod(
  ".cluster_by_generic",
  signature(object = "clusterMethod"),
  function(object, ...) {
    
    # Find element names.
    element_names <- get_similarity_names(object@similarity_table)
    
    # Check that any element names are present.
    if (is.null(element_names)) return(NULL)
    
    return(data.table::data.table(
      "name" = element_names,
      "cluster_id" = seq_along(element_names),
      "label_order" = seq_along(element_names)))
  }
)



# .cluster_by_silhouette methods -----------------------------------------------

## .cluster_by_silhouette (PAM) ------------------------------------------------
setMethod(
  ".cluster_by_silhouette",
  signature(object = "clusterMethodPAM"),
  function(object, ...) {
    
    # Get distance matrix.
    distance_matrix <- do.call(
      get_distance_matrix,
      args = c(
        list("object" = object),
        list(...)))
    
    # Skip if the distance matrix is NULL.
    if (is.null(distance_matrix)) return(NULL)
    
    require_package(
      x = "cluster",
      purpose = "to cluster similar features together")
    
    # Determine optimal numbers of clusters based on silhouette
    n_clusters <- .optimise_cluster_silhouette(
      object = object,
      distance_matrix = distance_matrix)
    
    # PAM clustering doesn't like it when you n_clusters is equal to the number
    # of features.
    if (n_clusters == length(get_similarity_names(object@similarity_table))) return(NULL)
    
    # Create clustering.
    cluster_object <- cluster::pam(
      x = distance_matrix,
      k = n_clusters,
      keep.diss = FALSE,
      keep.data = FALSE)
    
    return(data.table::data.table(
      "name" = names(cluster_object$clustering),
      "cluster_id" = cluster_object$clustering,
      "label_order" = seq_along(cluster_object$clustering)))
  }
)



## .cluster_by_silhouette (generic hierarchical) -------------------------------
setMethod(
  ".cluster_by_silhouette",
  signature(object = "clusterMethodHierarchical"),
  function(object, ...) {
    
    # Suppress NOTES due to non-standard evaluation in data.table
    .NATURAL <- NULL
    
    # Get distance matrix.
    distance_matrix <- do.call(
      get_distance_matrix,
      args = c(
        list("object" = object),
        list(...)))
    
    # Skip if the distance matrix is NULL.
    if (is.null(distance_matrix)) return(NULL)
    
    # Attempt to create the dendrogram.
    if (is.null(object@object)) {
      object <- apply_cluster_method(object)
    }
    
    # Check if a dendrogram could be created.
    if (is.null(object@object)) return(NULL)
    
    require_package(
      x = "cluster",
      purpose = "to cluster similar features together by silhouette")
    
    # Determine optimal numbers of clusters based on silhouette
    n_clusters <- .optimise_cluster_silhouette(
      object = object,
      distance_matrix = distance_matrix)
    
    # PAM clustering doesn't like it when you n_clusters is equal to the
    # number of features.
    if (n_clusters == length(get_similarity_names(object@similarity_table))) return(NULL)
    
    # Cut the tree for the optimal number of clusters
    cluster_object <- stats::cutree(
      tree = stats::as.hclust(object@object),
      k = n_clusters)
    
    # Set initial cluster table.
    cluster_table <- data.table::data.table(
      "name" = names(cluster_object),
      "cluster_id" = cluster_object)
    
    # Get an ordering table.
    order_table <- data.table::data.table(
      "name" = object@object$labels[object@object$order],
      "label_order" = seq_along(object@object$labels))
    
    # Insert label order into the cluster table.
    cluster_table <- cluster_table[order_table, on = .NATURAL]
    
    return(cluster_table)
  }
)



.optimise_cluster_silhouette <- function(
    object,
    distance_matrix,
    tol = 0.01) {
  
  # Determine the number of features.
  n_features <- length(get_similarity_names(object@similarity_table))
  
  highly_similar_distance <- similarity.highly_similar(similarity_metric = object@similarity_metric)
  
  # Check problematic values.
  if (n_features == 1) {
    return(1L)
    
  } else if (n_features == 2) {
    
    if (all(distance_matrix <= highly_similar_distance)) {
      # Zero distance can be safely imputed as being identical.
      return(1L)
      
    } else {
      # Otherwise interpret as singular clusters.
      return(n_features)
    }
  }
  
  # If all elements have distance 0, return 1 cluster.
  if (all(distance_matrix <= highly_similar_distance)) return(1L)
  
  # The optimiser doesn't like a singular interval, which occurs for n_features
  # == 3.
  if (n_features == 3) return(2L)
  
  # Set k to test.
  k_test <- seq(2, n_features - 1)
  silhouette_score <- numeric(n_features - 2)
  silhouette_gradient <- numeric(max(c(0, n_features - 4)))
  
  for (k in k_test) {
    # Compute average silhouette.
    silhouette_score[k - 1] <- ..optimise_cluster_silhouette(
      k = k,
      distance_matrix = distance_matrix,
      object = object)
    
    # Compute gradient as a symmetric difference coefficient.
    if (k > 3) {
      silhouette_gradient[k - 3] <- 0.5 * (silhouette_score[k - 1] - silhouette_score[k - 3])
      
      # Stop if the gradient is 0 or negative 3 times.
      if (sum(silhouette_gradient[seq(1, k - 3)] <= 0.0) > 2) break
    }
  }
  
  # Set the optimal number of clusters.
  k_optimal <- head(k_test[silhouette_score > max(silhouette_score) - tol], n = 1L)
  max_silhouette <- silhouette_score[k_test == k_optimal]
  
  # Determine if the silhoutte indicates reasonable structure (> 0.50), see
  # Kaufman and Rousseeuw: Finding groups in data.
  if (max_silhouette < 0.50) k_optimal <- n_features
  
  return(k_optimal)
}



..optimise_cluster_silhouette <- function(
    k,
    distance_matrix,
    object) {
  
  # Suppress NOTES due to non-standard evaluation in data.table
  sil_width <- cluster_size <- NULL
  
  # Round k to integer value.
  k <- round(k, digits = 0)
  
  if (is(object, "clusterMethodPAM")) {
    # Generate partioning around medoids cluster
    cluster_object <- cluster::pam(
      x = distance_matrix,
      k = k,
      keep.diss = FALSE,
      keep.data = FALSE)
    
    # Extract silhouette table
    silhouette_table <- data.table::as.data.table(
      cluster_object$silinfo$widths,
      keep.rownames = FALSE)
    
    # Compute the size and average silhouette in each cluster
    silhouette_table <- silhouette_table[, list(
      "average_cluster_silhouette" = mean(sil_width),
      "cluster_size" = .N),
      by = "cluster"]
    
    # Maintain only non-singular clusters
    silhouette_table <- silhouette_table[cluster_size > 1]
    
    # Return average silhouette in the formed non-singular clusters.
    if (!is_empty(silhouette_table)) {
      return(mean(silhouette_table$average_cluster_silhouette))
      
    } else {
      return(0.0)
    }
    
  } else if (is(object, "clusterMethodHierarchical")) {
    
    # Compute silhouette.
    silhouette_matrix <- cluster::silhouette(
      x = stats::cutree(
        tree = stats::as.hclust(object@object),
        k = k),
      dist = distance_matrix)
    
    # Parse to matrix by changing the class. 
    class(silhouette_matrix) <- "matrix"
    
    # Extract silhouette table
    silhouette_table <- data.table::as.data.table(
      silhouette_matrix,
      keep.rownames = FALSE)
    
    # Compute the size and average silhouette in each cluster
    silhouette_table <- silhouette_table[, list(
      "average_cluster_silhouette" = mean(sil_width),
      "cluster_size" = .N),
      by = "cluster"]
    
    # Maintain only non-singular clusters
    silhouette_table <- silhouette_table[cluster_size > 1]
    
    # Return average silhouette in the formed non-singular clusters.
    if (!is_empty(silhouette_table)) {
      return(mean(silhouette_table$average_cluster_silhouette))
      
    } else {
      return(0.0)
    }
    
  } else {
    ..error_reached_unreachable_code(
      "..optimise_cluster_silhouette: unknown clustering object encountered.")
  }
}

# .cluster_by_fixed_cut methods ------------------------------------------------

## .cluster_by_fixed_cut (generic hierarchical) --------------------------------
setMethod(
  ".cluster_by_fixed_cut",
  signature(object = "clusterMethodHierarchical"),
  function(object, ...) {
    
    # Suppress NOTES due to non-standard evaluation in data.table
    .NATURAL <- NULL
    
    # Attempt to create the dendrogram.
    if (is.null(object@object)) object <- apply_cluster_method(object)
    
    # Check if a dendrogram could be created.
    if (is.null(object@object)) return(NULL)
    
    # Compute the height at which the dendrogram should be cut.
    cut_height <- similarity.to_distance(
      x = object@similarity_threshold,
      similarity_metric = object@similarity_metric)
    
    # Cut the dendrogram at the given height.
    cluster_object <- stats::cutree(
      tree = stats::as.hclust(object@object),
      h = cut_height)
    
    # Set initial cluster table.
    cluster_table <- data.table::data.table(
      "name" = names(cluster_object),
      "cluster_id" = cluster_object)
    
    # Get an ordering table.
    order_table <- data.table::data.table(
      "name" = object@object$labels[object@object$order],
      "label_order" = seq_along(object@object$labels))
    
    # Insert label order into the cluster table.
    cluster_table <- cluster_table[order_table, on = .NATURAL]
    
    return(cluster_table)
  }
)

# .cluster_by_dynamic_cut methods ----------------------------------------------

## .cluster_by_dynamic_cut (hclust) --------------------------------------------
setMethod(
  ".cluster_by_dynamic_cut",
  signature(object = "clusterMethodHClust"),
  function(object, ...) {
    
    # Attempt to create the dendrogram.
    if (is.null(object@object)) object <- apply_cluster_method(object)
    
    # Check if a dendrogram could be created.
    if (is.null(object@object)) return(NULL)
    
    require_package(
      x = "dynamicTreeCut",
      purpose = "to cluster similar features together through dynamic dendrogram cutting")
    
    # Compute the height at which the dendrogram should be cut anyway.
    cut_height <- similarity.to_distance(
      x = object@similarity_threshold,
      similarity_metric = object@similarity_metric)
    
    if (length(get_similarity_names(object@similarity_table)) == 2) {
      # For two features, dynamicTreeCut seems to ignore maxTreeHeight.
      if (similarity.to_distance(
        x = object@similarity_table@data$value,
        similarity_metric = object@similarity_metric) <= cut_height) {
        
        cluster_ids <- c(1L, 1L)
        
      } else {
        cluster_ids <- c(1L, 2L)
      }
      
    } else {
      # From Langfelder P, Zhang B, Horvath S (2007) Defining clusters from a
      # hierarchical cluster tree: the Dynamic Tree Cut package for R.
      # Bioinformatics 2008 24(5):719-720
      cluster_ids <- tryCatch(
        dynamicTreeCut::cutreeDynamicTree(
          dendro = object@object,
          maxTreeHeight = cut_height,
          deepSplit = TRUE,
          minModuleSize = 1),
        error = identity)
      
      # Check that dynamic cutting does not produce an error.
      if (inherits(cluster_ids, "error")) return(.cluster_by_fixed_cut(object, ...))
      
      # Order the cluster identifiers correctly.
      cluster_ids <- cluster_ids[object@object$order]
    }
    
    # Create a clustering table.
    cluster_table <- data.table::data.table(
      "name" = object@object$labels[object@object$order],
      "cluster_id" = cluster_ids,
      "label_order" = seq_along(object@object$labels))
    
    return(cluster_table)
  }
)




.convert_cluster_table_to_cluster_objects <- function(
    cluster_table,
    representation_method) {
  
  # Check that the cluster table is not empty.
  if (is_empty(cluster_table)) return(NULL)
  
  # Check that the expect columns are present.
  if (!(all(c("name", "cluster_id") %in% colnames(cluster_table)))) {
    ..error_reached_unreachable_code(paste0(
      ".convert_cluster_table_to_cluster_objects: expected name and ",
      "cluster_id columns were not found."))
  }
  
  return(lapply(
    split(cluster_table, by = "cluster_id"),
    ..convert_cluster_table_to_cluster_objects,
    representation_method = representation_method))
}



..convert_cluster_table_to_cluster_objects <- function(
    cluster_table,
    representation_method) {
  
  if (nrow(cluster_table) == 1) {
    # Create singular cluster object.
    object <- methods::new(
      "singularClusteringObject",
      cluster_features = cluster_table$name)
    
  } else {
    # Create cluster object with multiple features or instances.
    object <- methods::new(
      "clusteringObject",
      cluster_features = cluster_table$name,
      representation_method = representation_method)
  }
  
  return(object)
}



.check_cluster_parameters <- function(
    cluster_method,
    cluster_linkage = NULL,
    cluster_cut_method = NULL,
    cluster_similarity_threshold = NULL,
    cluster_similarity_metric = NULL,
    cluster_representation_method = NULL,
    data_type = "cluster",
    test_required_packages = TRUE,
    message_type = "error") {
  
  # Perform checks by creating the relevant object. This checks whether the
  # applicable parameters are set.
  object <- create_cluster_method_object(
    cluster_method,
    data_type = data_type,
    cluster_linkage = cluster_linkage,
    cluster_cut_method = cluster_cut_method,
    cluster_similarity_threshold = cluster_similarity_threshold,
    cluster_similarity_metric = cluster_similarity_metric,
    cluster_representation_method = cluster_representation_method)
  
  if (test_required_packages) {
    # Check whether the cluster package has been installed.
    if (object@method %in% c("pam", "agnes", "diana")) {
      require_package(
        x = "cluster",
        purpose = "to compute similarity between features or instances",
        message_type = message_type)
    }
    
    # Check whether the dynamicTreeCut package has been installed.
    if (.hasSlot(object, "cluster_cut_method")) {
      if (object@cluster_cut_method == "dynamic_cut") {
        require_package(
          x = "dynamicTreeCut",
          purpose = "to cut dendrograms dynamically",
          message_type = message_type)
      }
    }
    
    # Check whether the nnet package has been installed.
    if (.hasSlot(object, "similarity_metric")) {
      if (object@similarity_metric %in% c("mcfadden_r2", "cox_snell_r2", "nagelkerke_r2")) {
        require_package(
          x = "nnet",
          purpose = paste0(
            "to compute log-likelihood pseudo R2 similarity using the ",
            object@similarity_metric, " metric"),
          message_type = message_type)
      }
    }
  }
  
  return(invisible(TRUE))
}



.get_cluster_data_type_element_name <- function(data_type) {
  # Determine whether the similarity table is for features (columns)
  # or samples (rows).
  element_1 <- ifelse(data_type %in% c("feature", "cluster"), "feature_name_1", "sample_1")
  element_2 <- ifelse(data_type %in% c("feature", "cluster"), "feature_name_2", "sample_2")
  
  return(c(element_1, element_2))
}

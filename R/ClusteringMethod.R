#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

#### Cluster method objects ----------------------------------------------------
setClass("clusterMethod",
         slots=list("method" = "character",
                    "data_type" = "character",
                    "cluster_cut_method" = "character",
                    "representation_method"="character"),
         prototype=list("method" = NA_character_,
                        "data_type" = NA_character_,
                        "cluster_cut_method" = "none",
                        "representation_method"="none"))

setClass("clusterMethodHierarchical",
         contains="clusterMethod",
         slots=list("similarity_metric" = "character",
                    "similarity_threshold"="numeric"),
         prototype=list("similarity_metric" = NA_character_,
                        "similarity_threshold" = NA_real_))

setClass("clusterMethodHClust",
         contains="clusterMethodHierarchical",
         slots=list("linkage_method"="character"),
         prototype=list("linkage_method"=NA_character_))

setClass("clusterMethodAgnes",
         contains="clusterMethodHierarchical",
         slots=list("linkage_method"="character"),
         prototype=list("linkage_method"=NA_character_))

setClass("clusterMethodDiana",
         contains="clusterMethodHierarchical")

setClass("clusterMethodPAM",
         contains="clusterMethod",
         slots=list("similarity_metric" = "character"),
         prototype=list("similarity_metric" = "character"))

setClass("clusterMethodNone",
         contains="clusterMethod")

#### Similarity table ----------------------------------------------------------
setClass("similarityTable",
         slots=list("data"="ANY",
                    "similarity_metric"="character",
                    "data_type"="character"),
         prototype=list("data"=NULL,
                        "similarity_metric"=NA_character_,
                        "data_type"=NA_character_))

#### Cluster object ------------------------------------------------------------

create_cluster_method_object <- function(cluster_method,
                                         data_type,
                                         cluster_linkage=NULL,
                                         cluster_cut_method=NULL,
                                         cluster_similarity_threshold=NULL,
                                         cluster_similarity_metric=NULL,
                                         cluster_representation_method=NULL){
  
  
  # Check that method is applicable.
  .check_parameter_value_is_valid(x=cluster_method,
                                  var_name=ifelse(data_type=="cluster", "cluster_method", paste0(data_type, "_cluster_method")),
                                  values=.get_available_cluster_methods())
  
  # Check that data_type is valid.
  .check_parameter_value_is_valid(x=data_type,
                                  var_name="data_type",
                                  values=c("feature", "cluster", "sample"))
  
  if(cluster_method == "none"){
    object <- methods::new("clusterMethodNone")
    
  } else if(cluster_method == "pam"){
    object <- methods::new("clusterMethodPAM")
    
  } else if(cluster_method == "hclust"){
    object <- methods::new("clusterMethodHClust")
    
  } else if(cluster_method == "agnes"){
    object <- methods::new("clusterMethodAgnes")
    
  } else if(cluster_method == "diana"){
    object <- methods::new("clusterMethodDiana")
    
  } else {
    ..error_reached_unreachable_code(paste0("create_cluster_method_object: encountered an unknown cluster method: ", cluster_method))
  }
  
  # Cluster method and data type are always set.
  object@method <- cluster_method
  object@data_type <- data_type
  
  # Set cluster object method parameters as required.
  object <- set_object_parameters(object=object,
                                  cluster_linkage=cluster_linkage,
                                  cluster_cut_method=cluster_cut_method,
                                  cluster_similarity_threshold=cluster_similarity_threshold,
                                  cluster_similarity_metric=cluster_similarity_metric,
                                  cluster_representation_method=cluster_representation_method)
  browser()
  return(object)
}


#### set_object_parameters (none) ----------------------------------------------
setMethod("set_object_parameters", signature(object="clusterMethodNone"),
          function(object,
                   ...){
            
            return(object)
          })



#### set_object_parameters (general) -------------------------------------------
setMethod("set_object_parameters", signature(object="clusterMethod"),
          function(object,
                   cluster_cut_method=NULL,
                   cluster_representation_method=NULL,
                   ...){
            
            # Check that data type is set correctly. This is used for many
            # checks.
            if(!object@data_type %in% c("feature", "sample", "cluster")){
              ..error_reached_unreachable_code(paste0("set_object_parameters,clusterMethod: the data_type attribut was not correctly set."))
            }
            
            # Cut methods are optional, and default to "none".
            if(!is.null(cluster_cut_method)){
              
              # Check cluster cut method.
              .check_parameter_value_is_valid(x=cluster_cut_method,
                                              var_name=ifelse(object@data_type=="cluster",
                                                              "cluster_cut_method",
                                                              paste0(object@data_type, "_cluster_cut_method")),
                                              values=.get_available_cluster_cut_methods(object@method))
              
              # Set cluster cut method.
              object@cluster_cut_method <- cluster_cut_method
            }
            
            # Check representation method.
            if(!is.null(cluster_representation_method) && object@cluster_cut_method != "none"){
              
              # Check representation method.
              .check_parameter_value_is_valid(x=cluster_representation_method,
                                              var_name=ifelse(object@data_type=="cluster",
                                                              "cluster_representation_method",
                                                              paste0(object@data_type, "_cluster_representation_method")),
                                              values=.get_available_cluster_representation_methods(object@method))
              
              # Set cluster representation method.
              object@representation_method <- cluster_representation_method
            }
            
            return(object)
          })



#### set_object_parameters (PAM) -----------------------------------------------
setMethod("set_object_parameters", signature(object="clusterMethodPAM"),
          function(object,
                   cluster_similarity_metric,
                   ...){
            
            # Call the method for the parent class (clusterMethod) first.
            object <- methods::callNextMethod()
            
            # Check that similarity metric is valid.
            .check_parameter_value_is_valid(x=cluster_similarity_metric,
                                            var_name=paste0(object@data_type, "_similarity_metric"),
                                            values=.get_available_similarity_metrics(data_type=object@data_type))
            
            # Set similarity metric.
            object@similarity_metric <- cluster_similarity_metric
            
            return(object)
          })



#### set_object_parameters (general hierarchical) ------------------------------
setMethod("set_object_parameters", signature(object="clusterMethodHierarchical"),
          function(object,
                   cluster_similarity_metric,
                   cluster_similarity_threshold=NULL,
                   ...){
            # Call the method for the parent class (clusterMethod) first.
            object <- methods::callNextMethod()
            
            # Check that similarity metric is valid.
            .check_parameter_value_is_valid(x=cluster_similarity_metric,
                                            var_name=paste0(object@data_type, "_similarity_metric"),
                                            values=.get_available_similarity_metrics(data_type=object@data_type))
            
            # Set similarity metric.
            object@similarity_metric <- cluster_similarity_metric
            
            if(object@cluster_cut_method == c("fixed_cut")){
              
              # Check cutting height for fixed cut. Multiple cut heights are
              # possible. Use as_distance to get two-value ranges, but note that
              # these values are similarity otherwise.
              sapply(cluster_similarity_threshold,
                     .check_number_in_valid_range,
                     var_name=paste0(object@data_type, "_similarity_threshold"),
                     range=similarity.metric_range(similarity_metric=object@similarity_metric,
                                                   as_distance=TRUE))
              
              # Attach to object.
              object@similarity_threshold <- cluster_similarity_threshold
            }
            
            return(object)
          })



#### set_object_parameters (hclust) --------------------------------------------
setMethod("set_object_parameters", signature(object="clusterMethodHClust"),
          function(object,
                   cluster_linkage_method,
                   ...){
            
            # Call next method (clusterMethodHierarchical). This will also call
            # the method for its parent method (clusterMethod).
            object <- methods::callNextMethod()
            
            # Check that linkage method is valid.
            .check_parameter_value_is_valid(x=cluster_linkage_method,
                                            var_name=paste0(object@data_type, "_linkage_method"),
                                            values=.get_available_linkage_methods())
            
            return(object)
          })



#### set_object_parameters (agnes) ---------------------------------------------
setMethod("set_object_parameters", signature(object="clusterMethodAgnes"),
          function(object,
                   cluster_linkage_method,
                   ...){
            
            # Call next method (clusterMethodHierarchical). This will also call
            # the method for its parent method (clusterMethod).
            object <- methods::callNextMethod()
            
            # Check that linkage method is valid.
            .check_parameter_value_is_valid(x=cluster_linkage_method,
                                            var_name=paste0(object@data_type, "_linkage_method"),
                                            values=.get_available_linkage_methods())
            
            return(object)
          })



#### set_object_parameters (diana) ---------------------------------------------
setMethod("set_object_parameters", signature(object="clusterMethodDiana"),
          function(object,
                   ...){
            
            # Call next method (clusterMethodHierarchical). This will also call
            # the method for its parent method (clusterMethod).
            object <- methods::callNextMethod()
            
            return(object)
          })



#### get_distance_matrix (none, ANY) -------------------------------------------
setMethod("get_distance_matrix", signature(object="clusterMethodNone", data="ANY"),
          function(object, data, ...){
            # Specific method for objects that indicate that no clustering
            # should be performed..
            return(NULL)
          })



#### get_distance_matrix (clusterMethod, NULL) ---------------------------------
setMethod("get_distance_matrix", signature(object="clusterMethod", data="NULL"),
          function(object, data, ...){
            # Generic method to handle cases where the data are NULL, i.e. a
            # similarity table could not be computed.
            return(NULL)
          })



#### get_distance_matrix (clusterMethod, dataObject) ---------------------------
setMethod("get_distance_matrix", signature(object="clusterMethod", data="dataObject"),
          function(object,
                   data,
                   ...){
            
            # Create similarity table first.
            similarity_table <- do.call(get_similarity_table, args=c(list("object"=object,
                                                                          "data"=data),
                                                                     list(...)))
            
            # Push to get_distance_matrix for similarity tables.
            return(get_distance_matrix(object=object,
                                       data=similarity_table,
                                       ...))
          })


#### get_distance_matrix (clusterMethod, similarityTable) ----------------------
setMethod("get_distance_matrix", signature(object="clusterMethod", data="similarityTable"),
          function(object,
                   data,
                   ...){
            # Converts a similarity table into a distance matrix.
            # Suppress NOTES due to non-standard evaluation in data.table
            value <- NULL
            
            # Extract similarity table.
            similarity_metric <- data@similarity_metric
            
            # Copy data from the similarity table.
            lower_triangle <- data.table::copy(data@data)
            
            # Determine whether the similarity table is for features (columns)
            # or samples (rows).
            element_1 <- ifelse(data@data_type %in% c("feature", "cluster"), "feature_name_1", "sample_1")
            element_2 <- ifelse(data@data_type %in% c("feature", "cluster"), "feature_name_2", "sample_2")
            
            # Find elements from the distance table.
            elements <- union(lower_triangle[[element_1]],
                              lower_triangle[[element_2]])
            
            # Convert similarity to distance.
            lower_triangle[, "value":=similarity.to_distance(x=value,
                                                             similarity_metric=similarity_metric)]
            
            # Add in other triangle of the table by switching around the columns.
            upper_triangle <- data.table::copy(lower_triangle)
            data.table::setnames(upper_triangle,
                                 old=c(element_1, element_2),
                                 new=c(element_2, element_1))
            
            # Create diagonals that always have distance 0.
            diagonal_table <- data.table::data.table("element_1"=elements,
                                                     "element_2"=elements,
                                                     "value"=as.double(0))
            
            # Add names to the diagonal table.
            data.table::setnames(diagonal_table,
                                 old=c("element_1", "element_2"),
                                 new=c(element_1, element_2))
            
            # Combine to single, long, table
            distance_table  <- rbind(lower_triangle, diagonal_table, upper_triangle)
            
            # Create n x n table
            distance_table  <- data.table::dcast(distance_table,
                                                 stats::as.formula(paste(element_1, "~", element_2)),
                                                 value.var="value")
            
            # Add rownames into the distance table -- I know. Blasphemy.
            # Otherwise as.dist doesn't function.
            rownames(distance_table) <- distance_table[[element_1]]
            distance_table[, (element_1):=NULL]
            
            # Create dissimilarity matrix
            distance_matrix <- stats::as.dist(distance_table)
            
            return(distance_matrix)
          })



#### get_similarity_table (none, ANY) ------------------------------------------
setMethod("get_similarity_table", signature(object="clusterMethodNone", data="ANY"),
          function(object,
                   data,
                   ...){
            # Specific method for objects that indicate that no clustering
            # should be performed.
            return(NULL)
          })



#### get_similarity_table (clusterMethod, dataObject) --------------------------
setMethod("get_similarity_table", signature(object="clusterMethod", data="dataObject"),
          function(object,
                   data,
                   feature_info_list,
                   cl=NULL,
                   verbose=FALSE,
                   ...){
            
            # Check that the data are not empty.
            if(is_empty(data)) return(NULL)
            
            # Get feature columns.
            feature_columns <- get_feature_columns(data)
            
            # Set the categorical mask.
            categorical_mask <- sapply(feature_info_list[feature_columns],
                                       function(x) (x@feature_type == "factor"))
            
            # Sanity check.
            if(!(setequal(feature_columns, get_available_features(feature_info_list=feature_info_list)))){
              ..error_reached_unreachable_code(paste0("get_similarity_table,clusterMethod,dataObject:",
                                                      "features in data and the feature info list are expect to be the same, ",
                                                      "but were not."))
            }
            
            if(object@data_type %in% c("cluster", "feature")){
              
              # Internal function for computing pair-wise similarity between
              # features.
              ..compute_similarity <- function(ii, combinations, data, similarity_metric, categorical_mask){
                
                # Identify features that are being compared.
                feature_1 <- combinations[1, ii]
                feature_2 <- combinations[2, ii]
                
                # Compute pairwise similarity
                similarity <- similarity.compute_similarity(x=data[[feature_1]],
                                                            y=data[[feature_2]],
                                                            x_categorical=categorical_mask[feature_1],
                                                            y_categorical=categorical_mask[feature_2],
                                                            similarity_metric=similarity_metric)
                
                return(similarity)
              }
              
              # Check that the number of features is at least two. This is more
              # of technical requirement than anything else.
              if(length(feature_columns) < 2) return(NULL)
              
              # Generate all combinations of features
              combinations <- utils::combn(sort(feature_columns), 2)
              
              # Determine similarity measures for each feature pair.
              similarity <- fam_sapply(cl=cl,
                                       assign=NULL,
                                       X=seq_len(ncol(combinations)),
                                       FUN=..compute_similarity,
                                       progress_bar=verbose,
                                       combinations=combinations,
                                       data=droplevels(data@data),
                                       similarity_metric=object@similarity_metric,
                                       categorical_mask=categorical_mask,
                                       chopchop=TRUE)
              
              # Transform similarity scores into a data.table.
              similarity_data  <- data.table::data.table("feature_name_1"=combinations[1,],
                                                         "feature_name_2"=combinations[2,],
                                                         "value"=similarity)
              
            } else if(object@data_type == "sample"){
              
              # Internal function for computing pair-wise similarity between
              # instances.
              ..compute_similarity <- function(ii, combinations, data, similarity_metric, categorical_mask){
                
                # Identify features that are being compared.
                row_1 <- combinations[1, ii]
                row_2 <- combinations[2, ii]
                
                # Compute pairwise similarity
                similarity <- similarity.compute_similarity(x=as.numeric(data[row_1, ]),
                                                            y=as.numeric(data[row_2, ]),
                                                            x_categorical=categorical_mask,
                                                            y_categorical=categorical_mask,
                                                            similarity_metric=similarity_metric)
                
                return(similarity)
              }
              
              # Determine if data requires normalisation
              if(similarity.requires_normalisation(similarity_metric=similarity_metric)){
                # Identify numerical features
                numerical_features <- feature_columns[!categorical_mask]
                
                # Create a local copy of data.
                data@data <- data.table::copy(data@data)
                
                # Find the normalisation method.
                if(grepl(pattern="_trim", x=similarity_metric, fixed=TRUE)){
                  norm_method <- "normalisation_trim"
                  
                } else if(grepl(pattern="_winsor", x=similarity_metric, fixed=TRUE)){
                  norm_method <- "normalisation_winsor"
                  
                } else {
                  norm_method <- "normalisation"
                }
                
                # Perform normalisation.
                for(ii in numerical_features){
                  data.table::set(data@data,
                                  j=ii,
                                  value=.normalise(x=data@data[[ii]],
                                                   normalisation_method=norm_method,
                                                   range=c(0, 1)))
                }
              }
              
              # Check that the number of rows is at least two. This is more
              # of technical requirement than anything else.
              if(nrow(data@data) < 2) return(NULL)
              
              # Generate all combinations of samples
              combinations <- utils::combn(seq_len(nrow(data@data)), 2)
              
              # Determine similarity measures for each sample pair.
              similarity <- fam_sapply(cl=cl,
                                       assign=NULL,
                                       X=seq_len(ncol(combinations)),
                                       FUN=..compute_similarity,
                                       progress_bar=verbose,
                                       combinations=combinations,
                                       data=data@data[, mget(feature_columns)],
                                       similarity_metric=object@similarity_metric,
                                       categorical_mask=categorical_mask,
                                       chopchop=TRUE)
              
              # Create unique row names.
              row_names <- get_unique_row_names(x=data)
              
              # Transform similarity scores into a data.table.
              similarity_data  <- data.table::data.table("sample_1"=row_names[combinations[1, ]],
                                                         "sample_2"=row_names[combinations[2, ]],
                                                         "value"=similarity)
              
            } else {
              ..error_reached_unreachable_code(paste0("get_similarity_table,clusterMethod,dataObject:",
                                                      "encountered an unknown data_type: ",
                                                      object@data_type))
            }
            
            # Create similarity table.
            similarity_table <- methods::new("similarityTable",
                                             "data"=similarity_data,
                                             "similarity_metric"=object@similarity_metric,
                                             "data_type"=object@data_type)
            
            return(similarity_table)
          })


#### create_cluster_object (none, ANY) -----------------------------------------

#### create_cluster_object (clusterMethod, NULL) -------------------------------

#### create_cluster_object (clusterMethod, dataObject) -------------------------

#### create_cluster_object (clusterMethod, similarityTable) --------------------

#### create_cluster_object (clusterMethod, dist) -------------------------------



.check_cluster_parameters <- function(cluster_method,
                                      cluster_linkage=NULL,
                                      cluster_cut_method=NULL,
                                      cluster_similarity_threshold=NULL,
                                      cluster_similarity_metric=NULL,
                                      cluster_representation_method=NULL,
                                      data_type="cluster",
                                      test_required_packages=TRUE,
                                      message_type="error"){
  
  # Perform checks by creating the relevant object. This checks whether the
  # applicable parameters are set.
  object <- create_cluster_method_object(cluster_method,
                                         data_type=data_type,
                                         cluster_linkage=cluster_linkage,
                                         cluster_cut_method=cluster_cut_method,
                                         cluster_similarity_threshold=cluster_similarity_metric,
                                         cluster_similarity_metric=cluster_similarity_metric,
                                         cluster_representation_method=cluster_representation_method)
  
  if(test_required_packages){
    # Check whether the cluster package has been installed.
    if(object@method %in% c("pam", "agnes", "diana")){
      require_package(x="cluster",
                      purpose="to compute similarity between features or instances",
                      message_type=message_type)
    }
    
    # Check whether the dynamicTreeCut package has been installed.
    if(.hasSlot(object, "cluster_cut_method")){
      if(object@cluster_cut_method == "dynamic_cut"){
        require_package(x="dynamicTreeCut",
                        purpose="to cut dendrograms dynamically",
                        message_type=message_type)
        
      }
    }
    
    # Check whether the VGAM package has been installed.
    if(.hasSlot(object, "similarity_metric")){
      if(object@similarity_metric %in% c("mcfadden_r2", "cox_snell_r2", "nagelkerke_r2")){
        require_package(x="VGAM",
                        purpose=paste0("to compute log-likelihood pseudo R2 similarity using the ",
                                       object@similarity_metric, " metric"),
                        message_type=message_type)
      }
    }
  }
}

#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarDataElementFeatureSimilarity",
         contains="familiarDataElement",
         slots=list("similarity_metric"="character",
                    "cluster_method"="character",
                    "linkage_method"="character",
                    "cluster_cut_method"="character",
                    "similarity_threshold"="ANY",
                    "dendrogram"="ANY"),
         prototype = methods::prototype(detail_level="ensemble",
                                        similarity_metric=NA_character_,
                                        cluster_method=NA_character_,
                                        linkage_method=NA_character_,
                                        cluster_cut_method=NA_character_,
                                        similarity_threshold=NULL,
                                        dendrogram=NULL,
                                        value_column="value",
                                        grouping_column=c("feature_name_1", "feature_name_2")))

#'@title Internal function to extract the feature distance table.
#'
#'@description Computes and extracts the feature distance table for features
#'  used in a `familiarEnsemble` object. This table can be used to cluster
#'  features, and is exported directly by `export_feature_similarity`.
#'
#'@inheritParams extract_data
#'
#'@return A data.table containing pairwise distance between features. This data
#'  is only the upper triangular of the complete matrix (i.e. the sparse
#'  unitriangular representation). Diagonals will always be 0.0 and the lower
#'  triangular is mirrored.
#'@md
#'@keywords internal
setGeneric("extract_feature_similarity",
           function(object,
                    data,
                    cl=NULL,
                    estimation_type=waiver(),
                    aggregate_results=waiver(),
                    confidence_level=waiver(),
                    bootstrap_ci_method=waiver(),
                    is_pre_processed=FALSE,
                    feature_cluster_method=waiver(),
                    feature_linkage_method=waiver(),
                    feature_cluster_cut_method=waiver(),
                    feature_similarity_threshold=waiver(),
                    feature_similarity_metric=waiver(),
                    verbose=FALSE,
                    message_indent=0L,
                    ...) standardGeneric("extract_feature_similarity"))

#####extract_feature_similarity#####
setMethod("extract_feature_similarity", signature(object="familiarEnsemble", data="dataObject"),
          function(object,
                   data,
                   cl=NULL,
                   estimation_type=waiver(),
                   aggregate_results=waiver(),
                   confidence_level=waiver(),
                   bootstrap_ci_method=waiver(),
                   is_pre_processed=FALSE,
                   feature_cluster_method=waiver(),
                   feature_linkage_method=waiver(),
                   feature_cluster_cut_method=waiver(),
                   feature_similarity_threshold=waiver(),
                   feature_similarity_metric=waiver(),
                   verbose=FALSE,
                   message_indent=0L,
                   ...){
            
            
            # Message extraction start
            logger.message(paste0("Computing pairwise similarity between features."),
                           indent=message_indent,
                           verbose=verbose)
            
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
                                      data_type="feature")
            
            # Obtain confidence level from the settings file stored with the
            # familiarEnsemble object.
            if(is.waive(confidence_level)) confidence_level <- object@settings$confidence_level
            
            # Check alpha
            .check_number_in_valid_range(confidence_level, var_name="confidence_level",
                                         range=c(0.0, 1.0), closed=c(FALSE, FALSE))
            
            # Load the bootstrap method
            if(is.waive(bootstrap_ci_method)){
              bootstrap_ci_method <- object@settings$bootstrap_ci_method
            }
            
            .check_parameter_value_is_valid(x=bootstrap_ci_method, var_name="bootstrap_ci_methpd",
                                            values=.get_available_bootstrap_confidence_interval_methods())
            
            # Check the estimation type.
            estimation_type <- .parse_estimation_type(x = estimation_type,
                                                      object = object,
                                                      default = "point",
                                                      data_element = "feature_similarity",
                                                      detail_level = "ensemble",
                                                      has_internal_bootstrap = TRUE)
            
            # Check whether results should be aggregated.
            aggregate_results <- .parse_aggregate_results(x = aggregate_results,
                                                          object = object,
                                                          default = TRUE,
                                                          data_element = "feature_similarity")
            
            # Generate a prototype data element.
            proto_data_element <- new("familiarDataElementFeatureSimilarity",
                                      estimation_type = estimation_type,
                                      confidence_level = confidence_level,
                                      bootstrap_ci_method = bootstrap_ci_method,
                                      similarity_metric=feature_similarity_metric,
                                      cluster_method=feature_cluster_method,
                                      linkage_method=feature_linkage_method,
                                      cluster_cut_method=feature_cluster_cut_method,
                                      similarity_threshold=feature_similarity_threshold)
            
            # Generate elements to send to dispatch.
            similarity_data <- extract_dispatcher(FUN=.extract_feature_similarity,
                                                  has_internal_bootstrap=TRUE,
                                                  cl=cl,
                                                  object=object,
                                                  data=data,
                                                  proto_data_element=proto_data_element,
                                                  is_pre_processed=is_pre_processed,
                                                  aggregate_results=aggregate_results,
                                                  message_indent=message_indent + 1L,
                                                  verbose=verbose)
            
            return(similarity_data)
          })



.extract_feature_similarity <- function(object,
                                        data,
                                        proto_data_element,
                                        cl=NULL,
                                        is_pre_processed,
                                        progress_bar=FALSE,
                                        aggregate_results,
                                        n_bootstraps,
                                        message_indent=0L,
                                        verbose=FALSE,
                                        ...){
  
  # Add the name of the ensemble model
  data_element <- add_model_name(data=proto_data_element, object=object)
  
  # Retrieve input data.
  data <- process_input_data(object=object,
                             data=data,
                             stop_at="imputation",
                             is_pre_processed=is_pre_processed)
  
  # Check if the input data is not empty
  if(is_empty(data)) return(NULL)
  
  # Check if the number of samples is sufficient (>5), and return an
  # empty table if not.
  if(data.table::uniqueN(data@data, by=get_id_columns(id_depth="series")) <= 5) return(data_element)
  
  # Maintain only important features. The current set is based on the
  # required features.
  data <- filter_features(data=data,
                          available_features=object@model_features)
  
  # Identify eligible columns.
  feature_columns <- get_feature_columns(x=data)
  
  # Break if there are not at least 2 features present between which
  # correlation can be compared.
  if(length(feature_columns) < 2) return(data_element)
  
  # Add bootstrap data.
  bootstrap_data <- add_data_element_bootstrap(x=data_element,
                                               n_bootstraps=n_bootstraps,
                                               ...)
  if(n_bootstraps > 1){
    
    # Iterate over elements.
    data_elements <- fam_mapply(cl=cl,
                                assign=NULL,
                                FUN=..extract_feature_similarity,
                                data_element=bootstrap_data$data_element,
                                bootstrap=bootstrap_data$bootstrap,
                                bootstrap_seed = bootstrap_data$seed,
                                MoreArgs=list("data"=data,
                                              "feature_info_list"=object@feature_info),
                                progress_bar=progress_bar,
                                chopchop=TRUE)
    
  } else {
    data_elements <- fam_mapply(cl=NULL,
                                assign=NULL,
                                FUN=..extract_feature_similarity,
                                data_element=bootstrap_data$data_element,
                                bootstrap=bootstrap_data$bootstrap,
                                bootstrap_seed = bootstrap_data$seed,
                                MoreArgs=list("data"=data,
                                              "feature_info_list"=object@feature_info,
                                              "cl"=cl,
                                              "verbose"=verbose,
                                              "message_indent"=message_indent),
                                progress_bar=FALSE)
    
  }
  
  # Merge data elements
  data_elements <- merge_data_elements(data_elements)
  
  if(aggregate_results) data_elements <- .compute_data_element_estimates(x=data_elements)
  
  return(data_elements)
}



..extract_feature_similarity <- function(cl=NULL,
                                         data_element,
                                         data,
                                         feature_info_list,
                                         bootstrap,
                                         bootstrap_seed,
                                         message_indent=0L,
                                         verbose=FALSE){
  
  # Bootstrap the data.
  if(bootstrap) data <- get_bootstrap_sample(data=data,
                                             seed=bootstrap_seed)
  
  # Check if the number of samples is sufficient (>5), and return an
  # empty table if not.
  if(data.table::uniqueN(data@data, by=get_id_columns(id_depth="series")) <= 5) return(NULL)
  
  # Identify eligible columns.
  feature_columns <- get_feature_columns(x=data)
  
  # Compute the similarity table
  data_element@data <- set_similarity_table(data=data,
                                            feature_info_list=feature_info_list[feature_columns],
                                            similarity_metric=data_element@similarity_metric,
                                            data_type="feature",
                                            cl=cl,
                                            message_indent=message_indent + 1L,
                                            verbose=verbose)
  
  return(data_element)
}            



.append_feature_similarity_dendrogram <- function(x){
  
  if(is_empty(x)) return(x)
  
  # Create a cluster method object using data stored in x.
  cluster_method_object <- .create_feature_similarity_cluster_method_object(x=x)
  
  if(is.null(cluster_method_object)) return(x)
  
  # Create the cluster object.
  object <- apply_cluster_method(object=cluster_method_object)
  
  # Attach to data element.
  x@dendrogram <- object@object
  
  return(x)
}



.append_feature_similarity_clusters <- function(x){
  
  if(is_empty(x)) return(x)
  
  # Generate the clustering table.
  cluster_table <- .compute_feature_similarity_cluster_table(x=x)
  
  # Check for empty cluster tables.
  if(is_empty(cluster_table)) return(x)
  
  # Keep only name and cluster_id columns
  cluster_table <- cluster_table[, mget(c("name", "cluster_id"))]
  
  # Compute cluster size
  cluster_table[, "cluster_size":=.N, by="cluster_id"]
  
  # Rename name column to 
  data.table::setnames(cluster_table,
                       old="name",
                       new="feature")
  
  # Set cluster info as data.
  x@data <- cluster_table
  
  # Reset value and grouping columns.
  x@value_column <- c("cluster_id", "cluster_size")
  x@grouping_column <- "feature"
  
  return(x)
}



.append_feature_similarity_clustering <- function(x){
  
  if(is_empty(x)) return(x)
  
  # Generate the clustering table.
  cluster_table <- .compute_feature_similarity_cluster_table(x=x)
  
  # Check for empty cluster tables.
  if(is_empty(cluster_table)) return(x)
  
  # Keep only name and label_order columns,
  cluster_table <- cluster_table[, mget(c("name", "label_order"))]
  
  # Merge ordering into feature_similarity_table. The table is first
  # merged on feature_name_1 and then on feature_name_2.
  mutual_correlation_table <- data.table::copy(x@data)
  mutual_correlation_table <- merge(x=mutual_correlation_table,
                                    y=cluster_table,
                                    by.x="feature_name_1",
                                    by.y="name",
                                    all.x=TRUE,
                                    all.y=FALSE)
  mutual_correlation_table <- merge(x=mutual_correlation_table,
                                    y=cluster_table,
                                    by.x="feature_name_2",
                                    by.y="name",
                                    all.x=TRUE,
                                    all.y=FALSE)
  
  # Rename columns
  data.table::setnames(mutual_correlation_table,
                       old=c("label_order.x", "label_order.y"),
                       new=c("label_order_1", "label_order_2"))
  
  # Add to data element.
  x@data <- mutual_correlation_table
  
  # Add grouping column.
  x@grouping_column <- c(x@grouping_column, "label_order_1", "label_order_2")
  
  return(x)
}



.compute_feature_similarity_cluster_table <- function(x){
  # Computes the feature similarity cluster table from the similarity table in
  # x.
  
  # Create a cluster method object using data stored in x.
  cluster_method_object <- .create_feature_similarity_cluster_method_object(x=x)
  
  if(is.null(x)) return(NULL)
  
  # Compute the cluster table.
  cluster_table <- create_clusters(object=cluster_method_object,
                                   as_cluster_object=FALSE)
  
  return(cluster_table)
}



.create_feature_similarity_cluster_method_object <- function(x){
  
  if(is_empty(x)) return(NULL)
  
  if(length(x@similarity_threshold) > 1){
    # Remove 1.0 because that does not yield clustering info.
    available_thresholds <- setdiff(x@similarity_threshold, 1.0)
    
    # Select the maximum threshold.
    x@similarity_threshold <- max(available_thresholds)
  }
  
  # Create cluster method object.
  cluster_method_object <- create_cluster_method_object(cluster_method=x@cluster_method,
                                                        data_type = "feature",
                                                        cluster_linkage=x@linkage_method,
                                                        cluster_cut_method=x@cluster_cut_method,
                                                        cluster_similarity_threshold=x@similarity_threshold,
                                                        cluster_similarity_metric=x@similarity_metric,
                                                        cluster_representation_method="none")
  
  # Attach the similarity table to the cluster_method_object.
  cluster_method_object@similarity_table <- methods::new("similarityTable",
                                                         data=x@data[, mget(c("feature_name_1", "feature_name_2", "value"))],
                                                         similarity_metric=x@similarity_metric,
                                                         data_type=cluster_method_object@data_type)
  return(cluster_method_object)
}



#####export_feature_similarity#####

#'@title Extract and export mutual correlation between features.
#'
#'@description Extract and export mutual correlation between features in a
#'  familiarCollection.
#'
#'@param export_dendrogram Add dendrogram in the data element objects.
#'@param export_ordered_data Add feature label ordering to data in the data
#'  element objects.
#'@param export_clustering Add clustering information to data.
#'
#'@inheritParams export_all
#'@inheritParams extract_data
#'@inheritParams plot_univariate_importance
#'
#'@inheritDotParams as_familiar_collection
#'
#'@details Data is usually collected from a `familiarCollection` object.
#'  However, you can also provide one or more `familiarData` objects, that will
#'  be internally converted to a `familiarCollection` object. It is also
#'  possible to provide a `familiarEnsemble` or one or more `familiarModel`
#'  objects together with the data from which data is computed prior to export.
#'  Paths to the previous files can also be provided.
#'
#'  All parameters aside from `object` and `dir_path` are only used if `object`
#'  is not a `familiarCollection` object, or a path to one.
#'
#'@return A list containing a data.table (if `dir_path` is not provided), or
#'  nothing, as all data is exported to `csv` files.
#'@exportMethod export_feature_similarity
#'@md
#'@rdname export_feature_similarity-methods
setGeneric("export_feature_similarity",
           function(object,
                    dir_path=NULL,
                    aggregate_results=TRUE,
                    feature_cluster_method=waiver(),
                    feature_linkage_method=waiver(),
                    feature_cluster_cut_method=waiver(),
                    feature_similarity_threshold=waiver(),
                    export_dendrogram=FALSE,
                    export_ordered_data=FALSE,
                    export_clustering=FALSE,
                    export_collection=FALSE,
                    ...) standardGeneric("export_feature_similarity"))

#####export_feature_similarity (collection)#####

#'@rdname export_feature_similarity-methods
setMethod("export_feature_similarity", signature(object="familiarCollection"),
          function(object,
                   dir_path=NULL,
                   aggregate_results=TRUE,
                   feature_cluster_method=waiver(),
                   feature_linkage_method=waiver(),
                   feature_cluster_cut_method=waiver(),
                   feature_similarity_threshold=waiver(),
                   export_dendrogram=FALSE,
                   export_ordered_data=FALSE,
                   export_clustering=FALSE,
                   export_collection=FALSE,
                   ...){
            
            # Make sure the collection object is updated.
            object <- update_object(object=object)
            
            # Extract data.
            x <- object@feature_similarity
            
            if(export_ordered_data & export_clustering) stop("Cannot simultaneously export cluster information and ordering of features.")
            
            # Check that the data are not empty.
            if(is_empty(x)) return(NULL)
            
            if(!is.waive(feature_cluster_method)){
              
              # Set clustering method.
              x <- lapply(x, function(x, feature_cluster_method){
                x@cluster_method <- feature_cluster_method
                return(x)
              },
              feature_cluster_method=feature_cluster_method)
            }
            
            if(!is.waive(feature_linkage_method)){
             
              # Set linkage method.
              x <- lapply(x, function(x, feature_linkage_method){
                x@linkage_method <- feature_linkage_method
                return(x)
              },
              feature_linkage_method=feature_linkage_method)
            }
            
            if(!is.waive(feature_cluster_cut_method)){
              
              # Set cut method
              x <- lapply(x, function(x, feature_cluster_cut_method){
                x@cluster_cut_method <- feature_cluster_cut_method
                return(x)
              },
              feature_cluster_cut_method=feature_cluster_cut_method)
            }
            
            if(!is.waive(feature_similarity_threshold)){
             
              # Set similarity threshold.
              x <- lapply(x, function(x, feature_similarity_threshold){
                x@similarity_threshold <- feature_similarity_threshold
                return(x)
              },
              feature_similarity_threshold=feature_similarity_threshold)
            }
            
            # Check whether the input parameters are valid and create a cluster
            # object.
            .check_cluster_parameters(cluster_method=x[[1]]@cluster_method,
                                      data_type = "feature",
                                      cluster_linkage=x[[1]]@linkage_method,
                                      cluster_cut_method=x[[1]]@cluster_cut_method,
                                      cluster_similarity_threshold=x[[1]]@similarity_threshold,
                                      cluster_similarity_metric=x[[1]]@similarity_metric,
                                      cluster_representation_method="none")
            
            if(aggregate_results | export_dendrogram | export_ordered_data | export_clustering){
              x <- .compute_data_element_estimates(x)
              
              if(export_dendrogram | export_ordered_data | export_clustering){
                # Add dendrogram and other cluster objects.
                x <- lapply(x,
                            .append_feature_similarity_dendrogram)
              }
              
              if(export_clustering){
                x <- lapply(x,
                            .append_feature_similarity_clusters)
              }
              
              if(export_ordered_data){
                # Add clustering information.
                x <- lapply(x,
                            .append_feature_similarity_clustering)
              }
            }
            
            return(.export(x=object,
                           data_elements=x,
                           dir_path=dir_path,
                           aggregate_results=aggregate_results,
                           type="feature_similarity",
                           subtype=x[[1]]@similarity_metric,
                           export_dendrogram=export_dendrogram,
                           export_ordered_data=export_ordered_data,
                           export_collection=export_collection))
          })


#####export_feature_similarity (generic)#####

#'@rdname export_feature_similarity-methods
setMethod("export_feature_similarity", signature(object="ANY"),
          function(object,
                   dir_path=NULL,
                   aggregate_results=TRUE,
                   feature_cluster_method=waiver(),
                   feature_linkage_method=waiver(),
                   feature_cluster_cut_method=waiver(),
                   feature_similarity_threshold=waiver(),
                   export_collection=FALSE,
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object,
                                          "data_element"="feature_similarity",
                                          "aggregate_results"=aggregate_results,
                                          "feature_cluster_method"=feature_cluster_method,
                                          "feature_linkage_method"=feature_linkage_method,
                                          "feature_cluster_cut_method"=feature_cluster_cut_method,
                                          "feature_similarity_threshold"=feature_similarity_threshold),
                                     list(...)))
            
            return(do.call(export_feature_similarity,
                           args=c(list("object"=object,
                                       "dir_path"=dir_path,
                                       "aggregate_results"=aggregate_results,
                                       "feature_cluster_method"=feature_cluster_method,
                                       "feature_linkage_method"=feature_linkage_method,
                                       "feature_cluster_cut_method"=feature_cluster_cut_method,
                                       "feature_similarity_threshold"=feature_similarity_threshold,
                                       "export_collection"=export_collection),
                                  list(...))))
          })



#####.export (familiarDataElementFeatureSimilarity)-----------------------------
setMethod(".export", signature(x="familiarDataElementFeatureSimilarity"),
          function(x, x_list, aggregate_results=FALSE, export_dendrogram, export_ordered_data, ...){
            # This is like .export,familiarDataElement, but the elements are
            # merged prior to computing estimates.
            
            # Only merge if dendrograms are missing for all entries.
            if(!export_dendrogram & !export_ordered_data){
              # Merge data elements.
              x <- merge_data_elements(x=x_list,
                                       as_data="all",
                                       as_grouping_column=TRUE,
                                       force_data_table=TRUE)
              
            } else {
              x <- x_list
            }
            
            if(aggregate_results){
              x <- .compute_data_element_estimates(x)
            }
            
            return(x)
          })


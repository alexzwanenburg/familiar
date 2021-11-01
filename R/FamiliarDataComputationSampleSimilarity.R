#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


setClass("familiarDataElementSampleSimilarity",
         contains="familiarDataElement",
         slots=list("similarity_metric"="character",
                    "cluster_method"="character",
                    "linkage_method"="character",
                    "cluster_cut_method"="character",
                    "similarity_threshold"="ANY",
                    "dendrogram"="ANY"),
         prototype = methods::prototype(detail_level="ensemble",
                                        estimation_type="point",
                                        similarity_metric=NA_character_,
                                        cluster_method=NA_character_,
                                        linkage_method=NA_character_,
                                        cluster_cut_method=NA_character_,
                                        similarity_threshold=NULL,
                                        dendrogram=NULL,
                                        value_column="value",
                                        grouping_column=c("sample_1", "sample_2")))



#'@title Internal function to extract the sample distance table.
#'
#'@description Computes and extracts the sample distance table for samples
#'  analysed using a `familiarEnsemble` object to form a `familiarData` object. This table can be used to cluster
#'  samples, and is exported directly by `extract_feature_expression`.
#'
#'@inheritParams extract_data
#'
#'@return A data.table containing pairwise distance between samples. This data
#'  is only the upper triangular of the complete matrix (i.e. the sparse
#'  unitriangular representation). Diagonals will always be 0.0 and the lower
#'  triangular is mirrored.
#'@md
#'@keywords internal
setGeneric("extract_sample_similarity",
           function(object,
                    data,
                    cl=NULL,
                    is_pre_processed=FALSE,
                    sample_limit=waiver(),
                    sample_cluster_method=waiver(),
                    sample_linkage_method=waiver(),
                    sample_similarity_metric=waiver(),
                    verbose=FALSE,
                    message_indent=0L,
                    ...) standardGeneric("extract_sample_similarity"))


#####extract_sample_similarity#####
setMethod("extract_sample_similarity", signature(object="familiarEnsemble", data="ANY"),
          function(object,
                   data,
                   cl=NULL,
                   is_pre_processed=FALSE,
                   sample_limit=waiver(),
                   sample_cluster_method=waiver(),
                   sample_linkage_method=waiver(),
                   sample_similarity_metric=waiver(),
                   verbose=FALSE,
                   message_indent=0L,
                   ...){
            
            # Message extraction start
            logger.message(paste0("Computing pairwise similarity between samples."),
                           indent=message_indent,
                           verbose=verbose)
            
            # Obtain sample cluster method from stored settings, if required.
            if(is.waive(sample_cluster_method)) sample_cluster_method <- object@settings$sample_cluster_method

            # Obtain sample linkage function from stored settings, if required.
            if(is.waive(sample_linkage_method)) sample_linkage_method <- object@settings$sample_linkage_method

            # Obtain sample similarity metric from stored settings, if required.
            if(is.waive(sample_similarity_metric)) sample_similarity_metric <- object@settings$sample_similarity_metric

            # Replace sample cluster method == "none" with "hclust"
            if(sample_cluster_method == "none") sample_cluster_method <- "hclust"

            .check_cluster_parameters(cluster_method=sample_cluster_method,
                                      cluster_linkage=sample_linkage_method,
                                      cluster_similarity_metric=sample_similarity_metric,
                                      var_type="sample")
            
            # Check the sample limit.
            sample_limit <- .parse_sample_limit(x = sample_limit,
                                                object = object,
                                                default = Inf,
                                                data_element = "sample_similarity")
            
            # Generate a prototype data element.
            proto_data_element <- new("familiarDataElementSampleSimilarity",
                                      similarity_metric = sample_similarity_metric,
                                      cluster_method = sample_cluster_method,
                                      linkage_method = sample_linkage_method)
            
            # Generate elements to send to dispatch.
            similarity_data <- extract_dispatcher(FUN=.extract_sample_similarity,
                                                  has_internal_bootstrap=FALSE,
                                                  cl=cl,
                                                  object=object,
                                                  data=data,
                                                  sample_limit=sample_limit,
                                                  proto_data_element=proto_data_element,
                                                  is_pre_processed=is_pre_processed,
                                                  aggregate_results=TRUE,
                                                  message_indent=message_indent + 1L,
                                                  verbose=verbose)
            
            return(similarity_data)
          })



.extract_sample_similarity <- function(object,
                                       data,
                                       proto_data_element,
                                       cl=NULL,
                                       sample_limit,
                                       is_pre_processed,
                                       message_indent,
                                       aggregate_results=TRUE,
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
  
  # Check if the number of samples is sufficient to form pairs (>= 2),
  # and return an empty table if not.
  if(data.table::uniqueN(data@data, by=get_id_columns(id_depth="sample")) < 2) return(data_element)
  
  # Select samples up to sample_limit.
  data <- get_subsample(data=data,
                        size=sample_limit,
                        seed=0L)
  
  # Maintain only important features. The current set is based on the
  # required features.
  data <- filter_features(data=data,
                          available_features=object@model_features)
  
  # Aggregate features.
  data <- aggregate_data(data=data)
  
  # Compute the similarity table
  data_element@data <- cluster.get_samplewise_similarity_table(cl=cl,
                                                               data_obj=data,
                                                               similarity_metric=data_element@similarity_metric,
                                                               verbose=verbose,
                                                               message_indent=message_indent + 1L)
  
  # Merge data elements
  data_elements <- merge_data_elements(list(data_element))
  
  if(aggregate_results) data_elements <- .compute_data_element_estimates(x=data_elements)
  
  return(data_elements)
}



..compute_sample_similarity_dendrogram <- function(x){
  
  if(is_empty(x)) return(x)
  
  # Generate a distance matrix from the similarity table
  distance_matrix <- cluster.get_distance_matrix(similarity_table=x@data[, mget(c("sample_1", "sample_2", "value"))],
                                                 similarity_metric=x@similarity_metric)
  
  # Obtain cluster object.
  h <- cluster.get_cluster_object(distance_matrix=distance_matrix,
                                  cluster_method=x@cluster_method,
                                  cluster_linkage=x@linkage_method)
  
  # Attach to data element.
  x@dendrogram <- h
  
  return(x)
}



..limit_sample_similarity_samples <- function(x, sample_limit){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  sample_1 <- sample_2 <- NULL
  
  if(is_empty(x)) return(x)
  
  # Set for reproducibility.
  set.seed(0L)
  on.exit(set.seed(NULL))
  
  # Find the names of samples.
  sample_names <- unique(c(x@data$sample_1,
                           x@data$sample_2))
  
  # Check the number of samples against the limit.
  if(length(sample_names) <= sample_limit) return(x)
  
  # Select samples.
  sample_names <- fam_sample(sample_names,
                             size=sample_limit,
                             replace=FALSE)
  
  # Select only the selected samples.
  x@data <- x@data[sample_1 %in% sample_names & sample_2 %in% sample_names]
  
  return(x)
}



#####export_sample_similarity#####

#'@title Extract and export mutual correlation between features.
#'
#'@description Extract and export mutual correlation between features in a
#'  familiarCollection.
#'
#'@param export_dendrogram Add dendrogram in the data element objects.
#'
#'@inheritParams export_all
#'@inheritParams extract_data
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
#'@exportMethod export_sample_similarity
#'@md
#'@rdname export_sample_similarity-methods
setGeneric("export_sample_similarity",
           function(object,
                    dir_path=NULL,
                    aggregate_results=TRUE,
                    sample_limit=waiver(),
                    sample_cluster_method=waiver(),
                    sample_linkage_method=waiver(),
                    export_dendrogram=FALSE,
                    ...) standardGeneric("export_sample_similarity"))

#####export_sample_similarity (collection)#####

#'@rdname export_sample_similarity-methods
setMethod("export_sample_similarity", signature(object="familiarCollection"),
          function(object,
                   dir_path=NULL,
                   aggregate_results=TRUE,
                   sample_limit=waiver(),
                   sample_cluster_method=waiver(),
                   sample_linkage_method=waiver(),
                   export_dendrogram=FALSE,
                   ...){
            
            # Extract data.
            x <- object@sample_similarity
            
            # Check that the data are not empty.
            if(is_empty(x)) return(NULL)
            
            # Check sample cluster method.
            if(!is.waive(sample_cluster_method)){
              
              # Set clustering method.
              x <- lapply(x, function(x, sample_cluster_method){
                x@cluster_method <- sample_cluster_method
                return(x)
              },
              sample_cluster_method=sample_cluster_method)
            }
            
            # Check sample linkage method.
            if(!is.waive(sample_linkage_method)){
              
              # Set linkage method.
              x <- lapply(x, function(x, sample_linkage_method){
                x@linkage_method <- sample_linkage_method
                return(x)
              },
              sample_linkage_method=sample_linkage_method)
            }
            
            # Check the sample limit.
            if(!is.waive(sample_limit)){
              .check_number_in_valid_range(sample_limit, var_name="sample_limit",
                                           range=c(20L, Inf))
            } else {
              sample_limit <- Inf
            }
            
            # Check whether the input parameters are valid.
            .check_cluster_parameters(cluster_method=x[[1]]@cluster_method,
                                      cluster_linkage=x[[1]]@linkage_method,
                                      cluster_similarity_metric=x[[1]]@similarity_metric,
                                      var_type="sample")
            
            if(aggregate_results | export_dendrogram){
              x <- .compute_data_element_estimates(x)
              
              # Limit the number of samples.
              if(is.finite(sample_limit)) x <- lapply(x, ..limit_sample_similarity_samples, sample_limit=sample_limit)
              
              # Add clustering information.
              if(export_dendrogram){
                x <- lapply(x, ..compute_sample_similarity_dendrogram)
              }
              
            }
            
            return(.export(x=object,
                           data_elements=x,
                           dir_path=dir_path,
                           aggregate_results=aggregate_results,
                           type="sample_similarity",
                           subtype=x[[1]]@similarity_metric,
                           export_dendrogram=export_dendrogram))
          })


#####export_sample_similarity (generic)#####

#'@rdname export_sample_similarity-methods
setMethod("export_sample_similarity", signature(object="ANY"),
          function(object,
                   dir_path=NULL,
                   aggregate_results=TRUE,
                   sample_limit=waiver(),
                   sample_cluster_method=waiver(),
                   sample_linkage_method=waiver(),
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object,
                                          "data_element"="feature_similarity",
                                          "sample_limit"=sample_limit,
                                          "aggregate_results"=aggregate_results,
                                          "sample_cluster_method"=sample_cluster_method,
                                          "sample_linkage_method"=sample_linkage_method),
                                     list(...)))
            
            return(do.call(export_sample_similarity,
                           args=c(list("object"=object,
                                       "dir_path"=dir_path,
                                       "aggregate_results"=aggregate_results,
                                       "sample_cluster_method"=sample_cluster_method,
                                       "sample_linkage_method"=sample_linkage_method),
                                  list(...))))
          })



#####.export (familiarDataElementFeatureSimilarity)-----------------------------
setMethod(".export", signature(x="familiarDataElementSampleSimilarity"),
          function(x, x_list, aggregate_results=FALSE, export_dendrogram, ...){
            # This is like .export,familiarDataElement, but the elements are
            # merged prior to computing estimates.
            
            # Only merge if dendrograms are missing for all entries.
            if(!export_dendrogram){
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

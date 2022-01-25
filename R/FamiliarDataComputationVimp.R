#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarDataElementVimpData",
         contains="familiarDataElement",
         slots=list("rank_threshold"="numeric",
                    "rank_aggregation_method"="character"),
         prototype = methods::prototype(detail_level="hybrid",
                                        estimation_type="point",
                                        rank_threshold=NA_real_,
                                        rank_aggregation_method=NA_character_))





#'@title Internal function to extract variable importance from models.
#'
#'@description Aggregate variable importance from models in a
#'  `familiarEnsemble`.
#'
#'@param feature_similarity_table Table containing pairwise distance between
#'  features. This is used to determine cluster information, and indicate
#'  co-clustered important features. The table is created by the
#'  `extract_feature_similarity` method.
#'@inheritParams extract_data
#'
#'@return A list containing variable importance information.
#'@md
#'@keywords internal
setGeneric("extract_model_vimp", function(object,
                                          data,
                                          aggregation_method=waiver(),
                                          rank_threshold=waiver(),
                                          message_indent=0L,
                                          verbose=FALSE,
                                          ...) standardGeneric("extract_model_vimp"))

#####extract_model_vimp (familiarEnsemble)#####
setMethod("extract_model_vimp", signature(object="familiarEnsemble"),
          function(object,
                   data,
                   aggregation_method=waiver(),
                   rank_threshold=waiver(),
                   message_indent=0L,
                   verbose=FALSE,
                   ...){
            
            # Message extraction start
            logger.message(paste0("Extracting variable importance obtained from the models."),
                           indent=message_indent,
                           verbose=verbose)
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)) ..error_ensemble_models_not_loaded()
            
            # Obtain aggregation method from stored settings, if required.
            if(is.waive(aggregation_method)){
              if(length(object@model_list) > 1){
                aggregation_method <- object@settings$aggregation
                
              } else {
                # If only one model is evaluated, do not aggregate data.
                aggregation_method <- "none" 
              }
            } 
            
            # Check aggregation method
            rank.check_aggregation_method(method=aggregation_method)
            
            # Obtain rank thresholds from stored settings, if required
            if(is.waive(rank_threshold)) rank_threshold <- object@settings$aggr_rank_threshold
            
            # Check rank threshold.
            if(!is.waive(rank_threshold)){
              .check_number_in_valid_range(x=rank_threshold, var_name="rank_threshold", range=c(1, Inf))
            }
            
            # Generate a prototype data element
            proto_data_element <- methods::new("familiarDataElementVimpData",
                                               rank_threshold=rank_threshold,
                                               rank_aggregation_method=aggregation_method)
            
            # Generate elements to send to dispatch.
            vimp_info <- extract_dispatcher(FUN=.extract_model_vimp,
                                            cl=NULL,
                                            has_internal_bootstrap=FALSE,
                                            object=object,
                                            data=data,
                                            proto_data_element=proto_data_element,
                                            aggregation_method=aggregation_method,
                                            aggregate_results=FALSE,
                                            message_indent=message_indent + 1L,
                                            verbose=verbose)
            
            return(vimp_info)
          })

.extract_model_vimp <- function(object,
                                data,
                                proto_data_element,
                                ...){
  
  # Ensure that the object is loaded
  object <- load_familiar_object(object)
  
  # Add model name.
  data_element <- add_model_name(proto_data_element, object=object)
  
  # Check that the model has been trained.
  if(!model_is_trained(object)) return(NULL)
  
  # Extract variable importance from the models
  vimp_table <- ..vimp(object=object, data=data)
  
  # Check that the variable importance table is not empty.
  if(is_empty(vimp_table)) return(NULL)
  
  # Decluster the features.
  vimp_table <- rank.decluster_vimp_table(vimp_table=vimp_table,
                                          feature_info_list=object@feature_info)
  
  vimp_table[, ":="("data_id"=tail(object@run_table, n=1)$data_id,
                    "run_id"=tail(object@run_table, n=1)$run_id)]
  
  # Add data.
  data_element@data <- vimp_table
  
  # Add value and grouping columns.
  data_element@value_column <- c("rank", "score")

  return(data_element)
}



#'@title Internal function to extract feature selection variable importance.
#'
#'@description Aggregate variable importance obtained during feature selection.
#'  This information can only be obtained as part of the main `summon_familiar` process.
#'
#'@inheritParams extract_data
#'
#'@return A list containing feature selection variable importance information.
#'@md
#'@keywords internal
setGeneric("extract_fs_vimp", function(object,
                                       aggregation_method=waiver(),
                                       rank_threshold=waiver(),
                                       message_indent=0L,
                                       verbose=FALSE,
                                       ...) standardGeneric("extract_fs_vimp"))


#####extract_fs_vimp (familiarEnsemble)#####
setMethod("extract_fs_vimp", signature(object="familiarEnsemble"),
          function(object,
                   aggregation_method=waiver(),
                   rank_threshold=waiver(),
                   message_indent=0L,
                   verbose=FALSE,
                   ...){
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)) ..error_ensemble_models_not_loaded()
            
            # Obtain aggregation method from stored settings, if required.
            if(is.waive(aggregation_method)) aggregation_method <- object@settings$aggregation
            
            # Check aggregation method
            rank.check_aggregation_method(method=aggregation_method)
            
            # Obtain rank thresholds from stored settings, if required
            if(is.waive(rank_threshold)) rank_threshold <- object@settings$aggr_rank_threshold
            
            # Check rank threshold.
            if(!is.waive(rank_threshold)){
              .check_number_in_valid_range(x=rank_threshold, var_name="rank_threshold", range=c(1, Inf))
            }
            
            # Load project list and file_paths
            file_paths <- tryCatch(get_file_paths(), error=identity)
            project_list <- tryCatch(get_project_list(), error=identity)
            
            if(inherits(file_paths, "error") | inherits(project_list, "error")) return(NULL)
            
            # Message extraction start
            logger.message(paste0("Extracting variable importance obtained during feature selection."),
                           indent=message_indent,
                           verbose=verbose)
            
            # Define the run table -> at the pooling level
            run <- .get_run_list(iteration_list=project_list$iter_list,
                                 data_id=object@run_table$ensemble_data_id,
                                 run_id=object@run_table$ensemble_run_id)
            
            # Obtain variable importance table
            vimp_table <- rank.get_vimp_table(run=run,
                                              fs_method=object@fs_method,
                                              proj_list=project_list,
                                              file_paths=file_paths,
                                              decluster=TRUE)
            
            # Generate a prototype data element
            data_element <- methods::new("familiarDataElementVimpData",
                                         data=vimp_table,
                                         rank_threshold=rank_threshold,
                                         rank_aggregation_method=aggregation_method)
            
            return(list(data_element))
          })


##### ..compute_data_elements_estimates (familiarDataElementHyperparameters)------
setMethod("..compute_data_element_estimates", signature(x="familiarDataElementVimpData"),
          function(x, x_list=NULL, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            rank <- NULL
            
            # It might be that x was only used to direct to this method.
            if(!is.null(x_list)) x <- x_list
            if(!is.list(x)) x <- list(x)
            
            # Remove empty entries.
            x <- x[!sapply(x, is_empty)]
            if(is_empty(x)) return(NULL)
            
            # Collect all data.
            data <- data.table::rbindlist(lapply(x, function(x) (x@data)),
                                          use.names=TRUE,
                                          fill=TRUE)
            
            # Find translation table.
            translation_table <- rank.consensus_clustering(vimp_table=data)
            
            # Determine aggregate ranks.
            data <- rank.aggregate_feature_ranks(vimp_table=data,
                                                 aggregation_method=x[[1]]@rank_aggregation_method,
                                                 rank_threshold=x[[1]]@rank_threshold)
            
            # Merge the translation table into the data.
            data <- merge(x=data,
                          y=translation_table,
                          by.x="name",
                          by.y="feature_name")
            
            # Update column names.
            data.table::setnames(data,
                                 old=c("name", "aggr_rank", "aggr_score"),
                                 new=c("feature", "rank", "score"))
            
            # Remove cluster_name
            data[, "cluster_name":=NULL]
            
            # Order by rank.
            data <- data[order(rank)]
            
            # Add cluster size.
            data[, "cluster_size":=.N, by="cluster_id"]
            
            # Copy data element.
            y <- x[[1]]
            y@data <- data
            
            return(y)
          })



#####export_model_vimp#####

#'@title Extract and export model-based variable importance.
#'
#'@description Extract and export model-based variable importance from a
#'  familiarCollection.
#'
#'@param aggregation_method (*optional*) The method used to aggregate variable
#'  importances over different data subsets, e.g. bootstraps. The following
#'  methods can be selected:
#'
#'  * `mean` (default): Use the mean rank of a feature over the subsets to
#'  determine the aggregated feature rank.
#'
#'  * `median`: Use the median rank of a feature over the subsets to determine
#'  the aggregated feature rank.
#'
#'  * `best`: Use the best rank the feature obtained in any subset to determine
#'  the aggregated feature rank.
#'
#'  * `worst`: Use the worst rank the feature obtained in any subset to
#'  determine the aggregated feature rank.
#'
#'  * `stability`: Use the frequency of the feature being in the subset of
#'  highly ranked features as measure for the aggregated feature rank
#'  (Meinshausen and Buehlmann, 2010).
#'
#'  * `exponential`: Use a rank-weighted frequence of occurrence in the subset
#'  of highly ranked features as measure for the aggregated feature rank (Haury
#'  et al., 2011).
#'
#'  * `borda`: Use the borda count as measure for the aggregated feature rank
#'  (Wald et al., 2012).
#'
#'  * `enhanced_borda`: Use an occurrence frequency-weighted borda count as
#'  measure for the aggregated feature rank (Wald et al., 2012).
#'
#'  * `truncated_borda`: Use borda count computed only on features within the
#'  subset of highly ranked features.
#'
#'  * `enhanced_truncated_borda`: Apply both the enhanced borda method and the
#'  truncated borda method and use the resulting borda count as the aggregated
#'  feature rank.
#'
#'@param rank_threshold (*optional*) The threshold used to define the subset of
#'  highly important features. If not set, this threshold is determined by
#'  maximising the variance in the occurrence value over all features over the
#'  subset size.
#'
#'  This parameter is only relevant for `stability`, `exponential`,
#'  `enhanced_borda`, `truncated_borda` and `enhanced_truncated_borda` methods.
#'
#'@inheritParams export_all
#'@inheritParams export_univariate_analysis_data
#'
#'@inheritDotParams as_familiar_collection
#'
#'@details Data, such as model performance and calibration information, is
#'  usually collected from a `familiarCollection` object. However, you can also
#'  provide one or more `familiarData` objects, that will be internally
#'  converted to a `familiarCollection` object. It is also possible to provide a
#'  `familiarEnsemble` or one or more `familiarModel` objects together with the
#'  data from which data is computed prior to export. Paths to the previous
#'  files can also be provided.
#'
#'  All parameters aside from `object` and `dir_path` are only used if `object`
#'  is not a `familiarCollection` object, or a path to one.
#'
#'  Variable importance is based on the ranking produced by model-specific
#'  variable importance routines, e.g. permutation for random forests. If such a
#'  routine is absent, variable importance is based on the feature selection
#'  method that led to the features included in the model. In case multiple
#'  models (`familiarModel` objects) are combined, feature ranks are first
#'  aggregated using the method defined by the `aggregation_method`, some of
#'  which require a `rank_threshold` to indicate a subset of most important
#'  features.
#'
#'  Information concerning highly similar features that form clusters is
#'  provided as well. This information is based on consensus clustering of the
#'  features that were used in the signatures of the underlying models. This
#'  clustering information is also used during aggregation to ensure that
#'  co-clustered features are only taken into account once.
#'
#'@return A data.table (if `dir_path` is not provided), or nothing, as all data
#'  is exported to `csv` files.
#'@exportMethod export_model_vimp
#'@md
#'@rdname export_model_vimp-methods
setGeneric("export_model_vimp",
           function(object,
                    dir_path=NULL,
                    aggregate_results=TRUE,
                    aggregation_method=waiver(),
                    rank_threshold=waiver(),
                    export_collection=FALSE,
                    ...) standardGeneric("export_model_vimp"))

#####export_model_vimp (collection)#####

#'@rdname export_model_vimp-methods
setMethod("export_model_vimp", signature(object="familiarCollection"),
          function(object,
                   dir_path=NULL,
                   aggregate_results=TRUE,
                   aggregation_method=waiver(),
                   rank_threshold=waiver(),
                   export_collection=FALSE,
                   ...){
            
            # Extract data.
            x <- object@model_vimp
            
            # Check that the data are not empty.
            if(is_empty(x)) return(NULL)
            
            if(!is.waive(aggregation_method)){
              # Check if the aggregation method is valid.
              rank.check_aggregation_method(method=aggregation_method)
              
              # Set aggregation method.
              x <- lapply(x, function(x, aggregation_method){
                x@rank_aggregation_method <- aggregation_method
                return(x)
              },
              aggregation_method=aggregation_method)
            }
            
            if(!is.waive(rank_threshold)){
              # Check if the threshold is valid.
              .check_number_in_valid_range(x=rank_threshold, var_name="rank_threshold", range=c(1, Inf))
              
              # Set threshold.
              x <- lapply(x, function(x, rank_threshold){
                x@rank_threshold <- rank_threshold
                return(x)
              },
              rank_threshold=rank_threshold)
            }
            
            subtype <- paste("learner", x[[1]]@rank_aggregation_method, sep="_")
            
            return(.export(x=object,
                           data_elements=x,
                           dir_path=dir_path,
                           aggregate_results=aggregate_results,
                           type="variable_importance",
                           subtype=subtype,
                           export_collection=export_collection))
          })

#####export_model_vimp (generic)#####

#'@rdname export_model_vimp-methods
setMethod("export_model_vimp", signature(object="ANY"),
          function(object,
                   dir_path=NULL,
                   aggregate_results=TRUE,
                   aggregation_method=waiver(),
                   rank_threshold=waiver(),
                   export_collection=FALSE,
                   ...){
            
            # Attempt conversion to familiarCollection object. Note that we pass
            # on aggregation_method and rank_threshold values.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object,
                                          "data_element"="model_vimp",
                                          "aggregation_method"=aggregation_method,
                                          "rank_threshold"=rank_threshold),
                                     list(...)))
            
            return(do.call(export_model_vimp,
                           args=c(list("object"=object,
                                       "dir_path"=dir_path,
                                       "aggregate_results"=aggregate_results,
                                       "aggregation_method"=aggregation_method,
                                       "rank_threshold"=rank_threshold,
                                       "export_collection"=export_collection),
                                  list(...))))
          })



#####export_fs_vimp#####

#'@title Extract and export feature selection variable importance.
#'
#'@description Extract and export feature selection variable importance from a
#'  familiarCollection.
#'
#'@param aggregation_method (*optional*) The method used to aggregate variable
#'  importances over different data subsets, e.g. bootstraps. The following
#'  methods can be selected:
#'
#'  * `mean` (default): Use the mean rank of a feature over the subsets to
#'  determine the aggregated feature rank.
#'
#'  * `median`: Use the median rank of a feature over the subsets to determine
#'  the aggregated feature rank.
#'
#'  * `best`: Use the best rank the feature obtained in any subset to determine
#'  the aggregated feature rank.
#'
#'  * `worst`: Use the worst rank the feature obtained in any subset to
#'  determine the aggregated feature rank.
#'
#'  * `stability`: Use the frequency of the feature being in the subset of
#'  highly ranked features as measure for the aggregated feature rank
#'  (Meinshausen and Buehlmann, 2010).
#'
#'  * `exponential`: Use a rank-weighted frequence of occurrence in the subset
#'  of highly ranked features as measure for the aggregated feature rank (Haury
#'  et al., 2011).
#'
#'  * `borda`: Use the borda count as measure for the aggregated feature rank
#'  (Wald et al., 2012).
#'
#'  * `enhanced_borda`: Use an occurrence frequency-weighted borda count as
#'  measure for the aggregated feature rank (Wald et al., 2012).
#'
#'  * `truncated_borda`: Use borda count computed only on features within the
#'  subset of highly ranked features.
#'
#'  * `enhanced_truncated_borda`: Apply both the enhanced borda method and the
#'  truncated borda method and use the resulting borda count as the aggregated
#'  feature rank.
#'
#'@param rank_threshold (*optional*) The threshold used to define the subset of
#'  highly important features. If not set, this threshold is determined by
#'  maximising the variance in the occurrence value over all features over the
#'  subset size.
#'
#'  This parameter is only relevant for `stability`, `exponential`,
#'  `enhanced_borda`, `truncated_borda` and `enhanced_truncated_borda` methods.
#'
#'@inheritParams export_all
#'@inheritParams export_univariate_analysis_data
#'
#'@inheritDotParams as_familiar_collection
#'
#'@details Data, such as model performance and calibration information, is
#'  usually collected from a `familiarCollection` object. However, you can also
#'  provide one or more `familiarData` objects, that will be internally
#'  converted to a `familiarCollection` object. Paths to the previous files can
#'  also be provided.
#'
#'  Unlike other export function, export using `familiarEnsemble` or
#'  `familiarModel` objects is not possible. This is because feature selection
#'  variable importance is not stored within `familiarModel` objects.
#'
#'  All parameters aside from `object` and `dir_path` are only used if `object`
#'  is not a `familiarCollection` object, or a path to one.
#'
#'  Variable importance is based on the ranking produced by feature selection
#'  routines. In case feature selection was performed repeatedly, e.g. using
#'  bootstraps, feature ranks are first aggregated using the method defined by
#'  the `aggregation_method`, some of which require a `rank_threshold` to
#'  indicate a subset of most important features.
#'
#'  Information concerning highly similar features that form clusters is
#'  provided as well. This information is based on consensus clustering of the
#'  features. This clustering information is also used during aggregation to
#'  ensure that co-clustered features are only taken into account once.
#'
#'@return A data.table (if `dir_path` is not provided), or nothing, as all data
#'  is exported to `csv` files.
#'@exportMethod export_fs_vimp
#'@md
#'@rdname export_fs_vimp-methods
setGeneric("export_fs_vimp",
           function(object,
                    dir_path=NULL,
                    aggregate_results=TRUE,
                    aggregation_method=waiver(),
                    rank_threshold=waiver(),
                    export_collection=FALSE,
                    ...) standardGeneric("export_fs_vimp"))

#####export_fs_vimp (collection)#####

#'@rdname export_fs_vimp-methods
setMethod("export_fs_vimp", signature(object="familiarCollection"),
          function(object,
                   dir_path=NULL,
                   aggregate_results=TRUE,
                   aggregation_method=waiver(),
                   rank_threshold=waiver(),
                   export_collection=FALSE,
                   ...){
            
            # Extract data.
            x <- object@fs_vimp
            
            # Check that the data are not empty.
            if(is_empty(x)) return(NULL)
            
            if(!is.waive(aggregation_method)){
              # Check if the aggregation method is valid.
              rank.check_aggregation_method(method=aggregation_method)
              
              # Set aggregation method.
              x <- lapply(x, function(x, aggregation_method){
                x@rank_aggregation_method <- aggregation_method
                return(x)
              },
              aggregation_method=aggregation_method)
            }
            
            if(!is.waive(rank_threshold)){
              # Check if the threshold is valid.
              .check_number_in_valid_range(x=rank_threshold, var_name="rank_threshold", range=c(1, Inf))
              
              # Set threshold.
              x <- lapply(x, function(x, rank_threshold){
                x@rank_threshold <- rank_threshold
                return(x)
              },
              rank_threshold=rank_threshold)
            }
            
            # Get subtype
            subtype <- paste("feature_selection", x[[1]]@rank_aggregation_method, sep="_")
            
            return(.export(x=object,
                           data_elements=x,
                           dir_path=dir_path,
                           aggregate_results=aggregate_results,
                           type="variable_importance",
                           subtype=subtype,
                           export_collection=export_collection))
          })

#####export_fs_vimp (generic)#####

#'@rdname export_fs_vimp-methods
setMethod("export_fs_vimp", signature(object="ANY"),
          function(object,
                   dir_path=NULL,
                   aggregate_results=TRUE,
                   aggregation_method=waiver(),
                   rank_threshold=waiver(),
                   export_collection=FALSE,
                   ...){
            
            # Attempt conversion to familiarCollection object. Note that we pass
            # on aggregation_method and rank_threshold values.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object,
                                          "data_element"="fs_vimp",
                                          "aggregation_method"=aggregation_method,
                                          "rank_threshold"=rank_threshold),
                                     list(...)))
            
            return(do.call(export_fs_vimp,
                           args=c(list("object"=object,
                                       "dir_path"=dir_path,
                                       "aggregate_results"=aggregate_results,
                                       "aggregation_method"=aggregation_method,
                                       "rank_threshold"=rank_threshold,
                                       "export_collection"=export_collection),
                                  list(...))))
          })

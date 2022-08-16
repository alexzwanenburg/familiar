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
            .check_parameter_value_is_valid(x=aggregation_method,
                                            var_name="aggregation_method",
                                            values=.get_available_rank_aggregation_methods())
            
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
  
  # Check that the model has been trained.
  if(!model_is_trained(object)) return(NULL)
  
  # Extract variable importance from the models
  vimp_table <- get_vimp_table(x=object, data=data, as_object=TRUE)
  
  # Check that the variable importance table is not empty.
  if(is_empty(vimp_table)) return(NULL)
  
  # Add data.
  data_element <- proto_data_element
  data_element@data <- vimp_table

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
            .check_parameter_value_is_valid(x=aggregation_method,
                                            var_name="aggregation_method",
                                            values=.get_available_rank_aggregation_methods())
            
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
            
            # Retrieve variable importance table objects.
            vimp_table_list <- .retrieve_feature_selection_data(fs_method=object@fs_method,
                                                                project_list=project_list,
                                                                file_paths=file_paths)[[object@fs_method]]
            
            # Define the run table -> at the pooling level
            run <- .get_run_list(iteration_list=project_list$iter_list,
                                 data_id=object@run_table$ensemble_data_id,
                                 run_id=object@run_table$ensemble_run_id)
            
            # Collect the correct vimp tables from the full list.
            vimp_table_list <- collect_vimp_table(x=vimp_table_list,
                                                  run_table=run$run_table)
            
            # Check if the variable importance table has been set.
            if(is_empty(vimp_table_list)) return(NULL)
            if(all(sapply(vimp_table_list, is_empty))) return(NULL)
            
            # Create list of data elements.
            data_element_list <- lapply(vimp_table_list, function(x, rank_threshold, aggregation_method){
              return(methods::new("familiarDataElementVimpData",
                                  data=x,
                                  rank_threshold=rank_threshold,
                                  rank_aggregation_method=aggregation_method))
            },
            rank_threshold=rank_threshold,
            aggregation_method=aggregation_method)
            
            # Merge data elements
            data_element <- merge_data_elements(data_element_list)
            
            return(data_element)
          })


#####merge_data_elements (familiarDataElementVimpData)--------------------------
setMethod("merge_data_elements", signature(x="familiarDataElementVimpData"),
          function(x, x_list, ...){
            
            # Identify items that can be joined.
            id_table <- identify_element_sets(x=x_list, ...)
            
            # Identify the element identifiers that should be grouped.
            grouped_data_element_ids <- lapply(split(id_table[, c("element_id", "group_id")], by="group_id"), function(id_table) (id_table$element_id))
            
            # List of data elements.
            data_elements <- list()
            
            for(current_group_data_element_ids in grouped_data_element_ids){
              # Copy the first data element in the group and use it as a
              # prototype.
              prototype_data_element <- x_list[[current_group_data_element_ids[1]]]
              
              if(any(sapply(x_list[current_group_data_element_ids], function(x) (is(x@data, "vimpTable"))))) {
                
                # Data attribute contains a variable importance table object.
                data_attribute <- lapply(x_list[current_group_data_element_ids], function(x) (x@data))
                
                # Set data attribute.
                prototype_data_element@data <- data_attribute
                
              } else if(length(current_group_data_element_ids) != 1){
                ..error_reached_unreachable_code("merge_data_elements,familiarDataElementVimpData: exactly one element is expected")
              }
              
              # Add merged data element to the list.
              data_elements <- c(data_elements, list(prototype_data_element))
            }
            
            return(data_elements)
          })


##### ..compute_data_elements_estimates (familiarDataElementHyperparameters)------
setMethod("..compute_data_element_estimates", signature(x="familiarDataElementVimpData"),
          function(x, x_list=NULL, ...){
            
            # It might be that x was only used to direct to this method.
            if(!is.null(x_list)) x <- x_list
            if(!is.list(x)) x <- list(x)
            
            # Remove empty entries.
            x <- x[!sapply(x, is_empty)]
            if(is_empty(x)) return(NULL)
            
            # Collect all the stored variable importance table objects.
            vimp_table_list <- unlist(lapply(x, function(x) x@data))
            
            # Aggregate list of vimp tables.
            vimp_object <- aggregate_vimp_table(x=vimp_table_list,
                                                aggregation_method=x[[1]]@rank_aggregation_method,
                                                rank_threshold=x[[1]]@rank_threshold)
            
            # Copy data element.
            y <- x[[1]]
            y@data <- vimp_object
            
            return(y)
          })


#####.export (familiarDataElementVimpData)--------------------------------------
setMethod(".export", signature(x="familiarDataElementVimpData"),
          function(x, x_list, aggregate_results=FALSE, ...){
            
            if(aggregate_results){
              x_list <- .compute_data_element_estimates(x_list)
            }
            
            # Extract variable importance tables, and add identifiers.
            x_list <- lapply(x_list,
                             ..export_vimp_data_elements)
            
            # Extract the data.table and merge to a single data.table.
            vimp_table <- lapply(x_list, function(x) x@data)
            vimp_table <- data.table::rbindlist(vimp_table, use.names=TRUE)
            
            # Form prototype.
            x <- x_list[[1]]
            x@data <- vimp_table
            
            return(x)
          })


..export_vimp_data_elements <- function(x){
  # Until export, all variable importance data elements are vimpTable objects.
  # We need to export these as data.tables/
  
  # Convert to data.table internally.
  if(is.list(x@data)){
    vimp_table_list <- lapply(x@data, get_vimp_table)
    
  } else {
    vimp_table_list <- list(get_vimp_table(x@data))
  }
  
  # Combine to a single table.
  vimp_table_list <- data.table::rbindlist(vimp_table_list,
                                           use.names=TRUE)
  
  # Check if the resulting combined table is empty.
  if(is_empty(vimp_table_list)) vimp_table_list <- NULL
  
  # Update the data attribute.
  x@data <- vimp_table_list
  
  # Update grouping and value columns.
  x@grouping_column <- "name"
  x@value_column <- c("score", "rank")
  
  # Add data attributes to the table
  x <- .identifier_as_data_attribute(x=x,
                                     identifier="all",
                                     as_grouping_column=TRUE)
  
  return(x)
}


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
              .check_parameter_value_is_valid(x=aggregation_method,
                                              var_name="aggregation_method",
                                              values=.get_available_rank_aggregation_methods())
              
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
              .check_parameter_value_is_valid(x=aggregation_method,
                                              var_name="aggregation_method",
                                              values=.get_available_rank_aggregation_methods())
              
              # Set aggregation method.
              x <- lapply(x, function(x, aggregation_method){
                x@rank_aggregation_method <- aggregation_method
                return(x)
              },
              aggregation_method=aggregation_method)
            }
            
            if(!is.waive(rank_threshold)){
              # Check if the threshold is valid.
              .check_number_in_valid_range(x=rank_threshold,
                                           var_name="rank_threshold",
                                           range=c(1, Inf))
              
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

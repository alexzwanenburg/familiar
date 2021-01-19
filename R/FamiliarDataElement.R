#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#####add_data_element_identifier (list)-----------------------------------------
setMethod("add_data_element_identifier", signature(x="list"),
          function(x, ...){
            # Add identifier to every data element.
            data_elements <- unlist(lapply(x, add_data_element_identifier, ...))
            
            return(data_elements)
          })

#####add_data_element_identifier (familiarDataElement)--------------------------
setMethod("add_data_element_identifier", signature(x="familiarDataElement"),
          function(x, ...){
            
            # Get dots, which contains the identifier to be set.
            dots <- list(...)
            
            if(length(dots) > 1) ..error_reached_unreachable_code("add_data_element_identifier: can only add one identifier at a time.")
            
            # Find the name of the identifier.
            identifier_name <- names(dots)
            
            # Iterate over values and create a separate data element for each value.
            data_elements <- lapply(dots[[identifier_name]], function(value, x, identifier_name){
              
              # Find the list of previous identifiers, or create a new one.
              identifier_list <- x@identifiers
              if(is.null(identifier_list)) identifier_list <- list()
              
              # Add value to identifier list.
              identifier_list[[identifier_name]] <- value
              
              # Combine old and new identifiers.
              x@identifiers <- identifier_list
              
              return(x)
            },
            x=x,
            identifier_name=identifier_name)
            
            return(data_elements)
          })

#####add_data_element_identifier (ANY)--------------------------
setMethod("add_data_element_identifier", signature(x="ANY"),
          function(x, ...){
            return(NULL)
          })


#####add_data_element_bootstrap (list)------------------------------------------
setMethod("add_data_element_bootstrap", signature(x="list"),
          function(x, n_bootstraps, n_instances, bootstrap_seed_offset, ...){
            
            if(n_bootstraps > 0){
              # Repeat elements.
              data_element <- rep(x, each=n_bootstraps)
              
              # Set bootstrap to TRUE
              bootstrap <- rep(TRUE, times=length(x) * n_bootstraps)
              
              # Set the seeds for the bootstraps to allow for reproducibility.
              bootstrap_seed <- rep(seq(n_bootstraps) + bootstrap_seed_offset, times=length(x))
              
              # Iterate over elements to check whether a point estimate should
              # be computed in addition.
              for(current_element in x){
                if(current_element@detail_level %in% c("ensemble", "model") &
                   current_element@estimation_type %in% c("bci", "bootstrap_confidence_interval")){
                  
                  # Add a new element that estimates the point estimate.
                  new_element <- current_element
                  new_element@estimation_type <- "point"
                  
                  # Add the element to the list of elements.
                  data_element <- c(data_element, new_element)
                  bootstrap <- c(bootstrap, FALSE)
                  bootstrap_seed <- c(bootstrap_seed, NA)
                }
              }
              
            } else {
              # Use the list x of data_elements.
              data_element <- x
              
              # No bootstraps need to be created.
              bootstrap <- rep(FALSE, times=length(x))
              
              # No seed is set
              bootstrap_seed <- rep(NA, times=length(x))
            }
            
            return(list("data_element"=data_element,
                        "bootstrap"=bootstrap,
                        "seed"=bootstrap_seed))
          })


#####add_data_element_bootstrap (familiarDataElement)---------------------------
setMethod("add_data_element_bootstrap", signature(x="familiarDataElement"),
          function(x, ...){
            
            return(add_data_element_bootstrap(x=list(x),
                                              ...))
          })

#####.identifier_as_data_attribute----------------------------------------------
setMethod(".identifier_as_data_attribute", signature(x="familiarDataElement"),
          function(x, identifier, as_grouping_column=TRUE){
            if(length(identifier) == 0) ..error_reached_unreachable_code(".identifier_as_data_attribute: Cannot pass an empty identifier.")
            
            # If an "all" value is passed (e.g. during export), all identifiers
            # are added to the data.
            if(any(identifier == "all")) identifier <- names(x@identifiers)
            
            # Determine which of the identifiers is actually present. If none
            # are present, return x.
            identifier_present <- intersect(identifier, names(x@identifiers))
            if(length(identifier_present) == 0) return(x)
            
            if(as_grouping_column) x@grouping_column <- unique(c(x@grouping_column, identifier_present))
            # Determine the indices of the selected list elements.
            identifier_index <- which(names(x@identifiers) %in% identifier_present)
            
            # Find values.
            identifier_values <- x@identifiers[identifier_index]
            
            # Remove identifiers from list.
            x@identifiers[identifier_index] <- NULL
            
            if(data.table::is.data.table(x@data)){
              # The data element is a data.table.
              
              # Iterate over identifier names.
              for(id_name in names(identifier_values)){
                # Add identifier to the dataset.
                data.table::set(x=x@data,
                                j=id_name,
                                value=identifier_values[[id_name]])
              }
              
            } else if(is.list(x@data)){
              # Determine the number of instances in x@data
              n_instances <- length(x@data[[1]])
              
              # Set length og
              new_data <- lapply(identifier_values, function(x, n) (rep(x, times=n)), n=n_instances)
              names(new_data) <- names(identifier_values)
              
              # Add identifiers to the list.
              x@data <- c(x@data, new_data)
            }
            
            return(x)
          })


#####identify_element_sets (list)-----------------------------------------------
setMethod("identify_element_sets", signature(x="list"),
          function(x, ...){
            
            # Check that that the list is not empty.
            if(is_empty(x)) return(NULL)
            
            # Check for empty elements.
            empty_elements <- sapply(x, is_empty)
            if(all(empty_elements)) return(NULL)
            
            # Iterate over list.
            id_table <- lapply(x, identify_element_sets, ...)
            
            # Combine to table and add group ids and model ids.
            id_table <- data.table::rbindlist(id_table, use.names=TRUE)
            
            # Add group identifier.
            id_table[, "group_id":=.GRP, by=c(colnames(id_table))] 
            
            # Add element identifier.
            id_table[, "element_id":=.I]
            
            return(id_table)
          })

#####identify_element_sets (familiarDataElement)--------------------------------
setMethod("identify_element_sets", signature(x="familiarDataElement"),
          function(x, ignore_estimation_type=FALSE, ...){
            
            # Get the identifiers and the detail level and combine to a list.
            id_list <- c(x@identifiers,
                         list("detail_level"=x@detail_level))
            
            # Add the estimation type if it is not to be ignored.
            if(!ignore_estimation_type) id_list <- c(id_list, list("estimation_type"=x@estimation_type))
            
            return(data.table::as.data.table(id_list))
          })

#####identify_element_sets (NULL)--------------------------------
setMethod("identify_element_sets", signature(x="NULL"),
          function(x, ignore_estimation_type=FALSE, ...){
            return(NULL)
          })



#####merge_data_elements (list)-------------------------------------------------
setMethod("merge_data_elements", signature(x="list"),
          function(x, ...){
            
            # Check that the list is not empty.
            if(is_empty(x)) return(NULL)
            
            # Flatten (nested) lists.
            x <- unlist(x)
            if(!is.list(x)) x <- list(x)
            
            # Check that at least one of the data elements in the list is not
            # empty.
            if(all(sapply(x, is_empty))) return(NULL)
            
            # Run familiarDataElement-specific analysis. This means that we pass
            # the first element as x with the list of elements.
            return(merge_data_elements(x=x[[1]], x_list=x, ...))
          })


#####merge_data_elements (familiarDataElement)----------------------------------
setMethod("merge_data_elements", signature(x="familiarDataElement"),
          function(x, x_list, as_data=NULL, as_grouping_column=TRUE, force_data_table=FALSE,...){
            
            # Move identifiers from the identifiers attribute to the data
            # attribute. The primary reason for doing so is to group and merge
            # similar elements, byt e.g. from different models.
            if(!is.null(as_data)) x_list <- lapply(x_list,
                                                   .identifier_as_data_attribute,
                                                   identifier=as_data,
                                                   as_grouping_column=as_grouping_column)
            
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
              
              if(any(sapply(x_list[current_group_data_element_ids], function(x) (data.table::is.data.table(x@data))))){
                # Data attribute contains data.table.
                data_attribute <- lapply(x_list[current_group_data_element_ids], function(x) (x@data))
                
                # Combine data attributes.
                data_attribute <- data.table::rbindlist(data_attribute,
                                                        use.names=TRUE,
                                                        fill=TRUE)
                
                # Set data attribute.
                prototype_data_element@data <- data_attribute
                
              } else if(any(sapply(x_list[current_group_data_element_ids], function(x) (is.list(x@data))))) {
                # Data attribute contains data.table.
                element_names <- unique(unlist(lapply(x_list[current_group_data_element_ids], function(x) (names(x@data)))))
                
                # Iterate over different names in the list.
                data_attribute <- lapply(element_names, function(ii, x){
                  # Find values for the element.
                  element_values <- unlist(lapply(x,
                                                  function(x, ii) (x@data[[ii]]),
                                                  ii=ii))
                  
                  return(element_values)
                },
                x=x_list[current_group_data_element_ids])
                
                # Set names.
                names(data_attribute) <- element_names
                
                # Force to data attribute.
                if(force_data_table) data_attribute <- data.table::as.data.table(data_attribute)
                
                # Set data attribute.
                prototype_data_element@data <- data_attribute
                
              } else if(all(sapply(x_list[current_group_data_element_ids], function(x) (is_empty(x@data))))) {
                # All data attributes are unset. We don't need to do anything.
                
              } else {
                # Unknown data type.
                ..error_reached_unreachable_code("merge_data_elements,familiarDataElement: data attribute is neither data.table, list, or empty.")
              }
              
              # Add merged data element to the list.
              data_elements <- c(data_elements, list(prototype_data_element))
            }
            
            return(data_elements)
          })



#####collect (list)-------------------------------------------------------------
setMethod("collect", signature(x="list"),
          function(x, data_slot, identifiers=c("data_set", "fs_method", "learner"), ...){
            
            # Collect from all 
            collected_data_elements <- lapply(x,
                                              collect,
                                              data_slot=data_slot,
                                              identifiers=identifiers)
            
            # Flatten (nested) lists.
            collected_data_elements <- unlist(collected_data_elements)
            if(!is.list(collected_data_elements)) collected_data_elements <- list(collected_data_elements)
            
            # Select unique elements. First identify which elements are present.
            id_table <- identify_element_sets(collected_data_elements)
            
            # Check that the identifier table is not empty.
            if(is_empty(id_table)) return(NULL)
            
            # Identify the first element id in each group.
            unique_elements <- sapply(split(id_table, by="group_id"), function(x) (head(x$element_id, n=1L)))
            
            # Keep unique elements.
            collected_data_elements <- collected_data_elements[unique_elements]
            
            return(collected_data_elements)
          })

#####collect (familiarData)-----------------------------------------------------
setMethod("collect", signature(x="familiarData"),
          function(x, data_slot, identifiers, ...){
            
            # Collect data elements.
            collected_data_elements <- slot(x, name=data_slot)
            
            if(is_empty(collected_data_elements)) return(NULL)
            
            # Add elements
            if("data_set" %in% identifiers){
              collected_data_elements <- add_data_element_identifier(x=collected_data_elements,
                                                                     data_set=x@name)
            }
            
            if("fs_method" %in% identifiers){
              collected_data_elements <- add_data_element_identifier(x=collected_data_elements,
                                                                     fs_method=x@fs_method)
            }
            
            if("learner" %in% identifiers){
              collected_data_elements <- add_data_element_identifier(x=collected_data_elements,
                                                                     learner=x@learner)
            }
            
            return(collected_data_elements)
          })


#####.export (familiarCollection)------------------------------------------------
setMethod(".export", signature(x="familiarCollection"),
          function(x, data_slot, dir_path=NULL, type, subtype,...){
            
            # Get the back element.
            data_elements <- slot(x, name=data_slot)
            
            if(is_empty(data_elements)) return(NULL)
            
            if(is(data_elements, "familiarDataElement")){
              data <- .export(x=data_elements,
                              x_list=list(data_elements),
                              ...)
              
            } else {
              data <- .export(x=data_elements[[1]],
                              x_list=data_elements,
                              ...)
            }
            
            # Apply labels.
            data <- .apply_labels(data=data,
                                  object=x)
            
            # Check that the data variable is not empty
            if(is_empty(data)) return(NULL)
            
            if(is.null(dir_path)){
              # Export data.
              return(data)
              
            } else {
              
              # Check that the data variable is a data.table.
              if(!data.table::is.data.table(data)) ..error_reached_unreachable_code(".export,familiarCollection: data is not a single data.table.")
              
              # Export to file
              .export_to_file(data=data,
                              object=x,
                              dir_path=dir_path,
                              type=type,
                              subtype=paste(subtype, type, sep="_"))
              
              return(NULL)
            }
          })

#####.export (familiarDataElement)-----------------------------------------------
setMethod(".export", signature(x="familiarDataElement"),
          function(x, x_list, aggregate_results=FALSE, ...){
            
            if(aggregate_results){
              x_list <- .compute_data_element_estimates(x_list)
            }
            
            # Merge data elements.
            x <- merge_data_elements(x=x_list,
                                     as_data="all",
                                     as_grouping_column=TRUE,
                                     force_data_table=TRUE)
            
            return(x)
          })



#####extract_dispatcher---------------------------------------------------------

#'@title Internal function to dispatch extraction functions.
#'
#'@description This function provides a unified access point to extraction
#'  functions. Some of these functions require bootstrapping and result
#'  aggregation, which are handled here.
#'
#'@param FUN Extraction function or method to which data and parameters are
#'  dispatched.
#'@param object A `familiarEnsemble` object.
#'@param proto_data_element A `familiarDataElement` object, or an object that
#'  inherits from it.
#'@param aggregate_results A logical flag indicating whether results should be
#'  aggregated.
#'@param has_internal_bootstrap A logical flag that indicates whether `FUN` has
#'  internal bootstrapping capabilities.
#'
#'@inheritParams extract_data
#'
#'@details This function first determines how many data points need to be
#'  evaluated to complete the desired estimation, i.e. 1 for point estimates, 20
#'  for bias-corrected estimates, and 20 / (1 - confidence level) for bootstrap
#'  confidence intervals.
#'
#'  Subsequently, we determine the number of models. This number is used to set
#'  external or internal clusters, the number of bootstraps, and to evaluate
#'  whether the estimation can be done in case `FUN` does not support
#'  bootstrapping.
#'
#'@return A list of `familiarDataElement` objects.
#'@md
#'@keywords internal
setMethod("extract_dispatcher", signature(object="familiarEnsemble", proto_data_element="familiarDataElement"),
          function(cl=NULL,
                   FUN,
                   object,
                   proto_data_element,
                   aggregate_results,
                   has_internal_bootstrap,
                   ...,
                   message_indent=0L,
                   verbose=TRUE){
            
            # Check that any models were trained.
            if(!model_is_trained(object)) return(NULL)
            
            # Determine the number of instances we need to find the estimates.
            if(proto_data_element@estimation_type == "point"){
              n_instances <- 1L
              
            } else if(proto_data_element@estimation_type %in% c("bias_correction", "bc")) {
              n_instances <- 20L
              
            } else if(proto_data_element@estimation_type %in% c("bootstrap_confidence_interval", "bci")){
              n_instances <- ceiling(20 / (1.00 - proto_data_element@confidence_level))
            }
            
            # Determine the number of models we need to evaluate.
            if(proto_data_element@detail_level == "ensemble"){
              n_models <- 1L
              
            } else {
              n_models <- length(object@model_list)
            }
            
            
            # Check if the proposed analysis can be executed.
            if(!has_internal_bootstrap &
               n_instances > 1L &
               !(proto_data_element@detail_level == "hybrid" & n_models >= n_instances)){
              # The required number of instances cannot be created from models
              # alone.
              
              # Add a message
              if(verbose) message("extract_dispatcher,familiarEnsemble,familiarDataElement: too few models to compute confidence intervals.")
              
              # Set the detail level to ensemble.
              if(proto_data_element@detail_level == "hybrid"){
                # Only one ensemble model is formed.
                proto_data_element@detail_level <- "ensemble"
                n_models <- 1L
              } 
              
              # Set the estimation type to point estimates.
              proto_data_element@estimation_type <- "point"
              n_instances <- 1L
            } 
            
            
            # Determine the number of bootstraps that should be computed
            # internally.
            if(has_internal_bootstrap){
              if(proto_data_element@detail_level == "hybrid"){
                n_bootstraps <- ceiling(n_instances / n_models)
                n_instances <- n_models * n_bootstraps
                
              } else {
                n_bootstraps <- n_instances
              }
              
            } else {
              n_bootstraps <- 0L
            }
            
            # If one bootstrap is required, that means no bootstraps are
            # required.
            if(n_bootstraps <= 1L) n_bootstraps <- 0L
            
            
            # Determine the number of parallel cluster nodes.
            n_nodes <- length(cl)
            
            # Determine whether we should perform parallel processing across
            # models or internally.
            if(n_nodes > 1){
              if(n_models == 1){
                # No need to perform parallelisation across models, when there
                # is only 1 model.
                parallel_external <- FALSE
                
              } else if(n_bootstraps == 0) {
                # No need to perform internal parallelisation in case this is
                # not necessary. This check is hit when has_internal_bootstrap
                # is false.
                parallel_external <- TRUE
                
              } else if(n_models >= 0.80 * n_nodes){
                # Perform external parallelisation if the number of models would
                # occupy at least 80% of the nodes. This is because the parallel
                # overhead in any internal bootstrapping takes up more time.
                parallel_external <- TRUE
                
              } else if(n_models > n_bootstraps){
                # Perform external parallelisation if the number of bootstraps
                # would underutilize the nodes compared to the number of nodes.
                parallel_external <- TRUE
                
              } else {
                parallel_external <- FALSE
              }
              
            } else {
              # Back-up when the number of nodes is 1 or none.
              parallel_external <- FALSE
            }
            
            # Message user concerning evaluation
            # - Type of estimation
            # - Which model(s)
            # - How many bootstraps each (if n models > 1), as well as total.
            # - If parallelisation takes place, and where.
            ..message_dispatcher_details(estimation_type = proto_data_element@estimation_type,
                                         detail_level = proto_data_element@detail_level,
                                         n_bootstraps = n_bootstraps,
                                         n_instances = n_instances,
                                         n_models = n_models,
                                         n_nodes = n_nodes,
                                         parallel_external = parallel_external,
                                         message_indent = message_indent,
                                         verbose = verbose)
            
            # Dispatch for ensemble models.
            if(proto_data_element@detail_level == "ensemble"){
              # Dispatch for results aggregated at the ensemble level.
              x <- .extract_dispatcher_ensemble(cl=cl,
                                                FUN=FUN,
                                                object=object,
                                                proto_data_element=proto_data_element,
                                                aggregate_results=aggregate_results,
                                                n_instances=n_instances,
                                                n_bootstraps=n_bootstraps,
                                                parallel_external=parallel_external,
                                                message_indent = message_indent,
                                                verbose=verbose,
                                                ...)
              
            } else if(proto_data_element@detail_level == "hybrid"){
              # Dispatch for results aggregated with hybrid details.
              x <- .extract_dispatcher_hybrid(cl=cl,
                                              FUN=FUN,
                                              object=object,
                                              proto_data_element=proto_data_element,
                                              aggregate_results=aggregate_results,
                                              n_instances=n_instances,
                                              n_bootstraps=n_bootstraps,
                                              parallel_external=parallel_external,
                                              message_indent = message_indent,
                                              verbose=verbose,
                                              ...)
              
            } else if(proto_data_element@detail_level == "model"){
              # Dispatch for results aggregated at the model level.
              x <- .extract_dispatcher_model(cl=cl,
                                             FUN=FUN,
                                             object=object,
                                             proto_data_element=proto_data_element,
                                             aggregate_results=aggregate_results,
                                             n_instances=n_instances,
                                             n_bootstraps=n_bootstraps,
                                             parallel_external=parallel_external,
                                             message_indent = message_indent,
                                             verbose=verbose,
                                             ...)
              
            } else {
              ..error_reached_unreachable_code(paste0("extract_dispatcher: encountered an unknown detail_level attribute: ",
                                                      proto_data_element@detail_level))
            }
            
            return(x)
          })


.extract_dispatcher_ensemble <- function(cl=NULL,
                                         FUN,
                                         object,
                                         proto_data_element,
                                         aggregate_results,
                                         n_instances,
                                         n_bootstraps,
                                         parallel_external,
                                         ...,
                                         verbose=FALSE){
  
  # Add ensemble model name.
  proto_data_element <- add_model_name(proto_data_element,
                                       object=object)
  
  # Set flag for interval aggregation.
  aggregate_internal <- aggregate_results & n_instances == n_bootstraps & n_bootstraps > 0
  
  # Never perform outer-loop parallelisation when dispatching for ensemble-level
  # details.
  x <- FUN(cl = cl,
           object = object,
           bootstrap_seed_offset = 0,
           proto_data_element = proto_data_element,
           aggregate_results = aggregate_internal,
           n_instances = n_instances,
           n_bootstraps = n_bootstraps,
           verbose = verbose,
           progress_bar = verbose,
           ...)
  
  # Merge data elements together.
  x <- merge_data_elements(x)
  
  # Aggregate results if required.
  if(aggregate_results){
    x <- .compute_data_element_estimates(x)
  }
  
  return(x)
}



.extract_dispatcher_hybrid <- function(cl=NULL,
                                       FUN,
                                       object,
                                       proto_data_element,
                                       aggregate_results,
                                       n_instances,
                                       n_bootstraps,
                                       parallel_external,
                                       ...,
                                       verbose=FALSE){
  
  # Add ensemble model name.
  proto_data_element <- add_model_name(proto_data_element, object=object)
  
  if(parallel_external){
    x <- fam_mapply(cl=cl,
                    FUN=FUN,
                    object=object@model_list,
                    bootstrap_seed_offset = seq(from=0, by=n_bootstraps, length.out=length(object@model_list)),
                    MoreArgs=c(list("proto_data_element"=proto_data_element,
                                    "aggregate_results"=FALSE,
                                    "n_instances"=n_instances,
                                    "n_bootstraps"=n_bootstraps,
                                    "verbose"=verbose,
                                    "progress_bar"= verbose & length(object@model_list) == 1),
                               list(...)),
                    progress_bar = verbose & length(object@model_list) > 1)
    
  } else {
    x <- fam_mapply(cl=NULL,
                    FUN=FUN,
                    object=object@model_list,
                    bootstrap_seed_offset = seq(from=0, by=n_bootstraps, length.out=length(object@model_list)),
                    MoreArgs=c(list("cl"=cl,
                                    "proto_data_element"=proto_data_element,
                                    "aggregate_results"=FALSE,
                                    "n_instances"=n_instances,
                                    "n_bootstraps"=n_bootstraps,
                                    "verbose"=verbose,
                                    "progress_bar"= verbose & length(object@model_list) == 1),
                               list(...)),
                    progress_bar = verbose & length(object@model_list) > 1)
  }
  
  # Merge data elements together. The model_name identifier gets added as data
  # instead, but not as a grouping column.
  x <- merge_data_elements(x, as_data="model_name", as_grouping_column=FALSE)
  
  # Create point estimate from the data.
  if(proto_data_element@estimation_type %in% c("bootstrap_confidence_interval", "bci")){
    x <- .add_point_estimate_from_elements(x)
  }
  
  # Aggregate results if required.
  if(aggregate_results){
    x <- .compute_data_element_estimates(x)
  }
  
  return(x)
}



.extract_dispatcher_model <- function(cl=NULL,
                                      FUN,
                                      object,
                                      proto_data_element,
                                      aggregate_results,
                                      n_instances,
                                      n_bootstraps,
                                      parallel_external,
                                      ...,
                                      verbose=FALSE){
  
  # Create a list of data-elements.
  proto_data_element <- rep.int(list(proto_data_element),
                                times=length(object@model_list))
  
  # Add model (not ensemble) names to the prototype data elements.
  proto_data_element <- mapply(add_model_name,
                               data=proto_data_element,
                               object=object@model_list)
  
  # Set flag for interval aggregation.
  aggregate_internal <- aggregate_results & n_instances == n_bootstraps & n_bootstraps > 0
  
  if(parallel_external){
    x <- fam_mapply(cl=cl,
                    FUN=FUN,
                    object=object@model_list,
                    bootstrap_seed_offset = seq(from=0, by=n_bootstraps, length.out=length(object@model_list)),
                    proto_data_element=proto_data_element,
                    MoreArgs=c(list("aggregate_results" = aggregate_internal,
                                    "n_instances"=n_instances,
                                    "n_bootstraps"=n_bootstraps,
                                    "verbose"=verbose,
                                    "progress_bar"= verbose & length(object@model_list) == 1),
                               list(...)),
                    progress_bar = verbose & length(object@model_list) > 1)
    
  } else {
    x <- fam_mapply(cl=NULL,
                    FUN=FUN,
                    object=object@model_list,
                    bootstrap_seed_offset = seq(from=0, by=n_bootstraps, length.out=length(object@model_list)),
                    proto_data_element=proto_data_element,
                    MoreArgs=c(list("cl"=cl,
                                    "aggregate_results" = aggregate_internal,
                                    "n_instances"=n_instances,
                                    "n_bootstraps"=n_bootstraps,
                                    "verbose"=verbose,
                                    "progress_bar"= verbose & length(object@model_list) == 1),
                               list(...)),
                    progress_bar = verbose & length(object@model_list) > 1)
  }
  
  # Merge data elements together.
  x <- merge_data_elements(x)
  
  # Aggregate results if required.
  if(aggregate_results){
    x <- .compute_data_element_estimates(x)
  }
  
  return(x)
}



.add_point_estimate_from_elements <- function(x){
  
  if(is_empty(x)) return(NULL)
  
  # Find any unique elements that have not been aggregated and are not empty.
  id_table <- identify_element_sets(x, ignore_estimation_type=TRUE)
  
  # Identify the element identifiers that should be grouped.
  grouped_data_element_ids <- lapply(split(id_table[, c("element_id", "group_id")], by="group_id"), function(id_table) (id_table$element_id))
  
  # List of data elements.
  data_elements <- list()
  
  for(current_group_data_element_ids in grouped_data_element_ids){
    
    # Check that there is no point estimate present in the current table.
    if(any(sapply(x[current_group_data_element_ids], function(x) (x@estimation_type == "point")))) next()
    
    # Set conversion back to list, in case this is required.
    data_as_list <- FALSE
    
    # Copy the first data element in the group and use it as a
    # prototype.
    prototype_data_element <- x[[current_group_data_element_ids[1]]]
    
    # Set point estimate.
    prototype_data_element@estimation_type <- "point"
    
    # Check if all data are empty.
    if(all(sapply(x[current_group_data_element_ids], function(x) (is_empty(x@data))))){
      
      # Add empty element to data_elements and skip to the next
      data_elements <- c(data_elements,
                         list(prototype_data_element))
      
      next()
    }
    
    # Extract the data.
    if(any(sapply(x[current_group_data_element_ids], function(x) (data.table::is.data.table(x@data))))){
      # Data attribute contains data.table.
      data <- lapply(x[current_group_data_element_ids], function(x) (x@data))
      
    } else if(any(sapply(x[current_group_data_element_ids], function(x) (is.list(x@data))))) {
      # Convert all lists to data tables.
      data <- lapply(x[current_group_data_element_ids], function(x) (data.table::as.data.table(x@data)))
      
      # Convert back to list in the end.
      data_as_list <- TRUE
    }
    
    # Combine data attributes.
    data <- data.table::rbindlist(data,
                                  use.names=TRUE,
                                  fill=TRUE)
    
    if(length(prototype_data_element@grouping_column) > 0){
      # Compute the mean value as point estimate.
      data <- data[, list("value"=mean(get(prototype_data_element@value_column), na.rm=TRUE)),
                   by=c(prototype_data_element@grouping_column)]
      
    } else {
      data <- data[, list("value"=mean(get(prototype_data_element@value_column), na.rm=TRUE))]
    }
    
    # Rename the "value" column to the actual name.
    data.table::setnames(data, old="value", new=prototype_data_element@value_column)
    
    # Convert to list again, if necessary.
    if(data_as_list) data <- as.list(data)
    
    # Update data attribute with point estimate.
    prototype_data_element@data <- data
    
    # Add merged data element to the list.
    data_elements <- c(data_elements, list(prototype_data_element))
  }
  
  return(c(x, data_elements))
}



.compute_data_element_estimates  <- function(x){
  
  # Merge data.
  data_elements <- merge_data_elements(x=x)
  
  # Find any data elements that were already aggregated and keep these apart.
  is_aggregated <- sapply(data_elements, function(x) (x@is_aggregated))
  if(all(is_aggregated)) return(data_elements)
  
  # Continue with unaggregated data elements.
  unaggregated_data_elements <- data_elements[!is_aggregated]
  data_elements <- data_elements[is_aggregated]
  
  # Find any unique elements that have not been aggregated.
  id_table <- identify_element_sets(unaggregated_data_elements,
                                    ignore_estimation_type=TRUE)
  
  # Identify the element identifiers that should be grouped.
  grouped_data_element_ids <- lapply(split(id_table[, c("element_id", "group_id")], by="group_id"), function(id_table) (id_table$element_id))
  
  # Aggregate unaggregated data.
  for(current_group_data_element_ids in grouped_data_element_ids){
    
    # Select data elements corresponding to the current group.
    current_data_elements <- unaggregated_data_elements[current_group_data_element_ids]
    
    # Identify the estimation types of the current data elements.
    current_estimation_types <- sapply(current_data_elements, function(x) (x@estimation_type))
    
    if(any(sapply(current_data_elements, is_empty))){
      # Don't aggregate empty elements.
      aggregated_data_element <- current_data_elements[[1]]
      
    } else if(any(current_estimation_types %in% c("bci", "bootstrap_confidence_interval"))){
      
      # Check the number of elements.
      if(length(current_estimation_types) != 2L) ..error_reached_unreachable_code(".compute_data_element_estimates: exactly two data elements are required for bootstrap confidence intervals.")
      if(!any(current_estimation_types %in% c("point"))) ..error_reached_unreachable_code(".compute_data_element_estimates: a point estimate is required for bootstrap confidence intervals.")
      
      # Select point estimate.
      point_values <- data.table::as.data.table(current_data_elements[current_estimation_types == "point"][[1]]@data)
      point_values[, "estimation_type":="point"]
      
      # Select bootstrap values
      bootstrap_values <- data.table::as.data.table(current_data_elements[current_estimation_types %in% c("bci", "bootstrap_confidence_interval")][[1]]@data)
      bootstrap_values[, "estimation_type":="bootstrap_confidence_interval"]
      
      # Combine to single table.
      data <- data.table::rbindlist(list(point_values, bootstrap_values), use.names=TRUE, fill=TRUE)
      
      if(length(current_data_elements[[1]]@grouping_column > 0)){
        # Split table by grouping column and compute estimate and confidence intervals.
        data <- lapply(split(data, by=current_data_elements[[1]]@grouping_column),
                       ..compute_bootstrap_confidence_estimate,
                       confidence_level = current_data_elements[[1]]@confidence_level,
                       bootstrap_ci_method = current_data_elements[[1]]@bootstrap_ci_method,
                       value_column = current_data_elements[[1]]@value_column,
                       grouping_column = current_data_elements[[1]]@grouping_column)
        
        # Combine to single table
        data <- data.table::rbindlist(data, use.names=TRUE, fill=TRUE)
        
      } else {
        # Compute in absence of grouping columns.
        data <- ..compute_bootstrap_confidence_estimate(x = data,
                                                        confidence_level = current_data_elements[[1]]@confidence_level,
                                                        bootstrap_ci_method = current_data_elements[[1]]@bootstrap_ci_method,
                                                        value_column = current_data_elements[[1]]@value_column)
      }
      
      # Update the data attribute.
      aggregated_data_element <- current_data_elements[current_estimation_types %in% c("bci", "bootstrap_confidence_interval")][[1]]
      aggregated_data_element@data <- data
      
    } else if(any(current_estimation_types %in% c("bc", "bias_correction"))){
      
      # Check the number of elements.
      if(length(current_estimation_types) != 1L) ..error_reached_unreachable_code(".compute_data_element_estimates: exactly one data element is required for bias corrected estimates.")
      
      # Select values.
      bootstrap_values <- data.table::as.data.table(current_data_elements[current_estimation_types %in% c("bc", "bias_correction")][[1]]@data)
      
      if(length(current_data_elements[[1]]@grouping_column > 0)){
        # Split table by grouping column and compute bias corrected estimate.
        data <- lapply(split(bootstrap_values, by=current_data_elements[[1]]@grouping_column),
                       ..compute_bias_corrected_estimate,
                       value_column = current_data_elements[[1]]@value_column,
                       grouping_column = current_data_elements[[1]]@grouping_column)
        
        # Combine to single table
        data <- data.table::rbindlist(data, use.names=TRUE, fill=TRUE)
        
      } else {
        # Compute in absence of grouping columns.
        data <- ..compute_bias_corrected_estimate(x = bootstrap_values,
                                                  value_column = current_data_elements[[1]]@value_column)
      }
      
      # Update the data attribute.
      aggregated_data_element <- current_data_elements[[1]]
      aggregated_data_element@data <- data
      
    } else if(any(current_estimation_types %in% c("point"))){
      # This follows the same procedure as for bias-corrected estimates. For
      # ensemble and hybrid detail levels a single value needs to be generated.
      # However, in the case of hybrid detail level, a point estimate is created
      # for each model, and requires aggregation.
      
      # Select values.
      bootstrap_values <- data.table::as.data.table(current_data_elements[current_estimation_types %in% c("point")][[1]]@data)
      
      if(length(current_data_elements[[1]]@grouping_column > 0)){
        # Split table by grouping column and compute bias corrected estimate.
        data <- lapply(split(bootstrap_values, by=current_data_elements[[1]]@grouping_column),
                       ..compute_bias_corrected_estimate,
                       value_column = current_data_elements[[1]]@value_column,
                       grouping_column = current_data_elements[[1]]@grouping_column)
        
        # Combine to single table
        data <- data.table::rbindlist(data, use.names=TRUE, fill=TRUE)
        
      } else {
        # Compute in absence of grouping columns.
        data <- ..compute_bias_corrected_estimate(x = bootstrap_values,
                                                  value_column = current_data_elements[[1]]@value_column)
      }
      
      # Update the data attribute.
      aggregated_data_element <- current_data_elements[[1]]
      aggregated_data_element@data <- data
      
      # Check the number of elements.
      if(length(current_estimation_types) != 1L) ..error_reached_unreachable_code(".compute_data_element_estimates: exactly one data element is required for point estimates.")
      
      # The aggregated point element is itself.
      aggregated_data_element <- current_data_elements[[1]]
      aggregated_data_element@data <- data
      
    } else {
      ..error_reached_unreachable_code(paste0(".compute_data_element_estimates: unknown estimation type: ", paste_s(current_estimation_types)))
    }
    
    # Update the is_aggregated attribute.
    aggregated_data_element@is_aggregated <- TRUE
    
    # Add aggregated element.
    data_elements <- c(data_elements, list(aggregated_data_element))
  }
  
  return(data_elements)
}



..compute_bootstrap_confidence_estimate <- function(x, value_column, confidence_level, bootstrap_ci_method, grouping_column=NULL){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  estimation_type <- NULL
  
  # Compute the estimate and its bootstrap confidence interval.
  ci_estimate <- ..bootstrap_ci(x = x[estimation_type == "bootstrap_confidence_interval"][[value_column]],
                                x_0 = x[estimation_type == "point"][[value_column]],
                                confidence_level = confidence_level,
                                bootstrap_ci_method = bootstrap_ci_method)
  
  # Convert to data.table.
  ci_estimate <- data.table::as.data.table(ci_estimate)
  
  # Add in grouping columns, if any.
  if(length(grouping_column) > 0){
    ci_estimate <- cbind(head(x[, mget(grouping_column)], n=1L),
                         ci_estimate)
  }
  
  # Rename the "median" column.
  data.table::setnames(x=ci_estimate,
                       old="median",
                       new=value_column)
  
  return(ci_estimate)
}



..compute_bias_corrected_estimate <- function(x, value_column, grouping_column=NULL){
  
  # Compute the bias-corrected estimate.
  bc_estimate <- ..bootstrap_bias_correction(x = x[[value_column]])
  
  # Convert to data.table.
  bc_estimate <- data.table::as.data.table(bc_estimate)
  
  # Add in grouping columns, if any.
  if(length(grouping_column) > 0){
    bc_estimate <- cbind(head(x[, mget(grouping_column)], n=1L),
                         bc_estimate)
  }
  
  # Rename the "median" column.
  data.table::setnames(x=bc_estimate,
                       old="median",
                       new=value_column)
  
  return(bc_estimate)
}



..message_dispatcher_details <- function(estimation_type,
                                         detail_level,
                                         n_bootstraps,
                                         n_instances,
                                         n_models,
                                         n_nodes,
                                         parallel_external,
                                         message_indent,
                                         verbose){
  
  # Skip if the dispatcher is not verbose.
  if(!verbose) return(NULL)
  
  # Set the estimator string.
  estimator_str <- switch(estimation_type,
                          "point" = "point estimate",
                          "bc" = "bias-corrected estimate",
                          "bias_correction" = "bias-corrected estimate",
                          "bci" = "bias-corrected estimate with confidence interval",
                          "bootstrap_confidence_interval" = "bias-corrected estimate with confidence interval")
  
  # Set the detail level string.
  detail_level_str <- switch(detail_level,
                             "ensemble" = "the ensemble model as a whole. ",
                             "hybrid" = paste0("the ensemble model from the ",
                                               ifelse(n_models > 1,
                                                      paste0(n_models, " underlying models. "),
                                                      paste0("single underlying model. "))),
                             "model" = paste0(ifelse(n_models > 1,
                                                     paste0("each of the ", n_models, " models"),
                                                     paste0("the single model")),
                                              " in the ensemble. "))
  
  # Bootstraps.
  if(n_bootstraps > 0L){
    bootstrap_str <- paste0(n_bootstraps, " bootstrap samples are obtained ",
                            ifelse(n_models > 1L,
                                   paste0("for each model (total: ", n_instances, "). "),
                                   "in total. "))
    
  } else {
    bootstrap_str <- ""
  }
  
  # Parallelisation.
  if(n_nodes > 1L){
    if(parallel_external){
      parallel_str <- paste0("Computation is parallelised over models.")
      
    } else {
      parallel_str <- paste0("Computation is parallelised over bootstraps.")
    }
    
  } else {
    parallel_str <- ""
  }
  
  logger.message(paste0("Computing the ",
                        estimator_str,
                        " of the value(s) of interest for ",
                        detail_level_str,
                        bootstrap_str,
                        parallel_str),
                 indent=message_indent)
}

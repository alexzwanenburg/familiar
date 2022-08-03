#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R



#### show (vimpTable) ----------------------------------------------------------
setMethod("show", signature(object="vimpTable"),
          function(object){
            # Suppress NOTES due to non-standard evaluation in data.table
            score <- NULL
            
            # Make sure the object is updated.
            object <- update_object(object=object)
            
            # Create an initial descriptor.
            if(is_empty(object)){
              cat(paste0("An empty variable importance table for the ", object@vimp_method,
                         " variable importance method ",
                         "(", .familiar_version_string(object), ")."))
              
            } else {
              cat(paste0("A variable importance table for the ", object@vimp_method,
                         " variable importance method ",
                         "(", .familiar_version_string(object), "):\n\n"))
              
              if(object@invert){
                show(object@vimp_table[order(-score)])
                
              } else {
                show(object@vimp_table[order(score)]) 
              }
            }
          })



#'@title Extract variable importance table.
#'
#'@description This method retrieves and parses variable importance tables from
#'  their respective `vimpTable` objects.
#'
#'@param x Variable importance (`vimpTable`) object, a list thereof, or one or
#'  more paths to these objects. This method extracts the variable importance
#'  table from such objects.
#'@param state State of the returned variable importance table. This affects
#'  what contents are shown, and in which format. The variable importance table
#'  can be returned with the following states:
#'
#'  * `initial`: initial state, directly after the variable importance table is
#'  filled. The returned variable importance table shows the raw, un-processed
#'  data.
#'
#'  * `decoded`: depending on the variable importance method, the initial
#'  variable importance table may contain the scores of individual contrasts for
#'  categorical variables. When decoded, scores from all contrasts are
#'  aggregated to a single score for each feature.
#'
#'  * `declustered`: variable importance is determined from fully processed
#'  features, which includes clustering. This means that a single feature in the
#'  variable importance table may represent multiple original features. When a
#'  variable importance table has been declustered, all clusters have been
#'  turned into their constituent features.
#'
#'  * `ranked` (default): The scores have been used to create ranks, with lower
#'  ranks indicating better features.
#'
#'  Internally, the variable importance table will go through each state, i.e.
#'  an variable importance table in the initial state will be decoded,
#'  declustered and then ranked prior to returning the variable importance
#'  table.
#'@param data Internally used argument for use with `familiarModel` objects.
#'@param as_object Internally used argument for use with `familiarModel`
#'  objects.
#'@param ... Unused arguments.
#'
#'@return A `data.table` with variable importance scores and, with
#'  `state="ranked"`, the respective ranks.
#'@exportMethod get_vimp_table
#'
#'@md
#'@rdname get_vimp_table-methods
setGeneric("get_vimp_table", function(x,
                                      state="ranked",
                                      ...) standardGeneric("get_vimp_table"))

#### get_vimp_table (list) -----------------------------------------------------

#'@rdname get_vimp_table-methods
setMethod("get_vimp_table", signature(x="list"),
          function(x, state="ranked", ...){
            
            # Dispatch to methods for underlying objects.
            return(lapply(x, get_vimp_table, state=state, ...))
          })


#### get_vimp_table (character) ------------------------------------------------

#'@rdname get_vimp_table-methods
setMethod("get_vimp_table", signature(x="character"),
          function(x, state="ranked", ...){
            
            # Dispatch to list, if x contains more than one element.
            if(length(x) > 1) return(get_vimp_table(as.list(x), state=state, ...))
            
            # Attempt to read file.
            x <- readRDS(x)
            
            # Prior to version 1.2.0, files would contain a list of data.
            if(is.list(x)){
              x <- as_vimp_table_object(x,
                                        project_id="")
            }
            
            # Make sure the object is updated.
            x <- update_object(object=x)
            
            # Dispatch to object-specific routines.
            return(get_vimp_table(x=x, state=state, ...))
          })


#### get_vimp_table (vimpTable) ------------------------------------------------

#'@rdname get_vimp_table-methods
setMethod("get_vimp_table", signature(x="vimpTable"),
          function(x, state="ranked", ...){
            # Check that x is not empty.
            if(is_empty(x)) return(NULL)
            
            # Ensure that the variable importance table is processed.
            x <- preprocess_vimp_table(x, stop_at=state)
            
            # Return a copy of the variable importance table attribute.
            return(data.table::copy(x@vimp_table))
          })

#### get_vimp_table (NULL) -----------------------------------------------------

#'@rdname get_vimp_table-methods
setMethod("get_vimp_table", signature(x="NULL"),
          function(x, state="ranked", ...){
            return(NULL)
          })


#### get_vimp_table (experimentData) -------------------------------------------

#'@rdname get_vimp_table-methods
setMethod("get_vimp_table", signature(x="experimentData"),
          function(x, state="ranked", ...){
            # Check if the attribute has been set.
            if(is.null(x@vimp_table_list)){
              warning("No variable importance tables are present.")
              
              return(NULL)
            }
            
            # Iterate over data for the different variable importance methods.
            return(lapply(x@vimp_table_list,
                          get_vimp_table,
                          state=state,
                          ...))
          })

#### get_vimp_table (familiarModel) --------------------------------------------

#'@rdname get_vimp_table-methods
setMethod("get_vimp_table", signature(x="familiarModel"),
          function(x, state="ranked", data=NULL, as_object=FALSE, ...){
            # This method is used to obtain post-hoc variable importance tables
            # from a trained model.
            
            # Determine variable importance using internal routines of the
            # familiarModel object.
            vimp_object <- ..vimp(object=x,
                                  data=data)
            
            # Set up a table with clustering information.
            cluster_table <- .create_clustering_table(x@feature_info)
            
            # Update variable importance table object.
            vimp_object@vimp_method <- x@learner
            vimp_object@run_table <- x@run_table
            vimp_object@cluster_table <- cluster_table
            vimp_object@project_id <- x@project_id
            
            # Set package version.
            vimp_object <- add_package_version(vimp_object)
            
            # Determine what should be returned.
            if(as_object){
              # The as_object flag allows for returning the actual data.
              return(vimp_object)
              
            } else {
              return(get_vimp_table(x=vimp_object,
                                    state=state)) 
            }
          })




#### decode_vimp_table (vimpTable) ---------------------------------------------
setMethod("decode_vimp_table", signature(x="vimpTable"),
          function(x, ...){
            # Use encoding table to parse variable importance table.
            
            # Suppress NOTES due to non-standard evaluation in data.table
            name <- original_name <- score <- NULL
            
            # Check if the table has already been decoded.
            if(.as_vimp_table_state(x@state) >= "decoded") return(x)
            
            # Update state.
            x@state <- "decoded"
            
            # Skip if the vimp table is empty.
            if(is_empty(x)) return(x)
            
            # Skip if there is no encoding table.
            if(is_empty(x@encoding_table)) return(x)
            
            # Split vimp_table into categorical and non-categorical features.
            data_non_categorical <- x@vimp_table[!name %in% x@encoding_table$reference_name, ]
            data_categorical <- x@vimp_table[name %in% x@encoding_table$reference_name, ]
            
            if(!is_empty(data_categorical)){
              
              # Merge with reference.
              data_categorical <- merge(x=data_categorical,
                                        y=x@encoding_table,
                                        by.x="name", by.y="reference_name")
              
              # Summarise score by single value according to the method
              # indicated by the score_aggregation attribute.
              if(x@score_aggregation == "max"){
                data_categorical <- suppressWarnings(data_categorical[, list(score=max(score, na.rm=TRUE)), by=original_name])
                
              } else if(x@score_aggregation == "abs_max"){
                data_categorical <- suppressWarnings(
                  data_categorical[, list(score=max(abs(score), na.rm=TRUE)), by=original_name])
                
              } else if(x@score_aggregation == "min"){
                data_categorical <- suppressWarnings(
                  data_categorical[, list(score=min(score, na.rm=TRUE)), by=original_name])
                
              } else if(x@score_aggregation == "abs_min"){
                data_categorical <- suppressWarnings(
                  data_categorical[, list(score=min(abs(score), na.rm=TRUE)), by=original_name])
                
              } else if(x@score_aggregation == "mean"){
                data_categorical <- suppressWarnings(
                  data_categorical[, list(score=mean(score, na.rm=TRUE)), by=original_name])
                
              } else if(x@score_aggregation == "abs_mean"){
                data_categorical <- suppressWarnings(
                  data_categorical[, list(score=mean(abs(score), na.rm=TRUE)), by=original_name])
                
              } else if(x@score_aggregation == "median"){
                data_categorical <- suppressWarnings(
                  data_categorical[, list(score=stats::median(score, na.rm=TRUE)), by=original_name])
                
              } else if(x@score_aggregation == "abs_median"){
                data_categorical <- suppressWarnings(
                  data_categorical[, list(score=stats::median(abs(score), na.rm=TRUE)), by=original_name])
                
              } else {
                ..error_reached_unreachable_code("decode_vimp_table,vimpTable: unknown score aggregation method")
              }
              
              # Change name of original_name column to name
              data.table::setnames(data_categorical, "original_name", "name")
            }
            
            # Combine to single data.table
            vimp_table <- rbind(data_non_categorical,
                                data_categorical)
            
            # Replace infinite/nan/etc values by NA
            vimp_table[!is.finite(score), "score":=NA_real_]
            
            # Replace vimp_table attribute.
            x@vimp_table <- vimp_table
            
            return(x)
          })

#### decode_vimp_table (NULL) --------------------------------------------------
setMethod("decode_vimp_table", signature(x="NULL"),
          function(x, ...){
            return(x)
          })



#### decluster_vimp_table (vimpTable) ------------------------------------------
setMethod("decluster_vimp_table", signature(x="vimpTable"),
          function(x, show_weights=FALSE, show_cluster_name=FALSE, ...){
            # Check if the table has already been declustered.
            if(.as_vimp_table_state(x@state) >= "declustered") return(x)
            
            # Pre-process the variable importance table.
            x <- preprocess_vimp_table(x, stop_at="decoded")
            
            # Update state.
            x@state <- "declustered"
            
            # Skip if the vimp table is empty.
            if(is_empty(x)) return(x)
            
            # Skip if there is no cluster table to work with.
            if(is_empty(x@cluster_table)) return(x)
            
            # Merge the cluster table into the variable importance table..
            vimp_table <- merge(x=x@vimp_table,
                                y=x@cluster_table,
                                by.x="name",
                                by.y="cluster_name",
                                all.x=FALSE,
                                all.y=FALSE)
            
            # Adapt table by removing the original name column and renaming the
            # feature name column.
            vimp_table[, ":="("name"=NULL, "feature_required"=NULL)]
            data.table::setnames(vimp_table, old="feature_name", new="name")
            
            # Remove weights, unless explicitly stated.
            if(!show_weights & "weight" %in% colnames(vimp_table)) vimp_table[, "weight":=NULL]
            
            # Update vimp_table attribute.
            x@vimp_table <- vimp_table
            
            return(x)
          })

#### decluster_vimp_table (NULL) -----------------------------------------------
setMethod("decluster_vimp_table", signature(x="NULL"),
          function(x, ...){
            return(x)
          })



#### recluster_vimp_table (list) -----------------------------------------------
setMethod("recluster_vimp_table", signature(x="list"),
          function(x, ...){
            # If the list is empty, return NULL instead.
            if(is_empty(x)) return(NULL)
            
            # Dispatch to method for single variable importance tables.
            return(lapply(x,
                          recluster_vimp_table))
          })

#### recluster_vimp_table (vimpTable) ------------------------------------------
setMethod("recluster_vimp_table", signature(x="vimpTable"),
          function(x, ...){
            # Suppress NOTES due to non-standard evaluation in data.table
            score <- NULL
            
            # Check if the table has already been reclustered.
            if(.as_vimp_table_state(x@state) >= "reclustered") return(x)
            
            # Pre-process the variable importance table. Generally, this should
            # not be necessary. The idea is that one calls
            # update_vimp_table_to_reference prior to calling
            # recluster_vimp_table. This makes it possible to form clusters
            # according in the local context -- even if the same clusters did
            # not appear when computing variable importance.
            x <- preprocess_vimp_table(x, stop_at="declustered")
            
            # Update state.
            x@state <- "reclustered"
            
            # Skip if the vimp table is empty.
            if(is_empty(x)) return(x)
            
            # Skip if there is no cluster table to work with.
            if(is_empty(x@cluster_table)) return(x)
            
            # Merge variable importance table with reference cluster table.
            vimp_table <- merge(x=x@vimp_table[, mget(c("name", "score"))],
                                y=x@cluster_table,
                                by.x="name",
                                by.y="feature_name",
                                all.x=FALSE,
                                all.y=TRUE)
            
            # Compute mean score for all features in the same cluster.
            vimp_table <- vimp_table[, list("score"=mean(score, na.rm=TRUE)), by="cluster_name"]
            
            # Keep only relevant columns, and rename "cluster_name" to "name".
            vimp_table[, mget(c("score", "cluster_name"))]
            data.table::setnames(vimp_table, old="cluster_name", new="name")
            
            # Update the vimp_table attribute.
            x@vimp_table <- vimp_table
            
            return(x)
          })

#### recluster_vimp_table (NULL) -----------------------------------------------
setMethod("recluster_vimp_table", signature(x="NULL"),
          function(x, ...){
            return(x)
          })



#### rank_vimp_table (vimpTable) -----------------------------------------------
setMethod("rank_vimp_table", signature(x="vimpTable"),
          function(x, ...){
            # Suppress NOTES due to non-standard evaluation in data.table
            score <- NULL
            
            # Check if the table has already been ranked
            if(.as_vimp_table_state(x@state) >= "ranked") return(x)
            
            # Pre-process the variable importance table.
            x <- preprocess_vimp_table(x, stop_at="declustered")
            
            # Update state.
            x@state <- "declustered"
            
            # Skip if the vimp table is empty.
            if(is_empty(x)) return(x)
            
            # Make copy to avoid updating by reference.
            vimp_table <- data.table::copy(x@vimp_table)
            
            # Remove NA values.
            vimp_table <- vimp_table[is.finite(score)]
            
            # Compute dense ranks, with NA ranked as NA.
            if(x@invert){
              vimp_table[, "rank":=data.table::frank(-score, ties.method="dense", na.last="keep")]
              
            } else {
              vimp_table[, "rank":=data.table::frank(score, ties.method="dense", na.last="keep")]
            }
            
            # Update vimp_table attribute.
            x@vimp_table <- vimp_table
            
            return(x)
          })

#### rank_vimp_table (NULL) ----------------------------------------------------
setMethod("rank_vimp_table", signature(x="NULL"),
          function(x, ...){
            return(x)
          })



#### preprocess_vimp_table (vimpTable) -----------------------------------------
setMethod("preprocess_vimp_table", signature(x="vimpTable"),
          function(x, stop_at="ranked", ...){
            # Convert the state attained and the requested state to ordinals.
            state_attained <- .as_vimp_table_state(x@state)
            stop_at <- .as_vimp_table_state(stop_at)
            
            if(state_attained < "decoded" & stop_at >= "decoded"){
              # Transform the features.
              x <- decode_vimp_table(x, ...)
            }
            
            if(state_attained < "declustered" & stop_at >= "declustered"){
              # Normalise feature values.
              x <- decluster_vimp_table(x, ...)
            }
            
            if(state_attained < "ranked" & stop_at >= "ranked"){
              # Batch-normalise feature values
              x <- rank_vimp_table(x, ...)
            }
            
            return(x)
          })

#### preprocess_vimp_table (NULL) ----------------------------------------------
setMethod("preprocess_vimp_table", signature(x="NULL"),
          function(x, ...){
            return(x)
          })



#### remove_signature_features (vimpTable) -------------------------------------
setMethod("remove_signature_features", signature(x="vimpTable"),
          function(x, features=NULL){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            original_name <- name <- NULL
            
            # Check state.
            if(x@state != "initial"){
              ..error_reached_unreachable_code(paste0("remove_signature_features,vimpTable: can only remove signature ",
                                                      "features from a variable importance table in its initial state."))
            } 
            
            # If there are no signature features, we don't need to remove them.
            if(is_empty(features)) return(x)
            
            # Check if any of the signature features are encoded.
            if(!is.null(x@encoding_table)){
              
              # Find features that were encoded in the variable importance table.
              encoded_features <- features[features %in% x@encoding_table$original_name]
              
              # Find features that did not require encoding.
              non_encoded_features <- setdiff(features, encoded_features)
              
              # Find names of the encoded features as they appear in the
              # variable importance table.
              encoded_features <- x@encoding_table[original_name %in% encoded_features]$reference_name
              
              # Combine encoded and non-encoded feature names.
              features <- c(encoded_features, non_encoded_features)
            }
            
            # Keep only features in the variable importance table that are not
            # signature features.
            x@vimp_table <- x@vimp_table[!name %in% features]
            
            return(x)
          })

#### remove_signature_features (NULL) ------------------------------------------
setMethod("remove_signature_features", signature(x="NULL"),
          function(x, features=NULL){
            return(x)
          })



#### update_vimp_table_to_reference (list) -------------------------------------
setMethod("update_vimp_table_to_reference", signature(x="list"),
          function(x, reference_cluster_table=NULL, ...){
            
            # If the list is empty, return NULL instead.
            if(is_empty(x)) return(NULL)
            
            # Dispatch to method for single variable importance tables.
            return(lapply(x,
                          update_vimp_table_to_reference,
                          reference_cluster_table=reference_cluster_table))
          })

#### update_vimp_table_to_reference (vimpTable) --------------------------------
setMethod("update_vimp_table_to_reference", signature(x="vimpTable"),
          function(x, reference_cluster_table=NULL, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            score <- NULL
            
            # Force declustering.
            if(.as_vimp_table_state(x@state) == "reclustered") x@state <- "decoded"
            
            # Ensure that the vimp table is declustered.
            x <- preprocess_vimp_table(x, "declustered", ...)
            
            # Update the cluster table.
            x@cluster_table <- reference_cluster_table
            
            # Set the state to declustered, since ranking is removed..
            x@state <- "declustered"
            
            if(is_empty(x)) return(x)
            
            # Merge variable importance table with reference cluster table.
            vimp_table <- merge(x=x@vimp_table[, mget(c("name", "score"))],
                                y=reference_cluster_table,
                                by.x="name",
                                by.y="feature_name",
                                all.x=FALSE,
                                all.y=TRUE)
            
            # Compute mean score for all features in the same cluster.
            vimp_table[, "score":=mean(score, na.rm=TRUE), by="cluster_name"]
            
            # Select only those entries with a finite score (i.e. those where
            # the merge produces matches).
            vimp_table <- vimp_table[is.finite(score)]
            
            # Keep only relevant columns, and rename "feature_name" to "name".
            # Update the vimp_table attribute.
            x@vimp_table <- vimp_table[, mget(c("score", "name"))]
            
            return(x)
          })

#### update_vimp_table_to_reference (NULL) -------------------------------------
setMethod("update_vimp_table_to_reference", signature(x="NULL"),
          function(x, reference_cluster_table=NULL, ...){
            return(x)
          })


#### collect_vimp_table (list) -------------------------------------------------
setMethod("collect_vimp_table", signature(x="list"),
          function(x, run_table=NULL, ...){
            # Collect variable importance tables, potentially limiting to
            
            # Suppress NOTES due to non-standard evaluation in data.table
            data_id <- run_id <- NULL
            
            # Check that all variable importance tables are for the same
            # variable importance method. Check first which variable importance
            # methods are present.
            vimp_method <- unique(sapply(x, function(x) x@vimp_method))
            
            # Dispatch to separate lists.
            if(length(vimp_method) > 1){
              
              # Initialise list to store variable importance tables.
              vimp_table_list <- list()
              for(current_vimp_method in vimp_method){
                # Identify elements which use the same variable importance
                # method.
                same_vimp_method <- sapply(x, function(x, vimp_method) x@vimp_method == current_vimp_method)
                
                # Store all elements with the same variable importance method as
                # an element in the vimp_table_list list.
                vimp_table_list[[current_vimp_method]] <- list(x[same_vimp_method])
              }
              
              # Dispatch collections of the lists.
              return(lapply(vimp_table_list, aggregate_vimp_table, run_table=run_table, ...))
            }
            
            # Select only elements that share the same dataset.
            if(!is.null(run_table)){
              
              # Try and identify matching data and run ids between vimpTable
              # object and the run_table. Start at the bottom and try to match.
              for(ii in rev(run_table$perturb_level)){
                # Get a list with identifiers.
                run_id_list <- .get_iteration_identifiers(run=list("run_table"=run_table),
                                                          perturb_level=ii)
                
                # Check whether there are any matching data and run ids by
                # checking if matching creates a non-empty table.
                matching_elements <- sapply(x, function(x, run_id_list){
                  if(is_empty(x)) return(FALSE)
                  
                  return(!is_empty(x@run_table[data_id==run_id_list$data & run_id==run_id_list$run]))
                },
                run_id_list=run_id_list)
                
                # If there is any match, we break from the loop
                if(any(matching_elements)) break()
              }
              
              # Update matching_elements with entries where the run_table has
              # not been specified.
              matching_elements <- matching_elements | sapply(x, function(x) is_empty(x@run_table))
              
              # Shrink x to matching elements.
              x <- x[matching_elements]
            }
            
            # Check that x is not empty.
            if(is_empty(x)) return(NULL)
            
            return(x)
          })

#### collect_vimp_table (vimpTable) --------------------------------------------
setMethod("collect_vimp_table", signature(x="vimpTable"),
          function(x, run_table=NULL, ...){
            
            # Parse to list.
            return(collect_vimp_table(x=list(x),
                                      run_table=run_table,
                                      ...))
          })

#### collect_vimp_table (NULL) -------------------------------------------------
setMethod("collect_vimp_table", signature(x="NULL"),
          function(x, run_table=NULL, ...){
            return(x)
          })



#'@title Aggregate variable importance from multiple variable importance
#'  objects.
#'
#'@description This methods aggregates variable importance from one or more
#'  `vimpTable` objects.
#'
#'@param x Variable importance (`vimpTable`) object, a list thereof, or one or
#'  more paths to these objects.
#'@param aggregation_method Method used to aggregate variable importance. The
#'  available methods are described in the *feature selection methods* vignette.
#'@param rank_threshold Rank threshold used within several aggregation methods.
#'  See the *feature selection methods* vignette for more details.
#'@param ... unused parameters.
#'
#'@return A `vimpTable` object with aggregated variable importance data.
#'@exportMethod aggregate_vimp_table
#'
#'@md
#'@rdname aggregate_vimp_table-methods
setGeneric("aggregate_vimp_table",
           function(x,
                    aggregation_method,
                    rank_threshold=NULL,
                    ...) standardGeneric("aggregate_vimp_table"))



#### aggregate_vimp_table (list) -----------------------------------------------

#'@rdname aggregate_vimp_table-methods
setMethod("aggregate_vimp_table", signature(x="list"),
          function(x, aggregation_method, rank_threshold=NULL, ...){
            
            # Check that the list itself is not empty.
            if(is_empty(x)) return(NULL)
            
            # Check that any character elements are interpreted as paths, and
            # are loaded.
            x <- lapply(x,
                        function(x){
                          if(is.character(x)){
                            # Attempt to read from file.
                            x <- readRDS(x)
                            
                            # Prior to version 1.2.0, files would contain a list
                            # of data.
                            if(is.list(x)){
                              x <- as_vimp_table_object(x,
                                                        project_id="")
                            }
                            
                            # Update object definitions.
                            x <- update_object(object=x)
                          }
                          
                          return(x)
                        })
            
            # Check that the list contents are not empty.
            empty_elements <- sapply(x, is_empty)
            if(all(empty_elements)) return(NULL)
            
            # Keep only non-empty elements.
            x <- x[!empty_elements]
            
            # Preprocess individual elements prior to merging.
            x <- lapply(x, preprocess_vimp_table, ...)
            
            # Add in unique run_id, and export the variable importance
            # themselves.
            vimp_table <- mapply(function(x, ii){
              # Prevent updating by reference.
              vimp_table <- data.table::copy(x@vimp_table)
              
              # Add run identifier.
              vimp_table[, "run_id":=ii]
              
              return(vimp_table)
            },
            x=x,
            ii=seq_along(x),
            SIMPLIFY=FALSE)
            
            # Combine all variable importance tables into a single table.
            vimp_table <- data.table::rbindlist(vimp_table, use.names=TRUE)
            
            # Create an aggregated variable importance table.
            aggregated_vimp_object <- methods::new("vimpTable", x[[1]])
            
            # Attach the aggregated variable importance table.
            aggregated_vimp_object@vimp_table <- vimp_table
            
            if(aggregation_method == "none"){
              aggregated_vimp_object <- .compute_rank_mean_score(x=aggregated_vimp_object)
              
            } else if(aggregation_method %in% c("mean", "median", "best", "worst")){
              # Perform aggregation using simple ensemble methods
              
              if(aggregation_method == "mean"){
                aggregated_vimp_object <- .compute_rank_mean_rank(x=aggregated_vimp_object)
                
              } else if(aggregation_method == "median"){
                aggregated_vimp_object <- .compute_rank_median_rank(x=aggregated_vimp_object)
                
              } else if(aggregation_method == "best"){
                aggregated_vimp_object <- .compute_rank_best_rank(x=aggregated_vimp_object)
                
              } else if(aggregation_method == "worst"){
                aggregated_vimp_object <- .compute_rank_worst_rank(x=aggregated_vimp_object)
              }
              
            } else if(aggregation_method %in% c("stability", "exponential")){
              # Perform aggregation using occurence-based methods.
              if(is.null(rank_threshold)) {
                rank_threshold <- rank.optimise_occurrence_threshold(vimp_table=aggregated_vimp_object)
              }
              
              if(aggregation_method == "stability"){
                aggregated_vimp_object <- .compute_rank_stability(x=aggregated_vimp_object,
                                                                  rank_threshold=rank_threshold)
                
              } else if(aggregation_method == "exponential"){
                aggregated_vimp_object <- .compute_rank_exponential(x=aggregated_vimp_object,
                                                                    rank_threshold=rank_threshold)
              } 
              
            } else if(aggregation_method %in% c("borda", "enhanced_borda", "truncated_borda", "enhanced_truncated_borda")){
              # Perform aggregation using Borda-count based methods
              if(is.null(rank_threshold)) {
                rank_threshold <- rank.optimise_occurrence_threshold(vimp_table=aggregated_vimp_object)
              }
              
              if(aggregation_method == "borda") {
                aggregated_vimp_object <- .compute_rank_borda(x=aggregated_vimp_object,
                                                              rank_threshold=rank_threshold,
                                                              truncated=FALSE,
                                                              enhanced=FALSE)
                
              } else if(aggregation_method == "enhanced_borda") {
                aggregated_vimp_object <- .compute_rank_borda(x=aggregated_vimp_object,
                                                              rank_threshold=rank_threshold,
                                                              truncated=FALSE,
                                                              enhanced=TRUE)
                
              } else if(aggregation_method == "truncated_borda") {
                aggregated_vimp_object <- .compute_rank_borda(x=aggregated_vimp_object,
                                                              rank_threshold=rank_threshold,
                                                              truncated=TRUE,
                                                              enhanced=FALSE)
                
              } else if(aggregation_method == "enhanced_truncated_borda") {
                aggregated_vimp_object <- .compute_rank_borda(x=aggregated_vimp_object,
                                                              rank_threshold=rank_threshold,
                                                              truncated=TRUE,
                                                              enhanced=TRUE)
              }
              
            } else {
              ..error_reached_unreachable_code(paste0("aggregate_vimp_table,list: encountered an unknown aggregation method:", aggregation_method))
            }
            
            # Force ranking of the aggregated variable importance object.
            aggregated_vimp_object@state <- "declustered"
            
            # Rank aggregated scores.
            aggregated_vimp_object <- rank_vimp_table(aggregated_vimp_object)
            
            # Update state again to reflect the current state.
            aggregated_vimp_object@state <- "aggregated"
            
            return(aggregated_vimp_object)
          })

#### aggregate_vimp_table (character) ------------------------------------------

#'@rdname aggregate_vimp_table-methods
setMethod("aggregate_vimp_table", signature(x="character"),
          function(x, aggregation_method, rank_threshold=NULL, ...){
            
            # Force x to a list, and pass to main method.
            return(aggregate_vimp_table(x=as.list(x),
                                        aggregation_method=aggregation_method,
                                        rank_threshold=rank_threshold,
                                        ...))
          })


#### aggregate_vimp_table (vimpTable) ------------------------------------------

#'@rdname aggregate_vimp_table-methods
setMethod("aggregate_vimp_table", signature(x="vimpTable"),
          function(x, aggregation_method, rank_threshold=NULL, ...){
            # Check if the variable importance table object already has been
            # aggregated.
            if(.as_vimp_table_state(x@state) == "aggregated") return(x)
            
            return(aggregate_vimp_table(x=list(x),
                                        aggregation_method=aggregation_method,
                                        rank_threshold=rank_threshold,
                                        ...))
          })

#### aggregate_vimp_table (NULL) -----------------------------------------------

#'@rdname aggregate_vimp_table-methods
setMethod("aggregate_vimp_table", signature(x="NULL"),
          function(x, aggregation_method, rank_threshold=NULL,...){
            return(NULL)
          })


#### aggregate_vimp_table (experimentData) -------------------------------------

#'@rdname aggregate_vimp_table-methods
setMethod("aggregate_vimp_table", signature(x="experimentData"),
          function(x, aggregation_method, rank_threshold=NULL,...){
            # Check if the attribute has been set.
            if(is.null(x@vimp_table_list)){
              warning("No variable importance tables are present.")
              
              return(NULL)
            }
            
            # Iterate over data for the different variable importance methods.
            return(lapply(x@vimp_table_list,
                          aggregate_vimp_table,
                          aggregation_method=aggregation_method,
                          rank_threshold=rank_threshold,
                          ...))
          })


#### add_package_version (vimpTable) -------------------------------------------
setMethod("add_package_version", signature(object="vimpTable"),
          function(object){
            
            # Set version of familiar
            return(.add_package_version(object=object))
          })



.as_vimp_table_state <- function(x){
  
  if(is(x, "vimpTable")) x <- x@state
  
  # Available states
  state_levels <- c("initial",
                    "decoded",
                    "declustered",
                    "reclustered",
                    "ranked",
                    "aggregated")
  
  if(!all(x %in% state_levels)){
    ..error_reached_unreachable_code(".as_vimp_table_state: one or more of x could not be matched to vimp table states.")
  }
  
  return(factor(x=x,
                levels=state_levels,
                ordered=TRUE))
}



as_vimp_table_object <- function(x,
                                 project_id){
  
  vimp_table <- x$vimp
  if(!is_empty(vimp_table)){
    # Determine if an increasing score leads to an increasing rank.
    invert <- stats::cor(vimp_table$score,
                         vimp_table$rank,
                         method="spearman") < 0.0
    
    # Select only expected columns.
    vimp_table <- vimp_table[, mget(c("score", "name"))]
    
  } else {
    invert <- FALSE
  }
  
  # Create variable importance object.
  vimp_table <- methods::new("vimpTable",
                             vimp_table = vimp_table,
                             vimp_method = x$fs_method,
                             run_table = x$run_table,
                             score_aggregation = "max",
                             encoding_table = NULL,
                             cluster_table = "declustered",
                             invert = invert,
                             project_id = project_id,
                             familiar_version = "1.2.0",
                             state="initial")
  
  return(vimp_table)
}



prepare_vimp_table_object <- function(vimp_method,
                                      vimp_table=NULL,
                                      run_table=NULL,
                                      score_aggregation="max",
                                      encoding_table=NULL,
                                      cluster_table=NULL,
                                      invert,
                                      state){
  # This function helps prepare a vimpTable object for unit testing. It is not
  # used as part of the main workflow.
  
  proto_vimp_table <- methods::new("vimpTable",
                                   vimp_table = vimp_table,
                                   vimp_method = vimp_method,
                                   run_table = run_table,
                                   score_aggregation = score_aggregation,
                                   encoding_table = encoding_table,
                                   cluster_table = cluster_table,
                                   invert = invert,
                                   state=state)
  
  # Add package version.
  proto_vimp_table <- add_package_version(proto_vimp_table)
  
  return(proto_vimp_table)
}

#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R



#### show (vimpTable) ----------------------------------------------------------
setMethod("show", signature(object="vimpTable"),
          function(object){
            # Suppress NOTES due to non-standard evaluation in data.table
            score <- NULL
            
            # Make sure the collection object is updated.
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



#### get_vimp_table (vimpTable) ------------------------------------------------
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
setMethod("get_vimp_table", signature(x="NULL"),
          function(x, state="ranked", ...){
            return(NULL)
          })

#### get_vimp_table (familiarModel) --------------------------------------------
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



#### aggregate_vimp_table (list) -----------------------------------------------
setMethod("aggregate_vimp_table", signature(x="list"),
          function(x, aggregation_method, rank_threshold=NULL, ...){
            
            # Check that the list itself is not empty.
            if(is_empty(x)) return(NULL)
            
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
            ii=seq_along(x))
            
            # Combine all variable importance tables into a single table.
            vimp_table <- data.table::rbindlist(vimp_table, use.names=TRUE)
            
            # Make a local copy of vimp_table
            vimp_table <- data.table::copy(vimp_table)
            
            if(aggregation_method == "none"){
              rank_table <- rank.none(vimp_table=vimp_table)
              
            } else if(aggregation_method %in% c("mean", "median", "best", "worst")){
              # Perform aggregation using simple ensemble methods
              
              if(aggregation_method == "mean"){
                rank_table <- rank.mean(vimp_table=vimp_table)
                
              } else if(aggregation_method == "median"){
                rank_table <- rank.median(vimp_table=vimp_table)
                
              } else if(aggregation_method == "best"){
                rank_table <- rank.best_rank(vimp_table=vimp_table)
                
              } else if(aggregation_method == "worst"){
                rank_table <- rank.worst_rank(vimp_table=vimp_table)
              }
              
            } else if(aggregation_method %in% c("stability", "exponential")){
              # Perform aggregation using occurence-based methods.
              if(is.null(rank_threshold)) {
                rank_threshold <- rank.optimise_occurrence_threshold(vimp_table=vimp_table)
              }
              
              if(aggregation_method == "stability"){
                rank_table <- rank.stability(vimp_table=vimp_table,
                                             rank_threshold=rank_threshold)
                
              } else if(aggregation_method == "exponential"){
                rank_table <- rank.exponential(vimp_table=vimp_table,
                                               rank_threshold=rank_threshold)
              } 
              
            } else if(aggregation_method %in% c("borda", "enhanced_borda", "truncated_borda", "enhanced_truncated_borda")){
              # Perform aggregation using Borda-count based methods
              if(is.null(rank_threshold)) {
                rank_threshold <- rank.optimise_occurrence_threshold(vimp_table=vimp_table)
              }
              
              if(aggregation_method == "borda") {
                rank_table <- rank.borda_aggregation(vimp_table=vimp_table,
                                                     rank_threshold=rank_threshold,
                                                     truncated=FALSE,
                                                     enhanced=FALSE)
                
              } else if(aggregation_method == "enhanced_borda") {
                rank_table <- rank.borda_aggregation(vimp_table=vimp_table,
                                                     rank_threshold=rank_threshold,
                                                     truncated=FALSE,
                                                     enhanced=TRUE)
                
              } else if(aggregation_method == "truncated_borda") {
                rank_table <- rank.borda_aggregation(vimp_table=vimp_table,
                                                     rank_threshold=rank_threshold,
                                                     truncated=TRUE,
                                                     enhanced=FALSE)
                
              } else if(aggregation_method == "enhanced_truncated_borda") {
                rank_table <- rank.borda_aggregation(vimp_table=vimp_table,
                                                     rank_threshold=rank_threshold,
                                                     truncated=TRUE,
                                                     enhanced=TRUE)
                
              } else {
                ..error_reached_unreachable_code(paste0("aggregate_vimp_table,list: encountered an unknown aggregation method:", aggregation_method))
              }
            }
            
            # Create an aggregated variable importance table.
            x <- methods::new("vimpTable", x[[1]])
            
            # Update the vimp_table attribute and update the state attribute.
            x@vimp_table <- rank_table
            x@state <- "aggregated"
            
            return(x)
          })

#### aggregate_vimp_table (vimpTable) ------------------------------------------
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
setMethod("aggregate_vimp_table", signature(x="NULL"),
          function(x, aggregation_method, rank_threshold=NULL,...){
            return(NULL)
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
    invert <- cor(vimp_table$score,
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

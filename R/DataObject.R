#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#####create_data_object#####
setMethod("create_data_object", signature(object="familiarModel", data="ANY"),
          function(object, data, is_pre_processed=FALSE){
            # Creates a data object
            
            # Skip checks if the data already is a data objects
            if(any(class(data) == "dataObject")){
              return(data)
            }
            
            # TODO check input data consistency, i.e. column naming etc.
            new_data <- methods::new("dataObject", data=data, is_pre_processed=is_pre_processed, outcome_type=object@outcome_type)
            
            return(new_data)
            
          })


#####load_delayed_data (model)#####
setMethod("load_delayed_data", signature(data="dataObject", object="familiarModel"),
          function(data, object, stop_at){
            # Loads data from internal memory

            # Check if loading was actually delayed
            if(!data@delay_loading){
              return(data)
            }
            
            # Read project list and settings
            iter_list <- get_project_list()$iter_list
            settings  <- get_settings()
            
            # Read required features
            req_feature_cols <- object@req_feature_cols
            
            # Get columns in data frame which are not features, but identifiers and outcome instead
            non_feature_cols <- get_non_feature_columns(x=object)
            
            # Get subject ids
            run_id_list      <- getIterID(run=list("run_table"=object@run_table), perturb_level=data@perturb_level)
            run_subj_id      <- getSubjectIDs(iter_list=iter_list, data_id=run_id_list$data, run_id=run_id_list$run,
                                              train_or_validate=ifelse(data@load_validation, "valid", "train"))
            
            # Check subjects and select unique subjects
            if(!is.null(run_subj_id)){
              uniq_subj_id   <- unique(run_subj_id)
            } else {
              # Return an updated data object, but without data
              return(methods::new("dataObject", data=NULL, is_pre_processed=FALSE, outcome_type=data@outcome_type, aggregate_on_load=data@aggregate_on_load))
            }
            
            # Prepare a new data object
            new_data <- methods::new("dataObject",
                                     data = getDataFromBackend(subj_id=uniq_subj_id, col_names=c(non_feature_cols, req_feature_cols), settings=settings),
                                     is_pre_processed = FALSE,
                                     outcome_type = data@outcome_type,
                                     delay_loading = FALSE,
                                     perturb_level = NA_integer_,
                                     load_validation = data@load_validation,
                                     aggregate_on_load = data@aggregate_on_load,
                                     sample_set_on_load = data@sample_set_on_load)
            
            # Preprocess data
            new_data <- preprocess_data(data=new_data, object=object, stop_at=stop_at)
            
            # Recreate iteration
            new_data <- select_data_from_samples(data=new_data, samples=run_subj_id)
            
            # Aggregate data if required
            if(new_data@aggregate_on_load){
              
              # Aggregate
              new_data <- aggregate_data(data=new_data)
              
              # Reset flag to FALSE, as data has been loaded
              new_data@aggregate_on_load <- FALSE
            }
            
            return(new_data)
            
          })


#####preprocess_data (model)#####
setMethod("preprocess_data", signature(data="dataObject", object="familiarModel"),
          function(data, object, stop_at){
            
            # Check whether pre-processing is required
            if(data@is_pre_processed) {
              return(data)
            }
            
            # Set pre-processing flag to TRUE
            data@is_pre_processed <- TRUE
            
            # Select only required features
            data      <- apply_signature(data_obj=data, selected_feat=object@req_feature_cols)
            
            # Transform features
            data      <- transform_features(data=data, feature_info_list=object@feature_info)
            
            # Normalise feature values
            data      <- normalise_features(data=data, feature_info_list=object@feature_info)
            
            # Batch-normalise feature values
            data      <- batch_normalise_features(data=data, feature_info_list=object@feature_info)
            
            # Impute missing values
            data      <- impute_features(data=data, feature_info_list=object@feature_info)
            
            # Cluster features
            data      <- cluster_features(data=data, feature_info_list=object@feature_info)
            
            # Select only the signature (if present)
            if(!is.null(object@signature)){
              data    <- apply_signature(data_obj=data, selected_feat=object@signature)
            }
            
            return(data)
          })


#####load_delayed_data (ensemble)#####
setMethod("load_delayed_data", signature(data="dataObject", object="familiarEnsemble"),
          function(data, object, stop_at="clustering"){
            # Loads data from internal memory -- for familiarEnsemble objects
            
            # Suppress NOTES due to non-standard evaluation in data.table
            perturb_level <- NULL
            
            # Check if loading was actually delayed
            if(!data@delay_loading){
              return(data)
            }
            
            # Read project list and settings
            iter_list <- get_project_list()$iter_list
            settings  <- get_settings()
            
            # Read required features
            req_feature_cols <- object@req_feature_cols
            
            # Get columns in data frame which are not features, but identifiers and outcome instead
            non_feature_cols <- get_non_feature_columns(x=object)
            
            # Join run tables to identify the runs that should be evaluated.
            combined_run_table <- lapply(object@run_table$run_table, function(model_run_table, data_perturb_level){
              return(model_run_table[perturb_level == data_perturb_level])
            }, data_perturb_level=data@perturb_level)
            
            # Merge to single table
            combined_run_table <- data.table::rbindlist(combined_run_table)
            
            # Remove duplicate rows
            combined_run_table <- unique(combined_run_table)
            
            # Check length and extract sample ids.
            if(nrow(combined_run_table) == 1){
              run_subj_id <- getSubjectIDs(iter_list=iter_list,
                                           data_id=combined_run_table$data_id,
                                           run_id=combined_run_table$run_id,
                                           train_or_validate=ifelse(data@load_validation, "valid", "train"))
              
            } else {
              # Extract all subject ids. This happens if the the data is pooled.
              run_subj_id <- unlist(lapply(seq_len(nrow(combined_run_table)), function(ii, run_table, iter_list, train_or_validate){
                return(getSubjectIDs(iter_list=iter_list,
                                     data_id=run_table$data_id[ii],
                                     run_id=run_table$run_id[ii],
                                     train_or_validate=train_or_validate))
              }, run_table=combined_run_table, iter_list=iter_list, train_or_validate=ifelse(data@load_validation, "valid", "train")))
              
              # Select only unique subject ids.
              run_subj_id <- unique(run_subj_id)
            }
            
            # Check subjects and select unique subjects
            if(!is.null(run_subj_id)){
              uniq_subj_id   <- unique(run_subj_id)
              
            } else {
              # Return an updated data object, but without data
              return(methods::new("dataObject",
                                  data = NULL,
                                  is_pre_processed = FALSE,
                                  outcome_type = data@outcome_type,
                                  aggregate_on_load = data@aggregate_on_load))
            }
            
            # Prepare a new data object
            new_data <- methods::new("dataObject",
                                     data = getDataFromBackend(subj_id=uniq_subj_id,
                                                               col_names=c(non_feature_cols, req_feature_cols),
                                                               settings=settings),
                                     is_pre_processed = FALSE,
                                     outcome_type = data@outcome_type,
                                     delay_loading = FALSE,
                                     perturb_level = NA_integer_,
                                     load_validation = data@load_validation,
                                     aggregate_on_load = data@aggregate_on_load,
                                     sample_set_on_load = data@sample_set_on_load)
            
            # Preprocess data
            new_data <- preprocess_data(data=new_data, object=object, stop_at=stop_at)
            
            # Recreate iteration
            new_data <- select_data_from_samples(data=new_data, samples=run_subj_id)
            
            # Aggregate data if required
            if(new_data@aggregate_on_load){
              
              # Aggregate
              new_data <- aggregate_data(data=new_data)
              
              # Reset flag to FALSE, as data has been loaded
              new_data@aggregate_on_load <- FALSE
            }
            
            return(new_data)
            
          })


#####preprocess_data (ensemble)#####
setMethod("preprocess_data", signature(data="dataObject", object="familiarEnsemble"),
          function(data, object, stop_at="clustering"){
            # Preprocessing for data used by familiarEnsemble objects
            
            # Check whether pre-processing is required
            if(data@is_pre_processed) {
              return(data)
            }
            
            if(!stop_at %in% c("signature", "transformation", "normalisation", "batch_normalisation", "imputation", "clustering")){
              stop(paste0("Data pre-processing can only be stopped after \"signature\", \"transformation\", \"normalisation\", \"imputation\", \"clustering\". \"",
                          stop_at, "\" is not a valid option."))
            }
            
            # Set pre-processing flag to TRUE
            data@is_pre_processed <- TRUE
            
            # Select only required features
            data      <- apply_signature(data_obj=data, selected_feat=object@req_feature_cols)
            
            if(stop_at == "signature"){
              return(data)
            }
            
            # Transform features
            data      <- transform_features(data=data, feature_info_list=object@feature_info)
            
            if(stop_at == "transformation"){
              return(data)
            }
            
            # Normalise feature values
            data      <- normalise_features(data=data, feature_info_list=object@feature_info)
            
            if(stop_at == "normalisation"){
              return(data)
            }
            
            # Batch-normalise feature values
            data      <- batch_normalise_features(data=data, feature_info_list=object@feature_info)
            
            if(stop_at == "batch_normalisation"){
              return(data)
            }
            
            # Impute missing values
            data      <- impute_features(data=data, feature_info_list=object@feature_info)
            
            if(stop_at == "imputation"){
              return(data)
            }
            
            # Cluster features
            data <- cluster_features(data=data, feature_info_list=object@feature_info)
            
            return(data)
          })


#####select_data_from_samples#####
setMethod("select_data_from_samples", signature(data="dataObject", samples="ANY"),
          function(data, samples=NULL){

            # Check if data is loaded
            if(data@delay_loading){

              # Store samples until the data is loaded.
              data@sample_set_on_load <- samples
              
            } else {
              
              if(is.null(samples) & is.null(data@sample_set_on_load)) {
                # Return an empty data set if no samples are provided
                data@data <- head(data@data, n=0)
                
              } else if(is.null(samples) & !is.null(data@sample_set_on_load)) {
                # Use samples in the sample_set_on_load attribute.
                dt_iter   <- data.table::data.table("subject_id"=data@sample_set_on_load)
                data@data <- merge(dt_iter, data@data, by="subject_id", all.x=FALSE, all.y=FALSE)
                
              } else if(!is.null(samples) & is.null(data@sample_set_on_load)) {
                # Use samples from the samples function argument. allow.cartesian is set to true to allow
                # use with repeated measurements.
                dt_iter   <- data.table::data.table("subject_id"=samples)
                data@data <- merge(dt_iter, data@data, by="subject_id", all.x=FALSE, all.y=FALSE,
                                   allow.cartesian=TRUE)
                
              } else {
                # Use samples that appear both as function argument and within the sample_set_on_load attribute.
                # The sample_set_on_load attribute is used as a filter. allow.cartesian is set to true to allow
                # use with repeated measurements.
                samples <- samples[samples %in% data@sample_set_on_load]
                dt_iter <- data.table::data.table("subject_id"=samples)
                data@data <- merge(dt_iter, data@data, by="subject_id", all.x=FALSE, all.y=FALSE,
                                   allow.cartesian=TRUE)
                
              }
            }
            
            return(data)
          })


#####get_unique_samples#####
setMethod("get_unique_samples", signature(data="dataObject"),
          function(data){

            return(unique(data@data$subject_id))
          })


#####aggregate_data#####
setMethod("aggregate_data", signature(data="dataObject"),
          function(data){
            
            # Check if loading of the data object was delayed
            if(data@delay_loading){
              # Mark for future aggregation after loading the data
              data@aggregate_on_load <- TRUE
              return(data)
              
            } else {
              # Set aggregation flag to FALSE and continue
              data@aggregate_on_load <- FALSE
            }
            
            # Check if the data is empty
            if(is_empty(data)){
              return(data)
            }
            
            # Identify the columns containing outcome and subject and cohort identifiers
            id_cols <- get_non_feature_columns(x=data, include_repetition_id=FALSE)
            
            # Determine the number of different entries
            
            if(all(data@data$repetition_id==1)){
              # No duplicates present
              return(data)
              
            } else {
              # Repeated measurements or bootstraps.
              
              # Identify feature columns
              feature_columns <- get_feature_columns(x=data)

              # Identify class of features
              column_class <- lapply(feature_columns, function(ii, data) (class(data[[ii]])), data=data@data)
              
              # Determine numerical features
              feat_numeric <- sapply(column_class, function(selected_column_class) (any(selected_column_class %in% c("numeric", "integer"))))
              feat_numeric <- feature_columns[feat_numeric]
              
              # Determine categorical features
              feat_factor <- sapply(column_class, function(selected_column_class) (any(selected_column_class %in% c("logical", "character", "factor"))))
              feat_factor <- feature_columns[feat_factor]
              
              # Find non-duplicate id columns
              dt_out       <- unique(data@data[, id_cols, with=FALSE])
              
              # Add aggregated numeric columns
              if(length(feat_numeric) > 0){
                dt_num     <- data@data[, lapply(.SD, stats::median), by=id_cols, .SDcols=feat_numeric]
                dt_out     <- merge(dt_out, dt_num, by=id_cols)
              }
              
              # Add aggregated factor columns
              if(length(feat_factor) > 0){
                dt_fac     <- data@data[, lapply(.SD, get_mode), by=id_cols, .SDcols=feat_factor]
                dt_out     <- merge(dt_out, dt_fac, by=id_cols)
              }
              
              # Add in repetition_id column again
              dt_out[, "repetition_id":=-1]
              
              # Reorder columns so that it matches the input
              data.table::setcolorder(dt_out, neworder=colnames(data@data))
              
              data@data <- dt_out
              
              return(data)
            }
          })


#####filter_features#####
setMethod("filter_features", signature(data="dataObject"),
          function(data, remove_features=NULL, available_features=NULL){
            # Removes features from a data set

            # If both are provided, use remove_features
            if(!is.null(remove_features) & !is.null(available_features)){
              available_features <- NULL
            }
            
            # Do not do anything if both are null
            if(is.null(remove_features) & is.null(available_features)){
              return(data)
              
            # Based on remove_features input
            } else if(!is.null(remove_features)){
              
              # Determine which remove_features are actually present in data
              remove_features <- intersect(remove_features, get_feature_columns(x=data))
              
            # Based on available_features input
            } else if(!is.null(available_features)){
              
              # Skip if length equals 0
              if(length(available_features) == 0){
                return(data)
              }
              
              # Determine which features should be removed
              remove_features <- setdiff(get_feature_columns(x=data), available_features)
              
              # Only keep features that are in data.
              available_features <- intersect(get_feature_columns(x=data), available_features)
              
            } else {
              stop("This point should never be reachable. Check for inconsistencies if it does.")
            }
            
            # Remove features from data if there is 1 or more feature to remove
            if(length(remove_features) > 0){
              data@data[, (remove_features):=NULL]
            }
            
            # Make sure that the column order is the same as available_features
            if(!is.null(available_features)){
              data.table::setcolorder(data@data, c(get_non_feature_columns(x=data), available_features))
            }
            
            return(data)
          })


#####filter_missing_outcome#####
setMethod("filter_missing_outcome", signature(data="dataObject"),
          function(data, is_validation=FALSE){
            
            # Check if data is empty
            if(is_empty(data)){
              return(data)
            }
            
            # Change behaviour by outcome_type
            if(data@outcome_type == "survival"){
              outcome_is_valid <- is_valid_data(data@data[["outcome_time"]]) & is_valid_data(data@data[["outcome_event"]])
            } else if(data@outcome_type %in% c("binomial", "multinomial", "continuous", "count")) {
              outcome_is_valid <- is_valid_data(data@data[["outcome"]])
            } else {
              stop(paste0("Implementation for outcome_type ", data@outcome_type, " is missing."))
            }
            
            if(is_validation){
              # Check whether all outcome information is missing for validation. It may be a prospective study.
              # In that case, keep all data.
              if(all(!outcome_is_valid)){
                outcome_is_valid <- !outcome_is_valid
              }
            }
            
            # Keep only data for which the outcome exists
            data@data <- data@data[(outcome_is_valid), ]
            
            return(data)
          })


#####filter_bad_samples#####
setMethod("filter_bad_samples", signature(data="dataObject"),
          function(data, threshold){
            
            # Check if data is empty
            if(is_empty(data)){
              return(data)
            }
            
            # Find the columns containing features
            feature_columns <- get_feature_columns(x=data)
            
            # Still very fast, but is much friendlier to our poor memory.
            n_missing <- numeric(nrow(data@data))
            for(feature in feature_columns){
              n_missing <- n_missing + !is_valid_data(data@data[[feature]])
            }
            
            # Set threshold number, as threshold is a fraction.
            threshold_number <- length(feature_columns) * threshold

            # Only keep samples with a number of missing values below the threshold
            data@data <- data@data[(n_missing <= threshold_number)]
            
            return(data)
          })


#####transform_features (dataObject)#####
setMethod("transform_features", signature(data="dataObject"),
          function(data, feature_info_list, invert=FALSE){
            
            # Check if data is empty
            if(is_empty(data)){
              return(data)
            }
            
            # Find the columns containing features
            feature_columns <- get_feature_columns(x=data)
            
            # Transform features
            data@data <- transform_features(data=data@data,
                                            feature_info_list=feature_info_list,
                                            features=feature_columns,
                                            invert=invert)
            
            return(data)
          })


#####transform_features (data.table)#####
setMethod("transform_features", signature(data="data.table"),
          function(data, feature_info_list, features, invert=FALSE){
            
            # Check if data is empty
            if(is_empty(data)){
              return(data)
            }
            
            # Apply transformations
            transformed_list <- lapply(features, function(ii, data, feature_info_list, invert){
              
              x <- transformation.apply_transform(x=data[[ii]],
                                                  trans_param=feature_info_list[[ii]]@transformation_parameters,
                                                  invert=invert)
              
              return(x)
            }, data=data, feature_info_list=feature_info_list, invert=invert)
            
            # Update name of data in columns
            names(transformed_list) <- features
            
            # Update with replacement in the data object
            data <- update_with_replacement(data=data, replacement_list=transformed_list)
            
            return(data)
          })


#####normalise_features (dataObject)#####
setMethod("normalise_features", signature(data="dataObject"),
          function(data, feature_info_list, invert=FALSE){
            
            # Check if data is empty
            if(is_empty(data)){
              return(data)
            }
            
            # Find the columns containing features
            feature_columns <- get_feature_columns(x=data)
            
            # Apply normalisation
            data@data <- normalise_features(data=data@data,
                                            feature_info_list=feature_info_list,
                                            features=feature_columns,
                                            invert=invert)
            
            return(data)
          })


#####normalise_features (data.table)#####
setMethod("normalise_features", signature(data="data.table"),
          function(data, feature_info_list, features, invert=FALSE){
            
            # Check if data is empty
            if(is_empty(data)){
              return(data)
            }
            
            # Apply normalisation
            normalised_list <- lapply(features, function(ii, data, feature_info_list, invert){
              
              x <- normalise.apply_normalisation(x=data[[ii]],
                                                 norm_param=feature_info_list[[ii]]@normalisation_parameters,
                                                 invert=invert)
              
              return(x)
            }, data=data, feature_info_list=feature_info_list, invert=invert)
            
            # Update name of data in columns
            names(normalised_list) <- features
            
            # Update with replacement in the data object
            data <- update_with_replacement(data=data, replacement_list=normalised_list)
            
            return(data)
          })


#####batch_normalise_features#########
setMethod("batch_normalise_features", signature(data="dataObject"),
          function(data, feature_info_list, cl=NULL){
            
            # Check if data is empty
            if(is_empty(data)){
              return(data)
            }
            
            # Find the columns containing features
            feature_columns <- get_feature_columns(x=data)
            
            # Update feature_info_list by adding info for missing batches
            feature_info_list <- add_batch_normalisation_parameters(feature_info_list=feature_info_list,
                                                                    data_obj=data)
            
            # Apply batch-normalisation
            batch_normalised_list <- lapply(feature_columns, function(ii, data, feature_info_list){
              
              x <- batch_normalise.apply_normalisation(x=data@data[, c(ii, "subject_id", "cohort_id", "repetition_id"), with=FALSE],
                                                       feature_info=feature_info_list[[ii]])
              
              return(x)
            }, data=data, feature_info_list=feature_info_list)
            
            # Update name of data in columns
            names(batch_normalised_list) <- feature_columns
            
            # Update with replacement in the data object
            data <- update_with_replacement(data=data, replacement_list=batch_normalised_list)
            
            return(data)
          })



#####impute_features#####
setMethod("impute_features", signature(data="dataObject"),
          function(data, feature_info_list, cl=NULL){
            
            # Check if data is empty
            if(is_empty(data)){
              return(data)
            }
            
            # Check if data has features
            if(!has_feature_data(x=data)){
              return(data)
            }

            # Find the columns containing features
            feature_columns <- get_feature_columns(x=data)
            
            # Determine which columns have missing entries
            censored_features <- feature_columns[sapply(feature_columns, function(ii, data_obj) (!all(is_valid_data(data_obj@data[[ii]]))), data_obj=data)]
            
            # Skip if there are no censored features
            if(length(censored_features) == 0){
              return(data)
            }
            
            # Fill out all censored entries by simple imputation
            uncensored_data <- impute.impute_simple(cl=cl, data_obj=data, feature_info_list=feature_info_list, censored_features=censored_features)
            
            # Fill out all censored entries by lasso-based imputation
            data <- impute.impute_lasso(cl=cl, data_obj=data, uncensored_data_obj=uncensored_data,
                                        feature_info_list=feature_info_list, censored_features=censored_features)
            
            return(data)
          })


#####cluster_features#####
setMethod("cluster_features", signature(data="dataObject"),
          function(data, feature_info_list){
            
            # Check if data is empty
            if(is_empty(data)){
              return(data)
            }
            
            # Check if data has features
            if(!has_feature_data(x=data)){
              return(data)
            }

            # Find the columns containing features
            feature_columns <- get_feature_columns(x=data)
            
            # Identify features that form non-singular clusters
            clustering_features <- find_clustering_features(features=feature_columns, feature_info_list=feature_info_list)
            
            # Skip if all clusters are singular
            if(length(clustering_features) == 0){
              return(data)
            }
            
            # Reconstitute a cluster table
            cluster_table <- get_cluster_table(feature_info_list=feature_info_list, selected_features=clustering_features)

            # Cluster data
            clustered_data <- data.table::setDT(unlist(unname(lapply(split(cluster_table, by="cluster_name"), cluster.compute_cluster, data_obj=data)), recursive=FALSE))
            
            # Combine with non-clustered input data
            data@data <- cbind(data@data[, !clustering_features, with=FALSE], clustered_data)
            
            return(data)
          })



#####update_with_replacement (dataObject)#####
setMethod("update_with_replacement", signature(data="dataObject"),
          function(data, replacement_list){
            # Updates columns of a data table with replacement data from repl_list
            dt_repl <- data.table::copy(data@data)
            
            # Find feature names corresponding to columns to be replaced
            repl_feat <- names(replacement_list)
            
            # Iterate over replacement list entries
            for(curr_feat in repl_feat){
              dt_repl[, (curr_feat):=replacement_list[[curr_feat]] ]
            }
            
            # Replace data
            data@data <- dt_repl
            
            return(data)
          })


#####update_with_replacement (data.table)#####
setMethod("update_with_replacement", signature(data="data.table"),
          function(data, replacement_list){
            # Updates columns of a data table with replacement data from the
            # replacement list.
            replacement_table <- data.table::copy(data)
            
            # Find feature names corresponding to columns to be replaced
            replace_features <- names(replacement_list)
            
            # Iterate over replacement list entries
            for(curr_feat in replace_features){
              replacement_table[, (curr_feat):=replacement_list[[curr_feat]] ]
            }
            
            return(replacement_table)
          })


#####select_features#####
setMethod("select_features", signature(data="dataObject"),
          function(data, features){
            # Allows for selection of samples
            
            # Find non-feature columns
            non_feature_columns <- get_non_feature_columns(x=data)
            
            # Check if features are present as column name
            if(!all(features %in% colnames(data@data))){
              logger.stop("Not all features were found in the data set.")
            }
            
            # Select features
            dt <- data.table::copy(data@data[, c(non_feature_columns, features), with=FALSE])
            
            # Substitute in data 
            data@data <- dt
            
            return(data)
          })

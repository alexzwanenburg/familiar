#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#####as_data_object (dataObject)#####
setMethod("as_data_object", signature(data="dataObject"),
          function(data, ...) return(data))


#####as_data_object (data.table)#####
setMethod("as_data_object", signature(data="data.table"),
          function(data,
                   sample_id_column=waiver(),
                   batch_id_column=waiver(),
                   development_batch_id=waiver(),
                   validation_batch_id=waiver(),
                   outcome_name=waiver(),
                   outcome_column=waiver(),
                   outcome_type=waiver(),
                   event_indicator=waiver(),
                   censoring_indicator=waiver(),
                   competing_risk_indicator=waiver(),
                   class_levels=waiver(),
                   exclude_features=waiver(),
                   include_features=waiver(),
                   ...){
            
            # Load settings from ellipsis
            settings <- do.call(.parse_initial_settings, args=c(list("experimental_design"="fs+mb",
                                                                     "sample_id_column"=sample_id_column,
                                                                     "batch_id_column"=batch_id_column,
                                                                     "development_batch_id"=development_batch_id,
                                                                     "validation_batch_id"=validation_batch_id,
                                                                     "outcome_name"=outcome_name,
                                                                     "outcome_column"=outcome_column,
                                                                     "outcome_type"=outcome_type,
                                                                     "event_indicator"=event_indicator,
                                                                     "censoring_indicator"=censoring_indicator,
                                                                     "competing_risk_indicator"=competing_risk_indicator,
                                                                     "class_levels"=class_levels,
                                                                     "exclude_features"=exclude_features,
                                                                     "include_features"=include_features),
                                                                list(...)))
            
            # Prepare data.table.
            data <- .load_data(data=data,
                               sample_id_column=settings$data$sample_col,
                               batch_id_column=settings$data$batch_col)
            
            # Update settings
            settings <- .update_initial_settings(data=data, settings=settings)
            
            # Parse data
            data <- .finish_data_preparation(data = data,
                                             sample_id_column = settings$data$sample_col,
                                             batch_id_column = settings$data$batch_col,
                                             outcome_column = settings$data$outcome_col,
                                             outcome_type = settings$data$outcome_type,
                                             include_features = settings$data$include_features,
                                             class_levels = settings$data$class_levels,
                                             censoring_indicator=settings$data$censoring_indicator,
                                             event_indicator=settings$data$event_indicator,
                                             competing_risk_indicator=settings$data$competing_risk_indicator)
            
            # Convert to dataObject
            data <- methods::new("dataObject",
                                 data = data,
                                 preprocessing_level="none",
                                 outcome_type = settings$data$outcome_type,
                                 outcome_info = create_outcome_info(settings=settings))
            
            return(data)
          })


#####as_data_object (ANY)#####
setMethod("as_data_object", signature(data="ANY"),
          function(data,
                   sample_id_column=waiver(),
                   batch_id_column=waiver(),
                   ...){
            
            # Create a local copy of batch_id_column to pass on to .load_data.
            if(is.waive(sample_id_column)){
              sample_id_column_local <- NULL
              
            } else {
              sample_id_column_local <- sample_id_column
            }
            
            # Create a local copy of batch_id_column to pass on to .load_data.
            if(is.waive(batch_id_column)){
              batch_id_column_local <- NULL
              
            } else {
              batch_id_column_local <- batch_id_column
            }
            
            # Load data and convert to data.table
            data <- .load_data(data=data,
                               sample_id_column=sample_id_column_local,
                               batch_id_column=batch_id_column_local)
            
            # Pass on to data.table method.
            return(do.call(as_data_object, args=c(list("data"=data,
                                                       "sample_id_column"=sample_id_column,
                                                       "batch_id_column"=batch_id_column),
                                                  list(...))))
          })


setMethod("extract_settings_from_data", signature(data="dataObject"),
          function(data, settings=NULL){
            
            if(is.null(settings)){
              settings <- list("data"=list())
            }
            
            # Sample identifier column
            settings$data$sample_col <- "subject_id"
            settings$data$batch_col <- "cohort_id"
            settings$data$outcome_col <- get_outcome_columns(data)
            settings$data$outcome_type <- data@outcome_type
            settings$data$outcome_name <- get_outcome_name(data@outcome_info)
            settings$data$class_levels <- get_outcome_class_levels(data@outcome_info)
            settings$data$event_indicator <- data@outcome_info@event
            settings$data$censoring_indicator <- data@outcome_info@censored
            settings$data$competing_risk_indicator <- data@outcome_info@competing_risk
            settings$data$include_features <- get_feature_columns(data)
            
            return(settings)
          })


#####create_data_object (vimp method)#####
setMethod("create_data_object", signature(object="familiarVimpMethod", data="ANY"),
          function(object, data, is_pre_processed=FALSE) .create_data_object(object=object,
                                                                             data=data,
                                                                             is_pre_processed=is_pre_processed))

#####create_data_object (model)#####
setMethod("create_data_object", signature(object="familiarModel", data="ANY"),
          function(object, data, is_pre_processed=FALSE) .create_data_object(object=object,
                                                                             data=data,
                                                                             is_pre_processed=is_pre_processed))
          
          
#####create_data_object (ensemble)#####
setMethod("create_data_object", signature(object="familiarEnsemble", data="ANY"),
          function(object, data, is_pre_processed=FALSE) .create_data_object(object=object,
                                                                             data=data,
                                                                             is_pre_processed=is_pre_processed))

.create_data_object <- function(object, data, is_pre_processed){
  # Skip checks if the data already is a data objects
  if(is(data, "dataObject")) return(data)
  
  # TODO check input data consistency, i.e. column naming etc.
  new_data <- methods::new("dataObject",
                           data=data,
                           preprocessing_level=ifelse(is_pre_processed, "clustering", "none"),
                           outcome_type=object@outcome_type)
  
  return(new_data)
}


#####load_delayed_data (model)#####
setMethod("load_delayed_data", signature(data="dataObject", object="ANY"),
          function(data, object, stop_at){
            # Loads data from internal memory

            if(!(is(object, "familiarModel") | is(object, "familiarVimpMethod"))){
              ..error_reached_unreachable_code("load_delayed_data: object is expected to be a familiarModel or familiarVimpMethod.")
            }
            
            # Check if loading was actually delayed
            if(!data@delay_loading) return(data)

            # Read project list and settings
            iter_list <- get_project_list()$iter_list

            # Read required features
            req_feature_cols <- object@req_feature_cols
            
            # Get columns in data frame which are not features, but identifiers and outcome instead
            non_feature_cols <- get_non_feature_columns(x=object)
            
            # Get subject ids
            run_id_list <- getIterID(run=list("run_table"=object@run_table), perturb_level=data@perturb_level)
            run_subj_id <- getSubjectIDs(iter_list=iter_list, data_id=run_id_list$data, run_id=run_id_list$run,
                                         train_or_validate=ifelse(data@load_validation, "valid", "train"))
            
            # Check subjects and select unique subjects
            if(!is.null(run_subj_id)){
              uniq_subj_id <- unique(run_subj_id)
              
            } else {
              # Return an updated data object, but without data
              return(methods::new("dataObject",
                                  data=NULL,
                                  preprocessing_level="none",
                                  outcome_type=data@outcome_type,
                                  aggregate_on_load=data@aggregate_on_load))
            }
            
            # Prepare a new data object
            new_data <- methods::new("dataObject",
                                     data = get_data_from_backend(sample_identifiers=uniq_subj_id, column_names=c(non_feature_cols, req_feature_cols)),
                                     preprocessing_level="none",
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
                                  preprocessing_level="none",
                                  outcome_type = data@outcome_type,
                                  aggregate_on_load = data@aggregate_on_load))
            }
            
            # Prepare a new data object
            new_data <- methods::new("dataObject",
                                     data = get_data_from_backend(sample_identifiers=uniq_subj_id, column_names=c(non_feature_cols, req_feature_cols)),
                                     preprocessing_level="none",
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


#####preprocess_data (vimp method)#####
setMethod("preprocess_data", signature(data="dataObject", object="familiarVimpMethod"),
          function(data, object, stop_at="clustering") .pre_process_data(data=data,
                                                                         object=object,
                                                                         stop_at=stop_at))


#####preprocess_data (model)#####
setMethod("preprocess_data", signature(data="dataObject", object="familiarModel"),
          function(data, object, stop_at="clustering") .pre_process_data(data=data,
                                                                         object=object,
                                                                         stop_at=stop_at))


#####preprocess_data (ensemble)#####
setMethod("preprocess_data", signature(data="dataObject", object="familiarEnsemble"),
          function(data, object, stop_at="clustering") .pre_process_data(data=data,
                                                                         object=object,
                                                                         stop_at=stop_at))


.pre_process_data <- function(data, object, stop_at){
  
  # Convert the preprocessing_level attained and the requested
  # stopping level to ordinals.
  preprocessing_level_attained <- .as_preprocessing_level(data@preprocessing_level)
  stop_at <- .as_preprocessing_level(stop_at)
  
  # Check whether pre-processing is required
  if(preprocessing_level_attained == stop_at){
    return(data)
    
  } else if(preprocessing_level_attained > stop_at) {
    ..error_reached_unreachable_code("preprocess_data,dataObject,ANY: data were preprocessed at a higher level than required by stop_at.")
  }
  
  if(preprocessing_level_attained < "signature" & stop_at >= "signature"){
    # Apply the signature.
    data <- apply_signature(data_obj=data,
                            selected_feat=object@req_feature_cols)
    
    # Update pre-processing level externally from apply_signature, as
    # it is not limited to pre-processing per sé.
    data@preprocessing_level <- "signature"
  }
  
  if(preprocessing_level_attained < "transformation" & stop_at >= "transformation"){
    # Transform the features.
    data <- transform_features(data=data,
                               feature_info_list=object@feature_info)
  }
  
  if(preprocessing_level_attained < "normalisation" & stop_at >= "normalisation"){
    # Normalise feature values.
    data <- normalise_features(data=data,
                               feature_info_list=object@feature_info)
  }
  
  if(preprocessing_level_attained < "batch_normalisation" & stop_at >= "batch_normalisation"){
    # Batch-normalise feature values
    data <- batch_normalise_features(data=data,
                                     feature_info_list=object@feature_info)
  }
  
  if(preprocessing_level_attained < "imputation" & stop_at >= "imputation"){
    # Impute missing values
    data  <- impute_features(data=data,
                             feature_info_list=object@feature_info)
  }
  
  if(preprocessing_level_attained < "clustering" & stop_at >= "clustering"){
    # Cluster features
    data <- cluster_features(data=data,
                             feature_info_list=object@feature_info)
  }
  
  if(is(object, "familiarModel") & stop_at >= "clustering"){
    # Select only the signature (if present)
    if(!is.null(object@signature)){
      data <- apply_signature(data_obj=data,
                              selected_feat=object@signature)
    }
  }
  
  return(data)
}



#####process_input_data (vimp method)#####
setMethod("process_input_data", signature(object="familiarVimpMethod", data="ANY"),
          function(object, data, is_pre_processed=FALSE, stop_at="clustering") .process_input_data(object=object,
                                                                                                   data=data,
                                                                                                   is_pre_processed=is_pre_processed,
                                                                                                   stop_at=stop_at))

#####process_input_data (model)#####
setMethod("process_input_data", signature(object="familiarModel", data="ANY"),
          function(object, data, is_pre_processed=FALSE, stop_at="clustering") .process_input_data(object=object,
                                                                                                   data=data,
                                                                                                   is_pre_processed=is_pre_processed,
                                                                                                   stop_at=stop_at))

#####process_input_data (ensemble)#####
setMethod("process_input_data", signature(object="familiarEnsemble", data="ANY"),
          function(object, data, is_pre_processed=FALSE, stop_at="clustering") .process_input_data(object=object,
                                                                                                   data=data,
                                                                                                   is_pre_processed=is_pre_processed,
                                                                                                   stop_at=stop_at))

.process_input_data <- function(object, data, is_pre_processed, stop_at){
  # Check whether data is a dataObject, and create one otherwise
  if(!is(data, "dataObject")){
    data <- create_data_object(object=object, data=data, is_pre_processed=is_pre_processed)
  }
  
  # Load data from internal memory, if not provided otherwise
  if(data@delay_loading){
    data <- load_delayed_data(data=data, object=object, stop_at=stop_at)
  }
  
  # Pre-process data in case it has not been pre-processed
  data <- preprocess_data(data=data, object=object, stop_at=stop_at)
  
  # Return data
  return(data)
}


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
            id_cols <- get_non_feature_columns(x=data, sample_level_only=TRUE)
            
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
            
            # Check if transformation was already performed.
            if(!invert & .as_preprocessing_level(data) >= "transformation"){
              ..error_reached_unreachable_code("transform_features,dataObject: attempting to transform data that are already transformed.")
            }
            
            # Update the preprocessing level.
            if(!invert) data@preprocessing_level <- "transformation"
            
            # Check if data is empty
            if(is_empty(data)) return(data)
            
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
            
            # Check if normalisation was already performed.
            if(!invert & .as_preprocessing_level(data) >= "normalisation"){
              ..error_reached_unreachable_code("normalise_features,dataObject: attempting to normalise data that are already normalised.")
            }
            
            # Check if the previous step (transformation) was conducted.
            if(!invert & .as_preprocessing_level(data) < "transformation"){
              ..error_reached_unreachable_code("normalise_features,dataObject: data should be transformed prior to normalisation.")
            }
            
            # Update the preprocessing_level.
            if(!invert) data@preprocessing_level <- "normalisation"
            
            # Check if data is empty
            if(is_empty(data)) return(data)
            
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
            
            # Check if batch normalisation was already performed.
            if(.as_preprocessing_level(data) >= "batch_normalisation"){
              ..error_reached_unreachable_code("batch_normalise_features,dataObject: attempting to batch normalise data that are already batch normalised.")
            }
            
            # Check if the previous step (normalisation) was conducted.
            if(.as_preprocessing_level(data) < "normalisation"){
              ..error_reached_unreachable_code("batch_normalise_features,dataObject: data should be normalised globally prior to batch normalisation.")
            }
            
            # Update the attained processing level.
            data@preprocessing_level <- "batch_normalisation"
            
            # Check if data is empty
            if(is_empty(data)) return(data)
            
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
            
            # Check if imputation was already performed.
            if(.as_preprocessing_level(data) >= "imputation"){
              ..error_reached_unreachable_code("impute_features,dataObject: attempting to impute data that already have been imputed.")
            }
            
            # Check if the previous step (batch normalisation) was conducted.
            if(.as_preprocessing_level(data) < "batch_normalisation"){
              ..error_reached_unreachable_code("impute_features,dataObject: data should be batch normalised prior to imputation.")
            }
            
            # Update the attained processing level.
            data@preprocessing_level <- "imputation"
            
            # Check if data is empty
            if(is_empty(data)) return(data)

            # Check if data has features
            if(!has_feature_data(x=data)) return(data)
            
            # Find the columns containing features
            feature_columns <- get_feature_columns(x=data)
            
            # Determine which columns have missing entries
            censored_features <- feature_columns[sapply(feature_columns, function(ii, data_obj) (!all(is_valid_data(data_obj@data[[ii]]))), data_obj=data)]
            
            # Skip if there are no censored features
            if(length(censored_features) == 0) return(data)
            
            # Fill out all censored entries by simple imputation
            uncensored_data <- impute.impute_simple(cl=cl,
                                                    data_obj=data,
                                                    feature_info_list=feature_info_list,
                                                    censored_features=censored_features)
            
            # Fill out all censored entries by lasso-based imputation
            data <- impute.impute_lasso(cl=cl,
                                        data_obj=data,
                                        uncensored_data_obj=uncensored_data,
                                        feature_info_list=feature_info_list,
                                        censored_features=censored_features)
            
            return(data)
          })


#####cluster_features#####
setMethod("cluster_features", signature(data="dataObject"),
          function(data, feature_info_list){
            
            if(.as_preprocessing_level(data) >= "clustering"){
              ..error_reached_unreachable_code("cluster_features,dataObject: attempting to cluster data that already have been clustered.")
            }
            
            # Check if the previous step (imputation) was conducted.
            if(.as_preprocessing_level(data) < "imputation"){
              ..error_reached_unreachable_code("cluster_features,dataObject: data should be imputed prior to clustering.")
            }
            
            # Update the attained processing level.
            data@preprocessing_level <- "clustering"
            
            # Check if data is empty
            if(is_empty(data)) return(data)
            
            # Check if data has features
            if(!has_feature_data(x=data)) return(data)
            
            # Find the columns containing features
            feature_columns <- get_feature_columns(x=data)
            
            # Identify features that form non-singular clusters
            clustering_features <- find_clustering_features(features=feature_columns, feature_info_list=feature_info_list)
            
            # Skip if all clusters are singular
            if(length(clustering_features) == 0) return(data)
            
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

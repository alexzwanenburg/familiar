#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

create_feature_info <- function(data, signature=NULL, ...){
  # This creates a list of featureInfo objects, with processing, based on data.
  # This code is primarily used within unit tests.
  
   #####Prepare settings###############################################
  
  # Reconstitute settings from the data.
  settings <- extract_settings_from_data(data=data)
  
  # Update some missing settings that can be fixed within this method.
  settings$data$train_cohorts <- unique(data@data[[get_id_columns(single_column="batch")]])
  
  # Parse the remaining settings that are important. Remove
  # outcome_type from ... This prevents an error caused by multiple
  # matching arguments.
  dots <- list(...)
  
  settings <- do.call(.parse_general_settings,
                      args=c(list("settings"=settings,
                                  "data"=data@data),
                             dots))
  
  
  #####Prepare featureInfo objects####################################
  
  # Create a list of featureInfo objects.
  feature_info_list <- .get_feature_info_data(data=data@data,
                                              file_paths=NULL,
                                              project_id=character(),
                                              outcome_type=data@outcome_type)
  
  # Extract the generic data.
  feature_info_list <- feature_info_list[["generic"]]
  
  # Add signature feature info
  feature_info_list <- add_signature_info(feature_info_list=feature_info_list, signature=signature)
  
  # Perform some pre-processing (i.e. remove singular features)
  feature_info_list <- .determine_preprocessing_parameters(cl=NULL,
                                                           data=data,
                                                           feature_info_list=feature_info_list,
                                                           settings=settings,
                                                           verbose=FALSE)
  
  return(feature_info_list)
}



.get_feature_info_data <- function(data, file_paths, project_id, outcome_type){
  
  # Create path to the feature info file
  feature_info_file <- .get_feature_info_file_name(file_paths=file_paths,
                                                   project_id=project_id)
  if(is.null(file_paths)){
    
    # Create, but do not store to disk.
    feature_info_list <- list()
    feature_info_list[["generic"]] <- .get_generic_feature_info(data=data,
                                                                outcome_type=outcome_type,
                                                                descriptor=NULL)
    
  } else if(!file.exists(feature_info_file)){
    
    # Generate feature information
    feature_info_list <- list()
    feature_info_list[["generic"]] <- .get_generic_feature_info(data=data, 
                                                                outcome_type=outcome_type,
                                                                descriptor=NULL)
    
    # Write to file
    saveRDS(feature_info_list, file=feature_info_file)
    
  } else {
    feature_info_list <- readRDS(feature_info_file)
  }
  
  return(feature_info_list)
}



.get_generic_feature_info <- function(data, outcome_type, descriptor=NULL){
  # Initialises feature_info objects

  # Expect that data is a data.table.
  if(is(data, "dataObject")) data <- data@data
  
  # Identify feature columns
  feature_columns   <- get_feature_columns(x=data, outcome_type=outcome_type)
  
  # Iterate over feature columns and create a list of feature_info objects
  feature_info_list <- lapply(feature_columns, function(ii, data, descriptor) {
    
    # Determine feature type
    if(is.factor(data[[ii]])){
      feature_type <- "factor"
    } else if(is.numeric(data[[ii]])){
      feature_type <- "numeric"
    } else {
      feature_type <- "unknown"
    }
    
    # Initialise featureInfo object
    feature_info <- methods::new("featureInfo",
                                 name = ii,
                                 set_descriptor = ifelse(is.null(descriptor), NA_character_, as.character(descriptor)),
                                 feature_type = feature_type)
    
    # Set factor levels for future reproducibility
    if(feature_type == "factor"){
      feature_info@levels <- levels(data[[ii]])
      feature_info@ordered <- is.ordered(data[[ii]])
    }
    
    # Mark "unknown" feature types for removal
    if(feature_type == "unknown"){
      feature_info@removed <- TRUE
      feature_info@removed_unknown_type <- TRUE
    }
    
    # Update the familiar version.
    feature_info <- add_package_version(object=feature_info)
    
    return(feature_info)
    
  }, data=data, descriptor=descriptor)
  
  # Set names in the list of featureInfo objects
  names(feature_info_list) <- feature_columns
  
  return(feature_info_list)
}


add_control_info <- function(feature_info_list, data_id, run_id){
  # Control information is added to every feature regardless of "removed'.
  
  # Make sure that both identifiers are integers
  data_id <- as.integer(data_id)
  run_id  <- as.integer(run_id)
  
  # Update the feature info list.
  feature_info_list <- lapply(feature_info_list, function(object, data_id, run_id){
    
    # Add data and run ids.
    object@data_id <- data_id
    object@run_id  <- run_id
    
    return(object)
  }, data_id=data_id, run_id=run_id)
}



add_signature_info <- function(feature_info_list, signature=NULL){
  # Sets the in_signature flag on features in the signature variable.
  
  # Check if there is a signature
  if(is.null(signature)) return(feature_info_list)
  
  # Set signature status
  upd_list <- lapply(signature, function(signature_feature, feature_info_list){

    # Obtain object
    object <- feature_info_list[[signature_feature]]
    
    # Mark signature
    object@in_signature <- TRUE
    
    # Update removed status
    object <- update_removed_status(object=object)
    
    return(object)
  }, feature_info_list=feature_info_list)
  
  # Update the names
  names(upd_list) <- signature
  
  # Copy into the list
  feature_info_list[signature] <- upd_list
  
  return(feature_info_list)
}


add_novelty_info <- function(feature_info_list, novelty_features=NULL){
  # Sets the in_novelty flag on features in the novelty_features variable.
  
  # Check if there is a signature
  if(is.null(novelty_features)) return(feature_info_list)
  
  # Set signature status
  upd_list <- lapply(novelty_features, function(novelty_feature, feature_info_list){
    
    # Obtain object
    object <- feature_info_list[[novelty_feature]]
    
    # Mark signature
    object@in_novelty <- TRUE
    
    # Update removed status
    object <- update_removed_status(object=object)
    
    return(object)
  }, feature_info_list=feature_info_list)
  
  # Update the names
  names(upd_list) <- novelty_features
  
  # Copy into the list
  feature_info_list[novelty_features] <- upd_list
  
  return(feature_info_list)
}


add_missing_value_fractions <- function(cl=NULL, feature_info_list, data, threshold){
  # Add the fraction of missing values for features

  # Identify the feature columns in the data
  feature_columns <- get_feature_columns(x=data)
  
  # Determine number of missing values per column
  n_valid_val <- fam_sapply(cl=cl,
                            assign=NULL,
                            X=data@data[, mget(feature_columns)],
                            FUN=function(data) (return(sum(is_valid_data(data)))),
                            progress_bar=FALSE,
                            chopchop=TRUE)
  
  # Determine fraction of missing values
  missing_frac    <- 1.0 - n_valid_val / nrow(data@data)
  
  upd_list <- lapply(seq_len(length(feature_columns)), function(ii, feature_columns, feature_info_list, missing_frac, threshold){
    
    # Extract the featureInfo object
    object <- feature_info_list[[feature_columns[ii]]]
    
    # Add missing value fraction
    object@fraction_missing <- unname(missing_frac[ii])
    
    # Compare with threshold and set removal status
    if(missing_frac[ii] >= threshold){
      object@removed_missing_values <- TRUE
      
      # Update removed status
      object <- update_removed_status(object=object)
    }
    
    return(object)
  }, feature_columns=feature_columns, feature_info_list=feature_info_list, missing_frac=missing_frac, threshold=threshold)
  
  # Set names of the updated list
  names(upd_list) <- feature_columns
  
  # Update the list
  feature_info_list[feature_columns] <- upd_list
  
  return(feature_info_list)
  
}


# find_clustering_features <- function(features, feature_info_list){
#   # Determine features that form non-singular clusters
#   
#   clustering_features <- features[sapply(features, function(ii, feature_info_list){
#     
#     # Get featureInfo for the current feature
#     object <- feature_info_list[[ii]]
#     
#     # A valid cluster column contains cluster parameters
#     # Cluster parameters will be missing for features that are part of a signature.
#     if(is.null(object@cluster_parameters)){
#       return(FALSE)
#     }
#     
#     # A non-singular cluster has a size larger than 1
#     if(object@cluster_parameters$cluster_size > 1){
#       return(TRUE)
#     } else {
#       return(FALSE)
#     }
#     
#   }, feature_info_list=feature_info_list)]
#   
#   return(clustering_features)
# }


add_required_features <- function(feature_info_list){
  
  # Find features that are not removed
  available_features <- get_available_features(feature_info_list=feature_info_list)
  
  # Add required features slot
  upd_list <- lapply(available_features, function(ii, feature_info_list){
    
    # featureInfo object for the current feature
    object <- feature_info_list[[ii]]
    
    # A required feature is the feature itself
    required_features <- object@name
    
    # Required features for imputation
    if(is(object@imputation_parameters, "featureInfoParametersImputation")){
      required_features <- c(required_features, object@imputation_parameters@required_features)
    }
    
    # Required features for clustering
    if(is(object@cluster_parameters, "featureInfoParametersCluster")){
      required_features <- c(required_features, object@cluster_parameters@required_features)
    }
    
    # Add required features
    object@required_features <- unique(required_features)
    
    return(object)
  }, feature_info_list=feature_info_list)
  
  # Set names of the updated list
  names(upd_list) <- available_features
  
  # Update the list
  feature_info_list[available_features] <- upd_list
  
  return(feature_info_list)
}



# Add feature distribution data
compute_feature_distribution_data <- function(cl, feature_info_list, data){
  
  
  # Identify the feature columns in the data
  feature_columns <- get_feature_columns(x=data)
  
  # Compute feature distributions
  updated_feature_info <- fam_mapply(cl=cl,
                                     assign=NULL,
                                     FUN=.compute_feature_distribution_data,
                                     object=feature_info_list[feature_columns],
                                     x=data@data[, mget(feature_columns)],
                                     progress_bar=FALSE,
                                     chopchop=TRUE)

  if(length(feature_columns) > 0){
    feature_info_list[feature_columns] <- updated_feature_info
  }
  
  return(feature_info_list)
}



.compute_feature_distribution_data <- function(object, x){
  
  # Placeholder distribution list
  distr_list <- list()
  
  if(object@feature_type %in% c("factor")){
    
    # Number of samples
    distr_list[["n"]] <- length(x)
    
    # Number of instances for each class
    distr_list[["frequency"]] <- data.table::data.table("factor_level"=x)[, list("count"=.N), by="factor_level"]
    
  } else if(object@feature_type %in% c("numeric")){
    
    # Number of samples
    distr_list[["n"]] <- length(x)
    
    # Five-number summary of outcome values
    distr_list[["fivenum"]] <- fivenum_summary(x, na.rm=TRUE)
    
    # Mean value
    distr_list[["mean"]] <- mean(x, na.rm=TRUE)
    
  } else {
    ..error_reached_unreachable_code(".compute_feature_distribution_data: unknown feature type encountered.")
  }
  
  # Add to slot
  object@distribution <- distr_list
  
  return(object)
}



find_invariant_features <- function(cl=NULL, feature_info_list, data){
  # Find features that are invariant. Such features are ill-behaved and should be removed.
  
  # Identify the feature columns in the data
  feature_columns <- get_feature_columns(x=data)
  
  # Shorthand.
  singular_features <- fam_sapply(cl=cl,
                                  assign=NULL,
                                  X=data@data[, mget(feature_columns)],
                                  FUN=is_singular_data,
                                  progress_bar=FALSE,
                                  chopchop=TRUE)
  
  singular_features <- feature_columns[singular_features]
  
  # Iterate over singular features and mark for removal
  if(length(singular_features) > 0){
    upd_list <- lapply(singular_features, function(ii, feature_info_list){
      # Get the featureInfo for the current feature
      object <- feature_info_list[[ii]]
      
      # Set flag to TRUE (default FALSE)
      object@removed_no_variance <- TRUE
      
      # Update the removed flag
      object <- update_removed_status(object=object)
      
      return(object)
    }, feature_info_list=feature_info_list)
    
    # Add names to the elements of upd_list
    names(upd_list) <- singular_features
    
    # Update the list
    feature_info_list[singular_features] <- upd_list
  }
  
  return(feature_info_list)
}


find_low_variance_features <- function(cl=NULL,
                                       feature_info_list,
                                       data,
                                       settings){
  # Determine which features have a very low variance and remove these

  # Suppress NOTES due to non-standard evaluation in data.table
  variance <- NULL
  
  # Identify the feature columns in the data
  feature_columns <- get_feature_columns(x=data)
  
  # Determine which columns actually contains numeric data
  numeric_columns <- feature_columns[sapply(feature_columns, function(ii, data) (is.numeric(data@data[[ii]])), data=data)]
  
  # Skip if there are no numeric columns
  if(length(numeric_columns) == 0){
    return(feature_info_list)
  }
  
  # Determine variance.
  feature_variances <- fam_sapply(cl=cl,
                                  assign=NULL,
                                  X=data@data[, mget(numeric_columns)],
                                  FUN=stats::var,
                                  progress_bar=FALSE,
                                  na.rm=TRUE,
                                  chopchop=TRUE)
  
  # Define a data table containing the variances
  dt_var <- data.table::data.table("name"=numeric_columns,
                                   "variance"=feature_variances)
  
  # Set missing parameters
  if(is.null(settings$prep$low_var_threshold)){
    # If unset, potentially include all data, pending low_var_max_feature_set_size.
    settings$prep$low_var_threshold <- -1
  } 
  
  if(is.null(settings$prep$low_var_max_feature_set_size)){
    # If unset, potentially allow all data, pending low_var_threshold.
    settings$prep$low_var_max_feature_set_size <- nrow(dt_var)
  }
  
  # Determine cutoff variance required to obtain the maximally sized feature set
  if(settings$prep$low_var_max_feature_set_size > nrow(dt_var)){
    # If the number allowed features in the feature set exceeds the actual number of features, set the
    # mimimum variance threshold to 0.
    min_var_thresh <- 0
    
  } else {
    # Else, set the minimum variance threshold to the level that corresponds to maximum feature set size.
    min_var_thresh <- sort(dt_var$variance, decreasing=TRUE)[settings$prep$max_var_feat_set_size]
    
  }
  
  # Set the variance threshold
  sel_var_thresh <- max(min_var_thresh, settings$prep$low_var_threshold)
  
  # Determine the low-variance features that are to be removed
  low_variance_features <- dt_var[variance < sel_var_thresh]$name
  
  # Set removal status
  if(length(low_variance_features) > 0){
    
    upd_list <- lapply(low_variance_features, function(ii, feature_info_list){
      # Get the featureInfo for the current feature
      object <- feature_info_list[[ii]]
      
      # Set flag to TRUE (default FALSE)
      object@removed_low_variance <- TRUE
      
      # Update the removed flag
      object <- update_removed_status(object=object)
      
      return(object)
    }, feature_info_list=feature_info_list)
    
    # Add names to the elements of upd_list
    names(upd_list) <- low_variance_features
    
    # Update the list
    feature_info_list[low_variance_features] <- upd_list
  }
  
  return(feature_info_list)
}


find_non_robust_features <- function(cl=NULL,
                                     feature_info_list,
                                     data,
                                     settings){
  # Determine which features lack robustness and are to be removed.
  # This is only possible for repeated measurements.
  
  # Check if repeated measurements are present, otherwise return feature info
  # list as is..
  if(all(data@data$repetition_id==1)) return(feature_info_list)
  
  # Determine which columns contain feature data
  feature_columns <- get_feature_columns(x=data)
  
  # Determine which columns actually contains numeric data.
  numeric_columns <- feature_columns[sapply(feature_columns, function(ii, data) (is.numeric(data@data[[ii]])), data=data)]
  
  # Skip if there are no columns with numeric data.
  if(length(numeric_columns) == 0) return(feature_info_list)
  
  # Read several items from settings
  icc_type          <- settings$prep$robustness_icc_type
  icc_filter_column <- settings$prep$robustness_threshold_param
  icc_threshold     <- settings$prep$robustness_threshold_value
  
  # Compute ICC values.
  icc_list <- fam_mapply(cl=cl,
                         assign=NULL,
                         FUN=compute_icc,
                         x=data@data[, mget(numeric_columns)],
                         feature=numeric_columns,
                         progress_bar=FALSE,
                         MoreArgs=list("id_data"=data@data[, mget(get_id_columns())],
                                       "type"=icc_type),
                         chopchop=TRUE)
  
  # Combine ICC data from list
  icc_table <- data.table::rbindlist(icc_list)
  
  # Identify the features with low robustness
  low_robustness_features <- icc_table[get(icc_filter_column) < icc_threshold]$feature
  
  # Set removal flags for features with low robustness
  if(length(low_robustness_features) > 0){
    
    upd_list <- lapply(low_robustness_features, function(ii, feature_info_list){
      # Get the featureInfo for the current feature
      object <- feature_info_list[[ii]]
      
      # Set flag to TRUE (default FALSE)
      object@removed_low_robustness <- TRUE
      
      # Update the removed flag
      object <- update_removed_status(object=object)
      
      return(object)
    }, feature_info_list=feature_info_list)
    
    # Add names to the elements of upd_list
    names(upd_list) <- low_robustness_features
    
    # Update the list
    feature_info_list[low_robustness_features] <- upd_list
  }
  
  return(feature_info_list)
}


find_unimportant_features <- function(cl=NULL,
                                      feature_info_list,
                                      data,
                                      settings){
  # Find which features are not important for the current endpoint.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  p_full <- q_full <- p_val <- q_val <- name <- p_median <- q_median <- q_sel <- p_sel <- NULL
  
  # Base calculatutions on medians/modes for repeated data. Repeated
  # measurements are not independent and may inflate statistics.
  data <- aggregate_data(data=data)
  
  # Determine feature columns
  feature_columns <- get_feature_columns(x=data)
  
  # Set q-value to 1 if none is provided
  if(is.null(settings$prep$univar_threshold))  {
    # If NULL, allow potential selection of all features, pending
    # univar_feat_set_size.
    settings$prep$univar_threshold <- 1
  }
  
  if(is.null(settings$prep$univar_feat_set_size)){
    # If NULL, allow potential selection of all features, pending
    # univar_threshold
    settings$prep$univar_feat_set_size <- length(feature_columns)
  }
  
  # Generate bootstraps
  n_iter <- 10
  iter_list <- .create_bootstraps(n_iter=n_iter,
                                  settings=settings,
                                  data=data@data,
                                  stratify=TRUE)
  
  
  ##### Calculate metric values over the full data #####
  # Calculate p-values of the coefficients
  regr_pval <- compute_univariable_p_values(cl=cl,
                                            data_obj=data,
                                            feature_columns=feature_columns)
  
  # Find and replace non-finite values
  regr_pval[!is.finite(regr_pval)] <- 1.0
  
  # Add to table.
  dt_regr_full   <- data.table::data.table("name"=names(regr_pval), "p_full"=regr_pval)
  dt_regr_full[!is.finite(p_full), "p_full":=1]
  
  # Calculate q-value
  if(nrow(dt_regr_full) >= 2 & is_package_installed(name="qvalue")){
    dt_regr_full[, "q_full":=qvalue::qvalue(p=p_full, lambda=0)$qvalues]
  } else {
    dt_regr_full[, "q_full":=p_full]
  }
  
  if(settings$prep$univar_metric=="q_value"){
    # Filter out features with high q-value
    feature_columns <- feature_columns[feature_columns %in% dt_regr_full[q_full<=settings$prep$univar_threshold, ]$name]
  } else if(settings$prep$univar_metric=="p_value") {
    feature_columns <- feature_columns[feature_columns %in% dt_regr_full[p_full<=settings$prep$univar_threshold, ]$name]
  }
  
  ##### Calculate metric values over bootstraps #####
  
  # Initiate storage list
  bt_regr_pval   <- list()
  
  # Iterate over bootstraps
  for(ii in 1:n_iter){
    
    # Bootstrap data
    data_bootstrap_obj <- select_data_from_samples(data=data,
                                                   samples=iter_list$train_list[[ii]])
    
    # Calculate p-values for the current bootstrap
    if(length(feature_columns)>0){
      regr_pval          <- compute_univariable_p_values(cl=cl, data_obj=data_bootstrap_obj, feature_columns=feature_columns)
      bt_regr_pval[[ii]] <- data.table::data.table("name"=names(regr_pval), "p_val"=regr_pval, "iter_id"=ii)
      bt_regr_pval[[ii]][!is.finite(p_val), "p_val":=1.0]
      
      # Calculate q-values for the current bootstrap
      if(nrow(bt_regr_pval[[ii]]) >= 2 & is_package_installed(name="qvalue")){
        bt_regr_pval[[ii]][, "q_val":=qvalue::qvalue(p=p_val, lambda=0)$qvalues]
      } else {
        bt_regr_pval[[ii]][, "q_val":=p_val]
      }
      rm(regr_pval)
      
    } else {
      bt_regr_pval[[ii]] <- data.table::data.table("name"=character(), "p_val"=numeric(), "iter_id"=numeric(), "q_val"=numeric())
    }
    
    rm(data_bootstrap_obj)
  }
  
  # Combine into single data table
  dt_regr_bs <- data.table::rbindlist(bt_regr_pval)
  
  # Calculate median metric values of the bootstraps
  dt_regr_bs <- dt_regr_bs[, list(p_median=stats::median(p_val, na.rm=TRUE), q_median=stats::median(q_val, na.rm=TRUE)), by=name]
  
  # Merge median metric value table and the full table
  dt_regr_bs <- merge(x=dt_regr_bs, y=dt_regr_full, by="name", all=TRUE)
  
  # Address non-finite entries
  dt_regr_bs[!is.finite(p_median), "p_median":=1]
  dt_regr_bs[!is.finite(q_median), "q_median":=1]
  
  # Find the worst entries among the bootstraps and the full analysis
  dt_regr_bs[, ":="("p_sel"=pmax(p_median, p_full), "q_sel"=pmax(q_median, q_full)), by=name]
  
  rm(dt_regr_full)
  
  # Determine cutoff required to obtain the maximally sized feature set
  if(settings$prep$univar_feat_set_size > nrow(dt_regr_bs)){
    max_thresh <- 1
  } else if(settings$prep$univar_metric=="q_value") {
    max_thresh <- sort(dt_regr_bs$q_sel)[settings$prep$univar_feat_set_size]
  } else if(settings$prep$univar_metric=="p_value") {
    max_thresh <- sort(dt_regr_bs$p_sel)[settings$prep$univar_feat_set_size]
  }
  
  # Set the actual q threshold (the lower of max_q_thresh and settings$prep$univar_threshold)
  sel_thresh   <- min(max_thresh, settings$prep$univar_threshold)
  
  # Return features which have a q-value above the selected threshold
  if(settings$prep$univar_metric=="q_value"){
    unimportant_features <- dt_regr_bs[q_sel > sel_thresh, ]$name
  } else if(settings$prep$univar_metric=="p_value"){
    unimportant_features <- dt_regr_bs[p_sel > sel_thresh, ]$name
  } else {
    unimportant_features <- character(0)
  }
  
  if(length(unimportant_features) > 0){
    
    upd_list <- lapply(unimportant_features, function(ii, feature_info_list){
      # Get the featureInfo for the current feature
      object <- feature_info_list[[ii]]
      
      # Set flag to TRUE (default FALSE)
      object@removed_low_importance <- TRUE
      
      # Update the removed flag
      object <- update_removed_status(object=object)
      
      return(object)
    }, feature_info_list=feature_info_list)
    
    # Add names to the elements of upd_list
    names(upd_list) <- unimportant_features
    
    # Update the list
    feature_info_list[unimportant_features] <- upd_list
    
  }
  
  return(feature_info_list)
}



get_available_features <- function(feature_info_list, data_obj=NULL, exclude_signature=FALSE, exclude_novelty=FALSE){
  # Determine the intersect of features a removed slot == FALSE and 
  # available columns in dt (if not NULL).
  
  # Check that any features are available.
  if(length(feature_info_list) == 0) return(NULL)
  
  available_list_features <- names(feature_info_list)[sapply(feature_info_list, is_available)]
  
  if(!is.null(data_obj)){
    available_data_features <- get_feature_columns(x=data_obj)
    
    # The set of available features is the intersect of both.
    available_features <- intersect(available_list_features, available_data_features)
  } else {
    
    # The set of available features is equal to available_list_features.
    available_features <- available_list_features
  }
  
  if(exclude_signature){
    # Determine the features in the signature
    signature_features <- names(feature_info_list)[sapply(feature_info_list, is_in_signature)]
    
    # Exclude these from the available features, e.g. for operations that only
    # work on non-signature features.
    available_features <- setdiff(available_features, signature_features)
  }
  
  if(exclude_novelty){
    # Determine the features that are specifically novelty features.
    novelty_features <- names(feature_info_list)[sapply(feature_info_list, is_in_novelty)]
    
    # Exclude these from the available features, e.g. for operations that only
    # work on non-signature features.
    available_features <- setdiff(available_features, novelty_features)
  }
  
  return(available_features)
}


# find_required_features <- function(features, feature_info_list){
# 
#   if(length(features) == 0) return(features)
#   
#   # Make sure that the input features are original features
#   features <- features_before_clustering(features=features, feature_info_list=feature_info_list)
#   
#   # Iterate over features to find all required features
#   required_features <- unlist(lapply(features, function(ii, feature_info_list) {
#     return(feature_info_list[[ii]]@required_features)
#   }, feature_info_list=feature_info_list))
#   
#   return(unique(required_features))
# }


# find_model_features <- function(features, feature_info_list){
#   
#   # Important features are original features
#   features <- features_before_clustering(features=features, feature_info_list=feature_info_list)
#   
#   return(features)
# }


find_novelty_features <- function(model_features=NULL, feature_info_list){
  
  # Find additional features that should be used for novelty detection.
  novelty_features <- unlist(lapply(feature_info_list, function(feature_info) {
    # Check if the feature is a novelty feature.
    if(!feature_info@in_novelty) return(NULL)
    
    return(feature_info@name)
  }))
  
  return(union(model_features, novelty_features))
}




trim_unused_features_from_list <- function(feature_info_list){
  
  # Iterate over features to find all required features
  required_features <- unlist(lapply(feature_info_list, function(feature_info) {
    # Check if the feature was removed.
    if(feature_info@removed) return(NULL)
    
    return(feature_info@required_features)
    
  }))
  
  # All required features.
  required_features <- unique(required_features)
  
  # All signature features.
  signature_features <- unlist(lapply(feature_info_list, function(feature_info) {
    # Check if the feature is in the signature.
    if(!feature_info@in_signature) return(NULL)
    
    return(feature_info@name)
    
  }))
  
  # All novelty features.
  novelty_features <- unlist(lapply(feature_info_list, function(feature_info) {
    # Check if the feature is a novelty feature.
    if(!feature_info@in_novelty) return(NULL)
    
    return(feature_info@name)
    
  }))
  
  features_kept <- unique(c(required_features, signature_features, novelty_features))
  
  return(feature_info_list[features_kept])
}



.collect_and_aggregate_feature_info <- function(feature, object, model_list, stop_at="imputation"){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  min <- Q1 <- median <- Q3 <- max <- count <- NULL
  
  # Find all featureInfo objects for the current feature
  feature_info_list <- lapply(model_list, function(fam_model, feature){
    if(is.null(fam_model@feature_info[[feature]])){
      return(NULL)
    } else {
      return(fam_model@feature_info[[feature]])
    }
  }, feature=feature)
  
  # Remove NULL entries
  feature_info_list[sapply(feature_info_list, is.null)] <- NULL
  
  # Create a skeleton
  feature_info <- methods::new("featureInfo",
                               name = feature,
                               set_descriptor = feature_info_list[[1]]@set_descriptor,
                               feature_type = feature_info_list[[1]]@feature_type,
                               levels = feature_info_list[[1]]@levels,
                               data_id = as.integer(object@run_table$ensemble_data_id),
                               run_id = as.integer(object@run_table$ensemble_run_id),
                               in_signature = feature_info_list[[1]]@in_signature)

  # Add package version.
  feature_info <- add_package_version(object=feature_info)
  
  # Compute average freaction of data missing
  feature_info@fraction_missing <- mean(extract_from_slot(object_list=feature_info_list, slot_name="fraction_missing", na.rm=TRUE))
  
  # Compute average robustness
  if(!all_empty_slot(object_list=feature_info_list, slot_name="robustness")){
    feature_info@robustness <- mean(extract_from_slot(object_list=feature_info_list, slot_name="robustness", na.rm=TRUE))
  }
  
  # Compute average univariate importance
  if(!all_empty_slot(object_list=feature_info_list, slot_name="univariate_importance")){
    feature_info@univariate_importance <- mean(extract_from_slot(object_list=feature_info_list, slot_name="univariate_importance", na.rm=TRUE))
  }
  
  # Find distribution items
  distribution_items <- names(feature_info_list[[1]]@distribution)
  
  if(!is.null(distribution_items)){
    
    # Placeholder list
    distr_list <- list()
    
    # Iterate over items in the distribution list
    for(item in distribution_items){
      
      if(grepl(pattern="fivenum", x=item, fixed=TRUE)){
        
        # Aggregate from list
        fivenum_values <- lapply(feature_info_list, function(feature_info, item) (feature_info@distribution[[item]]), item=item)
        
        # Combine all the data.tables
        fivenum_values <- data.table::rbindlist(fivenum_values)
        
        # Check for zero-length lists.
        if(is_empty(fivenum_values)) next()
        
        # Summarise
        fivenum_values <- fivenum_values[, list("min"=min(min),
                                                "Q1"=mean(Q1),
                                                "median"=mean(median),
                                                "Q3"=mean(Q3),
                                                "max"=max(max)), ]
        
        # Add to list
        distr_list[[item]] <- fivenum_values
        
      } else if(grepl(pattern="frequency", x=item, fixed=TRUE)){
        
        # Aggregate from list
        frequency_values <- lapply(feature_info_list, function(feature_info, item) (feature_info@distribution[[item]]), item=item)
        
        # Combine all the data.tables
        frequency_values <- data.table::rbindlist(frequency_values)
        
        if(is_empty(frequency_values)) next()
        
        # Summarise and add to list
        distr_list[[item]] <- frequency_values[, list("count"=mean(count)), by="factor_level"]
        
      } else {
        # Find mean value
        distr_list[[item]] <- mean(extract_from_slot(feature_info_list, "distribution", item, na.rm=TRUE))
      }
    }
    
    # Update distribution slot
    feature_info@distribution <- distr_list
  }
  
  # Determine if instances of the feature were removed.
  instance_mask <- sapply(feature_info_list, is_available)
  
  # Extract transformation parameter data.
  transformation_parameter_data <- ..collect_and_aggregate_transformation_info(feature_info_list=feature_info_list,
                                                                               instance_mask=instance_mask,
                                                                               feature_name=feature)
  
  # Set the aggregated transformation parameters.
  feature_info@transformation_parameters <- transformation_parameter_data$parameters
  
  
  if(stop_at == "transformation") return(feature_info)
  
  # Extract normalisation parameter data.
  normalisation_parameter_data <- ..collect_and_aggregate_normalisation_info(feature_info_list=feature_info_list,
                                                                             instance_mask=instance_mask,
                                                                             feature_name=feature)
  
  # Set the aggregated normalisation methods.
  feature_info@normalisation_parameters <- normalisation_parameter_data$parameters
  
  if(stop_at == "normalisation") return(feature_info)
  
  # Extract batch normalisation parameter data.
  batch_normalisation_parameter_data <- ..collect_and_aggregate_batch_normalisation_info(feature_info_list=feature_info_list,
                                                                                         instance_mask=instance_mask,
                                                                                         feature_name=feature)
  
  # Update batch normalisation parameter data.
  feature_info@batch_normalisation_parameters <- batch_normalisation_parameter_data$parameters
  
  if(stop_at == "batch_normalisation") return(feature_info)
  
  # Extract imputation data
  imputation_parameter_data <- ..collect_and_aggregate_imputation_info(feature_info_list=feature_info_list,
                                                                       feature_name=feature,
                                                                       feature_type=feature_info@feature_type)
  
  # Add to slot.
  feature_info@imputation_parameters <- imputation_parameter_data$parameters
  
  # Set required features.
  feature_info@required_features <- feature_info@imputation_parameters@required_features
  
  return(feature_info)
}



.show_simple_feature_info <- function(object, line_end=""){
  # Determine the feature type.
  if(object@feature_type == "factor"){
    if(object@ordered){
      feature_type <- "ordinal"
    } else {
      feature_type <- "categorical"
    }
    
  } else {
    feature_type <- "numeric"
  }
  
  # Initialise the feature string.
  feature_str <- paste0(object@name, " (", feature_type, ")")
  
  if(feature_type == "categorical"){
    # Show levels, including which level is the reference.
    classes_str <- object@levels
    classes_str[1] <- paste0(classes_str[1], " (reference)")
    
    feature_str <- paste0(feature_str, ", with levels: ", paste_s(classes_str))
    
  } else if(feature_type == "ordinal"){
    # Show ordered levels of the ordinal.
    feature_str <- paste0(feature_str, ", with levels: ", paste0(object@levels, collapse= " < "))
    
  }
  
  return(paste0(feature_str, line_end))
}


#####show (featureInfo)#####
setMethod("show", signature(object="featureInfo"),
          function(object){
            
            # Make sure the model object is updated.
            object <- update_object(object=object)
            
            # Create basic feature string.
            feature_str <- .show_simple_feature_info(object=object)

            # Transformation parameters.
            # Placeholder string
            transform_str <- character(0L)
            
            # Attempt to create an actual descriptor, if meaningful.
            transform_parameters <- object@transformation_parameters
            if(!is.null(transform_parameters)){
              if(is(transform_parameters, "featureInfoParametersTransformationPowerTransform")){
                if(transform_parameters@lambda != 1.0){
                  transform_str <- paste0("  transformation (",
                                          transform_parameters@method,
                                          ") with \u03BB = ",
                                          transform_parameters@lambda,
                                          ".\n")
                }
              }
            }
            
            # Normalisation parameters
            # Placeholder string
            normalisation_str <- character(0L)
            
            # Attempt to create an actual descriptor, if meaningful.
            normalisation_parameters <- object@normalisation_parameters
            if(!is.null(normalisation_parameters)){
              if(is(normalisation_parameters, "featureInfoParametersNormalisationShiftScale")){
                if(normalisation_parameters@shift != 0.0 || normalisation_parameters@scale != 1.0){
                  normalisation_str <- paste0("  normalisation (",
                                              normalisation_parameters@method,
                                              ") with shift = ",
                                              normalisation_parameters@shift,
                                              " and scale = ",
                                              normalisation_parameters@scale,
                                              ".\n")
                }
                
              } else if(is(normalisation_parameters, "featureInfoParametersNormalisationShift")){
                if(normalisation_parameters@shift != 0.0){
                  normalisation_str <- paste0("  normalisation (",
                                              normalisation_parameters@method,
                                              ") with shift = ",
                                              normalisation_parameters@shift,
                                              ".\n")
                }
              }
            }
            
            # Batch normalisation parameters
            batch_norm_str <- character(0L)
            
            # Attempt to create an actual descriptor, if meaningful.
            batch_parameters <- object@batch_normalisation_parameters
            if(!is.null(batch_parameters)){
              if(batch_parameters@method != "none"){
                
                # Iterate over normalisation parameters stored within the
                # container.
                for(normalisation_parameters in batch_parameters@batch_parameters){
                  if(is(normalisation_parameters, "featureInfoParametersNormalisationShiftScale")){
                    if(normalisation_parameters@shift != 0.0 || normalisation_parameters@scale != 1.0){
                      batch_norm_str <- c(batch_norm_str,
                                          paste0("  [",
                                                 normalisation_parameters@batch,
                                                 "] normalisation (",
                                                 normalisation_parameters@method,
                                                 ") with shift = ",
                                                 normalisation_parameters@shift,
                                                 " and scale = ",
                                                 normalisation_parameters@scale,
                                                 ".\n"))
                    }
                    
                  } else if(is(normalisation_parameters, "featureInfoParametersNormalisationShift")){
                    if(normalisation_parameters@shift != 0.0){
                      batch_norm_str <- c(batch_norm_str,
                                          paste0("  [",
                                                 normalisation_parameters@batch,
                                                 "] normalisation (",
                                                 normalisation_parameters@method,
                                                 ") with shift = ",
                                                 normalisation_parameters@shift,
                                                 ".\n"))
                    }
                  }
                }
              }
            }
              
            
            # Clustering
            # Placeholder string
            cluster_str <- character(0L)
            
            # Attempt to create an actual descriptor, if meaningful.
            if(!is(object@cluster_parameters, "featureInfoParametersCluster")){
              if(object@cluster_parameters@cluster_size > 1){
                
                # Find the feature(s) required to form the cluster.
                cluster_feature_names <- object@cluster_parameters@required_features
                
                # Find the clustering method.
                if(is(object@cluster_parameters@method, "clusterMethod")){
                  cluster_method_str <- paste0("(", object@cluster_parameters@method@method, ") ")
                  
                } else if(is(object@cluster_parameters@method, "character")){
                  cluster_method_str <- paste0("(", object@cluster_parameters@method, ") ")
                  
                } else {
                  cluster_method_str <- NULL
                }
                

                if(length(cluster_feature_names) == 1){
                  # Only one feature is required to form the cluster.
                  if(cluster_feature_names == object@name){
                    # The current feature is the reference feature.
                    cluster_str <- paste0("  forms cluster ",
                                          cluster_method_str,
                                          "with ",
                                          object@cluster_parameters@cluster_size - 1,
                                          " other features, and is the reference feature.\n")
                    
                  } else {
                    # The current feature is not the reference feature.
                    cluster_str <- paste0("  forms cluster ",
                                          cluster_method_str,
                                          "with ",
                                          object@cluster_parameters@cluster_size - 1,
                                          " other features, with ",
                                          cluster_feature_names,
                                          " as the reference feature.\n")
                  }
                  
                } else {
                  # Multiple features are required to form the cluster.
                  cluster_str <- paste0("  forms cluster ",
                                        cluster_method_str,
                                        "with ",
                                        paste_s(setdiff(cluster_feature_names, object@name)),
                                        ".\n")
                }
              }
            }
            
            # Make the feature string complete, depending on additional
            # information.
            if(length(transform_str) == 0 &
               length(normalisation_str) == 0 &
               length(batch_norm_str) == 0 &
               length(cluster_str) == 0){
              feature_str <- paste0(feature_str, ".\n")
              
            } else {
              feature_str <- paste0(feature_str, ":\n")
            }
            
            # Export to console.
            cat(feature_str)
            if(length(transform_str) > 0) cat(transform_str)
            if(length(normalisation_str) > 0) cat(normalisation_str)
            if(length(batch_norm_str) > 0) cat(batch_norm_str)
            if(length(cluster_str) > 0) cat(cluster_str)
          })



##### feature_info_complete (feature info) -------------------------------------
setMethod("feature_info_complete", signature(object="featureInfo"),
          function(object, level="clustering"){
            # Check if the feature is removed.
            if(!is_available(object)) return(TRUE)
            
            if(level=="none") return(TRUE)
            if(level=="signature") return(TRUE)
            
            if(!feature_info_complete(object@transformation_parameters)) return(FALSE)
            
            if(level=="transformation") return(TRUE)
            
            if(!feature_info_complete(object@normalisation_parameters)) return(FALSE)
            
            if(level=="normalisation") return(TRUE)
            
            if(!feature_info_complete(object@batch_normalisation_parameters)) return(FALSE)
            
            if(level=="batch_normalisation") return(TRUE)
            
            if(is.null(object@imputation_parameters)) return(FALSE)
            
            if(level=="imputation") return(TRUE)
            
            if(is.null(object@cluster_parameters)) return(FALSE)
            
            if(level=="clustering") return(TRUE)
          })


##### feature_info_complete (list) ---------------------------------------------
setMethod("feature_info_complete", signature(object="list"),
          function(object, level="clustering"){
            return(all(sapply(object, feature_info_complete, level=level)))
          })


##### feature_info_complete (NULL) ---------------------------------------------
setMethod("feature_info_complete", signature(object="NULL"),
          function(object, level="clustering"){
            # No feature information found.
            return(FALSE)
          })


##### feature_info_complete (featureInfoParameters) ----------------------------
setMethod("feature_info_complete", signature(object="featureInfoParameters"),
          function(object, ...){
            # Check the 'complete' attribute.
            return(object@complete)
          })



#####is_in_signature#####
setMethod("is_in_signature", signature(object="featureInfo"),
          function(object){
            
            return(object@in_signature)
          })


#####is_in_novelty#####
setMethod("is_in_novelty", signature(object="featureInfo"),
          function(object){
            
            return(object@in_novelty)
          })


####is_available####
setMethod("is_available", signature(object="featureInfo"),
          function(object) {

            # Checks whether a feature is marked as being available
            return(!object@removed)
          })


#####update_removed_status#####
setMethod("update_removed_status", signature(object="featureInfo"),
          function(object) {
            # Updates the "removed" slot based on other slots
            
            if(object@removed_unknown_type){
              object@removed <- TRUE
            } else if(object@in_signature){
              object@removed <- FALSE
            } else if(object@in_novelty){
              object@removed <- FALSE
            } else if(object@removed_missing_values){
              object@removed <- TRUE
            } else if(object@removed_no_variance){
              object@removed <- TRUE
            } else if(object@removed_low_variance){
              object@removed <- TRUE
            } else if(object@removed_low_robustness){
              object@removed <- TRUE
            } else if(object@removed_low_importance){
              object@removed <- TRUE
            }
            
            return(object)
          })


##### add_package_version (feature info) ---------------------------------------
setMethod("add_package_version", signature(object="featureInfo"),
          function(object){
            
            # Set version of familiar
            return(.add_package_version(object=object))
          })

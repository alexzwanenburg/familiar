add_batch_normalisation_parameters <- function(cl=NULL, feature_info_list, data_obj, settings=NULL, batch_normalisation_method=NULL){
  # Find batch normalisation parameters and add them to the feature_info_list

  # Check if the feature_info_list is empty. This may occur for empty models.
  if(is_empty(feature_info_list)){
    return(feature_info_list)
  }
  
  # Find the batch_normalisation_method.
  if(!is.null(settings) & is.null(batch_normalisation_method)){
    batch_normalisation_method <- settings$prep$batch_normalisation_method
    
  } else if(is.null(batch_normalisation_method)){
    
    # Attempt to identify the methods from feature_info_list
    batch_normalisation_parameter_lists <- extract_from_slot(object_list=feature_info_list,
                                                             slot_name="batch_normalisation_parameters")
    
    # Select unique normalisation methods.
    batch_normalisation_method <- unique(unlist(sapply(batch_normalisation_parameter_lists, function(list_entry) list_entry$norm_method)))

    # Select the unique method. Note that 
    # length(batch_normalisation_method) == 1, i.e. a single method is present,
    # is the default option.
    
    if(length(batch_normalisation_method) == 0){
      # Fallback in case no data is present.
      batch_normalisation_method <- "none"
      
    } else if (length(batch_normalisation_method) == 2){
      # Fallback in case two methods are present (i.e. when categorical and
      # numerical data are both present). In that case select the method that is
      # not "none".
      batch_normalisation_method <- setdiff(batch_normalisation_method, "none")
      
    } else if (length(batch_normalisation_method) > 2){
      ..error_reached_unreachable_code("add_batch_normalisation_parameters_more_than_2_batch_normalisation_methods_present")
    }
  }
  
  # Determine which columns contain feature data
  feature_columns <- get_feature_columns(x=data_obj)
  
  # Split features into numerical and categorical features
  numeric_features <- intersect(feature_columns, sapply(feature_info_list, function(list_entry){
    if(list_entry@feature_type=="numeric") {
      return(list_entry@name)
    } else {
      return(NULL)
    }
  }))
  categorical_features <- setdiff(feature_columns, numeric_features)
  
  # Find available batches in the data
  available_batch_ids <- unique(data_obj@data[[get_id_columns(single_column="batch")]])
  
  # Find batches already known in feature_info_list
  known_batches <- names(feature_info_list[[1]]@batch_normalisation_parameters)
  
  # Return if all batches are already known.
  unknown_batches <- setdiff(available_batch_ids, known_batches)
  if(length(unknown_batches) == 0){
    return(feature_info_list)
  }
  
  # Obtain normalisation parameters for numeric features
  if(batch_normalisation_method %in% .get_available_batch_normalisation_methods("basic")){
    updated_feature_info_list <- batch_normalise.set_basic_normalisation_parameters(cl=cl,
                                                                                    features=numeric_features,
                                                                                    feature_info_list=feature_info_list,
                                                                                    data=data_obj@data,
                                                                                    batches=unknown_batches,
                                                                                    batch_normalisation_method=batch_normalisation_method,
                                                                                    progress_bar=FALSE)
    
  } else if(batch_normalisation_method %in% .get_available_batch_normalisation_methods("combat")){
    updated_feature_info_list <- batch_normalise.set_combat_normalisation_parameters(cl=cl,
                                                                                     features=numeric_features,
                                                                                     feature_info_list=feature_info_list,
                                                                                     data=data_obj@data,
                                                                                     batches=unknown_batches,
                                                                                     batch_normalisation_method=batch_normalisation_method,
                                                                                     progress_bar=FALSE)
    
  } else{
    ..error_reached_unreachable_code("add_batch_normalisation_parameters_unknown_batch_normalisation_method")
  }
  
  # Obtain parameters for categorical features. Categorical features do not
  # undergo batch normalisation.
  updated_feature_info_list <- append(updated_feature_info_list,
                                      batch_normalise.set_basic_normalisation_parameters(cl=cl,
                                                                                         features=categorical_features,
                                                                                         feature_info_list=feature_info_list,
                                                                                         data=data_obj@data,
                                                                                         batches=unknown_batches,
                                                                                         batch_normalisation_method="none",
                                                                                         progress_bar=FALSE))
  
  # Iterate over features to resolve any unset batch-normalisation parameters,
  # e.g. due to low sample counts. Check if any normalisation parameters could
  # not be set.
  updated_feature_info_list <- batch_normalise.set_unknown_normalisation_parameters(cl=cl,
                                                                                    features=feature_columns,
                                                                                    feature_info_list=updated_feature_info_list,
                                                                                    batch_normalisation_method=batch_normalisation_method)
  
  # Update the list of featureInfo objects.
  if(length(updated_feature_info_list) > 0){
    feature_info_list[feature_columns] <- updated_feature_info_list
  }

  return(feature_info_list)
}



batch_normalise.set_basic_normalisation_parameters <- function(cl=NULL,
                                                               features,
                                                               feature_info_list,
                                                               data,
                                                               batches,
                                                               batch_normalisation_method,
                                                               progress_bar=TRUE){
  
  # Check length of features
  if(length(features) == 0){
    return(NULL)
  }
  
  # Determine batch-normalisation parameters by iterating over the features.
  updated_feature_info_list <- fam_lapply(cl=cl,
                                          assign=NULL,
                                          X=features,
                                          FUN=batch_normalise.get_normalisation_per_feature,
                                          progress_bar=progress_bar,
                                          feature_info_list=feature_info_list,
                                          data=data,
                                          batch_normalisation_method=batch_normalisation_method,
                                          batches=batches)
  
  # Set names of the updated list
  names(updated_feature_info_list) <- features

  return(updated_feature_info_list)
}



batch_normalise.get_normalisation_per_feature <- function(feature, feature_info_list, data, batch_normalisation_method, batches){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  batch_id <- NULL
  
  # Get object corresponding to the current featues
  object <- feature_info_list[[feature]]
  
  # Extract normalisation parameters for each batch.
  batch_parameter_list <- lapply(batches, function(batch_identifier, object, data, feature, batch_normalisation_method){
    
    # Extract vector of values
    x <- data[batch_id==batch_identifier, ][[feature]]
    
    # Obtain batch normalisaton for the curent feature
    batch_normalisation_parameters <- batch_normalise.get_normalisation_parameters(x=x,
                                                                                   norm_method=batch_normalisation_method)
    
    return(batch_normalisation_parameters)
  }, object=object, data=data, feature=feature, batch_normalisation_method=batch_normalisation_method)
  
  # Add names of processed batches to the batch parameter list entries.
  names(batch_parameter_list) <- batches
  
  # Add to list of existing batch normalisation parameters (if any)
  object@batch_normalisation_parameters <- append(object@batch_normalisation_parameters,
                                                  batch_parameter_list)
  
  return(object)
}


batch_normalise.set_combat_normalisation_parameters <- function(cl=NULL,
                                                                features,
                                                                feature_info_list,
                                                                data,
                                                                batches,
                                                                batch_normalisation_method,
                                                                progress_bar=TRUE){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  batch_id <- feature <- NULL
  
  # Check length of features
  if(length(features) == 0){
    return(NULL)
    
  } else if(length(features) < 3){
    # Combat using cross-feature information, which is impossible to obtain for
    # less than three features.
    return(batch_normalise.set_basic_normalisation_parameters(cl=cl,
                                                              features=features,
                                                              feature_info_list=feature_info_list,
                                                              data=data,
                                                              batches=batches,
                                                              batch_normalisation_method="standardisation",
                                                              progress_bar=progress_bar))
  }
  
  # Isolate the dataset
  x <- data.table::copy(data[batch_id %in% batches, mget(c(get_id_columns(single_column="batch"), features))])
  
  # Obtain combat batch parameters. This is a data.table
  batch_parameters <- combat.get_normalisation_parameters(x=x,
                                                          batch_normalisation_method=batch_normalisation_method,
                                                          cl=cl,
                                                          progress_bar=progress_bar)
  
  # Update feature info list with batch parameters.
  updated_feature_info_list <- lapply(features, function(current_feature, batch_parameters, feature_info_list){

    # Determine the name of the batch identifier column.
    batch_id_column <- get_id_columns(single_column="batch")
    
    # Identify the current featureInfo object from feature_info_list.
    object <- feature_info_list[[current_feature]]
    
    # Limit to batch normalisation paramaters for the current feature.
    feature_batch_parameters <- batch_parameters[feature==current_feature]
    
    # Parse the batch_parameters table to a list
    batch_parameter_list <- lapply(split(feature_batch_parameters, by=batch_id_column, sorted=FALSE), function(x){
      return(list("norm_method"=x$norm_method[1],
                  "norm_shift"=x$norm_shift[1],
                  "norm_scale"=x$norm_scale[1],
                  "n"=x$n[1]))
    })
    
    # Set names to the parameter list
    names(batch_parameter_list) <- feature_batch_parameters[[batch_id_column]]
    
    # Add to list of existing batch normalisation parameters (if any)
    object@batch_normalisation_parameters <- append(object@batch_normalisation_parameters,
                                                    batch_parameter_list)
    
    return(object)
  }, batch_parameters=batch_parameters, feature_info_list=feature_info_list)
  
  # Set names.
  names(updated_feature_info_list) <- features
  
  # Return to calling function.
  return(updated_feature_info_list)
}


batch_normalise.set_unknown_normalisation_parameters <- function(cl, features, feature_info_list,
                                                                 batch_normalisation_method){

  updated_feature_info_list <- lapply(features, function(feature, feature_info_list, batch_normalisation_method){
    # Find the featureInfo object.
    object <- feature_info_list[[feature]]
    
    # Extract the batch parameter list
    batch_parameter_list <- object@batch_normalisation_parameters
    
    # Identify the batches with unknown parameter sets.
    unknown_parameter_sets <- sapply(batch_parameter_list, function(list_entry) list_entry$norm_method=="unknown")
    
    # Update sets with unknown parameters
    if(any(unknown_parameter_sets)){
      
      # Replace parameters for replacement.
      replaced_parameter_sets <- lapply(batch_parameter_list[unknown_parameter_sets],
                                        batch_normalise.replace_unknown_parameters,
                                        known_parameters = batch_parameter_list[!unknown_parameter_sets],
                                        norm_method = batch_normalisation_method)
      
      # Set names of imputed parameter sets
      names(replaced_parameter_sets) <- names(batch_parameter_list)[unknown_parameter_sets]
      
      # Replace unknown parameter sets with the imputed parameters
      batch_parameter_list[unknown_parameter_sets] <- replaced_parameter_sets
    }
    
    # Replace current set of normalisation parameters.
    object@batch_normalisation_parameters <- batch_parameter_list
    
    return(object)
  }, feature_info_list=feature_info_list, batch_normalisation_method=batch_normalisation_method)
  
  # Set names of the updated list
  names(updated_feature_info_list) <- features
  
  return(updated_feature_info_list)
}


batch_normalise.get_normalisation_parameters <- function(x, norm_method){

  if(norm_method %in% .get_available_batch_normalisation_methods("basic")){
    
    # x is expected to be a vector of values.
    # It does not make a lot of sense to determine normalisation parameters from
    # less than 5 values.
    if(sum(!is.na(x)) < 5 & norm_method != "none"){
      # Set placeholder parameters.
      normalisation_parameters <- list("norm_method"="unknown", "norm_shift"=NA_real_, "norm_scale"=NA_real_, "n"=sum(!is.na(x)))
      
    } else {
      # Set normalisation parameters
      normalisation_parameters <- normalise.get_normalisation_parameters(x=x, norm_method=norm_method)
      
      # Add the number of values used to compute parameters
      normalisation_parameters$n <- length(x)
    }
    
  } else if(norm_method %in% .get_available_batch_normalisation_methods("combat")){
    # x is expected to be a matrix of values.
    normalisation_parameters <- combat.get_normalisation_parameters(x=x, batch_normalisation_method=norm_method)
    
  } else {
    ..error_reached_unreachable_code("batch_normalise.get_normalisation_parameters_unknown_batch_normalisation_method")
  }
  
  return(normalisation_parameters)
}


batch_normalise.replace_unknown_parameters <- function(parameter_set, known_parameters, norm_method){

  # Check if there are any known parameters that can be used for imputation.
  if(length(known_parameters) == 0){
    # Return none in case there is no data that can be used for imputation.
    return(list("norm_method"="none", "norm_shift"=0, "norm_scale"=1))
  }
  
  # Procedure for other standardardisation methods
  if(norm_method %in% .get_available_batch_normalisation_methods("all")){
    
    # Determine the total number of samples underlying the known parameter sets.
    n_total <- sum(sapply(known_parameters, function(list_entry) list_entry$n))
    
    # Derive shift and scale parameters.
    norm_shift <- sum(sapply(known_parameters, function(list_entry) list_entry$norm_shift * list_entry$n)) / n_total
    norm_scale <- sum(sapply(known_parameters, function(list_entry) list_entry$norm_scale * list_entry$n)) / n_total
    
    return(list("norm_method"=norm_method, "norm_shift"=norm_shift, "norm_scale"=norm_scale, "n"=parameter_set$n[1]))
    
  } else {
    ..error_reached_unreachable_code("batch_normalise.replace_unknown_parameters_unknown_batch_normalisation_method")
  }
}


batch_normalise.apply_normalisation <- function(x, feature_info, invert=FALSE){
  # Applies normalisation parameters to input data
  
  # Add another identifier to make sure that everything is correctly ordered.
  x <- data.table::copy(x)
  x[, "order_index_id":=.I]
  
  # Iterate over batches to determine the transformed data.
  y <- lapply(split(x, by=get_id_columns(id_depth="batch"), sorted=FALSE, keep.by=TRUE),
              function(x, feature_info, invert){
                
                # Identify the id of the current batch
                current_batch_id <- x[[get_id_columns(single_column="batch")]][1]
                
                # Find the batch-normalisation parameters
                norm_param <- feature_info@batch_normalisation_parameters[[as.character(current_batch_id)]]
                
                # Determine the normalisation method
                norm_method <- norm_param$norm_method[1]
                
                if(norm_method %in% .get_available_batch_normalisation_methods("basic")){
                  y <- normalise.apply_normalisation(x = x[[feature_info@name]],
                                                     norm_param = norm_param,
                                                     invert = invert)
                  
                } else if(norm_method %in% .get_available_batch_normalisation_methods("combat")){
                  y <- combat.apply_normalisation(x = x[[feature_info@name]],
                                                  norm_param = norm_param,
                                                  invert = invert)
                  
                } else {
                  ..error_reached_unreachable_code("batch_normalise.apply_normalisation_unknwon_batch_normalisation_method")
                }
              
                # Return y
                return(data.table::data.table("order_index_id"=x$order_index_id,
                                              "y"=y))
                
              }, feature_info=feature_info, invert=invert)

  # Bind to single table
  y <- data.table::rbindlist(y, use.names=TRUE)
  
  # Merge with input x, while making sure that the order remains the same.
  y <- merge(x=x, y=y, by=c("order_index_id"), sort=FALSE)
  
  # Return transformed values
  return(y$y)
}



.get_available_batch_normalisation_methods <- function(type="all"){
  available_methods <- NULL
  
  if(type %in% c("basic", "all")){
    available_methods <- c(available_methods, .get_available_normalisation_methods())
  }
  
  if(type %in% c("combat", "all")){
    available_methods <- c(available_methods, "combat", "combat_p",
                           "combat_parametric", "combat_np", "combat_non_parametric")
  }
  
  return(available_methods)
}

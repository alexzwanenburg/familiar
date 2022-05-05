#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL



setClass("featureInfoParametersBatchNormalisationContainer",
         contains="featureInfoParameters",
         slots=list("method" = "character",
                    "batch_parameters" = "ANY",
                    "type" = "character",
                    "available" = "logical"),
         prototype=list("method" = NA_character_,
                        "batch_parameters" = NULL,
                        "type"=NA_character_,
                        "available"=NA))


.get_available_batch_normalisation_methods <- function(type="all"){
  
  # Find all basic normalisation.
  available_methods <- .get_available_normalisation_methods(type=type)
  
  if(type %in% c("combat", "combat_non_parametric", "all")){
    available_methods <- c(available_methods,
                           .get_available_combat_parametric_normalisation_methods())
  }
  
  if(type %in% c("combat", "combat_parametric", "all")){
    available_methods <- c(available_methods,
                           .get_available_combat_non_parametric_normalisation_methods())
  }
  
  return(available_methods)
}



create_batch_normalisation_parameter_skeleton <- function(feature_info_list,
                                                          feature_names=NULL,
                                                          normalisation_method,
                                                          .override_existing=FALSE){
  
  # Creates a skeleton for the provided batch normalisation method.
  
  # Determine feature names from the feature info list, if provided.
  if(is.null(feature_names)) feature_names <- names(feature_info_list)
  
  # Select only features that appear in the feature info list.
  feature_names <- intersect(names(feature_info_list),
                             feature_names)
  
  # Skip step if no feature info objects are updated.
  if(is_empty(feature_names)) return(feature_info_list)
  
  # Check that method is applicable.
  .check_parameter_value_is_valid(x=normalisation_method,
                                  var_name="normalisation_method",
                                  values=.get_available_batch_normalisation_methods())
  
  # Update familiar info objects with a feature normalisation skeleton.
  updated_feature_info <- fam_lapply(X=feature_info_list[feature_names],
                                     FUN=.create_batch_normalisation_parameter_skeleton,
                                     method=normalisation_method,
                                     .override_existing=.override_existing)
  
  # Provide names for the updated feature info objects.
  names(updated_feature_info) <- feature_names
  
  # Replace list elements.
  feature_info_list[feature_names] <- updated_feature_info
  
  return(feature_info_list)
}



.create_batch_normalisation_parameter_skeleton <- function(feature_info,
                                                           method,
                                                           .override_existing=FALSE){
  
  # Check if normalisation data was already completed, and does not require
  # being determined anew.
  if(feature_info_complete(feature_info@batch_normalisation_parameters) & !.override_existing) return(feature_info)
  
  # Pass to underlying function that constructs the skeleton.
  object <- ..create_batch_normalisation_parameter_skeleton(feature_name=feature_info@name,
                                                            feature_type=feature_info@feature_type,
                                                            method=method,
                                                            available=is_available(feature_info))
  
  # Update normalisation_parameters slot.
  feature_info@batch_normalisation_parameters <- object
  
  return(feature_info)
}



..create_batch_normalisation_parameter_skeleton <- function(method,
                                                            feature_name,
                                                            feature_type="numeric",
                                                            available=TRUE){
  # This is the lowest level function for creating batch normalisation parameter
  # skeletons.
  
  # If the feature type is not numeric, or the feature is not available, change
  # the main method to "none".
  if(feature_type != "numeric" || !available) method <- "none"
  
  # Generate container.
  object <- methods::new("featureInfoParametersBatchNormalisationContainer",
                         "method"=method,
                         "type"=feature_type,
                         "available"=available)
  
  # Set the name of the object.
  object@name <- feature_name
  
  # Update the familiar version.
  object <- add_package_version(object=object)
  
  return(object)
}



add_batch_normalisation_parameters <- function(cl=NULL,
                                               feature_info_list,
                                               data,
                                               verbose=FALSE){
  # Determine normalisation parameters and add them to the feature_info_list.
  
  # Find feature columns.
  feature_names <- get_feature_columns(x=data)
  
  # Sanity check.
  if(!(setequal(feature_names, get_available_features(feature_info_list=feature_info_list)))){
    ..error_reached_unreachable_code("add_transformation_parameters: features in data and the feature info list are expect to be the same, but were not.")
  }
  
  # Identify methods specified by the containers for different features.
  container_batch_methods <- sapply(feature_info_list[feature_names],
                                    function(x) (x@batch_normalisation_parameters@method))
  
  # Iterate over methods. The names of features that are assessed using the same
  # batch normalisation method are dispatched to ensure computation within that
  # feature set only.
  for(container_batch_method in unique(container_batch_methods)){
    
    # Find feature names.
    batch_method_feature_names <- feature_names[container_batch_methods == container_batch_method]
    
    # Check if a ComBat method is used, and pre-compute the z-matrix, if it is.
    if(container_batch_method %in% .get_available_batch_normalisation_methods(type="combat")){
      
      # Pre-compute z-matrix. Otherwise we need to compute for each feature,
      # over and over again.
      z <- .compute_combat_batch_normalisation_z_matrix(data=data@data,
                                                        feature_names=batch_method_feature_names)
      
      if(container_batch_method %in% .get_available_batch_normalisation_methods(type="combat_parametric")){
        # Obtain ComBat data using the parametric solver.
        batch_parameter_data <- .combat_iterative_parametric_bayes_solver(z=z,
                                                                          cl=cl,
                                                                          progress_bar=FALSE)
        
      } else {
        # Obtain ComBat data using the non-parametric solver.
        batch_parameter_data <- .combat_iterative_parametric_bayes_solver(z=z,
                                                                          cl=cl,
                                                                          progress_bar=FALSE)
      }
      
    } else {
      batch_parameter_data <- NULL
    }
    
    # Iterate over features.
    updated_feature_info <- fam_mapply(cl=cl,
                                       FUN=.add_batch_normalisation_parameters,
                                       feature_info=feature_info_list[batch_method_feature_names],
                                       data=data@data[, mget(batch_method_feature_names)],
                                       MoreArgs=list("batch_column_data"= data@data[[get_id_columns("batch", single_column=TRUE)]],
                                                     "batch_parameter_data"=batch_parameter_data),
                                       progress_bar=verbose,
                                       chopchop=TRUE)

    
    # Provide names for the updated feature info objects.
    names(updated_feature_info) <- batch_method_feature_names
    
    # Replace list elements.
    feature_info_list[batch_method_feature_names] <- updated_feature_info
  }
  
  
  return(feature_info_list)
}



.add_batch_normalisation_parameters <- function(feature_info,
                                                data,
                                                batch_column_data,
                                                batch_parameter_data=NULL){
  
  # Combine data and batch column data into a data.table again. The primary
  # reason for splitting is to prevent dispatching the full dataset to each
  # node.
  data <- data.table::data.table("batch_column_id"=batch_column_data, "feature_value"=data)
  
  # Rename columns to original names.
  data.table::setnames(data,
                       old=c("batch_column_id", "feature_value"),
                       new=c(get_id_columns("batch", single_column=TRUE),
                             feature_info@batch_normalisation_parameters@name))
  
  # Pass to underlying function that adds the feature info.
  object <- add_feature_info_parameters(object=feature_info@batch_normalisation_parameters,
                                        data=data,
                                        batch_parameter_data=batch_parameter_data)
  
  # Update normalisation_parameters slot.
  feature_info@batch_normalisation_parameters <- object
  
  return(feature_info)
}



#### add_feature_info_parameters (container, data object) ----------------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersBatchNormalisationContainer", data="dataObject"),
          function(object,
                   data,
                   ...){
            # Pass to method with signature data=data.table.
            return(add_feature_info_parameters(object=object,
                                               data=data@data,
                                               ...))
          })


#### add_feature_info_parameters (container, data.table) -----------------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersBatchNormalisationContainer", data="data.table"),
          function(object,
                   data,
                   ...){
            
            #### Populate container with batch parameter objects ---------------
            
            # Find the batch identifiers in the data.
            batch_ids <- unique(data[[get_id_columns(id_depth="batch")]])
            
            # Determine which normalisation objects already exist.
            visited_batch <- names(object@batch_parameters)
            
            # Remove batches that were already created.
            batch <- setdiff(batch_ids, visited_batch)
            
            if(length(batch) > 0){
              # Create batch objects.
              normalisation_objects <- fam_mapply(FUN=..create_normalisation_parameter_skeleton,
                                                  batch=batch,
                                                  MoreArgs=list("feature_name"=object@name,
                                                                "feature_type"=object@type,
                                                                "available"=object@available,
                                                                "method"=object@method))
              
              # Set names.
              names(normalisation_objects) <- batch
              
              # Append to the batch parameters.
              object@batch_parameters <- c(object@batch_parameters,
                                           normalisation_objects)
            }
            
            #### Determine batch parameters ------------------------------------
            
            # Select normalisation objects that have not been completed. 
            batch_to_update <- intersect(names(object@batch_parameters)[!sapply(object@batch_parameters, feature_info_complete)],
                                         batch_ids)
            
            if(length(batch_to_update) > 0){
              # Determine normalisation parameters within each batch.
              normalisation_objects <- fam_mapply(FUN=add_feature_info_parameters,
                                                  object=object@batch_parameters[batch_to_update],
                                                  data=split(data, by=get_id_columns(id_depth="batch"))[batch_to_update],
                                                  MoreArgs=c(list(...)))
              
              # Set names
              names(normalisation_objects) <- batch_to_update
              
              # Update batch parameters
              object@batch_parameters[batch_to_update] <- normalisation_objects
            }
            
            #### Check new none-class parameter sets ---------------------------
            
            # Select parameter sets that are a) newly created, and b) were
            # updated. If these objects are none, for various reasons such as
            # small batches, attempt to replace them by an average object.
            batch_to_check <- intersect(batch, batch_to_update)
            
            if(length(batch_to_check) > 0 && object@method != "none"){
              # Generate a new object.
              replacement_normalisation_object <- ...collect_and_aggregate_normalisation_info(object_list=object@batch_parameters,
                                                                                              instance_mask=rep_len(TRUE, length(object@batch_parameters)),
                                                                                              feature_name=object@name)$parameters
              
              # Identify which new objects are none objects.
              batch_to_check <- batch_to_check[sapply(object@batch_parameters[batch_to_check], is, class2="featureInfoParametersNormalisationNone")]
              
              if(length(batch_to_check) > 0){
                
                # Add batch attribute to normalisation objects.
                normalisation_objects <- lapply(batch_to_check, function(x, object){
                  
                  # Add batch name
                  object@batch <- x
                  
                  return(object)
                },
                object=replacement_normalisation_object)
                
                # Update names.
                names(normalisation_objects) <- batch_to_check
                
                # Replace these objects.
                object@batch_parameters[batch_to_check] <- normalisation_objects
              }
            }
            
            # Check that all objects are complete.
            if(!all(sapply(object@batch_parameters, feature_info_complete))){
              ..error_reached_unreachable_code(paste0("add_feature_info_parameters,featureInfoParametersBatchNormalisationContainer, data.table: ",
                                                      "all normalisation objects in the container should be complete, but some were not."))
            }
            
            # Mark complete.
            object@complete <- TRUE
            
            return(object)
          })


#### apply_feature_info_parameters (container, ANY) ----------------------------
setMethod("apply_feature_info_parameters", signature(object="featureInfoParametersBatchNormalisationContainer", "data"="ANY"),
          function(object,
                   data,
                   ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            batch_normalisation_ordering_id <- NULL
            
            # Find the batch identifiers in the data.
            batch_ids <- unique(data[[get_id_columns(id_depth="batch")]])
            
            # Sanity check: batch normalisation objects are available for all
            # batch identifiers.
            if(!all(batch_ids %in% names(object@batch_parameters))){
              ..error_reached_unreachable_code(paste0("apply_feature_info_parameters,featureInfoParametersBatchNormalisationContainer,ANY: ",
                                                      "Batch normalisation objects are missing for one or more batches."))
            }
            
            # Sanity check: all relevant batch normalisation objects are
            # complete.
            if(!all(sapply(object@batch_parameters[batch_ids], feature_info_complete))){
              ..error_reached_unreachable_code(paste0("apply_feature_info_parameters,featureInfoParametersBatchNormalisationContainer,ANY: ",
                                                      "One or more batch normalisation objects are incomplete and may be missing parameters."))
            }
            
            # Avoid updating by reference.
            data <- data.table::copy(data)
            
            # Insert ordering variable.
            data[, "batch_normalisation_ordering_id":=.I]
            
            # Transform per batch.
            data <- fam_mapply(FUN=apply_feature_info_parameters,
                               object=object@batch_parameters[batch_ids],
                               data=split(data, by=get_id_columns(id_depth="batch"))[batch_ids],
                               MoreArgs=list(...))
            
            # Combine into a single data.table.
            data <- data.table::rbindlist(data, use.names=TRUE)
            
            # Order by ordering variable to maintain original order.
            data <- data[order(batch_normalisation_ordering_id)]
            
            # Extract the feature values and return.
            return(data[[object@name]])
          })



..collect_and_aggregate_batch_normalisation_info <- function(feature_info_list, instance_mask, feature_name){
  # Aggregate batch normalisation parameters. This function exists so that it
  # can be tested as part of a unit test.
  
  # Find all batches in the parameters.
  batch_ids <- unique(unlist(lapply(feature_info_list, function(x){
    
    # Return NULL if parameters are missing.
    if(is.null(x@batch_normalisation_parameters)) return(NULL)
    
    return(names(x@batch_normalisation_parameters@batch_parameters))
  })))
  
  # Main batch method
  main_batch_method <- unique(unlist(lapply(feature_info_list, function(x){
    # Return NULL if parameters are missing.
    if(is.null(x@batch_normalisation_parameters)) return(NULL)
    
    return(x@batch_normalisation_parameters@method)
  })))
  
  # Create parameter container
  batch_parameter_container <- ..create_batch_normalisation_parameter_skeleton(method=main_batch_method[1],
                                                                               feature_name=feature_name)
  
  if(!any(instance_mask)){
    # Create placeholder objects.
    batch_normalisation_objects <- lapply(batch_ids, function(batch, feature_name){
      ..create_normalisation_parameter_skeleton(feature_name=feature_name,
                                                method="none",
                                                batch=batch)
    },
    feature_name=feature_name)
    
    # Add names.
    names(batch_normalisation_objects) <- batch_ids
    
    # Add to container.
    batch_parameter_container@batch_parameters <- batch_normalisation_objects
    
    return(list("parameters"=batch_parameter_container,
                "instance_mask"=instance_mask))
  }
  
  # Initialise list of objects.
  batch_normalision_object_list <- list()
  
  # Iterate over batch identifiers and collect all batch identifiers.
  for(batch_id in batch_ids){
    
    # Collect batch normalisation objects.
    batch_normalisation_objects <- lapply(feature_info_list[instance_mask], function(x, batch_id){
      # Return NULL if parameters are missing.
      if(is.null(x@batch_normalisation_parameters)) return(NULL)
      
      # Return parameters for the specific batch.
      return(x@batch_normalisation_parameters@batch_parameters[[batch_id]])
    },
    batch_id=batch_id)
    
    # Collect aggregate object.
    batch_normalision_object_list[[batch_id]] <- ...collect_and_aggregate_normalisation_info(object_list=batch_normalisation_objects,
                                                                                             instance_mask=rep_len(TRUE, length(batch_normalisation_objects)),
                                                                                             feature_name=feature_name)$parameters
    
    # Set batch name.
    batch_normalision_object_list[[batch_id]]@batch <- batch_id
  }
  
  # Add batch objects to container.
  batch_parameter_container@batch_parameters <- batch_normalision_object_list
  
  return(list("parameters"=batch_parameter_container,
              "instance_mask"=instance_mask))
}



# 
# add_batch_normalisation_parameters <- function(cl=NULL, feature_info_list, data_obj, settings=NULL, batch_normalisation_method=NULL, progress_bar=FALSE){
#   # Find batch normalisation parameters and add them to the feature_info_list
# 
#   # Check if the feature_info_list is empty. This may occur for empty models.
#   if(is_empty(feature_info_list)){
#     return(feature_info_list)
#   }
#   
#   # Find the batch_normalisation_method.
#   if(!is.null(settings) & is.null(batch_normalisation_method)){
#     batch_normalisation_method <- settings$prep$batch_normalisation_method
#     
#   } else if(is.null(batch_normalisation_method)){
#     
#     # Attempt to identify the methods from feature_info_list
#     batch_normalisation_parameter_lists <- extract_from_slot(object_list=feature_info_list,
#                                                              slot_name="batch_normalisation_parameters")
#     
#     # Select unique normalisation methods.
#     batch_normalisation_method <- unique(unlist(sapply(batch_normalisation_parameter_lists, function(list_entry) list_entry$norm_method)))
# 
#     # Select the unique method. Note that 
#     # length(batch_normalisation_method) == 1, i.e. a single method is present,
#     # is the default option.
#     
#     if(length(batch_normalisation_method) == 0){
#       # Fallback in case no data is present.
#       batch_normalisation_method <- "none"
#       
#     } else if (length(batch_normalisation_method) == 2){
#       # Fallback in case two methods are present (i.e. when categorical and
#       # numerical data are both present). In that case select the method that is
#       # not "none".
#       batch_normalisation_method <- setdiff(batch_normalisation_method, "none")
#       
#     } else if (length(batch_normalisation_method) > 2){
#       ..error_reached_unreachable_code("add_batch_normalisation_parameters_more_than_2_batch_normalisation_methods_present")
#     }
#   }
#   
#   # Find available batches in the data
#   available_batch_ids <- unique(data_obj@data[[get_id_columns(single_column="batch")]])
#   
#   # Find batches already known in feature_info_list
#   known_batches <- names(feature_info_list[[1]]@batch_normalisation_parameters)
#   
#   # Return if all batches are already known.
#   unknown_batches <- setdiff(available_batch_ids, known_batches)
#   if(length(unknown_batches) == 0) return(feature_info_list)
#   
#   # Determine which columns contain feature data
#   feature_columns <- get_feature_columns(x=data_obj)
#   
#   # Split features into numeric and categorical features.
#   numeric_features <- intersect(names(feature_info_list)[sapply(feature_info_list, function(list_entry) (list_entry@feature_type == "numeric"))],
#                                 feature_columns)
#   categorical_features <- setdiff(feature_columns, numeric_features)
#   
#   # Obtain normalisation parameters for numeric features
#   if(batch_normalisation_method %in% .get_available_batch_normalisation_methods("basic")){
#     updated_feature_info_list <- batch_normalise.set_basic_normalisation_parameters(cl=cl,
#                                                                                     features=numeric_features,
#                                                                                     feature_info_list=feature_info_list,
#                                                                                     data=data_obj@data,
#                                                                                     batches=unknown_batches,
#                                                                                     batch_normalisation_method=batch_normalisation_method,
#                                                                                     progress_bar=progress_bar)
#     
#   } else if(batch_normalisation_method %in% .get_available_batch_normalisation_methods("combat")){
#     updated_feature_info_list <- batch_normalise.set_combat_normalisation_parameters(cl=cl,
#                                                                                      features=numeric_features,
#                                                                                      feature_info_list=feature_info_list,
#                                                                                      data=data_obj@data,
#                                                                                      batches=unknown_batches,
#                                                                                      batch_normalisation_method=batch_normalisation_method,
#                                                                                      progress_bar=progress_bar)
#     
#   } else{
#     ..error_reached_unreachable_code("add_batch_normalisation_parameters_unknown_batch_normalisation_method")
#   }
#   
#   # Obtain parameters for categorical features. Categorical features do not
#   # undergo batch normalisation.
#   updated_feature_info_list <- append(updated_feature_info_list,
#                                       batch_normalise.set_basic_normalisation_parameters(cl=cl,
#                                                                                          features=categorical_features,
#                                                                                          feature_info_list=feature_info_list,
#                                                                                          data=data_obj@data,
#                                                                                          batches=unknown_batches,
#                                                                                          batch_normalisation_method="none",
#                                                                                          progress_bar=progress_bar))
#   
#   # Iterate over features to resolve any unset batch-normalisation parameters,
#   # e.g. due to low sample counts. Check if any normalisation parameters could
#   # not be set.
#   updated_feature_info_list <- batch_normalise.set_unknown_normalisation_parameters(cl=cl,
#                                                                                     features=feature_columns,
#                                                                                     feature_info_list=updated_feature_info_list,
#                                                                                     batch_normalisation_method=batch_normalisation_method)
#   
#   # Update the list of featureInfo objects.
#   if(length(updated_feature_info_list) > 0){
#     feature_info_list[feature_columns] <- updated_feature_info_list
#   }
# 
#   return(feature_info_list)
# }


# 
# batch_normalise.set_basic_normalisation_parameters <- function(cl=NULL,
#                                                                features,
#                                                                feature_info_list,
#                                                                data,
#                                                                batches,
#                                                                batch_normalisation_method,
#                                                                progress_bar=TRUE){
#   
#   # Check length of features
#   if(length(features) == 0){
#     return(NULL)
#   }
#   
#   # Determine batch-normalisation parameters by iterating over the features.
#   updated_feature_info_list <- fam_lapply(cl=cl,
#                                           assign=NULL,
#                                           X=features,
#                                           FUN=batch_normalise.get_normalisation_per_feature,
#                                           progress_bar=progress_bar,
#                                           feature_info_list=feature_info_list,
#                                           data=data,
#                                           batch_normalisation_method=batch_normalisation_method,
#                                           batches=batches,
#                                           chopchop=TRUE)
#   
#   # Set names of the updated list
#   names(updated_feature_info_list) <- features
# 
#   return(updated_feature_info_list)
# }



# batch_normalise.get_normalisation_per_feature <- function(feature, feature_info_list, data, batch_normalisation_method, batches){
#   
#   # Suppress NOTES due to non-standard evaluation in data.table
#   batch_id <- NULL
#   
#   # Get object corresponding to the current featues
#   object <- feature_info_list[[feature]]
#   
#   # Extract normalisation parameters for each batch.
#   batch_parameter_list <- lapply(batches, function(batch_identifier, object, data, feature, batch_normalisation_method){
#     
#     # Extract vector of values
#     x <- data[batch_id==batch_identifier, ][[feature]]
#     
#     # Obtain batch normalisaton for the curent feature
#     batch_normalisation_parameters <- batch_normalise.get_normalisation_parameters(x=x,
#                                                                                    norm_method=batch_normalisation_method)
#     
#     return(batch_normalisation_parameters)
#   }, object=object, data=data, feature=feature, batch_normalisation_method=batch_normalisation_method)
#   
#   # Add names of processed batches to the batch parameter list entries.
#   names(batch_parameter_list) <- batches
#   
#   # Add to list of existing batch normalisation parameters (if any)
#   object@batch_normalisation_parameters <- c(object@batch_normalisation_parameters,
#                                              batch_parameter_list)
#   
#   return(object)
# }


# 
# batch_normalise.set_combat_normalisation_parameters <- function(cl=NULL,
#                                                                 features,
#                                                                 feature_info_list,
#                                                                 data,
#                                                                 batches,
#                                                                 batch_normalisation_method,
#                                                                 progress_bar=TRUE){
#   
#   # Suppress NOTES due to non-standard evaluation in data.table
#   batch_id <- feature <- NULL
#   
#   # Check length of features
#   if(length(features) == 0) return(NULL)
#     
#   if(length(features) < 3){
#     # Combat using cross-feature information, which is impossible to obtain for
#     # less than three features.
#     return(batch_normalise.set_basic_normalisation_parameters(cl=cl,
#                                                               features=features,
#                                                               feature_info_list=feature_info_list,
#                                                               data=data,
#                                                               batches=batches,
#                                                               batch_normalisation_method="standardisation",
#                                                               progress_bar=progress_bar))
#   }
#   
#   # Isolate the dataset
#   x <- data.table::copy(data[batch_id %in% batches, mget(c(get_id_columns(single_column="batch"), features))])
#   
#   # Obtain combat batch parameters. This is a data.table
#   batch_parameters <- combat.get_normalisation_parameters(x=x,
#                                                           batch_normalisation_method=batch_normalisation_method,
#                                                           cl=cl,
#                                                           progress_bar=progress_bar)
#   
#   # Update feature info list with batch parameters.
#   updated_feature_info_list <- lapply(features, function(current_feature, batch_parameters, feature_info_list){
# 
#     # Determine the name of the batch identifier column.
#     batch_id_column <- get_id_columns(single_column="batch")
#     
#     # Identify the current featureInfo object from feature_info_list.
#     object <- feature_info_list[[current_feature]]
#     
#     # Limit to batch normalisation paramaters for the current feature.
#     feature_batch_parameters <- batch_parameters[feature==current_feature]
#     
#     # Parse the batch_parameters table to a list
#     batch_parameter_list <- lapply(split(feature_batch_parameters, by=batch_id_column, sorted=FALSE), function(x){
#       return(list("norm_method"=x$norm_method[1],
#                   "norm_shift"=x$norm_shift[1],
#                   "norm_scale"=x$norm_scale[1],
#                   "n"=x$n[1]))
#     })
#     
#     # Set names to the parameter list
#     names(batch_parameter_list) <- feature_batch_parameters[[batch_id_column]]
#     
#     # Add to list of existing batch normalisation parameters (if any)
#     object@batch_normalisation_parameters <- append(object@batch_normalisation_parameters,
#                                                     batch_parameter_list)
#     
#     return(object)
#   }, batch_parameters=batch_parameters, feature_info_list=feature_info_list)
#   
#   # Set names.
#   names(updated_feature_info_list) <- features
#   
#   # Return to calling function.
#   return(updated_feature_info_list)
# }


# batch_normalise.set_unknown_normalisation_parameters <- function(cl, features, feature_info_list,
#                                                                  batch_normalisation_method){
# 
#   updated_feature_info_list <- lapply(features, function(feature, feature_info_list, batch_normalisation_method){
#     # Find the featureInfo object.
#     object <- feature_info_list[[feature]]
#     
#     # Extract the batch parameter list
#     batch_parameter_list <- object@batch_normalisation_parameters
#     
#     # Identify the batches with unknown parameter sets.
#     unknown_parameter_sets <- sapply(batch_parameter_list, function(list_entry) list_entry$norm_method=="unknown")
#     
#     # Update sets with unknown parameters
#     if(any(unknown_parameter_sets)){
#       
#       # Replace parameters for replacement.
#       replaced_parameter_sets <- lapply(batch_parameter_list[unknown_parameter_sets],
#                                         batch_normalise.replace_unknown_parameters,
#                                         known_parameters = batch_parameter_list[!unknown_parameter_sets],
#                                         norm_method = batch_normalisation_method)
#       
#       # Set names of imputed parameter sets
#       names(replaced_parameter_sets) <- names(batch_parameter_list)[unknown_parameter_sets]
#       
#       # Replace unknown parameter sets with the imputed parameters
#       batch_parameter_list[unknown_parameter_sets] <- replaced_parameter_sets
#     }
#     
#     # Replace current set of normalisation parameters.
#     object@batch_normalisation_parameters <- batch_parameter_list
#     
#     return(object)
#   }, feature_info_list=feature_info_list, batch_normalisation_method=batch_normalisation_method)
#   
#   # Set names of the updated list
#   names(updated_feature_info_list) <- features
#   
#   return(updated_feature_info_list)
# }
# 
# 
# batch_normalise.get_normalisation_parameters <- function(x, norm_method){
# 
#   if(norm_method %in% .get_available_batch_normalisation_methods("basic")){
#     
#     # x is expected to be a vector of values.
#     # It does not make a lot of sense to determine normalisation parameters from
#     # less than 5 values.
#     if(sum(!is.na(x)) < 5 & norm_method != "none"){
#       # Set placeholder parameters.
#       normalisation_parameters <- list("norm_method"="unknown", "norm_shift"=NA_real_, "norm_scale"=NA_real_, "n"=sum(!is.na(x)))
#       
#     } else {
#       # Set normalisation parameters
#       normalisation_parameters <- normalise.get_normalisation_parameters(x=x, norm_method=norm_method)
#       
#       # Add the number of values used to compute parameters
#       normalisation_parameters$n <- length(x)
#     }
#     
#   } else if(norm_method %in% .get_available_batch_normalisation_methods("combat")){
#     # x is expected to be a matrix of values.
#     normalisation_parameters <- combat.get_normalisation_parameters(x=x, batch_normalisation_method=norm_method)
#     
#   } else {
#     ..error_reached_unreachable_code("batch_normalise.get_normalisation_parameters_unknown_batch_normalisation_method")
#   }
#   
#   return(normalisation_parameters)
# }
# 
# 
# batch_normalise.replace_unknown_parameters <- function(parameter_set, known_parameters, norm_method){
# 
#   # Check if there are any known parameters that can be used for imputation.
#   if(length(known_parameters) == 0){
#     # Return none in case there is no data that can be used for imputation.
#     return(list("norm_method"="none", "norm_shift"=NA_real_, "norm_scale"=NA_real_))
#   }
#   
#   # Procedure for other standardardisation methods
#   if(norm_method %in% .get_available_batch_normalisation_methods("all")){
#     
#     # Determine the total number of samples underlying the known parameter sets.
#     n_total <- sum(sapply(known_parameters, function(list_entry) list_entry$n))
#     
#     # Derive shift and scale parameters.
#     norm_shift <- sum(sapply(known_parameters, function(list_entry) list_entry$norm_shift * list_entry$n)) / n_total
#     norm_scale <- sum(sapply(known_parameters, function(list_entry) list_entry$norm_scale * list_entry$n)) / n_total
#     
#     return(list("norm_method"=norm_method, "norm_shift"=norm_shift, "norm_scale"=norm_scale, "n"=parameter_set$n[1]))
#     
#   } else {
#     ..error_reached_unreachable_code("batch_normalise.replace_unknown_parameters_unknown_batch_normalisation_method")
#   }
# }

# 
# batch_normalise.apply_normalisation <- function(x, feature_info, invert=FALSE){
#   # Applies normalisation parameters to input data
#   
#   # Add another identifier to make sure that everything is correctly ordered.
#   x <- data.table::copy(x)
#   x[, "order_index_id":=.I]
#   
#   # Iterate over batches to determine the transformed data.
#   y <- lapply(split(x, by=get_id_columns(id_depth="batch"), sorted=FALSE, keep.by=TRUE),
#               function(x, feature_info, invert){
#                 
#                 # Identify the id of the current batch
#                 current_batch_id <- x[[get_id_columns(single_column="batch")]][1]
#                 
#                 # Find the batch-normalisation parameters
#                 norm_param <- feature_info@batch_normalisation_parameters[[as.character(current_batch_id)]]
#                 
#                 # Determine the normalisation method
#                 norm_method <- norm_param$norm_method[1]
#                 
#                 if(norm_method %in% .get_available_batch_normalisation_methods("all")){
#                   y <- normalise.apply_normalisation(x = x[[feature_info@name]],
#                                                      norm_param = norm_param,
#                                                      invert = invert)
# 
#                 } else {
#                   ..error_reached_unreachable_code(paste0("batch_normalise.apply_normalisation: encountered an unknown batch normalisation method: ", norm_method))
#                 }
#               
#                 # Return y
#                 return(data.table::data.table("order_index_id"=x$order_index_id,
#                                               "y"=y))
#                 
#               }, feature_info=feature_info, invert=invert)
# 
#   # Bind to single table
#   y <- data.table::rbindlist(y, use.names=TRUE)
#   
#   # Merge with input x, while making sure that the order remains the same.
#   y <- merge(x=x, y=y, by=c("order_index_id"), sort=FALSE)
#   
#   # Return transformed values
#   return(y$y)
# }

# 
# 
# .get_available_batch_normalisation_methods <- function(type="all"){
#   available_methods <- NULL
#   
#   if(type %in% c("basic", "all")){
#     available_methods <- c(available_methods, .get_available_normalisation_methods())
#   }
#   
#   if(type %in% c("combat", "all")){
#     available_methods <- c(available_methods, "combat", "combat_p",
#                            "combat_parametric", "combat_np", "combat_non_parametric")
#   }
#   
#   return(available_methods)
# }

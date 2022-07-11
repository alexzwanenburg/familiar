#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

#####is_available#####
setMethod("is_available", signature(object="familiarVimpMethod"),
          function(object, ...) return(FALSE))


#####require_package (vimp method)#####
setMethod("require_package", signature(x="familiarVimpMethod"),
          function(x, purpose="vimp", message_type="error", ...){
            
            # Skip if no package is required.
            if(is_empty(x@package)) return(invisible(TRUE))
            
            # Set standard purposes for common uses.
            if(!is.null(purpose)){
              if(purpose %in% c("vimp")){
                purpose <- switch(purpose,
                                  "vimp"="to determine variable importance")
              }
            }
            
            return(invisible(.require_package(x=x@package, purpose=purpose, message_type=message_type)))
          })


#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarVimpMethod"),
          function(object, ...) return(list()))


#####..vimp######
setMethod("..vimp", signature(object="familiarVimpMethod"),
          function(object, ...) return(get_placeholder_vimp_table(vimp_method=object@vimp_method,
                                                                  run_table=object@run_table)))



####.vimp (familiarVimpMethod)--------------------------------------------------
setMethod(".vimp", signature(object="familiarVimpMethod"),
          function(object, data, is_pre_processed=FALSE, ...) {
            
            # Suppress NOTES due to non-standard evaluation in data.table
            cluster_name <- NULL
            
            # Check if the class of object is a subclass of familiarVimpMethod
            if(!is_subclass(class(object)[1], "familiarVimpMethod")) object <- promote_vimp_method(object)
            
            # Return empty table if data is absent.
            if(is_empty(data)) return(get_placeholder_vimp_table(vimp_method=object@vimp_method,
                                                                 run_table=object@run_table))
            
            # Prepare input data
            data <- process_input_data(object=object,
                                       data=data,
                                       is_pre_processed=is_pre_processed,
                                       stop_at="clustering")
            
            # Work only with data that has known outcomes when determining
            # variable importance.
            data <- filter_missing_outcome(data=data)
            
            # Check again if data is absent because data may not have been
            # loaded in the check above.
            if(is_empty(data)) return(get_placeholder_vimp_table(vimp_method=object@vimp_method,
                                                                 run_table=object@run_table))
            
            # Identify invariant features and remove them.
            invariant_features <- get_feature_columns(x=data)[sapply(get_feature_columns(x=data), function(feature, data) is_singular_data(data[, get(feature)]), data=data@data)]
            
            # Remove invariant features.
            if(length(invariant_features) > 0){
              data <- filter_features(data=data, remove_features=invariant_features)
            }
            
            # Check that the data is suitable for predictions.
            if(has_bad_training_data(object=object, data=data)) return(get_placeholder_vimp_table(vimp_method=object@vimp_method,
                                                                                                  run_table=object@run_table))
            
            # Determine variable importance.
            vimp_object <- ..vimp(object=object,
                                  data=data)
            
            # Find signature features.
            signature_features <- names(object@feature_info)[sapply(object@feature_info, is_in_signature)]
            
            # Remove signature features.
            vimp_object <- remove_signature_features(vimp_object,
                                                     features=signature_features)
            
            # Set up a table with clustering information.
            cluster_table <- .create_clustering_table(object@feature_info)
            
            # Remove invariant features from the cluster_table
            if(length(invariant_features) > 0) cluster_table <- cluster_table[!cluster_name %in% invariant_features]
            
            # Remove features in the signature.
            if(length(signature_features) > 0) cluster_table <- cluster_table[!cluster_name %in% signature_features]
            
            # Update variable importance table object.
            vimp_object@vimp_method <- object@vimp_method
            vimp_object@run_table <- object@run_table
            vimp_object@cluster_table <- cluster_table
            vimp_object@project_id <- object@project_id
            
            # Set package version.
            vimp_object <- add_package_version(vimp_object)
            
            return(vimp_object)  
          })



####.vimp (familiarModel) ------------------------------------------------------
setMethod(".vimp", signature(object="familiarModel"),
          function(object, data, is_pre_processed=FALSE, ...) {
            
            # Suppress NOTES due to non-standard evaluation in data.table
            cluster_name <- NULL
            
            # Check if the class of object is a subclass of familiarModel.
            if(!is_subclass(class(object)[1], "familiarModel")) object <- promote_learner(object)
            
            # Return empty table if data is absent.
            if(is_empty(data)) return(get_placeholder_vimp_table(vimp_method=object@learner,
                                                                 run_table=object@run_table))
            
            # Prepare input data
            data <- process_input_data(object=object,
                                       data=data,
                                       is_pre_processed=is_pre_processed,
                                       stop_at="clustering")
            
            # Work only with data that has known outcomes when determining
            # variable importance.
            data <- filter_missing_outcome(data=data)
            
            # Check again if data is absent because data may not have been
            # loaded in the check above.
            if(is_empty(data)) return(get_placeholder_vimp_table(vimp_method=object@learner,
                                                                 run_table=object@run_table))
            
            # Identify invariant features and remove them.
            invariant_features <- get_feature_columns(x=data)[sapply(get_feature_columns(x=data), function(feature, data) is_singular_data(data[, get(feature)]), data=data@data)]
            
            # Remove invariant features.
            if(length(invariant_features) > 0){
              data <- filter_features(data=data, remove_features=invariant_features)
            }
            
            # Check that the data is suitable for predictions.
            if(has_bad_training_data(object=object, data=data)) return(get_placeholder_vimp_table(vimp_method=object@learner,
                                                                                                  run_table=object@run_table))
            
            # Determine variable importance.
            vimp_object <- ..vimp(object=object,
                                  data=data)
            
            # Find signature features.
            signature_features <- names(object@feature_info)[sapply(object@feature_info, is_in_signature)]
            
            # Remove signature features.
            vimp_object <- remove_signature_features(vimp_object,
                                                     features=signature_features)
            
            # Set up a table with clustering information.
            cluster_table <- .create_clustering_table(object@feature_info)
            
            # Remove invariant features from the cluster_table
            if(length(invariant_features) > 0) cluster_table <- cluster_table[!cluster_name %in% invariant_features]
            
            # Remove features in the signature.
            if(length(signature_features) > 0) cluster_table <- cluster_table[!cluster_name %in% signature_features]
            
            # Update variable importance table object.
            vimp_object@vimp_method <- object@learner
            vimp_object@run_table <- object@run_table
            vimp_object@cluster_table <- cluster_table
            vimp_object@project_id <- object@project_id
            
            # Set package version.
            vimp_object <- add_package_version(vimp_object)

            return(vimp_object)  
          })

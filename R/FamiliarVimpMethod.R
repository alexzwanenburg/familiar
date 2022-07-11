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
            vimp_table <- ..vimp(object=object,
                                 data=data)

            # Order output columns.
            data.table::setcolorder(vimp_table,
                                    neworder=colnames(get_placeholder_vimp_table()))

            return(vimp_table)  
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
            vimp_table <- ..vimp(object=object,
                                 data=data)
            
            # Order output columns.
            data.table::setcolorder(vimp_table,
                                    neworder=colnames(get_placeholder_vimp_table()))

            return(vimp_table)  
          })

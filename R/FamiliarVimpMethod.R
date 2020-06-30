#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

#####is_available#####
setMethod("is_available", signature(object="familiarVimpMethod"),
          function(object, ...) return(FALSE))


#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarVimpMethod"),
          function(object, ...) return(list()))


#####..vimp######
setMethod("..vimp", signature(object="familiarVimpMethod"),
          function(object, ...) return(get_placeholder_vimp_table()))


#####process_input_data (model)#####
setMethod("process_input_data", signature(object="familiarVimpMethod", data="ANY"),
          function(object, data, is_pre_processed=FALSE, stop_at="clustering"){
            # Prepares data for variable importance methods.
            
            # Check whether data is a dataObject, and create one otherwise
            if(!is(data, "dataObject")){
              data <- create_data_object(object=object, data=data, is_pre_processed=is_pre_processed)
            }
            
            # Load data from internal memory, if not provided otherwise
            if(data@delay_loading){
              data <- load_delayed_data(data=data, object=object, stop_at=stop_at)
            }
            
            # Pre-process data in case it has not been pre-processed
            if(!data@is_pre_processed){
              data <- preprocess_data(data=data, object=object, stop_at=stop_at)
            }
            
            # Return data
            return(data)
          })



#####.vimp (vimp method)#####
setMethod(".vimp", signature(object="familiarVimpMethod"),
          function(object, data, is_pre_processed=FALSE, ...) {
            
            # Check if the class of object is a subclass of familiarVimpMethod
            if(!is_subclass(class(object)[1], "familiarVimpMethod")) object <- promote_vimp_method(object)
            
            # Return empty table if data is absent.
            if(is_empty(data)) return(get_placeholder_vimp_table())
            
            # Prepare input data
            data <- process_input_data(object=object,
                                       data=data,
                                       is_pre_processed=is_pre_processed,
                                       stop_at="clustering")
            
            # Identify invariant features and remove them.
            invariant_features <- get_feature_columns(x=data)[sapply(get_feature_columns(x=data), function(feature, data) is_singular_data(data[, get(feature)]), data=data@data)]
            
            # Remove invariant features.
            if(length(invariant_features) > 0){
              data <- filter_features(data=data, remove_features=invariant_features)
            }
            
            # Check that the data is suitable for predictions.
            if(has_bad_training_data(object=object, data=data)) return(get_placeholder_vimp_table())
            
            # Determine variable importance.
            vimp_table <- ..vimp(object=object,
                                 data=data)

            # Order output columns.
            data.table::setcolorder(vimp_table,
                                    neworder=colnames(get_placeholder_vimp_table()))

            return(vimp_table)  
          })



#####.vimp (model)#####
setMethod(".vimp", signature(object="familiarModel"),
          function(object, data, is_pre_processed=FALSE, ...) {
            
            # Check if the class of object is a subclass of familiarModel.
            if(!is_subclass(class(object)[1], "familiarModel")) object <- promote_learner(object)
            
            # Return empty table if data is absent.
            if(is_empty(data)) return(get_placeholder_vimp_table())
            
            # Prepare input data
            data <- process_input_data(object=object,
                                       data=data,
                                       is_pre_processed=is_pre_processed,
                                       stop_at="clustering")
            
            # Identify invariant features and remove them.
            invariant_features <- get_feature_columns(x=data)[sapply(get_feature_columns(x=data), function(feature, data) is_singular_data(data[, get(feature)]), data=data@data)]
            
            # Remove invariant features.
            if(length(invariant_features) > 0){
              data <- filter_features(data=data, remove_features=invariant_features)
            }
            
            # Check that the data is suitable for predictions.
            if(has_bad_training_data(object=object, data=data)) return(get_placeholder_vimp_table())
            
            # Determine variable importance.
            vimp_table <- ..vimp(object=object,
                                 data=data)
            
            # Order output columns.
            data.table::setcolorder(vimp_table,
                                    neworder=colnames(get_placeholder_vimp_table()))

            return(vimp_table)  
          })

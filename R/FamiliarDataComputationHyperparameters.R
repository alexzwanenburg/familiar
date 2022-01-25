#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarDataElementHyperparameters",
         contains="familiarDataElement",
         prototype = methods::prototype(detail_level="hybrid",
                                        estimation_type="point"))

#'@title Internal function to extract hyperparameters from models.
#'
#'@description Collects hyperparameters from models in a `familiarEnsemble`.
#'
#'@inheritParams extract_data
#'
#'@return A list of familiarDataElements with hyperparameters.
#'@md
#'@keywords internal
setGeneric("extract_hyperparameters", function(object,
                                               message_indent=0L,
                                               verbose=FALSE,
                                               ...) standardGeneric("extract_hyperparameters"))

#####extract_hyperparameters (familiarEnsemble)#####
setMethod("extract_hyperparameters", signature(object="familiarEnsemble"),
          function(object,
                   message_indent=0L,
                   verbose=FALSE){
            # Extracts hyper-parameters from each model and collects them.
            
            # Message extraction start
            logger.message(paste0("Extracting hyperparameters from the models in the ensemble."),
                           indent=message_indent,
                           verbose=verbose)
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)) ..error_ensemble_models_not_loaded()
            
            # Test if the any of the models in the ensemble were trained.
            if(!model_is_trained(object)) return(NULL)
            
            proto_data_element <- methods::new("familiarDataElementHyperparameters")
            
            # Generate elements to send to dispatch.
            hyperparameter_data <- extract_dispatcher(FUN=.extract_hyperparameters,
                                                      cl=NULL,
                                                      has_internal_bootstrap=FALSE,
                                                      object=object,
                                                      proto_data_element=proto_data_element,
                                                      aggregate_results=FALSE,
                                                      message_indent=message_indent + 1L,
                                                      verbose=verbose)
            
            return(hyperparameter_data)
          })




.extract_hyperparameters <- function(object,
                                     proto_data_element,
                                     ...){
  
  # Ensure that the object is loaded
  object <- load_familiar_object(object)
  
  # Add model name.
  data_element <- add_model_name(proto_data_element, object=object)
  
  # Test if the any of the models in the ensemble were trained.
  if(!model_is_trained(object)) return(NULL)
  
  # Parse hyperparameters as data.table
  data_element@data <- data.table::as.data.table(object@hyperparameters)
  
  # Set value columns
  data_element@value_column <- names(object@hyperparameters)
  
  return(data_element)
}


..hyperparameter_to_string <- function(x){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  instance <- n <- NULL
  
  if(is.character(x) | is.factor(x) | is.logical(x)){
    
    # Count instances and sort by number.
    data <- data.table::data.table("instance"=x)
    data <- data[, list(n=.N), by=instance][order(n)]
    
    # Parse to a vector
    export_vec <- sapply(seq_len(nrow(data)), function(ii, data) (paste0(data$instance[ii], " (", data$n[ii],")") ), data=data)
    export_vec <- paste(export_vec, collapse="; ")
    
  } else if(is.numeric(x)){
    export_vec <- paste0(stats::quantile(x=x, probs=0.5, na.rm=TRUE, type=1, names=FALSE),
                         " [", min(x, na.rm=TRUE), ", ",
                         max(x, na.rm=TRUE), "]")
  }
  
  return(export_vec)
}


#####.identifier_as_data_attribute (familiarDataElementHyperparameters)---------
setMethod(".identifier_as_data_attribute", signature(x="familiarDataElementHyperparameters"),
          function(x, identifier, as_grouping_column=TRUE){
            if(length(identifier) == 0) ..error_reached_unreachable_code(".identifier_as_data_attribute: Cannot pass an empty identifier.")
            
            # Different learners have different hyperparameters. We therefore
            # keep splitting by learner.
            if(any(identifier == "all")){
              
              identifier <- names(x@identifiers)
              
              # Remove learner, if present.
              identifier <- setdiff(identifier, "learner")
              
              if(length(identifier) == 0) return(x)
              
              return(callNextMethod(x=x, identifier=identifier, as_grouping_column=as_grouping_column))
              
            } else {
              return(callNextMethod())
            }
          })


##### ..compute_data_elements_estimates (familiarDataElementHyperparameters)------
setMethod("..compute_data_element_estimates", signature(x="familiarDataElementHyperparameters"),
          function(x, x_list=NULL, ...){
            # It might be that x was only used to direct to this method.
            if(!is.null(x_list)) x <- x_list
            if(!is.list(x)) x <- list(x)
            
            # Remove empty entries.
            x <- x[!sapply(x, is_empty)]
            if(is_empty(x)) return(NULL)
            
            # Collect all data.
            data <- data.table::rbindlist(lapply(x, function(x) (x@data)),
                                          use.names=TRUE,
                                          fill=TRUE)
            
            # Split by fs_method
            data <- split(data, by="fs_method")
            
            learner <- x[[1]]@identifiers$learner
            if(is.null(learner)) learner <- x[[1]]@data$learner[1]
            
            # Set learner.
            parameter_string <- paste0("learner\t", learner)
            
            for(current_data in data){
              # Determine the vimp_method
              vimp_method <- current_data$fs_method[1]
              
              # Set vimp method.
              parameter_string <- c(parameter_string,
                                    paste0("fs_method\t", vimp_method),
                                    "---------------------")
              
              # Parse data.
              parameter_string <- c(parameter_string,
                                    sapply(x[[1]]@value_column, function(hyperparameter, data) (paste0(hyperparameter, "\t", ..hyperparameter_to_string(data[[hyperparameter]]))), data=current_data))
              parameter_string <- c(parameter_string, " ")
              
            }
            
            # Collapse to text.
            parameter_string <- paste0(parameter_string, collapse="\n")
            
            # Copy data element.
            y <- x[[1]]
            y@data <- parameter_string
            
            # Update value column
            y@value_column <- NA_character_
            
            return(y)
          })



#####.export (familiarDataElementHyperparameters)-------------------------------
setMethod(".export", signature(x="familiarDataElementHyperparameters"),
          function(x, x_list, aggregate_results=FALSE, ...){
            # This is like .export,familiarDataElement, but the elements are
            # merged prior to computing estimates.
            
            # Merge data elements.
            x <- merge_data_elements(x=x_list,
                                     as_data="all",
                                     as_grouping_column=TRUE,
                                     force_data_table=TRUE)
            
            if(aggregate_results){
              x <- .compute_data_element_estimates(x)
            }
            
            return(x)
          })



#####export_hyperparameters#####

#'@title Extract and export model hyperparameters.
#'
#'@description Extract and export model hyperparameters from models in a
#'  familiarCollection.
#'
#'@inheritParams export_all
#'@inheritParams extract_univariate_analysis
#'
#'@inheritDotParams as_familiar_collection
#'
#'@details Data, such as model performance and calibration information, is
#'  usually collected from a `familiarCollection` object. However, you can also
#'  provide one or more `familiarData` objects, that will be internally
#'  converted to a `familiarCollection` object. It is also possible to provide a
#'  `familiarEnsemble` or one or more `familiarModel` objects together with the
#'  data from which data is computed prior to export. Paths to the previous
#'  files can also be provided.
#'
#'  All parameters aside from `object` and `dir_path` are only used if `object`
#'  is not a `familiarCollection` object, or a path to one.
#'
#'  Many model hyperparameters are optimised using sequential model-based
#'  optimisation. The extracted hyperparameters are those that were selected to
#'  construct the underlying models (`familiarModel` objects).
#'
#'@return A data.table (if `dir_path` is not provided), or nothing, as all data
#'  is exported to `csv` files. In case of the latter, hyperparameters are
#'  summarised.
#'@exportMethod export_hyperparameters
#'@md
#'@rdname export_hyperparameters-methods
setGeneric("export_hyperparameters",
           function(object,
                    dir_path=NULL,
                    aggregate_results=TRUE,
                    export_collection=FALSE,
                    ...) standardGeneric("export_hyperparameters"))

#####export_hyperparameters (collection)#####

#'@rdname export_hyperparameters-methods
setMethod("export_hyperparameters", signature(object="familiarCollection"),
          function(object,
                   dir_path=NULL,
                   aggregate_results=TRUE,
                   export_collection=FALSE,
                   ...){
            
            if(aggregate_results){
              subtype <- "summary"
              
            } else {
              subtype <- NULL
            }
            
            return(.export(x=object,
                           data_slot="hyperparameters",
                           dir_path=dir_path,
                           aggregate_results=aggregate_results,
                           type="hyperparameter",
                           subtype=subtype,
                           export_collection=export_collection))
          })

#####export_hyperparameters (generic)#####

#'@rdname export_hyperparameters-methods
setMethod("export_hyperparameters", signature(object="ANY"),
          function(object,
                   dir_path=NULL,
                   aggregate_results=TRUE,
                   export_collection=FALSE,
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object,
                                          "data_element"="hyperparameters"),
                                     list(...)))
            
            return(do.call(export_hyperparameters,
                           args=c(list("object"=object,
                                       "dir_path"=dir_path,
                                       "aggregate_results"=aggregate_results,
                                       "export_collection"=export_collection),
                                  list(...))))
          })

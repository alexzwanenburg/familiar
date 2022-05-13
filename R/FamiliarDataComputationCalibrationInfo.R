#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarDataElementCalibrationInfo",
         contains="familiarDataElement",
         slots=list("outcome_type"="ANY"),
         prototype = methods::prototype(estimation_type="point",
                                        outcome_type=NULL))



#'@title Internal function to extract calibration info from data.
#'
#'@description Collects .
#'
#'@inheritParams extract_data
#'
#'@return A list of familiarDataElements with hyperparameters.
#'@md
#'@keywords internal
setGeneric("extract_calibration_info", function(object,
                                                detail_level=waiver(),
                                                message_indent=0L,
                                                verbose=FALSE,
                                                ...) standardGeneric("extract_calibration_info"))

#####extract_calibration_info (familiarEnsemble)#####
setMethod("extract_calibration_info", signature(object="familiarEnsemble"),
          function(object,
                   detail_level=waiver(),
                   message_indent=0L,
                   verbose=FALSE){
            
            # Extracts calibration info for survival outcomes. Note that some
            # routines for count and continuous outcomes are available, but not
            # used.
            if(!object@outcome_type %in% c("survival")) return(NULL)
            
            # Message extraction start
            logger.message(paste0("Extracting calibration information."),
                           indent=message_indent,
                           verbose=verbose)
            
            # Check the level detail.
            detail_level <- .parse_detail_level(x = detail_level,
                                                object = object,
                                                default = "ensemble",
                                                data_element = "calibration_info")
            
            proto_data_element <- methods::new("familiarDataElementCalibrationInfo",
                                               detail_level=detail_level)
            
            # Generate elements to send to dispatch.
            calibration_info <- extract_dispatcher(FUN=.extract_calibration_info,
                                                   cl=NULL,
                                                   has_internal_bootstrap=FALSE,
                                                   object=object,
                                                   proto_data_element=proto_data_element,
                                                   aggregate_results=FALSE,
                                                   message_indent=message_indent + 1L,
                                                   verbose=verbose)
            
            return(calibration_info)
          })



.extract_calibration_info <- function(object,
                                      proto_data_element,
                                      ...){
  
  # Ensure that the object is loaded
  object <- load_familiar_object(object)
  
  # Add model name.
  data_element <- add_model_name(proto_data_element, object=object)
  
  # Add data.
  data_element@data <- object@calibration_info
  
  # Add outcome type.
  data_element@outcome_type <- object@outcome_type
  
  # Check if the element is empty.
  if(is_empty(data_element@data)) return(NULL)
  
  # Set value and grouping columns.
  if(object@outcome_type == "survival"){
    data_element@value_column <- setdiff(colnames(data_element@data),
                                         "time")
    data_element@grouping_column <- "time"
    
  } else if(object@outcome_type %in% c("count", "continuous")){
    data_element@value_column <- colnames(data_element@data)
    
  } else {
    ..error_outcome_type_not_implemented(object@outcome_type)
  }
  
  return(data_element)
}



##### ..compute_data_elements_estimates (familiarDataElementHyperparameters)------
setMethod("..compute_data_element_estimates", signature(x="familiarDataElementCalibrationInfo"),
          function(x, x_list=NULL, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            min_value <- max_value <- time <- NULL
            
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
            
            if(x[[1]]@outcome_type %in% c("continuous", "count")){
              
              # Extract min and max values of the range for continuous and count
              # type outcomes.
              if(is.null(x[[1]]@grouping_column)){
                data <- data[, list("min_value"=min(min_value),
                                    "max_value"=max(max_value))]
                
              } else {
                data <- data[,
                             list("min_value"=min(min_value),
                                  "max_value"=max(max_value)),
                             by=x[[1]]]@grouping_column
              }
              
            } else if (x[[1]]@outcome_type %in% c("survival")) {
              
              # Identify all unique time points.
              new_time <- sort(unique(data$time))

              # Find non-time grouping columns.
              grouping_column <- setdiff(colnames(data),
                                         c("time", x[[1]]@value_column))
              
              # Select only unique data points to prevent warnings during
              # approximation.
              data <- unique(data)
              
              if(length(grouping_column) > 0){
                # Interpolate survival columns by grouping column.
                data <- data[,
                             ..interpolate_survival_data(.SD, time=time, new_time=new_time),
                             by=c(grouping_column),
                             .SDcols=x[[1]]@value_column]
                
              } else {
                # Interpolate survival columns.
                data <- data[,
                             ..interpolate_survival_data(.SD, time=time, new_time=new_time),
                             .SDcols=x[[1]]@value_column]
              }
              
              # Compute mean value at each time point, by group.
              data <- data[,
                           lapply(.SD, mean, na.rm=TRUE),
                           by=c(x[[1]]@grouping_column),
                           .SDcols=c(x[[1]]@value_column)]
              
            } else {
              ..error_outcome_type_not_implemented(outcome_type=x[[1]]@outcome_type)
            }
            
            # Copy data element.
            y <- x[[1]]
            y@data <- data
            
            return(y)
          })



..interpolate_survival_data <- function(data,
                                        time,
                                        new_time,
                                        extrapolate=FALSE){
  # Get columns names.
  value_columns <- colnames(data)
  
  # Interpolate survival data.
  data <- data[, lapply(.SD,
                        function(y, x, x_out, extrapolate){
                          return(stats::approx(x=x,
                                               y=y,
                                               xout=x_out,
                                               rule=ifelse(extrapolate, 2, 1),
                                               method="linear")$y)
                        },
                        x=time,
                        x_out=new_time,
                        extrapolate=extrapolate)]
  
  # Insert time column.
  data[, "time":=new_time]
  
  # Select only finite values. This removes extrapolated values in case
  # extrapolate equals FALSE.
  data <- data[!is.na(value_columns[1])]
  
  return(as.list(data))
}



#####export_calibration_info#####

#'@title Extract and export calibration information.
#'
#'@description Extract and export calibration information (e.g. baseline
#'  survival) for data in a familiarCollection.
#'
#'@inheritParams export_all
#'@inheritParams export_univariate_analysis_data
#'
#'@inheritDotParams as_familiar_collection
#'
#'@details Data is usually collected from a `familiarCollection` object.
#'  However, you can also provide one or more `familiarData` objects, that will
#'  be internally converted to a `familiarCollection` object. It is also
#'  possible to provide a `familiarEnsemble` or one or more `familiarModel`
#'  objects together with the data from which data is computed prior to export.
#'  Paths to the previous files can also be provided.
#'
#'  All parameters aside from `object` and `dir_path` are only used if `object`
#'  is not a `familiarCollection` object, or a path to one.
#'
#'  Currently only baseline survival is exported as supporting calibration
#'  information. See `export_calibration_data` for export of direct assessment
#'  of calibration, including calibration and goodness-of-fit tests.
#'
#'@return A data.table (if `dir_path` is not provided), or nothing, as all data
#'  is exported to `csv` files.
#'@exportMethod export_calibration_info
#'@md
#'@rdname export_calibration_info-methods
setGeneric("export_calibration_info", function(object,
                                               dir_path=NULL,
                                               aggregate_results=TRUE,
                                               export_collection=FALSE,
                                               ...) standardGeneric("export_calibration_info"))

#####export_calibration_info (collection)#####

#'@rdname export_calibration_info-methods
setMethod("export_calibration_info", signature(object="familiarCollection"),
          function(object,
                   dir_path=NULL,
                   aggregate_results=TRUE,
                   export_collection=FALSE,
                   ...){
            
            # Make sure the collection object is updated.
            object <- update_object(object=object)
            
            if(object@outcome_type %in% c("binomial", "multinomial")){
              return(NULL)
              
            } else if(object@outcome_type %in% c("count", "continuous")){
              subtype <- "observed_value_range"
              
            } else if(object@outcome_type %in% c("survival")){
              subtype <- "baseline_value"
              
            } else {
              ..error_outcome_type_not_implemented(object@outcome_type)
            }
            
            return(.export(x=object,
                           data_slot="calibration_info",
                           dir_path=dir_path,
                           aggregate_results=aggregate_results,
                           type="calibration",
                           subtype=subtype,
                           export_collection=export_collection))
          })

#####export_calibration_info (generic)#####

#'@rdname export_calibration_info-methods
setMethod("export_calibration_info", signature(object="ANY"),
          function(object,
                   dir_path=NULL,
                   aggregate_results=TRUE,
                   export_collection=FALSE,
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object,
                                          "data_element"="calibration_info"),
                                     list(...)))
            
            return(do.call(export_calibration_info,
                           args=c(list("object"=object,
                                       "dir_path"=dir_path,
                                       "aggregate_results"=aggregate_results,
                                       "export_collection"=export_collection),
                                  list(...))))
          })

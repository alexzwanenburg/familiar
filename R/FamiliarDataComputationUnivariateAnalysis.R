#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarDataElementUnivariateAnalysis",
         contains="familiarDataElement",
         prototype = methods::prototype(detail_level="ensemble",
                                        estimation_type="point"))

setClass("familiarDataElementRobustness",
         contains="familiarDataElement",
         slots=list("icc_type"="character"),
         prototype = methods::prototype(detail_level="ensemble",
                                        estimation_type="point",
                                        icc_type="1"))

#'@title Internal function to extract data from a univariate analysis.
#'
#'@description Computes and extracts univariate analysis for the features used
#'  in a `familiarEnsemble` object. This assessment includes the computation of
#'  p and q-values, as well as robustness (in case of repeated measurements).
#'
#'@inheritParams extract_data
#'
#'@return A list with a data.table containing information concerning the
#'  univariate analysis of important features.
#'@md
#'@keywords internal
setGeneric("extract_univariate_analysis",
           function(object,
                    data,
                    cl=NULL,
                    icc_type=waiver(),
                    feature_similarity=NULL,
                    feature_cluster_method=waiver(),
                    feature_cluster_cut_method=waiver(),
                    feature_linkage_method=waiver(),
                    feature_similarity_threshold=waiver(),
                    feature_similarity_metric=waiver(),
                    message_indent=0L,
                    verbose=FALSE,
                    ...) standardGeneric("extract_univariate_analysis"))

#####extract_univariate_analysis#####
setMethod("extract_univariate_analysis", signature(object="familiarEnsemble", data="ANY"),
          function(object,
                   data,
                   cl=NULL,
                   icc_type=waiver(),
                   feature_similarity=NULL,
                   feature_cluster_method=waiver(),
                   feature_cluster_cut_method=waiver(),
                   feature_linkage_method=waiver(),
                   feature_similarity_threshold=waiver(),
                   feature_similarity_metric=waiver(),
                   message_indent=0L,
                   verbose=FALSE){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            p_value <- NULL
            
            # Message extraction start
            logger.message(paste0("Extracting univariate analysis information."),
                           indent=message_indent,
                           verbose=verbose)
            
            # Obtain from settings, if unset.
            if(is.waive(icc_type)) icc_type <- object@settings$icc_type

            # Check icc_type
            .check_parameter_value_is_valid(x=icc_type, var_name="icc_type",
                                            values=.get_available_icc_types())

            # Get and process the input data
            data <- process_input_data(object=object, data=data, stop_at="normalisation")
            
            # Check if the data object is empty.
            if(is_empty(data)) return(NULL)
            
            # Maintain only important features. The current set is based on the
            # required features.
            data <- filter_features(data=data,
                                    available_features=object@model_features)
            
            # Determine feature columns
            feature_columns <- get_feature_columns(x=data)
            
            # Check if there are any features in the model.
            if(length(feature_columns) == 0) return(NULL)
            
            ##### Univariate p-values ------------------------------------------
            # Remove data with missing outcomes.
            feature_data <- remove_missing_outcomes(data=data,
                                                    outcome_type=object@outcome_type)

            if(is_empty(feature_data)){
              # Check that data are not empty
              univariate_data <- NULL
              
            } else if(data.table::uniqueN(feature_data@data, by=get_id_columns(id_depth="sample")) <= 5){
              # Check if the number of samples is sufficient (>5), and return an
              # empty table if not.
              univariate_data <- NULL
              
            } else {
              # Check that the qvalue package is installed.
              has_qvalue_package <- is_package_installed(name="qvalue")
              
              # Calculate univariate P values, based on aggregated data
              regression_p_values <- compute_univariable_p_values(cl=cl,
                                                                  data_obj=aggregate_data(data=feature_data),
                                                                  feature_columns=feature_columns)
              
              # Find and replace non-finite values
              regression_p_values[!is.finite(regression_p_values)] <- NA_real_
              
              # Collect to table
              univariate_data <- data.table::data.table("feature"=names(regression_p_values),
                                                        "p_value"=regression_p_values)[order(p_value)]
              
              # Only introduce q-values if the qvalue package is installed.
              if(has_qvalue_package){

                if(all(!is.finite(regression_p_values))){
                  # q-values can only be computed if any p-values are not NA.
                  computed_q_value <- NA_real_
                  
                } else {
                  # q-values can only be computed for larger numbers of features
                  computed_q_value <- tryCatch(qvalue::qvalue(p=univariate_data$p_value)$qvalues,
                                               warning=identity,
                                               error=identity)
                  
                  if(inherits(computed_q_value, "error")) computed_q_value <- NA_real_
                  if(inherits(computed_q_value, "warning")) computed_q_value <- NA_real_
                }
                
                # Set q-value
                univariate_data[, "q_value":=computed_q_value]
              }
              
              # Set univariate data.
              univariate_data <- methods::new("familiarDataElementUnivariateAnalysis",
                                              data=univariate_data,
                                              value_column=ifelse(has_qvalue_package, c("p_value", "q_value"), "p_value"),
                                              grouping_column="feature")
              
              # Add model name.
              univariate_data <- add_model_name(univariate_data, object)
            }
            
            ##### Feature robustness -------------------------------------------
            
            if(!all(data@data$repetition_id == 1)){
              
              # Determine which columns actually contains numeric data
              numeric_columns <- feature_columns[sapply(feature_columns, function(ii, data) (is.numeric(data@data[[ii]])), data=data)]
              
              if(length(numeric_columns) == 0){
                icc_data <- NULL
                
              } else {
                # Compute ICC values
                icc_data <- fam_mapply(cl=cl,
                                       assign=NULL,
                                       FUN=compute_icc,
                                       x=data@data[, mget(numeric_columns)],
                                       feature=numeric_columns,
                                       progress_bar=FALSE,
                                       MoreArgs=list("id_data"=data@data[, mget(get_id_columns(id_depth="repetition"))],
                                                     "type"=icc_type))
                
                # Compute values
                icc_data <- data.table::rbindlist(icc_data, use.names=TRUE)
                
                # Create object.
                icc_data <- methods::new("familiarDataElementRobustness",
                                         data=icc_data,
                                         value_column=c("icc", "icc_low", "icc_up", "icc_panel", "icc_panel_low", "icc_panel_up"),
                                         grouping_column="feature")
                
                # Add model name.
                icc_data <- add_model_name(icc_data, object)
              }
              
            } else {
              icc_data <- NULL
            }
            
            return(list(univariate_data, icc_data))
          })



#####export_univariate_analysis_data#####

#'@title Extract and export univariate analysis data of features.
#'
#'@description Extract and export univariate analysis data of features for data
#'  in a familiarCollection.
#'
#'@inheritParams export_all
#'@inheritParams plot_univariate_importance
#'
#'@inheritDotParams extract_univariate_analysis
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
#'  Univariate analysis includes the computation of p and q-values, as well as
#'  robustness (in case of repeated measurements). p-values are derived from
#'  Wald's test.
#'
#'@return A data.table (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_univariate_analysis_data
#'@md
#'@rdname export_univariate_analysis_data-methods
setGeneric("export_univariate_analysis_data",
           function(object,
                    dir_path=NULL,
                    p_adjustment_method=waiver(),
                    export_collection=FALSE,
                    ...) standardGeneric("export_univariate_analysis_data"))

#####export_univariate_analysis_data (collection)#####

#'@rdname export_univariate_analysis_data-methods
setMethod("export_univariate_analysis_data", signature(object="familiarCollection"),
          function(object,
                   dir_path=NULL,
                   p_adjustment_method=waiver(),
                   export_collection=FALSE,
                   ...){
            
            # Make sure the collection object is updated.
            object <- update_object(object=object)
            
            # Set default adjust method.
            if(is.waive(p_adjustment_method)) p_adjustment_method <- "fdr"
            if(is.null(p_adjustment_method)) p_adjustment_method <- "none"
            
            # Obtain data from the univariate analysis.
            univariate_data <- .export(x=object,
                                        data_slot="univariate_analysis",
                                        dir_path=dir_path,
                                        aggregate_results=FALSE,
                                        type="variable_importance",
                                        subtype="univariate",
                                        object_class="familiarDataElementUnivariateAnalysis")
            
            # Compute the adjusted p-values.
            univariate_data <- lapply(univariate_data,
                                      .compute_adjusted_univariate_p_value,
                                      method=p_adjustment_method)
            
            # Obtain robustness data.
            icc_data <- .export(x=object,
                                data_slot="univariate_analysis",
                                dir_path=dir_path,
                                aggregate_results=FALSE,
                                type="variable_importance",
                                subtype="robustness",
                                object_class="familiarDataElementRobustness")

            # Set data list.
            data_list <- list("univariate"=univariate_data,
                              "icc"=icc_data)
            
            if(!is.null(dir_path)) data_list <- NULL
            if(export_collection) data_list <- c(data_list, list("collection"=object))
            
            return(data_list)
          })

#####export_univariate_analysis_data (generic)#####

#'@rdname export_univariate_analysis_data-methods
setMethod("export_univariate_analysis_data", signature(object="ANY"),
          function(object,
                   dir_path=NULL,
                   p_adjustment_method=waiver(),
                   export_collection=FALSE,
                   ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object,
                                          "data_element"="univariate_analysis"),
                                     list(...)))
            
            return(do.call(export_univariate_analysis_data,
                           args=c(list("object"=object,
                                       "dir_path"=dir_path,
                                       "p_adjustment_method"=p_adjustment_method,
                                       "export_collection"=export_collection),
                                  list(...))))
          })



.compute_adjusted_univariate_p_value <- function(x, method="fdr"){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  p_value <- NULL
  
  # If the method is not a correction method, return the data as is.
  if(!method %in% stats::p.adjust.methods) return(x)
  
  # Check that the data are not empty.
  if(is_empty(x)) return(x)
  
  # Determine grouping columns.
  grouping_column <- setdiff(x@grouping_column, "feature")
  
  # Make a local copy of the data.
  data <- data.table::copy(x@data)
  
  if(length(grouping_column) == 0){
    data[, "adjusted_p_value":=stats::p.adjust(p_value,
                                               method=method)]
  } else {
    data[, "adjusted_p_value":=stats::p.adjust(p_value,
                                               method=method),
         by=c(grouping_column)]
  }
  
  # Add to data element.
  x@data <- data
  
  return(x)
}

#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

#####export_all#####

#'@title Extract and export all data.
#'
#'@description Extract and export all data from a familiarCollection.
#'
#'@param object A `familiarCollection` object, or other other objects from which
#'  a `familiarCollection` can be extracted. See details for more information.
#'@param dir_path Path to folder where extracted data should be saved. `NULL`
#'  will allow export as a structured list of data.tables.
#'@param export_raw Allows export of raw data. Only used when exporting model
#'  performance information.
#'
#'@inheritDotParams extract_data
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
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_all
#'@md
#'@rdname export_all-methods
setGeneric("export_all",
           function(object, dir_path=NULL, export_raw=FALSE, ...) standardGeneric("export_all"))


#####export_all (collection)#####

#'@rdname export_all-methods
setMethod("export_all", signature(object="familiarCollection"),
          function(object, dir_path=NULL, export_raw=FALSE, ...){
            
            # Export feature selection variable importance
            fs_vimp <- export_fs_vimp(object=object, dir_path=dir_path)
            
            # Export model variable importance
            model_vimp <- export_model_vimp(object=object, dir_path=dir_path)
            
            # Export permutation variable importance.
            permutation_vimp <- export_permutation_vimp(object=object, dir_path=dir_path, export_raw=export_raw)
            
            # Export model hyperparameters
            hyperparameters <- export_hyperparameters(object=object, dir_path=dir_path)
            
            # Export prediction tables
            prediction_data <- export_prediction_data(object=object, dir_path=dir_path, export_raw=export_raw)
            
            # Export decision curve analysis data
            dca_data <- export_decision_curve_analysis_data(object=object, dir_path=dir_path, export_raw=export_raw)
            
            # Export calibration information
            calibration_info <- export_calibration_info(object=object, dir_path=dir_path)
            
            # Export calibration data
            calibration_data <- export_calibration_data(object=object, dir_path=dir_path)
            
            # Export model performance
            model_performance <- export_model_performance(object=object, dir_path=dir_path, export_raw=export_raw)
            
            # Export confusion matrix
            confusion_matrix <- export_confusion_matrix_data(object=object, dir_path=dir_path, export_raw=export_raw)
            
            # Export kaplan-meier info
            km_info <- export_stratification_cutoff(object=object, dir_path=dir_path)
            
            # Export stratification data
            km_data <- export_stratification_data(object=object, dir_path=dir_path)
            
            # Export AUC data
            auc_data <- export_auc_data(object=object, dir_path=dir_path, export_raw=export_raw)
            
            # Export data from the univariate analysis
            univariate_analysis <- export_univariate_analysis_data(object=object, dir_path=dir_path)
            
            # Export data from feature expressions
            feature_expressions <- export_feature_expressions(object=object, dir_path=dir_path)
            
            # Export mutual-correlation data
            mutual_correlation <- export_feature_similarity(object=object, dir_path=dir_path)
            
            if(is.null(dir_path)){
              return(list("fs_vimp" = fs_vimp,
                          "model_vimp" = model_vimp,
                          "permutation_vimp" = permutation_vimp,
                          "hyperparameters" = hyperparameters,
                          "prediction_data" = prediction_data,
                          "calibration_info" = calibration_info,
                          "calibration_data" = calibration_data,
                          "model_performance" = model_performance,
                          "confusion_matrix" = confusion_matrix,
                          "decision_curve" = dca_data,
                          "km_info" = km_info,
                          "km_data" = km_data,
                          "auc_data" = auc_data,
                          "univariate_analysis" = univariate_analysis,
                          "feature_expressions" = feature_expressions,
                          "mutual_correlation" = mutual_correlation))
            }
          })

#####export_all (generic)#####

#'@rdname export_all-methods
setMethod("export_all", signature(object="ANY"),
          function(object, dir_path=NULL, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection, args=append(list("object"=object), list(...)))
            
            return(do.call(export_all,
                           args=append(list("object"=object, "dir_path"=dir_path), list(...))))
          })


#####export_fs_vimp#####

#'@title Extract and export feature selection variable importance.
#'
#'@description Extract and export feature selection variable importance from a
#'  familiarCollection.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams extract_fs_vimp
#'@inheritDotParams as_familiar_collection
#'
#'@details Data, such as model performance and calibration information, is
#'  usually collected from a `familiarCollection` object. However, you can also
#'  provide one or more `familiarData` objects, that will be internally
#'  converted to a `familiarCollection` object. Paths to the previous files can
#'  also be provided.
#'
#'  Unlike other export function, export using `familiarEnsemble` or
#'  `familiarModel` objects is not possible. This is because feature selection
#'  variable importance is not stored within `familiarModel` objects.
#'
#'  All parameters aside from `object` and `dir_path` are only used if `object`
#'  is not a `familiarCollection` object, or a path to one.
#'
#'  Variable importance is based on the ranking produced by feature selection
#'  routines. In case feature selection was performed repeatedly, e.g. using
#'  bootstraps, feature ranks are first aggregated using the method defined by
#'  the `aggregation_method`, some of which require a `rank_threshold` to
#'  indicate a subset of most important features.
#'
#'  Information concerning highly similar features that form clusters is
#'  provided as well. This information is based on consensus clustering of the
#'  features. This clustering information is also used during aggregation to
#'  ensure that co-clustered features are only taken into account once.
#'
#'@return A data.table (if `dir_path` is not provided), or nothing, as all data
#'  is exported to `csv` files.
#'@exportMethod export_fs_vimp
#'@md
#'@rdname export_fs_vimp-methods
setGeneric("export_fs_vimp", function(object, dir_path=NULL, ...) standardGeneric("export_fs_vimp"))

#####export_fs_vimp (collection)#####

#'@rdname export_fs_vimp-methods
setMethod("export_fs_vimp", signature(object="familiarCollection"),
          function(object, dir_path=NULL, ...){
            
            # Extract the relevant table
            main_table <- object@fs_vimp$vimp_table
            
            # Check if any information was stored
            if(is_empty(x=main_table)) { return(NULL) }
            
            # Apply labels
            main_table <- .apply_labels(data=main_table, object=object)

            # Sort table before export
            data.table::setorder(main_table, "data_set", "fs_method", "rank")
            
            if(is.null(dir_path)){
              return(main_table)
              
            } else {
              .export_to_file(data=main_table, object=object, dir_path=dir_path,
                              type="variable_importance", subtype="feature_selection")
              
              return(NULL)
            }

          })

#####export_fs_vimp (generic)#####

#'@rdname export_fs_vimp-methods
setMethod("export_fs_vimp", signature(object="ANY"),
          function(object, dir_path=NULL, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="fs_vimp"), list(...)))
            
            return(do.call(export_fs_vimp,
                           args=append(list("object"=object, "dir_path"=dir_path), list(...))))
          })


#####export_model_vimp#####

#'@title Extract and export model-based variable importance.
#'
#'@description Extract and export model-based variable importance from a
#'  familiarCollection.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams extract_model_vimp
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
#'  Variable importance is based on the ranking produced by model-specific
#'  variable importance routines, e.g. permutation for random forests. If such a
#'  routine is absent, variable importance is based on the feature selection
#'  method that led to the features included in the model. In case multiple
#'  models (`familiarModel` objects) are combined, feature ranks are first
#'  aggregated using the method defined by the `aggregation_method`, some of
#'  which require a `rank_threshold` to indicate a subset of most important
#'  features.
#'
#'  Information concerning highly similar features that form clusters is
#'  provided as well. This information is based on consensus clustering of the
#'  features that were used in the signatures of the underlying models. This
#'  clustering information is also used during aggregation to ensure that
#'  co-clustered features are only taken into account once.
#'
#'@return A data.table (if `dir_path` is not provided), or nothing, as all data
#'  is exported to `csv` files.
#'@exportMethod export_model_vimp
#'@md
#'@rdname export_model_vimp-methods
setGeneric("export_model_vimp",
           function(object, dir_path=NULL, ...) standardGeneric("export_model_vimp"))

#####export_model_vimp (collection)#####

#'@rdname export_model_vimp-methods
setMethod("export_model_vimp", signature(object="familiarCollection"),
          function(object, dir_path=NULL, ...){
            
            # Extract the relevant table
            main_table <- object@model_vimp$vimp_table
            
            # Check if any information was stored
            if(is_empty(x=main_table)) { return(NULL) }
            
            # Apply labels
            main_table <- .apply_labels(data=main_table, object=object)
            
            # Sort table before export
            data.table::setorder(main_table, "data_set", "learner", "fs_method", "rank")
            
            if(is.null(dir_path)){
              return(main_table)
              
            } else {
              .export_to_file(data=main_table, object=object, dir_path=dir_path,
                              type="variable_importance", subtype="learner")
              
              return(NULL)
            }
            
          })

#####export_model_vimp (generic)#####

#'@rdname export_model_vimp-methods
setMethod("export_model_vimp", signature(object="ANY"),
          function(object, dir_path=NULL, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="model_vimp"), list(...)))
            
            return(do.call(export_model_vimp,
                           args=append(list("object"=object, "dir_path"=dir_path), list(...))))
          })


#####export_permutation_vimp#####

#'@title Extract and export permutation variable importance.
#'
#'@description Extract and export model-based variable importance from a
#'  familiarCollection.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams extract_permutation_vimp
#'@inheritDotParams as_familiar_collection
#'
#'@details Data, such as permutation variable importance and calibration
#'  information, is usually collected from a `familiarCollection` object.
#'  However, you can also provide one or more `familiarData` objects, that will
#'  be internally converted to a `familiarCollection` object. It is also
#'  possible to provide a `familiarEnsemble` or one or more `familiarModel`
#'  objects together with the data from which data is computed prior to export.
#'  Paths to the previously mentioned files can also be provided.
#'
#'  All parameters aside from `object` and `dir_path` are only used if `object`
#'  is not a `familiarCollection` object, or a path to one.
#'
#'  Permutation Variable importance assesses the improvement in model
#'  performance due to a feature. For this purpose, the performance of the model
#'  is measured as normal, and is measured again with a dataset where the values
#'  of the feature in question have been randomly permuted. The difference
#'  between both performance measurements is the permutation variable
#'  importance.
#'
#'  In familiar, this basic concept is extended in several ways:
#'
#'  * Point estimates of variable importance are based on multiple (21) random
#'  permutations. The difference between model performance on the normal dataset
#'  and the median performance measurement of the randomly permuted datasets is
#'  used as permutation variable importance.
#'
#'  * Confidence intervals for the ensemble model are determined using bootstrap
#'  methods.
#'
#'  * Permutation variable importance is assessed for any metric specified using
#'  the `metric` argument.
#'
#'  * Permutation variable importance can take into account similarity between
#'  features and permute similar features simultaneously.
#'
#'@return A data.table (if `dir_path` is not provided), or nothing, as all data
#'  is exported to `csv` files.
#'@exportMethod export_permutation_vimp
#'@md
#'@rdname export_permutation_vimp-methods
setGeneric("export_permutation_vimp",
           function(object, dir_path=NULL, ...) standardGeneric("export_permutation_vimp"))

#####export_permutation_vimp (collection)#####

#'@rdname export_permutation_vimp-methods
setMethod("export_permutation_vimp", signature(object="familiarCollection"),
          function(object, dir_path=NULL, export_raw=FALSE, ...){
            
            return(universal_exporter(object=object,
                                      dir_path=dir_path,
                                      export_raw=export_raw,
                                      data_slot="permutation_vimp",
                                      extra_data=NULL,
                                      target_column="value",
                                      splitting_variable=c("metric", "feature"),
                                      main_type="variable_importance",
                                      sub_type="permutation"))
          })

#####export_permutation_vimp (generic)#####

#'@rdname export_permutation_vimp-methods
setMethod("export_permutation_vimp", signature(object="ANY"),
          function(object, dir_path=NULL, export_raw=FALSE, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="prediction_data"), list(...)))
            
            return(do.call(export_permutation_vimp,
                           args=append(list("object"=object, "dir_path"=dir_path, "export_raw"=export_raw), list(...))))
          })


#####export_hyperparameters#####

#'@title Extract and export model hyperparameters.
#'
#'@description Extract and export model hyperparameters from models in a
#'  familiarCollection.
#'
#'@inheritParams export_all
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
           function(object, dir_path=NULL, ...) standardGeneric("export_hyperparameters"))

#####export_hyperparameters (collection)#####

#'@rdname export_hyperparameters-methods
setMethod("export_hyperparameters", signature(object="familiarCollection"),
          function(object, dir_path=NULL, ...){
            
            parse_hyperparameter_entries_to_string <- function(param){
              
              # Suppress NOTES due to non-standard evaluation in data.table
              instance <- n <- NULL
              
              if(is.character(param) | is.factor(param) | is.logical(param)){
                # Count instances
                dt <- data.table::data.table("instance"=param)
                dt <- dt[, list(n=.N), by=instance][order(n)]
                export_vec <- sapply(seq_len(nrow(dt)), function(ii, dt) (paste0(dt$instance[ii], " (", dt$n[ii],")") ), dt=dt)
                export_vec <- paste(export_vec, collapse="; ")
                
              } else if(is.numeric(param)){
                export_vec <- paste0(stats::quantile(x=param, probs=0.5, na.rm=TRUE, type=1, names=FALSE), " [", min(param, na.rm=TRUE), ", ", max(param, na.rm=TRUE), "]")
              }
              
              return(export_vec)
            }
            
            # Extract the list of hyperparameters from the relevant slot
            main_list <- object@hyperparameters
            
            if(is.null(dir_path)){
              
              # Apply labels
              main_list <- lapply(split(main_list, .apply_labels, object=object))
              
              return(main_list)
              
            } else {
            
              # Parse list into a summary string
              parameter_string <- unname(unlist(lapply(main_list, function(list_entry, object){
                
                # Check if the entry is empty
                if(is_empty(list_entry)){ return(character(0)) }
                
                # Apply labels
                list_entry <- .apply_labels(data=list_entry, object=object)
                
                # Order entries
                data.table::setorder(list_entry, "data_set", "learner", "fs_method")
                
                param_string <- unname(unlist(lapply(split(list_entry, by=c("data_set", "learner", "fs_method")), function(dt_param){
                  if(nrow(dt_param) > 0) {
                    # Parse table distribution based on column class
                    param_string <- sapply(as.list(dt_param[,-c("data_set", "fs_method", "learner", "model_name"), with=FALSE]), parse_hyperparameter_entries_to_string)
                    
                    # Add parameter names into the parameter string
                    param_string <- sapply(seq_len(length(param_string)), function(ii, param_string) (paste0(names(param_string[ii]), ": ", param_string[ii])), param_string=param_string)
                  } else {
                    # Add warning to output
                    param_string <- "optimal hyper-parameters were not found."
                  }
                  
                  # Add learner and fs_method
                  param_string <- c(paste0("data: ", dt_param$data_set[1], "; learner: ", dt_param$learner[1], "; fs_method: ", dt_param$fs_method[1]), param_string, " ")
                  
                  return(param_string)
                })))
                
                return(param_string)
              }, object=object)))
              
              .export_to_file(data=parameter_string, object=object, dir_path=dir_path, type="hyperparameter")
              
              return(NULL)
            }
          })

#####export_hyperparameters (generic)#####

#'@rdname export_hyperparameters-methods
setMethod("export_hyperparameters", signature(object="ANY"),
          function(object, dir_path=NULL, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="hyperparameters"), list(...)))
            
            return(do.call(export_hyperparameters,
                           args=append(list("object"=object, "dir_path"=dir_path), list(...))))
          })

#####export_prediction_data#####

#'@title Extract and export predicted values.
#'
#'@description Extract and export the values predicted by single and ensemble
#'  models in a familiarCollection.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams extract_predictions
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
#'  Both single and ensemble predictions are exported.
#'
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_prediction_data
#'@md
#'@rdname export_prediction_data-methods
setGeneric("export_prediction_data",
           function(object, dir_path=NULL, export_raw=FALSE, ...) standardGeneric("export_prediction_data"))

#####export_prediction_data (collection)#####

#'@rdname export_prediction_data-methods
setMethod("export_prediction_data", signature(object="familiarCollection"),
          function(object, dir_path=NULL, export_raw=FALSE, ...){
            
            return(universal_exporter(object=object,
                                      dir_path=dir_path,
                                      export_raw=export_raw,
                                      data_slot="prediction_data",
                                      extra_data=NULL,
                                      target_column="predicted_outcome",
                                      splitting_variable=NULL,
                                      main_type="prediction",
                                      sub_type=NULL))
          })

#####export_prediction_data (generic)#####

#'@rdname export_prediction_data-methods
setMethod("export_prediction_data", signature(object="ANY"),
          function(object, dir_path=NULL, export_raw=FALSE, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="prediction_data"), list(...)))
            
            return(do.call(export_prediction_data,
                           args=append(list("object"=object, "dir_path"=dir_path, "export_raw"=export_raw), list(...))))
          })



#####export_decision_curve_analysis_data#####

#'@title Extract and export decision curve analysis data.
#'
#'@description Extract and export decision curve analysis data in a
#'  familiarCollection.
#'
#'@inheritParams export_all
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
#'  Decision curve analysis data is computed for categorical outcomes, i.e.
#'  binomial and multinomial, as well as survival outcomes.
#'
#'@return A list of data.table (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_decision_curve_analysis_data
#'@md
#'@rdname export_decision_curve_analysis_data-methods
setGeneric("export_decision_curve_analysis_data", function(object, dir_path=NULL, ...) standardGeneric("export_decision_curve_analysis_data"))

#####export_decision_curve_analysis_data (collection)#####

#'@rdname export_decision_curve_analysis_data-methods
setMethod("export_decision_curve_analysis_data", signature(object="familiarCollection"),
          function(object, dir_path=NULL, export_raw=FALSE, ...){
            
            return(universal_exporter(object=object,
                                      dir_path=dir_path,
                                      export_raw=export_raw,
                                      data_slot="decision_curve_data",
                                      extra_data="intervention_all",
                                      target_column="net_benefit",
                                      splitting_variable="threshold_probability",
                                      main_type="decision_curve_analysis",
                                      sub_type="data"))
          })

#####export_decision_curve_analysis_data (generic)#####

#'@rdname export_decision_curve_analysis_data-methods
setMethod("export_decision_curve_analysis_data", signature(object="ANY"),
          function(object, dir_path=NULL, export_raw=FALSE, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="decision_curve_analyis"), list(...)))
            
            return(do.call(export_decision_curve_analysis_data,
                           args=append(list("object"=object, "dir_path"=dir_path, "export_raw"=export_raw), list(...))))
          })


#####export_calibration_info#####

#'@title Extract and export calibration information.
#'
#'@description Extract and export calibration information (e.g. baseline
#'  survival) for data in a familiarCollection.
#'
#'@inheritParams export_all
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
setGeneric("export_calibration_info", function(object, dir_path=NULL, ...) standardGeneric("export_calibration_info"))

#####export_calibration_info (collection)#####

#'@rdname export_calibration_info-methods
setMethod("export_calibration_info", signature(object="familiarCollection"),
          function(object, dir_path=NULL, ...){
            
            # Extract data
            calibration_info <- object@calibration_info
            
            # Calibration information may not be present
            if(is.null(calibration_info)){
              return(NULL)
            }
            
            # Apply labels
            calibration_info <- .apply_labels(data=calibration_info, object=object)
            
            if(is.null(dir_path)){
              return(calibration_info)
              
            } else {
              # Export to file
              if(object@outcome_type %in% c("survival")){
                subtype <- "baseline_survival"
                
              } else if(object@outcome_type %in% c("continuous", "count")){
                subtype <- "observed_value_range"
                
              } else {
                stop("No subtype defined.")
              }
              
              .export_to_file(data=calibration_info, object=object, dir_path=dir_path,
                              type="calibration", subtype=subtype)
              
              return(NULL)
            }
          })

#####export_calibration_info (generic)#####

#'@rdname export_calibration_info-methods
setMethod("export_calibration_info", signature(object="ANY"),
          function(object, dir_path=NULL, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="calibration_info"), list(...)))
            
            return(do.call(export_calibration_info,
                           args=append(list("object"=object, "dir_path"=dir_path), list(...))))
          })

#####export_calibration_data#####

#'@title Extract and export calibration and goodness-of-fit tests.
#'
#'@description Extract and export calibration and goodness-of-fit tests for data
#'  in a familiarCollection.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams extract_calibration_data
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
#'  Calibration tests are performed based on expected (predicted) and observed
#'  outcomes. For all outcomes, calibration-at-the-large and calibration slopes
#'  are determined. Furthermore, for all but survival outcomes, a repeated,
#'  randomised grouping Hosmer-Lemeshow test is performed. For survival
#'  outcomes, the Nam-D'Agostino and Greenwood-Nam-D'Agostino tests are
#'  performed.
#'
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_calibration_data
#'@md
#'@rdname export_calibration_data-methods
setGeneric("export_calibration_data", function(object, ...) standardGeneric("export_calibration_data"))

#####export_calibration_data (collection)#####

#'@rdname export_calibration_data-methods
setMethod("export_calibration_data", signature(object="familiarCollection"),
          function(object, dir_path=NULL, ...){
            
            # Extract data
            main_list <- object@calibration_data
            
            # Calibration at large
            calibration_at_large <- .apply_labels(data=main_list$linear_test, object=object)
            
            # Calibration goodness of fit test
            calibration_test <- .apply_labels(data=main_list$gof_test, object=object)
            
            # Calibration data
            calibration_data <- .apply_labels(data=main_list$data, object=object)
            
            if(is.null(dir_path)){
              return(list("linear_test"=calibration_at_large,
                          "gof_test"=calibration_test,
                          "data"=calibration_data))
              
            } else {
              # Export calibration at large
              .export_to_file(data=calibration_at_large, object=object, dir_path=dir_path,
                              type="calibration", subtype="at_large")
              
              # Export calibration goodness-of-fit tests
              .export_to_file(data=calibration_test, object=object, dir_path=dir_path,
                              type="calibration", subtype="gof_test")
              
              # Export calibration data points
              .export_to_file(data=calibration_data, object=object, dir_path=dir_path,
                              type="calibration", subtype="data")
              
              return(NULL)
            }
          })

#####export_calibration_data (generic)#####

#'@rdname export_calibration_data-methods
setMethod("export_calibration_data", signature(object="ANY"),
          function(object, dir_path=NULL, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="calibration_data"), list(...)))
            
            return(do.call(export_calibration_data,
                           args=append(list("object"=object, "dir_path"=dir_path), list(...))))
          })

#####export_stratification_cutoff#####

#'@title Extract and export cut-off values for risk group stratification.
#'
#'@description Extract and export cut-off values for risk group stratification
#'  by models in a familiarCollection.
#'
#'@inheritParams export_all
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
#'  Stratification cut-off values are determined when creating a model, using
#'  one of several methods set by the `stratification_method` parameter. These
#'  values are then used to stratify samples in any new dataset. The available
#'  methods are:
#'
#'  * `median` (default): The median predicted value in the development cohort
#'  is used to stratify the samples into two risk groups.
#'
#'  * `fixed`: Samples are stratified based on the sample quantiles of the
#'  predicted values. These quantiles are defined using the
#'  `stratification_threshold` parameter.
#'
#'  * `optimised`: Use maximally selected rank statistics to determine the
#'  optimal threshold (Lausen and Schumacher, 1992; Hothorn et al., 2003) to
#'  stratify samples into two optimally separated risk groups.
#'
#'@return A data.table (if `dir_path` is not provided), or nothing, as all data
#'  is exported to `csv` files.
#'@references 1. Lausen, B. & Schumacher, M. Maximally Selected Rank Statistics.
#'  Biometrics 48, 73 (1992).
#'
#'  1. Hothorn, T. & Lausen, B. On the exact distribution of maximally selected
#'  rank statistics. Comput. Stat. Data Anal. 43, 121–137 (2003).
#'@exportMethod export_stratification_cutoff
#'@md
#'@rdname export_stratification_cutoff-methods
setGeneric("export_stratification_cutoff",
           function(object, dir_path=NULL, ...) standardGeneric("export_stratification_cutoff"))

#####export_stratification_cutoff (collection)#####

#'@rdname export_stratification_cutoff-methods
setMethod("export_stratification_cutoff", signature(object="familiarCollection"),
          function(object, dir_path=NULL, ...){

            # Check if the attribute has any contents
            if(is.null(object@km_info)){
              return(NULL)
            }
            
            # Update threshold table by applying labels
            km_info <- .apply_labels(data=object@km_info, object=object)
            
            if(is.null(dir_path)){
              return(km_info)
              
            } else {
              # Export to file
              .export_to_file(data=km_info, object=object, dir_path=dir_path,
                              type="stratification", subtype="cutoff_values")
              
              return(NULL)
            }
            
          })

#####export_stratification_cutoff (generic)#####

#'@rdname export_stratification_cutoff-methods
setMethod("export_stratification_cutoff", signature(object="ANY"),
          function(object, dir_path=NULL, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="kaplan_meier_info"), list(...)))
            
            return(do.call(export_stratification_cutoff,
                           args=append(list("object"=object, "dir_path"=dir_path), list(...))))
          })

#####export_stratification_data#####

#'@title Extract and export sample risk group stratification and associated
#'  tests.
#'
#'@description Extract and export sample risk group stratification and
#'  associated tests for data in a familiarCollection.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams extract_stratification_data
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
#'  Three tables are exported in a list:
#'
#'  * `data`: Contains the assigned risk group for a given sample, along with
#'  its reported survival time and censoring status.
#'
#'  * `hr_ratio`: Contains the hazard ratio between different risk groups.
#'
#'  * `logrank`: Contains the results from the logrank test between different
#'  risk groups.
#'
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_stratification_data
#'@md
#'@rdname export_stratification_data-methods
setGeneric("export_stratification_data",
           function(object, dir_path=NULL, ...) standardGeneric("export_stratification_data"))

#####export_stratification_data (collection)#####

#'@rdname export_stratification_data-methods
setMethod("export_stratification_data", signature(object="familiarCollection"),
          function(object, dir_path=NULL, ...){
            
            # Check if the attribute has any contents
            if(is.null(object@km_data)){
              return(NULL)
            }
            
            # Extract data
            main_list <- object@km_data
            
            # Collect data
            km_data_data <- lapply(main_list, function(list_entry, object){
              return(.apply_labels(data=list_entry$data, object=object))
            }, object=object)
            
            # Collect logrank data
            km_data_logrank <- lapply(main_list, function(list_entry, object){
              # Apply labels
              data <- .apply_labels(data=list_entry$logrank, object=object)
              
              # Restore "all" as level in risk groups 1 and 2
              data$risk_group_1 <- factor(data$risk_group_1, levels=c(NA, levels(data$risk_group_1)),
                                          labels=c("all", levels(data$risk_group_1)), exclude=NULL)
              
              data$risk_group_2 <- factor(data$risk_group_2, levels=c(NA, levels(data$risk_group_2)),
                                          labels=c("all", levels(data$risk_group_2)), exclude=NULL)
              
              return(data)
            }, object=object)
            
            # Collect HR-ratio test data
            km_data_hr_ratio <- lapply(main_list, function(list_entry, object){
              return(.apply_labels(data=list_entry$hr_ratio, object=object))
            }, object=object)
            
            # Collect time-max
            time_max <- unname(unlist(sapply(main_list, function(list_entry) (list_entry$time_max))))[1]
            
            if(is.null(dir_path)){
              return(list("data"=km_data_data,
                          "logrank"=km_data_logrank,
                          "hr_ratio"=km_data_hr_ratio,
                          "time_max"=time_max))
              
            } else {
              
              # Export stratification data
              sapply(names(km_data_data), function(strat_method, data, object, dir_path){
                .export_to_file(data=data[[strat_method]], object=object, dir_path=dir_path,
                                type="stratification", subtype=paste0(strat_method, "_stratification_data"))
              }, data=km_data_data, object=object, dir_path=dir_path)
              
              # Export logrank test data
              sapply(names(km_data_logrank), function(strat_method, data, object, dir_path){
                .export_to_file(data=data[[strat_method]], object=object, dir_path=dir_path,
                                type="stratification", subtype=paste0(strat_method, "_logrank_test"))
              }, data=km_data_logrank, object=object, dir_path=dir_path)
              
              # Export HR-ratio test data
              sapply(names(km_data_hr_ratio), function(strat_method, data, object, dir_path){
                .export_to_file(data=data[[strat_method]], object=object, dir_path=dir_path,
                                type="stratification", subtype=paste0(strat_method, "_hazard_ratio_test"))
              }, data=km_data_hr_ratio, object=object, dir_path=dir_path)
              
              return(NULL)
            }
          })

#####export_stratification_data (generic)#####

#'@rdname export_stratification_data-methods
setMethod("export_stratification_data", signature(object="ANY"),
          function(object, dir_path=NULL, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="kaplan_meier_data"), list(...)))
            
            return(do.call(export_stratification_data,
                           args=append(list("object"=object, "dir_path"=dir_path), list(...))))
          })


#####export_model_performance#####

#'@title Extract and export metrics for model performance.
#'
#'@description Extract and export metrics for model performance of models in a
#'  familiarCollection.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams extract_performance
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
#'  Performance of individual and ensemble models is exported. For ensemble
#'  models, a credibility interval is determined using bootstrapping for each
#'  metric.
#'
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_model_performance
#'@md
#'@rdname export_model_performance-methods
setGeneric("export_model_performance",
           function(object, dir_path=NULL, export_raw=FALSE, ...) standardGeneric("export_model_performance"))

#####export_model_performance (collection)#####

#'@rdname export_model_performance-methods
setMethod("export_model_performance", signature(object="familiarCollection"),
          function(object, dir_path=NULL, export_raw=FALSE, ...){
            
            return(universal_exporter(object=object,
                                      dir_path=dir_path,
                                      export_raw=export_raw,
                                      data_slot="model_performance",
                                      extra_data=NULL,
                                      target_column="value",
                                      splitting_variable="metric",
                                      main_type="performance",
                                      sub_type="metric"))
          })
          
#####export_model_performance (generic)#####

#'@rdname export_model_performance-methods
setMethod("export_model_performance", signature(object="ANY"),
          function(object, dir_path=NULL, export_raw=FALSE, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="model_performance"), list(...)))
            
            return(do.call(export_model_performance,
                           args=append(list("object"=object, "dir_path"=dir_path, "export_raw"=export_raw),
                                       list(...))))
          })

#####export_auc_data#####

#'@title Extract and export ROC-curves.
#'
#'@description Extract and export ROC-curves for models in a familiarCollection.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams extract_auc_data
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
#'  ROC curve data are exported for individual and ensemble models. For ensemble
#'  models, a credibility interval for the ROC curve is determined using
#'  bootstrapping for each metric. In case of multinomial outcomes, ROC-curves
#'  are computed for each class, using a one-against-all approach.
#'
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_auc_data
#'@md
#'@rdname export_auc_data-methods
setGeneric("export_auc_data", function(object, dir_path=NULL, export_raw=FALSE, ...) standardGeneric("export_auc_data"))

#####export_auc_data (collection)#####

#'@rdname export_auc_data-methods
setMethod("export_auc_data", signature(object="familiarCollection"),
          function(object, dir_path=NULL, export_raw=FALSE, ...){
            
            return(universal_exporter(object=object,
                                      dir_path=dir_path,
                                      export_raw=export_raw,
                                      data_slot="auc_data",
                                      extra_data=NULL,
                                      target_column="tpr",
                                      splitting_variable="fpr",
                                      main_type="performance",
                                      sub_type="roc"))
          })

#####export_auc_data (generic)#####

#'@rdname export_auc_data-methods
setMethod("export_auc_data", signature(object="ANY"),
          function(object, dir_path=NULL, export_raw=FALSE, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="auc_data"), list(...)))
            
            return(do.call(export_auc_data,
                           args=append(list("object"=object, "dir_path"=dir_path, "export_raw"=export_raw),
                                       list(...))))
          })



#####export_confusion_matrix_data#####

#'@title Extract and export confusion matrices.
#'
#'@description Extract and export confusion matrics for models in a
#'  familiarCollection.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams extract_confusion_matrix
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
#'  Confusion matrices are exported for individual and ensemble models.
#'
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_confusion_matrix_data
#'@md
#'@rdname export_confusion_matrix_data-methods
setGeneric("export_confusion_matrix_data",
           function(object, dir_path=NULL, export_raw=FALSE, ...) standardGeneric("export_confusion_matrix_data"))

#####export_confusion_matrix_data (collection)#####

#'@rdname export_confusion_matrix_data-methods
setMethod("export_confusion_matrix_data", signature(object="familiarCollection"),
          function(object, dir_path=NULL, export_raw=FALSE, ...){
            
            return(universal_exporter(object=object,
                                      dir_path=dir_path,
                                      export_raw=export_raw,
                                      data_slot="confusion_matrix",
                                      extra_data=NULL,
                                      target_column="count",
                                      splitting_variable=NULL,
                                      main_type="performance",
                                      sub_type="confusion_matrix"))
          })

#####export_confusion_matrix_data (generic)#####

#'@rdname export_confusion_matrix_data-methods
setMethod("export_confusion_matrix_data", signature(object="ANY"),
          function(object, dir_path=NULL, export_raw=FALSE, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="confusion_matrix"), list(...)))
            
            return(do.call(export_confusion_matrix_data,
                           args=append(list("object"=object, "dir_path"=dir_path, "export_raw"=FALSE,), list(...))))
          })



#####export_univariate_analysis_data#####

#'@title Extract and export univariate analysis data of features.
#'
#'@description Extract and export univariate analysis data of features for data
#'  in a familiarCollection.
#'
#'@inheritParams export_all
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
           function(object, dir_path=NULL, ...) standardGeneric("export_univariate_analysis_data"))

#####export_univariate_analysis_data (collection)#####

#'@rdname export_univariate_analysis_data-methods
setMethod("export_univariate_analysis_data", signature(object="familiarCollection"),
          function(object, dir_path=NULL, ...){

            # Process univariate analysis data
            univariate_analysis <- .apply_labels(data=object@univariate_analysis$data, object=object)
            
            if(is.null(dir_path)){
              return(univariate_analysis)
              
            } else {
              .export_to_file(data=univariate_analysis, object=object, dir_path=dir_path,
                              type="variable_importance", subtype="univariate")
              
              return(NULL)
            }
          })

#####export_univariate_analysis_data (generic)#####

#'@rdname export_univariate_analysis_data-methods
setMethod("export_univariate_analysis_data", signature(object="ANY"),
          function(object, dir_path=NULL, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="univariate_analysis"), list(...)))
            
            return(do.call(export_univariate_analysis_data,
                           args=append(list("object"=object, "dir_path"=dir_path), list(...))))
          })


#####export_feature_expressions#####

#'@title Extract and export feature expressions.
#'
#'@description Extract and export feature expressions for the features in a
#'  familiarCollection.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams extract_feature_expression
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
#'  Feature expressions are computed by standardising each feature, i.e. sample
#'  mean is 0 and standard deviation is 1.
#'
#'@return A data.table (if `dir_path` is not provided), or nothing, as all data
#'  is exported to `csv` files.
#'@exportMethod export_feature_expressions
#'@md
#'@rdname export_feature_expressions-methods
setGeneric("export_feature_expressions",
           function(object, dir_path=NULL, ...) standardGeneric("export_feature_expressions"))

#####export_feature_expressions (collection)#####

#'@rdname export_feature_expressions-methods
setMethod("export_feature_expressions", signature(object="familiarCollection"),
          function(object, dir_path=NULL, ...){

            if(is.null(object@feature_expressions)){
              return(NULL)
            }
            
            if(is.null(dir_path)){
              # Return list of feature expressions with updated labels.
              feature_expression_list <- lapply(object@feature_expressions, function(list_entry, object){
                
                # Add labels.
                list_entry$data <- .apply_labels(data=list_entry$data, object=object)
                
                return(list_entry)
              }, object=object)
              
              return(feature_expression_list)
              
            } else {
              # Label feature expressions.
              feature_expressions <- lapply(object@feature_expressions, function(list_entry, object){
                return(.apply_labels(data=list_entry$data, object=object))
              }, object=object)
              
              # Export expressions tables separately.
              lapply(seq_len(length(feature_expressions)), function(ii, feature_expressions, object, dir_path){
                     .export_to_file(data=feature_expressions[[ii]], object=object, dir_path=dir_path,
                                     type="feature_expression", subtype=ii)
              }, feature_expressions=feature_expressions, object=object, dir_path=dir_path)
              
              return(NULL)
            }
          })

#####export_feature_expressions (generic)#####

#'@rdname export_feature_expressions-methods
setMethod("export_feature_expressions", signature(object="ANY"),
          function(object, dir_path=NULL, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="feature_expressions"), list(...)))
            
            return(do.call(export_feature_expressions,
                           args=append(list("object"=object, "dir_path"=dir_path), list(...))))
          })


#####export_feature_similarity#####

#'@title Extract and export mutual correlation between features.
#'
#'@description Extract and export mutual correlation between features in a
#'  familiarCollection.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams extract_mutual_correlation
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
#'@return A list containing a data.table (if `dir_path` is not provided), or
#'  nothing, as all data is exported to `csv` files.
#'@exportMethod export_feature_similarity
#'@md
#'@rdname export_feature_similarity-methods
setGeneric("export_feature_similarity",
           function(object, dir_path=NULL, ...) standardGeneric("export_feature_similarity"))

#####export_feature_similarity (collection)#####

#'@rdname export_feature_similarity-methods
setMethod("export_feature_similarity", signature(object="familiarCollection"),
          function(object, dir_path=NULL, ...){
          
            if(is.null(object@mutual_correlation)){
              return(NULL)
            }
            
            if(is.null(dir_path)){
              # Return list of mutual correlation data with updated labels.
              mutual_correlation_list <- lapply(object@mutual_correlation, function(list_entry, object){
                
                # Add labels.
                list_entry$data <- .apply_labels(data=list_entry$data, object=object)
                
                return(list_entry)
              }, object=object)
              
              return(mutual_correlation_list)
              
            } else {
              # Update labels on mutual correlation data table.
              mutual_correlation_data <- data.table::rbindlist(lapply(object@mutual_correlation, function(list_entry, object){
                return(.apply_labels(data=list_entry$data, object=object))
              }, object=object), use.names=TRUE)
              
              .export_to_file(data=mutual_correlation_data, object=object, dir_path=dir_path, type="feature_similarity")
              
              return(NULL)
            }
          })


#####export_feature_similarity (generic)#####

#'@rdname export_feature_similarity-methods
setMethod("export_feature_similarity", signature(object="ANY"),
          function(object, dir_path=NULL, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=append(list("object"=object, "data_element"="mutual_correlation"), list(...)))
            
            return(do.call(export_feature_similarity,
                           args=append(list("object"=object, "dir_path"=dir_path), list(...))))
          })


#####.summarise_model_performance#####
setMethod(".summarise_model_performance", signature(object="familiarCollection"),
          function(object, data=NULL, metrics=NULL, confidence_level=NULL){
            # Ensemble model performance contains bootstrapped performance metrics.
            # This method summarises this data and extracts confidence intervals.
            
            if(is.null(data)){
              data <- .apply_labels(data=object@model_performance$ensemble, object=object)
            }
            
            if(is.null(metrics)){
              metrics <- object@model_performance$metric
            }
            
            if(is.null(confidence_level)){
              confidence_level <- object@model_performance$confidence_level
            }
            
            # Identify the columns containing metric values
            metric_columns <- paste0("performance_", metrics)

            # Compute descriptive statistics for each column
            ensemble_performance <- data[, sapply(.SD, ..bootstrap_ci, confidence_level=confidence_level),
                                         by=c("data_set", "fs_method", "learner", "model_name"),
                                         .SDcols=metric_columns]
            
            # Define new column names
            new_col_names <- as.vector(sapply(metrics, function(curr_metric) (paste0(curr_metric, c("_median", "_ci_low", "_ci_up")))))
            
            # Update column names in the ensemble_performance table
            data.table::setnames(x=ensemble_performance, old=paste0("V", seq_len(length(new_col_names))), new=new_col_names)
            
            return(ensemble_performance)
          })


#####.export_to_file#####
setMethod(".export_to_file", signature(data="ANY", object="familiarCollection", dir_path="character"),
          function(data, object, dir_path, type, subtype=NULL){
            
            # Check if data exists
            if(is_empty(data)) { return(NULL) }
            
            # Check if directory exists
            file_dir <- normalizePath(file.path(dir_path, object@collection_name, type), mustWork=FALSE)
            if(!dir.exists(file_dir)) { dir.create(file_dir, recursive=TRUE) }
            
            base_file_name <- paste0(type, ifelse(is.null(subtype), "", paste0("_", subtype)))
            
            if(inherits(data, "data.table")){
              # Generate file name
              file_name <- file.path(file_dir, paste0(base_file_name, ".csv"))
              
              # Write data to file
              data.table::fwrite(x=data, file=file_name, sep=";", dec=".")
              
            } else if(inherits(data, "character")){
              # Generate file name
              file_name <- file.path(file_dir, paste0(base_file_name, ".txt"))
              
              # Write to text file with each element of x on a new line.
              write(x=data, file=file_name, append=FALSE, sep=ifelse(.Platform$OS.type=="windows", "\r\n","\n"))
              
            }
          })

#####.apply_labels#####
setMethod(".apply_labels", signature(data="ANY", object="familiarCollection"),
          function(data, object){
            
            # Return NULL for NULL input
            if(is.null(data)) return(NULL)
            
            # Check if data has the expected class.
            if(!data.table::is.data.table(data)){
              
              if(is.list(data)){
                # Get the names of the list elements.
                element_names <- names(data)
                
                # Update underlying data
                data <- lapply(data, .apply_labels, object=object)
                
                # Set the names
                names(data) <- element_names
                
                return(data)
              } else if(is.numeric(data) | is.character(data) | is.logical(data) | is.factor(data)){
                return(data)
                
              } else {
                stop(paste0("\"data\" is expected to be a \"data.frame\" or \"data.table\", or a list of these. Found: ", paste(class(data), sep=", "), "."))
              }
            }

            # Make sure that a local copy is updated
            data <- data.table::copy(data)
            
            # Check which labels are present, based on column names
            columns <- colnames(data)
            
            has_data_set           <- ifelse("data_set" %in% columns, TRUE, FALSE)
            has_learner            <- ifelse("learner" %in% columns, TRUE, FALSE)
            has_fs_method          <- ifelse("fs_method" %in% columns, TRUE, FALSE)
            has_feature            <- ifelse(any(c("name", "feature_1", "feature_2") %in% columns), TRUE, FALSE)
            has_risk_group         <- ifelse(any(c("risk_group", "risk_group_1", "risk_group_2", "reference_group") %in% columns), TRUE, FALSE)
            has_multiclass_outcome <- ifelse(any(c("pos_class", "outcome") %in% columns) & object@outcome_type=="multinomial", TRUE, FALSE)
            has_categorical_outcome <- ifelse(any(c("observed_outcome", "expected_outcome") %in% columns) & object@outcome_type %in% c("binomial", "multinomial"),
                                              TRUE, FALSE)
            has_evaluation_time <- any(c("evaluation_time", "eval_time") %in% columns) & object@outcome_type %in% c("survival", "competing_risk")
            
            # Apply levels
            if(has_data_set){
              data$data_set <- factor(x=data$data_set, levels=get_data_set_name_levels(x=object), labels=get_data_set_names(x=object))
            }
            
            if(has_learner){
              data$learner <- factor(x=data$learner, levels=get_learner_name_levels(x=object), labels=get_learner_names(x=object))
            }
            
            if(has_fs_method){
              data$fs_method <- factor(x=data$fs_method, levels=get_fs_method_name_levels(x=object), labels=get_fs_method_names(x=object))
            }
            
            if(has_feature){
              for(curr_col_name in c("name", "feature_1", "feature_2")){
                if(!is.null(data[[curr_col_name]])){
                  data[[curr_col_name]] <- factor(x=data[[curr_col_name]], levels=get_feature_name_levels(x=object), labels=get_feature_names(x=object))
                }
              }
            }
            
            if(has_risk_group){
              for(curr_col_name in c("risk_group", "risk_group_1", "risk_group_2", "reference_group")){
                if(!is.null(data[[curr_col_name]])){
                  data[[curr_col_name]] <- factor(x=data[[curr_col_name]], levels=get_risk_group_name_levels(x=object), labels=get_risk_group_names(x=object))
                }
              }
            }
            
            if(has_multiclass_outcome){
              for(curr_col_name in c("pos_class", "outcome")){
                if(!is.null(data[[curr_col_name]])){
                  data[[curr_col_name]] <- factor(x=data[[curr_col_name]], levels=get_class_name_levels(x=object), labels=get_class_names(x=object))
                }
              }
            }
            
            if(has_categorical_outcome){
              # For confusion matrices.
              for(curr_col_name in c("observed_outcome", "expected_outcome")){
                if(!is.null(data[[curr_col_name]])){
                  data[[curr_col_name]] <- factor(x=data[[curr_col_name]], levels=get_class_name_levels(x=object), labels=get_class_names(x=object))
                }
              }
            }
            
            if(has_evaluation_time){
              for(curr_col_name in c("evaluation_time", "eval_time")){
                if(!is.null(data[[curr_col_name]])){
                  data[[curr_col_name]] <- factor(x=data[[curr_col_name]])
                }
              }
            }
            
            # Drop unused levels
            data <- droplevels(x=data)
            
            return(data)
          })


universal_exporter <- function(object,
                               dir_path=NULL,
                               export_raw=FALSE,
                               data_slot,
                               extra_data=NULL,
                               target_column,
                               splitting_variable=NULL,
                               main_type,
                               sub_type=NULL){
  
  # Extract list of lists
  main_list <- slot(object=object, name=data_slot)
                    
  # This list will be filled.
  export_list <- list()
  
  for(type in c("individual", "ensemble")){
    # Confidence level
    confidence_level <- main_list[[type]]$confidence_level
    
    # Apply labels.
    data <- .apply_labels(data=main_list[[type]], object=object)
    
    if(!export_raw){
      # Export summarised data.
      
      export_data <- list("data"=.compute_bootstrap_ci(x0=data$model_data,
                                                       xb=data$bootstrap_data,
                                                       target_column=target_column,
                                                       bootstrap_ci_method=data$bootstrap_ci_method,
                                                       additional_splitting_variable=splitting_variable,
                                                       confidence_level=confidence_level),
                          "confidence_level"=confidence_level)
      
      if(!is.null(extra_data)){
        
        # Retrieve extra data.
        extra_export_data <- lapply(extra_data, function(list_element, data) data[[list_element]], data=data)
        
        # Rename exported data.
        names(extra_export_data) <- extra_data
        
        # Join the two lists.
        export_data <- c(export_data, extra_export_data)
      }
      
      
      if(!is.null(dir_path)){
        # Export to file.
        
        # Extract the summarised model data.
        write_data <- export_data$data
        
        if(!is.null(extra_data)){
          
          # Identify the identifier columns
          id_columns <- setdiff(colnames(write_data),
                                c(target_column, "ci_low", "ci_up"))
          
          for(current_data_set in extra_data){
            
            # Skip empty datasets.
            if(is_empty(export_data[[current_data_set]])) next()
            
            # Parse current dataset
            current_data <- export_data[[current_data_set]]
            data.table::setnames(current_data, old=target_column, new=current_data_set)
            
            # Merge with summarised data.
            write_data <- merge(x=write_data,
                                y=current_data,
                                by=id_columns)
          }
        }
        
        # Export model performances of the models
        .export_to_file(data=write_data, object=object, dir_path=dir_path,
                        type=main_type, subtype=paste(sub_type, type, sep="_")) 
      }
      
    } else {
      # Export all the data.
      
      if(!is.null(dir_path)){
        # Prepare data for writing by combining, model, intervention
        # and bootstrap data into a single table before writing it to
        # a folder.
        
        if(!is_empty(data$bootstrap_data)){
          # Cast wide by bootstrap id.
          bootstrap_data <- dcast(data=data$bootstrap_data,
                                  stats::reformulate(termlabels=setdiff(colnames(individual_export_data), c(target_column, "bootstrap_id")),
                                                     response="bootstrap_id",
                                                     intercept=FALSE),
                                  value.var=targer_column)
        } else {
          bootstrap_data <- NULL
        }
        
        if(!is_empty(data$model_data)){
          
          # Parse model data.
          write_data <- data$model_data
          setnames(export_data, old=target_column, new="model")
          
          # Specify identifier columns
          id_columns <- setdiff(colnames(write_data), c("model", "ci_low", "ci_up"))
          
          if(!is.null(bootstrap_data)){
            write_data <- merge(x=write_data,
                                y=bootstrap_data,
                                by=id_columns)
          }
          
        } else {
          write_data <- NULL
        }
        
        if(!is.null(write_data) & !is.null(extra_data)){
          
          for(current_data_set in extra_data){
            
            # Skip empty datasets.
            if(is_empty(write_data[[current_data_set]])) next()
            
            # Parse current dataset
            current_data <- data[[current_data_set]]
            data.table::setnames(current_data, old=target_column, new=current_data_set)
            
            # Merge with summarised data.
            write_data <- merge(x=write_data,
                                y=current_data,
                                by=id_columns)
          }
        }

        # Export data to file.
        .export_to_file(data=export_data, object=object, dir_path=dir_path,
                        type=main_type, subtype=paste(sub_type, type, sep="_"))
        
      } else {
        # Data is exported directly.
        export_data <- data
      }
    }
    
    # Add export data to list.
    export_list[[type]] <- export_data
  }
  
  # Return list of data.
  if(is.null(dir_path)) return(export_list)
}

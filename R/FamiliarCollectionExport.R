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
           function(object, dir_path=NULL, aggregate_results=waiver(), ...) standardGeneric("export_all"))


#####export_all (collection)#####

#'@rdname export_all-methods
setMethod("export_all", signature(object="familiarCollection"),
          function(object, dir_path=NULL, aggregate_results=waiver(), ...){
            
            # Export feature selection variable importance
            fs_vimp <- export_fs_vimp(object=object, dir_path=dir_path)
            
            # Export model variable importance
            model_vimp <- export_model_vimp(object=object, dir_path=dir_path)
            
            # Export permutation variable importance.
            permutation_vimp <- export_permutation_vimp(object=object,
                                                        dir_path=dir_path,
                                                        aggregate_results=ifelse(is.waive(aggregate_results), TRUE, aggregate_results))
            
            # Export model hyperparameters
            hyperparameters <- export_hyperparameters(object=object,
                                                      dir_path=dir_path,
                                                      aggregate_results=ifelse(is.waive(aggregate_results), TRUE, aggregate_results))
            
            # Export prediction tables
            prediction_data <- export_prediction_data(object=object,
                                                      dir_path=dir_path)
            
            # Export decision curve analysis data
            dca_data <- export_decision_curve_analysis_data(object=object,
                                                            dir_path=dir_path,
                                                            aggregate_results=ifelse(is.waive(aggregate_results), TRUE, aggregate_results))
            
            # Export calibration information
            calibration_info <- export_calibration_info(object=object, dir_path=dir_path)
            
            # Export calibration data
            calibration_data <- export_calibration_data(object=object,
                                                        dir_path=dir_path,
                                                        aggregate_results=ifelse(is.waive(aggregate_results), TRUE, aggregate_results))
            
            # Export model performance
            model_performance <- export_model_performance(object=object,
                                                          dir_path=dir_path,
                                                          aggregate_results=ifelse(is.waive(aggregate_results), TRUE, aggregate_results))
            
            # Export confusion matrix
            confusion_matrix <- export_confusion_matrix_data(object=object,
                                                             dir_path=dir_path)
            
            # Export kaplan-meier info
            km_info <- export_risk_stratification_info(object=object, dir_path=dir_path)
            
            # Export stratification data
            km_data <- export_risk_stratification_data(object=object,
                                                       dir_path=dir_path)
            
            # Export AUC data
            auc_data <- export_auc_data(object=object,
                                        dir_path=dir_path,
                                        aggregate_results=ifelse(is.waive(aggregate_results), TRUE, aggregate_results))
            
            # Export data from the univariate analysis
            univariate_analysis <- export_univariate_analysis_data(object=object, dir_path=dir_path)
            
            # Export data from feature expressions
            feature_expressions <- export_feature_expressions(object=object, dir_path=dir_path)
            
            # Export mutual-correlation data
            feature_similarity <- export_feature_similarity(object=object, dir_path=dir_path)
            
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
                          "feature_similarity" = feature_similarity))
            }
          })

#####export_all (generic)#####

#'@rdname export_all-methods
setMethod("export_all", signature(object="ANY"),
          function(object, dir_path=NULL, aggregate_results=waiver(), ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object,
                                          "aggregate_results"=aggregate_results),
                                     list(...)))
            
            return(do.call(export_all,
                           args=c(list("object"=object,
                                       "dir_path"=dir_path,
                                       "aggregate_results"=aggregate_results),
                                  list(...))))
          })




#####.export_to_file (familiarDataElement)######################################
setMethod(".export_to_file", signature(data="familiarDataElement", object="familiarCollection", dir_path="character"),
          function(data, object, dir_path, type, subtype=NULL){
            
            if(is_empty(data)) return(NULL)
            
            # Check if any identifiers remain, and add to the subtype.
            if(length(data@identifiers) > 0) subtype <- c(subtype, unlist(data@identifiers))
            
            return(.export_to_file(data@data,
                                   object=object,
                                   dir_path=dir_path,
                                   type=type,
                                   subtype=subtype))
          })


#####.export_to_file (list)#####################################################
setMethod(".export_to_file", signature(data="list", object="familiarCollection", dir_path="character"),
          function(data, object, dir_path, type, subtype=NULL){
            
            # Check if data exists
            if(is_empty(data)) return(NULL)
            
            return(lapply(data,
                          .export_to_file,
                          object=object,
                          dir_path=dir_path,
                          type=type,
                          subtype=subtype))
          })



#####.export_to_file (data.table)#####################################################
setMethod(".export_to_file", signature(data="data.table", object="familiarCollection", dir_path="character"),
          function(data, object, dir_path, type, subtype=NULL){
            
            # Check if data exists.
            if(is_empty(data)) return(NULL)
            
            # Check if directory exists.
            file_dir <- normalizePath(file.path(dir_path, object@name, type), mustWork=FALSE)
            if(!dir.exists(file_dir)) dir.create(file_dir, recursive=TRUE)
            
            # Generate file name.
            base_file_name <- paste0(type, subtype, collapse="_")
            file_name <- file.path(file_dir, paste0(base_file_name, ".csv"))
            
            # Write data to file.
            data.table::fwrite(x=data,
                               file=file_name,
                               sep=";",
                               dec=".")
            
            return(NULL)
          })


#####.export_to_file (character)################################################
setMethod(".export_to_file", signature(data="character", object="familiarCollection", dir_path="character"),
          function(data, object, dir_path, type, subtype=NULL){
            
            # Check if data exists
            if(is_empty(data)) return(NULL)
            
            # Check if directory exists
            file_dir <- normalizePath(file.path(dir_path, object@name, type), mustWork=FALSE)
            if(!dir.exists(file_dir)) dir.create(file_dir, recursive=TRUE)
            
            # Generate file name
            base_file_name <- paste0(type, subtype, collapse="_")
            file_name <- file.path(file_dir, paste0(base_file_name, ".txt"))
            
            # Write to text file with each element of x on a new line.
            write(x=data,
                  file=file_name,
                  append=FALSE,
                  sep=ifelse(.Platform$OS.type=="windows", "\r\n","\n"))
            
            return(NULL)
          })




#####.apply_labels (data.table, familiarCollection)#####
setMethod(".apply_labels", signature(data="data.table", object="familiarCollection"),
          function(data, object){
            browser()
            # Return NULL for empty input
            if(is_empty(data)) return(NULL)
            
            # Check if data has the expected class.
            # if(!data.table::is.data.table(data)){
            #   
            #   if(is.list(data)){
            #     # Get the names of the list elements.
            #     element_names <- names(data)
            #     
            #     # Update underlying data
            #     data <- lapply(data, .apply_labels, object=object)
            #     
            #     # Set the names
            #     names(data) <- element_names
            #     
            #     return(data)
            #   } else if(is.numeric(data) | is.character(data) | is.logical(data) | is.factor(data)){
            #     return(data)
            #     
            #   } else {
            #     stop(paste0("\"data\" is expected to be a \"data.frame\" or \"data.table\", or a list of these. Found: ", paste(class(data), sep=", "), "."))
            #   }
            # }

            # Make sure that a local copy is updated
            data <- data.table::copy(data)
            
            # Check which labels are present, based on column names
            columns <- colnames(data)
            
            has_data_set           <- ifelse("data_set" %in% columns, TRUE, FALSE)
            has_learner            <- ifelse("learner" %in% columns, TRUE, FALSE)
            has_fs_method          <- ifelse("fs_method" %in% columns, TRUE, FALSE)
            has_feature            <- ifelse(any(c("name", "feature_name_1", "feature_name_2", "feature") %in% columns), TRUE, FALSE)
            has_risk_group         <- ifelse(any(c("risk_group", "risk_group_1", "risk_group_2", "reference_group") %in% columns), TRUE, FALSE)
            has_multiclass_outcome <- ifelse(any(c("pos_class", "outcome") %in% columns) & object@outcome_type=="multinomial", TRUE, FALSE)
            has_categorical_outcome <- ifelse(any(c("observed_outcome", "expected_outcome") %in% columns) & object@outcome_type %in% c("binomial", "multinomial"),
                                              TRUE, FALSE)
            has_evaluation_time <- any(c("evaluation_time", "eval_time") %in% columns) & object@outcome_type %in% c("survival", "competing_risk")
            has_performance_metric <- any(c("metric") %in% columns)
            
            # Apply levels
            if(has_data_set){
              data$data_set <- factor(x=data$data_set,
                                      levels=get_data_set_name_levels(x=object),
                                      labels=get_data_set_names(x=object))
            }
            
            if(has_learner){
              data$learner <- factor(x=data$learner,
                                     levels=get_learner_name_levels(x=object),
                                     labels=get_learner_names(x=object))
            }
            
            if(has_fs_method){
              data$fs_method <- factor(x=data$fs_method,
                                       levels=get_fs_method_name_levels(x=object),
                                       labels=get_fs_method_names(x=object))
            }
            
            if(has_feature){
              for(curr_col_name in c("name", "feature_name_1", "feature_name_2", "feature")){
                if(!is.null(data[[curr_col_name]])){
                  data[[curr_col_name]] <- factor(x=data[[curr_col_name]],
                                                  levels=get_feature_name_levels(x=object),
                                                  labels=get_feature_names(x=object))
                }
              }
            }
            
            if(has_risk_group){
              for(curr_col_name in c("risk_group", "risk_group_1", "risk_group_2", "reference_group")){
                if(!is.null(data[[curr_col_name]])){
                  data[[curr_col_name]] <- factor(x=data[[curr_col_name]],
                                                  levels=get_risk_group_name_levels(x=object),
                                                  labels=get_risk_group_names(x=object))
                }
              }
            }
            
            if(has_multiclass_outcome){
              for(curr_col_name in c("pos_class", "outcome")){
                if(!is.null(data[[curr_col_name]])){
                  data[[curr_col_name]] <- factor(x=data[[curr_col_name]],
                                                  levels=get_class_name_levels(x=object),
                                                  labels=get_class_names(x=object))
                }
              }
            }
            
            if(has_categorical_outcome){
              # For confusion matrices.
              for(curr_col_name in c("observed_outcome", "expected_outcome")){
                if(!is.null(data[[curr_col_name]])){
                  data[[curr_col_name]] <- factor(x=data[[curr_col_name]],
                                                  levels=get_class_name_levels(x=object),
                                                  labels=get_class_names(x=object))
                }
              }
            }
            
            if(has_evaluation_time){
              for(curr_col_name in c("evaluation_time", "eval_time")){
                if(!is.null(data[[curr_col_name]])){
                  data[[curr_col_name]] <- factor(x=data[[curr_col_name]],
                                                  levels=sort(unique(data[[curr_col_name]])))
                }
              }
            }
            
            if(has_performance_metric){
              for(curr_col_name in c("metric")){
                if(!is.null(data[[curr_col_name]])){
                  data[[curr_col_name]] <- factor(x=data[[curr_col_name]],
                                                  levels=sort(unique(data[[curr_col_name]])))
                }
              }
            }
            
            # Drop unused levels
            data <- droplevels(x=data)
            
            return(data)
          })


#####.apply_labels (familiarDataElement, familiarCollection)#####
setMethod(".apply_labels", signature(data="familiarDataElement", object="familiarCollection"),
          function(data, object){
            
            # Return NULL for empty input
            if(is_empty(data)) return(NULL)
            
            # Don't update data if it is not a data.table.
            if(!data.table::is.data.table(data@data)) return(data)
            
            # Make sure that a local copy is updated
            x <- data.table::copy(data@data)
            
            # Check which labels are present, based on column names
            columns <- colnames(x)
            
            # Determine whether certain columns are present.
            has_data_set <- "data_set" %in% columns
            has_learner <- "learner" %in% columns
            has_fs_method <- "fs_method" %in% columns
            has_feature <- any(c("name", "feature_name_1", "feature_name_2", "feature") %in% columns)
            has_risk_group <- any(c("risk_group", "risk_group_1", "risk_group_2", "reference_group") %in% columns)
            has_multiclass_outcome <- any(c("pos_class", "positive_class", "outcome") %in% columns) & object@outcome_type == "multinomial"
            has_categorical_outcome <- any(c("observed_outcome", "expected_outcome") %in% columns) & object@outcome_type %in% c("binomial", "multinomial")
            has_evaluation_time <- any(c("evaluation_time", "eval_time") %in% columns) & object@outcome_type %in% c("survival", "competing_risk")
            has_performance_metric <- any(c("metric") %in% columns)
            has_model_name <- any(c("ensemble_model_name", "model_name") %in% columns)
            
            # Apply levels
            if(has_data_set){
              data.table::set(x,
                              j="data_set",
                              value = factor(x=x$data_set,
                                             levels=get_data_set_name_levels(x=object),
                                             labels=get_data_set_names(x=object)))
            }
            
            if(has_learner){
              data.table::set(x,
                              j="learner",
                              value = factor(x=x$learner,
                                             levels=get_learner_name_levels(x=object),
                                             labels=get_learner_names(x=object)))
            }
            
            if(has_fs_method){
              data.table::set(x,
                              j="fs_method",
                              value = factor(x=x$fs_method,
                                             levels=get_fs_method_name_levels(x=object),
                                             labels=get_fs_method_names(x=object)))
            }
            
            if(has_feature){
              for(current_column_name in c("name", "feature_name_1", "feature_name_2", "feature")){
                if(!is.null(x[[current_column_name]])){
                  data.table::set(x,
                                  j=current_column_name,
                                  value = factor(x=x[[current_column_name]],
                                                 levels=get_feature_name_levels(x=object),
                                                 labels=get_feature_names(x=object)))
                }
              }
            }
            
            if(has_risk_group){
              for(current_column_name in c("risk_group", "risk_group_1", "risk_group_2", "reference_group")){
                if(!is.null(x[[current_column_name]])){
                  data.table::set(x,
                                  j=current_column_name,
                                  value = factor(x=x[[current_column_name]],
                                                 levels=get_risk_group_name_levels(x=object),
                                                 labels=get_risk_group_names(x=object)))
                }
              }
            }
            
            if(has_multiclass_outcome){
              for(current_column_name in c("pos_class", "positive_class", "outcome")){
                if(!is.null(x[[current_column_name]])){
                  data.table::set(x,
                                  j=current_column_name,
                                  value = factor(x=x[[current_column_name]],
                                                 levels=get_class_name_levels(x=object),
                                                 labels=get_class_names(x=object)))
                }
              }
            }
            
            if(has_categorical_outcome){
              # For confusion matrices.
              for(current_column_name in c("observed_outcome", "expected_outcome")){
                if(!is.null(x[[current_column_name]])){
                  data.table::set(x,
                                  j=current_column_name,
                                  value = factor(x=x[[current_column_name]],
                                                 levels=get_class_name_levels(x=object),
                                                 labels=get_class_names(x=object)))
                }
              }
            }
            
            if(has_evaluation_time){
              for(current_column_name in c("evaluation_time", "eval_time")){
                if(!is.null(x[[current_column_name]])){
                  data.table::set(x,
                                  j=current_column_name,
                                  value = factor(x=x[[current_column_name]],
                                                 levels=sort(unique(x[[current_column_name]]))))
                }
              }
            }
            
            if(has_performance_metric){
              for(current_column_name in c("metric")){
                if(!is.null(x[[current_column_name]])){
                  data.table::set(x,
                                  j=current_column_name,
                                  value = factor(x=x[[current_column_name]],
                                                 levels=sort(unique(x[[current_column_name]]))))
                }
              }
            }
            
            if(has_model_name){
              for(current_column_name in c("ensemble_model_name", "model_name")){
                if(!is.null(x[[current_column_name]])){
                  data.table::set(x,
                                  j=current_column_name,
                                  value = factor(x=x[[current_column_name]],
                                                 levels=sort(unique(x[[current_column_name]]))))
                }
              }
            }
            
            # Order columns. Grouping columns appear on the left, whereas value
            # columns appear on the right. First we identify the grouping
            # columns. Note that not all 
            grouping_columns <- c("data_set", "fs_method", "learner",
                                  "ensemble_model_name", "model_name",
                                  "evaluation_time", "eval_time",
                                  "name", "feature_name_1", "feature_name_2", "feature",
                                  "pos_class", "positive_class",
                                  "metric")
            
            # Find the grouping columns actually present
            grouping_columns <- intersect(grouping_columns, columns)
            
            # Then identify the value columns.
            value_columns <- data@value_column
            if(all(is.na(value_columns))) value_columns <- NULL
            
            # Find any remaining columns.
            remaining_columns <- setdiff(columns, c(grouping_columns, value_columns))
            
            # Order columns.
            data.table::setcolorder(x=x, neworder=c(grouping_columns, remaining_columns, value_columns))
            
            # Drop unused levels.
            x <- droplevels(x)
            
            # Replace data attribute.
            data@data <- x
            
            return(data)
          })

#####.apply_labels (list, familiarCollection)#####
setMethod(".apply_labels", signature(data="list", object="familiarCollection"),
          function(data, object){
            
            return(lapply(data,
                          .apply_labels,
                          object=object))
          })


#####.apply_labels (ANY, familiarCollection)#####
setMethod(".apply_labels", signature(data="ANY", object="familiarCollection"),
          function(data, object){
            # This is the fall-back option for empty data.
            
            return(NULL)
          })

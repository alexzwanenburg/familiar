#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

#'@title Creates a valid data object from input data.
#'
#'@description Creates `dataObject` a object from input data. Input data can be
#'  a `data.frame` or `data.table`, a path to such tables on a local or network
#'  drive, or a path to tabular data that may be converted to these formats.
#'
#'  In addition, a `familiarEnsemble` or `familiarModel` object can be passed
#'  along to check whether the data are formatted correctly, e.g. by checking
#'  the levels of categorical features, whether all expected columns are
#'  present, etc.
#'
#'@param data A `data.frame` or `data.table`, a path to such tables on a local
#'  or network drive, or a path to tabular data that may be converted to these
#'  formats.
#'
#'@param object A `familiarEnsemble` or `familiarModel` object that is used to
#'  check consistency of these objects.
#'
#'@param check_stringency Specifies stringency of various checks. This is mostly:
#'
#'  * `strict`: default value used for `summon_familiar`. Thoroughly checks
#'  input data. Used internally for checking development data.
#'  
#'  * `external_warn`: value used for `extract_data` and related methods. Less
#'  stringent checks, but will warn for possible issues. Used internally for
#'  checking data for evaluation and explanation.
#'  
#'  * `external`: value used for external methods such as `predict`. Less
#'  stringent checks, particularly for identifier and outcome columns, which may
#'  be completely absent. Used internally for `predict`.
#'
#'@inheritParams .parse_experiment_settings
#'
#'@details You can specify settings for your data manually, e.g. the column for
#'  sample identifiers (`sample_id_column`). This prevents you from having to
#'  change the column name externally. In the case you provide a `familiarModel`
#'  or `familiarEnsemble` for the `object` argument, any parameters you provide
#'  take precedence over parameters specified by the object.
#'
#'@return A `dataObject` object.
#'@exportMethod as_data_object
#'@md
#'@rdname as_data_object-methods

#####as_data_object (generic)#####
setGeneric("as_data_object", function(data, ...) standardGeneric("as_data_object"))

#####as_data_object (dataObject)#####
#'@rdname as_data_object-methods
setMethod("as_data_object", signature(data="dataObject"),
          function(data, object=NULL, ...) return(data))


#####as_data_object (data.table)#####
#'@rdname as_data_object-methods
setMethod("as_data_object", signature(data="data.table"),
          function(data,
                   object=NULL,
                   sample_id_column=waiver(),
                   batch_id_column=waiver(),
                   series_id_column=waiver(),
                   development_batch_id=waiver(),
                   validation_batch_id=waiver(),
                   outcome_name=waiver(),
                   outcome_column=waiver(),
                   outcome_type=waiver(),
                   event_indicator=waiver(),
                   censoring_indicator=waiver(),
                   competing_risk_indicator=waiver(),
                   class_levels=waiver(),
                   exclude_features=waiver(),
                   include_features=waiver(),
                   check_stringency="strict",
                   ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            type <- NULL
            
            # Determine whether the object contains data concerning columns, and
            # outcome. Note that user-provided names always take precedence.
            has_model_object <- is(object, "familiarModel") | is(object, "familiarEnsemble") | is(object, "familiarNoveltyDetector")
            
            # Check whether a model potentially has outcome information.
            if(has_model_object){
              has_outcome_info_slot <- methods::.hasSlot(object, "outcome_info")
              
            } else {
              has_outcome_info_slot <- FALSE
            }
            
            if(check_stringency != "strict"){
              if(!has_model_object) stop("Dummy columns cannot be set without a model or ensemble object.")
            } 
            
            # Attempt to identify a sample identifier column.
            if(is.waive(sample_id_column)){
              
              if(has_model_object){
                if(!is_empty(object@data_column_info)){
                  # Find the sample id column stored with the model.
                  model_sample_id_column <- object@data_column_info[type == "sample_id_column"]$external
                  
                  # Check that the model actually has a column name (not
                  # character(0)) that is not NA, and set this column name.
                  if(length(model_sample_id_column) > 0){
                    if(!is.na(model_sample_id_column)) sample_id_column <- model_sample_id_column
                  }
                }
              }
            }
            
            # Attempt to identify a batch identifier column.
            if(is.waive(batch_id_column)){
              
              if(has_model_object){
                if(!is_empty(object@data_column_info)){
                  # Find the batch id column stored with the model.
                  model_batch_id_column <- object@data_column_info[type == "batch_id_column"]$external
                  
                  # Check that the model actually has a column name (not
                  # character(0)) that is not NA, and set this column name.
                  if(length(model_batch_id_column) > 0){
                    if(!is.na(model_batch_id_column)) batch_id_column <- model_batch_id_column
                  }
                }
              }
            }
            
            # Attempt to identify a series identifier column.
            if(is.waive(series_id_column)){
              
              if(has_model_object){
                if(!is_empty(object@data_column_info)){
                  # Find the series id column stored with the model.
                  model_series_id_column <- object@data_column_info[type == "series_id_column"]$external
                  
                  # Check that the model actually has a column name (not
                  # character(0)) that is not NA, and set this column name.
                  if(length(model_series_id_column) > 0){
                    if(!is.na(model_series_id_column)) series_id_column <- model_series_id_column
                  }
                }
              }
            }
            
            # Development and validation batch ids are not incorporated into
            # familiarModel or familiarEnsemble objects.
            
            # Attempt to identify the name of the outcome.
            if(is.waive(outcome_name)){
              
              if(has_model_object & has_outcome_info_slot){
                if(is(object@outcome_info, "outcomeInfo")){
                  
                  # Check that the outcome name is not empty.
                  if(length(object@outcome_info@name) >= 1) outcome_name <- object@outcome_info@name
                }
              }
            }
            
            # Attempt to identify the outcome columns.
            if(is.waive(outcome_column)){
              
              if(has_model_object & has_outcome_info_slot){
                if(!is_empty(object@data_column_info)){
                  # Find the model columns.
                  outcome_column <- object@data_column_info[type == "outcome_column"]$external
                }
              }
            }
            
            # Attempt to identify the type of outcome.
            if(is.waive(outcome_type)){
              if(is(object, "familiarNoveltyDetector")){
                outcome_type <- "unsupervised"
                
              } else if(has_model_object){
                outcome_type <- object@outcome_type
              }
            }
            
            # Attempt to identify the event indicator.
            if(is.waive(event_indicator)){
              
              if(has_model_object & has_outcome_info_slot){
                if(is(object@outcome_info, "outcomeInfo")){
                  if(length(object@outcome_info@event) > 0){
                    if(!is.na(object@outcome_info@event)) event_indicator <- object@outcome_info@event
                  }
                }
              }
            }
            
            # Attempt to identify the censoring indicator.
            if(is.waive(censoring_indicator)){
              
              if(has_model_object & has_outcome_info_slot){
                if(is(object@outcome_info, "outcomeInfo")){
                  if(length(object@outcome_info@censored) > 0){
                    if(!is.na(object@outcome_info@censored)) censoring_indicator <- object@outcome_info@censored
                  }
                }
              }
            }
            
            # Attempt to identify the competing risk indicator.
            if(is.waive(competing_risk_indicator)){
              
              if(has_model_object & has_outcome_info_slot){
                if(is(object@outcome_info, "outcomeInfo")){
                  if(length(object@outcome_info@competing_risk) > 0){
                    if(!is.na(object@outcome_info@competing_risk)) competing_risk_indicator <- object@outcome_info@competing_risk
                  }
                }
              }
            }
            
            # Attempt to identify class levels of the outcome.
            if(is.waive(class_levels)){
              
              if(has_model_object & has_outcome_info_slot){
                if(is(object@outcome_info, "outcomeInfo")){
                  if(length(object@outcome_info@levels) > 0) class_levels <- object@outcome_info@levels
                }
              }
            }
            
            # Load settings from input.
            settings <- do.call(.parse_initial_settings,
                                args=c(list("experimental_design"="fs+mb",
                                            "sample_id_column"=sample_id_column,
                                            "batch_id_column"=batch_id_column,
                                            "series_id_column"=series_id_column,
                                            "development_batch_id"=development_batch_id,
                                            "validation_batch_id"=validation_batch_id,
                                            "outcome_name"=outcome_name,
                                            "outcome_column"=outcome_column,
                                            "outcome_type"=outcome_type,
                                            "event_indicator"=event_indicator,
                                            "censoring_indicator"=censoring_indicator,
                                            "competing_risk_indicator"=competing_risk_indicator,
                                            "class_levels"=class_levels,
                                            "exclude_features"=exclude_features,
                                            "include_features"=include_features),
                                       list(...)))
            
            # Prepare data.table.
            data <- .load_data(data=data,
                               sample_id_column=settings$data$sample_col,
                               batch_id_column=settings$data$batch_col,
                               series_id_column=settings$data$series_col)
            
            # Update settings
            settings <- .update_initial_settings(data=data,
                                                 settings=settings,
                                                 check_stringency=check_stringency)
            
            # Parse data
            data <- .finish_data_preparation(data = data,
                                             sample_id_column = settings$data$sample_col,
                                             batch_id_column = settings$data$batch_col,
                                             series_id_column = settings$data$series_col,
                                             outcome_column = settings$data$outcome_col,
                                             outcome_type = settings$data$outcome_type,
                                             include_features = settings$data$include_features,
                                             class_levels = settings$data$class_levels,
                                             censoring_indicator = settings$data$censoring_indicator,
                                             event_indicator = settings$data$event_indicator,
                                             competing_risk_indicator = settings$data$competing_risk_indicator,
                                             check_stringency = check_stringency)
            
            # Update the dataset according to the feature info list.
            if(has_model_object) data <- update_data_set(data=data, object=object)
            
            # Add outcome information, preferentially from the familiarModel or
            # familiarEnsemble, as it is more complete.
            if(has_model_object & has_outcome_info_slot){
              outcome_info <- object@outcome_info
              
            } else {
              outcome_info <- create_outcome_info(settings=settings)
            }
            
            if(has_model_object){
              if(!is_empty(object@data_column_info)){
                data_info <- object@data_column_info
                
              } else {
                data_info <- create_data_column_info(settings=settings)
              }
              
            } else {
              data_info <- create_data_column_info(settings=settings)
            }
            
            # Convert to dataObject
            data <- methods::new("dataObject",
                                 data = data,
                                 preprocessing_level="none",
                                 outcome_type = settings$data$outcome_type,
                                 outcome_info = outcome_info,
                                 data_column_info = data_info)
            
            return(data)
          })


#####as_data_object (ANY)#####
#'@rdname as_data_object-methods
setMethod("as_data_object", signature(data="ANY"),
          function(data,
                   object=NULL,
                   sample_id_column=waiver(),
                   batch_id_column=waiver(),
                   series_id_column=waiver(),
                   ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            type <- NULL
            
            # Determine whether the object contains data concerning columns.
            # Note that user-provided names always take precedence.
            has_model_object <- FALSE
            if(is(object, "familiarModel") | is(object, "familiarEnsemble")){
              if(!is_empty(object@data_column_info)) has_model_object <- TRUE
            }
            
            # Create a local copy of sample_id_column to pass on to .load_data.
            if(is.waive(sample_id_column)){
              sample_id_column_local <- NULL
              
              if(has_model_object){
                # Find the sample id column stored with the model.
                model_sample_id_column <- object@data_column_info[type == "sample_id_column"]$external
                
                # Check that the model actually has a column name (not
                # character(0)) that is not NA, and set this column name.
                if(length(model_sample_id_column) > 0){
                  if(!is.na(model_sample_id_column)) sample_id_column_local <- model_sample_id_column
                }
              }
              
            } else {
              sample_id_column_local <- sample_id_column
            }
            
            # Create a local copy of batch_id_column to pass on to .load_data.
            if(is.waive(batch_id_column)){
              batch_id_column_local <- NULL
              
              if(has_model_object){
                # Find the batch id column stored with the model.
                model_batch_id_column <- object@data_column_info[type == "batch_id_column"]$external
                
                # Check that the model actually has a column name (not
                # character(0)) that is not NA, and set this column name.
                if(length(model_batch_id_column) > 0){
                  if(!is.na(model_batch_id_column)) batch_id_column_local <- model_batch_id_column
                }
              }
              
            } else {
              batch_id_column_local <- batch_id_column
            }
            
            # Create a local copy of series_id_column to pass on to .load_data
            if(is.waive(series_id_column)){
              series_id_column_local <- NULL
              
              if(has_model_object){
                # Find the series id column stored with the model.
                model_series_id_column <- object@data_column_info[type == "series_id_column"]$external
                
                # Check that the model actually has a column name (not
                # character(0)) that is not NA, and set this column name.
                if(length(model_series_id_column) > 0){
                  if(!is.na(model_series_id_column)) series_id_column_local <- model_series_id_column
                }
              }
              
            } else {
              series_id_column_local <- series_id_column
            }
            
            # Load data and convert to data.table
            data <- .load_data(data=data,
                               sample_id_column=sample_id_column_local,
                               batch_id_column=batch_id_column_local,
                               series_id_column=series_id_column_local)
            
            # Pass on to data.table method.
            return(do.call(as_data_object, args=c(list("data"=data,
                                                       "object"=object,
                                                       "sample_id_column"=sample_id_column,
                                                       "batch_id_column"=batch_id_column,
                                                       "series_id_column"=series_id_column),
                                                  list(...))))
          })



#####extract_settings_from_data####
setMethod("extract_settings_from_data", signature(data="dataObject"),
          function(data, settings=NULL, signature=NULL){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            type <- NULL
            
            if(is.null(settings)) settings <- list("data"=list())
            
            # Placeholders
            sample_id_column <- batch_id_column <- series_id_column <- outcome_columns <- NULL
            
            if(!is_empty(data@data_column_info)){
              # Sample identifier
              sample_id_column <- ..set_identifier_column(current=sample_id_column,
                                                          data=data@data,
                                                          internal=data@data_column_info[type == "sample_id_column"]$internal,
                                                          external=data@data_column_info[type == "sample_id_column"]$external)
              
              # Batch identifier
              batch_id_column <- ..set_identifier_column(current=batch_id_column,
                                                         data=data@data,
                                                         internal=data@data_column_info[type == "batch_id_column"]$internal,
                                                         external=data@data_column_info[type == "batch_id_column"]$external)
              
              # Series identifier
              series_id_column <- ..set_identifier_column(current=series_id_column,
                                                          data=data@data,
                                                          internal=data@data_column_info[type == "series_id_column"]$internal,
                                                          external=data@data_column_info[type == "series_id_column"]$external)
              
              # Outcome columns
              outcome_columns <- ..set_identifier_column(current=outcome_columns,
                                                         data=data@data,
                                                         internal=data@data_column_info[type == "outcome_column"]$internal,
                                                         external=data@data_column_info[type == "outcome_column"]$external)
            }
            
            if(is.null(outcome_columns)) get_outcome_columns(data)
            
            # Sample identifier column
            settings$data$sample_col <- sample_id_column
            settings$data$batch_col <- batch_id_column
            settings$data$series_col <- series_id_column
            settings$data$outcome_col <- outcome_columns
            settings$data$outcome_type <- data@outcome_type
            settings$data$outcome_name <- get_outcome_name(data@outcome_info)
            settings$data$class_levels <- get_outcome_class_levels(data@outcome_info)
            settings$data$event_indicator <- data@outcome_info@event
            settings$data$censoring_indicator <- data@outcome_info@censored
            settings$data$competing_risk_indicator <- data@outcome_info@competing_risk
            settings$data$signature <- signature
            settings$data$include_features <- get_feature_columns(data)
            
            return(settings)
          })



..set_identifier_column <- function(current=NULL,
                                    data=NULL,
                                    internal,
                                    external){
  
  if(!(is.waive(current) | is.null(current))){
    return(current)
  }
  
  temporary <- NULL
  
  # Prefer external before internal, as long as it is present in data.
  if(all(sapply(external, length) > 0)){
    if(!any(sapply(external, is.na))){
      if(data.table::is.data.table(data)){
        if(all(external %in% colnames(data))) temporary <- external
      }
    }
  }
  
  # Prefer internal if it is present in data.
  if(data.table::is.data.table(data) & is.null(temporary)){
    if(all(internal %in% colnames(data))) temporary <- internal
  }
  
  # Use external if internal is not present in data.
  if(all(sapply(external, length) > 0) & is.null(temporary)){
    if(!any(sapply(external, is.na))) temporary <- internal
  }
  
  if(is.null(temporary)){
    return(current)
  } else {
    return(temporary)
  }
}



#####load_delayed_data (model)#####
setMethod("load_delayed_data", signature(data="dataObject", object="ANY"),
          function(data, object, stop_at, keep_novelty=FALSE){
            # Loads data from internal memory

            if(!(is(object, "familiarModel") | is(object, "familiarVimpMethod") | is(object, "familiarNoveltyDetector"))){
              ..error_reached_unreachable_code(paste0("load_delayed_data: object is expected to be a familiarModel, ",
                                                      "familiarVimpMethod or familiarNoveltyDetector."))
            }
            
            # Check if loading was actually delayed
            if(!data@delay_loading) return(data)

            # Read project list and settings
            iteration_list <- get_project_list()$iter_list

            # Read required features
            required_features <- object@required_features
            
            # Get columns in data frame which are not features, but identifiers and outcome instead
            non_feature_cols <- get_non_feature_columns(x=object)
            
            # Find the identifiers for the current run.
            run_id_list <- .get_iteration_identifiers(run=list("run_table"=object@run_table),
                                                      perturb_level=data@perturb_level)
            
            # Derive sample identifiers based on the selected iteration data.
            sample_identifiers <- .get_sample_identifiers(iteration_list=iteration_list,
                                                          data_id=run_id_list$data,
                                                          run_id=run_id_list$run,
                                                          train_or_validate=ifelse(data@load_validation, "valid", "train"))
            
            # Currently select only unique samples from the backend.
            if(!is_empty(sample_identifiers)){
              unique_sample_identifiers <- unique(sample_identifiers)
              
            } else {
              # Return an updated data object, but without data
              return(methods::new("dataObject",
                                  data=NULL,
                                  preprocessing_level="none",
                                  outcome_type=data@outcome_type,
                                  aggregate_on_load=data@aggregate_on_load))
            }
            
            # Prepare a new data object
            new_data <- methods::new("dataObject",
                                     data = get_data_from_backend(sample_identifiers=unique_sample_identifiers,
                                                                  column_names=c(non_feature_cols, required_features)),
                                     preprocessing_level="none",
                                     outcome_type = data@outcome_type,
                                     delay_loading = FALSE,
                                     perturb_level = NA_integer_,
                                     load_validation = data@load_validation,
                                     aggregate_on_load = data@aggregate_on_load,
                                     sample_set_on_load = data@sample_set_on_load)
            
            # Preprocess data
            new_data <- preprocess_data(data=new_data,
                                        object=object,
                                        stop_at=stop_at,
                                        keep_novelty=keep_novelty)
            
            # Recreate iteration. Note that we here also use duplicate samples
            # to recreate e.g. bootstraps.
            new_data <- select_data_from_samples(data=new_data,
                                                 samples=sample_identifiers)
            
            # Aggregate data if required
            if(new_data@aggregate_on_load){
              
              # Aggregate
              new_data <- aggregate_data(data=new_data)
              
              # Reset flag to FALSE, as data has been loaded
              new_data@aggregate_on_load <- FALSE
            }
            
            return(new_data)
          })



#####load_delayed_data (ensemble)#####
setMethod("load_delayed_data", signature(data="dataObject", object="familiarEnsemble"),
          function(data, object, stop_at="clustering", keep_novelty=FALSE){
            # Loads data from internal memory -- for familiarEnsemble objects
            
            # Suppress NOTES due to non-standard evaluation in data.table
            perturb_level <- NULL
            
            # Check if loading was actually delayed
            if(!data@delay_loading) return(data)
            
            # Read project list and settings
            iteration_list <- get_project_list()$iter_list
            settings <- get_settings()
            
            # Read required features
            required_features <- object@required_features
            
            # Get columns in data frame which are not features, but identifiers and outcome instead
            non_feature_cols <- get_non_feature_columns(x=object)
            
            # Join run tables to identify the runs that should be evaluated.
            combined_run_table <- lapply(object@run_table$run_table, function(model_run_table, data_perturb_level){
              return(model_run_table[perturb_level == data_perturb_level])
            }, data_perturb_level=data@perturb_level)
            
            # Merge to single table
            combined_run_table <- data.table::rbindlist(combined_run_table)
            
            # Remove duplicate rows
            combined_run_table <- unique(combined_run_table)
            
            # Check length and extract sample identifiers.
            if(nrow(combined_run_table) == 1){
              sample_identifiers <- .get_sample_identifiers(iteration_list=iteration_list,
                                                            data_id=combined_run_table$data_id,
                                                            run_id=combined_run_table$run_id,
                                                            train_or_validate=ifelse(data@load_validation, "valid", "train"))
              
            } else {
              # Extract all sample identifiers. This happens if the the data is
              # pooled.
              sample_identifiers <- data.table::rbindlist(lapply(seq_len(nrow(combined_run_table)), function(ii, run_table, iteration_list, train_or_validate){
               sample_identifiers <- .get_sample_identifiers(iteration_list=iteration_list,
                                                             data_id=run_table$data_id[ii],
                                                             run_id=run_table$run_id[ii],
                                                             train_or_validate=train_or_validate)
               return(sample_identifiers)
              },
              run_table=combined_run_table,
              iteration_list=iteration_list,
              train_or_validate=ifelse(data@load_validation, "valid", "train")))
              
              # Select only unique sample identifiers.
              sample_identifiers <- unique(sample_identifiers)
            }
            
            # Currently select only unique samples from the backend.
            if(!is_empty(sample_identifiers)){
              unique_sample_identifiers <- unique(sample_identifiers)
              
            } else {
              # Return an updated data object, but without data
              return(methods::new("dataObject",
                                  data = NULL,
                                  preprocessing_level="none",
                                  outcome_type = data@outcome_type,
                                  aggregate_on_load = data@aggregate_on_load))
            }
            
            # Prepare a new data object
            new_data <- methods::new("dataObject",
                                     data = get_data_from_backend(sample_identifiers=unique_sample_identifiers,
                                                                  column_names=c(non_feature_cols, required_features)),
                                     preprocessing_level="none",
                                     outcome_type = data@outcome_type,
                                     delay_loading = FALSE,
                                     perturb_level = NA_integer_,
                                     load_validation = data@load_validation,
                                     aggregate_on_load = data@aggregate_on_load,
                                     sample_set_on_load = data@sample_set_on_load)
            
            # Preprocess data
            new_data <- preprocess_data(data=new_data,
                                        object=object,
                                        stop_at=stop_at,
                                        keep_novelty=keep_novelty)
            
            # Recreate iteration. Note that we here also use duplicate samples
            # to recreate e.g. bootstraps.
            new_data <- select_data_from_samples(data=new_data,
                                                 samples=sample_identifiers)
            
            # Aggregate data if required
            if(new_data@aggregate_on_load){
              
              # Aggregate
              new_data <- aggregate_data(data=new_data)
              
              # Reset flag to FALSE, as data has been loaded
              new_data@aggregate_on_load <- FALSE
            }
            
            return(new_data)
            
          })


#####preprocess_data (vimp method)#####
setMethod("preprocess_data", signature(data="dataObject", object="familiarVimpMethod"),
          function(data, object, stop_at="clustering", keep_novelty=FALSE) .preprocess_data(data=data,
                                                                                             object=object,
                                                                                             stop_at=stop_at,
                                                                                             keep_novelty=keep_novelty))


#####preprocess_data (model)#####
setMethod("preprocess_data", signature(data="dataObject", object="familiarModel"),
          function(data, object, stop_at="clustering", keep_novelty=FALSE){
            
            # Pre-process the data.
            data <- .preprocess_data(data=data,
                                     object=object,
                                     stop_at=stop_at,
                                     keep_novelty=keep_novelty)
            
            # Post-process the data to select the correct feature and identifier
            # set.
            data <- postprocess_data(data=data,
                                     object=object,
                                     stop_at=stop_at,
                                     keep_novelty=keep_novelty)
            
            return(data)
          })

#####preprocess_data (novelty detector)#########################################
# Note that keep_novelty is always false to prevent reading novelty_features
# slot. Novelty features are stored in the model_features slot of
# familiarNoveltyDetector objects.
setMethod("preprocess_data", signature(data="dataObject", object="familiarNoveltyDetector"),
          function(data, object, stop_at="clustering", ...){
            
            # Assign "unsupervised" outcome type to the data to prevent outcome
            # columns being selected.
            data@outcome_type <- "unsupervised"
            
            # Pre-process the data.
            data <- .preprocess_data(data=data,
                                     object=object,
                                     stop_at=stop_at,
                                     keep_novelty=FALSE)
            
            # Post-process the data to select the correct feature and identifier
            # set.
            data <- postprocess_data(data=data,
                                     object=object,
                                     stop_at=stop_at)
            
            return(data)
          })


#####preprocess_data (ensemble)#####
setMethod("preprocess_data", signature(data="dataObject", object="familiarEnsemble"),
          function(data, object, stop_at="clustering", keep_novelty=FALSE).preprocess_data(data=data,
                                                                                             object=object,
                                                                                             stop_at=stop_at,
                                                                                             keep_novelty=keep_novelty))


.preprocess_data <- function(data, object, stop_at, keep_novelty=FALSE){
  
  # Convert the preprocessing_level attained and the requested
  # stopping level to ordinals.
  preprocessing_level_attained <- .as_preprocessing_level(data@preprocessing_level)
  stop_at <- .as_preprocessing_level(stop_at)
  
  # Check whether pre-processing is required
  if(preprocessing_level_attained == stop_at){
    
    return(data)
    
  } else if(preprocessing_level_attained > stop_at) {
    ..error_reached_unreachable_code("preprocess_data,dataObject,ANY: data were preprocessed at a higher level than required by stop_at.")
  }
  
  if(preprocessing_level_attained < "signature" & stop_at >= "signature"){
    # Apply the signature.
    data <- select_features(data=data,
                            features=object@required_features)
    
    # Update pre-processing level externally as
    # it is not limited to pre-processing per sé.
    data@preprocessing_level <- "signature"
    
  } else if(preprocessing_level_attained == "signature" & stop_at >= "signature"){
    
    required_features <- object@required_features
    selected_features <- object@model_features
    if(keep_novelty) selected_features <- union(selected_features, object@novelty_features)
    
    if(length(required_features) > 0 & length(selected_features) > 0 & has_feature_data(data)){
      
      # Select available features specific to the object.
      if(all(required_features %in% get_feature_columns(data))){
        data <- select_features(data=data,
                                features=required_features)
        
      } else if(all(selected_features %in% get_feature_columns(data))) {
        data <- select_features(data=data,
                                features=selected_features)
        
      } else {
        ..error_reached_unreachable_code(".preprocess_data: could not identify overlapping features")
      }
    }
  }
  
  if(preprocessing_level_attained < "transformation" & stop_at >= "transformation"){
    # Transform the features.
    data <- transform_features(data=data,
                               feature_info_list=object@feature_info)
  }
  
  if(preprocessing_level_attained < "normalisation" & stop_at >= "normalisation"){
    # Normalise feature values.
    data <- normalise_features(data=data,
                               feature_info_list=object@feature_info)
  }
  
  if(preprocessing_level_attained < "batch_normalisation" & stop_at >= "batch_normalisation"){
    # Batch-normalise feature values
    data <- batch_normalise_features(data=data,
                                     feature_info_list=object@feature_info)
  }
  
  if(preprocessing_level_attained < "imputation" & stop_at >= "imputation"){
    # Impute missing values
    data  <- impute_features(data=data,
                             feature_info_list=object@feature_info)
  }
  
  if(preprocessing_level_attained < "clustering" & stop_at >= "clustering"){
    # Cluster features
    data <- cluster_features(data=data,
                             feature_info_list=object@feature_info)
  }
  # 
  # if(is(object, "familiarModel") & stop_at >= "clustering"){
  #   
  #   # Select features.
  #   features <- object@model_features
  #   if(keep_novelty) features <- union(features, object@novelty_features)
  #   
  #   # Return data if there are no features.
  #   if(length(features) == 0) return(data)
  #   
  #   # Determine the features after clustering.
  #   features <- features_after_clustering(features=features,
  #                                         feature_info_list=object@feature_info)
  #   
  #   # Create a slice of the data for the feature set.
  #   data <- select_features(data=data,
  #                           features=features)
  #   
  # } else if(is(object, "familiarNoveltyDetector") & stop_at >= "clustering"){
  #   # Select features.
  #   features <- object@model_features
  #   
  #   # Return data if there are no features.
  #   if(length(features) == 0) return(data)
  #   
  #   # Determine the features after clustering.
  #   features <- features_after_clustering(features=features,
  #                                         feature_info_list=object@feature_info)
  #   
  #   # Create a slice of the data for the feature set.
  #   data <- select_features(data=data,
  #                           features=features)
  # }
  
  return(data)
}


##### postprocess_data (model)--------------------------------------------
setMethod("postprocess_data", signature(data="dataObject", object="familiarModel"),
          function(data, object, stop_at="clustering", keep_novelty=FALSE){
            
            # Convert the preprocessing_level attained and the requested
            # stopping level to ordinals.
            preprocessing_level_attained <- .as_preprocessing_level(data@preprocessing_level)
            stop_at <- .as_preprocessing_level(stop_at)
            
            if(stop_at < "clustering" | preprocessing_level_attained < "clustering") return(data)
            
            # Select features.
            features <- object@model_features
            if(keep_novelty) features <- union(features, object@novelty_features)
            
            # Return data if there are no features.
            if(length(features) == 0) return(data)
            
            # Determine the features after clustering.
            features <- features_after_clustering(features=features,
                                                  feature_info_list=object@feature_info)
            
            # Create a slice of the data for the feature set.
            data <- select_features(data=data,
                                    features=features)
            
            return(data)
          })

##### postprocess_data (novelty detector)---------------------------------
setMethod("postprocess_data", signature(data="dataObject", object="familiarNoveltyDetector"),
          function(data, object, stop_at="clustering"){
            
            # Convert the preprocessing_level attained and the requested
            # stopping level to ordinals.
            preprocessing_level_attained <- .as_preprocessing_level(data@preprocessing_level)
            stop_at <- .as_preprocessing_level(stop_at)
            
            if(stop_at < "clustering" | preprocessing_level_attained < "clustering") return(data)
            
            # Select features.
            features <- object@model_features
            
            # Return data if there are no features.
            if(length(features) == 0) return(data)
            
            # Determine the features after clustering.
            features <- features_after_clustering(features=features,
                                                  feature_info_list=object@feature_info)
            
            # Create a slice of the data for the feature set.
            data <- select_features(data=data,
                                    features=features)
            
            return(data)
          })

          

#####process_input_data (vimp method)#####
setMethod("process_input_data", signature(object="familiarVimpMethod", data="ANY"),
          function(object, data, is_pre_processed=FALSE, stop_at="clustering", keep_novelty=FALSE) .process_input_data(object=object,
                                                                                                                       data=data,
                                                                                                                       is_pre_processed=is_pre_processed,
                                                                                                                       stop_at=stop_at,
                                                                                                                       keep_novelty=keep_novelty))

#####process_input_data (novelty detector)#####
setMethod("process_input_data", signature(object="familiarNoveltyDetector", data="ANY"),
          function(object, data, is_pre_processed=FALSE, stop_at="clustering", ...){
            
            # Ensure that the outcome_type of data is changed to "unsupervised".
            # We don't need any outcome columns.
            data@outcome_type <- "unsupervised"
            
            # Process data.
            data <- .process_input_data(object=object,
                                        data=data,
                                        is_pre_processed=is_pre_processed,
                                        stop_at=stop_at,
                                        keep_novelty=FALSE)
          })

#####process_input_data (model)#####
setMethod("process_input_data", signature(object="familiarModel", data="ANY"),
          function(object, data, is_pre_processed=FALSE, stop_at="clustering", keep_novelty=FALSE) .process_input_data(object=object,
                                                                                                                       data=data,
                                                                                                                       is_pre_processed=is_pre_processed,
                                                                                                                       stop_at=stop_at,
                                                                                                                       keep_novelty=keep_novelty))

#####process_input_data (ensemble)#####
setMethod("process_input_data", signature(object="familiarEnsemble", data="ANY"),
          function(object, data, is_pre_processed=FALSE, stop_at="clustering", keep_novelty=FALSE) .process_input_data(object=object,
                                                                                                                       data=data,
                                                                                                                       is_pre_processed=is_pre_processed,
                                                                                                                       stop_at=stop_at,
                                                                                                                       keep_novelty=keep_novelty))

.process_input_data <- function(object, data, is_pre_processed, stop_at, keep_novelty=FALSE){
  # Check whether data is a dataObject, and create one otherwise
  if(!is(data, "dataObject")){
    data <- as_data_object(data=data,
                           object=object)
    
    # Set pre-processing level.
    data@preprocessing_level <- ifelse(is_pre_processed, "clustering", "none")
  }
  
  # Load data from internal memory, if not provided otherwise
  if(data@delay_loading){
    data <- load_delayed_data(data=data,
                              object=object,
                              stop_at=stop_at,
                              keep_novelty=keep_novelty)
  }
  
  # Pre-process data in case it has not been pre-processed
  data <- preprocess_data(data=data,
                          object=object,
                          stop_at=stop_at,
                          keep_novelty=keep_novelty)
  
  # Return data
  return(data)
}



#####select_data_from_samples#####
setMethod("select_data_from_samples", signature(data="dataObject", samples="ANY"),
          function(data, samples=NULL){
            
            # Check if data is loaded
            if(data@delay_loading){

              # Store samples until the data is loaded.
              data@sample_set_on_load <- samples
              
            } else {
              
              # Determine the names of the id-columns, up to the series level.
              id_columns <- get_id_columns(id_depth="series")
              
              if(is_empty(samples) & is.null(data@sample_set_on_load)) {
                # Return an empty data set if no samples are provided
                data@data <- head(data@data, n=0)
                
              } else if(is_empty(samples) & !is.null(data@sample_set_on_load)) {
                # Use samples in the sample_set_on_load attribute.
                data@data <- merge(x=data@sample_set_on_load,
                                   y=data@data,
                                   by=id_columns,
                                   all=FALSE,
                                   allow.cartesian=TRUE)
                
              } else if(!is_empty(samples) & is.null(data@sample_set_on_load)) {
                # Use samples from the samples function argument.
                # allow.cartesian is set to true to allow use with repeated
                # measurements.
                if(all(id_columns %in% colnames(samples))){
                  data@data <- merge(x=samples,
                                     y=data@data,
                                     by=id_columns,
                                     all=FALSE,
                                     allow.cartesian=TRUE)
                  
                } else {
                  data@data <- merge(x=samples,
                                     y=data@data,
                                     by=get_id_columns(id_depth="sample"),
                                     all=FALSE,
                                     allow.cartesian=TRUE)
                }
                
              } else {
                # Use samples that appear both as function argument and within
                # the sample_set_on_load attribute. The sample_set_on_load
                # attribute is used as a filter. allow.cartesian is set to true
                # to allow use with repeated measurements.
                samples <- data.table::fintersect(samples, data@sample_set_on_load)

                if(is_empty(samples)){
                  # Return an empty data set if no samples are left.
                  data@data <- head(data@data, n=0)
                  
                } else {
                  # Check if series identifiers are present. They may be absent
                  # if samples were generated using fam_sample
                  
                  if(all(id_columns %in% colnames(sample))){
                    data@data <- merge(x=samples,
                                       y=data@data,
                                       by=id_columns,
                                       all=FALSE,
                                       allow.cartesian=TRUE)
                    
                  } else {
                    data@data <- merge(x=samples,
                                       y=data@data,
                                       by=get_id_columns(id_depth="sample"),
                                       all=FALSE,
                                       allow.cartesian=TRUE)
                  }
                }
              }
            }
            
            return(data)
          })



#####aggregate_data#####
setMethod("aggregate_data", signature(data="dataObject"),
          function(data){
            
            # Check if loading of the data object was delayed
            if(data@delay_loading){
              # Mark for future aggregation after loading the data
              data@aggregate_on_load <- TRUE
              return(data)
              
            } else {
              # Set aggregation flag to FALSE and continue
              data@aggregate_on_load <- FALSE
            }
            
            # Check if the data is empty
            if(is_empty(data)){
              return(data)
            }
            
            # Identify the columns containing outcome, series, sample, and batch
            # identifiers.
            id_cols <- get_non_feature_columns(x=data, id_depth="series")
            
            # Determine the number of different entries
            
            if(all(data@data$repetition_id==1)){
              # No duplicates present
              return(data)
              
            } else {
              # Repeated measurements or bootstraps.
              
              # Identify feature columns
              feature_columns <- get_feature_columns(x=data)

              # Identify class of features
              column_class <- lapply(feature_columns, function(ii, data) (class(data[[ii]])), data=data@data)
              
              # Determine numerical features
              feat_numeric <- sapply(column_class, function(selected_column_class) (any(selected_column_class %in% c("numeric", "integer"))))
              feat_numeric <- feature_columns[feat_numeric]
              
              # Determine categorical features
              feat_factor <- sapply(column_class, function(selected_column_class) (any(selected_column_class %in% c("logical", "character", "factor"))))
              feat_factor <- feature_columns[feat_factor]
              
              # Find non-duplicate id columns
              dt_out       <- unique(data@data[, id_cols, with=FALSE])
              
              # Add aggregated numeric columns
              if(length(feat_numeric) > 0){
                dt_num     <- data@data[, lapply(.SD, stats::median), by=id_cols, .SDcols=feat_numeric]
                dt_out     <- merge(dt_out, dt_num, by=id_cols)
              }
              
              # Add aggregated factor columns
              if(length(feat_factor) > 0){
                dt_fac     <- data@data[, lapply(.SD, get_mode), by=id_cols, .SDcols=feat_factor]
                dt_out     <- merge(dt_out, dt_fac, by=id_cols)
              }
              
              # Add in repetition_id column again
              dt_out[, "repetition_id":=-1]
              
              # Reorder columns so that it matches the input
              data.table::setcolorder(dt_out, neworder=colnames(data@data))
              
              data@data <- dt_out
              
              return(data)
            }
          })


#####filter_features#####
setMethod("filter_features", signature(data="dataObject"),
          function(data, remove_features=NULL, available_features=NULL){
            # Removes features from a data set

            # If both are provided, use remove_features
            if(!is.null(remove_features) & !is.null(available_features)){
              available_features <- NULL
            }
            
            # Do not do anything if both are null
            if(is.null(remove_features) & is.null(available_features)){
              return(data)
              
            # Based on remove_features input
            } else if(!is.null(remove_features)){
              
              # Determine which remove_features are actually present in data
              remove_features <- intersect(remove_features, get_feature_columns(x=data))
              
            # Based on available_features input
            } else if(!is.null(available_features)){
              
              # Skip if length equals 0
              if(length(available_features) == 0){
                return(data)
              }
              
              # Determine which features should be removed
              remove_features <- setdiff(get_feature_columns(x=data), available_features)
              
              # Only keep features that are in data.
              available_features <- intersect(get_feature_columns(x=data), available_features)
              
            } else {
              stop("This point should never be reachable. Check for inconsistencies if it does.")
            }
            
            # Remove features from data if there is 1 or more feature to remove
            if(length(remove_features) > 0){
              data@data[, (remove_features):=NULL]
            }
            
            # Make sure that the column order is the same as available_features
            if(!is.null(available_features)){
              data.table::setcolorder(data@data, c(get_non_feature_columns(x=data), available_features))
            }
            
            return(data)
          })


#####filter_missing_outcome#####
setMethod("filter_missing_outcome", signature(data="dataObject"),
          function(data, is_validation=FALSE){
            
            # Check if data is empty
            if(is_empty(data)) return(data)
            
            # Change behaviour by outcome_type
            if(data@outcome_type == "survival"){
              outcome_is_valid <- is_valid_data(data@data[["outcome_time"]]) & is_valid_data(data@data[["outcome_event"]])
              
            } else if(data@outcome_type %in% c("binomial", "multinomial", "continuous", "count")) {
              outcome_is_valid <- is_valid_data(data@data[["outcome"]])
              
            } else {
              stop(paste0("Implementation for outcome_type ", data@outcome_type, " is missing."))
            }
            
            if(is_validation){
              # Check whether all outcome information is missing for validation.
              # It may be a prospective study. In that case, keep all data.
              if(all(!outcome_is_valid)) outcome_is_valid <- !outcome_is_valid
            }
            
            # Keep only data for which the outcome exists
            data@data <- data@data[(outcome_is_valid), ]
            
            return(data)
          })


#####filter_bad_samples#####
setMethod("filter_bad_samples", signature(data="dataObject"),
          function(data, threshold){
            
            # Check if data is empty
            if(is_empty(data)){
              return(data)
            }
            
            # Find the columns containing features
            feature_columns <- get_feature_columns(x=data)
            
            # Still very fast, but is much friendlier to our poor memory.
            n_missing <- numeric(nrow(data@data))
            for(feature in feature_columns){
              n_missing <- n_missing + !is_valid_data(data@data[[feature]])
            }
            
            # Set threshold number, as threshold is a fraction.
            threshold_number <- length(feature_columns) * threshold

            # Only keep samples with a number of missing values below the threshold
            data@data <- data@data[(n_missing <= threshold_number)]
            
            return(data)
          })


#####transform_features (dataObject)#####
setMethod("transform_features", signature(data="dataObject"),
          function(data, feature_info_list, invert=FALSE){
            
            # Check if transformation was already performed.
            if(!invert & .as_preprocessing_level(data) >= "transformation"){
              ..error_reached_unreachable_code("transform_features,dataObject: attempting to transform data that are already transformed.")
            }
            
            # Update the preprocessing level.
            if(!invert) data@preprocessing_level <- "transformation"
            if(invert) data@preprocessing_level <- "signature"
            
            # Check if data is empty
            if(is_empty(data)) return(data)
            
            # Find the columns containing features
            feature_columns <- get_feature_columns(x=data)
            
            # Transform features
            data@data <- transform_features(data=data@data,
                                            feature_info_list=feature_info_list,
                                            features=feature_columns,
                                            invert=invert)

            return(data)
          })


#####transform_features (data.table)#####
setMethod("transform_features", signature(data="data.table"),
          function(data, feature_info_list, features, invert=FALSE){
            
            # Check if data is empty
            if(is_empty(data)) return(data)
            
            # Apply transformations
            transformed_list <- lapply(features, function(ii, data, feature_info_list, invert){
              
              x <- apply_feature_info_parameters(object=feature_info_list[[ii]]@transformation_parameters,
                                                 data=data[[ii]],
                                                 invert=invert)
              
              return(x)
            },
            data=data,
            feature_info_list=feature_info_list,
            invert=invert)
            
            # Update name of data in columns
            names(transformed_list) <- features
            
            # Update with replacement in the data object
            data <- update_with_replacement(data=data,
                                            replacement_list=transformed_list)
            
            return(data)
          })


#####normalise_features (dataObject)#####
setMethod("normalise_features", signature(data="dataObject"),
          function(data, feature_info_list, invert=FALSE){
            
            # Check if normalisation was already performed.
            if(!invert & .as_preprocessing_level(data) >= "normalisation"){
              ..error_reached_unreachable_code("normalise_features,dataObject: attempting to normalise data that are already normalised.")
            }
            
            # Check if the previous step (transformation) was conducted.
            if(!invert & .as_preprocessing_level(data) < "transformation"){
              ..error_reached_unreachable_code("normalise_features,dataObject: data should be transformed prior to normalisation.")
            }
            
            # Update the preprocessing_level.
            if(!invert) data@preprocessing_level <- "normalisation"
            if(invert) data@preprocessing_level <- "transformation"
            
            # Check if data is empty
            if(is_empty(data)) return(data)
            
            # Find the columns containing features
            feature_columns <- get_feature_columns(x=data)
            
            # Apply normalisation
            data@data <- normalise_features(data=data@data,
                                            feature_info_list=feature_info_list,
                                            features=feature_columns,
                                            invert=invert)
            
            return(data)
          })


#####normalise_features (data.table)#####
setMethod("normalise_features", signature(data="data.table"),
          function(data, feature_info_list, features, invert=FALSE){
            
            # Check if data is empty.
            if(is_empty(data)) return(data)
            
            # Apply normalisation.
            normalised_list <- lapply(features, function(ii, data, feature_info_list, invert){
              
              x <- apply_feature_info_parameters(object=feature_info_list[[ii]]@normalisation_parameters,
                                                 data=data[[ii]],
                                                 invert=invert)
              
              return(x)
            },
            data=data,
            feature_info_list=feature_info_list,
            invert=invert)
            
            # Update name of data in columns.
            names(normalised_list) <- features
            
            # Update with replacement in the data object.
            data <- update_with_replacement(data=data,
                                            replacement_list=normalised_list)
            
            return(data)
          })


#####batch_normalise_features#########
setMethod("batch_normalise_features", signature(data="dataObject"),
          function(data, feature_info_list, cl=NULL, invert=FALSE){
            
            # Check if batch normalisation was already performed.
            if(!invert & .as_preprocessing_level(data) >= "batch_normalisation"){
              ..error_reached_unreachable_code("batch_normalise_features,dataObject: attempting to batch normalise data that are already batch normalised.")
            }
            
            # Check if the previous step (normalisation) was conducted.
            if(!invert & .as_preprocessing_level(data) < "normalisation"){
              ..error_reached_unreachable_code("batch_normalise_features,dataObject: data should be normalised globally prior to batch normalisation.")
            }
            
            # Update the preprocessing_level.
            if(!invert) data@preprocessing_level <- "batch_normalisation"
            if(invert) data@preprocessing_level <- "normalisation"
            
            # Check if data is empty
            if(is_empty(data)) return(data)
            
            # Find the columns containing features
            feature_columns <- get_feature_columns(x=data)
            
            # Update feature_info_list by adding info for missing batches
            feature_info_list <- add_batch_normalisation_parameters(feature_info_list=feature_info_list,
                                                                    data=data)
            
            # Apply batch-normalisation
            batch_normalised_list <- lapply(feature_columns, function(ii, data, feature_info_list, invert){
              
              # Dispatch to apply-method.
              x <- apply_feature_info_parameters(object=feature_info_list[[ii]]@batch_normalisation_parameters,
                                                 data=data@data[, mget(c(ii, get_id_columns("batch")))],
                                                 invert=invert)
              
              return(x)
            },
            data=data,
            feature_info_list=feature_info_list,
            invert=invert)
            
            # Update name of data in columns
            names(batch_normalised_list) <- feature_columns
            
            # Update with replacement in the data object
            data <- update_with_replacement(data=data, replacement_list=batch_normalised_list)
            
            return(data)
          })



#####impute_features#####
setMethod("impute_features", signature(data="dataObject"),
          function(data, feature_info_list, cl=NULL){
            
            # Check if imputation was already performed.
            if(.as_preprocessing_level(data) >= "imputation"){
              ..error_reached_unreachable_code("impute_features,dataObject: attempting to impute data that already have been imputed.")
            }
            
            # Check if the previous step (batch normalisation) was conducted.
            if(.as_preprocessing_level(data) < "batch_normalisation"){
              ..error_reached_unreachable_code("impute_features,dataObject: data should be batch normalised prior to imputation.")
            }
            
            # Update the attained processing level.
            data@preprocessing_level <- "imputation"
            
            # Check if data is empty
            if(is_empty(data)) return(data)

            # Check if data has features
            if(!has_feature_data(x=data)) return(data)
            
            # Find the columns containing features
            feature_columns <- get_feature_columns(x=data)
            
            # Determine which columns have missing entries
            censored_features <- feature_columns[sapply(feature_columns, function(ii, data_obj) (!all(is_valid_data(data_obj@data[[ii]]))), data_obj=data)]
            
            # Skip if there are no censored features
            if(length(censored_features) == 0) return(data)
            
            # Fill out all censored entries by simple imputation
            uncensored_data <- impute.impute_simple(cl=cl,
                                                    data_obj=data,
                                                    feature_info_list=feature_info_list,
                                                    censored_features=censored_features)
            
            # Fill out all censored entries by lasso-based imputation
            data <- impute.impute_lasso(cl=cl,
                                        data_obj=data,
                                        uncensored_data_obj=uncensored_data,
                                        feature_info_list=feature_info_list,
                                        censored_features=censored_features)
            
            return(data)
          })


#####cluster_features#####
setMethod("cluster_features", signature(data="dataObject"),
          function(data, feature_info_list){
            
            if(.as_preprocessing_level(data) >= "clustering"){
              ..error_reached_unreachable_code("cluster_features,dataObject: attempting to cluster data that already have been clustered.")
            }
            
            # Check if the previous step (imputation) was conducted.
            if(.as_preprocessing_level(data) < "imputation"){
              ..error_reached_unreachable_code("cluster_features,dataObject: data should be imputed prior to clustering.")
            }
            
            # Update the attained processing level.
            data@preprocessing_level <- "clustering"
            
            # Check if data is empty
            if(is_empty(data)) return(data)
            
            # Check if data has features
            if(!has_feature_data(x=data)) return(data)
            
            # Find the columns containing features
            feature_columns <- get_feature_columns(x=data)
            
            # Identify features that form non-singular clusters
            clustering_features <- find_clustering_features(features=feature_columns, feature_info_list=feature_info_list)
            
            # Skip if all clusters are singular
            if(length(clustering_features) == 0) return(data)
            
            # Reconstitute a cluster table
            cluster_table <- get_cluster_table(feature_info_list=feature_info_list, selected_features=clustering_features)

            # Cluster data
            clustered_data <- data.table::setDT(unlist(unname(lapply(split(cluster_table, by="cluster_name"), cluster.compute_cluster, data_obj=data)), recursive=FALSE))
            
            # Combine with non-clustered input data
            data@data <- cbind(data@data[, !clustering_features, with=FALSE], clustered_data)
            
            return(data)
          })



#####update_with_replacement (dataObject)#####
setMethod("update_with_replacement", signature(data="dataObject"),
          function(data, replacement_list){
            # Updates columns of a data table with replacement data from repl_list
            dt_repl <- data.table::copy(data@data)
            
            # Find feature names corresponding to columns to be replaced
            repl_feat <- names(replacement_list)
            
            # Iterate over replacement list entries
            for(curr_feat in repl_feat){
              dt_repl[, (curr_feat):=replacement_list[[curr_feat]] ]
            }
            
            # Replace data
            data@data <- dt_repl
            
            return(data)
          })


#####update_with_replacement (data.table)#####
setMethod("update_with_replacement", signature(data="data.table"),
          function(data, replacement_list){
            # Updates columns of a data table with replacement data from the
            # replacement list.
            replacement_table <- data.table::copy(data)
            
            # Find feature names corresponding to columns to be replaced
            replace_features <- names(replacement_list)
            
            # Iterate over replacement list entries
            for(curr_feat in replace_features){
              replacement_table[, (curr_feat):=replacement_list[[curr_feat]] ]
            }
            
            return(replacement_table)
          })


#####select_features#####
setMethod("select_features", signature(data="dataObject"),
          function(data, features){
            # Allows for slicing the data.
            
            # Find non-feature columns
            non_feature_columns <- get_non_feature_columns(x=data)
            
            # Check if features are present as column name
            if(length(features) > 0){
              if(!all(features %in% colnames(data@data))){
                logger.stop("Not all features were found in the data set.")
              }
              
            } else {
              warning("No features were selected.")
            }
            
            # Define the selected columns
            selected_columns <- unique(c(non_feature_columns, features))
            
            # Check if all columns are already present in the data. In that case
            # we do not need to copy the dataset.
            if(!setequal(selected_columns, colnames(data@data))){
              # Select features
              data@data <- data.table::copy(data@data[, mget(selected_columns)])
            }
            
            return(data)
          })



create_data_column_info <- function(settings){
  
  # Read from settings. If not set, these will be NULL.
  sample_id_column <- settings$data$sample_col
  batch_id_column <- settings$data$batch_col
  series_id_column <- settings$data$series_col
  
  # Replace any missing.
  if(is.null(sample_id_column)) sample_id_column <- NA_character_
  if(is.null(batch_id_column)) batch_id_column <- NA_character_
  if(is.null(series_id_column)) series_id_column <- NA_character_
  
  # Repetition column ids are only internal.
  repetition_id_column <- NA_character_
  
  # Create table
  data_info_table <- data.table::data.table("type"=c("batch_id_column", "sample_id_column", "series_id_column", "repetition_id_column"),
                                            "internal"=get_id_columns(),
                                            "external"=c(batch_id_column, sample_id_column, series_id_column, repetition_id_column))
  
  if(settings$data$outcome_type %in% c("survival", "competing_risk")){
    
    # Find internal and external outcome column names.
    internal_outcome_columns <- get_outcome_columns(settings$data$outcome_type)
    external_outcome_columns <- settings$data$outcome_col
    
    if(is.null(external_outcome_columns)) external_outcome_columns <- c(NA_character_, NA_character_)
    
    # Add to table
    outcome_info_table <- data.table::data.table("type"=c("outcome_column", "outcome_column"),
                                                 "internal"=internal_outcome_columns,
                                                 "external"=external_outcome_columns)
    
  } else if(settings$data$outcome_type %in% c("binomial", "multinomial", "continuous", "count")){
    
    # Find internal and external outcome column names.
    internal_outcome_columns <- get_outcome_columns(settings$data$outcome_type)
    external_outcome_columns <- settings$data$outcome_col
    
    if(is.null(external_outcome_columns)) external_outcome_columns <- c(NA_character_)
    
    # Add to table
    outcome_info_table <- data.table::data.table("type"="outcome_column",
                                                 "internal"=internal_outcome_columns,
                                                 "external"=external_outcome_columns)
    
  } else if(settings$data$outcome_type %in% c("unsupervised")){
    
    # There is no outcome for unsupervised learners.
    outcome_info_table <- NULL
    
  } else {
    ..error_no_known_outcome_type(outcome_type=settings$data$outcome_type)
  }
  
  # Combine into one table and add to object
  return(rbind(data_info_table, outcome_info_table))
}

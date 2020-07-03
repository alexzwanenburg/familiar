#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#####.train#####
setMethod(".train", signature(object="familiarModel", data="dataObject"),
          function(object, data, get_additional_info=FALSE, is_pre_processed=FALSE) {
            # Train method for model training
            
            # Check if the class of object is a subclass of familiarModel.
            if(!is_subclass(class(object)[1], "familiarModel")) object <- promote_learner(object)
            
            # Process data, if required.
            data <- process_input_data(object=object,
                                       data=data,
                                       is_pre_processed = is_pre_processed,
                                       stop_at="clustering")
            
            # Check if there are any data entries. The familiar model cannot be
            # trained otherwise
            if(is_empty(x=data)) return(object)
            
            # Check the number of features in data; if it has no features, the
            # familiar model can not be trained
            if(!has_feature_data(x=data)) return(object)
            
            # Train a new model based on data.
            object <- ..train(object=object, data=data)
            
            # Extract information required for assessing model performance,
            # calibration (e.g. baseline survival) etc.
            if(get_additional_info){
              # Remove duplicate subject_id from the data prior to obtaining fut
              data <- aggregate_data(data=data)
              
              # Create calibration models and add to the object. Not all models
              # require recalibration.
              object <- ..set_recalibration_model(object=object, data=data)
              
              # Extract data required for assessing calibration. Not all outcome
              # types require calibration info. Currently calibration
              # information is only retrieved for survival outcomes, in the form
              # of baseline survival curves.
              object <- ..set_calibration_info(object=object, data=data)
              
              # Set stratification thresholds. This is currently only done for
              # survival outcomes.
              object <- ..set_risk_stratification_thresholds(object=object, data=data)
              
              # TODO Novelty detector
              object@novelty_detector <- NULL
              
              # Add column data
              object <- add_data_column_info(object=object)
            }
            
            # Add outcome distribution data
            object@outcome_info <- .compute_outcome_distribution_data(object=object@outcome_info, data=data)
            
            return(object)
          })


#####assess_performance (model)#####
setMethod("assess_performance", signature(object="familiarModel"),
          function(object, newdata, metric, allow_recalibration=TRUE, is_pre_processed=FALSE, time_max=NULL, as_objective=FALSE, na.rm=FALSE){
            
            # This function is the same for familiarModel and familiarEnsemble objects
            return(.assess_performance(object=object,
                                       data=newdata, 
                                       metric=metric,
                                       allow_recalibration=allow_recalibration,
                                       is_pre_processed=is_pre_processed,
                                       time=time_max,
                                       as_objective=as_objective,
                                       na.rm=na.rm))
            
          })

#####assess_calibration (model)#####
setMethod("assess_calibration", signature(object="familiarModel"),
          function(object, data, eval_times=NULL, is_pre_processed=FALSE){
            
            # Load eval_times from the object settings attribute, if it is not provided.
            if(is.waive(eval_times)){
              eval_times <- object@settings$eval_times
            }
            
            # Check eval_times argument
            if(object@outcome_type %in% c("survival")){
              .check_number_in_valid_range(eval_times, var_name="eval_times", range=c(0.0, Inf), closed=c(FALSE, TRUE))
            }
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)){
              ..error_ensemble_models_not_loaded()
            }
            
            # This function is the same for familiarModel and familiarEnsemble objects
            return(.assess_calibration(object=object, data=data, eval_times=eval_times, is_pre_processed=is_pre_processed))
            
          })

#####assign_risk_groups (model)#####
setMethod("assign_risk_groups", signature(object="familiarModel", prediction_data="data.frame"),
          function(object, prediction_data, stratification_method=NULL){

            # Check if predictions were generated
            if(is.null(prediction_data)){
              return(NULL)
            }

            # Allow for selection of a single method by setting stratification_method
            if(is.null(stratification_method)){
              km_info_list <- object@km_info$parameters
            } else {
              
              if(!any(stratification_method %in% object@km_info$stratification_method)){
                stop(paste0("Data for stratification method ", stratification_method, " was not found."))
              }
              
              km_info_list <- object@km_info$parameters[stratification_method]
            }
            
            # Iterate over methods used to determine thresholds
            data <- lapply(km_info_list, function(km_info, object, prediction_data, learner){
              
              # Assign risk group based on cutoff values stored in the familiarModel object
              risk_group <- learner.apply_risk_threshold(object=object,
                                                         predicted_values=prediction_data$predicted_outcome,
                                                         cutoff=km_info$cutoff,
                                                         learner=learner)
              
              # Create a table to assign the risk group.
              data <- data.table::data.table("strat_method"=km_info$method,
                                             "subject_id"=prediction_data$subject_id,
                                             "cohort_id"=prediction_data$cohort_id,
                                             "repetition_id"=prediction_data$repetition_id,
                                             "outcome_time"=prediction_data$outcome_time,
                                             "outcome_event"=prediction_data$outcome_event,
                                             "risk_group"=risk_group)
              
              return(data)
              
            }, object=object, prediction_data=prediction_data, learner=object@learner)
            
            return(data)
          })


######assess_stratification (model)######
setMethod("assess_stratification", signature(object="familiarModel"),
          function(object, ...){
            
            # Convert to familiarEnsemble to simply calls.
            object <- as_familiar_ensemble(object=object)
            
            return(do.call(assess_stratification, args=append(list("object"=object),
                                                              list(...))))
          })


#####compute_calibration_data (model, dataObject)#####
setMethod("compute_calibration_data", signature(object="familiarModel", data="dataObject"),
          function(object, data, time=NULL){
            
            # This function is the same for familiarModel and familiarEnsemble objects
            return(.compute_calibration_data(object=object, data=data, time=time))
          })

# #####compute_calibration_data (model, list)#####
# setMethod("compute_calibration_data", signature(object="familiarModel", data="list"),
#           function(object, data, time_max=NULL){
#             
#             # This function is the same for familiarModel and familiarEnsemble objects
#             return(.compute_calibration_data(object=object, data=data, time=time))
#           })

#####save (model)#####
setMethod("save", signature(list="familiarModel", file="character"),
          function(list, file) {
            .save(object=list, dir_path=file)
          })

#####process_input_data (model)#####
setMethod("process_input_data", signature(object="familiarModel", data="ANY"),
          function(object, data, is_pre_processed=FALSE, stop_at="clustering"){
            # Prepares data for prediction, assessing calibration etc.
            
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

#####add_model_name (model)#####
setMethod("add_model_name", signature(data="ANY", object="familiarModel"),
          function(data, object){
              
            # This is the same for objects of the familiarModel and familiarEnsemble classes
            return(.add_model_name(data=data, object=object))
          })

#####get_object_name (model)#####
setMethod("get_object_name", signature(object="familiarModel"),
          function(object, abbreviated=FALSE){
            
            # Extract data and run id
            model_data_id <- tail(object@run_table, n=1)$data_id
            model_run_id  <- tail(object@run_table, n=1)$run_id
            
            if(abbreviated){
              # Create an abbreviated name
              model_name <- paste0("model.", model_data_id, ".", model_run_id)
            } else {
              # Create the full name of the model
              model_name <- get_object_file_name(learner=object@learner, fs_method=object@fs_method, project_id=object@project_id, data_id=model_data_id,
                                                 run_id=model_run_id, object_type="familiarModel", with_extension=FALSE)
            }
            
            return(model_name)
          })

#####model_is_trained (model)#####
setMethod("model_is_trained", signature(object="familiarModel"),
          function(object){
            # Check if a model was trained
            if(is.null(object@model)){
              # Check if a model is present
              return(FALSE)
              
            } else {
              # Assume that the model is present if it is not specifically
              # stated using the model_trained element
              return(TRUE)
            }
          })


#####add_package_version (model)#####
setMethod("add_package_version", signature(object="familiarModel"),
          function(object){
            
            # Set version of familiar
            return(.add_package_version(object=object))
          })


setMethod("add_data_column_info", signature(object="familiarModel"),
          function(object, sample_id_column=NULL, batch_id_column=NULL){
            
            # Don't determine new column information if this information is
            # already present.
            if(!is.null(object@data_column_info)) return(object)
            
            if(is.null(sample_id_column) & is.null(batch_id_column)){
              # Load settings to find identifier columns
              settings <- get_settings()
              
              # Read from settings. If not set, these will be NULL.
              sample_id_column <- settings$data$sample_col
              batch_id_column <- settings$data$batch_col
            }
            
            # Replace any missing.
            if(is.null(sample_id_column)) sample_id_column <- NA_character_
            
            if(is.null(batch_id_column)) batch_id_column <- NA_character_
            
            # Repetition column ids are only internal.
            repetition_id_column <- NA_character_
            
            # Create table
            data_info_table <- data.table::data.table("type"=c("sample_id_column", "batch_id_column", "repetition_id_column"),
                                                      "internal"=c("subject_id", "cohort_id", "repetition_id"),
                                                      "external"=c(sample_id_column, batch_id_column, repetition_id_column))
            
            if(object@outcome_type %in% c("survival", "competing_risk")){
              
              # Find internal and external outcome column names.
              internal_outcome_columns <- get_outcome_columns(object@outcome_type)
              external_outcome_columns <- object@outcome_info@outcome_column
              
              # Add to table
              outcome_info_table <- data.table::data.table("type"=c("outcome_column", "outcome_column"),
                                                           "internal"=internal_outcome_columns,
                                                           "external"=external_outcome_columns)
              
            } else if(object@outcome_type %in% c("binomial", "multinomial", "continuous", "count")){
              
              # Find internal and external outcome column names.
              internal_outcome_columns <- get_outcome_columns(object@outcome_type)
              external_outcome_columns <- object@outcome_info@outcome_column
              
              # Add to table
              outcome_info_table <- data.table::data.table("type"="outcome_column",
                                                           "internal"=internal_outcome_columns,
                                                           "external"=external_outcome_columns)
              
            } else {
              ..error_no_known_outcome_type(outcome_type=object@outcome_type)
            }
            
            # Combine into one table and add to object
            object@data_column_info <- rbind(data_info_table, outcome_info_table)
            
            return(object)
          })


#####is_available#####
setMethod("is_available", signature(object="familiarModel"),
          function(object, ...) return(FALSE))


#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarModel"),
          function(object, ...) return(list()))


#####..train#####
setMethod("..train", signature(object="familiarModel", data="dataObject"),
          function(object, data){
            
            # Set a NULL model
            object@model <- NULL
            
            return(object)
          })


#####..train#####
setMethod("..train", signature(object="familiarModel", data="NULL"),
          function(object, data){
            
            # Set a NULL model
            object@model <- NULL
            
            return(object)
          })

#####..predict#####
setMethod("..predict", signature(object="familiarModel", data="dataObject"),
          function(object, data, ...) return(get_placeholder_prediction_table(object=object, data=data)))

#####..predict_survival_probability####
setMethod("..predict_survival_probability", signature(object="familiarModel", data="dataObject"),
          function(object, data, time) return(get_placeholder_prediction_table(object=object, data=data)))

#####..set_calibration_info#####
setMethod("..set_calibration_info", signature(object="familiarModel"),
          function(object, data){
            if(is.null(object@calibration_info)) object@calibration_info <- NULL
            
            return(object)
          })

#####..set_recalibration_model#####
setMethod("..set_recalibration_model", signature(object="familiarModel", data="dataObject"),
          function(object, data){
            
            # Set a series of NULL models.
            object@calibration_model <- NULL
            
            return(object)
          })

#####..set_risk_stratification_thresholds#####
setMethod("..set_risk_stratification_thresholds", signature(object="familiarModel", data="dataObject"),
          function(object, data){
            
            if(object@outcome_type %in% c("survival")){
              object@km_info <- learner.find_survival_grouping_thresholds(object=object, data=data)
            } else {
              object@km_info <- NULL
            }
            
            return(object)
          })
#####..set_vimp_parameters#####
setMethod("..set_vimp_parameters", signature(object="familiarModel"),
          function(object, ...) return(object))

#####..vimp######
setMethod("..vimp", signature(object="familiarModel"),
          function(object, ...) return(get_placeholder_vimp_table()))

####has_calibration_info####
setMethod("has_calibration_info", signature(object="familiarModel"),
          function(object) return(!is.null(object@calibration_info)))

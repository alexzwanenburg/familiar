#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#####train#####
setMethod("train", signature(object="familiarModel", data="ANY"),
          function(object, data, get_recalibration=FALSE, get_additional_info=FALSE) {
            # Train method for model training
            
            # Check if input data is available.
            if(is.null(data)){
              stop("Data should be provided for training a supervised learner.")
            } else if(class(data)!="dataObject"){
              stop("Data should be a dataObject.")
            }
            
            # Extract outcome type
            fam_model    <- object
            outcome_type <- fam_model@outcome_type
            
            # Check if there are any data entries. The familiar model cannot be trained otherwise
            if(is_empty(x=data)){ return(fam_model) }
            
            # Check the number of features in data; if it has no features, the familiar model can not be trained
            if(!has_feature_data(x=data)){ return(fam_model) }
            
            # Train a new model based on data in dt
            fam_model@model <- learner.main(object=fam_model, data_obj=data, purpose="train")
            
            # Allow model recalibration
            if(get_recalibration){
              # Remove duplicate subject_id from the data for validation
              data <- aggregate_data(data=data)
              
              # Create calibration models
              fam_model@calibration_model <- learner.main(object=fam_model, data_obj=data, purpose="recalibrate")
            }
            
            # Extract information required for assessing model performance, calibration (e.g. baseline survival) etc.
            if(get_additional_info){
              # Remove duplicate subject_id from the data prior to calibration
              data       <- aggregate_data(data=data)
              
              # Extract data required for assessing calibration
              fam_model@calibration_info <- learner.main(object=fam_model, data_obj=data, purpose="calibration_info")
              
              if(outcome_type =="survival"){
                fam_model@km_info <- learner.find_survival_grouping_thresholds(object=fam_model, data_obj=data)
              }
              
            }
            
            # Extract mean outcome value from input data and attach to the model
            if(outcome_type %in% c("count", "continuous")){
              fam_model@mean_outcome_value <- mean(data@data$outcome)
            }
            
            return(fam_model)
          })


#####assess_performance (model)#####
setMethod("assess_performance", signature(object="familiarModel"),
          function(object, newdata, metric, allow_recalibration=TRUE, is_pre_processed=FALSE, time_max=NULL, as_objective=FALSE, na.rm=FALSE){
            
            # This function is the same for familiarModel and familiarEnsemble objects
            return(.assess_performance(object=object, newdata=newdata, metric=metric, allow_recalibration=allow_recalibration,
                                       is_pre_processed=is_pre_processed, time_max=time_max, as_objective=as_objective, na.rm=na.rm))
            
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
            data <- lapply(km_info_list, function(km_info, prediction_data, learner){
              
              # Assign risk group based on cutoff values stored in the familiarModel object
              risk_group <- learner.apply_risk_threshold(predicted_values=prediction_data$outcome_pred,
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
              
            }, prediction_data=prediction_data, learner=object@learner)
            
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
          function(object, data, time_max=NULL){
            
            # This function is the same for familiarModel and familiarEnsemble objects
            return(.compute_calibration_data(object=object, data=data, time_max=time_max))
          })

#####compute_calibration_data (model, list)#####
setMethod("compute_calibration_data", signature(object="familiarModel", data="list"),
          function(object, data, time_max=NULL){
            
            # This function is the same for familiarModel and familiarEnsemble objects
            return(.compute_calibration_data(object=object, data=data, time_max=time_max))
          })

#####save (model)#####
setMethod("save", signature(list="familiarModel", file="character"),
          function(list, file) {
            .save(object=list, dir_path=file)
          })

#####process_input_data (model)#####
setMethod("process_input_data", signature(object="familiarModel", data="ANY"),
          function(object, data, is_pre_processed=FALSE){
            # Prepares data for prediction, assessing calibration etc.
            
            # Check whether data is a dataObject, and create one otherwise
            if(class(data)!="dataObject"){
              data <- create_data_object(object=object, data=data, is_pre_processed=is_pre_processed)
            }
            
            # Load data from internal memory, if not provided otherwise
            if(data@delay_loading){
              data <- load_delayed_data(data=data, object=object)
            }

            # Pre-process data in case it has not been pre-processed
            if(!data@is_pre_processed){
              data <- preprocess_data(data=data, object=object)
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
              
            } else if(!(is.null(object@model$model_trained))){
              # Look at the model_trained list element that is created by
              # many implementations during training
              return(object@model$model_trained)
              
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

#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


#####.train#####
setMethod(".train", signature(object="familiarModel", data="dataObject"),
          function(object, 
                   data,
                   get_additional_info=FALSE,
                   is_pre_processed=FALSE,
                   trim_model=TRUE,
                   ...) {
            # Train method for model training
            
            # Check if the class of object is a subclass of familiarModel.
            if(!is_subclass(class(object)[1], "familiarModel")) object <- promote_learner(object)
            
            # Process data, if required.
            data <- process_input_data(object=object,
                                       data=data,
                                       is_pre_processed = is_pre_processed,
                                       stop_at="clustering")
            
            # Work only with data that has known outcomes when training.
            data <- filter_missing_outcome(data=data)
            
            # Set the training flag
            can_train <- TRUE
            
            # Check if there are any data entries. The familiar model cannot be
            # trained otherwise
            if(is_empty(x=data)) can_train <- FALSE
            
            # Check the number of features in data; if it has no features, the
            # familiar model can not be trained
            if(!has_feature_data(x=data)) can_train <- FALSE
            
            # Check if the hyperparameters are plausible.
            if(!has_optimised_hyperparameters(object=object)) can_train <- FALSE
            
            # Train a new model based on data.
            if(can_train) object <- ..train(object=object, data=data)
            
            # Extract information required for assessing model performance,
            # calibration (e.g. baseline survival) etc.
            if(get_additional_info){

              # Remove duplicate samples from the data prior to obtaining
              # additional data.
              data <- aggregate_data(data=data)
              
              # Create calibration models and add to the object. Not all models
              # require recalibration.
              if(can_train) object <- ..set_recalibration_model(object=object, data=data)
              
              # Extract data required for assessing calibration. Not all outcome
              # types require calibration info. Currently calibration
              # information is only retrieved for survival outcomes, in the form
              # of baseline survival curves.
              if(can_train) object <- ..set_calibration_info(object=object, data=data)
              
              # Set stratification thresholds. This is currently only done for
              # survival outcomes.
              if(can_train) object <- ..set_risk_stratification_thresholds(object=object, data=data)
              
              # Add column data
              object <- add_data_column_info(object=object,
                                             data=data)
            }
            
            if(trim_model) object <- trim_model(object=object)
            
            # Add outcome distribution data
            object@outcome_info <- .compute_outcome_distribution_data(object=object@outcome_info, data=data)
            
            # Empty slots if a model can not be trained.
            if(!can_train){
              object@required_features <- NULL
              object@model_features <- NULL
              object@novelty_features <- NULL
            }
            
            return(object)
          })


#####.train_novelty_detector#####
setMethod(".train_novelty_detector", signature(object="familiarModel", data="dataObject"),
          function(object,
                   data,
                   detector,
                   get_additional_info=FALSE,
                   is_pre_processed=FALSE,
                   trim_model=TRUE,
                   ...) {
            # Train method for novelty detectors.
            
            # Check if the class of object is a subclass of familiarModel.
            if(!is_subclass(class(object)[1], "familiarModel")) object <- promote_learner(object)
            
            # Process data, if required.
            data <- process_input_data(object=object,
                                       data=data,
                                       is_pre_processed = is_pre_processed,
                                       stop_at="clustering")
            
            # Create detector object.
            fam_detector <- methods::new("familiarNoveltyDetector",
                                         learner=detector,
                                         feature_info=object@feature_info,
                                         required_features=object@required_features,
                                         model_features=object@novelty_features,
                                         run_table=object@run_table,
                                         project_id=object@project_id)
            
            # Promote to the correct type of detector.
            fam_detector <- promote_detector(object=fam_detector)
            
            # Optimise hyperparameters if they were not previously set.
            if(!has_optimised_hyperparameters(object=fam_detector)){
              fam_detector <- optimise_hyperparameters(object=fam_detector,
                                                       data=data,
                                                       ...)
            }
            
            # Train the novelty detector.
            fam_detector <- .train(object=fam_detector,
                                   data=data)
            
            # Add the detector to the familiarModel object.
            object@novelty_detector <- fam_detector
            
            return(object)
          })


#####show (model)#####
setMethod("show", signature(object="familiarModel"),
          function(object){
            if(!model_is_trained(object)){
              cat(paste0("A ", object@learner, " model (class: ", class(object)[1],
                         ") that was not successfully trained (v", object@familiar_version, ").\n"))
              
            } else {
              # Describe the learner and the version of familiar.
              message_str <- paste0("A ", object@learner, " model (class: ", class(object)[1],
                                    "; v", object@familiar_version, ")")
              
              # Describe the package(s), if any
              if(!is.null(object@package)){
                message_str <- c(message_str,
                                 paste0(" trained using "),
                                 paste_s(mapply(..message_package_version, x=object@package, version=object@package_version)),
                                 ifelse(length(object@package) > 1, " packages", " package"))
              }
              
              # Complete message and write.
              message_str <- paste0(c(message_str, ".\n"), collapse="")
              cat(message_str)
              
              cat(paste0("\n--------------- Model details ---------------\n"))
              
              # Model details
              if(object@is_trimmed){
                cat(object@trimmed_function$show, sep="\n")
                
              } else {
                show(object@model)
              }

              cat(paste0("---------------------------------------------\n"))
              
              # Outcome details
              cat("\nThe following outcome was modelled:\n")
              show(object@outcome_info)
              
              # Details concerning hyperparameters.
              cat("\nThe model was trained using the following hyperparameters:\n")
              invisible(lapply(names(object@hyperparameters), function(x, object){
                cat(paste0("  ", x, ": ", object@hyperparameters[[x]], "\n"))
              }, object=object))
              
              # Details concerning variable importance.
              cat(paste0("\nVariable importance was determined using the ", object@fs_method, " variable importance method.\n"))
              
              # Details concerning model features:
              cat("\nThe following features were used in the model:\n")
              lapply(object@model_features, function(x, object) show(object@feature_info[[x]]), object=object)
              
              # Details concerning novelty features:
              if(is.null(object@novelty_detector)){
                cat("\nNo novelty detector was trained.\n")
                
              } else if(setequal(object@model_features, object@novelty_features)){
                cat("\nA novelty detector was trained using the model features.\n")
                
              } else {
                cat("\nA novelty detector was trained using the model features above, and additionally:\n\n")
                
                # Identify novelty features that were set in addition to model
                # features.
                novelty_features <- setdiff(object@novelty_features, object@model_features)
                
                lapply(novelty_features, function(x, object) show(object@feature_info[[x]]), object=object)
              }
              
              # Check package version.
              check_package_version(object)
            }
          })



#####require_package (model)#####
setMethod("require_package", signature(x="familiarModel"),
          function(x, purpose=NULL, message_type="error", ...){
            
            # Skip if no package is required.
            if(is_empty(x@package)) return(invisible(TRUE))
            
            # Set standard purposes for common uses.
            if(!is.null(purpose)){
              if(purpose %in% c("train", "vimp", "predict", "show", "distribution")){
                purpose <- switch(purpose,
                                  "train"="to train a model",
                                  "vimp"="to determine variable importance",
                                  "predict"="to create model predictions",
                                  "show"="to capture output",
                                  "distribution"="to set the model distribution")
              }
            }
            
            return(invisible(.require_package(x=x@package, purpose=purpose, message_type=message_type)))
          })



#####set_package_version (model)#####
setMethod("set_package_version", signature(object="familiarModel"),
          function(object){
            # Do not add package versions if there are no packages.
            if(is_empty(object@package)) return(object)
            
            # Obtain package versions.
            object@package_version <- sapply(object@package, function(x) (as.character(utils::packageVersion(x))))
            
            return(object)
          })



#####check_package_version (model)#####
setMethod("check_package_version", signature(object="familiarModel"),
          function(object){
            .check_package_version(name=object@package,
                                   version=object@package_version,
                                   when="at model creation")
          })



#####save (model)#####
setMethod("save", signature(list="familiarModel", file="character"),
          function(list, file) {
            .save(object=list, dir_path=file)
          })



#####add_model_name (ANY, familiarModel)---------------------
setMethod("add_model_name", signature(data="ANY", object="familiarModel"),
          function(data, object){
            if(is_empty(data)) return(NULL)
            
            ..error_reached_unreachable_code("add_model_name,any,familiarModel: no method for non-empty data.")
          })

#####add_model_name (familiarDataElement, familiarModel)------------------------
setMethod("add_model_name", signature(data="familiarDataElement", object="familiarModel"),
          function(data, object){
            
            # Determine the model name
            if(length(object@name) == 0){
              model_name <- get_object_name(object=object, abbreviated=TRUE)
              
            } else {
              model_name <- object@name
            }
            
            if(is.null(data@identifiers)){
              data@identifiers <- list("model_name" = model_name)
              
            } else {
              data@identifiers[["model_name"]] <- model_name
            }

            return(data)
          })

#####add_model_name (familiarDataElement, character)----------------------------
setMethod("add_model_name", signature(data="familiarDataElement", object="character"),
          function(data, object){
            # Load object.
            object <- load_familiar_object(object)
            
            return(do.call(add_model_name, args=c(list("data"=data,
                                                       "object"=object))))
          })



#####set_object_name (familiarModel)#####

#' @title Set the name of a `familiarModel` object.
#'  
#' @description Set the `name` slot using the object name.
#'
#' @param x A `familiarModel` object.
#' 
#' @return A `familiarModel` object with a generated or a provided name.
#' @md
#' @keywords internal
setMethod("set_object_name", signature(x="familiarModel"),
          function(x, new=NULL){
            
            if(x@project_id == 0 & is.null(new)){
              # Generate a random object name. A project_id of 0 means that the
              # objects was auto-generated (i.e. through object conversion). We
              # randomly generate characters and add a time stamp, so that
              # collision is practically impossible.
              slot(object=x, name="name") <- paste0(as.character(as.numeric(format(Sys.time(),"%H%M%S"))),
                                                    "_", stringi::stri_rand_strings(1, 20, '[A-Z]'))
              
            } else if(is.null(new)) {
              # Generate a sensible object name.
              slot(object=x, name="name") <- get_object_name(object=x)
              
            } else {
              slot(object=x, name="name") <- new
            }
            
            return(x)
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
              model_name <- get_object_file_name(learner=object@learner,
                                                 fs_method=object@fs_method,
                                                 project_id=object@project_id,
                                                 data_id=model_data_id,
                                                 run_id=model_run_id,
                                                 object_type="familiarModel",
                                                 with_extension=FALSE)
            }
            
            return(model_name)
          })



#####model_is_trained (familiarModel)#####
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

#####model_is_trained (character)#####
setMethod("model_is_trained", signature(object="character"),
          function(object){
            # Load object.
            object <- load_familiar_object(object)
            
            return(do.call(model_is_trained, args=c(list("object"=object))))
          })



#####add_package_version (familiarModel)#####
setMethod("add_package_version", signature(object="familiarModel"),
          function(object){
            
            # Set version of familiar
            return(.add_package_version(object=object))
          })


#####add_data_column_info (familiarModel)#####
setMethod("add_data_column_info", signature(object="familiarModel"),
          function(object, data=NULL, sample_id_column=NULL, batch_id_column=NULL, series_id_column=NULL){
            
            # Don't determine new column information if this information is
            # already present.
            if(!is.null(object@data_column_info)) return(object)
            
            # Don't determine new column information if this information can be
            # inherited from a dataObject.
            if(is(data, "dataObject")){
              if(!is_empty(data@data_column_info)){
                object@data_column_info <- data@data_column_info
                
                return(object)
              }
            }
            
            # Load settings to find identifier columns
            settings <- get_settings()
            
            # Read from settings. If not set, these will be NULL.
            if(is.null(sample_id_column)) sample_id_column <- settings$data$sample_col
            if(is.null(batch_id_column)) batch_id_column <- settings$data$batch_col
            if(is.null(series_id_column)) series_id_column <- settings$data$series_col
            
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



#####..train (familiarModel, dataObject)#####
setMethod("..train", signature(object="familiarModel", data="dataObject"),
          function(object, data, ...){
            
            # Set a NULL model
            object@model <- NULL
            
            return(object)
          })

#####..train (familiarModel, NULL)#####
setMethod("..train", signature(object="familiarModel", data="NULL"),
          function(object, data, ...){
            
            # Set a NULL model
            object@model <- NULL
            
            return(object)
          })




#####..predict (familiarModel, dataObject)#####
setMethod("..predict", signature(object="familiarModel", data="dataObject"),
          function(object, data, ...) return(get_placeholder_prediction_table(object=object, data=data)))

#####..predict (character, dataObject)#####
setMethod("..predict", signature(object="character", data="dataObject"),
          function(object, data, ...){
            # Load object.
            object <- load_familiar_object(object)
            
            return(do.call(..predict, args=c(list("object"=object,
                                                  "data"=data))))
          })



#####..predict_survival_probability (familiarModel, dataObject)####
setMethod("..predict_survival_probability", signature(object="familiarModel", data="dataObject"),
          function(object, data, time) return(get_placeholder_prediction_table(object=object, data=data, type="survival_probability")))


#####..predict (character, dataObject)#####
setMethod("..predict_survival_probability", signature(object="character", data="dataObject"),
          function(object, data, ...){
            # Load object.
            object <- load_familiar_object(object)
            
            return(do.call(..predict_survival_probability, args=c(list("object"=object,
                                                                       "data"=data),
                                                                  list(...))))
          })



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
            
            if(object@outcome_type %in% c("survival", "competing_risk") & model_is_trained(object)){
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

#####trim_model (familiarModel)-------------------------------------------------
setMethod("trim_model", signature(object="familiarModel"),
          function(object, ...){
            
            # Do not trim the model if there is nothing to trim.
            if(!model_is_trained(object)) return(object)
            
            # Trim the model.
            trimmed_object <- .trim_model(object=object)
            
            # Skip further processing if the model object was not trimmed.
            if(!trimmed_object@is_trimmed) return(object)
            
            # Go over different functions.
            trimmed_object <- .replace_broken_functions(object=object,
                                                        trimmed_object=trimmed_object)
            
            return(trimmed_object)
          })


#####.trim_model (familiarModel)------------------------------------------------
setMethod(".trim_model", signature(object="familiarModel"),
          function(object, ...){
            # Default method for models that lack a more specific method.
            return(object)
          })


####has_calibration_info####
setMethod("has_calibration_info", signature(object="familiarModel"),
          function(object) return(!is.null(object@calibration_info)))


#####set_signature (familiarModel)----------------------------------------------
setMethod("set_signature", signature(object="familiarModel"),
          function(object, rank_table=NULL, signature_features=NULL, minimise_footprint=FALSE, ...){
            
            if(is.null(rank_table) & is.null(signature_features)){
              ..error_reached_unreachable_code("set_signature: rank_table and signature_features cannot both be NULL")
            }
            
            if(is.null(signature_features)){
              # Get signature features using the table with ranked features.
              # Those features may be clustered.
              signature_features <- get_signature(object=object,
                                                  rank_table=rank_table)
            }
            
            # Find important features, i.e. those that constitute the signature
            # either individually or as part of a cluster.
            model_features <- find_model_features(features=signature_features,
                                                  feature_info_list=object@feature_info)
            
            # Find novelty features.
            novelty_features <- find_novelty_features(model_features=model_features,
                                                      feature_info_list=object@feature_info)
            
            if(minimise_footprint){
              # Find only features that are required for running the model.
              required_features <- union(model_features, novelty_features)
              
            } else {
              # Find features that are required for processing the data.
              required_features <- find_required_features(features=signature_features,
                                                          feature_info_list=object@feature_info)
            }
            
            # Select only necessary feature info objects.
            available_feature_info <- names(object@feature_info) %in% unique(c(required_features, model_features, novelty_features))
            object@feature_info <- object@feature_info[available_feature_info]
            
            # Set feature-related attribute slots
            object@required_features <- required_features
            object@model_features <- model_features
            object@novelty_features <- novelty_features
            
            return(object)
          })


#####get_signature (familiarModel)----------------------------------------------
setMethod("get_signature", signature(object="familiarModel"),
          function(object, rank_table=NULL, ...){
            
            # Attempt to get signature directly from the object.
            if(!is_empty(object@model_features)){
              return(features_after_clustering(features=object@model_features,
                                               feature_info_list=object@feature_info))
            }
            
            return(do.call(get_signature, args=list("object"=object@feature_info,
                                                    "vimp_method"=object@fs_method,
                                                    "parameter_list"=object@hyperparameters,
                                                    "rank_table"=rank_table)))
          })


#####get_signature (list)-------------------------------------------------------
setMethod("get_signature", signature(object="list"),
          function(object, vimp_method, parameter_list, rank_table, ...){
            
            # Suppress NOTES due to non-standard evaluation in data.table
            name <- aggr_rank <- NULL
            
            # Get signature size
            if(is_empty(parameter_list$sign_size)){
              signature_size <- 0
              
            } else {
              signature_size <- parameter_list$sign_size
            }
            
            # Find features that are pre-assigned to the signature.
            signature_features <- names(object)[sapply(object, is_in_signature)]
            
            if(vimp_method == "signature_only"){
              # Only select signature
              if(length(signature_features) == 0) stop("No signature was provided.")
              
              selected_features <- signature_features
              
            } else if(vimp_method == "none"){
              # Select all features
              selected_features <- features_after_clustering(features=get_available_features(feature_info_list=object),
                                                             feature_info_list=object)
              
              # Order randomly so that there is no accidental dependency on
              # order.
              selected_features <- fam_sample(x=selected_features,
                                              size=length(selected_features),
                                              replace=FALSE)
              
            } else if(vimp_method == "random"){
              # Select all features.
              selected_features <- features_after_clustering(features=get_available_features(feature_info_list=object),
                                                             feature_info_list=object)
              
              # Randomly pick the signature.
              selected_features <- fam_sample(x=selected_features,
                                              size=signature_size,
                                              replace=FALSE)
              
            } else {
              # Select signature and any additional features according to rank.
              selected_features <- signature_features
              
              # Get number remaining available features
              n_allowed_features <- signature_size - length(signature_features)
              if(n_allowed_features > 0){
                
                # Get available features.
                features <- features_after_clustering(features=get_available_features(feature_info_list=object),
                                                      feature_info_list=object)
                
                # Remove signature features, if any, to prevent duplicates.
                features <- setdiff(features, signature_features)
                
                # Keep only feature ranks of feature corresponding to available features,
                # and order by rank.
                rank_table <- rank_table[name %in% features,][order(aggr_rank)]
                
                # Add good features (low rank) to the selection
                selected_features <- c(signature_features,
                                       head(x=rank_table, n=n_allowed_features)$name)
              }
            }
            
            return(selected_features)
          })


.get_available_risklike_prediction_types <- function(){
  return(c("hazard_ratio", "cumulative_hazard", "survival_probability"))
}

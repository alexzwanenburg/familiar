#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarDataElementIndividualConditionalExpectation",
         contains="familiarDataElement",
         slots=list("class_levels"="ANY"),
         prototype = methods::prototype(class_levels=NULL))

setClass("familiarDataElementPartialDependence",
         contains="familiarDataElement",
         slots=list("class_levels"="ANY"),
         prototype = methods::prototype(class_levels=NULL))


#'@title Internal function to extract data for individual conditional
#'  expectation plots.
#'
#'@description Computes data for individual conditional expectation plots and
#'  partial dependence plots for the model(s) in a `familiarEnsemble` object.
#'
#'@param features Names of the feature or features (2) assessed simultaneously.
#'  By default `NULL`, which means that all features are assessed one-by-one.
#'@param n_sample_points Number of points used to sample continuous features.
#'@inheritParams extract_data
#'
#'@return A data.table containing predicted and observed outcome data together
#'  with a co-occurence count.
#'@md
#'@keywords internal
setGeneric("extract_ice", function(object,
                                   data,
                                   cl=NULL,
                                   features=NULL,
                                   n_sample_points=50L,
                                   ensemble_method=waiver(),
                                   eval_times=waiver(),
                                   sample_limit=waiver(),
                                   detail_level=waiver(),
                                   estimation_type=waiver(),
                                   aggregate_results=waiver(),
                                   confidence_level=waiver(),
                                   bootstrap_ci_method=waiver(),
                                   is_pre_processed=FALSE,
                                   message_indent=0L,
                                   verbose=FALSE,
                                   ...) standardGeneric("extract_ice"))

#####extract_ice#####
setMethod("extract_ice", signature(object="familiarEnsemble"),
          function(object,
                   data,
                   cl=NULL,
                   features=NULL,
                   n_sample_points=50L,
                   ensemble_method=waiver(),
                   eval_times=waiver(),
                   sample_limit=waiver(),
                   detail_level=waiver(),
                   estimation_type=waiver(),
                   aggregate_results=waiver(),
                   confidence_level=waiver(),
                   bootstrap_ci_method=waiver(),
                   is_pre_processed=FALSE,
                   message_indent=0L,
                   verbose=FALSE,
                   ...){
            
            # Message extraction start
            if(verbose & is.null(features)){
              logger.message(paste0("Computing individual conditional expectation 
                                    and partial dependence data for features in the dataset."),
                             indent=message_indent)
              
            } else if(verbose){
              logger.message(paste0("Computing individual conditional expectation 
                                    and partial dependence data for the selected features."),
                             indent=message_indent)
            }
            
            # Load eval_times from the object settings attribute, if it is not provided.
            if(is.waive(eval_times)) eval_times <- object@settings$eval_times
            
            # Check eval_times argument
            if(object@outcome_type %in% c("survival")){
              sapply(eval_times, .check_number_in_valid_range, var_name="eval_times", range=c(0.0, Inf), closed=c(FALSE, TRUE))
            }
            
            # Check n_sample_points argument
            .check_number_in_valid_range(x=n_sample_points, var_name="n_sample_points",
                                         range=c(1, Inf))
            
            # Obtain ensemble method from stored settings, if required.
            if(is.waive(ensemble_method)) ensemble_method <- object@settings$ensemble_method
            
            # Check ensemble_method argument
            .check_parameter_value_is_valid(x=ensemble_method, var_name="ensemble_method",
                                            values=.get_available_ensemble_prediction_methods())
            
            # Load confidence alpha from object settings attribute if not
            # provided externally.
            if(is.waive(confidence_level)) confidence_level <- object@settings$confidence_level
            
            # Check confidence_level input argument
            .check_number_in_valid_range(x=confidence_level, var_name="confidence_level",
                                         range=c(0.0, 1.0), closed=c(FALSE, FALSE))
            
            # Load the bootstrap method
            if(is.waive(bootstrap_ci_method)) bootstrap_ci_method <- object@settings$bootstrap_ci_method
            
            .check_parameter_value_is_valid(x=bootstrap_ci_method, var_name="bootstrap_ci_method",
                                            values=.get_available_bootstrap_confidence_interval_methods())
            
            # Check the sample limit.
            sample_limit <- .parse_sample_limit(x = sample_limit,
                                                default = Inf,
                                                data_element = "ice_data")
            
            # Check the level detail.
            detail_level <- .parse_detail_level(x = detail_level,
                                                default = "hybrid",
                                                data_element = "ice_data")
            
            # Check the estimation type.
            estimation_type <- .parse_estimation_type(x = estimation_type,
                                                      default = "bootstrap_confidence_interval",
                                                      data_element = "ice_data",
                                                      detail_level = detail_level,
                                                      has_internal_bootstrap = FALSE)
            
            # Check whether results should be aggregated.
            aggregate_results <- .parse_aggregate_results(x = aggregate_results,
                                                          default = TRUE,
                                                          data_element = "ice_data")
            
            # Test if models are properly loaded
            if(!is_model_loaded(object=object)) ..error_ensemble_models_not_loaded()
            
            # Test if any model in the ensemble was successfully trained.
            if(!model_is_trained(object=object)) return(NULL)
            
            # Generate a prototype data element.
            proto_data_element <- new("familiarDataElementIndividualConditionalExpectation",
                                      detail_level = detail_level,
                                      estimation_type = estimation_type,
                                      confidence_level = confidence_level,
                                      bootstrap_ci_method = bootstrap_ci_method)
            
            # Generate elements to send to dispatch.
            ice_data <- extract_dispatcher(FUN=.extract_ice,
                                           has_internal_bootstrap=FALSE,
                                           cl=cl,
                                           object=object,
                                           data=data,
                                           features=features,
                                           sample_limit=sample_limit,
                                           n_sample_points=n_sample_points,
                                           proto_data_element=proto_data_element,
                                           is_pre_processed=is_pre_processed,
                                           ensemble_method=ensemble_method,
                                           eval_times=eval_times,
                                           aggregate_results=TRUE,
                                           message_indent=message_indent + 1L,
                                           verbose=verbose)
            
            return(ice_data)
          })



.extract_ice <- function(object,
                         data,
                         proto_data_element,
                         eval_times=NULL,
                         features=NULL,
                         sample_limit,
                         aggregate_results,
                         is_pre_processed=FALSE,
                         cl,
                         verbose=FALSE,
                         ...){
  
  # Ensure that the object is loaded
  object <- load_familiar_object(object)
  
  # Add model name.
  proto_data_element <- add_model_name(proto_data_element, object=object)
  
  # Retrieve input data.
  data <- process_input_data(object=object,
                             data=data,
                             stop_at="signature",
                             is_pre_processed=is_pre_processed)
  
  # Maintain only important features. The current set is based on the
  # required features.
  data <- filter_features(data=data,
                          available_features=object@model_features)
  
  # Check if the input data is not empty
  if(is_empty(data)) return(NULL)
  
  # Select samples up to sample_limit.
  data <- get_subsample(data=data,
                        size=sample_limit,
                        seed=0L)
  
  # Aggregate data.
  data <- aggregate_data(data)
  
  # Set class levels.
  if(object@outcome_type %in% c("binomial", "multinomial")){
    proto_data_element@class_levels <- get_outcome_class_levels(object)
  }
  
  # Add evaluation time as a identifier to the data element.
  if(length(eval_times) > 0 & object@outcome_type == "survival"){
    data_elements <- add_data_element_identifier(x=proto_data_element, evaluation_time=eval_times)
    
  } else {
    data_elements <- list(proto_data_element)
  }
  
  if(!is.null(features)){
    # Check that the features exist in the data set.
    if(!all(features %in% c(object@model_features))){
      warning(paste0("Data for individual conditional expectation or partial dependence plots could not be computed for ",
                     paste_s(setdiff(features, object@model_features)),
                     " feature(s) as they are not used by the model."))
    }
    
    # Add features as identifier.
    if(length(features) == 1){
      data_elements <- add_data_element_identifier(x=data_elements, feature_x=features)
      
    } else if(length(features) == 2){
      
      if(length(unique(features)) != 2){
        stop(paste0("Data for individual conditional expectation or partial dependence plots ",
                    "could not be computed as the provided features are not unique: ",
                    paste_s(features), "."))
      }
      
      data_elements <- add_data_element_identifier(x=data_elements, feature_x=features[1])
      data_elements <- add_data_element_identifier(x=data_elements, feature_y=features[2])
      
    } else {
      stop(paste0("Data for individual conditional expectation or partial dependence plots cannot ",
                  "be computed for more than 2 features simultaneously. Found: ",
                  paste_s(features), "."))
    }
    
  } else {
    # Add features as identifier.
    data_elements <- add_data_element_identifier(x=data_elements,
                                                 feature_x=object@model_features)
  }
  
  # Iterate over elements.
  data_elements <- fam_mapply(cl=cl,
                              assign=NULL,
                              FUN=..extract_ice_data,
                              data_element=data_elements,
                              MoreArgs=c(list("data"=data,
                                              "object"=object,
                                              "verbose"=verbose),
                                         list(...)),
                              progress_bar=verbose,
                              chopchop=TRUE)
  
  # Flatten list of data elements.
  data_elements <- unlist(data_elements)
  if(!is.list(data_elements)) data_elements <- list(data_elements)
  
  # Merge data elements
  data_elements <- merge_data_elements(data_elements)
  
  if(aggregate_results) data_elements <- .compute_data_element_estimates(x=data_elements)
  
  return(data_elements)
}



..extract_ice_data <- function(data_element,
                               data,
                               object,
                               aggregate_results,
                               n_sample_points,
                               ensemble_method,
                               verbose=FALSE,
                               message_indent,
                               ...){
  # Divide feature(s) into points.
  
  # Generate range
  feature_x_range <- .create_feature_range(feature_info=object@feature_info,
                                           feature=data_element@identifiers$feature_x,
                                           column_type=class(data@data[[data_element@identifiers$feature_x]]),
                                           n=n_sample_points)
  
  # Add feature values.
  data_elements <- add_data_element_identifier(x=data_element,
                                               feature_x_value=feature_x_range)
  
  if(!is.null(data_element@identifiers$feature_y)){
    feature_y_range <- .create_feature_range(feature_info=object@feature_info,
                                             feature=data_element@identifiers$feature_y,
                                             column_type=class(data@data[[data_element@identifiers$feature_y]]),
                                             n=n_sample_points)
    
    # Add feature values.
    data_elements <- add_data_element_identifier(x=data_elements,
                                                 feature_y_value=feature_y_range)
  }
  
  # Iterate over elements.
  data_elements <- lapply(data_elements,
                          ...extract_ice_data,
                          data=data,
                          object=object,
                          ensemble_method=ensemble_method)
  
  return(data_elements)
}



...extract_ice_data <- function(data_element,
                                data,
                                object,
                                ensemble_method){
  
  # Make a local copy of the data.
  data@data <- data.table::copy(data@data)
  
  # Replace the feature indicated by feature_x by the value in feature_x_value.
  data@data[, (data_element@identifiers$feature_x):=data_element@identifiers$feature_x_value]
  
  # Replace the feature indicated by feature_y by the value in feature_y_value.
  if(!is.null(data_element@identifiers$feature_y)){
    data@data[, (data_element@identifiers$feature_y):=data_element@identifiers$feature_y_value]
  }
  
  # Predict both novelty
  if(object@outcome_type %in% c("survival", "competing_risk")){
    type <- c("survival_probability", "novelty")
    
  } else {
    type <- c("default", "novelty")
  }
  
  # Compute performance data.
  prediction_data <- .predict(object=object,
                              data=data,
                              ensemble_method=ensemble_method,
                              time=data_element@identifiers$evaluation_time,
                              type=type,
                              aggregate_results=TRUE)
  
  # Check that valid prediction data were generated.
  if(!any_predictions_valid(prediction_data, outcome_type=object@outcome_type)) return(NULL)
  
  # Select prediction columns
  if(object@outcome_type %in% c("survival", "competing_risk")){
    prediction_columns <- c("survival_probability", "novelty")
    
  } else if(object@outcome_type %in% c("binomial", "multinomial")){
    prediction_columns <- c(get_class_probability_name(object),
                            "novelty")
    
  } else if(object@outcome_type %in% c("count", "continuous")){
    prediction_columns <- c("predicted_outcome", "novelty")
    
  } else {
    ..error_no_known_outcome_type(object@outcome_type)
  }
  
  # Select only the prediction columns.
  ice_data <- prediction_data[, mget(prediction_columns)]
  
  # Create unique row names for samples and insert.
  ice_data[, "sample":=get_unique_row_names(x=data)]

  # Generate partial dependence data by computing the average over the ICE data.
  pd_data <- ice_data[, lapply(.SD, mean, na.rm=TRUE), .SDcols=prediction_columns]
  
  # Create ice and pd data elements.
  ice_data_element <- data_element
  pd_data_element <- methods::new("familiarDataElementPartialDependence", data_element)
  
  # Update ice data element.
  ice_data_element@grouping_column <- "sample"
  ice_data_element@data <- ice_data
  ice_data_element@value_column <- prediction_columns
  
  # Update pd data element.
  pd_data_element@data <- pd_data
  pd_data_element@value_column <- prediction_columns
  
  return(list(ice_data_element, pd_data_element))
}



.create_feature_range <- function(feature_info, feature, n, column_type){
  
  # Find the feature information associated with the feature.
  feature_info <- feature_info[[feature]]
  
  # Check that the feature info is present.
  if(is.null(feature_info)){
    stop(paste0("Feature information could not be found for the ", feature, " feature."))
  }
  
  # Determine if the feature is categorical or numerical.
  if(feature_info@feature_type == "factor"){
    # Get the levels.
    feature_range <- feature_info@levels
    feature_range <- factor(feature_range, levels=feature_range)
    
  } else if(feature_info@feature_type == "numeric"){
    
    # Create the range of values.
    if(n == 1){
      feature_range <- as.numeric(feature_info@distribution$fivenum)[3]
      
    } else {
      # Sample the five-number summary.
      feature_range <- stats::spline(x=c(0.00, 0.25, 0.50, 0.75, 1.00),
                                     y=as.numeric(feature_info@distribution$fivenum),
                                     n=n,
                                     method="hyman")$y
      
      # Convert to integer if required.
      if(any(column_type == "integer")) feature_range <- as.integer(feature_range)
      
      # Select unique values.
      feature_range <- unique(feature_range)
    }
    
  } else {
    ..error_reached_unreachable_code(paste0(".create_feature_range: encountered unknown feature type (",
                                            feature_info@feature_type,
                                            ") for the ",
                                            feature,
                                            " feature."))
  }
  
  return(feature_range)
}



#####export_ice_data#####

#'@title Extract and export individual conditional expectation data.
#'
#'@description Extract and export individual conditional expectation data.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams extract_ice
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
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_ice_data
#'@md
#'@rdname export_ice_data-methods
setGeneric("export_ice_data", function(object, dir_path=NULL, aggregate_results=TRUE, ...) standardGeneric("export_ice_data"))

#####export_ice_data (collection)#####

#'@rdname export_ice_data-methods
setMethod("export_ice_data", signature(object="familiarCollection"),
          function(object, dir_path=NULL, aggregate_results=TRUE, ...){
            
            # Obtain individual conditional expectation plots.
            ice_data <- .export(x=object,
                                data_slot="ice_data",
                                dir_path=dir_path,
                                aggregate_results=aggregate_results,
                                type="explanation",
                                subtype="ice",
                                object_class="familiarDataElementIndividualConditionalExpectation")
            
            if(is.null(dir_path)){
              return(ice_data)
              
            } else {
              return(NULL)
            }
          })

#####export_ice_data (generic)#####

#'@rdname export_ice_data-methods
setMethod("export_ice_data", signature(object="ANY"),
          function(object, dir_path=NULL, aggregate_results=TRUE, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object,
                                          "data_element"="ice_data",
                                          "aggregate_results"=aggregate_results),
                                     list(...)))
            
            return(do.call(export_ice_data,
                           args=c(list("object"=object,
                                       "dir_path"=dir_path,
                                       "aggregate_results"=aggregate_results),
                                  list(...))))
          })


#####.export (familiarDataElementIndividualConditionalExpectation)--------------
setMethod(".export", signature(x="familiarDataElementIndividualConditionalExpectation"),
          function(x, x_list, aggregate_results=FALSE, ...){
            
            if(aggregate_results){
              x_list <- .compute_data_element_estimates(x_list)
            }
            
            # Determine identifiers that should be merged. Since the feature
            # values of the x and y features may be different (e.g. numeric and
            # factor), merging them would cause features values to merged
            # incorrectly.
            merging_identifiers <- setdiff(names(x@identifiers), c("feature_x", "feature_y"))
            
            # Merge data elements.
            x <- merge_data_elements(x=x_list,
                                     as_data=merging_identifiers,
                                     as_grouping_column=TRUE,
                                     force_data_table=TRUE)
            
            return(x)
          })


#####export_partial_dependence_data#####

#'@title Extract and export partial dependence data.
#'
#'@description Extract and export partial dependence data.
#'
#'@inheritParams export_all
#'
#'@inheritDotParams extract_ice
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
#'@return A list of data.tables (if `dir_path` is not provided), or nothing, as
#'  all data is exported to `csv` files.
#'@exportMethod export_partial_dependence_data
#'@md
#'@rdname export_partial_dependence_data-methods
setGeneric("export_partial_dependence_data", function(object, dir_path=NULL, aggregate_results=TRUE, ...) standardGeneric("export_partial_dependence_data"))

#####export_partial_dependence_data (collection)#####

#'@rdname export_partial_dependence_data-methods
setMethod("export_partial_dependence_data", signature(object="familiarCollection"),
          function(object, dir_path=NULL, aggregate_results=TRUE, ...){
            
            # Obtain partial dependence
            pd_data <- .export(x=object,
                               data_slot="ice_data",
                               dir_path=dir_path,
                               aggregate_results=aggregate_results,
                               type="explanation",
                               subtype="pd",
                               object_class="familiarDataElementPartialDependence")
            
            if(is.null(dir_path)){
              return(pd_data)
              
            } else {
              return(NULL)
            }
          })

#####export_partial_dependence_data (generic)#####

#'@rdname export_partial_dependence_data-methods
setMethod("export_partial_dependence_data", signature(object="ANY"),
          function(object, dir_path=NULL, aggregate_results=TRUE, ...){
            
            # Attempt conversion to familiarCollection object.
            object <- do.call(as_familiar_collection,
                              args=c(list("object"=object,
                                          "data_element"="ice_data",
                                          "aggregate_results"=aggregate_results),
                                     list(...)))
            
            return(do.call(export_partial_dependence_data,
                           args=c(list("object"=object,
                                       "dir_path"=dir_path,
                                       "aggregate_results"=aggregate_results),
                                  list(...))))
          })


#####.export (familiarDataElementIndividualConditionalExpectation)--------------
setMethod(".export", signature(x="familiarDataElementPartialDependence"),
          function(x, x_list, aggregate_results=FALSE, ...){
            
            if(aggregate_results){
              x_list <- .compute_data_element_estimates(x_list)
            }
            
            # Determine identifiers that should be merged. Since the feature
            # values of the x and y features may be different (e.g. numeric and
            # factor), merging them would cause features values to merged
            # incorrectly.
            merging_identifiers <- setdiff(names(x@identifiers), c("feature_x", "feature_y"))
            
            # Merge data elements.
            x <- merge_data_elements(x=x_list,
                                     as_data=merging_identifiers,
                                     as_grouping_column=TRUE,
                                     force_data_table=TRUE)
            
            return(x)
          })

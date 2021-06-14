#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarDataElementIndividualConditionalExpectation",
         contains="familiarDataElement",
         prototype = methods::prototype(grouping_column=c("sample")))

setClass("familiarDataElementPartialDependence",
         contains="familiarDataElement",
         prototype = methods::prototype())


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
              logger.message(paste0("Computing individual conditional expectation for features in the dataset."),
                             indent=message_indent)
            }
            
            # Load eval_times from the object settings attribute, if it is not provided.
            if(is.waive(eval_times)) eval_times <- object@settings$eval_times
            
            # Check eval_times argument
            if(object@outcome_type %in% c("survival")){
              sapply(eval_times, .check_number_in_valid_range, var_name="eval_times", range=c(0.0, Inf), closed=c(FALSE, TRUE))
            }
            
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
  
  # Check if the input data is not empty
  if(is_empty(data)) return(NULL)
  
  # Select samples up to sample_limit.
  data <- get_subsample(data=data,
                        size=sample_limit,
                        seed=0L)
  
  # Aggregate data.
  data <- aggregate_data(data)
  
  # Add evaluation time as a identifier to the data element.
  if(length(eval_times) > 0 & object@outcome_type == "survival"){
    data_elements <- add_data_element_identifier(x=proto_data_element, evaluation_time=eval_times)
    
  } else {
    data_elements <- list(proto_data_element)
  }
  
  if(!is.null(features)){
    # Check that the features exist in the data set.
    if(!all(features %in% c(object@model_features))){
      warning(paste0("Individual conditional expectation or partial dependence plots could not be created for ",
                     paste_s(setdiff(features, object@model_features)),
                     " feature(s) as they are not used by the model."))
    }
    
    # Add features as identifier.
    if(length(features) == 1){
      data_elements <- add_data_element_identifier(x=data_elements, feature_x=features)
      
    } else if(length(features) == 2){
      data_elements <- add_data_element_identifier(x=data_elements, feature_x=features[1])
      data_elements <- add_data_element_identifier(x=data_elements, feature_y=features[2])
      
    } else {
      stop(paste0("Individual conditional expectation or partial dependence plots cannot be created for more than 2 features simultaneously. Found: ",
                  paste_s(features)))
    }
    
  } else {
    # Add features as identifier.
    data_elements <- add_data_element_identifier(x=data_elements, feature_x=features)
  }
  browser()
  # Iterate over elements.
  ice_data <- fam_mapply(cl=cl,
                         assign=NULL,
                         FUN=..extract_ice_data,
                         data_element=data_elements,
                         MoreArgs=c(list("data"=data,
                                         "object"=object,
                                         "verbose"=verbose),
                                    list(...)),
                         progress_bar=verbose,
                         chopchop=TRUE)
  
  return(ice_data)
}



..extract_ice_data <- function(data_element,
                               data,
                               object,
                               aggregate_results,
                               n_sample_points,
                               verbose=FALSE,
                               message_indent){
  
  browser()
  # Divide feature(s) into points.
  if(data_element@identifiers$feature_x){
    # Generate range
    feature_x_range <- .create_feature_range(feature_info=object@feature_info,
                                             feature=data_element@identifiers$feature_x,
                                             n=n_sample_points)
    
    # Add feature values.
    data_elements <- add_data_element_identifier(x=data_element,
                                                 feature_x_value=feature_x_range)
  }
    
  if(!is.null(data_element@identifiers$feature_y)){
    feature_y_range <- .create_feature_range(feature_info=object@feature_info,
                                             feature=data_element@identifiers$feature_y,
                                             n=n_sample_points)
    
    # Add feature values.
    data_elements <- add_data_element_identifier(x=data_elements,
                                                 feature_y_value=feature_y_range)
  }
  
  # Iterate over elements.
  data_elements <- lapply(data_elements,
                          ...extract_ice_data,
                          data=data,
                          object=object)
  
  # Generate partial dependence data.
  
  
  return(list(ice_data,
              pd_data))
}



.create_feature_range <- function(feature_info, feature, n){
  browser()
  # Find the feature information associated with the feature.
  feature_info <- feature_info[[feature]]
  
  # Check that the feature info is present.
  if(is.null(feature_info)){
    stop(paste0("Feature information could not be found for the ", feature, " feature."))
  }
  
  # Determine if the feature is categorical or numerical.
  if(feature_info@feature_type == "factor"){
    feature_range <- feature_info@levels
    feature_range <- factor(feature_range, levels=feature_range)
    
  } else if(feature_info@feature_type == "numeric"){
    
    
  } else {
    ..error_reached_unreachable_code(paste0(".create_feature_range: encountered unknown feature type (",
                                            feature_info@feature_type,
                                            ") for the ",
                                            feature,
                                            " feature."))
  }
  
}

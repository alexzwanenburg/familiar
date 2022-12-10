#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("featureInfoParametersNormalisationNone",
         contains="featureInfoParameters",
         slots=list("method" = "character",
                    "batch" = "ANY",
                    "reason" = "ANY"),
         prototype=list("method" = "none",
                        "batch" = NULL,
                        "reason" = NULL))

setClass("featureInfoParametersNormalisationShiftScale",
         contains="featureInfoParameters",
         slots=list("batch" = "ANY",
                    "shift" = "numeric",
                    "scale" = "numeric"),
         prototype=list("batch" = NULL,
                        "shift" = NA_real_,
                        "scale" = NA_real_))

setClass("featureInfoParametersNormalisationShift",
         contains="featureInfoParameters",
         slots=list("batch" = "ANY",
                    "shift" = "numeric"),
         prototype=list("batch" = NULL,
                        "shift" = NA_real_))

setClass("featureInfoParametersNormalisationStandardisation",
         contains="featureInfoParametersNormalisationShiftScale",
         slots=list("method" = "character"),
         prototype=list("method"=NA_character_))

setClass("featureInfoParametersNormalisationQuantile",
         contains="featureInfoParametersNormalisationShiftScale",
         slots=list("method" = "character"),
         prototype=list("method"=NA_character_))

setClass("featureInfoParametersNormalisationNormalisation",
         contains="featureInfoParametersNormalisationShiftScale",
         slots=list("method" = "character"),
         prototype=list("method"=NA_character_))

setClass("featureInfoParametersNormalisationMeanCentering",
         contains="featureInfoParametersNormalisationShift",
         slots=list("method" = "character"),
         prototype=list("method"=NA_character_))



.get_available_none_normalisation_methods <- function(){
  return("none")
}

.get_available_standardisation_normalisation_methods <- function(){
  return(c("standardisation", "standardisation_trim", "standardisation_winsor", "standardisation_robust"))
}

.get_available_quantile_normalisation_methods <- function(){
  return(c("quantile"))
}

.get_available_normalisation_normalisation_methods <- function(){
  return(c("normalisation", "normalisation_trim", "normalisation_winsor"))
}

.get_available_mean_centering_normalisation_methods <- function(){
  return(c("mean_centering"))
}

.get_available_normalisation_methods <- function(type="all"){
  
  available_normalisation_method <- NULL
  
  if(type %in% c("none", "all")){
    available_normalisation_method <- c(available_normalisation_method,
                                        .get_available_none_normalisation_methods())
  }
  
  if(type %in% c("all", "standardisation")){
    available_normalisation_method <- c(available_normalisation_method,
                                        .get_available_standardisation_normalisation_methods())
  }
  
  if(type %in% c("all", "quantile")){
    available_normalisation_method <- c(available_normalisation_method,
                                        .get_available_quantile_normalisation_methods())
  }
  
  if(type %in% c("all", "mean_centering")){
    available_normalisation_method <- c(available_normalisation_method,
                                        .get_available_mean_centering_normalisation_methods())
  }
  
  if(type %in% c("all", "normalisation")){
    available_normalisation_method <- c(available_normalisation_method,
                                        .get_available_normalisation_normalisation_methods())
  }
  
  return(available_normalisation_method)
}



create_normalisation_parameter_skeleton <- function(feature_info_list,
                                                    feature_names=NULL,
                                                    batch=NULL,
                                                    normalisation_method,
                                                    normalisation_shift=NULL,
                                                    normalisation_scale=NULL,
                                                    .override_existing=FALSE){
  
  # Creates a skeleton for the provided normalisation method. If
  # normalisation_shift and / or normalisation_scale are provided (typically
  # not), these values are updated as well.
  
  # Determine feature names from the feature info list, if provided.
  if(is.null(feature_names)) feature_names <- names(feature_info_list)
  
  # Select only features that appear in the feature info list.
  feature_names <- intersect(names(feature_info_list),
                             feature_names)
  
  # Skip step if no feature info objects are updated.
  if(is_empty(feature_names)) return(feature_info_list)
  
  # Check that method is applicable.
  .check_parameter_value_is_valid(x=normalisation_method,
                                  var_name="normalisation_method",
                                  values=.get_available_normalisation_methods())
  
  # Check that normalisation_shift is numeric.
  if(!is.null(normalisation_shift)){
    .check_number_in_valid_range(x=normalisation_shift,
                                 var_name="normalisation_shift",
                                 range=c(-Inf, Inf))
  }
  
  # Check that normalisation_scale is numeric.
  if(!is.null(normalisation_scale)){
    .check_number_in_valid_range(x=normalisation_scale,
                                 var_name="normalisation_scale",
                                 range=c(-Inf, Inf))
  }
  
  # Update familiar info objects with a feature normalisation skeleton.
  updated_feature_info <- fam_lapply(X=feature_info_list[feature_names],
                                     FUN=.create_normalisation_parameter_skeleton,
                                     method=normalisation_method,
                                     batch=batch,
                                     shift=normalisation_shift,
                                     scale=normalisation_scale,
                                     .override_existing=.override_existing)
  
  # Provide names for the updated feature info objects.
  names(updated_feature_info) <- feature_names
  
  # Replace list elements.
  feature_info_list[feature_names] <- updated_feature_info
  
  return(feature_info_list)
}



.create_normalisation_parameter_skeleton <- function(feature_info,
                                                     method,
                                                     batch=NULL,
                                                     shift=NULL,
                                                     scale=NULL,
                                                     .override_existing=FALSE){
  
  # Check if normalisation data was already completed, and does not require
  # being determined anew.
  if(feature_info_complete(feature_info@normalisation_parameters) & !.override_existing) return(feature_info)
  
  # Pass to underlying function that constructs the skeleton.
  object <- ..create_normalisation_parameter_skeleton(feature_name=feature_info@name,
                                                      feature_type=feature_info@feature_type,
                                                      available=is_available(feature_info),
                                                      method=method,
                                                      batch=batch,
                                                      shift=shift,
                                                      scale=scale)
  
  # Update normalisation_parameters slot.
  feature_info@normalisation_parameters <- object
  
  return(feature_info)
}



..create_normalisation_parameter_skeleton <- function(feature_name,
                                                      feature_type="numeric",
                                                      available=TRUE,
                                                      method,
                                                      batch=NULL,
                                                      shift=NULL,
                                                      scale=NULL){
  # This is the lowest level function for creating normalisation parameter
  # skeletons.
  
  # Create the relevant objects.
  if(feature_type != "numeric"){
    object <- methods::new("featureInfoParametersNormalisationNone",
                           reason="not a numeric feature")
    
  } else if(!available){
    object <- methods::new("featureInfoParametersNormalisationNone",
                           reason="feature was omitted prior to transformation")
    
  } else if(method %in% .get_available_none_normalisation_methods()){
    object <- methods::new("featureInfoParametersNormalisationNone")
    
  } else if(method %in% .get_available_standardisation_normalisation_methods()){
    object <- methods::new("featureInfoParametersNormalisationStandardisation",
                           "method"=method)
    
  } else if(method %in% .get_available_quantile_normalisation_methods()){
    object <- methods::new("featureInfoParametersNormalisationQuantile",
                           "method"=method)
  
  } else if(method %in% .get_available_normalisation_normalisation_methods()){
    object <- methods::new("featureInfoParametersNormalisationNormalisation",
                           "method"=method)
    
  } else if(method %in% .get_available_mean_centering_normalisation_methods()){
    object <- methods::new("featureInfoParametersNormalisationMeanCentering",
                           "method"=method)
  
  } else if(method %in% .get_available_combat_parametric_normalisation_methods()){
    object <- methods::new("featureInfoParametersNormalisationParametricCombat",
                           "method"=method)
    
  } else if(method %in% .get_available_combat_non_parametric_normalisation_methods()){
    object <- methods::new("featureInfoParametersNormalisationNonParametricCombat",
                           "method"=method)
    
  } else {
    ..error_reached_unreachable_code(paste0("..create_normalisation_parameter_skeleton: encountered an unknown normalisation method: ", paste_s(method)))
  }
  
  # Set the name of the object.
  object@name <- feature_name
  
  # Add batch, if provided.
  if(!is.null(batch)){
    object@batch <- batch
  }
  
  # Add shift and/or scale, when provided.
  if(!is.null(shift) || !is.null(scale)){
    object <- add_feature_info_parameters(object,
                                          data=NULL,
                                          batch=batch,
                                          shift=shift,
                                          scale=scale)
  }
  
  # Update the familiar version.
  object <- add_package_version(object=object)
  
  return(object)
}



add_normalisation_parameters <- function(cl=NULL,
                                         feature_info_list,
                                         data,
                                         verbose=FALSE){
  # Determine normalisation parameters and add them to the feature_info_list.
  
  # Find feature columns.
  feature_names <- get_feature_columns(x=data)
  
  # Sanity check.
  if(!(setequal(feature_names, get_available_features(feature_info_list=feature_info_list)))){
    ..error_reached_unreachable_code("add_normalisation_parameters: features in data and the feature info list are expect to be the same, but were not.")
  }
  
  # Iterate over features.
  updated_feature_info <- fam_mapply(cl=cl,
                                     FUN=.add_normalisation_parameters,
                                     feature_info=feature_info_list[feature_names],
                                     data=data@data[, mget(feature_names)],
                                     progress_bar=verbose,
                                     chopchop=TRUE)
  
  # Provide names for the updated feature info objects.
  names(updated_feature_info) <- feature_names
  
  # Replace list elements.
  feature_info_list[feature_names] <- updated_feature_info
  
  return(feature_info_list)
}



.add_normalisation_parameters <- function(feature_info,
                                          data){
  
  # Pass to underlying function that adds the feature info.
  object <- add_feature_info_parameters(object=feature_info@normalisation_parameters,
                                        data=data)
  
  # Update normalisation_parameters slot.
  feature_info@normalisation_parameters <- object
  
  return(feature_info)
}



##### initialize (none) --------------------------------------------------------
setMethod("initialize", signature(.Object="featureInfoParametersNormalisationNone"),
          function(.Object, ...){
            
            # Update with parent class first.
            .Object <- callNextMethod()
            
            # The parameter set is by definition complete when no normalisation
            # is performed.
            .Object@complete <- TRUE
            
            return(.Object)
          })



##### add_feature_info_parameters (none, NULL) ---------------------------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersNormalisationNone", data="NULL"),
          function(object,
                   data,
                   batch=NULL,
                   ...){
            if(!is.null(batch)){
              object@batch <- batch
            }
            
            return(object)
          })



##### add_feature_info_parameters (shift normalisation, NULL) ------------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersNormalisationShift", data="NULL"),
          function(object, 
                   data,
                   batch=NULL,
                   shift=NULL,
                   ...) {
            
            if(!is.null(batch)){
              object@batch <- batch
              
            } else if(!is.null(object@batch)){
              # If batch has been set as part of the object, get it here.
              batch <- object@batch
            }

            if(is.numeric(shift)){
              if(is.finite(shift)){
                # Shift is numeric, and not NA. This is typical when shift is set
                # externally. We then update the shift parameter.
                object@shift <- shift
                object@complete <- TRUE
                
                return(object)
                
              } else {
                # Shift is numeric, but NA or Inf. We then return a none-class
                # normalisation instead. This is typical when updating versions
                # prior to familiar v1.2.0.
                object <- ..create_normalisation_parameter_skeleton(feature_name=object@name,
                                                                    batch=batch,
                                                                    method="none")
                
                object@reason <- "shift was NA or infinite"
                
                return(object)
              }
            }
            
            # If shift is not set, but data is NULL, shift cannot be
            # determined.
            if(is.null(shift)){
              object <- ..create_normalisation_parameter_skeleton(feature_name=object@name,
                                                                  batch=batch,
                                                                  method="none")
              
              object@reason <- "insufficient data to determine shift"
              
              return(object)
            }
            
            # If shift is not numeric, it can still be NA. We then return a
            # none-class normalisation instead. This is typical when updating
            # versions prior to familiar v1.2.0.
            if(is.na(shift)){
              object <- ..create_normalisation_parameter_skeleton(feature_name=object@name,
                                                                  batch=batch,
                                                                  method="none")
              
              object@reason <- "shift was NA"
              
              return(object)
            }
            
            # Any other reasons why shift cannot be set directly.
            object <- ..create_normalisation_parameter_skeleton(feature_name=object@name,
                                                                batch=batch,
                                                                method="none")
            
            object@reason <- "shift could not be determined for an unknown reason"
            
            return(object)
          })



##### add_feature_info_parameters (shift normalisation, ANY) ---------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersNormalisationShift", data="ANY"),
          function(object, 
                   data,
                   ...) {
            
            # This method is targeted when directly determining the
            # normalisation parameters. It is usually called from the child
            # functions.
            
            # Check if all required parameters have been set.
            if(feature_info_complete(object)) return(object)
            
            # Check if data is not empty, and return none-class normalisation
            # object, if it is. This is done by calling the method with
            # signature data=NULL to handle setting the data.
            if(is_empty(data)) return(add_feature_info_parameters(object=object, data=NULL))
            
            # Remove non-finite values from data.
            data <- data[is.finite(data)]
            
            # Again, check if data is not empty.
            if(is_empty(data)) return(add_feature_info_parameters(object=object, data=NULL))
            
            # Check that at least three unique values are present.
            if(length(unique(data)) <= 3) return(add_feature_info_parameters(object=object, data=NULL))
            
            # For batch normalisation, check that at least five instances are
            # present.
            if(!is.null(object@batch)){
              if(length(data) < 5) return(add_feature_info_parameters(object=object, data=NULL))
            }
            
            return(object)
          })



##### add_feature_info_parameters (shift and scale normalisation, NULL) --------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersNormalisationShiftScale", data="NULL"),
          function(object, 
                   data,
                   batch=NULL,
                   shift=NULL,
                   scale=NULL,
                   ...) {
            
            if(!is.null(batch)){
              object@batch <- batch
              
            } else if(!is.null(object@batch)){
              # If batch has been set as part of the object, get it here.
              batch <- object@batch
            }
            
            if(is.numeric(shift) && is.numeric(scale)){
              if(is.finite(shift) && is.finite(scale)){
                # Shift and scale are numeric, and not NA. This is typical when
                # shift and scale are set externally. We then update the shift
                # and scale parameters.
                object@shift <- shift
                object@scale <- scale
                object@complete <- TRUE
                
                return(object)
                
              } else {
                # Shift and scale are numeric, but NA or Inf. We then return a
                # none-class normalisation instead. This is typical when
                # updating versions prior to familiar v1.2.0.
                object <- ..create_normalisation_parameter_skeleton(feature_name=object@name,
                                                                    batch=batch,
                                                                    method="none")
                
                object@reason <- "shift or scale were NA or infinite"
                
                return(object)
              }
            }
            
            # If shift or scale are not set, but data is NULL, so shift and
            # scale cannot be determined.
            if(is.null(shift) || is.null(scale)){
              object <- ..create_normalisation_parameter_skeleton(feature_name=object@name,
                                                                  batch=batch,
                                                                  method="none")
              
              object@reason <- "insufficient data to determine shift and scale"
              
              return(object)
            }
            
            # If shift and scale are not numeric, they can still be NA. We then
            # return a none-class normalisation instead. This is typical when
            # updating versions prior to familiar v1.2.0.
            if(is.na(shift) || is.na(scale)){
              object <- ..create_normalisation_parameter_skeleton(feature_name=object@name,
                                                                  batch=batch,
                                                                  method="none")
              
              object@reason <- "shift or scale were NA"
              
              return(object)
            }
            
            # Any other reasons why shift and scale cannot be set directly.
            object <- ..create_normalisation_parameter_skeleton(feature_name=object@name,
                                                                batch=batch,
                                                                method="none")
            
            object@reason <- "shift and scale could not be determined for an unknown reason"
            
            return(object)
          })



##### add_feature_info_parameters (shift and scale normalisation, ANY) ---------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersNormalisationShiftScale", data="ANY"),
          function(object, 
                   data,
                   ...) {
            
            # This method is targeted when directly determining the
            # normalisation parameters. It is usually called from the child
            # functions.
            
            # Check if all required parameters have been set.
            if(feature_info_complete(object)) return(object)
            
            # Check if data is not empty, and return none-class normalisation
            # object, if it is. This is done by calling the method with
            # signature data=NULL to handle setting the data.
            if(is_empty(data)) return(add_feature_info_parameters(object=object, data=NULL))
            
            # Remove non-finite values from data.
            data <- data[is.finite(data)]
            
            # Again, check if data is not empty.
            if(is_empty(data)) return(add_feature_info_parameters(object=object, data=NULL))
            
            # Check that at least three unique values are present.
            if(length(unique(data)) <= 3) return(add_feature_info_parameters(object=object, data=NULL))
            
            # For batch normalisation, check that at least five instances are
            # present.
            if(!is.null(object@batch)){
              if(length(data) < 5) return(add_feature_info_parameters(object=object, data=NULL))
            }            
            
            return(object)
          })



##### add_feature_info_parameters (standardisation, data.table) ----------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersNormalisationStandardisation", data="data.table"),
          function(object, 
                   data,
                   ...){
            
            # Pass to non-data.table method.
            return(add_feature_info_parameters(object=object,
                                               data=data[[object@name]],
                                               ...))
          })



##### add_feature_info_parameters (standardisation, ANY) -----------------------
setMethod(
  "add_feature_info_parameters",
  signature(
    object="featureInfoParametersNormalisationStandardisation",
    data="ANY"),
  function(
    object, 
    data,
    ...)
  {
    # Check if all required parameters have been set.
    if(feature_info_complete(object)) return(object)
    
    # Run general checks for shift and scale transforms. This may yield
    # none-transforms which are complete by default.
    object <- callNextMethod()
    
    # Check if all required parameters have been set now.
    if(feature_info_complete(object)) return(object)
    
    # Remove any non-finite values.
    data <- data[is.finite(data)]
    
    # Check if data is not empty after removing non-finite data and
    # return none-class normalisation object, if it is. This is done by
    # calling the method with signature data=NULL to handle setting the
    # data.
    if(is_empty(data)){
      object <- add_feature_info_parameters(object=object, data=NULL)
      object@reason <- "no finite values left for normalisation"
      
      return(object)
    }
    
    # Trimming and winsoring of input data.
    if(object@method %in% c("standardisation_trim")){
      data <- trim(data, fraction=0.05)
      
    } else if(object@method %in% c("standardisation_winsor")){
      data <- winsor(data, fraction=0.05)
    }
    
    # Determine mean (shift) and standard deviation (scale)
    if(object@method %in% c("standardisation_robust")){
      # Using Huber's M-estimators for location and scale.
      robust_estimates <- huber_estimate(data)
      
      shift <- robust_estimates$mu
      scale <- robust_estimates$sigma
      
    } else {
      # Using conventional estimators.
      shift <- mean(data)
      scale <- sqrt(sum((data-shift)^2) / length(data))
    }
    
    # Check for scales which are close to 0, or could not be computed.
    if(!is.finite(scale)) scale <- 1.0
    if(scale < 2.0 * .Machine$double.eps) scale <- 1.0
    
    # Set shift and scale parameters.
    object@shift <- shift
    object@scale <- scale
    object@complete <- TRUE
    
    return(object)
  }
)



##### add_feature_info_parameters (quantile, data.table) ----------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersNormalisationQuantile", data="data.table"),
          function(object, 
                   data,
                   ...){
            
            # Pass to non-data.table method.
            return(add_feature_info_parameters(object=object,
                                               data=data[[object@name]],
                                               ...))
          })



##### add_feature_info_parameters (quantile, ANY) ------------------------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersNormalisationQuantile", data="ANY"),
          function(object, 
                   data,
                   ...) {
            
            # Check if all required parameters have been set.
            if(feature_info_complete(object)) return(object)
            
            # Run general checks for shift and scale transforms. This may yield
            # none-transforms which are complete by default.
            object <- callNextMethod()
            
            # Check if all required parameters have been set now.
            if(feature_info_complete(object)) return(object)
            
            # Remove any non-finite values.
            data <- data[is.finite(data)]
            
            # Check if data is not empty after removing non-finite data and
            # return none-class normalisation object, if it is. This is done by
            # calling the method with signature data=NULL to handle setting the
            # data.
            if(is_empty(data)){
              object <- add_feature_info_parameters(object=object, data=NULL)
              object@reason <- "no finite values left for normalisation"
              
              return(object)
            }
            
            # Determine median (shift) and interquartile range (scale)
            shift <- stats::median(data)
            scale <- unname(diff(stats::quantile(data, probs=c(0.25,0.75))))
            
            # Check for scales which are close to 0
            if(scale < 2.0 * .Machine$double.eps) scale <- 1.0
            
            # Set shift and scale parameters.
            object@shift <- shift
            object@scale <- scale
            object@complete <- TRUE
            
            return(object)
          })



##### add_feature_info_parameters (normalisation, data.table) ----------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersNormalisationNormalisation", data="data.table"),
          function(object, 
                   data,
                   ...){
            
            # Pass to non-data.table method.
            return(add_feature_info_parameters(object=object,
                                               data=data[[object@name]],
                                               ...))
          })



##### add_feature_info_parameters (normalisation, ANY) -------------------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersNormalisationNormalisation", data="ANY"),
          function(object, 
                   data,
                   ...) {
            
            # Check if all required parameters have been set.
            if(feature_info_complete(object)) return(object)
            
            # Run general checks for shift and scale transforms. This may yield
            # none-transforms which are complete by default.
            object <- callNextMethod()
            
            # Check if all required parameters have been set now.
            if(feature_info_complete(object)) return(object)
            
            # Remove any non-finite values.
            data <- data[is.finite(data)]
            
            # Check if data is not empty after removing non-finite data and
            # return none-class normalisation object, if it is. This is done by
            # calling the method with signature data=NULL to handle setting the
            # data.
            if(is_empty(data)){
              object <- add_feature_info_parameters(object=object, data=NULL)
              object@reason <- "no finite values left for normalisation"
              
              return(object)
            }
            
            # Trimming and winsoring of input data.
            if(object@method %in% c("normalisation_trim")){
              data <- trim(data, fraction=0.05)
              
            } else if(object@method %in% c("normalisation_winsor")){
              data <- winsor(data, fraction=0.05)
            }
            
            # Determine max and min values.
            x_min <- min(data, na.rm=TRUE)
            x_max <- max(data, na.rm=TRUE)
            
            # Set shift and scale parameters.
            shift <- x_min
            scale <- x_max - x_min
            
            # Check for scales which are close to 0.
            if(scale < 2.0 * .Machine$double.eps) scale <- 1.0
            
            # Set shift and scale parameters.
            object@shift <- shift
            object@scale <- scale
            object@complete <- TRUE
            
            return(object)
          })



##### add_feature_info_parameters (mean centering, data.table) ----------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersNormalisationMeanCentering", data="data.table"),
          function(object, 
                   data,
                   ...){
            
            # Pass to non-data.table method.
            return(add_feature_info_parameters(object=object,
                                               data=data[[object@name]],
                                               ...))
          })



##### add_feature_info_parameters (mean centering, ANY) ------------------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersNormalisationMeanCentering", data="ANY"),
          function(object, 
                   data,
                   ...) {
            
            # Check if all required parameters have been set.
            if(feature_info_complete(object)) return(object)
            
            # Run general checks for shift and scale transforms. This may yield
            # none-transforms which are complete by default.
            object <- callNextMethod()
            
            # Check if all required parameters have been set now.
            if(feature_info_complete(object)) return(object)
            
            # Remove any non-finite values.
            data <- data[is.finite(data)]
            
            # Check if data is not empty after removing non-finite data and
            # return none-class normalisation object, if it is. This is done by
            # calling the method with signature data=NULL to handle setting the
            # data.
            if(is_empty(data)){
              object <- add_feature_info_parameters(object=object, data=NULL)
              object@reason <- "no finite values left for normalisation"
              
              return(object)
            }
            
            # Determine mean as shift.
            shift <- mean(data)

            # Set shift parameter.
            object@shift <- shift
            object@complete <- TRUE
            
            return(object)
          })



##### apply_feature_info_parameters (shift and scale, data.table) --------------
setMethod("apply_feature_info_parameters", signature(object="featureInfoParametersNormalisationShiftScale", data="data.table"),
          function(object, 
                   data,
                   ...){
            
            # Pass to method with signature data="ANY" to perform the
            # normalisation.
            y <- apply_feature_info_parameters(object=object, data[[object@name]], ...)
            
            # Avoid updating by reference.
            data <- data.table::copy(data)
            
            # Update the respective column with the transformed values.
            data[, (object@name):=y]
            
            return(data)
          })



##### apply_feature_info_parameters (shift and scale, ANY) ---------------------
setMethod("apply_feature_info_parameters", signature(object="featureInfoParametersNormalisationShiftScale", data="ANY"),
          function(object, 
                   data,
                   invert=FALSE,
                   ...){
            
            if(invert){
              # Invert normalisation by multiplying input x by the scale and
              # adding the shift.
              return(data * object@scale + object@shift)
              
            } else {
              # Apply normalisation by subtracting the shift parameter and
              # dividing by the scale.
              return((data - object@shift) / (object@scale))
            }
          })



##### apply_feature_info_parameters (shift, data.table) ------------------------
setMethod("apply_feature_info_parameters", signature(object="featureInfoParametersNormalisationShift", data="data.table"),
          function(object, 
                   data,
                   ...){
            
            # Pass to method with signature data="ANY" to perform the
            # normalisation.
            y <- apply_feature_info_parameters(object=object, data[[object@name]], ...)
            
            # Avoid updating by reference.
            data <- data.table::copy(data)
            
            # Update the respective column with the transformed values.
            data[, (object@name):=y]
            
            return(data)
          })



##### apply_feature_info_parameters (shift, ANY) ------------------------------
setMethod("apply_feature_info_parameters", signature(object="featureInfoParametersNormalisationShift", data="ANY"),
          function(object, 
                   data,
                   invert=FALSE,
                   ...){
            
            if(invert){
              # Invert normalisation by adding the shift.
              return(data + object@shift)
              
            } else {
              # Apply normalisation by subtracting the shift parameter.
              return(data - object@shift)
            }
          })



.normalise <- function(x, normalisation_method, range=NULL){
  # Direct normalisation, no questions asked.
  
  # Create a skeleton object.
  object <- ..create_normalisation_parameter_skeleton(feature_name="x",
                                                      feature_type=ifelse(is.numeric(x), "numeric", "factor"),
                                                      available=TRUE,
                                                      method=normalisation_method)
  
  # Obtain normalisation parameters.
  object <- add_feature_info_parameters(object=object,
                                        data=x)
  
  # Apply normalisation parameters.
  y <- apply_feature_info_parameters(object=object,
                                     data=x)
  
  # Output to certain range (if provided).
  if(!is.null(range)){
    y[y < range[1]] <- range[1]
    y[y > range[2]] <- range[2]
  }
  
  return(y)
}



.get_default_normalisation_range_for_plotting <- function(norm_method){
  # Find a default range for normalised values during plotting.
  if(norm_method == "none"){
    return(c(NA, NA))
    
  } else if(norm_method == "mean_centering"){
    return(c(NA, NA))
    
  } else if(norm_method == "quantile"){
    return(c(-1.5, 0.0, 1.5))
    
  } else if(norm_method %in% c("standardisation", "standardisation_trim", "standardisation_winsor")){
    return(c(-3.0, 0.0, 3.0))
    
  } else if(norm_method %in% c("normalisation", "normalisation_trim", "normalisation_winsor")){
    return(c(0.0, 1.0))
    
  } else {
    ..error_reached_unreachable_code(paste0(".get_default_normalisation_range_for_plotting: unknown normalisation method: ", norm_method))
  }
}



..collect_and_aggregate_normalisation_info <- function(feature_info_list, instance_mask, feature_name){
  # Aggregate normalisation parameters. This function exists so that it can be
  # tested as part of a unit test.
  
  return(...collect_and_aggregate_normalisation_info(object_list=lapply(feature_info_list, function(x) (x@normalisation_parameters)),
                                                     instance_mask=instance_mask,
                                                     feature_name=feature_name))
}



...collect_and_aggregate_normalisation_info <- function(object_list, instance_mask, feature_name){
  # This is used to collect from a list of objects. It serves both to aggregate
  # data for the ensemble and when creating replacement data for batch
  # normalisation.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  n <- NULL
  
  if(!any(instance_mask)){
    return(list("parameters"=..create_normalisation_parameter_skeleton(feature_name=feature_name,
                                                                       method="none"),
                "instance_mask"=instance_mask))
  }
  
  # Check the class of the transformation objects.
  object_class <- sapply(object_list, function(x)(class(x)[1]))
  
  # Determine if there are any objects that are not NULL or
  # featureInfoParametersNormalisationNone.
  if(all(object_class[instance_mask] %in% c("NULL", "featureInfoParametersNormalisationNone"))){
    return(list("parameters"=..create_normalisation_parameter_skeleton(feature_name=feature_name,
                                                                       method="none"),
                "instance_mask"=instance_mask))
  }
  
  # For the remaining objects, check which class occurs most.
  class_table <- data.table::data.table("class"=object_class[instance_mask])[, list("n"=.N), by="class"]
  
  # Drop NULL and none transformations.
  class_table <- class_table[!class %in% c("NULL", "featureInfoParametersNormalisationNone")]
  
  # Select the object that occurs most often.
  most_common_class <- class_table[n==max(class_table$n), ]$class[1]
  
  # Update the instance mask.
  instance_mask <- instance_mask & object_class == most_common_class
  
  if(most_common_class %in% c("featureInfoParametersNormalisationStandardisation",
                              "featureInfoParametersNormalisationQuantile",
                              "featureInfoParametersNormalisationNormalisation",
                              "featureInfoParametersNormalisationParametricCombat",
                              "featureInfoParametersNormalisationNonParametricCombat")){
    
    # Aggregate shift values, and select median value.
    all_shift <- sapply(object_list[instance_mask], function(x) (x@shift))
    selected_shift <- stats::median(all_shift)
    
    # Aggregate scale values, and select mean value.
    all_scale <- sapply(object_list[instance_mask], function(x) (x@scale))
    selected_scale <- stats::median(all_scale)
    
    # Identify the first method -- its not that crucial.
    selected_method <- sapply(object_list[instance_mask], function(x) (x@method))[1]
    
  } else if(most_common_class %in% c("featureInfoParametersNormalisationMeanCentering")){
    
    # Aggregate shift values, and select median value.
    all_shift <- sapply(object_list[instance_mask], function(x) (x@shift))
    selected_shift <- stats::median(all_shift)
    
    # Identify the first method -- its not that crucial.
    selected_method <- sapply(object_list[instance_mask], function(x) (x@method))[1]
    
  } else {
    ..error_reached_unreachable_code(paste0("..collect_and_aggregate_normalisation_info: encountered an unknown class of objects: ", paste_s(most_common_class)))
  }
  
  return(list("parameters"=..create_normalisation_parameter_skeleton(feature_name=feature_name,
                                                                     method=unname(selected_method),
                                                                     shift=unname(selected_shift),
                                                                     scale=unname(selected_scale)),
              "instance_mask"=instance_mask))
}

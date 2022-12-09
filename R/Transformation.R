#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("featureInfoParametersTransformationNone",
         contains="featureInfoParameters",
         slots=list("method" = "character",
                    "reason" = "ANY"),
         prototype=list("method" = "none",
                        "reason" = NULL))

setClass("featureInfoParametersTransformationPowerTransform",
         contains="featureInfoParameters")

setClass("featureInfoParametersTransformationBoxCox",
         contains="featureInfoParametersTransformationPowerTransform",
         slots=list("method" = "character",
                    "lambda" = "numeric"),
         prototype=list("method"=NA_character_,
                        "lambda"=NA_real_))

setClass("featureInfoParametersTransformationYeoJohnson",
         contains="featureInfoParametersTransformationPowerTransform",
         slots=list("method" = "character",
                    "lambda" = "numeric"),
         prototype=list("method"=NA_character_,
                        "lambda"=NA_real_))

.get_available_none_transformation_methods <- function(){
  return("none")
}

.get_available_box_cox_transformation_methods <- function(){
  return(c("box_cox", "box_cox_trim", "box_cox_winsor", "box_cox_robust"))
}

.get_available_yeo_johnson_transformation_methods <- function(){
  return(c("yeo_johnson", "yeo_johnson_trim", "yeo_johnson_winsor", "yeo_johnson_robust"))
}

.get_available_transformation_methods <- function(type="all"){
  # Check type
  if(!type %in% c("all", "none", "box_cox", "yeo_johnson")) ..error_reached_unreachable_code(paste0(".get_available_transformation_methods: unspecified type: ", type))
  
  available_transformation_method <- NULL
  
  if(type %in% c("none", "all")){
    available_transformation_method <- c(available_transformation_method,
                                         .get_available_none_transformation_methods())
  }
  
  if(type %in% c("all", "box_cox")){
    available_transformation_method <- c(available_transformation_method,
                                         .get_available_box_cox_transformation_methods()) 
  }
  
  if(type %in% c("all", "yeo_johnson")){
    available_transformation_method <- c(available_transformation_method,
                                         .get_available_yeo_johnson_transformation_methods()) 
  }
  
  return(available_transformation_method)
}



create_transformation_parameter_skeleton <- function(feature_info_list,
                                                     feature_names=NULL,
                                                     transformation_method,
                                                     transformation_lambda=NULL,
                                                     .override_existing=FALSE){
  
  # Creates a skeleton for the provided transformation method. If
  # transformation_lambda is provided (typically not), this value is updated as
  # well.
  
  # Determine feature names from the feature info list, if provided.
  if(is.null(feature_names)) feature_names <- names(feature_info_list)
  
  # Select only features that appear in the feature info list.
  feature_names <- intersect(names(feature_info_list),
                             feature_names)
  
  # Skip step if no feature info objects are updated.
  if(is_empty(feature_names)) return(feature_info_list)
  
  # Check that method is applicable.
  .check_parameter_value_is_valid(x=transformation_method,
                                  var_name="transformation_method",
                                  values=.get_available_transformation_methods())
  
  # Check that transformation_lambda is numeric.
  if(!is.null(transformation_lambda)){
    .check_number_in_valid_range(x=transformation_lambda,
                                 var_name="transformation_lambda",
                                 range=c(-Inf, Inf))
  }
  
  # Update familiar info objects with a feature transformation skeleton.
  updated_feature_info <- fam_lapply(X=feature_info_list[feature_names],
                                     FUN=.create_transformation_parameter_skeleton,
                                     method=transformation_method,
                                     lambda=transformation_lambda,
                                     .override_existing=.override_existing)
  
  # Provide names for the updated feature info objects.
  names(updated_feature_info) <- feature_names
  
  # Replace list elements.
  feature_info_list[feature_names] <- updated_feature_info
  
  return(feature_info_list)
}



.create_transformation_parameter_skeleton <- function(feature_info, method, lambda=NULL, .override_existing=FALSE){
  
  # Check if transformation data was already completed, and does not require
  # being determined anew.
  if(feature_info_complete(feature_info@transformation_parameters) & !.override_existing) return(feature_info)
  
  # Pass to underlying function that constructs the skeleton.
  object <- ..create_transformation_parameter_skeleton(feature_name=feature_info@name,
                                                       feature_type=feature_info@feature_type,
                                                       available=is_available(feature_info),
                                                       method=method,
                                                       lambda=lambda)
  
  # Update transformation_parameters slot.
  feature_info@transformation_parameters <- object
  
  return(feature_info)
}



..create_transformation_parameter_skeleton <- function(feature_name,
                                                       feature_type="numeric",
                                                       available=TRUE,
                                                       method,
                                                       lambda=NULL){
  # This is the lowest level function for creation transformation parameter
  # skeletons.
  
  # Create the relevant objects.
  if(feature_type != "numeric"){
    object <- methods::new("featureInfoParametersTransformationNone",
                           reason="not a numeric feature")
  
  } else if(!available){
    object <- methods::new("featureInfoParametersTransformationNone",
                           reason="feature was omitted prior to transformation")
    
  } else if(method %in% .get_available_none_transformation_methods()){
    object <- methods::new("featureInfoParametersTransformationNone")
    
  } else if(method %in% .get_available_box_cox_transformation_methods()){
    object <- methods::new("featureInfoParametersTransformationBoxCox",
                           "method"=method)
    
  } else if(method %in% .get_available_yeo_johnson_transformation_methods()){
    object <- methods::new("featureInfoParametersTransformationYeoJohnson",
                           "method"=method)
    
  } else {
    ..error_reached_unreachable_code(paste0("create_transformation_parameter_skeleton: encountered an unknown transformation method: ", paste_s(method)))
  }
  
  # Set the name of the object.
  object@name <- feature_name
  
  # Check if lambda is not NULL.
  if(!is.null(lambda)){
    object <- add_feature_info_parameters(object, data=NULL, lambda=lambda)
  }
  
  # Update the familiar version.
  object <- add_package_version(object=object)
  
  return(object)
}



add_transformation_parameters <- function(cl=NULL,
                                          feature_info_list,
                                          data,
                                          verbose=FALSE){
  # Determine transformation parameters and add them to the feature_info_list.
  
  # Find feature columns.
  feature_names <- get_feature_columns(x=data)
  
  # Sanity check.
  if(!(setequal(feature_names, get_available_features(feature_info_list=feature_info_list)))){
    ..error_reached_unreachable_code("add_transformation_parameters: features in data and the feature info list are expect to be the same, but were not.")
  }
  
  # Iterate over features.
  updated_feature_info <- fam_mapply(cl=cl,
                                     FUN=.add_transformation_parameters,
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



.add_transformation_parameters <- function(feature_info,
                                           data){
  
  # Pass to underlying function that adds the feature info.
  object <- add_feature_info_parameters(object=feature_info@transformation_parameters,
                                        data=data)
  
  # Update transformation_parameters slot.
  feature_info@transformation_parameters <- object
  
  return(feature_info)
}



##### initialize (none) --------------------------------------------------------
setMethod("initialize", signature(.Object="featureInfoParametersTransformationNone"),
          function(.Object, ...){
            
            # Update with parent class first.
            .Object <- callNextMethod()
            
            # The parameter set is by definition complete when no transformation
            # is performed.
            .Object@complete <- TRUE
            
            return(.Object)
          })


##### add_feature_info_parameters (any power transform, NULL) ------------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersTransformationPowerTransform", data="NULL"),
          function(object, 
                   data,
                   lambda=NULL,
                   ...) {
            
            if(is.numeric(lambda)){
              if(is.finite(lambda)){
                # Lambda is numeric, and not NA. This is typical when lambda is set
                # externally. We then update the lambda parameter.
                object@lambda <- lambda
                object@complete <- TRUE
                
                return(object)
                
              } else {
                # Lambda is numeric, but NA or Inf. We then return a none-class
                # transformation instead. This is typical when updating versions
                # prior to familiar v1.2.0.
                object <- ..create_transformation_parameter_skeleton(feature_name=object@name,
                                                                     method="none")
                
                object@reason <- "lambda was NA or infinite"
                
                return(object)
              }
            }
            
            # If lambda is not set, but data is NULL, lambda cannot be
            # determined.
            if(is.null(lambda)){
              object <- ..create_transformation_parameter_skeleton(feature_name=object@name,
                                                                   method="none")
              
              object@reason <- "insufficient data to determine lambda"
              
              return(object)
            }
            
            # If lambda is not numeric, it can still be NA. We then return a
            # none-class transformation instead. This is typical when updating
            # versions prior to familiar v1.2.0.
            if(is.na(lambda)){
              object <- ..create_transformation_parameter_skeleton(feature_name=object@name,
                                                                   method="none")
              
              object@reason <- "lambda was NA"
              
              return(object)
            }
            
            # Any other reasons why lambda cannot be set directly.
            object <- ..create_transformation_parameter_skeleton(feature_name=object@name,
                                                                 method="none")
            
            object@reason <- "lambda could not be determined for an unknown reason"
            
            return(object)
          })



##### add_feature_info_parameters (any power transform, ANY) -------------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersTransformationPowerTransform", data="ANY"),
          function(object, 
                   data,
                   ...) {
            
            # This method is targeted in when directly determining the
            # transformation parameters. It is usually called from the child
            # functions.
            
            # Check if all required parameters have been set.
            if(feature_info_complete(object)) return(object)
            
            # Check if data is not empty, and return none-class transformation
            # object, if it is. This is done by calling the method with
            # signature data=NULL to handle setting the data.
            if(is_empty(data)) return(add_feature_info_parameters(object=object, data=NULL))
            
            # Remove non-finite values from data.
            data <- data[is.finite(data)]
            
            # Again, check if data is not empty.
            if(is_empty(data)) return(add_feature_info_parameters(object=object, data=NULL))
            
            # Check that at least three unique values are present.
            if(length(unique(data)) <= 3) return(add_feature_info_parameters(object=object, data=NULL))
            
            # TODO: test for unimodal distributions using the Denoho test.
            
            return(object)
          })



##### add_feature_info_parameters (Box-Cox, ANY) -------------------------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersTransformationBoxCox", data="ANY"),
          function(object, 
                   data,
                   ...) {
            
            # Check if all required parameters have been set.
            if(feature_info_complete(object)) return(object)
            
            # Run general checks for power transforms. This may yield
            # none-transforms which are complete by default.
            object <- callNextMethod()
            
            # Check if all required parameters have been set now.
            if(feature_info_complete(object)) return(object)
            
            # Remove any non-finite values.
            data <- data[is.finite(data)]
            
            # Remove any non-positive values. Box-Cox transformations can only
            # be applied to strictly positive transformations.
            data <- data[data > 0]
            
            # Check if data is not empty after removing non-, and return none-class transformation
            # object, if it is. This is done by calling the method with
            # signature data=NULL to handle setting the data.
            if(is_empty(data)){
              object <- add_feature_info_parameters(object=object, data=NULL)
              object@reason <- "no strictly positive values left for Box-Cox transformation"
              
              return(object)
            }
            
            # Trimming and winsoring of input data.
            if(object@method %in% c("box_cox_trim")){
              data <- trim(data, fraction=0.05)
              
            } else if(object@method %in% c("box_cox_winsor")){
              data <- winsor(data, fraction=0.05)
            }
            
            # Optimise lambda for Box-Cox transformations.
            if(object@method %in% c("box_cox_robust")){
              # Robust method based on Raymaekers J, Rousseeuw PJ. Transforming
              # variables to central normality. Mach Learn. 2021.
              # doi:10.1007/s10994-021-05960-5
              optimal_lambda <- .transformation_robust_optimisation(
                x=data,
                type="box_cox")
              
            } else {
              # Standard method based on optimising log-likelihood of the normal
              # distribution.
              optimal_lambda <- suppressWarnings(
                stats::optimise(
                  ..box_cox_loglik,
                  interval=c(-10, 10),
                  x=data,
                  maximum=TRUE))
              
              optimal_lambda <- optimal_lambda$maximum
            }
            
            # Select optimal lambda that maximises log-likelihood score.
            if(is.finite(optimal_lambda$objective)){
              lambda <- round(optimal_lambda$maximum, digits=1)
              
            } else {
              lambda <- 1.0
            }
            
            # Set lambda parameter.
            object@lambda <- lambda
            object@complete <- TRUE
            
            return(object)
          })



##### add_feature_info_parameters (Yeo-Johnson, ANY) ---------------------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersTransformationYeoJohnson", data="ANY"),
          function(object, 
                   data,
                   ...) {
            
            # Check if all required parameters have been set.
            if(feature_info_complete(object)) return(object)
            
            # Run general checks for power transforms. This may yield
            # none-transforms which are complete by default.
            object <- callNextMethod()
            
            # Check if all required parameters have been set now.
            if(feature_info_complete(object)) return(object)
            
            # Remove any non-finite values.
            data <- data[is.finite(data)]
            
            # Trimming and winsoring of input data.
            if(object@method %in% c("yeo_johnson_trim")){
              data <- trim(data, fraction=0.05)
              
            } else if(object@method %in% c("yeo_johnson_winsor")){
              data <- winsor(data, fraction=0.05)
            }
            
            # Optimise lambda for Box-Cox transformations.
            if(object@method %in% c("yeo_johnson_robust")){
              # Robust method based on Raymaekers J, Rousseeuw PJ. Transforming
              # variables to central normality. Mach Learn. 2021.
              # doi:10.1007/s10994-021-05960-5
              lambda <- .transformation_robust_optimisation(
                x=data,
                type="yeo_johnson")
              
            } else {
              # Standard method based on optimising log-likelihood of the normal
              # distribution.
              optimal_lambda <- suppressWarnings(
                stats::optimise(
                  ..yeo_johnson_loglik,
                  interval=c(-10, 10),
                  x=data,
                  maximum=TRUE))
              
              lambda <- ifelse(
                is.finite(optimal_lambda$objective),
                optimal_lambda$maximum,
                1.0)
            }
            
            # Set lambda parameter.
            object@lambda <- round(lambda, digits = 1)
            object@complete <- TRUE
            
            return(object)
          })



##### apply_feature_info_parameters (Box-Cox) ----------------------------------
setMethod("apply_feature_info_parameters", signature(object="featureInfoParametersTransformationBoxCox", data="ANY"),
          function(object, 
                   data,
                   invert=FALSE,
                   ...){
            
            return(..box_cox_transform(lambda=object@lambda,
                                       x=data,
                                       invert=invert))
          })



##### apply_feature_info_parameters (Yeo-Johnson) ------------------------------
setMethod("apply_feature_info_parameters", signature(object="featureInfoParametersTransformationYeoJohnson", data="ANY"),
          function(object, 
                   data,
                   invert=FALSE,
                   ...){
            
            return(..yeo_johnson_transform(lambda=object@lambda,
                                           x=data,
                                           invert=invert))
          })



..box_cox_transform <- function(lambda, x, invert=FALSE){
  # After Box, G. E., & Cox, D. R. (1964). An analysis of transformations.
  # Journal of the Royal Statistical Society. Series B (Methodological),
  # 211-252.
  
  if(invert){
    # Inverse transformations: From transformed value to original value
    if(lambda==0){
      y <- exp(x)
      
    } else {
      y <- (x * lambda + 1)^(1/lambda)
    }
    
  } else {
    # From original value to transformed value
    
    # Find any non-positive entries and replace them (this may happen in new
    # applications).
    neg_index <- x <= 0 & is.finite(x)
    if(any(neg_index)) x[neg_index] <- min(x[x>0 & is.finite(x)])
    
    if(lambda==0){
      y <- log(x)
      
    } else {
      y <- (x^lambda - 1) / lambda
    }
  }
  
  return(y)
}



..box_cox_dev <- function(lambda, x){
  # First order derivative of the Yeo-Johnson transformation with respect to x.
  return(x^(lambda - 1))
}



..box_cox_loglik <- function(lambda, x, w=NULL){
  
  # Set w
  if(is.null(w)) w <- numeric(length(x)) + 1.0
  
  # Transform x under the provided lambda.
  y <- ..box_cox_transform(lambda=lambda, x=x)
  
  # Compute the sum of the weights.
  sum_w <- sum(w)
  if(sum_w == 0) return(NA_real_)
  
  # Compute the weighted estimates of the mean mu and variance sigma squared for
  # y.
  mu_hat <- sum(w * y) / sum_w
  sigma_hat_squared <- sum(w * (y - mu_hat)^2) / sum_w
  
  # Log-likelihood cannot be determined if the sigma estimate equals 0.0
  if(sigma_hat_squared == 0) return(NA_real_)
  
  # Compute the log likelihood under the assumption that the transformed
  # variable y follows the normal distribution.
  llf <- (lambda - 1.0) * sum(w * log(x)) - sum_w / 2.0 * log(sigma_hat_squared)
  
  return(llf)
}



..box_cox_transform_rectified <- function(lambda, x){
  # The rectified transform replaces part of the transformed values by a first
  # order (linear) approximation. Linear approximation of a function f(x) at
  # point a is defined as y = f(a) + (x - a) * f'(a), with f'(a) being the
  # derivative of f(x=a). Here function f is the Box-Cox transformation, and
  # point a is the first or third quartile, depending on lambda.
  
  # Find first and third quartiles.
  cut_off <- stats::quantile(x, probs=c(0.25, 0.75), names=FALSE)
  
  y <- numeric(length(x))
  
  # Perform rectified transformation
  if(lambda == 1.0){
    # For lambda equal to 1, the mapping is linear, and no elements are
    # out-of-range and require rectification.
    out_of_range <- logical(length(x))
    
  } else if(lambda > 1.0){
    # Select the cut-off value, i.e. the first quartile.
    cut_off <- cut_off[1]
    
    # Elements that have value below Cl (1st quartile) are rectified.
    out_of_range <- x < cut_off
    
  } else {
    # Lambda < 1.0.
    
    # Select the cut-off value, i.e. the third quartile.
    cut_off <- cut_off[2]
    
    # Elements that have value above Cu (3rd quartile) are rectified.
    out_of_range <- x > cut_off
  }
  
  if(any(out_of_range)){
    # Linear approximation to out-of-range elements.
    y[out_of_range] <- ..box_cox_transform(lambda=lambda, x=cut_off) + (x[out_of_range]-cut_off) * ..box_cox_dev(lambda=lambda, x=cut_off)
  }
  
  # Map elements that do not require rectification using the normal Box-Cox
  # transformation.
  if(any(!out_of_range)){
    y[!out_of_range] <- ..box_cox_transform(lambda=lambda, x=x[!out_of_range])
  }
  
  return(y)
}



..yeo_johnson_transform <- function(lambda, x, invert=FALSE){
  # After Yeo, I. K., & Johnson, R. A. (2000). A new family of power
  # transformations to improve normality or symmetry. Biometrika, 87(4),
  # 954-959.
  
  # Copy output
  y <- x
  
  # Determine positive and negative elements of the input vector
  pos_index <- x >= 0 & is.finite(x)
  neg_index <- x < 0 & is.finite(x)
  
  if(invert) {
    # Inverse transformations: From transformed value to original value
    if(any(pos_index)){
      if(lambda != 0){
        y[pos_index] <- ((x[pos_index] * lambda + 1)^(1/lambda) - 1)
        
      } else {
        y[pos_index] <- exp(x[pos_index]) - 1
      }
    }
    
    if(any(neg_index)){
      if(lambda != 2) {
        y[neg_index] <- 1 - (x[neg_index] * (lambda-2) + 1)^(1/(2-lambda))
        
      } else {
        y[neg_index] <- 1 - exp(-x[neg_index])
      }
    }
    
  } else {
    
    # From original value to transformed value
    if(any(pos_index)){
      if(lambda == 0.0){
        y[pos_index] <- log1p(x[pos_index])
        
      } else {
        y[pos_index] <- ((x[pos_index] + 1)^lambda - 1) / lambda
      }
    }
    
    if(any(neg_index)){
      if(lambda == 2.0){
        y[neg_index] <- -log1p(-x[neg_index])
        
      } else {
        y[neg_index] <- -((-x[neg_index] + 1)^(2-lambda) - 1) / (2-lambda)
      }
    }
  }
  
  return(y)
}



..yeo_johnson_dev <- function(lambda, x){
  # First order derivative of the Yeo-Johnson transformation with respect to x.
  return((1 + abs(x))^(sgn(x) * (lambda - 1)))
}



..yeo_johnson_loglik <- function(lambda, x, w=NULL){
  
  # Set w
  if(is.null(w)) w <- numeric(length(x)) + 1.0
  
  # Transform x under the provided lambda.
  y <- ..yeo_johnson_transform(lambda=lambda, x=x)
  
  # Compute the sum of the weights.
  sum_w <- sum(w)
  if(sum_w == 0) return(NA_real_)
  
  # Compute the weighted estimates of the mean mu and variance sigma squared for
  # y.
  mu_hat <- sum(w * y) / sum_w
  sigma_hat_squared <- sum(w * (y - mu_hat)^2) / sum_w
  
  # Log-likelihood cannot be determined if the sigma estimate equals 0.0
  if(sigma_hat_squared == 0) return(NA_real_)
  
  # Compute the log likelihood under the assumption that the transformed
  # variable y follows the normal distribution.
  llf <- (lambda - 1.0) * sum(w * sign(x) * log1p(abs(x))) - sum_w/2.0 * log(sigma_hat_squared)
  
  return(llf)
}



..yeo_johnson_transform_rectified <- function(lambda, x){
  # The rectified transform replaces part of the transformed values by a first
  # order (linear) approximation. Linear approximation of a function f(x) at
  # point a is defined as y = f(a) + (x - a) * f'(a), with f'(a) being the
  # derivative of f(x=a). Here function f is the Yeo-Johnson transformation, and
  # point a is the first or third quartile, depending on lambda.
  
  # Find first and third quartiles.
  cut_off <- stats::quantile(x, probs=c(0.25, 0.75), names=FALSE)
  
  y <- numeric(length(x))
  
  # Perform rectified transformation
  if(lambda == 1.0){
    # For lambda equal to 1, the mapping is linear, and no elements are
    # out-of-range and require rectification.
    out_of_range <- logical(length(x))
    
  } else if(lambda > 1.0){
    # Select the cut-off value, i.e. the first quartile.
    cut_off <- cut_off[1]
    
    # Elements that have value below Cl (1st quartile) are rectified.
    out_of_range <- x < cut_off
    
  } else {
    # Lambda < 1.0.
    
    # Select the cut-off value, i.e. the third quartile.
    cut_off <- cut_off[2]
    
    # Elements that have value above Cu (3rd quartile) are rectified.
    out_of_range <- x > cut_off
  }
  
  if(any(out_of_range)){
    # Linear approximation to out-of-range elements.
    y[out_of_range] <- ..yeo_johnson_transform(lambda=lambda, x=cut_off) + (x[out_of_range]-cut_off) * ..yeo_johnson_dev(lambda=lambda, x=cut_off)
  }
  
  # Map elements that do not require rectification using the normal Box-Cox
  # transformation.
  if(any(!out_of_range)){
    y[!out_of_range] <- ..yeo_johnson_transform(lambda=lambda, x=x[!out_of_range])
  }
  
  return(y)
}





.transformation_robust_optimisation <- function(x, type){
  # This follows the algorithm from Raymaekers J, Rousseeuw PJ. Transforming
  # variables to central normality. Mach Learn. 2021.
  # doi:10.1007/s10994-021-05960-5
  
  # Sort x.
  x <- order(x)
  
  # Compute z-values according to the inverse cumulative density function.
  z_expected <- stats::qnorm(p=(seq_along(x) - 1/3) / (length(x) + 1/3))
  
  # Step 1: Compute initial estimate for lambda.
  # Standard method based on optimising log-likelihood of the normal
  # distribution.
  
  optimal_lambda <- suppressWarnings(
    stats::optimise(
      ..transformation_rectified_optimisation,
      interval=c(-10, 10),
      x=x,
      z=z_expected,
      type=type,
      maximum=FALSE))
  
  optimal_lambda <- ifelse(
    is.finite(optimal_lambda$objective),
    optimal_lambda$maximum,
    1.0)
  
  # Step 2: Compute lambda from reweighted maximum likelihood.
  optimal_lambda <- ..transformation_rectified_optimisation(
    lambda_0=optimal_lambda,
    x=x,
    type=type,
    ii=1L)
  
  # Step 3: Compute lambda from reweighted maximum likelihood again.
  optimal_lambda <- ..transformation_rectified_optimisation(
    lambda_0=optimal_lambda,
    x=x,
    type=type,
    ii=2L)
  
  if(!is.finite(optimal_lambda)) return(1.0)
  
  return(optimal_lambda)
}




..transformation_rectified_optimisation <- function(lambda, x, z, type){
  
  rectifier_FUN <- switch(
    type,
    "box_cox"=..box_cox_transform_rectified,
    "yeo_johnson"=..yeo_johnson_transform_rectified
  )
  
  # Compute values after power transformation with the appropriate lambda value.
  y <- do.call(
    rectifier_FUN,
    args=list(
      "lambda"=lambda,
      "x"=x))
  
  # Compute M-estimates for locality and scale
  robust_estimates <- huber_estimate(y)
  
  # Check problematic values.
  if(is.null(robust_estimates)) return(NA_real_)
  if(robust_estimates$sigma == 0.0) return(NA_real_)
  
  # Compute residuals.
  residual <- (y - robust_estimates$mu) / robust_estimates$sigma - z
  
  # Compute Tukey bisquare function to truncate weights of outlier residuals.
  truncated_weights <- numeric(length(residual)) + 1.0
  valid_residuals <- which(residual <= 0.5)
  
  if(length(valid_residuals) > 0){
    truncated_weights[valid_residuals] <- 1.0 - (1.0 - (residual[valid_residuals] / 0.5)^2)^3
  }
  
  return(sum(truncated_weights))
}



..transformation_reweighted_optimisation <- function(lambda_0, x, type, ii){

  if(!is.finite(lambda_0)) return(NA_real_)
  
  # Set transformation function.
  if(ii == 1){
    # For the initial step, use the rectified transformations
    transform_FUN <- switch(
      type,
      "box_cox"=..box_cox_transform_rectified,
      "yeo_johnson"=..yeo_johnson_transform_rectified
    )
    
  } else {
    transform_FUN <- switch(
      type,
      "box_cox"=..box_cox_transform,
      "yeo_johnson"=..yeo_johnson_transform
    )
  }
  
  # Set log-likelihood function.
  loglik_FUN <- switch(
    type,
    "box_cox"=..box_cox_loglik,
    "yeo_johnson"=..yeo_johnson_loglik
  )
  
  # Perform transformation for lambda_0
  y <- do.call(
    transform_FUN,
    args=list(
      "lambda"=lambda_0,
      "x"=x
    )
  )
  
  # Compute M-estimates for locality and scale
  robust_estimates <- huber_estimate(y)
  
  # Check problematic values.
  if(is.null(robust_estimates)) return(NA_real_)
  if(robust_estimates$sigma == 0.0) return(NA_real_)
  
  # Compute weights.
  weights <- as.numeric(abs(y - robust_estimates$mu) / robust_estimates$sigma <= stats::qnorm(0.995))
  if(sum(weights) == 0.0) return(NA_real_)
  
  # Compute optimal lambda.
  optimal_lambda <- suppressWarnings(
    stats::optimise(
      loglik_FUN,
      interval=c(-10, 10),
      x=x,
      w=weights,
      maximum=TRUE))
  
  lambda <- ifelse(
    is.finite(optimal_lambda$objective),
    optimal_lambda$maximum,
    1.0)
  
  return(lambda)
}



..collect_and_aggregate_transformation_info <- function(feature_info_list, instance_mask, feature_name){
  # Aggregate transformation parameters. This function exists so that it can be
  # tested as part of a unit test.
  
  # Suppress NOTES due to non-standard evaluation in data.table
  n <- NULL
  
  if(!any(instance_mask)){
    return(list("parameters"=..create_transformation_parameter_skeleton(feature_name=feature_name,
                                                                        method="none"),
                "instance_mask"=instance_mask))
  }
  
  # Check the class of the transformation objects.
  object_class <- sapply(feature_info_list, function(x)(class(x@transformation_parameters)[1]))
  
  # Determine if there are any objects that are not NULL or
  # featureInfoParametersTransformationNone.
  if(all(object_class[instance_mask] %in% c("NULL", "featureInfoParametersTransformationNone"))){
    return(list("parameters"=..create_transformation_parameter_skeleton(feature_name=feature_name,
                                                                        method="none"),
                "instance_mask"=instance_mask))
  }
  
  # For the remaining objects, check which class occurs most.
  class_table <- data.table::data.table("class"=object_class[instance_mask])[, list("n"=.N), by="class"]
  
  # Drop NULL and none transformations.
  class_table <- class_table[!class %in% c("NULL", "featureInfoParametersTransformationNone")]
  
  # Select the object that occurs most often.
  most_common_class <- class_table[n==max(class_table$n), ]$class[1]
  
  # Update the instance mask.
  instance_mask <- instance_mask & object_class == most_common_class
  
  if(most_common_class %in% c("featureInfoParametersTransformationBoxCox", "featureInfoParametersTransformationYeoJohnson")){
    # Aggregate lambda values, and select modal value.
    all_lambda <- sapply(feature_info_list[instance_mask], function(x) (x@transformation_parameters@lambda))
    selected_lambda <- get_mode(all_lambda)
    
    # Identify the method for the selected lambda value.
    selected_method <- sapply(feature_info_list[instance_mask], function(x) (x@transformation_parameters@method))
    selected_method <- selected_method[all_lambda == selected_lambda][1]
    
    # Update the instance mask to include any close lambda values.
    instance_mask[instance_mask] <- all_lambda >= selected_lambda - 0.1 & all_lambda <= selected_lambda + 0.1
    
  } else {
    ..error_reached_unreachable_code()
  }
  
  return(list("parameters"=..create_transformation_parameter_skeleton(feature_name=feature_name,
                                                                      method=selected_method,
                                                                      lambda=selected_lambda),
              "instance_mask"=instance_mask))
}

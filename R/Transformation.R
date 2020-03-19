add_transformation_parameters <- function(cl=NULL, feature_info_list, data_obj, settings){
  # Find transformation parameters and add them to the feature_info_list
  
  # Determine which columns contain feature data
  feature_columns <- get_feature_columns(x=data_obj)
  
  # Determine transformation parameters by iterating over the features
  upd_list <- lapply(feature_columns, function(ii, feature_info_list, data_obj, transform_method){
    
    # Get object corresponding to the current featues
    object <- feature_info_list[[ii]]
    
    # Set transformation parameters
    object@transformation_parameters <- transformation.get_transform_parameters(x=data_obj@data[[ii]], transform_method=transform_method)
    
    return(object)
  }, feature_info_list=feature_info_list, data_obj=data_obj, transform_method=settings$prep$transform_method)
  
  # Set names of the updated list
  names(upd_list) <- feature_columns
  
  # Update the list
  feature_info_list[feature_columns] <- upd_list

  return(feature_info_list)
}



transformation.apply_transform <- function(x, trans_param, invert=FALSE){
  
  transform_method <- trans_param$transform_method[1]
  if(transform_method=="none"){
    # No normalisation
    return(x)
  } else if(transform_method %in% c("box_cox", "box_cox_trim", "box_cox_winsor")){
    # Box-Cox transformation
    return(transformation.box_cox(lambda=trans_param$transform_lambda, x=x, invert=invert))
  } else if(transform_method %in% c("yeo_johnson", "yeo_johnson_trim", "yeo_johnson_winsor")) {
    return(transformation.yeo_johnson(lambda=trans_param$transform_lambda, x=x, invert=invert))
  } else {
    ..error_reached_unreachable_code("transformation.apply_transform_unknown_transformation")
  }
}



transformation.get_transform_parameters <- function(x, transform_method){
  
  # Determine class of vector x
  class_x <- class(x)
  
  # Filter out characters, logical and factor variables - these will not be considered for transformation
  if(any(class_x %in% c("character", "logical", "factor"))){
    return(list("transform_method"="none", "transform_lambda"=as.double(NA)))
  }
  
  # Remove non-finite value from x
  x <- x[is.finite(x)]
  if(length(x)==0){
    return(list("transform_method"="none", "transform_lambda"=as.double(NA)))
  }
  
  # Filter out numerical or integer variables that have 3 or less unique numerical variables - these are likely contrasts that shouldn't be transformed
  if(length(unique(x)) <= 3){
    return(list("transform_method"="none", "transform_lambda"=as.double(NA)))
  }
  
  # No transformate for
  if(transform_method=="none") {
    # No transformation takes place
    return(list("transform_method"="none", "transform_lambda"=as.double(NA)))
  }
  
  if(transform_method %in% c("box_cox", "box_cox_trim", "box_cox_winsor") & !all(x > 0)) {
    # No transformation takes place for x containing non-positive values
    return(list("transform_method"="none", "transform_lambda"=as.double(NA)))
  }
  
  ##### Identify lambda for transformation #####
  
  # Trimming and winsoring of input data
  if(transform_method %in% c("box_cox_trim", "yeo_johnson_trim")){
    x <- trim(x, fraction=0.05)
  } else if(transform_method %in% c("box_cox_winsor", "yeo_johnson_winsor")){
    x <- winsor(x, fraction=0.05)
  }
  
  if(transform_method %in% c("box_cox", "box_cox_trim", "box_cox_winsor")){
    # Box-Cox transformations (inverse, logarithmic, square root, linear, squared)
    lambda <- c(-2.0, -1.0, -0.5, 0.0, 0.3333, 0.5, 1.0, 1.5, 2.0)
    y_list <- lapply(lambda, transformation.box_cox, x=x)
    scores <- lapply(seq_len(length(lambda)), function(ii, x, y_list, lambda){
      return(.transformation.box_cox_loglik(x=x, y=y_list[[ii]], lambda=lambda[ii]))
    }, x=x, y_list=y_list, lambda=lambda)
    
  } else if(transform_method %in% c("yeo_johnson", "yeo_johnson_trim", "yeo_johnson_winsor")){
    # Yeo-Johnson transformations (1.0 is linear)
    lambda <- c(-2.0, -1.0, -0.5, 0.0, 0.33333, 0.5, 1.0, 1.5, 2.0)
    y_list <- lapply(lambda, transformation.yeo_johnson, x=x)
    scores <- sapply(seq_len(length(lambda)), function(ii, x, y_list, lambda){
      return(.transformation.yeo_johnson_loglik(x=x, y=y_list[[ii]], lambda=lambda[ii]))
    }, x=x, y_list=y_list, lambda=lambda)
  } else {
    ..error_reached_unreachable_code("transformation.get_transform_parameters_unknown_transformation")
  }

  # Select optimal lambda that maximises log-likelihood score
  opt_lambda <- lambda[which.max(scores)]
  
  # Return transformation
  return(list("transform_method"=transform_method, "transform_lambda"=opt_lambda))
}



transformation.box_cox <- function(lambda, x, invert=FALSE){
  # After Box, G. E., & Cox, D. R. (1964). An analysis of transformations. Journal of the Royal Statistical Society. Series B (Methodological), 211-252.
  
  if(invert){
    # Inverse transformations: From transformed value to original value
    if(lambda==0){
      y <- exp(x)
    } else {
      y <- (x * lambda + 1)^(1/lambda)
    }
    
  } else {
    # From original value to transformed value
    
    # Find any non-positive entries and replace them (this may happen in new applications)
    neg_index <- x <= 0 & is.finite(x)
    if(any(neg_index)) {
      x[neg_index] <- min(x[x>0 & is.finite(x)])
    }
    
    if(lambda==0){
      y <- log(x)
    } else {
      y <- (x^lambda - 1) / lambda
    }
  }
  
  return(y)
}



transformation.yeo_johnson <- function(lambda, x, invert=FALSE){
  # After Yeo, I. K., & Johnson, R. A. (2000). A new family of power transformations to improve normality or symmetry. Biometrika, 87(4), 954-959.
  
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
    
    if (any(neg_index)){
      if(lambda == 2.0){
        y[neg_index] <- -log1p(-x[neg_index])
      } else {
        y[neg_index] <- -((-x[neg_index] + 1)^(2-lambda) - 1) / (2-lambda)
      }
    }
  }
  
  return(y)
}

.transformation.box_cox_loglik <- function(x, y, lambda){
  # Compute log-likelihood of box-cox transformation
  
  # Remove non-finite values
  valid_values <- is.finite(y)
  x <- x[valid_values]
  y <- y[valid_values]
  
  # Find the number of samples
  n <- length(x)
  
  # Find the mean of y
  y_mean <- mean(y)
  
  # Compute the log-likelihood
  llf <- (lambda - 1.0) * sum(log(x)) - n / 2.0 * log(sum((y - y_mean)^2.0) / n)
  
  return(llf)
}

.transformation.yeo_johnson_loglik <- function(x, y, lambda){
  # Compute log-likelihood of yeo-johnson transform
  
  # Remove non-finite values
  valid_values <- is.finite(y)
  x <- x[valid_values]
  y <- y[valid_values]
  
  # Find the number of samples
  n <- length(x)
  
  # Find the mean of y
  y_mean <- mean(y)
  
  # Compute the log-likelihood
  llf <- (lambda - 1.0) * sum(sign(x) * log1p(abs(x))) - n / 2.0 * log(sum((y - y_mean)^2.0) /n)
  
  return(llf)
}

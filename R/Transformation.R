add_transformation_parameters <- function(cl=NULL, feature_info_list, data_obj, settings=NULL, transformation_method=NULL){
  # Find transformation parameters and add them to the feature_info_list
  
  if(is.null(transformation_method)) transformation_method <- settings$prep$transform_method
  
  # Determine which columns contain feature data
  feature_columns <- get_feature_columns(x=data_obj)
  
  # Determine transformation parameters by iterating over the features
  upd_list <- lapply(feature_columns, function(feature, feature_info_list, data_obj, transform_method){
    
    # Get object corresponding to the current featues
    object <- feature_info_list[[feature]]
    
    # Set transformation parameters
    object@transformation_parameters <- transformation.get_transform_parameters(x=data_obj@data[[feature]],
                                                                                transform_method=transform_method,
                                                                                feature=feature)
    
    return(object)
  },
  feature_info_list=feature_info_list,
  data_obj=data_obj,
  transform_method=transformation_method)
  
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
    
  } else if(transform_method %in% .get_available_transformation_methods("box_cox")){
    # Box-Cox transformation
    return(transformation.box_cox(lambda=trans_param$transform_lambda, x=x, invert=invert))
    
  } else if(transform_method %in% .get_available_transformation_methods("yeo_johnson")) {
    # Yeo-Johnson transformation.
    return(transformation.yeo_johnson(lambda=trans_param$transform_lambda, x=x, invert=invert))
    
  } else {
    ..error_reached_unreachable_code(paste0("transformation.apply_transform: encountered an unknown transformation: ", transform_method))
  }
}



transformation.get_transform_parameters <- function(x, transform_method, feature){
  
  # Determine class of vector x
  class_x <- class(x)
  
  # Filter out characters, logical and factor variables - these will not be
  # considered for transformation.
  if(any(class_x %in% c("character", "logical", "factor"))){
    return(list("transform_method"="none",
                "transform_lambda"=as.double(NA)))
  }
  
  # Remove non-finite value from x.
  x <- x[is.finite(x)]
  if(length(x)==0){
    return(list("transform_method"="none",
                "transform_lambda"=as.double(NA)))
  }
  
  # Filter out numerical or integer variables that have 3 or less unique
  # numerical variables - these are likely contrasts that shouldn't be
  # transformed.
  if(length(unique(x)) <= 3){
    return(list("transform_method"="none",
                "transform_lambda"=as.double(NA)))
  }
  
  # No transformations for none.
  if(transform_method=="none") {
    # No transformation takes place
    return(list("transform_method"="none",
                "transform_lambda"=as.double(NA)))
  }
  
  if(transform_method %in% .get_available_transformation_methods("box_cox") & !all(x > 0)) {
    warning(paste0("One or more negative or zero values were found in the ", feature, " feature. ",
                   "The Box-Cox transformation method requires positive, non-zero, values. ",
                   "Alternatively, use the Yeo-Johnson transformation."))
    
    # No box-coxtransformation takes place for x containing non-positive values.
    return(list("transform_method"="none",
                "transform_lambda"=as.double(NA)))
  }
  
  ##### Identify lambda for transformation #####
  
  # Trimming and winsoring of input data
  if(transform_method %in% c("box_cox_trim", "yeo_johnson_trim")){
    x <- trim(x, fraction=0.05)
    
  } else if(transform_method %in% c("box_cox_winsor", "yeo_johnson_winsor")){
    x <- winsor(x, fraction=0.05)
  }
  
  if(transform_method %in% .get_available_transformation_methods("box_cox")){
    # Box-Cox transformations.
    lambda <- c(-10, 10)
    opt_lambda <- stats::optimise(..box_cox_loglik, interval=lambda, x=x, maximum=TRUE)
    
  } else if(transform_method %in% .get_available_transformation_methods("yeo_johnson")){
    # Yeo-Johnson transformations.
    lambda <- c(-10, 10)
    opt_lambda <- stats::optimise(..yeo_johnson_loglik, interval=lambda, x=x, maximum=TRUE)
    
  } else {
    ..error_reached_unreachable_code("transformation.get_transform_parameters_unknown_transformation")
  }

  # Select optimal lambda that maximises log-likelihood score.
  opt_lambda <- round(opt_lambda$maximum, digits=1)
  
  # Return transformation
  return(list("transform_method"=transform_method,
              "transform_lambda"=opt_lambda))
}



transformation.box_cox <- function(lambda, x, invert=FALSE){
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



transformation.yeo_johnson <- function(lambda, x, invert=FALSE){
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


..box_cox_loglik <- function(lambda, x){
  # Determine length.
  n <- length(x)
  
  # Transform x under the provided lambda.
  y <- transformation.box_cox(lambda=lambda, x=x)
  
  # Compute the estimates of the mean mu and variance sigma squared for y.
  mu_hat <- mean(y)
  sigma_hat_squared <- 1 /n * sum((y - mu_hat)^2)
  
  # Compute the log likelihood under the assumption that the transformed
  # variable y follows the normal distribution.
  llf <- (lambda - 1.0) * sum(log(x)) - n / 2.0 * log(sigma_hat_squared)
  
  return(llf)
}


..yeo_johnson_loglik <- function(lambda, x){
  
  # Determine length.
  n <- length(x)
  
  # Transform x under the provided lambda.
  y <- transformation.yeo_johnson(lambda=lambda, x=x)
  
  # Compute the estimates of the mean mu and variance sigma squared for y.
  mu_hat <- mean(y)
  sigma_hat_squared <- 1 /n * sum((y - mu_hat)^2)
  
  # Compute the log likelihood under the assumption that the transformed
  # variable y follows the normal distribution.
  llf <- (lambda - 1.0) * sum(sign(x) * log1p(abs(x))) - n /2.0 * log(sigma_hat_squared)
  
  return(llf)
}


.get_available_transformation_methods <- function(type="all"){
  # Check type
  if(!type %in% c("all", "box_cox", "yeo_johnson")) ..error_reached_unreachable_code(paste0(".get_available_transformation_methods: unspecified type: ", type))
  
  available_transformation_method <- NULL
  
  if(type %in% c("all")){
    available_transformation_method <- c(available_transformation_method, "none")
  }
  
  if(type %in% c("all", "box_cox")){
    available_transformation_method <- c(available_transformation_method,  "box_cox", "box_cox_trim", "box_cox_winsor") 
  }
  
  if(type %in% c("all", "yeo_johnson")){
    available_transformation_method <- c(available_transformation_method, "yeo_johnson", "yeo_johnson_trim", "yeo_johnson_winsor") 
  }
  
  return(available_transformation_method)
}

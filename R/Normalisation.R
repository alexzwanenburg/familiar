add_normalisation_parameters <- function(cl=NULL, feature_info_list, data_obj, settings=NULL, normalisation_method=NULL){
  # Find normalisation parameters and add them to the feature_info_list
  
  # Determine which columns contain feature data
  feature_columns <- get_feature_columns(x=data_obj)
  
  if(is.null(normalisation_method)){
    normalisation_method <- settings$prep$normalisation_method
  }
  
  # Determine transformation parameters by iterating over the features
  upd_list <- lapply(feature_columns, function(ii, feature_info_list, data_obj, normalisation_method){
    
    # Get object corresponding to the current featues
    object <- feature_info_list[[ii]]
    
    # Set transformation parameters
    object@normalisation_parameters <- normalise.get_normalisation_parameters(x=data_obj@data[[ii]], norm_method=normalisation_method)
    
    return(object)
  },
  feature_info_list=feature_info_list,
  data_obj=data_obj,
  normalisation_method=normalisation_method)
  
  # Set names of the updated list
  names(upd_list) <- feature_columns
  
  # Update the list
  feature_info_list[feature_columns] <- upd_list
  
  return(feature_info_list)
}



normalise.get_normalisation_parameters <- function(x, norm_method="standardisation"){
  # Determines normalisation parameters

  # Determine class of vector x
  class_x <- class(x)
  
  # Filter out characters, logical and factor variables - these are not normalised
  if(any(class_x %in% c("character", "logical", "factor"))){
    return(list("norm_method"="none", "norm_shift"=0, "norm_scale"=1))
  }

  # Filter out missing data.
  x <- x[is.finite(x)]
  
  # Filter out numerical or integer variables that have 3 or less unique
  # numerical variables - these are likely contrasts. This also resolves issues
  # surrounding small datasets.
  if(length(unique(x)) <= 3){
    return(list("norm_method"=norm_method, "norm_shift"=0, "norm_scale"=1))
  }
  
  # Apply trimming or winsoring
  if(grepl(pattern="_trim", x=norm_method, fixed=TRUE)){
    x <- trim(x, fraction=0.05)
    
  } else if(grepl(pattern="_winsor", x=norm_method, fixed=TRUE)){
    x <- winsor(x, fraction=0.05)
  }
  
  # For the remainder both shift and scaling parameters are determined
  if(norm_method=="none") {
    # No transformation takes place
    return(list("norm_method"=norm_method, "norm_shift"=0, "norm_scale"=1))

  } else if(norm_method %in% c("standardisation", "standardisation_trim", "standardisation_winsor")){
    # Determine mean (shift) and standard deviation (scale)
    norm_shift <- mean(x)
    norm_scale <- sqrt(sum((x-norm_shift)^2) / length(x))

    # Check for scales which are close to 0
    if(norm_scale < 2 * .Machine$double.eps){ norm_scale <- 1 }

    return(list("norm_method"=norm_method, "norm_shift"=norm_shift, "norm_scale"=norm_scale))

  } else if(norm_method=="quantile"){
    # Determine median (shift) and interquartile range (scale)
    norm_shift <- stats::median(x)
    norm_scale <- unname(diff(stats::quantile(x, probs=c(0.25,0.75))))

    # Check for scales which are close to 0
    if(norm_scale < 2 * .Machine$double.eps){ norm_scale <- 1 }

    return(list("norm_method"=norm_method, "norm_shift"=norm_shift, "norm_scale"=norm_scale))

  } else if(norm_method=="mean_centering") {
    # Determine mean
    norm_shift <- mean(x)
    
    return(list("norm_method"=norm_method, "norm_shift"=norm_shift, "norm_scale"=1.0))
    
  } else if(norm_method %in% c("normalisation", "normalisation_trim", "normalisation_winsor")){
    # Determine max and min values
    x_min <- min(x, na.rm=TRUE)
    x_max <- max(x, na.rm=TRUE)
    
    # Set shift and scale parameters.
    norm_shift <- x_min
    norm_scale <- x_max - x_min
    
    # Check for scales which are close to 0
    if(norm_scale < 2 * .Machine$double.eps){ norm_scale <- 1 }
    
    return(list("norm_method"=norm_method, "norm_shift"=norm_shift, "norm_scale"=norm_scale))
    
  } else{
    ..error_reached_unreachable_code("normalise.get_normalisation_parameters_unknown_normalisation_method")
  }
}



normalise.apply_normalisation <- function(x, norm_param, invert=FALSE){
  # Applies normalisation parameters to input data

  norm_method <- norm_param$norm_method[1]
  if(norm_method=="none"){
    # No normalisation
    return(x)
  } else if(norm_method %in% c("standardisation", "quantile", "standardisation_trim", "standardisation_winsor",
                               "mean_centering", "normalisation", "normalisation_trim", "normalisation_winsor")){

    if(invert){
      y <- x * norm_param$norm_scale[1] + norm_param$norm_shift[1]
    } else {
      # Shift and scale parameters for standard and quantile methods
      y <- (x - norm_param$norm_shift[1]) / (norm_param$norm_scale[1])
    }
    return(y)
  } else {
    ..error_reached_unreachable_code("normalise.apply_normalisation_unknown_normalisation_method")
  }
}


.get_available_normalisation_methods <- function(){
  return(c("none", "standardisation", "standardisation_trim", "standardisation_winsor",
           "quantile", "mean_centering", "normalisation", "normalisation_trim", "normalisation_winsor"))
}


.normalise <- function(x, norm_method, range=NULL){
  # Direct normalisation, no questions asked.
  
  # Obtain normalisation parameters
  normalisation_parameters <- normalise.get_normalisation_parameters(x=x, norm_method=norm_method)
  
  # Apply normalisation parameters.
  y <- normalise.apply_normalisation(x=x, norm_param=normalisation_parameters)
  
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

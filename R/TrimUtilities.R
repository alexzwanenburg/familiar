..trim_functions <- function(){
  
  return(list(
    "coef"=list("FUN"=stats::coef,
                "with_timeout"=FALSE),
    "vcov"=list("FUN"=stats::vcov,
                "with_timeout"=TRUE),
    "summary"=list("FUN"=summary,
                   "with_timeout"=TRUE),
    "varimp"=list("FUN"=mboost::varimp,
                  "with_timeout"=TRUE))
  )
}



.capture_show <- function(object){
  
  # Check that required packages are loaded and installed.
  require_package(object, "show")
  
  # Capture message
  captured_message <- suppressWarnings(tryCatch(utils::capture.output(show(object@model), file=NULL),
                                                error=identity))
  
  # Check if show generated an error.
  if(inherits(captured_message, "error")) return(object)
  
  # Remove call.
  call_present <- startsWith(x=captured_message, prefix="Call:")
  if(any(call_present)){
    call_present <- which(call_present)
    call_present <- c(call_present - 1L, call_present)
    call_present <- setdiff(call_present, 0L)
    
    # Remove lines from message.
    captured_message <- captured_message[-c(call_present)]
  }
  
  # Add to trimmed functions.
  object@trimmed_function <- c(object@trimmed_function,
                               list("show"=captured_message))
  
  return(object)
}



.replace_broken_functions <- function(object, trimmed_object, timeout=60000){
  
  # Find all methods that are trimmable.
  trimmable_methods <- names(..trim_functions())
  
  # Load packages associated with the model into the namespace so that all
  # associated methods may be found.
  require_package(object)
  
  # Find all methods associated with the model object itself.
  class_methods <- .get_class_methods(object@model)
  
  # Special check for S3 methods defined outside of standard packages, or
  # dependent packages. There seems to be an issue with visibility of such
  # methods when the package is only loaded, not attached. I don't want to
  # attach packages because that may alter the search space of the user.
  if(any(class(object@model) %in% c("mboost"))){
    class_methods <- unique(c(class_methods, "varimp"))
  }
  
  # Find those methods that are actually associated with the class.
  trimmable_methods <- trimmable_methods[trimmable_methods %in% class_methods]
  
  # Return trimmed object if there are no associated trimmable functions.
  if(is_empty(trimmable_methods)) return(trimmed_object)
  
  trimmable_methods <- ..trim_functions()[trimmable_methods]
  
  # Generate replacement functions, if required.
  replacement_functions <- lapply(trimmable_methods,
                                  ..replace_broken_function,
                                  object=object,
                                  trimmed_object=trimmed_object,
                                  timeout=timeout)
  
  # Get non-null items and add to existing functions.
  trimmed_object@trimmed_function <- c(
    trimmed_object@trimmed_function,
    replacement_functions[!sapply(replacement_functions, is.null)]
  )
  
  return(trimmed_object)
}



..replace_broken_function <- function(method_list, object, trimmed_object, timeout){
  
  # Isolate FUN and with_timeout:
  FUN <- method_list$FUN
  if(!method_list$with_timeout) timeout <- Inf
  
  # Apply function using the original, untrimmed object.
  initial_info <- do.call_with_handlers_timeout(
    FUN,
    args=list(object@model),
    timeout=timeout,
    additional_packages=object@package
  )

  # If an error occurs or the project times out the required function is not
  # considered implemented for the object.
  if(!is.null(initial_info$error) | initial_info$timeout==TRUE) return(NULL)
  initial_info <- initial_info$value
  
  # Attempt to extract the results from the trimmed object.
  new_info <- do.call_with_handlers(
    FUN,
    args=list(trimmed_object@model)
  )
  
  # If an error occurs, it means that the information required to create the
  # function is no longer available due to object trimming, or recreating the
  # object takes a long time.
  if(!is.null(new_info$error)){
    
    # Check for elements that contain stuff.
    if(is.list(initial_info)){
      # This is the generic check for S3-methods.
      
      if(!is.null(names(initial_info))){
        # Check for call.
        if(is.call(initial_info$call)) initial_info$call <- call("trimmed")
        
        # Check for formula.
        if(inherits(initial_info$formula, "formula")) initial_info$formula <- .replace_environment(initial_info$formula)
        
        # Check for terms.
        if(inherits(initial_info$terms, "terms")) initial_info$terms <- .replace_environment(initial_info$terms)
      }
      
    } else if(is(initial_info, "summary.vglm")){
      # This is the check for the summary.vglm S4 object.
      
      # Check for call.
      if(is.call(initial_info@call)) initial_info@call <- call("trimmed")
      
      # Check for terms.
      initial_info@terms <- lapply(initial_info@terms,
                                   function(x){
                                     if(!inherits(x, "terms")) return(x)
                                     
                                     return(.replace_environment(x))
                                   })
      
      # Check for formula.
      if(inherits(initial_info@misc$formula, "formula")) initial_info@misc$formula <- .replace_environment(initial_info@misc$formula) 
      
      # Replace qr attribute.
      initial_info@qr <- list()
    }
    
    return(initial_info)
  }
  
  return(NULL)
}



.replace_environment <- function(x){
  
  if(".Environment" %in% names(attributes(x))){
    attr(x, ".Environment") <- rlang::base_env()
  }
  
  return(x)
}


.remove <- function(...,
                    envir){
  # Remove a variable from a specific environment.
  
  dots <- unlist(list(...))
  
  for(current_object in dots){
    
    if(is_empty(current_object)) next()
    
    if(exists(current_object, envir=envir, inherits=FALSE)){
      remove(list=current_object, envir=envir)
    }
  }
}



.duplicate_environment <- function(envir, parent=parent.env(envir)) {
  
  # Duplicate environment 
  duplicated_environment <- list2env(as.list.environment(envir,
                                                         all.names=TRUE,
                                                         sorted=FALSE),
                                     parent=parent)
  
  return(duplicated_environment)
}


.change_environment <- function(x, old_env, new_env, recursive=TRUE){
  
  if(is.environment(x)){
    # Iterate over elements in the environment.
    sapply(ls(x), function(y, x, old_env, new_env, recursive){
      
      # Import locally.
      local_x <- get(y, envir=x)
      
      # Change environment on variable.
      local_x <- .change_environment(x=local_x,
                                     old_env=old_env,
                                     new_env=new_env,
                                     recursive=recursive)
      
      # Re-assign to environment.
      assign(y, local_x, envir=x)
      
      return(invisible(NULL))
    },
    x=x,
    old_env=old_env,
    new_env=new_env,
    recursive=recursive)
    
    return(invisible(NULL))
    
  } else if(is.list(x)){
    
    if(length(x) == 0) return(x)
    
    # Determine class (if any).
    object_class <- class(x)
    
    # Skip if x is an extree_data object.
    if(inherits(x, "extree_data")) return(x)
    
    # Iterate over list elements.
    x <- lapply(x, function(x, old_env, new_env, recursive){
      
      # Update the environment of functions.
      if(is.function(x)){
        if(identical(environment(x), old_env)) environment(x) <- new_env
        
      } else if(is.list(x) & recursive){
        x <- .change_environment(x,
                                 old_env=old_env,
                                 new_env=new_env,
                                 recursive=recursive)
      }
      
      return(x)
    },
    old_env=old_env,
    new_env=new_env,
    recursive=recursive)
    
    class(x) <- object_class
    
  } else {
    
    if(is.function(x)){
      if(identical(environment(x), old_env)) environment(x) <- new_env
    }
  }
  
  return(x)
}

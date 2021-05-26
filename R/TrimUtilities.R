..trim_functions <- function(){
  
  return(list("coef"=stats::coef,
              "vcov"=stats::vcov,
              "summary"=summary,
              "varimp"=mboost::varimp))
}



.replace_broken_functions <- function(object, trimmed_object){
  
  # Generate replacement functions, if required.
  replacement_functions <- lapply(..trim_functions(),
                                  ..replace_broken_function,
                                  object=object,
                                  trimmed_object=trimmed_object)
  
  # Return non-null items.
  return(replacement_functions[!sapply(replacement_functions, is.null)])
}



..replace_broken_function <- function(FUN, object, trimmed_object){
  
  quiet(initial_info <- tryCatch(do.call(FUN, list(object@model)),
                                 error=identity))
  
  if(inherits(initial_info, "error")) return(NULL)
  
  quiet(new_info <- tryCatch(do.call(FUN, list(trimmed_object@model)),
                             error=identity))
  
  if(inherits(new_info, "error")){
    
    # Check for elements that contain stuff.
    if(is.list(initial_info)){
      if(!is.null(names(initial_info))){
        # Check for call.
        if(is.call(initial_info$call)) initial_info$call <- call("trimmed")
        
        # Check for formula.
        if(inherits(initial_info$formula, "formula")) initial_info$formula <- .replace_environment(initial_info$formula)
        
        # Check for terms.
        if(inherits(initial_info$terms, "terms")) initial_info$terms <- .replace_environment(initial_info$terms)
      }
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
  
  dots <- list(...)
  
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

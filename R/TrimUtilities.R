..trim_functions <- function(){
  
  return(list("coef"=stats::coef,
              "vcov"=stats::vcov,
              "summary"=summary))
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
  
  if(".Environment" %in% attributes(x)){
    attr(x, ".Environment") <- rlang::empty_env()
  }
  
  return(x)
}

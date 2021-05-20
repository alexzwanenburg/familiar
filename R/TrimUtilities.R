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
  
  if(inherits(new_info, "error")) return(initial_info)
  
  return(NULL)
}

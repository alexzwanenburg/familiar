do.call_strict <- function(what, args, quote = FALSE, envir = parent.frame()){
  # Only pass fully matching arguments. Side effect is that ... is always empty.
  # Use with care in case arguments need to be passed to another function.
  
  # Get arguments that can be passed.
  passable_argument_names <- intersect(names(formals(what)),
                                       names(args))
  
  return(do.call(what, args=args[passable_argument_names], quote=quote, envir=envir))
}



do.call_with_handlers <- function(what, args, quote=FALSE, envir=parent.frame(), muffle=TRUE){

  # Set up the muffler that captures all output.
  muffle_fun <- identity
  if(muffle) muffle_fun <- quiet
  
  # Set up initial warning and error logs. Note that the error log is only
  # written after the tryCatch.
  warn_logs <- error_logs <- NULL
  
  muffle_fun(value <- tryCatch(
    withCallingHandlers(
      do.call(what=what, args=args, envir=envir),
      warning=function(w){
        # Since warn_logs is found in the enclosing frame, we either have to use
        # <<- to access and update this variable. However, this causes CRAN
        # notes, because <<- can be a global assignment, i.e. in the user space.
        # Hence we explicitly use get and assign to update the variable.
        warn_logs <- assign("warn_logs",
                            append(get("warn_logs"), condition_parser(w)),
                            inherits=TRUE)
        
        # This prevents any warnings from being generated outside the function.
        invokeRestart("muffleWarning")
      }),
    error=identity))
  
  
  if(inherits(value, "error")){
    error_logs <- condition_parser(value)
    value <- NULL
  }
  
  return(list("value"=value,
              "warning"=warn_logs,
              "error"=error_logs))
}



condition_parser <- function(x, ...){
  # Based on print.condition
  
  # Find information on the condition message and call.
  condition_message <- conditionMessage(x)
  condition_call <- conditionCall(x)
  
  # Determine condition class.
  if(inherits(x, "error")){
    condition_class <- "error"
    
  } else if(inherits(x, "warning")){
    condition_class <- "warning"
    
  } else {
    condition_class <- class(x)[1L]
  }
  
  # Parse condition to string.
  if(!is.null(call)){
    parsed_condition <- paste0(condition_class, " in ",
                               paste0(deparse1(condition_call), collapse=""),
                               ": ", 
                               condition_message)
  } else {
    parsed_condition <- paste0(condition_class, ": ", 
                               condition_message)
  }
  
  return(parsed_condition)
}



quiet <- function(x) { 
  # Removes all output to console.
  
  sink(nullfile()) 
  on.exit(sink()) 
  
  invisible(utils::capture.output(x, file=nullfile(), type="message"))
} 

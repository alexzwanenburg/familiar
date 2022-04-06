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
  
  # Parse condition to string.
  if(!is.null(call)){
    
    # Get the condition call. Also, just in case, select only the first section
    # that contains the function itself.
    deparsed_condition_call <- deparse1(condition_call)[1]
    
    # We remove the function arguments from the call to prevent leaking data, if
    # any. Identify the first parenthesis that opens the argument section.
    parenthesis_position <- regexpr(text=deparsed_condition_call,
                                    pattern="(",
                                    fixed=TRUE)[1]
    
    # Check if a parenthesis appears in the initial section of the call.
    if(parenthesis_position > -1){
      # Select the function name which appears prior to the parenthesis.
      deparsed_condition_call <- substr(deparsed_condition_call,
                                        start=1L,
                                        stop=parenthesis_position-1L)
      
    } else {
      # If no parenthesis appears, the function call was likely cut off prior to
      # the argument section. We mark this abbreviation.
      deparsed_condition_call <- paste0(deparsed_condition_call, "[...]")
    }
    
    # Add in the deparsed condition call, and combine.
    parsed_condition <- paste0(deparsed_condition_call, ": ", 
                               condition_message)
    
  } else {
    # Set message.
    parsed_condition <- condition_message
  }
  
  return(parsed_condition)
}



quiet <- function(x) { 
  # Removes all output to console.
  
  sink(nullfile()) 
  on.exit(sink()) 
  
  invisible(utils::capture.output(x, file=nullfile(), type="message"))
} 

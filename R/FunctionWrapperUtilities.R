do.call_strict <- function(what, args, quote = FALSE, envir = parent.frame()){
  # Only pass fully matching arguments. Side effect is that ... is always empty.
  # Use with care in case arguments need to be passed to another function.
  
  # Get arguments that can be passed.
  passable_argument_names <- intersect(names(formals(what)),
                                       names(args))
  
  return(do.call(what, args=args[passable_argument_names], quote=quote, envir=envir))
}


quiet <- function(x) { 
  # Removes all output to console.
  
  sink(nullfile()) 
  on.exit(sink()) 
  
  invisible(utils::capture.output(x, file=nullfile(), type="message"))
} 

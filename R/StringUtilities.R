check_column_name <- function(column_name){
  
  # Remove spaces
  column_name <- gsub(pattern=" ", replacement="_", column_name)
  
  # Remove less/equal/greater than signs
  column_name <- gsub(pattern=">=", replacement="_geq_", fixed=TRUE, column_name)
  column_name <- gsub(pattern="<=", replacement="_leq_", fixed=TRUE, column_name)
  column_name <- gsub(pattern="!=", replacement="_neq_", fixed=TRUE, column_name)
  column_name <- gsub(pattern="<", replacement="_l_", fixed=TRUE, column_name)
  column_name <- gsub(pattern=">", replacement="_g_", fixed=TRUE, column_name)
  column_name <- gsub(pattern="=", replacement="_eq_", fixed=TRUE, column_name)
  
  # Remove punctuation
  column_name <- gsub(pattern="[[:punct:]]", replacement="_", column_name)
  
  # Remove starting number
  column_name <- gsub(pattern="^([0-9])", replacement="n_\\1", column_name)
  
  return(column_name)
}



paste_s <- function(...){
  # Function to collapse a series of strings into a summation in the form
  # "element_1, element_2, ..., and element_n".
  dots <- c(...)
  
  if(length(dots) > 2){
    # For more than 2 elements, split into an initial and final section.
    initial_string <- paste0(head(dots, n=length(dots)-2), collapse=", ")
    
    final_string <- paste0(tail(dots, n=2), collapse=" and ")
    
    return(paste0(c(initial_string, final_string), collapse=", "))
    
  } else if(length(dots) == 2){
    # For exactly 2 elements, combine with "and".
    return(paste0(dots, collapse=" and "))
    
  } else {
    
    # For only one element, return as is.
    return(paste0(dots))
  }
}

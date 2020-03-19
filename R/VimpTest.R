vimp.test.learner <- function(method, outcome_type){
  return(NULL)
}

vimp.test.outcome <- function(method, outcome_type){
  
  if(outcome_type %in% c("binomial", "multinomial", "continuous", "count", "survival")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}


vimp.test.param <- function(data_obj, method){
  
  return(list())
}


vimp.test.vimp <- function(data_obj){
 
  return(getEmptyVimp())
}

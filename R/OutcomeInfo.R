#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R


create_outcome_info <- function(settings){
  
  outcome_info <- methods::new("outcomeInfo",
                               name = settings$data$outcome_name,
                               outcome_type = settings$data$outcome_type,
                               outcome_column = settings$data$outcome_column)
  
  if(outcome_info@outcome_type %in% c("binomial", "multinomial")){
    # Set class levels
    outcome_info@levels <- settings$data$class_levels
    
    # Set flag indicating that the outcome is ordinal (currently not enabled)
    outcome_info@ordered <- FALSE
    
    # Set reference level of the outcome
    outcome_info@reference <- settings$data$class_levels[1]
  }
  
  if(outcome_info@outcome_type %in% c("survival", "competing_risk")){
    # Set indicator for censoring
    outcome_info@censored <- settings$data$censoring_indicator
    
    # Set indicator for events
    outcome_info@event <- settings$data$event_indicator
  }
  
  if(outcome_info@outcome_type %in% c("competing_risk")){
    # Set indicator for competing risks
    outcome_info@competing_risk <- settings$data$competing_risk_indicator
  }
  
  return(outcome_info)
}


create_outcome_info_from_data <- function(data){
  # This is typically an outcomeInfo object created at runtime, without access
  # to outcome_info in the global backend, or attached to an object.
  
  outcome_info <- methods::new("outcomeInfo",
                               name = "unset",
                               outcome_type = data@outcome_type,
                               outcome_column = get_outcome_columns(x=data))
  
  if(outcome_info@outcome_type %in% c("binomial", "multinomial")){
    # Set class levels
    outcome_info@levels <- get_outcome_class_levels(x=data)
    
    # Set flag indicating that the outcome is ordinal (currently not enabled)
    outcome_info@ordered <- is.ordered(data@data[[outcome_info@outcome_column]])
    
    # Set reference level of the outcome
    outcome_info@reference <- outcome_info@levels[1]
  }
  
  if(outcome_info@outcome_type %in% c("survival", "competing_risk")){
    # Set indicator for censoring
    outcome_info@censored <- 0
    
    # Set indicator for events
    outcome_info@event <- 1
  }
  
  if(outcome_info@outcome_type %in% c("competing_risk")){
    # Set indicator for competing risks
    outcome_info@competing_risk <- setdiff(unique_na(data@data[[outcome_info@outcome_column[2]]]), c(0, 1))
  }
  
  return(outoutcome_info)
}



.assign_outcome_info_to_global <- function(cl, outcome_info){
  # Put outcome_info in the familiar environment
  assign("outcome_info", outcome_info, envir=familiar_global_env)
  
  # Export outcome_info to the clusters as well
  if(!is.null(cl)){
    parallel::clusterExport(cl=cl, varlist="outcome_info", envir=familiar_global_env)
  }
}



get_outcome_info_from_backend <- function(){
  
  # Retrieve the paths to files and directories
  if(exists("familiar_global_env")){
    if(exists("outcome_info", where=familiar_global_env)){
      data_env <- familiar_global_env
    } else if (exists("outcome_info", where=.GlobalEnv)){
      data_env <- .GlobalEnv
    } else {
      stop("The outcomeInfo object was not found in backend environment.")
    }
  } else if (exists("outcome_info", where=.GlobalEnv)){
    data_env <- .GlobalEnv
  } else {
    stop("The outcomeInfo object was not found in backend environment.")
  }
  
  return(get("outcome_info", envir=data_env))
}

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

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
  
  if(outcome_info@outcome_type %in% c("survival", "multinomial")){
    # Set indicator for 
    
  }
  # 
  # 
  # name = "character",
  # # Outcome type
  # outcome_type = "character",
  # # Class levels of categorical outcomes.
  # levels = "ANY",
  # # Flag for ordinal categorical outcomes.
  # ordered = "logical",
  # # Reference class of categorical outcomes.
  # reference = "character",
  # # Censor indicator for survival outcomes, e.g. alive.
  # censored = "character",
  # # Event indicator for survival outcomes, e.g. recurrent disease.
  # event = "character",
  # # Competing risk indicator(s) for survival outcomes, e.g. dead.
  # competing_risk = "character",
  # # Distribution information of outcome variables.
  # distribution = "ANY",
  # # Data id to which this outcome data belongs.
  # data_id = "integer",
  # # Run id to which this outcome data belongs.
  # run_id = "integer",
  # # Transformation parameters for the outcome data.
  # transformation_parameters = "ANY",
  # # Normalisation parameters for the outcome data.
  # normalisation_parameters = "ANY"
  
}

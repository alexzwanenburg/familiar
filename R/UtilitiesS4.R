#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R


#####is_empty---------------------------------------------------------------------------
setMethod("is_empty", signature(x="NULL"), function(x) {return(TRUE)})

setMethod("is_empty", signature(x="data.table"), function(x){
  if(ncol(x) > 0 & nrow(x) > 0){
    return(FALSE)
    
  } else {
    return(TRUE)
  }
})

setMethod("is_empty", signature(x="character"), function(x){
  if(length(x) == 0){
    return(TRUE)
    
  } else if(all(x == "")){
    return(TRUE)
    
  } else {
    return(FALSE)
  }
})

setMethod("is_empty", signature(x="dataObject"), function(x){
  if(x@delay_loading){
    # Data is not empty until is has been properly loaded
    return(FALSE)
    
  } else if(is.null(x@data)){
    # Data is empty if it is NULL
    return(TRUE)
    
  } else if(nrow(x@data) == 0){
    # Data is empty if the data table has no rows
    return(TRUE)
    
  } else {
    # Data is present otherwise
    return(FALSE)
  }
})

setMethod("is_empty", signature(x="list"), function(x){
  if(length(x) == 0){
    return(TRUE)
    
  } else {
    return(FALSE)
  }
})

#####get_outcome_class_levels---------------------------------------------------
setMethod("get_outcome_class_levels", signature(x="outcomeInfo")){
  if(x@outcome_type %in% c("binomial", "multinomial")){
    return(x@levels)
    
  } else {
    return(character(0))
  }
}

setMethod("get_outcome_class_levels", signature(x="familiarModel")){
  return(get_outcome_class_levels(x@outcome_info))
}

setMethod("get_outcome_class_levels", signature(x="familiarEnsemble")){
  return(get_outcome_class_levels(x@outcome_info))
}

setMethod("get_outcome_class_levels", signature(x="familiarData")){
  return(get_outcome_class_levels(x@outcome_info))
}

setMethod("get_outcome_class_levels", signature(x="familiarCollection")){
  return(get_outcome_class_levels(x@outcome_info))
}

setMethod("get_outcome_class_levels", signature(x="data.table"), function(x, outcome_type){
  return(.get_outcome_class_levels(data=x, outcome_type=outcome_type))
})

setMethod("get_outcome_class_levels", signature(x="dataObject"), function(x){
  return(.get_outcome_class_levels(data=x@data, outcome_type=x@outcome_type))
})

.get_outcome_class_levels <- function(data, outcome_type){
  if(outcome_type %in% c("survival", "continuous", "count", "competing_risk")){
    return(character(0))
    
  } else if(outcome_type %in% c("binomial", "multinomial")){
    return(levels(data[[get_outcome_columns(x=outcome_type)]]))
    
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }
}


#####get_outcome_columns---------------------------------------------------------------
setMethod("get_outcome_columns", signature(x="character"), function(x){
  return(.get_outcome_columns(outcome_type=x))
})

setMethod("get_outcome_columns", signature(x="dataObject"), function(x){
  return(.get_outcome_columns(outcome_type=x@outcome_type))
})

setMethod("get_outcome_columns", signature(x="familiarModel"), function(x){
  return(.get_outcome_columns(outcome_type=x@outcome_type))
})

setMethod("get_outcome_columns", signature(x="familiarEnsemble"), function(x){
  return(.get_outcome_columns(outcome_type=x@outcome_type))
})

# Internal function
.get_outcome_columns <- function(outcome_type){
  # Set outcome columns
  if(outcome_type %in% c("survival")){
    outcome_cols <- c("outcome_time", "outcome_event")
    
  } else if(outcome_type %in% c("binomial", "multinomial", "continuous", "count")){
    outcome_cols <- c("outcome")
    
  } else if(outcome_type == "competing_risk"){
    ..error_outcome_type_not_implemented(outcome_type)
    
  } else {
    ..error_no_known_outcome_type(outcome_type)
  }
  
  return(outcome_cols)
}



#####get_non_feature_columns--------------------------------------------------------
setMethod("get_non_feature_columns", signature(x="character"), function(x, include_repetition_id=TRUE){
  return(.get_non_feature_columns(outcome_type=x, include_repetition_id=include_repetition_id))
})

setMethod("get_non_feature_columns", signature(x="dataObject"), function(x, include_repetition_id=TRUE){
  return(.get_non_feature_columns(outcome_type=x@outcome_type, include_repetition_id=include_repetition_id))
})

setMethod("get_non_feature_columns", signature(x="familiarModel"), function(x, include_repetition_id=TRUE){
  return(.get_non_feature_columns(outcome_type=x@outcome_type, include_repetition_id=include_repetition_id))
})

setMethod("get_non_feature_columns", signature(x="familiarEnsemble"), function(x, include_repetition_id=TRUE){
  return(.get_non_feature_columns(outcome_type=x@outcome_type, include_repetition_id=include_repetition_id))
})

# Internal function
.get_non_feature_columns <- function(outcome_type, include_repetition_id=TRUE){
  # Returns column names in data table which are not features
  
  # Find outcome columns
  outcome_columns <- get_outcome_columns(x=outcome_type)
  
  # Generate the names of the non-feature columns
  if(include_repetition_id){
    return(c(outcome_columns, "subject_id", "cohort_id", "repetition_id"))
  } else {
    return(c(outcome_columns, "subject_id", "cohort_id"))
  }
}


#####get_feature_columns---------------------------------------------------------
setMethod("get_feature_columns", signature(x="data.table"), function(x, outcome_type){
  return(.get_feature_columns(column_names=colnames(x), outcome_type=outcome_type))
})

setMethod("get_feature_columns", signature(x="dataObject"), function(x){
  return(.get_feature_columns(column_names=colnames(x@data), outcome_type=x@outcome_type))
})

# Internal function
.get_feature_columns <- function(column_names, outcome_type){
  
  if(is.null(column_names)){
    # Return 0-length character in case column_names is NULL.
    return(character(0))
    
  } else if(length(column_names) == 0){
    # Return 0-length character in case column_names has no length.
    return(character(0))
    
  } else {
    # Return all column names that are not related to identifiers and outcome.
    return(setdiff(column_names, get_non_feature_columns(x=outcome_type)))
  }
}


#####get_n_features------------------------------------------------------------
setMethod("get_n_features", signature(x="data.table"), function(x, outcome_type){
  return(length(get_feature_columns(x=x, outcome_type=outcome_type)))
})

setMethod("get_n_features", signature(x="dataObject"), function(x){
  return(length(get_feature_columns(x=x)))
})

#####has_feature_data----------------------------------------------------------
setMethod("has_feature_data", signature(x="data.table"), function(x, outcome_type){
  return(get_n_features(x=x, outcome_type=outcome_type) > 0)
})

setMethod("has_feature_data", signature(x="dataObject"), function(x, outcome_type){
  return(get_n_features(x=x) > 0)
})


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
setMethod("get_outcome_class_levels", signature(x="outcomeInfo"), function(x){
  if(x@outcome_type %in% c("binomial", "multinomial")){
    return(x@levels)
    
  } else {
    return(character(0))
  }
})

setMethod("get_outcome_class_levels", signature(x="familiarModel"), function(x){
  return(get_outcome_class_levels(x@outcome_info))
})

setMethod("get_outcome_class_levels", signature(x="familiarEnsemble"), function(x){
  return(get_outcome_class_levels(x@outcome_info))
})

setMethod("get_outcome_class_levels", signature(x="familiarData"), function(x){
  return(get_outcome_class_levels(x@outcome_info))
})

setMethod("get_outcome_class_levels", signature(x="familiarCollection"), function(x){
  return(get_outcome_class_levels(x@outcome_info))
})

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


#####get_class_probability_name-------------------------------------------------
setMethod("get_class_probability_name", signature(x="outcomeInfo"), function(x){
  
  # Check outcome type
  if(x@outcome_type %in% c("binomial", "multinomial")){
    
    # Find class levels
    class_levels <- get_outcome_class_levels(x=x)
    
    # Pass to the method for character strings.
    return(get_class_probability_name(x=class_levels))
    
  } else {
    
    return(NULL)
  }
})

setMethod("get_class_probability_name", signature(x="familiarModel"), function(x){
  return(get_class_probability_name(x=x@outcome_info))
})

setMethod("get_class_probability_name", signature(x="familiarEnsemble"), function(x){
  return(get_class_probability_name(x=x@outcome_info))
})

setMethod("get_class_probability_name", signature(x="familiarData"), function(x){
  return(get_class_probability_name(x=x@outcome_info))
})

setMethod("get_class_probability_name", signature(x="familiarCollection"), function(x){
  return(get_class_probability_name(x=x@outcome_info))
})

setMethod("get_class_probability_name", signature(x="ANY"), function(x){
  # Backup for numeric and logical categories.
  return(get_class_probability_name(x=as.character(x)))
})

setMethod("get_class_probability_name", signature(x="dataObject"), function(x){
  
  outcome_info <- .get_outcome_info(x=x)
  
  return(get_class_probability_name(x=outcome_info))
})

setMethod("get_class_probability_name", signature(x="data.table"), function(x, outcome_type){
  
  if(outcome_type %in% c("binomial", "multinomial")){
    
    # Find class levels
    class_levels <- get_outcome_class_levels(x=x)
    
    # Pass to the method for character strings.
    return(get_class_probability_name(x=class_levels))
    
  } else {
    
    return(NULL)
  }
})

setMethod("get_class_probability_name", signature(x="character"), function(x){
  # Create column names
  class_probability_columns <- check_column_name(column_name=paste0("predicted_class_probability_", x))
  
  return(class_probability_columns)
})


#####get_outcome_name-----------------------------------------------------------
setMethod("get_outcome_name", signature(x="familiarModel"), function(x){
  return(get_outcome_name(x@outcome_info))
})

setMethod("get_outcome_name", signature(x="familiarEnsemble"), function(x){
  return(get_outcome_name(x@outcome_info))
})

setMethod("get_outcome_name", signature(x="familiarData"), function(x){
  return(get_outcome_name(x@outcome_info))
})

setMethod("get_outcome_name", signature(x="familiarCollection"), function(x){
  return(get_outcome_name(x@outcome_info))
})

setMethod("get_outcome_name", signature(x="outcomeInfo"), function(x){
  return(x@name)
})



#####get_outcome_columns--------------------------------------------------------
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
  
  # Find the id-columns
  id_columns <- get_id_columns(include_repetition_id)
  
  # Generate the names of the non-feature columns
  return(c(outcome_columns, id_columns))
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



#####encode_categorical_variables-------------------------------------------------------------
setMethod("encode_categorical_variables", signature(data="dataObject", object="ANY"),
          function(object=NULL, data, encoding_method="effect", drop_levels=TRUE, feature_columns=NULL){
            
            # Obtain feature columns if not provided.
            if(is.null(feature_columns)) feature_columns <- get_feature_columns(data)

            # Find encoding data
            encoding_data <- encode_categorical_variables(data=data@data,
                                                          object=object,
                                                          encoding_method=encoding_method,
                                                          drop_levels=drop_levels,
                                                          feature_columns=feature_columns)
            
            # Update data
            data@data <- encoding_data$encoded_data
            
            return(list("encoded_data"=data, "reference_table"=encoding_data$reference_table))
          })


setMethod("encode_categorical_variables", signature(data="data.table", object="ANY"),
          function(object=NULL, data, encoding_method="effect", drop_levels=TRUE, feature_columns=NULL, outcome_type=NULL){
            
            # Determine columns with factors
            if(is.null(feature_columns) & !is.null(outcome_type)) {
              feature_columns <- get_feature_columns(x=data, outcome_type=outcome_type)
              
            } else if(is.null(feature_columns)){
              feature_columns <- colnames(data)
            }
            
            # Return original if no variables are available.
            if(length(feature_columns) == 0) return(list("encoded_data"=data, "reference_table"=NULL))
            
            # Find column classes
            column_class <- lapply(feature_columns, function(ii, data) (class(data[[ii]])), data=data)
            
            # Identify categorical columns
            factor_columns <- sapply(column_class, function(selected_column_class) (any(selected_column_class %in% c("logical", "character", "factor"))))
            factor_columns <- feature_columns[factor_columns]
            
            # Return original if no categorical variables are available.
            if(length(factor_columns)==0) return(list("encoded_data"=data, "reference_table"=NULL))
            
            # Initialise contrast list and reference list
            contrast_list  <- list()
            reference_list <- list()
            
            # Iterate over columns and encode the categorical variables.
            for(ii in seq_along(factor_columns)){
              # Extract data from current column.
              current_col <- factor_columns[ii]
              x <- data[[current_col]]
              
              # Drop missing levels
              if(drop_levels){
                x <- droplevels(x)
              }
              
              # Get names of levels and number of levels
              level_names <- levels(x)
              level_count <- nlevels(x)
              
              # Apply coding scheme
              if(encoding_method=="effect"){
                # Effect/one-hot encoding
                curr_contrast_list  <- lapply(level_names, function(ii, x) (as.numeric(x==ii)), x=x)
                
                # Check level_names for inconsistencies (i.e. spaces etc)
                contrast_names <- check_column_name(column_name=paste0(current_col, "_", level_names))
                names(curr_contrast_list) <- contrast_names
                
              } else if(encoding_method=="dummy"){
                # Dummy encoding. The first level is used as a reference (0).
                curr_contrast_list  <- lapply(level_names[2:level_count], function(ii, x) (as.numeric(x==ii)), x=x)
                
                # Check level_names for inconsistencies (i.e. spaces etc)
                contrast_names <- check_column_name(column_name=paste0(current_col, "_", level_names[2:level_count]))
                names(curr_contrast_list) <- contrast_names
                
              } else if(encoding_method=="numeric"){
                # Numeric encoding
                browser()
                curr_contrast_list <- list(as.numeric(x))
                
                # Set names
                contrast_names <- current_col
                names(curr_contrast_list) <- contrast_names
                
              } else {
                ..error_reached_unreachable_code("encode_categorical_variables: unknown encoding method encountered.")
              }
              
              # Add list of generated contrast to contrast list
              contrast_list <- append(contrast_list, curr_contrast_list)
              
              # Add data table with reference and contrast names to the reference list
              reference_list[[ii]] <- data.table::data.table("original_name"=current_col,
                                                             "reference_name"=contrast_names)
            }
            
            # Check if the original data set contained any features other than categorical
            # features. Combine original data and contrasts into data table with added
            # contrast data, and get a list with reference names.
            if(length(factor_columns) == ncol(data)){
              contrast_table <- data.table::as.data.table(contrast_list)
              
            } else {
              contrast_table <- cbind(data[, !factor_columns, with=FALSE],
                                      data.table::as.data.table(contrast_list))
            }
            
            # Return as list
            return(list("encoded_data"=contrast_table,
                        "reference_table"=data.table::rbindlist(reference_list)))
          })



#####decode_categorical_variables_vimp------------------------------------------
setMethod("decode_categorical_variables_vimp", signature(object="familiarModel"),
          function(object, vimp_table, method){
            if(!.hasSlot(object, "encoding_reference_table")){
              ..error_reached_unreachable_code("decode_categorical_variables_vimp: the familiarModel object did not inherit the required encoding_reference_table slot.")
            }
            
            return(decode_categorical_variables_vimp(object=object@encoding_reference_table,
                                                     vimp_table=vimp_table,
                                                     method=method))
          })


setMethod("decode_categorical_variables_vimp", signature(object="NULL"),
          function(object, vimp_table, method) return(vimp_table))


setMethod("decode_categorical_variables_vimp", signature(object="data.table"),
          function(object, vimp_table, method){
            # Here object is the data.table with reference data.
            
            # Suppress NOTES due to non-standard evaluation in data.table
            name <- original_name <- score <- NULL

            # Check if any categorical variables are present.
            if(is_empty(object)) return(vimp_table)
            
            # Split vimp_table into categorical and non-categorical features.
            data_non_categorical <- vimp_table[!name %in% object$reference_name, ]
            data_categorical <- vimp_table[name %in% object$reference_name, ]
            
            if(!is_empty(data_categorical)){
              
              # Merge with reference.
              data_categorical <- merge(x=data_categorical, y=object, by.x="name", by.y="reference_name")
              
              # Summarise score by single value according to "method"
              if(method == "max"){
                data_categorical <- data_categorical[, list(score=max(score, na.rm=TRUE)), by=original_name]
                
              } else if(method == "abs_max"){
                data_categorical <- data_categorical[, list(score=max(abs(score), na.rm=TRUE)), by=original_name]
                
              } else if(method == "min"){
                data_categorical <- data_categorical[, list(score=min(score, na.rm=TRUE)), by=original_name]
                
              } else if(method == "abs_min"){
                data_categorical <- data_categorical[, list(score=min(abs(score), na.rm=TRUE)), by=original_name]
                
              } else if(method == "mean"){
                data_categorical <- data_categorical[, list(score=mean(score, na.rm=TRUE)), by=original_name]
                
              } else if(method == "abs_mean"){
                data_categorical <- data_categorical[, list(score=mean(abs(score), na.rm=TRUE)), by=original_name]
                
              } else if(method == "median"){
                data_categorical <- data_categorical[, list(score=stats::median(score, na.rm=TRUE)), by=original_name]
                
              } else if(method == "abs_median"){
                data_categorical <- data_categorical[, list(score=stats::median(abs(score), na.rm=TRUE)), by=original_name]
                
              } else {
                ..error_reached_unreachable_code("decode_categorical_variables_vimp: unknown aggregation method")
              }
              
              # Replace infinite/nan/etc values by NA
              data_categorical[!is.finite(score), "score":=as.double(NA)]
              
              # Change name of original_name column to name
              data.table::setnames(data_categorical, "original_name", "name")
            }
            
            # Combine to single data.table
            vimp_table <- rbind(data_non_categorical,
                                data_categorical)
            
            return(vimp_table)
          })



#####get_placeholder_prediction_table------------------------------------------
setMethod("get_placeholder_prediction_table", signature(object="familiarModel", data="dataObject"),
          function(object, data) return(get_placeholder_prediction_table(object=object, data=data@data)))
            

setMethod("get_placeholder_prediction_table", signature(object="familiarModel", data="data.table"),
          function(object, data){
            
            # Find non-feature columns.
            non_feature_columns <- get_non_feature_columns(object)
            
            # Create the prediction table.
            prediction_table <- data.table::copy(data[, mget(non_feature_columns)])
            
            # Add prediction columns
            if(object@outcome_type %in% c("survival", "continuous", "count", "competing_risk")){
              # For survival and continuous outcomes, a single predicted outcome
              # column is added.
              prediction_table[, "predicted_outcome":=as.double(NA)]
              
            } else if(object@outcome_type %in% c("binomial", "multinomial")){
              # For categorical outcomes, both predicted class and predicted
              # class probabilities are added.
              prediction_table[, "predicted_class":=as.character(NA)]
              
              # Assign factor.
              prediction_table$predicted_class <- factor(prediction_table$predicted_class,
                                                         levels=get_outcome_class_levels(x=object))
              
              # Define probabilities columns
              outcome_probability_columns <- get_class_probability_name(object)
              
              for(ii in seq_along(outcome_probability_columns)) prediction_table[, (outcome_probability_columns[ii]):=as.double(NA)]
              
            } else {
              ..error_no_known_outcome_type(object@outcome_type)
            }
            
            return(prediction_table)
          })

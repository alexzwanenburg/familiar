#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R


#####is_empty---------------------------------------------------------------------------
setMethod("is_empty", signature(x="NULL"), function(x) return(TRUE))

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
    
  } else if(!has_feature_data(x)){
    # Data is empty if there are no features.
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

setMethod("is_empty", signature(x="vector"), function(x) length(x) == 0)

setMethod("is_empty", signature(x="familiarDataElement"), function(x) return(is_empty(x@data)))

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
setMethod("get_non_feature_columns", signature(x="character"), function(x, id_depth="repetition"){
  return(.get_non_feature_columns(outcome_type=x, id_depth=id_depth))
})

setMethod("get_non_feature_columns", signature(x="outcomeInfo"), function(x, id_depth="repetition"){
  return(.get_non_feature_columns(outcome_type=x@outcome_type, id_depth=id_depth))
})

setMethod("get_non_feature_columns", signature(x="dataObject"), function(x, id_depth="repetition"){
  return(.get_non_feature_columns(outcome_type=x@outcome_type, id_depth=id_depth))
})

setMethod("get_non_feature_columns", signature(x="familiarVimpMethod"), function(x, id_depth="repetition"){
  return(.get_non_feature_columns(outcome_type=x@outcome_type, id_depth=id_depth))
})

setMethod("get_non_feature_columns", signature(x="familiarModel"), function(x, id_depth="repetition"){
  return(.get_non_feature_columns(outcome_type=x@outcome_type, id_depth=id_depth))
})

setMethod("get_non_feature_columns", signature(x="familiarEnsemble"), function(x, id_depth="repetition"){
  return(.get_non_feature_columns(outcome_type=x@outcome_type, id_depth=id_depth))
})

# Internal function
.get_non_feature_columns <- function(outcome_type, id_depth="repetition"){
  # Returns column names in data table which are not features
  
  # Find outcome columns
  outcome_columns <- get_outcome_columns(x=outcome_type)
  
  # Find the id-columns
  id_columns <- get_id_columns(id_depth=id_depth)
  
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


#####get_unique_row_names------------------------------------------------------
setMethod("get_unique_row_names", signature(x="data.table"), function(x, include_batch=NULL, include_series=NULL){
  if(is_empty(x)) return(character(0L))
  
  if(is.null(include_batch)){
    # Determine if batch identifiers should be included, i.e. the same sample
    # identifiers appear in different batches.
    include_batch <- any(unique(x[, mget(get_id_columns(id_depth="sample"))])[, list("n"=.N), by=mget(get_id_columns(single_column="sample"))]$n > 1)
  }
  
  if(is.null(include_series)){
    # Determine if series identifiers should be included, i.e. any unique sample
    # has multiple series.
    include_series <- any(unique(x[, mget(get_id_columns(id_depth="series"))])[, list("n"=.N), by=mget(get_id_columns(id_depth="sample"))]$n > 1)
  }
  
  # Get sample names.
  row_names <- as.character(x[[get_id_columns(single_column="sample")]])
  
  if(any(c(include_batch, include_series))){
    add_string <- " ("
    
    if(include_batch) add_string <- paste0(add_string, x[[get_id_columns(single_column="batch")]])
    if(include_batch & include_series) add_string <- paste0(add_string, "; ")
    if(include_series) add_string <- paste0(add_string, x[[get_id_columns(single_column="series")]])
    
    row_names <- paste0(row_names, add_string, ")")
  }
  
  return(row_names)
})

setMethod("get_unique_row_names", signature(x="dataObject"), function(x, include_batch=NULL, include_series=NULL){
  return(get_unique_row_names(x=x@data, include_batch=include_batch, include_series=include_series))
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
          function(object, data) return(get_placeholder_prediction_table(object=object@outcome_info, data=data@data)))

setMethod("get_placeholder_prediction_table", signature(object="familiarModel", data="data.table"),
          function(object, data) return(get_placeholder_prediction_table(object=object@outcome_info, data=data)))


setMethod("get_placeholder_prediction_table", signature(object="familiarEnsemble", data="dataObject"),
          function(object, data) return(get_placeholder_prediction_table(object=object@outcome_info, data=data@data)))

setMethod("get_placeholder_prediction_table", signature(object="familiarEnsemble", data="data.table"),
          function(object, data) return(get_placeholder_prediction_table(object=object@outcome_info, data=data)))


setMethod("get_placeholder_prediction_table", signature(object="outcomeInfo", data="dataObject"),
          function(object, data) return(get_placeholder_prediction_table(object=object, data=data@data)))

setMethod("get_placeholder_prediction_table", signature(object="outcomeInfo", data="data.table"),
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




#####bootstrapper---------------------------------------------------------------
setMethod("bootstrapper", signature(data="dataObject"),
          function(data, n=NULL, alpha=0.05, FUN, cl=NULL, verbose=FALSE, ...){

            # Determine n.
            if(is.null(n)) n <- ceiling(20 / alpha)
            
            if(is_empty(data)) return(data)
            
            bootstrapped_data <- fam_lapply(cl=cl,
                                            X=seq_len(n),
                                            FUN=function(ii, data, FUN2, dots2){
                                              
                                              # Create a random subsample of the data.
                                              data <- get_bootstrap_sample(data=data)
                                              
                                              # Execute the function
                                              results <- do.call(FUN2, args=c(list("data"=data),
                                                                              dots2))
                                              
                                              # Add bootstrap id
                                              if(data.table::is.data.table(results)){
                                                results[, "bootstrap_id":=ii]
                                                
                                              } else if(is.null(results)) {
                                                results <- NULL
                                                
                                              } else {
                                                results <- list("results"=results,
                                                                "bootstrap_id"=ii)
                                              }
                                              
                                              return(results)
                                            },
                                            data=data,
                                            FUN2=FUN,
                                            dots2=list(...),
                                            progress_bar=verbose)
            
            # Combine data.tables, if bootstrapped_data is indeed a list of
            # data.tables.
            if(any(sapply(bootstrapped_data, data.table::is.data.table))){
              bootstrapped_data <- data.table::rbindlist(bootstrapped_data, use.names=TRUE)
            }
            
            return(bootstrapped_data)
          })



setMethod("bootstrapper", signature(data="data.table"),
          function(data, n=NULL, alpha=0.05, FUN, cl=NULL, verbose=FALSE, ...){
            
            # Determine n.
            if(is.null(n)) n <- ceiling(20 / alpha)
            
            if(is_empty(data)) return(data)
            
            bootstrapped_data <- fam_lapply(cl=cl,
                                            X=seq_len(n),
                                            FUN=function(ii, data, FUN2, dots2){
                                              
                                              # Create a random subsample of the data.
                                              data <- get_bootstrap_sample(data=data)
                                              
                                              # Execute the function
                                              results <- do.call(FUN2, args=c(list("data"=data),
                                                                              dots2))
                                              
                                              # Add bootstrap id
                                              if(data.table::is.data.table(results)){
                                                results[, "bootstrap_id":=ii]
                                                
                                              } else if(is.null(results)) {
                                                results <- NULL
                                                
                                              } else {
                                                results <- list("results"=results,
                                                                "bootstrap_id"=ii)
                                              }
                                              
                                              return(results)
                                            },
                                            data=data,
                                            FUN2=FUN,
                                            dots2=list(...),
                                            progress_bar=verbose)
            
            # Combine data.tables, if bootstrapped_data is indeed a list of
            # data.tables.
            if(any(sapply(bootstrapped_data, data.table::is.data.table))){
              bootstrapped_data <- data.table::rbindlist(bootstrapped_data, use.names=TRUE)
            }
            
            return(bootstrapped_data)
          })



#####get_bootstrap_sample------------------------------------------------------
setMethod("get_bootstrap_sample", signature(data="dataObject"),
          function(data, seed=NULL, ...){
            
            # Set seed for reproducible results.
            if(!is.null(seed)){
              if(is.finite(seed)) set.seed(seed)
            }
            
            if(.as_preprocessing_level(data) > "none"){
              # Indicating that some preprocessing has taken please.
              
              # Bootstrap the data element.
              data@data <- get_bootstrap_sample(data=data@data)
              
            } else if(length(data@sample_set_on_load) > 0){
              
              if(data.table::is.data.table(data@sample_set_on_load)){
                data@sample_set_on_load <- get_bootstrap_sample(data=data@sample_set_on_load)
                
              } else {
                # Reshuffle the samples -- This is for backward compatibility.
                data@sample_set_on_load <- fam_sample(x=unique(data@sample_set_on_load),
                                                      size=length(data@sample_set_on_load),
                                                      replace=TRUE)
              }
              
            } else {
              ..error_reached_unreachable_code("get_boostrap_sample,dataObject: could not identify a suitable method for bootstrapping.")
            }
            
            return(data)
          })

setMethod("get_bootstrap_sample", signature(data="data.table"),
          function(data, seed=NULL, ...){
            
            # Set seed for reproducible results.
            if(!is.null(seed)){
              if(is.finite(seed)) set.seed(seed)
            }
            
            # Find identifier columns at the sample level, i.e. excluding
            # repetitions and series.
            id_columns <- intersect(get_id_columns(id_depth="sample"), colnames(data))
            
            if(length(id_columns) == 0){
              # Sample rows.
              row_ids <- fam_sample(x=seq_len(nrow(data)),
                                    size=nrow(data),
                                    replace=TRUE)
              
              # Create a subset.
              data <- data[row_ids, ]
              
            } else {
              # List unique samples.
              id_table <- unique(data[, mget(id_columns)])

              # Sample the unique rows of the identifier table.
              row_ids <- fam_sample(x=seq_len(nrow(id_table)),
                                    size=nrow(id_table),
                                    replace=TRUE)
              
              # Create subsample.
              id_table <- id_table[row_ids, ]
              
              # Merge the subsampled identifier table with the data (removing
              # duplicates).
              data <- merge(x=id_table,
                            y=unique(data),
                            by=id_columns,
                            all=FALSE,
                            allow.cartesian=TRUE)
            }
            
            return(data)
          })

setMethod("get_bootstrap_sample", signature(data="NULL"), function(data, seed=NULL, ...) return(NULL))



#####universal_extractor--------------------------------------------------------
setMethod("universal_extractor", signature(object="familiarEnsemble"),
          function(object,
                   FUN,
                   compute_model_data=FALSE,
                   compute_model_ci=FALSE,
                   compute_ensemble_ci=TRUE,
                   cl=NULL,
                   verbose=FALSE,
                   message_indent=0L,
                   ...){
            
            # Only pass cl for individual model extraction if
            # compute_model_ci is TRUE.
            if(compute_model_ci){
              cl_model <- NULL
              cl_internal <- cl
              
            } else {
              cl_model <- cl
              cl_internal <- NULL
            }
            
            if(verbose & compute_model_data) logger.message("Performing computations for models in the ensemble.",
                                                            indent=message_indent)
            
            if(compute_model_data){
              
              # Individual model extraction
              individual_model_data <- fam_lapply(cl=cl_model, 
                                                  X=object@model_list,
                                                  FUN=function(object, FUN2, determine_ci, cl2, dots2, verbose=FALSE, message_indent=0L){
                                                    
                                                    # Load the object if it is not a familiarModel, but a character.
                                                    if(is.character(object)) object <- load_familiar_object(object)
                                                    
                                                    # Execute the function that computes the data.
                                                    results <- do.call(FUN2, args=c(list("determine_ci"=determine_ci,
                                                                                         "object"=object,
                                                                                         "cl"=cl2,
                                                                                         "verbose"=verbose,
                                                                                         "message_indent"=message_indent + 1L),
                                                                                    dots2))
                                                    
                                                    # Check that the results are not empty.
                                                    if(is_empty(results)) return(NULL)
                                                    
                                                    # Add model name.
                                                    if(data.table::is.data.table(results)){
                                                      # In case the results are a data.table
                                                      results <- add_model_name(data=results, object=object)
                                                      
                                                    } else if(is.list(results)){
                                                      # In case the results are a list, including data.tables. First
                                                      # determine the names of the list elements, and then add model
                                                      # names.
                                                      result_names <- names(results)
                                                      results <- lapply(results, add_model_name, object=object)
                                                      
                                                      # Restore names.
                                                      names(results) <- result_names
                                                    }
                                                    
                                                    return(results)
                                                    
                                                  },
                                                  FUN2=FUN,
                                                  dots2=list(...),
                                                  determine_ci=compute_model_ci,
                                                  cl2=cl_internal,
                                                  verbose=verbose,
                                                  message_indent=message_indent,
                                                  progress_bar=verbose)
              
              # Parse to single list.
              if(any(sapply(individual_model_data, data.table::is.data.table))){
                # Check if the results are a list of data.tables.
                individual_model_data <- data.table::rbindlist(individual_model_data, use.names=TRUE)
                
              } else if(all(sapply(individual_model_data, is_empty))){
                # Check if all entries are empty.
                individual_model_data <- NULL
                
              } else {
                # Check for nested data tables.
                element_names <- unique(unlist(lapply(individual_model_data, names)))
                
                # Aggregate data produced by the models.
                individual_model_data <- lapply(element_names, function(element, model_data){
                  
                  if(any(sapply(model_data,
                                function(list_entry, element) data.table::is.data.table(list_entry[[element]]),
                                element=element))) {
                    # Check if there is any data.table for the list element, and
                    # extract a combined data.table if so.
                    return(rbind_list_list(model_data, element))
                    
                  } else if(all(sapply(model_data,
                                       function(list_entry, element) is_empty(list_entry[[element]]),
                                       element=element))) {
                    # Check if there is any data for the list element, and return
                    # a NULL if not.
                    return(NULL)
                    
                  } else if(all(sapply(model_data,
                                       function(list_entry, element) length(list_entry[[element]]) <= 1,
                                       element=element))) {
                    
                    # Find the unique value.
                    value <- unique_na(unlist(sapply(model_data,
                                                     function(list_entry, element) list_entry[[element]],
                                                     element=element)))
                    
                    if(length(value) > 1){
                      ..error_reached_unreachable_code("universal_extractor,familiarEnsemble: cannot aggregate data because of multiple unique values.")
                    }
                    
                    return(value)
                    
                  } else {
                    ..error_reached_unreachable_code("universal_extractor,familiarEnsemble: cannot aggregate data because of unknown reasons.")
                  }
                  
                }, model_data=individual_model_data)
                
                # Set names.
                names(individual_model_data) <- element_names
              }
            } else {
              # Data is not computed for individual models.
              individual_model_data <- NULL
            }
            
            if(verbose) logger.message("Performing computations for the ensemble itself.",
                                       indent=message_indent)
            
            # Compute data for the ensemble itself.
            ensemble_model_data <- do.call(FUN, args=c(list("determine_ci"=compute_ensemble_ci,
                                                            "object"=object,
                                                            "cl"=cl,
                                                            "verbose"=verbose,
                                                            "message_indent"=message_indent + 1L),
                                                       list(...)))
            
            # Check that the results are not empty.
            if(is_empty(ensemble_model_data)) ensemble_model_data <- NULL
            
            # Add model name.
            if(data.table::is.data.table(ensemble_model_data)){
              # In case the results are a data.table
              ensemble_model_data <- add_model_name(data=ensemble_model_data, object=object)
              
            } else if(is.list(ensemble_model_data)){
              # In case the results are a list, including data.tables. First
              # determine the names of the list elements, and then add model
              # names.
              ensemble_model_data_names <- names(ensemble_model_data)
              ensemble_model_data <- lapply(ensemble_model_data, add_model_name, object=object)
              
              # Restore names.
              names(ensemble_model_data) <- ensemble_model_data_names
            }
            
            return(list("individual"=individual_model_data,
                        "ensemble"=ensemble_model_data))
          })


#####fam_sample-----------------------------------------------------------------
setMethod("fam_sample", signature(x="ANY"),
          function(x, size=NULL, replace=FALSE, prob=NULL, ...){
            # This function prevents the documented behaviour of the sample
            # function, where if x is positive, numeric and only has one
            # element, it interprets x as a series of x, i.e. x=seq_len(x).
            # That's bad news if x is a sample identifier.
            
            # Set size if it is unset.
            if(is.null(size)) size <- length(x)
            
            if(length(x) == 1){
              
              # Check that size is not greater than 1, if items are to be drawn
              # without replacement.
              if(!replace & size > 1){
                stop("cannot take a sample larger than the population when 'replace = FALSE'")
              }
              
              return(rep_len(x=x, length.out=size))
              
            } else {
              # If x is a vector, array or list with multiple elements, then all
              # of the above is not an issue, and we can make use of sample.
              
              return(sample(x=x, size=size, replace=replace, prob=prob))
            }
          })


setMethod("fam_sample", signature(x="data.table"),
          function(x, size=NULL, replace=FALSE, prob=NULL, ...){
            # This function prevents the documented behaviour of the sample
            # function, where if x is positive, numeric and only has one
            # element, it interprets x as a series of x, i.e. x=seq_len(x).
            # That's bad news if x is a sample identifier.
            
            # Suppress NOTES due to non-standard evaluation in data.table
            ..prob <- NULL
            
            # Make a local copy of x to prevent changing by reference.
            x <- data.table::copy(x)
            
            # Get id columns
            id_columns <- get_id_columns("sample")
            
            # Check if prob equals NULL, and set to FALSE if it is.
            if(is.null(prob)) prob <- FALSE
            
            # Check that batch_id and sample_id columns are present.
            if(!all(id_columns %in% colnames(x))){
              ..error_reached_unreachable_code("fam_sample,data.table: batch_id or sample_id columns are not present in x.")
            }
            
            # Check that the prob column is present.
            if(is.logical(prob)){
              if(prob & !"prob" %in% colnames(x)){
                ..error_reached_unreachable_code("fam_sample,data.table: prob column is not present in x.")
              }
              
              # Select unique samples.
              x <- unique(x, by=id_columns)
              
              # Set probability to 1.0 for all samples.
              if(!prob) x[, "prob":=1.0]
              
            } else {
              # Assume that the prob argument is provided as a separate vector
              if(length(x) == nrow(x)){
                # In this case, assume that prob can be inserted as a new
                # column.
                
                # Assign prob.
                x[, "prob":=..prob]
                
                # Select unique samples.
                x <- unique(x, by=id_columns)
                
              } else {
                ..error_reached_unreachable_code("fam_sample,data.table: the length of prob does not equal the number of instances in x.")
              }
            }
            
            # Set size if it is unset.
            if(is.null(size)) size <- nrow(x)
            
            # Sample x.
            if(nrow(x) == 1){
              
              # Check that size is not greater than 1, if items are to be drawn
              # without replacement.
              if(!replace & size > 1){
                stop("cannot take a sample larger than the population when 'replace = FALSE'")
              }
              
              # Get row-ids
              row_id <- rep_len(x=1L, length.out=size)
              
              # Create output table y.
              y <- x[row_id, mget(id_columns)]
              
            } else {
              # If x has a multiple instances, then all of the above is not an
              # issue, and we can use the standard implementation of sample.
              
              # Use default prob=NULL if all probabilities in x are 1.0.
              if(all(x$prob == 1.0)){
                prob <- NULL
                
              } else {
                prob <- x$prob
              }
              
              # Get sampled row identifiers.
              row_id <- sample(x=seq_len(nrow(x)),
                               size=size,
                               replace=replace,
                               prob=prob)
              
              # Create output table y.
              y <- x[row_id, mget(id_columns)]
            }
            
            return(y)
          })

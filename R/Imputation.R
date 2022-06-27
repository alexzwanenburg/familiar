add_imputation_info <- function(cl=NULL, feature_info_list, data_obj, settings, verbose=TRUE){
  
  # Add simple imputation info
  feature_info_list <- impute.add_simple_imputation_info(cl=cl,
                                                         feature_info_list=feature_info_list,
                                                         data_obj=data_obj,
                                                         verbose = verbose & settings$prep$imputation_method == "simple")
  
  if(settings$prep$imputation_method == "lasso"){
    # Process data with simple imputation
    local_train_data  <- impute.impute_simple(cl=cl, data_obj=data_obj, feature_info_list=feature_info_list)
    
    # Add lasso-regression info
    feature_info_list <- impute.add_lasso_imputation_info(cl=cl,
                                                          feature_info_list=feature_info_list,
                                                          data_obj=data_obj,
                                                          uncensored_data_obj=local_train_data,
                                                          verbose=verbose)
  }
  
  return(feature_info_list)
}



impute.add_lasso_imputation_info <- function(cl=NULL, feature_info_list, data_obj, uncensored_data_obj, verbose=TRUE){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  name <- NULL
  
  # Check that the glmnet package is installed.
  require_package(x="glmnet",
                  purpose="to impute data using lasso regression")
  
  .add_info <- function(feature, feature_info_list, data_obj, uncensored_data_obj){

    # local featureInfo object
    object <- feature_info_list[[feature]]
    
    # Select valid data for training
    valid_entries <- is_valid_data(data_obj@data[[feature]])
    train_data <- uncensored_data_obj@data[valid_entries, get_feature_columns(x=uncensored_data_obj), with=FALSE]
    train_data <- train_data[, (feature):=NULL]
    
    outcome_data  <- uncensored_data_obj@data[valid_entries, ][[feature]]
    
    # Determine the relevant distribution for the model
    if(object@feature_type == "numeric"){
      distribution <- "gaussian"
    } else {
      distribution <- "multinomial"
    }
    
    # Use effect coding to convert categorical data into encoded data
    encoded_data <- encode_categorical_variables(data=train_data,
                                                 encoding_method="dummy",
                                                 drop_levels=FALSE)

    # Extract data table with contrasts.
    train_data <- encoded_data$encoded_data
    
    # Perform a cross-validation to derive a optimal lambda. This may rarely
    # fail if y is numeric but does not change in a particular fold. In this
    # case, skip.
    lasso_model <- tryCatch(glmnet::cv.glmnet(x=as.matrix(train_data),
                                              y=outcome_data,
                                              family=distribution,
                                              alpha=1,
                                              standardize=FALSE,
                                              nfolds=min(nrow(train_data), 20),
                                              parallel=FALSE),
                            error=identity)
    
    if(inherits(lasso_model, "error")) return(object)
    
    # Determine the features with non-zero coefficients. We want to have the
    # minimal required support for the model to minimise the effort that is
    # required for external model validation. GLMNET by definition wants to have
    # the exact same complete input space, even though but a few features are
    # used. That is a bad idea for portability. Therefore we will be jumping
    # through some hoops to make a model with minimum support.
    if(distribution=="multinomial"){
      # Read coefficient lists
      coef_list <- coef(lasso_model, s="lambda.1se")
      
      # Parse into matrix and retrieve row names
      coef_mat <- sapply(coef_list, as.matrix)
      rownames(coef_mat) <- dimnames(coef_list[[1]])[[1]]
      
      # Calculate score
      score <- apply(abs(coef_mat), 1, max)
    } else {
      # Read coefficient matrix
      coef_mat <- as.matrix(coef(lasso_model, s="lambda.1se"))
      
      # Calculate score
      score <- abs(coef_mat)[,1]
    }
    
    # Parse score to data.table
    vimp_table <- data.table::data.table("score"=score, "name"=names(score))
  
    # Throw out the intercept and elements with 0.0 coefficients
    vimp_table <- vimp_table[name!="(Intercept)" & score!=0.0]
    
    # Find the original names
    vimp_table <- decode_categorical_variables_vimp(object=encoded_data$reference_table,
                                                    vimp_table=vimp_table,
                                                    method="abs_max")
    
    # Check that the optimal model complexity lambda is connected to at least
    # one feature. If not, we have to use the simple estimate.
    if(is_empty(vimp_table)) return(object)
    
    # Derive required features
    required_features <- vimp_table$name
    
    # Create a data set with minimal support
    train_data <- uncensored_data_obj@data[valid_entries, required_features, with=FALSE]
    
    # Use effect coding to convert categorical data into encoded data
    encoded_data <- encode_categorical_variables(data=train_data,
                                                 encoding_method="dummy",
                                                 drop_levels=FALSE)
    
    # Extract data table with contrasts.
    train_data <- encoded_data$encoded_data

    # Check the number of columns in train_data. glmnet wants at least two
    # columns.
    if(ncol(train_data) == 1) train_data[, "bogus__variable__":=0.0]
    
    # Determine the lambda.1se and train a small model at this lambda
    lasso_model <- glmnet::glmnet(x = as.matrix(train_data),
                                  y = outcome_data,
                                  family = distribution,
                                  lambda = lasso_model$lambda.1se,
                                  standardize = FALSE)
    
    # Remove extraneous information from the model.
    lasso_model <- ..trim_glmnet(lasso_model)
    
    # Add lasso model and required features to the information.
    if(!is.list(object@imputation_parameters)){
      object@imputation_parameters <- list("lasso_model" = list(lasso_model),
                                           "required_features" = required_features)
    } else {
      object@imputation_parameters$lasso_model <- list(lasso_model)
      object@imputation_parameters$required_features <- required_features
    }

    return(object)
  }
  
  # Determine feature columns
  feature_columns <- get_feature_columns(x=data_obj)

  # Add in check so that there is more one than feature available. We cannot define model using only an outcome column.
  if(length(feature_columns) > 1){
    
    upd_list <- fam_lapply(cl=cl,
                           assign=NULL,
                           X=feature_columns,
                           FUN=.add_info,
                           progress_bar=verbose,
                           feature_info_list=feature_info_list,
                           data_obj=data_obj,
                           uncensored_data_obj=uncensored_data_obj,
                           chopchop=TRUE)
    
    # Set names of the update object list
    names(upd_list) <- feature_columns
    
    # Update feature_info_list
    feature_info_list[feature_columns] <- upd_list
  }

  return(feature_info_list)
}



impute.impute_lasso <- function(cl=NULL, feature_info_list, data_obj, uncensored_data_obj, censored_features=NULL){
  
  # Suppress NOTES due to non-standard evaluation in data.table.
  values <- NULL
  
  # Check that the glmnet package is installed.
  require_package(x="glmnet",
                  purpose="to impute data using lasso regression")
  
  .get_imputed_values <- function(feature, data_obj, uncensored_data_obj, feature_info_list){

    # featureInfo object for the current feature.
    object <- feature_info_list[[feature]]
    
    # Get data for the current feature.
    y <- data_obj@data[[feature]]
    
    # Find the censored entries for the current data set.
    censored_entries <- !is_valid_data(y)
    
    # Replace missing values
    if(is_empty(object@imputation_parameters$lasso_model) | is.null(object@imputation_parameters$required_features)){
      # Check if required entries are present in the featureInfo object and
      # insert common value if not.
      y[censored_entries] <- object@imputation_parameters$common_value
      
    } else if(!all(object@imputation_parameters$required_features %in% colnames(uncensored_data_obj@data))) {
      # Check if the required columns are present in the data, and insert the
      # common value if not.
      y[censored_entries] <- object@imputation_parameters$common_value
      logger.warning(paste0("Imputation: not all required features were found for imputation of the ", feature, " feature. Missing are: ",
                            paste(setdiff(object@imputation_parameters$required_features, colnames(uncensored_data_obj@data)), collapse=", ")))
      
    } else {
      # Proceed with imputation using the lasso model
      
      # Create a data set with minimal support
      validation_data <- uncensored_data_obj@data[censored_entries,
                                                  mget(object@imputation_parameters$required_features)]
      
      # Use effect coding to convert categorical data into encoded data.
      encoded_data <- encode_categorical_variables(data=validation_data,
                                                   encoding_method="dummy",
                                                   drop_levels=FALSE)
      
      # Extract data table with contrasts.
      validation_data <- encoded_data$encoded_data
      
      # Check if the validation data has two or more columns
      if(ncol(validation_data) == 1) validation_data[, "bogus__variable__":=0.0]

      # Get the type of response for glmnet predict
      response_type <- ifelse(object@feature_type == "numeric", "response", "class")
      
      # Impute values
      imputed_values <- lapply(object@imputation_parameters$lasso_model, function(model_object, validation_data, response_type){

        # Skip if NULL.
        if(is.null(model_object)) return(NULL)
        
        if(is.list(model_object) && !inherits(model_object, "glmnet")) model_object <- model_object[[1]]
        
        # Required features
        if(inherits(model_object$beta, "dgCMatrix")){
          required_features <- model_object$beta@Dimnames[[1]]
          
        } else {
          required_features <- unique(unlist(lapply(model_object$beta, function(x) (x@Dimnames[[1]]))))
        }
        
        # Predict values.
        imputed_value_table <- data.table::data.table("values"=drop(predict(object=model_object,
                                                                            newx=as.matrix(validation_data[, mget(required_features)]),
                                                                            type=response_type)))
        
        # Add temporary sample id
        imputed_value_table[, "temp_sample_id":=.I]
        
        return(imputed_value_table)
      }, validation_data=validation_data, response_type=response_type)

      # Concatenate to single table
      imputed_values <- data.table::rbindlist(imputed_values)
      
      # The desired output is determined by the feature type (numeric or
      # factor).
      if(!is_empty(imputed_values)){
        if(object@feature_type == "numeric"){
          y[censored_entries] <- imputed_values[,
                                                list("values"=mean(values, na.rm=TRUE)),
                                                by="temp_sample_id"]$values
          
        } else {
          y[censored_entries] <- imputed_values[,
                                                list("values"=get_mode(values)),
                                                by="temp_sample_id"]$values
        }
        
      } else {
        y[censored_entries] <- object@imputation_parameters$common_value
      }
      
    }
    
    return(y)
  }

  # Check if data is present
  if(is_empty(data_obj)) return(data_obj)
  
  # Check if censored_features are provided as input.
  if(is.null(censored_features)){
    # Define feature columns
    feature_columns <- get_feature_columns(x=data_obj)
    
    # Determine which columns has missing entries
    censored_features <- feature_columns[sapply(feature_columns, function(ii, data_obj) (!all(is_valid_data(data_obj@data[[ii]]))), data_obj=data_obj)]
  }
  
  # Skip if there are no missing values.
  if(length(censored_features) == 0) return(data_obj)
  
  # Define the replacement list
  replacement_list <- fam_lapply(cl=cl,
                                 assign=NULL,
                                 X=censored_features,
                                 FUN=.get_imputed_values,
                                 progress_bar=FALSE,
                                 data_obj=data_obj,
                                 uncensored_data_obj=uncensored_data_obj,
                                 feature_info_list=feature_info_list,
                                 chopchop=TRUE)
  
  # Set replacement names
  names(replacement_list) <- censored_features
  
  # Update columns in the dataObject
  data_obj <- update_with_replacement(data=data_obj, replacement_list=replacement_list)

  return(data_obj)
}



impute.add_simple_imputation_info <- function(cl=NULL, feature_info_list, data_obj, settings, verbose=TRUE){
  
  # Local function
  .add_info <- function(feature, feature_info_list, data_obj){
    # Local featureInfo object
    object <- feature_info_list[[feature]]
    
    # Find all available data for the current feature
    x <- data_obj@data[[feature]][is_valid_data(data_obj@data[[feature]])]
    
    if(object@feature_type == "numeric"){
      # Select median values
      common_value <- stats::median(x)
      
    } else if(object@feature_type == "factor"){
      # Select the mode as the replacement value
      common_value <- get_mode(x)
      
    } else {
      logger.stop(paste0("Pre-processing: invalid data type encountered in data: \'", object@feature_type, "\'"))
    }
    
    # Add common value to the information
    if(!is.list(object@imputation_parameters)){
      object@imputation_parameters <- list("common_value"=common_value)
    } else {
      object@imputation_parameters$common_value <- common_value
    }
    
    return(object)
  }
  
  # Determine feature columns
  feature_columns <- get_feature_columns(x=data_obj)

  # Add simple imputation information
  upd_list <- fam_lapply(cl=cl,
                         assign=NULL,
                         X=feature_columns,
                         FUN=.add_info,
                         progress_bar=verbose,
                         feature_info_list=feature_info_list,
                         data_obj=data_obj,
                         chopchop=TRUE)
  
  # Set names of the update object list
  names(upd_list) <- feature_columns
  
  # Update feature_info_list
  feature_info_list[feature_columns] <- upd_list

  return(feature_info_list)
}


impute.impute_simple <- function(cl=NULL, data_obj, feature_info_list, censored_features=NULL){
  # Performs simple imputation of features
  
  .get_imputed_values <- function(feature, data_obj, feature_info_list){
    
    # Get data for the current feature
    x <- data_obj@data[[feature]]
    
    # Replace missing values
    x[!is_valid_data(x)] <-  feature_info_list[[feature]]@imputation_parameters$common_value
    
    return(x)
  }
  
  
  # Check if data is present
  if(is_empty(data_obj)){
    return(data_obj)
  }

  if(is.null(censored_features)){
    # Define feature columns
    feature_columns <- get_feature_columns(x=data_obj)
    
    # Determine which columns has missing entries
    censored_features <- feature_columns[sapply(feature_columns, function(ii, data_obj) (!all(is_valid_data(data_obj@data[[ii]]))), data_obj=data_obj)]
  }
  
  # Skip if there are no missing values.
  if(length(censored_features) == 0){
    return(data_obj)
  }
  
  # Define the replacement list
  replacement_list <- fam_lapply(cl=cl,
                                 assign=NULL,
                                 X=censored_features,
                                 FUN=.get_imputed_values,
                                 progress_bar=FALSE,
                                 data_obj=data_obj,
                                 feature_info_list=feature_info_list,
                                 chopchop=TRUE)
  
  # Set replacement names
  names(replacement_list) <- censored_features
  
  # Update columns in the dataObject
  data_obj <- update_with_replacement(data=data_obj, replacement_list=replacement_list)
  
  return(data_obj)
}

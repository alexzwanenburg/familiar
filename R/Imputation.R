#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

.get_available_imputation_methods <- function(){
  return(c(.get_available_simple_imputation_methods(),
           .get_available_lasso_imputation_methods()))
  
}

.get_available_simple_imputation_methods <- function(){
  return("simple")
}

.get_available_lasso_imputation_methods <- function(){
  return("lasso")
}

setClass("featureInfoParametersImputation",
         contains="featureInfoParameters",
         slots=list("method" = "character",
                    "type" = "character",
                    "required_features"="ANY"),
         prototype=list("method"=NA_character_,
                        "type" = NA_character_,
                        "required_features"=NULL))

setClass("featureInfoParametersImputationNone",
         contains="featureInfoParametersImputation",
         slots=list("reason" = "ANY"),
         prototype=list("reason" = NULL))

setClass("featureInfoParametersImputationSimple",
         contains="featureInfoParametersImputation",
         slots=list("model" = "ANY"),
         prototype=list("model" = NULL))

setClass("featureInfoParametersImputationLasso",
         contains="featureInfoParametersImputation",
         slots=list("simple" = "ANY",
                    "model" = "ANY"),
         prototype=list("simple" = NULL,
                        "model" = NULL))

setClass("featureInfoParametersImputationContainer",
         contains="featureInfoParameters",
         slots=list("type" = "character",
                    "required_features" = "ANY",
                    "model" = "ANY"),
         prototype=list("type" = NA_character_,
                        "required_features"=NULL,
                        "model" = NULL))


create_imputation_parameter_skeleton <- function(feature_info_list,
                                                 feature_names=NULL,
                                                 imputation_method,
                                                 .override_existing=FALSE){
  # Creates a skeleton for the provided imputation method.
  
  # Determine feature names from the feature info list, if provided.
  if(is.null(feature_names)) feature_names <- names(feature_info_list)
  
  # Select only features that appear in the feature info list.
  feature_names <- intersect(names(feature_info_list),
                             feature_names)
  
  # Skip step if no feature info objects are updated.
  if(is_empty(feature_names)) return(feature_info_list)
  
  # Check that method is applicable.
  .check_parameter_value_is_valid(x=imputation_method,
                                  var_name="imputation_method",
                                  values=.get_available_imputation_methods())
  
  # Update familiar info objects with a feature normalisation skeleton.
  updated_feature_info <- fam_lapply(X=feature_info_list[feature_names],
                                     FUN=.create_imputation_parameter_skeleton,
                                     method=imputation_method,
                                     .override_existing=.override_existing)
  
  # Provide names for the updated feature info objects.
  names(updated_feature_info) <- feature_names
  
  # Replace list elements.
  feature_info_list[feature_names] <- updated_feature_info
  
  return(feature_info_list)
}



.create_imputation_parameter_skeleton <- function(feature_info,
                                                  method,
                                                  .override_existing=FALSE){
  
  # Check if imputation data was already completed, and does not require being
  # determined anew.
  if(feature_info_complete(feature_info@imputation_parameters) & !.override_existing) return(feature_info)
  
  # Pass to underlying function that constructs the skeleton.
  object <- ..create_imputation_parameter_skeleton(feature_name=feature_info@name,
                                                   feature_type=feature_info@feature_type,
                                                   available=is_available(feature_info),
                                                   method=method)
  
  # Update imputation_parameters slot.
  feature_info@imputation_parameters <- object
  
  return(feature_info)
}



..create_imputation_parameter_skeleton <- function(feature_name,
                                                   feature_type="numeric",
                                                   available=TRUE,
                                                   method){
  # This is the lowest level function for creating imputation parameter
  # skeletons.
  
  # Create the relevant objects.
  if(!available || method=="none"){
    object <- methods::new("featureInfoParametersImputationNone",
                           reason="feature was omitted prior to transformation")
    
  } else if(method %in% .get_available_simple_imputation_methods()){
    object <- methods::new("featureInfoParametersImputationSimple",
                           "method"=method)
    
  } else if(method %in% .get_available_lasso_imputation_methods()){
    object <- methods::new("featureInfoParametersImputationLasso",
                           "method"=method)
  } else if(method == "container"){
    object <- methods::new("featureInfoParametersImputationContainer")
      
  } else {
    ..error_reached_unreachable_code(paste0("..create_imputation_parameter_skeleton: encountered an unknown imputation method: ", paste_s(method)))
  }
  
  # Set the name and type of the object.
  object@name <- feature_name
  object@type <- feature_type
  
  # Update the familiar version.
  object <- add_package_version(object=object)
  
  return(object)
}



add_imputation_info <- function(cl=NULL,
                                feature_info_list,
                                data,
                                verbose=FALSE){
  # Determine normalisation parameters and add them to the feature_info_list.
  
  # Find feature columns.
  feature_names <- get_feature_columns(x=data)
  
  # Sanity check.
  if(!(setequal(feature_names, get_available_features(feature_info_list=feature_info_list)))){
    ..error_reached_unreachable_code("add_imputation_info: features in data and the feature info list are expect to be the same, but were not.")
  }
  
  # Iterate over features and train univariate imputation.
  updated_feature_info <- fam_mapply(cl=cl,
                                     FUN=.add_imputation_info,
                                     feature_info=feature_info_list[feature_names],
                                     mask_data=data@data[, mget(feature_names)],
                                     MoreArgs=list("data"=data),
                                     progress_bar=verbose,
                                     chopchop=TRUE)
  
  # Provide names for the updated feature info objects.
  names(updated_feature_info) <- feature_names
  
  # Replace list elements.
  feature_info_list[feature_names] <- updated_feature_info
  
  # Apply initial results to the dataset.
  imputed_data <- .impute_features(data=data,
                                   feature_info_list=feature_info_list,
                                   initial_imputation=TRUE)
  
  # Iterate over features again to train multivariate imputation models.
  updated_feature_info <- fam_mapply(cl=cl,
                                     FUN=.add_imputation_info,
                                     feature_info=feature_info_list[feature_names],
                                     mask_data=data@data[, mget(feature_names)],
                                     MoreArgs=list("data"=imputed_data),
                                     progress_bar=verbose,
                                     chopchop=TRUE)
  
  # Provide names for the updated feature info objects.
  names(updated_feature_info) <- feature_names
  
  # Replace list elements.
  feature_info_list[feature_names] <- updated_feature_info
  
  return(feature_info_list)
}



.add_imputation_info <- function(feature_info,
                                 data,
                                 mask_data){
  
  # Pass to underlying function that adds the feature info.
  object <- add_feature_info_parameters(object=feature_info@imputation_parameters,
                                        data=data,
                                        mask_data=mask_data)
  
  # Update normalisation_parameters slot.
  feature_info@imputation_parameters <- object
  
  return(feature_info)
}



.impute_features <- function(cl=NULL,
                             data,
                             feature_info_list,
                             initial_imputation,
                             mask_data=NULL,
                             verbose=FALSE){
  
  # Check if data is empty
  if(is_empty(data)) return(data)
  
  # Check if data has features
  if(!has_feature_data(x=data)) return(data)
  
  # Find the columns containing features
  feature_names <- get_feature_columns(x=data)

  # Use data variable for masking uncensored data.
  if(is.null(mask_data)) mask_data <- data
  
  # Impute data.  
  imputation_list <- fam_mapply(cl=cl,
                                FUN=..impute_features,
                                feature_info=feature_info_list[feature_names],
                                mask_data=mask_data@data[, mget(feature_names)],
                                MoreArgs=list("data"=data,
                                              "initial_imputation"=initial_imputation),
                                progress_bar=verbose,
                                chopchop=TRUE)
  
  # Update name of data in columns.
  names(imputation_list) <- feature_names
  
  # Update with replacement in the data object.
  data <- update_with_replacement(data=data,
                                  replacement_list=imputation_list)
  
  return(data)
}



..impute_features <- function(feature_info,
                              data,
                              mask_data,
                              initial_imputation){
  
  # Find uncensored data.
  uncensored_data <- .mask_data_to_mask(mask_data = mask_data, type=feature_info@imputation_parameters@type)
  
  # Return data as is, if there is no censored data.
  if(all(uncensored_data)) return(data@data[[feature_info@imputation_parameters@name]])
  
  # Infer missing values.
  y <- apply_feature_info_parameters(object=feature_info@imputation_parameters,
                                     data=data,
                                     mask_data=mask_data,
                                     initial_imputation=initial_imputation)
  
  return(y)
}



##### initialize (none) --------------------------------------------------------
setMethod("initialize", signature(.Object="featureInfoParametersImputationNone"),
          function(.Object, ...){
            
            # Update with parent class first.
            .Object <- callNextMethod()
            
            # The parameter set is by definition complete when no normalisation
            # is performed.
            .Object@complete <- TRUE
            
            return(.Object)
          })


##### add_feature_info_parameters (generic imputation, ANY) -------------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersImputation", data="ANY"),
          function(object,
                   data,
                   mask_data,
                   ...){
            
            # Check if all required parameters have been set.
            if(feature_info_complete(object)) return(object)
            
            # Check that any data is present.
            if(is_empty(data)){
              # Return a none-class object.
              object <- ..create_imputation_parameter_skeleton(feature_name=object@name,
                                                               feature_type=object@type,
                                                               method="none")
              
              object@reason <- "insufficient data to infer imputation parameters"
              
              return(object)
            }
            
            # Find uncensored data.
            mask_data <- .mask_data_to_mask(mask_data = mask_data, type=object@type)
            
            # If there are no uncensored data, we cannot determine imputation
            # parameters.
            if(!any(mask_data)){
              # Return a none-class object.
              object <- ..create_imputation_parameter_skeleton(feature_name=object@name,
                                                               feature_type=object@type,
                                                               method="none")
              
              object@reason <- "insufficient data to infer imputation parameters"
              
              return(object)
            }

            return(object)
          })



##### add_feature_info_parameters (simple imputation, data object) --------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersImputationSimple", data="dataObject"),
          function(object, 
                   data,
                   mask_data,
                   ...) {

            # Check if all required parameters have been set.
            if(feature_info_complete(object)) return(object)
            
            # Run general checks for imputation. This may yield none-class
            # objects which are complete by default.
            object <- callNextMethod()
            
            # Check if all required parameters have been set now.
            if(feature_info_complete(object)) return(object)
            
            # Find uncensored data.
            mask_data <- .mask_data_to_mask(mask_data = mask_data, type=object@type)
            
            # Select data.
            y <- data@data[mask_data, mget(object@name)][[1]]
            
            # Select value.
            if(object@type == "numeric"){
              # Select median values
              common_value <- stats::median(y)
              
            } else if(object@type == "factor"){
              # Select the mode as the replacement value
              common_value <- get_mode(y)
            }
            
            # Add value to object.
            object@model <- common_value
            
            # Set required feature.
            object@required_features <- object@name
            
            # Set complete.
            object@complete <- TRUE
            
            return(object)
          })



##### add_feature_info_parameters (lasso imputation, data object) --------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParametersImputationLasso", data="dataObject"),
          function(object, 
                   data,
                   mask_data,
                   ...) {
            
            # Suppress NOTES due to non-standard evaluation in data.table
            name <- NULL
            
            # Check if all required parameters have been set.
            if(feature_info_complete(object)) return(object)
            
            # Run general checks for imputation. This may yield none-class
            # objects which are complete by default.
            object <- callNextMethod()
            
            # Check if all required parameters have been set now.
            if(feature_info_complete(object)) return(object)
            
            # Check if simple inference values have been set. For multivariate
            # inference methods, an initial univariate step is used to fill any
            # holes in the data, prior to prediction. This method is therefore
            # first called to set parameters of the univariate, prior to
            # determining the LASSO model.
            if(!feature_info_complete(object@simple)){
              object@simple <- ..create_imputation_parameter_skeleton(feature_name=object@name,
                                                                      feature_type=object@type,
                                                                      method="simple")
              
              # Determine parameters.
              object@simple <- add_feature_info_parameters(object=object@simple,
                                                           data=data,
                                                           mask_data=mask_data)
              
              return(object)
            }
            
            # Determine if more than one feature is present.
            if(get_n_features(data) == 1){
              # Switch to simple  univariate inference only, in case only one
              # feature is present.
              return(object@simple)
            }
            
            # Find uncensored data.
            mask_data <- .mask_data_to_mask(mask_data = mask_data, type=object@type)
            
            # Select finite data.
            distribution <- ifelse(object@type == "numeric", "gaussian", "multinomial")
            
            # Check that the glmnet package is installed.
            require_package(x="glmnet",
                            purpose="to impute data using lasso regression")
            
            # Select known data as response variable.
            y <- data@data[[object@name]][mask_data]
            
            # Select features.
            x <- filter_features(data, remove_features=object@name)

            # Use effect coding to convert categorical data into encoded data.
            encoded_data <- encode_categorical_variables(data=x,
                                                         object=NULL,
                                                         encoding_method="dummy",
                                                         drop_levels=FALSE)
            
            # Extract data table with contrasts.
            x <- encoded_data$encoded_data
            
            # Perform a cross-validation to derive a optimal lambda. This may
            # rarely fail if y is numeric but does not change in a particular
            # fold. In that case, skip.
            lasso_model <- tryCatch(glmnet::cv.glmnet(x=as.matrix(x@data[mask_data, mget(get_feature_columns(x))]),
                                                      y=y,
                                                      family=distribution,
                                                      alpha=1,
                                                      standardize=FALSE,
                                                      nfolds=min(sum(mask_data), 20),
                                                      parallel=FALSE),
                                    error=identity)
            
            if(inherits(lasso_model, "error") )return(object@simple)
            
            # Determine the features with non-zero coefficients. We want to have
            # the minimal required support for the model to minimise the effort
            # that is required for external model validation. GLMNET by
            # definition wants to have the exact same complete input space, even
            # though but a few features are used. That is a bad idea for
            # portability. Therefore we will be jumping through some hoops to
            # make a model with minimum support.
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
            if(is_empty(vimp_table)) return(object@simple)
            
            # Derive required features
            required_features <- vimp_table$name
            
            # Select features.
            x <- filter_features(data, available_features=required_features)
            
            # Use effect coding to convert categorical data into encoded data.
            encoded_data <- encode_categorical_variables(data=x,
                                                         object=NULL,
                                                         encoding_method="dummy",
                                                         drop_levels=FALSE)

            # Extract data table with contrasts.
            x <- encoded_data$encoded_data
            
            # Check the number of columns in train_data. glmnet wants at least
            # two columns.
            if(ncol(x) == 1) x[, "bogus__variable__":=0.0]
            browser()
            # Train a small model at this lambda.1se.
            lasso_model <- glmnet::glmnet(x = as.matrix(x),
                                          y = y,
                                          family = distribution,
                                          lambda = lasso_model$lambda.1se,
                                          standardize = FALSE)
            
            # Remove extraneous information from the model.
            lasso_model <- ..trim_glmnet(lasso_model)
            
            # Add LASSO regression model to object.
            object@model <- lasso_model
            object@required_features <- required_features
            
            # Set complete
            object@complete <- TRUE
            
            return(object)
          })



##### apply_feature_info_parameters (none, data object) ----------------------
setMethod("apply_feature_info_parameters",  signature(object="featureInfoParametersImputationNone", data="dataObject"),
          function(object, 
                   data,
                   mask_data,
                   ...) {
            return(data@data[[object@name]])
          })


##### apply_feature_info_parameters (simple, data object) ----------------------
setMethod("apply_feature_info_parameters",  signature(object="featureInfoParametersImputationSimple", data="dataObject"),
          function(object, 
                   data,
                   mask_data,
                   ...) {
            
            # Find uncensored data.
            mask_data <- .mask_data_to_mask(mask_data = mask_data, type=object@type)
            
            # Return data as is, if there is no censored data.
            if(all(mask_data)) return(data@data[[object@name]])
            
            # Get intended data.
            y <- data@data[[object@name]]
            
            # Replace censored values.
            y[!mask_data] <- object@model
            
            return(y)
          })


##### apply_feature_info_parameters (none, data object) ----------------------
setMethod("apply_feature_info_parameters",  signature(object="featureInfoParametersImputationLasso", data="dataObject"),
          function(object, 
                   data,
                   mask_data,
                   initial_imputation,
                   ...) {
            browser()
            # For initial imputation use univariate imputer.
            if(initial_imputation){
              return(apply_feature_info_parameters(object=object@simple,
                                                   data=data,
                                                   mask_data=mask_data,
                                                   initial_imputation=initial_imputation))
            } 
            
            # Find uncensored data.
            mask_data <- .mask_data_to_mask(mask_data = mask_data, type=object@type)
            
            # Return data as is, if there is no censored data.
            if(all(mask_data)) return(data@data[[object@name]])
            
            # Check that features required for imputation are present in data.
            if(!all(get_feature_columns(data) %in% object@required_features)){
              return(apply_feature_info_parameters(object=object@simple,
                                                   data=data,
                                                   mask_data=mask_data,
                                                   initial_imputation=initial_imputation))
            }
            browser()
            # Check that the glmnet package is installed.
            require_package(x="glmnet",
                            purpose="to impute data using lasso regression")
            
            # Get intended data.
            y <- data@data[[object@name]]
            
            # Select features.
            x <- filter_features(data, available_features=object@required_features)
            
            # Use effect coding to convert categorical data into encoded data.
            encoded_data <- encode_categorical_variables(data=x,
                                                         object=NULL,
                                                         encoding_method="dummy",
                                                         drop_levels=FALSE)
            
            # Extract data table with contrasts.
            x <- encoded_data$encoded_data
            
            # Check if the validation data has two or more columns
            if(ncol(x) == 1) x[, "bogus__variable__":=0.0]
            
            # Get the type of response for glmnet predict
            response_type <- ifelse(object@feature_type == "numeric", "response", "class")
            
            # Impute values for censored values.
            y[!mask_data] <- drop(predict(object=object@model,
                                          newx=as.matrix(x),
                                          type=response_type))
            
            return(y)
          })



##### apply_feature_info_parameters (container, data object) ----------------------
setMethod("apply_feature_info_parameters",  signature(object="featureInfoParametersImputationContainer", data="dataObject"),
          function(object, 
                   data,
                   mask_data,
                   initial_imputation,
                   ...) {
            browser()
            
            # Find uncensored data.
            uncensored_data <- .mask_data_to_mask(mask_data = mask_data, type=object@type)
            
            # Return data as is, if there is no censored data.
            if(all(uncensored_data)) return(data@data[[object@name]])
            
            # Get intended data.
            y <- data@data[[object@name]]
            
            # Push only rows of data to be imputed.
            censored_data <- data
            censored_data@data <- data.table::copy(censored_data@data[!uncensored_data, ])
            
            # Set aggregation function for 
            if(object@type == "numeric"){
              aggregation_function <- mean
              
            } else if(object@type == "feature"){
              aggregation_function <- get_mode
            }
            
            # Dispatch to imputers in the container.
            imputed_values <- lapply(object@model,
                                     apply_feature_info_parameters,
                                     data=censored_data,
                                     mask_data=mask_data[!uncensored_data])
            
            # Set as data.table.
            imputed_values <- data.table::as.data.table(imputed_values)
            
            # Aggregate by row.
            imputed_values[, list("value"=aggregation_function(do.call(c, .SD))),
                           .SDcols=colnames(imputed_values),
                           by=seq_len(nrow(imputed_values))]
            
            # Replace censored values in y.
            y[!uncensored_data] <- imputed_values$value
            
            return(y)
          })



.mask_data_to_mask <- function(mask_data, type){
  if(type == "numeric"){
    mask_data <- is.finite(mask_data)

  } else if(type == "factor"){
    mask_data <- !is.na(mask_data)

  } else {
    ..error_reached_unreachable_code(paste0(".mask_data_to_mask: encountered unknown feature type: ", type))
  }
  
  return(mask_data)
}



..collect_and_aggregate_imputation_info <- function(feature_info_list, feature_name, feature_type){
  # Aggregate transformation parameters. This function exists so that it can be
  # tested as part of a unit test.
  
  # Create container object.
  container_object <- ..create_imputation_parameter_skeleton(feature_name=feature_name,
                                                             feature_type=feature_type,
                                                             method="container")
  
  # Extract and store imputation objects.
  container_object@model <- lapply(feature_info_list,
                                   function(x){
                                     # Filter out "none" class imputation
                                     # objects.
                                     if(is(x@imputation_parameters, "featureInfoParametersImputationNone")) return(NULL)
                                     
                                     return(x@imputation_parameters)
                                   })
  
  if(is_empty(container_object@model)){
    container_object@model <- list(..create_imputation_parameter_skeleton(feature_name=feature_name,
                                                                          feature_type=feature_type,
                                                                          method="none"))
  }
  
  # Set required parameters.
  container_object@required_features <- unique(unlist(lapply(container_object@model, function(x) (x@required_features))))
  
  return(list("parameters"=container_object))
}

# 
# 
# add_imputation_info <- function(cl=NULL,
#                                 feature_info_list,
#                                 data,
#                                 verbose=FALSE){
#   
#   
#   
#   # Add simple imputation info
#   feature_info_list <- impute.add_simple_imputation_info(cl=cl,
#                                                          feature_info_list=feature_info_list,
#                                                          data_obj=data_obj,
#                                                          verbose = verbose & settings$prep$imputation_method == "simple")
#   
#   if(settings$prep$imputation_method == "lasso"){
#     # Process data with simple imputation
#     local_train_data  <- impute.impute_simple(cl=cl, data_obj=data_obj, feature_info_list=feature_info_list)
#     
#     # Add lasso-regression info
#     feature_info_list <- impute.add_lasso_imputation_info(cl=cl,
#                                                           feature_info_list=feature_info_list,
#                                                           data_obj=data_obj,
#                                                           uncensored_data_obj=local_train_data,
#                                                           verbose=verbose)
#   }
#   
#   return(feature_info_list)
# }
# 
# 
# 
# impute.add_lasso_imputation_info <- function(cl=NULL, feature_info_list, data_obj, uncensored_data_obj, verbose=TRUE){
#   
#   # Suppress NOTES due to non-standard evaluation in data.table
#   name <- NULL
#   
#   # Check that the glmnet package is installed.
#   require_package(x="glmnet",
#                   purpose="to impute data using lasso regression")
#   
#   .add_info <- function(feature, feature_info_list, data_obj, uncensored_data_obj){
# 
#     # local featureInfo object
#     object <- feature_info_list[[feature]]
#     
#     # Select valid data for training
#     valid_entries <- is_valid_data(data_obj@data[[feature]])
#     train_data <- uncensored_data_obj@data[valid_entries, get_feature_columns(x=uncensored_data_obj), with=FALSE]
#     train_data <- train_data[, (feature):=NULL]
#     
#     outcome_data  <- uncensored_data_obj@data[valid_entries, ][[feature]]
#     
#     # Determine the relevant distribution for the model
#     if(object@feature_type == "numeric"){
#       distribution <- "gaussian"
#     } else {
#       distribution <- "multinomial"
#     }
#     
#     # Use effect coding to convert categorical data into encoded data
#     encoded_data <- encode_categorical_variables(data=train_data,
#                                                  encoding_method="dummy",
#                                                  drop_levels=FALSE)
# 
#     # Extract data table with contrasts.
#     train_data <- encoded_data$encoded_data
#     
#     # Perform a cross-validation to derive a optimal lambda. This may rarely
#     # fail if y is numeric but does not change in a particular fold. In this
#     # case, skip.
#     lasso_model <- tryCatch(glmnet::cv.glmnet(x=as.matrix(train_data),
#                                               y=outcome_data,
#                                               family=distribution,
#                                               alpha=1,
#                                               standardize=FALSE,
#                                               nfolds=min(nrow(train_data), 20),
#                                               parallel=FALSE),
#                             error=identity)
#     
#     if(inherits(lasso_model, "error")) return(object)
#     
#     # Determine the features with non-zero coefficients. We want to have the
#     # minimal required support for the model to minimise the effort that is
#     # required for external model validation. GLMNET by definition wants to have
#     # the exact same complete input space, even though but a few features are
#     # used. That is a bad idea for portability. Therefore we will be jumping
#     # through some hoops to make a model with minimum support.
#     if(distribution=="multinomial"){
#       # Read coefficient lists
#       coef_list <- coef(lasso_model, s="lambda.1se")
#       
#       # Parse into matrix and retrieve row names
#       coef_mat <- sapply(coef_list, as.matrix)
#       rownames(coef_mat) <- dimnames(coef_list[[1]])[[1]]
#       
#       # Calculate score
#       score <- apply(abs(coef_mat), 1, max)
#     } else {
#       # Read coefficient matrix
#       coef_mat <- as.matrix(coef(lasso_model, s="lambda.1se"))
#       
#       # Calculate score
#       score <- abs(coef_mat)[,1]
#     }
#     
#     # Parse score to data.table
#     vimp_table <- data.table::data.table("score"=score, "name"=names(score))
#   
#     # Throw out the intercept and elements with 0.0 coefficients
#     vimp_table <- vimp_table[name!="(Intercept)" & score!=0.0]
#     
#     # Find the original names
#     vimp_table <- decode_categorical_variables_vimp(object=encoded_data$reference_table,
#                                                     vimp_table=vimp_table,
#                                                     method="abs_max")
#     
#     # Check that the optimal model complexity lambda is connected to at least
#     # one feature. If not, we have to use the simple estimate.
#     if(is_empty(vimp_table)) return(object)
#     
#     # Derive required features
#     required_features <- vimp_table$name
#     
#     # Create a data set with minimal support
#     train_data <- uncensored_data_obj@data[valid_entries, required_features, with=FALSE]
#     
#     # Use effect coding to convert categorical data into encoded data
#     encoded_data <- encode_categorical_variables(data=train_data,
#                                                  encoding_method="dummy",
#                                                  drop_levels=FALSE)
#     
#     # Extract data table with contrasts.
#     train_data <- encoded_data$encoded_data
# 
#     # Check the number of columns in train_data. glmnet wants at least two
#     # columns.
#     if(ncol(train_data) == 1) train_data[, "bogus__variable__":=0.0]
#     
#     # Determine the lambda.1se and train a small model at this lambda
#     lasso_model <- glmnet::glmnet(x = as.matrix(train_data),
#                                   y = outcome_data,
#                                   family = distribution,
#                                   lambda = lasso_model$lambda.1se,
#                                   standardize = FALSE)
#     
#     # Remove extraneous information from the model.
#     lasso_model <- ..trim_glmnet(lasso_model)
#     
#     # Add lasso model and required features to the information.
#     if(!is.list(object@imputation_parameters)){
#       object@imputation_parameters <- list("lasso_model" = list(lasso_model),
#                                            "required_features" = required_features)
#     } else {
#       object@imputation_parameters$lasso_model <- list(lasso_model)
#       object@imputation_parameters$required_features <- required_features
#     }
# 
#     return(object)
#   }
#   
#   # Determine feature columns
#   feature_columns <- get_feature_columns(x=data_obj)
# 
#   # Add in check so that there is more one than feature available. We cannot define model using only an outcome column.
#   if(length(feature_columns) > 1){
#     
#     upd_list <- fam_lapply(cl=cl,
#                            assign=NULL,
#                            X=feature_columns,
#                            FUN=.add_info,
#                            progress_bar=verbose,
#                            feature_info_list=feature_info_list,
#                            data_obj=data_obj,
#                            uncensored_data_obj=uncensored_data_obj,
#                            chopchop=TRUE)
#     
#     # Set names of the update object list
#     names(upd_list) <- feature_columns
#     
#     # Update feature_info_list
#     feature_info_list[feature_columns] <- upd_list
#   }
# 
#   return(feature_info_list)
# }
# 
# 
# 
# impute.impute_lasso <- function(cl=NULL, feature_info_list, data_obj, uncensored_data_obj, censored_features=NULL){
#   
#   # Suppress NOTES due to non-standard evaluation in data.table.
#   values <- NULL
#   
#   # Check that the glmnet package is installed.
#   require_package(x="glmnet",
#                   purpose="to impute data using lasso regression")
#   
#   .get_imputed_values <- function(feature, data_obj, uncensored_data_obj, feature_info_list){
# 
#     # featureInfo object for the current feature.
#     object <- feature_info_list[[feature]]
#     
#     # Get data for the current feature.
#     y <- data_obj@data[[feature]]
#     
#     # Find the censored entries for the current data set.
#     censored_entries <- !is_valid_data(y)
#     
#     # Replace missing values
#     if(is_empty(object@imputation_parameters$lasso_model) | is.null(object@imputation_parameters$required_features)){
#       # Check if required entries are present in the featureInfo object and
#       # insert common value if not.
#       y[censored_entries] <- object@imputation_parameters$common_value
#       
#     } else if(!all(object@imputation_parameters$required_features %in% colnames(uncensored_data_obj@data))) {
#       # Check if the required columns are present in the data, and insert the
#       # common value if not.
#       y[censored_entries] <- object@imputation_parameters$common_value
#       logger.warning(paste0("Imputation: not all required features were found for imputation of the ", feature, " feature. Missing are: ",
#                             paste(setdiff(object@imputation_parameters$required_features, colnames(uncensored_data_obj@data)), collapse=", ")))
#       
#     } else {
#       # Proceed with imputation using the lasso model
#       
#       # Create a data set with minimal support
#       validation_data <- uncensored_data_obj@data[censored_entries,
#                                                   mget(object@imputation_parameters$required_features)]
#       
#       # Use effect coding to convert categorical data into encoded data.
#       encoded_data <- encode_categorical_variables(data=validation_data,
#                                                    encoding_method="dummy",
#                                                    drop_levels=FALSE)
#       
#       # Extract data table with contrasts.
#       validation_data <- encoded_data$encoded_data
#       
#       # Check if the validation data has two or more columns
#       if(ncol(validation_data) == 1) validation_data[, "bogus__variable__":=0.0]
# 
#       # Get the type of response for glmnet predict
#       response_type <- ifelse(object@feature_type == "numeric", "response", "class")
#       
#       # Impute values
#       imputed_values <- lapply(object@imputation_parameters$lasso_model, function(model_object, validation_data, response_type){
# 
#         # Predict values.
#         imputed_value_table <- data.table::data.table("values"=drop(predict(object=model_object,
#                                                                             newx=as.matrix(validation_data),
#                                                                             type=response_type)))
#         
#         # Add temporary sample id
#         imputed_value_table[, "temp_sample_id":=.I]
#         
#         return(imputed_value_table)
#       }, validation_data=validation_data, response_type=response_type)
# 
#       # Concatenate to single table
#       imputed_values <- data.table::rbindlist(imputed_values)
#       
#       # The desired output is determined by the feature type (numeric or
#       # factor).
#       if(object@feature_type == "numeric"){
#         y[censored_entries] <- imputed_values[,
#                                               list("values"=mean(values, na.rm=TRUE)),
#                                               by="temp_sample_id"]$values
# 
#       } else {
#         y[censored_entries] <- imputed_values[,
#                                               list("values"=get_mode(values)),
#                                               by="temp_sample_id"]$values
#       }
#     }
#     
#     return(y)
#   }
# 
#   # Check if data is present
#   if(is_empty(data_obj)) return(data_obj)
#   
#   # Check if censored_features are provided as input.
#   if(is.null(censored_features)){
#     # Define feature columns
#     feature_columns <- get_feature_columns(x=data_obj)
#     
#     # Determine which columns has missing entries
#     censored_features <- feature_columns[sapply(feature_columns, function(ii, data_obj) (!all(is_valid_data(data_obj@data[[ii]]))), data_obj=data_obj)]
#   }
#   
#   # Skip if there are no missing values.
#   if(length(censored_features) == 0) return(data_obj)
#   
#   # Define the replacement list
#   replacement_list <- fam_lapply(cl=cl,
#                                  assign=NULL,
#                                  X=censored_features,
#                                  FUN=.get_imputed_values,
#                                  progress_bar=FALSE,
#                                  data_obj=data_obj,
#                                  uncensored_data_obj=uncensored_data_obj,
#                                  feature_info_list=feature_info_list,
#                                  chopchop=TRUE)
#   
#   # Set replacement names
#   names(replacement_list) <- censored_features
#   
#   # Update columns in the dataObject
#   data_obj <- update_with_replacement(data=data_obj, replacement_list=replacement_list)
# 
#   return(data_obj)
# }
# 
# 
# 
# impute.add_simple_imputation_info <- function(cl=NULL, feature_info_list, data_obj, settings, verbose=TRUE){
#   
#   # Local function
#   .add_info <- function(feature, feature_info_list, data_obj){
#     # Local featureInfo object
#     object <- feature_info_list[[feature]]
#     
#     # Find all available data for the current feature
#     x <- data_obj@data[[feature]][is_valid_data(data_obj@data[[feature]])]
#     
#     if(object@feature_type == "numeric"){
#       # Select median values
#       common_value <- stats::median(x)
#       
#     } else if(object@feature_type == "factor"){
#       # Select the mode as the replacement value
#       common_value <- get_mode(x)
#       
#     } else {
#       logger.stop(paste0("Pre-processing: invalid data type encountered in data: \'", object@feature_type, "\'"))
#     }
#     
#     # Add common value to the information
#     if(!is.list(object@imputation_parameters)){
#       object@imputation_parameters <- list("common_value"=common_value)
#     } else {
#       object@imputation_parameters$common_value <- common_value
#     }
#     
#     return(object)
#   }
#   
#   # Determine feature columns
#   feature_columns <- get_feature_columns(x=data_obj)
# 
#   # Add simple imputation information
#   upd_list <- fam_lapply(cl=cl,
#                          assign=NULL,
#                          X=feature_columns,
#                          FUN=.add_info,
#                          progress_bar=verbose,
#                          feature_info_list=feature_info_list,
#                          data_obj=data_obj,
#                          chopchop=TRUE)
#   
#   # Set names of the update object list
#   names(upd_list) <- feature_columns
#   
#   # Update feature_info_list
#   feature_info_list[feature_columns] <- upd_list
# 
#   return(feature_info_list)
# }
# 
# 
# impute.impute_simple <- function(cl=NULL, data_obj, feature_info_list, censored_features=NULL){
#   # Performs simple imputation of features
#   
#   .get_imputed_values <- function(feature, data_obj, feature_info_list){
#     
#     # Get data for the current feature
#     x <- data_obj@data[[feature]]
#     
#     # Replace missing values
#     x[!is_valid_data(x)] <-  feature_info_list[[feature]]@imputation_parameters$common_value
#     
#     return(x)
#   }
#   
#   
#   # Check if data is present
#   if(is_empty(data_obj)){
#     return(data_obj)
#   }
# 
#   if(is.null(censored_features)){
#     # Define feature columns
#     feature_columns <- get_feature_columns(x=data_obj)
#     
#     # Determine which columns has missing entries
#     censored_features <- feature_columns[sapply(feature_columns, function(ii, data_obj) (!all(is_valid_data(data_obj@data[[ii]]))), data_obj=data_obj)]
#   }
#   
#   # Skip if there are no missing values.
#   if(length(censored_features) == 0){
#     return(data_obj)
#   }
#   
#   # Define the replacement list
#   replacement_list <- fam_lapply(cl=cl,
#                                  assign=NULL,
#                                  X=censored_features,
#                                  FUN=.get_imputed_values,
#                                  progress_bar=FALSE,
#                                  data_obj=data_obj,
#                                  feature_info_list=feature_info_list,
#                                  chopchop=TRUE)
#   
#   # Set replacement names
#   names(replacement_list) <- censored_features
#   
#   # Update columns in the dataObject
#   data_obj <- update_with_replacement(data=data_obj, replacement_list=replacement_list)
#   
#   return(data_obj)
# }

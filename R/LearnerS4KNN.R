#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarKNN",
         contains="familiarModel")

setClass("familiarKNNlinear",
         contains="familiarKNN")

setClass("familiarKNNradial",
         contains="familiarKNN")

.get_available_radial_knn_learners <- function(show_general=TRUE) return(c("k_nearest_neighbours_radial"))

.get_available_linear_knn_learners <- function(show_general=TRUE) return(c("k_nearest_neighbours"))


#####is_available#####
setMethod("is_available", signature(object="familiarKNN"),
          function(object, ...){
           
            #k-nearest neighbours is only available for categorical outcomes.
            if(object@outcome_type %in% c("binomial", "multinomial")){
              return(TRUE)
              
            } else {
              return(FALSE)
            }
          })



#####get_default_hyperparameters,KNNlinear#####
setMethod("get_default_hyperparameters", signature(object="familiarKNN"),
          function(object, data=NULL){
            
            # Initialise list and declare hyperparameter entries.
            param <- list()
            param$sign_size <- list()
            param$k <- list()
            
            if(is(object, "familiarKNNradial")){
              param$gamma <- list()
            }
           
            # If data is explicitly NULL, return the list with hyperparameter
            # names only.
            if(is.null(data)) return(param)
            
            # Get the number of unique series.
            n_samples <- data.table::uniqueN(data@data, by=get_id_columns(id_depth="series"))
            n_classes <- length(get_outcome_class_levels(x=data))
            
            
            ##### Signature size ###############################################
            param$sign_size <- .get_default_sign_size(data_obj=data)
            
            
            ##### Number of nearest neighbours k ###############################
            
            # Define the range for the number of nearest neighbour clusters.
            k_range <- c(n_classes, max(c(n_classes, ceiling(2*n_samples^(1/3)))))
            
            # Define the default value.
            k_default <- sort(unique(c(5, 10, 20, k_range)))
            k_default <- k_default[k_default >= k_range[1] & k_default <= k_range[2]]
            
            param$k <- .set_hyperparameter(default=k_default, type="integer", range=k_range,
                                           valid_range=c(n_classes, Inf), randomise=TRUE)
            
            if(is(object, "familiarKNNradial")){
              ##### Radial basis function kernel gamma #########################
              
              # Gamma is based on the log10 scale with a 10^-5 offset, so that
              # gamma=-5 leads to no distance weighting.
              # Set the gamma parameter.
              param$gamma <- .set_hyperparameter(default=c(-5, -3, -1, 0, 1, 3, 5), type="numeric", range=c(-5, 5),
                                                 valid_range=c(-5, Inf), randomise=TRUE)
            }
            
            return(param)
          })


#####..train####
setMethod("..train", signature(object="familiarKNN", data="dataObject"),
          function(object, data){
            
            # Check if training data is ok.
            if(has_bad_training_data(object=object, data=data)) return(callNextMethod())
            
            # Find feature columns in the data.
            feature_columns <- get_feature_columns(x=data)

            # Parse formula
            formula <- stats::reformulate(termlabels=feature_columns, response=quote(outcome))
            
            # Generate model. NOTE: sknn is directly imported through NAMESPACE
            # as predict is not exported by klaR.
            if(is(object, "familiarKNNlinear")){
              model <- tryCatch(sknn(formula,
                                     data=data@data,
                                     kn=object@hyperparameters$k),
                                error=identity)
              
            } else if(is(object, "familiarKNNradial")){
              model <- tryCatch(sknn(formula,
                                     data=data@data,
                                     kn=object@hyperparameters$k,
                                     gamma=10^object@hyperparameters$gamma - 10^-5),
                                error=identity)
              
            } else {
              ..error_reached_unreachable_code(paste0("..train,familiarKNN: encountered unknown learner of unknown class: ", paste0(class(object), collapse=", ")))
            }
            
            
            # Check if the model trained at all.
            if(inherits(model, "error")) return(callNextMethod())
            
            # Add model to the familiarModel object.
            object@model <- model

            return(object)
          })



#####..predict#####
setMethod("..predict", signature(object="familiarKNN", data="dataObject"),
          function(object, data, type="default", ...){
            
            if(type == "default"){
              ##### Default method #############################################
              
              # Check if the model was trained.
              if(!model_is_trained(object)) return(callNextMethod())
              
              # Check if the data is empty.
              if(is_empty(data)) return(callNextMethod())
              
              # Get an empty prediction table.
              prediction_table <- get_placeholder_prediction_table(object=object,
                                                                   data=data,
                                                                   type=type)
              
              # Use the model for prediction.
              model_predictions <- predict(object=object@model,
                                           newdata=data@data)
              
              # Obtain class levels.
              class_levels <- get_outcome_class_levels(x=object)
              
              # Add class probabilities.
              class_probability_columns <- get_class_probability_name(x=object)
              for(ii in seq_along(class_probability_columns)){
                prediction_table[, (class_probability_columns[ii]):=model_predictions$posterior[, class_levels[ii]]]
              }
              
              # Add the predicted class.
              prediction_table[, "predicted_class":=model_predictions$class]
              
              return(prediction_table)
              
            } else {
              ##### User-specified method ######################################
              # Note: the predict method for klaR::sknn does not actually have a
              # type argument at the moment.
              
              # Check if the model was trained.
              if(!model_is_trained(object)) return(NULL)
              
              # Check if the data is empty.
              if(is_empty(data)) return(NULL)
              
              # Use the model for prediction.
              return(predict(object=object@model,
                             newdata=data@data,
                             type=type,
                             ...))
            }
          })



#####..vimp#####
# KNN does not have an associated variable importance method.

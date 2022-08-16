#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarCorrelationVimp",
         contains="familiarVimpMethod")

.get_available_correlation_vimp_methods <- function(show_general=TRUE){
  return(c("pearson", "spearman", "kendall"))
}



#####is_available#####
setMethod("is_available", signature(object="familiarCorrelationVimp"),
          function(object, ...){
            return(object@outcome_type %in% c("continuous", "count", "survival"))
          })



#####get_default_hyperparameters#####
setMethod("get_default_hyperparameters", signature(object="familiarCorrelationVimp"),
          function(object, data=NULL, ...) return(list()))



#####..vimp######
setMethod("..vimp", signature(object="familiarCorrelationVimp"),
          function(object, data, ...){
            # Suppress NOTES due to non-standard evaluation in data.table
            score <- outcome_event <- NULL
            
            if(is_empty(data)) return(callNextMethod())
            
            # Drop non-event data for censored data analysis for calculating
            # correlation and set outcome column.
            if(object@outcome_type=="survival"){
              data@data <- data@data[outcome_event == 1, ]
              
              # Check whether the filtered data does not allow for assessing
              # variable importance.
              if(has_bad_training_data(object=object, data=data)) return(callNextMethod)
            }

            # Use effect coding to convert categorical data into encoded data -
            # this is required to deal with factors with missing/new levels
            # between training and test data sets.
            encoded_data <- encode_categorical_variables(data=data,
                                                         object=object,
                                                         encoding_method="dummy",
                                                         drop_levels=FALSE)
            
            # Find feature columns in the data.
            feature_columns <- get_feature_columns(x=encoded_data$encoded_data)
            
            # Compute correlation coefficients.
            correlation_coefficients <- sapply(feature_columns, function(feature, data, outcome_type, correlation_method){
              
              if(outcome_type == "survival"){
                # Use the outcome_time column for survival data.
                correlation_coefficient <- stats::cor(x = data[[feature]],
                                                      y = data[["outcome_time"]],
                                                      method = correlation_method)
                
              } else {
                # Use the outcome column for continuous and count data.
                correlation_coefficient <- stats::cor(x = data[[feature]],
                                                      y = data[["outcome"]],
                                                      method = correlation_method)
              }
              
              return(correlation_coefficient)
            }, data=encoded_data$encoded_data@data,
            outcome_type = object@outcome_type,
            correlation_method = object@vimp_method)
            
            # Create variable importance object.
            vimp_object <- methods::new("vimpTable",
                                        vimp_table=data.table::data.table("score"=abs(correlation_coefficients), "name"=feature_columns),
                                        encoding_table=encoded_data$reference_table,
                                        score_aggregation="max",
                                        invert=TRUE)
            
            return(vimp_object)
          })

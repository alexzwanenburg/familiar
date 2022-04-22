#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


##### add_feature_info_parameters (NULL, ANY) ----------------------------------
setMethod("add_feature_info_parameters", signature(object="NULL", data="ANY"),
          function(object, 
                   data,
                   ...) {
            
            # This is when feature info parameters are completely missing.
            return(object)
          })


##### add_feature_info_parameters (feature info parameters, ANY) ---------------
setMethod("add_feature_info_parameters", signature(object="featureInfoParameters", data="ANY"),
          function(object, 
                   data,
                   ...) {
            
            # Default behaviour is to return the object as is.
            return(object)
          })


##### apply_feature_info_parameters (NULL, ANY) --------------------------------
setMethod("apply_feature_info_parameters", signature(object="NULL", data="ANY"),
          function(object, 
                   data,
                   ...) {
            
            # This is when feature info parameters are completely missing.
            return(data)
          })


##### apply_feature_info_parameters (feature info parameters, ANY) -------------
setMethod("apply_feature_info_parameters", signature(object="featureInfoParameters", data="ANY"),
          function(object, 
                   data,
                   ...) {
            
            # Default behaviour is to return the data as is.
            return(data)
          })

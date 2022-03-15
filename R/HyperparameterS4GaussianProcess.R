#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R


##### has_bad_training_data ----------------------------------------------------
setMethod("has_bad_training_data", signature(object="familiarHyperparameterGaussianProcess", data="data.table"),
          function(object, data, ...){
            
            # Perform the general check first.
            if(callNextMethod()) return(TRUE)
            
            # For Gaussian process at least eight instances should be present.
            if(nrow(data) < 8) return(TRUE)
            
            return(FALSE)
          })

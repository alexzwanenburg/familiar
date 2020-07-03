#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("familiarRandomVimp",
         contains="familiarVimpMethod")

setClass("familiarNoneVimp",
         contains="familiarVimpMethod")

setClass("familiarSignatureVimp",
         contains="familiarVimpMethod")

.get_available_random_vimp_methods <- function(show_general=TRUE){
  return("random")
}

.get_available_none_vimp_methods <- function(show_general=TRUE){
  return("none")
}

.get_available_signature_only_vimp_methods <- function(show_general=TRUE){
  return("signature_only")
}


#####is_available (random) #####
setMethod("is_available", signature(object="familiarRandomVimp"),
          function(object, ...){
            return(TRUE)
          })

#####is_available (none)#####
setMethod("is_available", signature(object="familiarNoneVimp"),
          function(object, ...){
            return(TRUE)
          })

#####is_available (signature only)#####
setMethod("is_available", signature(object="familiarSignatureVimp"),
          function(object, ...){
            return(TRUE)
          })

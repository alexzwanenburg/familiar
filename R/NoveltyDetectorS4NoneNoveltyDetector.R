#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL


setClass("familiarNoneNoveltyDetector",
         contains="familiarNoveltyDetector")



.get_available_none_detectors <- function(show_general=TRUE){
  return(c("none", "false", "no"))
} 



#####is_available#####
setMethod("is_available", signature(object="familiarNoneNoveltyDetector"),
          function(object, ...){
            # We can always not create a novelty detector.
            return(TRUE)
          })

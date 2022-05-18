#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

setClass("clusterMethod",
         slots=list("method" = "character",
                    "data_type" = "character",
                    "cluster_cut_method" = "character",
                    "representation_method"="character"),
         prototype=list("method" = NA_character_,
                        "data_type" = NA_character_,
                        "cluster_cut_method" = "none",
                        "representation_method"="none"))

setClass("clusterMethodHierarchical",
         contains="clusterMethod",
         slots=list("similarity_metric" = "character",
                    "similarity_threshold"="numeric"),
         prototype=list("similarity_metric" = NA_character_,
                        "similarity_threshold" = NA_real_))

setClass("clusterMethodHClust",
         contains="clusterMethodHierarchical",
         slots=list("linkage_method"="character"),
         prototype=list("linkage_method"=NA_character_))

setClass("clusterMethodAgnes",
         contains="clusterMethodHierarchical",
         slots=list("linkage_method"="character"),
         prototype=list("linkage_method"=NA_character_))

setClass("clusterMethodDiana",
         contains="clusterMethodHierarchical")

setClass("clusterMethodPAM",
         contains="clusterMethod",
         slots=list("similarity_metric" = "character"),
         prototype=list("similarity_metric" = "character"))

setClass("clusterMethodNone",
         contains="clusterMethod")



create_cluster_method_object <- function(cluster_method,
                                         data_type,
                                         cluster_linkage=NULL,
                                         cluster_cut_method=NULL,
                                         cluster_similarity_threshold=NULL,
                                         cluster_similarity_metric=NULL,
                                         cluster_representation_method=NULL){
  
  
  # Check that method is applicable.
  .check_parameter_value_is_valid(x=cluster_method,
                                  var_name=ifelse(data_type=="cluster", "cluster_method", paste0(data_type, "_cluster_method")),
                                  values=.get_available_cluster_methods())
  
  # Check that data_type is valid.
  .check_parameter_value_is_valid(x=data_type,
                                  var_name="data_type",
                                  values=c("feature", "cluster", "sample"))
  
  if(cluster_method == "none"){
    object <- methods::new("clusterMethodNone")
    
  } else if(cluster_method == "pam"){
    object <- methods::new("clusterMethodPAM")
    
  } else if(cluster_method == "hclust"){
    object <- methods::new("clusterMethodHClust")
    
  } else if(cluster_method == "agnes"){
    object <- methods::new("clusterMethodAgnes")
    
  } else if(cluster_method == "diana"){
    object <- methods::new("clusterMethodDiana")
    
  } else {
    ..error_reached_unreachable_code(paste0("create_cluster_method_object: encountered an unknown cluster method: ", cluster_method))
  }
  
  # Cluster method and data type are always set.
  object@method <- cluster_method
  object@data_type <- data_type
  
  # Set cluster object method parameters as required.
  object <- set_object_parameters(object=object,
                                  cluster_linkage=cluster_linkage,
                                  cluster_cut_method=cluster_cut_method,
                                  cluster_similarity_threshold=cluster_similarity_threshold,
                                  cluster_similarity_metric=cluster_similarity_metric,
                                  cluster_representation_method=cluster_representation_method)
  browser()
  return(object)
}


##### set_object_parameters (none) ---------------------------------------------
setMethod("set_object_parameters", signature(object="clusterMethodNone"),
          function(object,
                   ...){
            
            return(object)
          })



##### set_object_parameters (general) ------------------------------------------
setMethod("set_object_parameters", signature(object="clusterMethod"),
          function(object,
                   cluster_cut_method=NULL,
                   cluster_representation_method=NULL,
                   ...){
            
            # Check that data type is set correctly. This is used for many
            # checks.
            if(!object@data_type %in% c("feature", "sample", "cluster")){
              ..error_reached_unreachable_code(paste0("set_object_parameters,clusterMethod: the data_type attribut was not correctly set."))
            }
            
            # Cut methods are optional, and default to "none".
            if(!is.null(cluster_cut_method)){
              
              # Check cluster cut method.
              .check_parameter_value_is_valid(x=cluster_cut_method,
                                              var_name=ifelse(object@data_type=="cluster",
                                                              "cluster_cut_method",
                                                              paste0(object@data_type, "_cluster_cut_method")),
                                              values=.get_available_cluster_cut_methods(object@method))
              
              # Set cluster cut method.
              object@cluster_cut_method <- cluster_cut_method
            }
            
            # Check representation method.
            if(!is.null(cluster_representation_method) && object@cluster_cut_method != "none"){
              
              # Check representation method.
              .check_parameter_value_is_valid(x=cluster_representation_method,
                                              var_name=ifelse(object@data_type=="cluster",
                                                              "cluster_representation_method",
                                                              paste0(object@data_type, "_cluster_representation_method")),
                                              values=.get_available_cluster_representation_methods(object@method))
              
              # Set cluster representation method.
              object@representation_method <- cluster_representation_method
            }
            
            return(object)
          })



##### set_object_parameters (PAM) ----------------------------------------------
setMethod("set_object_parameters", signature(object="clusterMethodPAM"),
          function(object,
                   cluster_similarity_metric,
                   ...){
            
            # Call the method for the parent class (clusterMethod) first.
            object <- methods::callNextMethod()
            
            # Check that similarity metric is valid.
            .check_parameter_value_is_valid(x=cluster_similarity_metric,
                                            var_name=paste0(object@data_type, "_similarity_metric"),
                                            values=.get_available_similarity_metrics(data_type=object@data_type))
            
            # Set similarity metric.
            object@similarity_metric <- cluster_similarity_metric
            
            return(object)
          })



##### set_object_parameters (general hierarchical) -----------------------------
setMethod("set_object_parameters", signature(object="clusterMethodHierarchical"),
          function(object,
                   cluster_similarity_metric,
                   cluster_similarity_threshold=NULL,
                   ...){
            # Call the method for the parent class (clusterMethod) first.
            object <- methods::callNextMethod()
            
            # Check that similarity metric is valid.
            .check_parameter_value_is_valid(x=cluster_similarity_metric,
                                            var_name=paste0(object@data_type, "_similarity_metric"),
                                            values=.get_available_similarity_metrics(data_type=object@data_type))
            
            # Set similarity metric.
            object@similarity_metric <- cluster_similarity_metric
            
            if(object@cluster_cut_method == c("fixed_cut")){
              
              # Check cutting height for fixed cut. Multiple cut heights are
              # possible. Use as_distance to get two-value ranges, but note that
              # these values are similarity otherwise.
              sapply(cluster_similarity_threshold,
                     .check_number_in_valid_range,
                     var_name=paste0(object@data_type, "_similarity_threshold"),
                     range=similarity.metric_range(similarity_metric=object@similarity_metric,
                                                   as_distance=TRUE))
              
              # Attach to object.
              object@similarity_threshold <- cluster_similarity_threshold
            }
            
            return(object)
          })



##### set_object_parameters (hclust) -------------------------------------------
setMethod("set_object_parameters", signature(object="clusterMethodHClust"),
          function(object,
                   cluster_linkage_method,
                   ...){
            
            # Call next method (clusterMethodHierarchical). This will also call
            # the method for its parent method (clusterMethod).
            object <- methods::callNextMethod()
            
            # Check that linkage method is valid.
            .check_parameter_value_is_valid(x=cluster_linkage_method,
                                            var_name=paste0(object@data_type, "_linkage_method"),
                                            values=.get_available_linkage_methods())
            
            return(object)
          })



##### set_object_parameters (agnes) --------------------------------------------
setMethod("set_object_parameters", signature(object="clusterMethodAgnes"),
          function(object,
                   cluster_linkage_method,
                   ...){
            
            # Call next method (clusterMethodHierarchical). This will also call
            # the method for its parent method (clusterMethod).
            object <- methods::callNextMethod()
            
            # Check that linkage method is valid.
            .check_parameter_value_is_valid(x=cluster_linkage_method,
                                            var_name=paste0(object@data_type, "_linkage_method"),
                                            values=.get_available_linkage_methods())
            
            return(object)
          })



##### set_object_parameters (diana) --------------------------------------------
setMethod("set_object_parameters", signature(object="clusterMethodDiana"),
          function(object,
                   ...){
            
            # Call next method (clusterMethodHierarchical). This will also call
            # the method for its parent method (clusterMethod).
            object <- methods::callNextMethod()
            
            return(object)
          })

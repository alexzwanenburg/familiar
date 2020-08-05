#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

#'@title Conversion to familiarEnsemble object.
#'
#'@description Creates `familiarEnsemble` a object from `familiarModel` objects.
#'
#'@param object A `familiarEnsemble` object, or one or more
#'  `familiarModel` objects that will be internally converted to a
#'  `familiarEnsemble` object. Paths to such objects can also be provided.
#'@param ... Unused arguments.
#'
#'@return A `familiarEnsemble` object.
#'@exportMethod as_familiar_ensemble
#'@md
#'@rdname as_familiar_ensemble-methods
setGeneric("as_familiar_ensemble", function(object, ...) standardGeneric("as_familiar_ensemble"))

#####as_familiar_ensemble (ensemble)#####

#'@rdname as_familiar_ensemble-methods
setMethod("as_familiar_ensemble", signature(object="familiarEnsemble"),
          function(object, ...) return(object))

#####as_familiar_ensemble (model)#####

#'@rdname as_familiar_ensemble-methods
setMethod("as_familiar_ensemble", signature(object="familiarModel"),
          function(object, ...){
            # A separate familiar model is encapsulated in a list, and then transformed.
            return(do.call(as_familiar_ensemble, args=list("object"=list(object))))
          })

#####as_familiar_ensemble (list)#####

#'@rdname as_familiar_ensemble-methods
setMethod("as_familiar_ensemble", signature(object="list"),
          function(object, ...){
            
            # Load familiar objects. This does nothing if the list already
            # contains only familiar S4 objects, but will load any files from
            # the path and will check uniqueness of classes.
            object <- load_familiar_object(object=object)
            
            # Return the object if it contains a single familiarEnsemble.
            if(length(object) == 1 & all(sapply(object, is, "familiarEnsemble"))){
              return(object[[1]])
              
            } else if(!all(sapply(object, is, "familiarModel"))){
              stop("familiarEnsemble objects can only be constructed from familiarModel objects.")
            }

            # Generate a placeholder pooling table
            run_table <- data.table::data.table("data_id"=0L, "run_id"=0L, "can_pre_process"=TRUE, "perturbation"="new_data", "perturb_level"=0L)
            
            # Generate a skeleton familiarEnsemble
            fam_ensemble <- methods::new("familiarEnsemble",
                                         model_list = object,
                                         learner = object[[1]]@learner,
                                         fs_method = object[[1]]@fs_method,
                                         run_table = list("run_table"=run_table, "ensemble_data_id"=0L, "ensemble_run_id"=0L))
            
            # Complete the ensemble using information provided by the model(s)
            fam_ensemble <- complete_familiar_ensemble(object=fam_ensemble)
            
            return(fam_ensemble)
          })

#####as_familiar_ensemble (character)#####

#'@rdname as_familiar_ensemble-methods
setMethod("as_familiar_ensemble", signature(object="character"),
          function(object, ...){
            # Interpret character as if it is a path, and pass to the same
            # method for list objects.
            return(do.call(as_familiar_ensemble, args=list("object"=as.list(object))))
          })

#####as_familiar_ensemble (generic)#####

#'@rdname as_familiar_ensemble-methods
setMethod("as_familiar_ensemble", signature(object="ANY"),
          function(object, ...){
            # There familiar ensembles can only be generated from one of the above functions.
            ..error_cannot_convert_to_familiar_object(object=object, expected_class="familiarEnsemble")
          })



#'@title Conversion to familiarData object.
#'
#'@description Creates `familiarData` a object from `familiarEnsemble` or `familiarModel` objects.
#'
#'@param object A `familiarData` object, or a `familiarEnsemble` or
#'  `familiarModel` objects that will be internally converted to a
#'  `familiarData` object. Paths to such objects can also be provided.
#'
#'@inheritDotParams extract_data
#'
#'@details The `data` argument is required if `familiarEnsemble` or
#'  `familiarModel` objects are provided.
#'
#'@return A `familiarData` object.
#'@exportMethod as_familiar_data
#'@md
#'@rdname as_familiar_data-methods
setGeneric("as_familiar_data", function(object, ...) standardGeneric("as_familiar_data"))

#####as_familiar_data (data)######

#'@rdname as_familiar_data-methods
setMethod("as_familiar_data", signature(object="familiarData"),
          function(object, ...){
            return(object)
          })

#####as_familiar_data (ensemble)######

#'@rdname as_familiar_data-methods
setMethod("as_familiar_data", signature(object="familiarEnsemble"),
          function(object, ...){
            
            # Familiar data
            fam_data <- do.call(extract_data, args=append(list("object"=object),
                                                          list(...)))
              
            # Set a placeholder name for the familiarData object
            fam_data <- set_data_set_names(x=fam_data)
            
            return(fam_data)
          })

#####as_familiar_data (model)######

#'@rdname as_familiar_data-methods
setMethod("as_familiar_data", signature(object="familiarModel"),
          function(object, ...){
            # Push to the same method for lists. This creates a familiarEnsemble
            # and then allows for creation of a familiarData object.
            return(do.call(as_familiar_data, args=append(list("object"=list(object)),
                                                         list(...))))
          })

#####as_familiar_data (list)######

#'@rdname as_familiar_data-methods
setMethod("as_familiar_data", signature(object="list"),
          function(object, ...){
            
            # Load familiar objects. This does nothing if the list already
            # contains only familiar S4 objects, but will load any files from
            # the path and will check uniqueness of classes.
            object <- load_familiar_object(object=object)
            
            # Return the object if it contains a single familiarEnsemble.
            if(length(object) == 1 & all(sapply(object, is, "familiarData"))){
              return(object[[1]])
            } 
            
            # Convert familiarModel(s) to familiarEnsemble.
            if(all(sapply(object, is, "familiarModel"))) {
              object <- list(as_familiar_ensemble(object=object))
            } 
            
            # Check if a single familiarEnsemble has been supplied or generated.
            if(!all(sapply(object, is, "familiarEnsemble")) | length(object) > 1){
              stop("A familiarData object can only be constructed from a single familiarEnsemble object.")
              
            } else {
              object <- object[[1]]
            }
            
            return(do.call(as_familiar_data, args=append(list("object"=object),
                                                         list(...))))
          })

#####as_familiar_data (character)######

#'@rdname as_familiar_data-methods
setMethod("as_familiar_data", signature(object="character"),
          function(object, ...){
            # Pass to as_familiar_data method for lists to load objects there.
            return(do.call(as_familiar_data, args=append(list("object"=as.list(object)),
                                                         list(...))))
          })

#####as_familiar_data (generic)#####

#'@rdname as_familiar_data-methods
setMethod("as_familiar_data", signature(object="ANY"),
          function(object, ...){
            # There familiar ensembles can only be generated from one of the above functions.
            ..error_cannot_convert_to_familiar_object(object=object, expected_class="familiarData")
          })


#'@title Conversion to familiarCollection object.
#'
#'@description Creates a `familiarCollection` objects from `familiarData`,
#'  `familiarEnsemble` or `familiarModel` objects.
#'
#'@param object `familiarCollection` object, or one or more `familiarData`
#'  objects, that will be internally converted to a `familiarCollection` object.
#'  It is also possible to provide a `familiarEnsemble` or one or more
#'  `familiarModel` objects together with the data from which data is computed
#'  prior to export. Paths to such files can also be provided.
#'@param familiar_data_names Names of the dataset(s). Only used if the `object` parameter
#'  is one or more `familiarData` objects.
#'@param collection_name Name of the collection.
#'
#'@inheritDotParams extract_data
#'
#'@details A `data` argument is expected if the `object` argument is a
#'  `familiarEnsemble` object or one or more `familiarModel` objects.
#'
#'@return A `familiarCollection` object.
#'@exportMethod as_familiar_collection
#'@md
#'@rdname as_familiar_collection-methods
setGeneric("as_familiar_collection", function(object, familiar_data_names=NULL, collection_name=NULL, ...) standardGeneric("as_familiar_collection"))

#####as_familiar_collection (collection)#####

#'@rdname as_familiar_collection-methods
setMethod("as_familiar_collection", signature(object="familiarCollection"),
          function(object, ...) return(object))


#####as_familiar_collection (data)#####

#'@rdname as_familiar_collection-methods
setMethod("as_familiar_collection", signature(object="familiarData"),
          function(object, familiar_data_names=NULL, collection_name=NULL, ...){
            # Pass to as_familiar_collection for lists to load and process objects there.
            return(do.call(as_familiar_collection,
                           args=append(list("object"=list(object),
                                            "familiar_data_names"=familiar_data_names,
                                            "collection_name"=collection_name),
                                       list(...))))
          })


#####as_familiar_collection (ensemble)#####

#'@rdname as_familiar_collection-methods
setMethod("as_familiar_collection", signature(object="familiarEnsemble"),
          function(object, familiar_data_names=NULL, collection_name=NULL, ...){
            # Pass to as_familiar_collection for lists to load and process objects there.
            return(do.call(as_familiar_collection,
                           args=append(list("object"=list(object),
                                            "familiar_data_names"=familiar_data_names,
                                            "collection_name"=collection_name),
                                       list(...))))
          })


#####as_familiar_collection (model)#####

#'@rdname as_familiar_collection-methods
setMethod("as_familiar_collection", signature(object="familiarModel"),
          function(object, familiar_data_names=NULL, collection_name=NULL, ...){

            # Pass to as_familiar_collection for lists to load and process objects there.
            return(do.call(as_familiar_collection,
                           args=append(list("object"=list(object),
                                            "familiar_data_names"=familiar_data_names,
                                            "collection_name"=collection_name),
                                       list(...))))
          })


#####as_familiar_collection (list)#####

#'@rdname as_familiar_collection-methods
setMethod("as_familiar_collection", signature(object="list"),
          function(object, familiar_data_names=NULL, collection_name=NULL, ...){

            # Load familiar objects. This does nothing if the list already
            # contains only familiar S4 objects, but will load any files from
            # the path and will check uniqueness of classes.
            object <- load_familiar_object(object=object)

            # Return the object if it contains a single familiarCollection.
            if(length(object) == 1 & all(sapply(object, is, class2="familiarCollection"))){
              return(object[[1]])
              
            } else if(all(sapply(object, is, class2="familiarCollection"))){
              stop("Only a single familiarCollection can be returned.")
            }
            
            # Convert to familiarModel(s) to familiarData
            if(all(sapply(object, is, class2="familiarModel"))) {
              object <- do.call(as_familiar_data, args=append(list("object"=object),
                                                              list(...)))
              
              # Store in list, if required
              if(!is(object, "list")){
                object <- list(object)
              }
            } 
            
            # Convert familiarEnsemble to familiarData
            if(all(sapply(object, is, class2="familiarEnsemble")) & length(object) == 1){
              object <- do.call(as_familiar_data, args=append(list("object"=object),
                                                              list(...)))
              
              # Store in list, if required.
              if(!is(object, "list")){
                object <- list(object)
              }
              
            } else if(all(sapply(object, is, class2="familiarEnsemble"))){
              stop("A familiarData object can only be constructed from a single familiarEnsemble object.")
            }
            
            if(!all(sapply(object, is, class2="familiarData"))){
              stop("Only familiarData objects can be used to construct a familiarCollection object.")
            }

            # Obtain names of the familiarData objects.
            object_names <- sapply(object, function(fam_data_obj) (fam_data_obj@name))
            
            # Check if all the datasets are unique.
            if(any(duplicated(object_names))){
              stop(paste0("familiarCollections cannot contain identical familiarData sets. The following duplicates were found: ",
                          paste(unique(object_names[duplicated(object_names)]), collapse=", ")))
            }
            
            # Check if names for the data are externally provided, and obtain
            # them from the familiarData objects otherwise.
            if(is.null(familiar_data_names)){
              familiar_data_names <- object_names
            }
            
            # Set data names as a factor.
            if(!is.factor(familiar_data_names)){
              familiar_data_names <- factor(familiar_data_names, levels=unique(familiar_data_names))
            }
            
            # Check if the collection has a name
            if(is.null(collection_name)){
              collection_name <- "collection"
            } else {
              collection_name <- as.character(collection_name)
            }
            
            # Generate data names
            fam_collect <- methods::new("familiarCollection",
                                        collection_name = collection_name,
                                        data_sets = sapply(object, function(fam_data_obj) (fam_data_obj@name)),
                                        outcome_type = object[[1]]@outcome_type,
                                        outcome_info = .aggregate_outcome_info(x=lapply(object, function(list_elem) (list_elem@outcome_info))),
                                        fs_vimp = collect_fs_vimp(fam_data_list=object),
                                        model_vimp = collect_model_vimp(fam_data_list=object),
                                        permutation_vimp = collect_permutation_vimp(fam_data_list=object),
                                        hyperparameters = collect_hyperparameters(fam_data_list=object),
                                        hyperparameter_data = NULL,
                                        req_feature_cols = unique(unlist(lapply(object, function(fam_data_obj) (fam_data_obj@req_feature_cols)))),
                                        important_features = unique(unlist(extract_from_slot(object_list=object, slot_name="important_features", na.rm=TRUE))),
                                        learner = unique(sapply(object, function(fam_data_obj) (fam_data_obj@learner))),
                                        fs_method =  unique(sapply(object, function(fam_data_obj) (fam_data_obj@fs_method))),
                                        prediction_data = collect_prediction_data(fam_data_list=object),
                                        confusion_matrix = collect_confusion_matrix_data(fam_data_list=object),
                                        decision_curve_data = collect_decision_curve_analysis_data(fam_data_list=object),
                                        calibration_info = collect_calibration_info(fam_data_list=object),
                                        calibration_data = collect_calibration_data(fam_data_list=object),
                                        model_performance = collect_model_performance(fam_data_list=object),
                                        km_info = collect_stratification_info(fam_data_list=object),
                                        km_data = collect_stratification_data(fam_data_list=object),
                                        auc_data = collect_auc_data(fam_data_list=object),
                                        univariate_analysis = collect_univariate_analysis(fam_data_list=object),
                                        feature_expressions = collect_feature_expressions(fam_data_list=object),
                                        mutual_correlation = collect_mutual_correlation(fam_data_list=object),
                                        ice_data = NULL,
                                        is_anonymised = FALSE,
                                        project_id = object[[1]]@project_id)
            
            # Create labels for the data names for correct ordering of plots etc.
            fam_collect <- set_data_set_names(x=fam_collect, new=as.character(familiar_data_names), order=levels(familiar_data_names))
            
            # Add a package version to the familiarCollection object
            fam_collect <- add_package_version(object=fam_collect)
            
            return(fam_collect)
          })


#####as_familiar_collection (character)#####

#'@rdname as_familiar_collection-methods
setMethod("as_familiar_collection", signature(object="character"),
          function(object, familiar_data_names=NULL, collection_name=NULL, ...){

            # Pass to as_familiar_collection for lists to load and process objects there.
            return(do.call(as_familiar_collection,
                           args=append(list("object"=as.list(object), "familiar_data_names"=familiar_data_names, "collection_name"=collection_name),
                                       list(...))))
          })


#####as_familiar_collection (generic)#####

#'@rdname as_familiar_collection-methods
setMethod("as_familiar_collection", signature(object="ANY"),
          function(object, ...){
            # There familiar ensembles can only be generated from objects
            # defined in the previous methods.
            ..error_cannot_convert_to_familiar_object(object=object, expected_class="familiarCollection")
          })


#####load_familiar_object (character)#####
setMethod("load_familiar_object", signature(object="character"),
          function(object){
            
            # Determine if file(s) exist
            existing_files <- sapply(object, file.exists)
            if(!all(existing_files)){
              stop(paste0("Not all files could be found: ", paste0(object[!existing_files], collapse=", ")))
            }
            
            # Load object
            fam_object <- lapply(object, readRDS)
            
            # Check that objects are of one class.
            object_class <- unique(sapply(fam_object, class))
            
            if(length(object_class) > 1){
              stop(paste0("Found objects that do not have the same class: ", paste0(object_class, collapse=", ")))
            }
            
            if(!object_class %in% c("familiarModel", "familiarEnsemble", "familiarData", "familiarCollection")){
              stop(paste0("The loaded object is not a familiar S4 object. Found: ", object_class))
            }
            
            # Unlist if the input is singular.
            if(length(object) == 1){
              fam_object <- fam_object[[1]]
            }
            
            return(fam_object)
          })

#####load_familiar_object (list)#####
setMethod("load_familiar_object", signature(object="list"),
          function(object){
            
            # Load all objects in the list. 
            object <- lapply(object, load_familiar_object)
            
            # Check that objects are of one class.
            object_class <- unique(sapply(object, class))
            
            if(length(object_class) > 1){
              stop(paste0("Found objects that do not have the same class: ", paste0(object_class, collapse=", ")))
            }
            
            return(object)
          })

#####load_familiar_object (generic)#####
setMethod("load_familiar_object", signature(object="ANY"),
          function(object){

            # Return the object if it is a familiar S4 class object that has already been loaded. Else throw an error.
            
            if(is_any(object, class2=c("familiarModel", "familiarEnsemble", "familiarData", "familiarCollection"))){
              return(object)
              
            } else {
              stop(paste0("The loaded object is not a familiar S4 object. Found: ", paste0(class(object), collapse=", ")))
            }
          })

#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R
NULL

#####save (familiarData)#####
setMethod("save", signature(list="familiarData", file="character"),
          function(list, file) {
            .save(object=list, dir_path=file)
          })

#####get_object_name (familiarData)#####
setMethod("get_object_name", signature(object="familiarData"),
          function(object, abbreviated=FALSE){
            
            # Extract data and run id
            ensemble_data_id <- tail(object@pooling_table, n=1)$ensemble_data_id
            ensemble_run_id <- tail(object@pooling_table, n=1)$ensemble_run_id
            pool_data_id <- tail(object@pooling_table, n=1)$pool_data_id
            pool_run_id  <- tail(object@pooling_table, n=1)$pool_run_id
            
            data_pooling <- ifelse(tail(object@pooling_table, n=1)$data_perturb_level == tail(object@pooling_table, n=1)$pool_perturb_level,
                                   "ensemble",
                                   "pool")

            if(abbreviated){
              # Create an abbreviated name
              object_name <- paste(data_pooling, ensemble_data_id, ensemble_run_id, ifelse(object@is_validation, "validation", "development"), "data", sep=".")
            } else {
              # Create the full name of the object
              object_name <- get_object_file_name(learner=object@learner, fs_method=object@fs_method, project_id=object@project_id, data_id=ensemble_data_id,
                                                  run_id=ensemble_run_id, pool_data_id=pool_data_id, pool_run_id=pool_run_id,
                                                  object_type="familiarData", is_ensemble=data_pooling=="ensemble",
                                                  is_validation=object@is_validation, with_extension=FALSE)
            }
            
            return(object_name)
          })

####add_package_version (familiarData)#####
setMethod("add_package_version", signature(object="familiarData"),
          function(object){
            
            # Set version of familiar
            return(.add_package_version(object=object))
          })

#####add_identifiers#####
setMethod("add_identifiers", signature(data="ANY", object="familiarData"),
          function(data, object, more_identifiers=NULL){
            # Adds identifying columns to a table
            
            if(is.null(data)) {
              return(NULL)
            }
            
            if(!inherits(data, "data.table")){
              stop("\"data\" should be a data.table.")
            }

            # Check which identifiers should be added
            if(is.null(more_identifiers)){
              if(!all(more_identifiers %in% c("fs_method", "learner"))){
                stop("Only feature selection methods (\"fs_method\") and learners (\"learner\") can be added as additional identifiers.")
              }
            }
            id_order <- c("data_set", more_identifiers)

            if(nrow(data)>=1){
              
              # Get the name of the data
              data_set <- object@name
              
              # Insert "model_name" column
              data[, "data_set":=data_set]
              
              if(any(id_order == "fs_method")){
                data[, "fs_method":=object@fs_method]
              }
              
              if(any(id_order == "learner")){
                data[, "learner":=object@learner]
              }
              
              # Reorder columns and move model_name to the front
              data.table::setcolorder(data, neworder=id_order)
              
              return(data)
              
            } else {
              # In case the table is empty, return an empty table with the model name attached.
              empty_table <- data.table::data.table("data_set"=character(0))
              
              if(any(id_order == "fs_method")){
                empty_table[, "fs_method":=character(0)]
              }
              
              if(any(id_order == "learner")){
                empty_table[, "learner":=character(0)]
              }
              
              return(cbind(empty_table, data))
            }
            
          })

#####set_data_set_names#####

#' @title Name dataset
#'  
#' @description Set the `name` slot using the object name.
#'
#' @param x A `familiarData` object.
#' 
#' @return A `familiarData` object with a generated name.
#' @md
#' @keywords internal
setMethod("set_data_set_names", signature(x="familiarData"),
          function(x, new=NULL){

            if(x@project_id == 0 & is.null(new)){
              # Generate a random object name. A project_id of 0 means that the
              # objects was auto-generated (i.e. through object conversion). We
              # randomly generate chracters and add a time stamp, so that
              # collision is practically impossible.
              slot(object=x, name="name") <- paste0(as.character(as.numeric(format(Sys.time(),"%H%M%S"))),
                                                    "_", stringi::stri_rand_strings(1, 20, '[A-Z]'))
              
            } else if(is.null(new)) {
              # Generate a sensible object name.
              slot(object=x, name="name") <- get_object_name(object=x)
              
            } else {
              slot(object=x, name="name") <- new
            }
            
            return(x)
          })

# Contains functions to collect and combine entries from one or more familiarData objects




#' @title Collector for univariate feature analysis data
#'
#' @inheritParams collect_fs_vimp
#'
#' @return A list with aggregated univariate analysis information.
#' @noRd
collect_univariate_analysis <- function(fam_data_list){
  # Univariate analysis is partially shared between different familiarData
  # objects which share the same data set, but pre-processing parameters may
  # potentially differ. Moreover, some familiarData objects may lack data.
  
  # Univariate analysis data is shared between methods with the same pool data
  # id, validation status and data perturb level.
  unique_entries <- .which_unique_data(fam_data_list=fam_data_list,
                                       by=c("pool_data_id", "is_validation", "data_perturb_level", "fs_method", "learner"))
  
  # Select only unique entries
  univariate_data <- data.table::rbindlist(lapply(unique_entries, function(ii, fam_data_list){
    
    # Extract univariate data
    univar_data_table <- fam_data_list[[ii]]@univariate_analysis$data

    # Add identifiers
    univar_data_table <- add_identifiers(data=data.table::copy(univar_data_table),
                                         object=fam_data_list[[ii]],
                                         more_identifiers=c("fs_method", "learner"))
    
  }, fam_data_list=fam_data_list), fill=TRUE, use.names=TRUE)
  
  # Set to list
  univariate_data <- list("data"=univariate_data)

  return(univariate_data)
}




.which_unique_data <- function(fam_data_list, by){
  # Perform comparisons to find out which datasets provide unique data
  if(!all(sapply(fam_data_list, class) == "familiarData")){
    stop("Input is expected to be familiarData objects.")
  }

  # Generate an identifier table
  id_table <- data.table::rbindlist(lapply(fam_data_list, function(fam_data){
    dt <- data.table::data.table("ensemble_data_id"=fam_data@pooling_table$ensemble_data_id[1],
                                 "ensemble_run_id"=fam_data@pooling_table$ensemble_run_id[1],
                                 "data_perturb_level"=fam_data@pooling_table$data_perturb_level[1],
                                 "pool_data_id"=fam_data@pooling_table$pool_data_id[1],
                                 "pool_run_id"=fam_data@pooling_table$pool_run_id[1],
                                 "pool_perturb_level"=fam_data@pooling_table$pool_perturb_level[1],
                                 "fs_method"=fam_data@fs_method,
                                 "learner"=fam_data@learner,
                                 "is_validation"=fam_data@is_validation)
    
    return(dt)
  }))

  # Drop columns that are not required for uniqueness
  id_table <- id_table[, by, with=FALSE]
  
  # Drop all duplicate entries
  unique_entries <- which(!duplicated(id_table))
  
  return(unique_entries)
}


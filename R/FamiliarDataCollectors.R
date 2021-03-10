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



#' @title Collector for feature expression data
#'
#' @details Underlying tables with feature expression data may not contain the
#'   same features and may therefore not be joinable. The implementation here
#'   joins all expression tables that can be joined.
#'
#' @inheritParams collect_fs_vimp
#'
#' @return A list with aggregated univariate analysis information.
#' @noRd
collect_feature_expressions <- function(fam_data_list){
  # Feature expressions may be shared between different familiarData objects,
  # but we should check whether they can be combined directly.

  expression_list <- lapply(fam_data_list, function(fam_obj){
    
    # Extract expressions
    expression_table <- fam_obj@feature_expressions$expression_data
    
    if(is_empty(expression_table)){
      return(NULL)
      
    } else {
      # Add identifiers
      expression_table <- add_identifiers(data=data.table::copy(expression_table),
                                          object=fam_obj,
                                          more_identifiers=c("fs_method", "learner"))
    }
    
    return(list("data"=expression_table,
                "feature_info"=fam_obj@feature_expressions$feature_info,
                "feature_cluster_object"=fam_obj@feature_expressions$feature_cluster_object,
                "feature_order"=fam_obj@feature_expressions$feature_order,
                "feature_similarity_metric"=fam_obj@feature_expressions$feature_similarity_metric,
                "sample_cluster_object"=fam_obj@feature_expressions$sample_cluster_object,
                "sample_order"=fam_obj@feature_expressions$sample_order,
                "sample_similarity_metric"=fam_obj@feature_expressions$sample_similarity_metric,
                "evaluation_times"=fam_obj@feature_expressions$evaluation_times))
  })

  return(expression_list)
}



#' @title Collector for mutual correlation data
#' 
#' @inheritParams collect_fs_vimp
#'
#' @return A list with aggregated mutual correlation data.
#' @noRd
collect_mutual_correlation <- function(fam_data_list){
  # Mutual correlation is partially shared between different familiarData
  # objects which share the same data set, but pre-processing parameters may
  # potentially differ. Moreover, some familiarData objects may lack data.
  
  # Parse univariate data
  mutual_correlation_list <- lapply(fam_data_list, function(fam_obj){
    
    # Extract mutual correlation info
    mutual_correlation_data <- fam_obj@mutual_correlation$data
    
    if(is_empty(mutual_correlation_data)){
      return(NULL)
      
    } else {
      # Add identifiers
      mutual_correlation_data <- add_identifiers(data=data.table::copy(mutual_correlation_data),
                                                 object=fam_obj,
                                                 more_identifiers=c("fs_method", "learner"))
    }
    
    return(list("data"=mutual_correlation_data,
                "feature_cluster_object"=fam_obj@mutual_correlation$feature_cluster_object,
                "feature_similarity_metric"=fam_obj@mutual_correlation$feature_similarity_metric))
    
  })
  
  return(mutual_correlation_list)
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


# Contains functions to collect and combine entries from one or more familiarData objects


#' @title Collector for feature selection variable importance data
#'
#' @param fam_data_list A list containing familiarData objects.
#'
#' @return A list with aggregated information regarding feature selection variable importance.
#' @noRd
collect_fs_vimp <- function(fam_data_list){

  # Variable importance is shared between methods with the same feature selection method.
  unique_entries <- .which_unique_data(fam_data_list=fam_data_list, by="fs_method")
  
  # Select only unique entries
  fs_vimp <- data.table::rbindlist(lapply(unique_entries, function(ii, fam_data_list){
    
    # Get the feature selection vimp table  
    fs_vimp_table <- fam_data_list[[ii]]@fs_vimp$vimp_table
    
    # Add identifiers
    fs_vimp_table <- add_identifiers(data=data.table::copy(fs_vimp_table),
                                     object=fam_data_list[[ii]],
                                     more_identifiers="fs_method")
    
  }, fam_data_list=fam_data_list))
  
  # Set aggregation method and rank threshold. These do not change across a dataset
  fs_vimp_list <- list("vimp_table" = fs_vimp,
                       "aggregation_method" = fam_data_list[[unique_entries[1]]]@fs_vimp$aggregation_method,
                       "rank_threshold" = fam_data_list[[unique_entries[1]]]@fs_vimp$rank_threshold)
  
  return(fs_vimp_list)
}



#' @title Collector for model-based variable importance data
#'
#' @inheritParams collect_fs_vimp
#'
#' @return A list with aggregated information regarding model-based variable importance.
#' @noRd
collect_model_vimp <- function(fam_data_list){
  
  # Variable importance is shared between methods with the methods with the same feature selection method, learner and data ids
  unique_entries <- .which_unique_data(fam_data_list=fam_data_list, by=c("fs_method", "learner"))
  
  # Select only unique entries
  model_vimp <- data.table::rbindlist(lapply(unique_entries, function(ii, fam_data_list){
    
    # Get the model-based vimp table  
    model_vimp_table <- fam_data_list[[ii]]@model_vimp$vimp_table
    
    # Add identifiers
    model_vimp_table <- add_identifiers(data=data.table::copy(model_vimp_table),
                                        object=fam_data_list[[ii]],
                                        more_identifiers=c("fs_method", "learner"))
    
  }, fam_data_list=fam_data_list))
  
  # Set aggregation method and rank threshold. These do not change across a dataset
  model_vimp_list <- list("vimp_table" = model_vimp,
                          "aggregation_method" = fam_data_list[[unique_entries[1]]]@model_vimp$aggregation_method,
                          "rank_threshold" = fam_data_list[[unique_entries[1]]]@model_vimp$rank_threshold)

  return(model_vimp_list)
}



#' @title Collector for stratification information
#'
#' @inheritParams collect_fs_vimp
#'
#' @return A list with aggregated cutoffs.
#' @noRd
collect_stratification_info <- function(fam_data_list){
  # Stratification info (e.g. cutoffs) is shared between objects with the same
  # feature selection method and data ids, as they are generated at model
  # creation.
  unique_entries <- .which_unique_data(fam_data_list=fam_data_list, by=c("fs_method", "learner"))

  # Select only unique entries
  stratification_info_table <- data.table::rbindlist(lapply(unique_entries, function(ii, fam_data_list){
    
    # Get the model-based vimp table  
    strat_info_table <- fam_data_list[[ii]]@km_info
    
    # Check if the table is empty
    if(is_empty(strat_info_table)) return(NULL)
    
    # Add identifiers
    strat_info_table <- add_identifiers(data=data.table::copy(strat_info_table),
                                        object=fam_data_list[[ii]],
                                        more_identifiers=c("fs_method", "learner"))
    
  }, fam_data_list=fam_data_list))
  
  # Check of the calibration info table is empty
  if(is_empty(stratification_info_table)) return(NULL)

  return(stratification_info_table)
}




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


universal_collector <- function(fam_data_list, data_slot, extra_data=NULL, more_identifiers=NULL){
  
  # Create placeholder list.
  collection_list <- list()
  
  # Iterate over individual and ensemble data.
  for(type in c("individual", "ensemble")){
    
    # Find present elements in the list.
    present_elements <- unique(unlist(lapply(fam_data_list, function(fam_data_object, type, data_slot) names(slot(object=fam_data_object, name=data_slot)[[type]]),
                                             type=type,
                                             data_slot=data_slot)))
    
    # Find elements that are not related data.tables.
    simple_elements <- setdiff(present_elements,
                                c("model_data", "bootstrap_data", extra_data))
    
    # Fill place holder list
    type_list <- lapply(simple_elements, function(element, fam_data_list, type, data_slot){
      
      # Iterate over familiarData objects.
      for(fam_data_object in fam_data_list){
        # Find the value in the particular familiarData object.
        data_value <- slot(object=fam_data_object, name=data_slot)[[type]][[element]]
        
        if(!is_empty(data_value)) break()
      }
      
      return(data_value)
    },
    fam_data_list=fam_data_list,
    type=type,
    data_slot=data_slot)
    
    # Add names to the list.
    names(type_list) <- simple_elements
    
    for(element in c("model_data", "bootstrap_data", extra_data)){
      
      # Capture and combine data for the particular element by iterating over
      # the familiarData objects.
      element_list <- lapply(fam_data_list, function(fam_obj, type, data_slot, element, more_identifiers){
        
        # Collect data from the element.
        data <- slot(object=fam_obj, name=data_slot)[[type]][[element]]

        # Check if the data is empty.
        if(is_empty(data)) return(NULL)
        
        # Add identifiers.
        data <- add_identifiers(data=data.table::copy(data),
                                object=fam_obj,
                                more_identifiers=more_identifiers)
        
        return(data)
      },
      type=type,
      data_slot=data_slot,
      element=element,
      more_identifiers=more_identifiers)
      
      # Combine and add to the type list.
      type_list[[element]] <- data.table::rbindlist(element_list, use.names=TRUE)
    }
    
    # Add to collection_list
    collection_list[[type]] <- type_list
  }
  
  return(collection_list)
}

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
    fs_vimp_table <- add_identifiers(data=fs_vimp_table, object=fam_data_list[[ii]], more_identifiers="fs_method")
    
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
    model_vimp_table <- add_identifiers(data=model_vimp_table, object=fam_data_list[[ii]], more_identifiers=c("fs_method", "learner"))
    
  }, fam_data_list=fam_data_list))
  
  # Set aggregation method and rank threshold. These do not change across a dataset
  model_vimp_list <- list("vimp_table" = model_vimp,
                          "aggregation_method" = fam_data_list[[unique_entries[1]]]@model_vimp$aggregation_method,
                          "rank_threshold" = fam_data_list[[unique_entries[1]]]@model_vimp$rank_threshold)

  return(model_vimp_list)
}



#' @title Collector for model hyperparameters
#'
#' @inheritParams collect_fs_vimp
#'
#' @return A list with aggregated hyperparameter information. Contains a data.table for each list element.
#' @noRd
collect_hyperparameters <- function(fam_data_list){
  # Hyperparameters are shared between objects with the same feature selection method, learner and data ids
  # unique_entries <- .which_unique_data(fam_data_list=fam_data_list, incl_learner=TRUE, incl_validation_status=FALSE)
  unique_entries <- .which_unique_data(fam_data_list=fam_data_list, by=c("fs_method", "learner"))

  # Select only unique entries
  hyperparameter_list <- lapply(unique_entries, function(ii, fam_data_list){
    
    # Get the model-based vimp table  
    hyperparameter_table <- fam_data_list[[ii]]@hyperparameters
    
    # Add identifiers
    hyperparameter_table <- add_identifiers(data=hyperparameter_table, object=fam_data_list[[ii]], more_identifiers=c("fs_method", "learner"))
    
  }, fam_data_list=fam_data_list)

  # Hyperparameters are expected to differ between different learners, therefore we identify unique learners.
  # Entries for different learners are then stored within different elements of the list.
  data_learners <- sapply(fam_data_list, function(fam_data_obj) (fam_data_obj@learner))[unique_entries]
  learners <- unique(data_learners)
  
  # Empty hyperparameter table
  output_list <- list()
  
  # Iterate over learners
  for(curr_learner in learners){
    # Find the entries in data_learners that match the currently selected learner, combine
    # corresponding hyperparameter tables and add to a named list entry.
    output_list[[curr_learner]] <- data.table::rbindlist(hyperparameter_list[which(data_learners==curr_learner)])
  }
  
  # Return the output list
  return(output_list)
}



#' @title Collector for calibration information
#'
#' @inheritParams collect_fs_vimp
#'
#' @return A list with aggregated calibration information. May be NULL for outcome types other than survival.
#' @noRd
collect_calibration_info <- function(fam_data_list){
  # Calibration info (e.g. baseline survival) is shared between objects with the same feature selection method and data ids.
  # It may also not exist.
  # unique_entries <- .which_unique_data(fam_data_list=fam_data_list, incl_learner=TRUE, incl_validation_status=FALSE)
  unique_entries <- .which_unique_data(fam_data_list=fam_data_list, by=c("fs_method", "learner"))
  
  # Select only unique entries
  calibr_info_table <- data.table::rbindlist(lapply(unique_entries, function(ii, fam_data_list){
    
    # Get the model-based vimp table  
    calibr_info_table <- fam_data_list[[ii]]@calibration_info
    
    # Add identifiers
    calibr_info_table <- add_identifiers(data=calibr_info_table, object=fam_data_list[[ii]], more_identifiers=c("fs_method", "learner"))
    
  }, fam_data_list=fam_data_list))
  
  # Check of the calibration info table is empty
  if(ncol(calibr_info_table) == 0){
    calibr_info_table <- NULL
  }
  
  return(calibr_info_table)
}



#' @title Collector for calibration data
#'
#' @details Calibration data consists of a linear test which assesses calibration at large, goodness-of-fit test (Hosmer-Lemeshow or Nam-D'Agostino) and the data used to perform these tests.
#'
#' @inheritParams collect_fs_vimp
#'
#' @return A list with aggregated calibration information. May be NULL for outcome types other than survival.
#' @noRd
collect_calibration_data <- function(fam_data_list){
  # Calibration data is not shared between different data objects. We don't need to identify unique entries.
  
  # Retrieve data for linear tests
  lin_test_table <- data.table::rbindlist(lapply(fam_data_list, function(fam_obj){
    
    # Get the linear test data
    lin_test_table <- fam_obj@calibration_data$linear_test
    
    # Add identifiers
    lin_test_table <- add_identifiers(data=lin_test_table, object=fam_obj, more_identifiers=c("fs_method", "learner"))
    
  } ))
  
  # Retrieve data for goodness-of-fit test
  gof_test_table <- data.table::rbindlist(lapply(fam_data_list, function(fam_obj){
    
    # Get the goodness-of-fit data
    gof_test_table <- fam_obj@calibration_data$gof_test
    
    # Add identifiers
    gof_test_table <- add_identifiers(data=gof_test_table, object=fam_obj, more_identifiers=c("fs_method", "learner"))
    
  }))
  
  # Retrieve raw calibration fit data
  raw_calibr_data <- data.table::rbindlist(lapply(fam_data_list, function(fam_obj){
    
    # Get the raw data
    raw_calibr_data <- fam_obj@calibration_data$data
    
    # Add identifiers
    raw_calibr_data <- add_identifiers(data=raw_calibr_data, object=fam_obj, more_identifiers=c("fs_method", "learner"))
    
  }))
  
  # Add to list
  calibration_data_list <- list("linear_test"=lin_test_table, "gof_test"=gof_test_table, "data"=raw_calibr_data)
  
  return(calibration_data_list)
}


#' @title Collector for prediction tables
#'
#' @details Prediction tables exist for single models and ensemble models. These are collected here
#'
#' @inheritParams collect_fs_vimp
#'
#' @return A list with aggreagted prediction data.
#' @noRd
collect_prediction_data <- function(fam_data_list){
  # Model predictions are not shared between different data objects. We don't need to identify unique entries.
  
  return(universal_collector(fam_data_list=fam_data_list,
                             data_slot="prediction_data",
                             extra_data=NULL,
                             more_identifiers=c("fs_method", "learner")))
}


#' @title Collector for performance data
#'
#' @details Collects performance metric data.
#'
#' @inheritParams collect_fs_vimp
#'
#' @return A list with aggregated model performance information.
#' @noRd
collect_model_performance <- function(fam_data_list){
  # Model performance data is not shared between different data objects. We
  # don't need to identify unique entries.
  
  return(universal_collector(fam_data_list=fam_data_list,
                             data_slot="model_performance",
                             extra_data=NULL,
                             more_identifiers=c("fs_method", "learner")))
}



#' @title A collector for decision curve analysis data
#'
#' @inheritParams collect_fs_vimp
#'
#' @return A list with aggregated data.
#' @noRd
collect_decision_curve_analysis_data <- function(fam_data_list){
  
  return(universal_collector(fam_data_list=fam_data_list,
                             data_slot="decision_curve_data",
                             extra_data="intervention_all",
                             more_identifiers=c("fs_method", "learner")))
}



#' @title Collector for stratification information
#'
#' @inheritParams collect_fs_vimp
#'
#' @return A list with aggregated cutoffs.
#' @noRd
collect_stratification_info <- function(fam_data_list){
  # Stratification info (e.g. cutoffs) is shared between objects with the same feature selection method and data ids,
  # as they are generated at model creation.
  unique_entries <- .which_unique_data(fam_data_list=fam_data_list, by=c("fs_method", "learner"))

  # Select only unique entries
  stratification_info_table <- data.table::rbindlist(lapply(unique_entries, function(ii, fam_data_list){
    
    # Get the model-based vimp table  
    strat_info_table <- fam_data_list[[ii]]@km_info
    
    # Check if the table is empty
    if(is_empty(strat_info_table)){
      return(NULL)
    }
    
    # Add identifiers
    strat_info_table <- add_identifiers(data=strat_info_table, object=fam_data_list[[ii]], more_identifiers=c("fs_method", "learner"))
    
  }, fam_data_list=fam_data_list))
  
  # Check of the calibration info table is empty
  if(ncol(stratification_info_table) == 0){
    stratification_info_table <- NULL
  }
  
  return(stratification_info_table)
}


#' @title Collector for stratification data
#'
#' @inheritParams collect_fs_vimp
#'
#' @return A list of lists with aggregated stratification data.
#' @noRd
collect_stratification_data <- function(fam_data_list){
  
  # Determine the stratification methods that were used.
  stratification_methods <- unique(unlist(lapply(fam_data_list, function(fam_data) (names(fam_data@km_data$data)))))
  
  # Iterate over stratification methods
  stratification_data <- lapply(stratification_methods, function(strat_method, fam_data_list){
    
    # Collect stratification data
    stratification_data <-lapply(fam_data_list, function(fam_data, strat_method){
      
      # Extract stratification data
      stratification_data <- fam_data@km_data$data[[strat_method]]
      
      if(is_empty(stratification_data)) return(NULL)
      
      # Add identifiers
      stratification_data <- add_identifiers(data=stratification_data, object=fam_data,
                                             more_identifiers=c("fs_method", "learner"))
      
      return(stratification_data)
      
    }, strat_method=strat_method)
    
    # Concatenate list to table
    stratification_data <- data.table::rbindlist(stratification_data, use.names=TRUE)
    
    if(is_empty(stratification_data)) stratification_data <- NULL
    
    
    # Collect logrank test information
    logrank_test_info <- lapply(fam_data_list, function(fam_data, strat_method){
      
      # Extract logrank test info
      logrank_test_info <- fam_data@km_data$test[[strat_method]]$logrank
      
      if(is_empty(logrank_test_info)) return(NULL)
      
      # Add identifiers
      logrank_test_info <- add_identifiers(data=logrank_test_info, object=fam_data,
                                           more_identifiers=c("fs_method", "learner"))
      
      return(logrank_test_info)
      
    }, strat_method=strat_method)
    
    # Concatenate list to table
    logrank_test_info <- data.table::rbindlist(logrank_test_info, use.names=TRUE)
    
    if(is_empty(logrank_test_info)) logrank_test_info <- NULL
    
    
    # Collect hazard-ratio test information
    hr_test_info <- lapply(fam_data_list, function(fam_data, strat_method){
      
      # Extract hazard ratio test info
      hr_test_info <- fam_data@km_data$test[[strat_method]]$hr_ratio
      
      if(is_empty(hr_test_info)) return(NULL)
      
      # Add identifiers
      hr_test_info <- add_identifiers(data=hr_test_info, object=fam_data,
                                      more_identifiers=c("fs_method", "learner"))
      
      return(hr_test_info)
      
    }, strat_method=strat_method)
    
    # Concatenate list to table
    hr_test_info <- data.table::rbindlist(hr_test_info)
    
    if(is_empty(hr_test_info)) hr_test_info <- NULL
    
    
    # Extract time_max
    time_max <- unlist(sapply(fam_data_list, function(fam_data) (fam_data@km_data$time_max)))[1]
    
    return(list("data"=stratification_data, "logrank"=logrank_test_info, "hr_ratio"=hr_test_info, "time_max"=time_max))
    
  }, fam_data_list=fam_data_list)
  
  # Add name of stratification methods to the list elements
  names(stratification_data) <- stratification_methods
  
  return(stratification_data)
}


#' @title Collector for AUC plotting data
#'
#' @inheritParams collect_fs_vimp
#'
#' @return A list with aggregated AUC curve information.
#' @noRd
collect_auc_data <- function(fam_data_list){
  # AUC data is not shared between different familiarData objects. We don't need
  # to identify unique entries.
  
  return(universal_collector(fam_data_list=fam_data_list,
                             data_slot="auc_data",
                             extra_data=NULL,
                             more_identifiers=c("fs_method", "learner")))
}


#' @title Collector for confusion matrices
#'
#' @inheritParams collect_fs_vimp
#'
#' @return A list with aggregated confusion matrices.
#' @noRd
collect_confusion_matrix_data <- function(fam_data_list){
  
  return(universal_collector(fam_data_list=fam_data_list,
                             data_slot="confusion_matrix",
                             extra_data=NULL,
                             more_identifiers=c("fs_method", "learner")))
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
    univar_data_table <- add_identifiers(data=univar_data_table, object=fam_data_list[[ii]],
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
      expression_table <- add_identifiers(data=expression_table, object=fam_obj, more_identifiers=c("fs_method", "learner"))
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
      mutual_correlation_data <- add_identifiers(data=mutual_correlation_data,
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
    
    # Create type list and add the confidence level.
    type_list <- list("confidence_level"=slot(object=fam_data_list[[1]],
                                                    name=data_slot)[[type]]$confidence_level,
                            "bootstrap_ci_method"=slot(object=fam_data_list[[1]],
                                                       name=data_slot)[[type]]$bootstrap_ci_method)
    
    for(element in c("model_data", "bootstrap_data", extra_data)){
      
      # Capture and combine data for the particular element by iterating over
      # the familiarData objects.
      element_list <- lapply(fam_data_list, function(fam_obj, type, data_slot, element, more_identifiers){
        
        # Collect data from the element.
        data <- slot(object=fam_obj, name=data_slot)[[type]][[element]]

        # Check if the data is empty.
        if(is_empty(data)) return(NULL)
        
        # Add identifiers.
        data <- add_identifiers(data=data, object=fam_obj, more_identifiers=more_identifiers)
        
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

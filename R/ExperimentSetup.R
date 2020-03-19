#' Parse experimental design
#'
#' @inheritParams .parse_experiment_settings
#'
#' @details This function converts the experimental_design string
#'
#' @return data.table with subsampler information at different levels of the
#' experimental design.
#'
#' @md
#' @keywords internal
extract_experimental_setup <- function(experimental_design, file_dir){
  
  if(.experimental_design_is_file(file_dir=file_dir, experimental_design=experimental_design)){
    return(waiver())
  }
  
  # Remove all whitespace
  experimental_design <- gsub(pattern=" ", replacement="", x=experimental_design)
  
  # Generate a section table
  section_table <- .get_experimental_design_section_table(experimental_design=experimental_design)
  
  # Identify the subsampler algorithms
  section_table <- .complete_experimental_design_section_table(section_table=section_table, experimental_design=experimental_design)
  
  # Check consistency of the table, e.g. feature selection should only appear once, etc.
  .check_experimental_design_section_table(section_table=section_table)
  
  # Report experimental design to the user.
  .report_experimental_design(section_table=section_table)
  
  return(section_table)
}


.experimental_design_is_file <- function(file_dir, experimental_design){
  # Check if the experimental design argument is actually a path to a file.

  # Check if the file exists at all.
  if(!file.exists(file.path(file_dir, experimental_design)) & !file.exists(experimental_design)){
    return(FALSE)
  }
  
  # Check whether the file is an RDS file.
  if(tolower(tools::file_ext(basename(experimental_design))) == "rds"){
    return(TRUE)
    
  } else {
    return(FALSE)
  }
}


.get_experimental_design_section_table <- function(experimental_design){

  # Determine the number of experimental levels
  # First we locate the position of parentheses
  left_parenthesis  <- stringi::stri_locate_all(pattern="(", str=experimental_design, fixed=TRUE)[[1]][,1]
  right_parenthesis <- stringi::stri_locate_all(pattern=")", str=experimental_design, fixed=TRUE)[[1]][,1]
  
  # Subsequently generate the corresponding experimental levels
  experiment_levels <- integer(nchar(experimental_design))
  for(ii in right_parenthesis){
    experiment_levels[1:ii] <- experiment_levels[1:ii] + 1
  }
  for(ii in left_parenthesis){
    experiment_levels[1:(ii-1)] <- experiment_levels[1:(ii-1)] - 1
  }
  
  # Generate setup sections
  sections        <- rle(experiment_levels)
  sections$end    <- cumsum(sections$lengths)
  sections$start  <- sections$end - sections$lengths + 1
  class(sections) <- NULL
  
  # Set up data table with experimental sections
  section_table     <- data.table::as.data.table(sections)
  section_table[, "lengths":=NULL]
  data.table::setnames(section_table, c("values", "end", "start"), c("exp_level_id", "sect_end", "sect_start"))
  
  # Set up columns to be filled
  section_table[, ":="("ref_data_id"=0, "main_data_id"=0, "feat_sel"=FALSE, "model_building"=FALSE,
                     "external_validation"=FALSE, "perturb_method"="none", "perturb_n_rep"=0,
                     "perturb_n_folds"=0)]
  
  return(section_table)
}


.complete_experimental_design_section_table <- function(section_table, experimental_design){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  exp_level_id <- sect_start <- perturb_method <- NULL
  
  # Iterator
  main_data_id_iter <- 1
  
  #####Identify samplers-------------------------------------------------------------------------------
  # Iterate over sections to set main_data_id
  for(ii in 1:nrow(section_table)){
    
    # Check if ip, bt, lv or cv preceeds the current section
    if(section_table$sect_start[ii] > 2){
      sampler_str <- stringi::stri_sub(str=experimental_design, from=section_table$sect_start[ii]-2,
                                       to=section_table$sect_start[ii]-1)
      
      # Check for imbalance partition (ip), bootstrap (bt), cross-validation (cv) and leave-one-out-cross-validation (lv)
      if(sampler_str=="bt"){
        section_table$perturb_method[ii] <- "bootstrap"
        section_table$main_data_id[ii]   <- main_data_id_iter
        main_data_id_iter                <- main_data_id_iter + 1
      } else if(sampler_str=="cv"){
        section_table$perturb_method[ii] <- "cross_val"
        section_table$main_data_id[ii]   <- main_data_id_iter
        main_data_id_iter                <- main_data_id_iter + 1
      } else if(sampler_str=="lv"){
        section_table$perturb_method[ii] <- "loocv"
        section_table$main_data_id[ii]   <- main_data_id_iter
        main_data_id_iter                <- main_data_id_iter + 1
      } else if(sampler_str=="ip"){
        section_table$perturb_method[ii] <- "imbalance_part"
        section_table$main_data_id[ii]   <- main_data_id_iter
        main_data_id_iter                <- main_data_id_iter + 1
      }
      
      rm(sampler_str)
    } else {
      section_table$perturb_method[ii]   <- "main"
      section_table$main_data_id[ii]     <- main_data_id_iter
      main_data_id_iter                  <- main_data_id_iter + 1
    }
  }
  
  rm(main_data_id_iter)
  
  
  #####Add main_data_id and ref_data_id--------------------------------------------
  # Iterate over sections to set main_data_id (where missing) and ref_data_id
  for(ii in 1:nrow(section_table)){
    
    if(section_table$main_data_id[ii]==0){
      # Make subselection of data at the same level and select only preceding sections
      dt_sub <- section_table[exp_level_id==section_table$exp_level_id[ii] & sect_start < section_table$sect_start[ii], ]
      
      # Set data id from nearest preceding lower level section
      section_table$main_data_id[ii] <- dt_sub$main_data_id[nrow(dt_sub)]
      
      rm(dt_sub)
    }
    
    # Set reference data id
    if(section_table$exp_level_id[ii]==0) {
      # At the lowest level (main) there is no reference
      section_table$ref_data_id[ii] <- 0
      
    } else {
      # Make subselection of data one level higher and select only preceding sections
      dt_sub <- section_table[exp_level_id==section_table$exp_level_id[ii]-1 & sect_start < section_table$sect_start[ii], ]
      
      # Set data id from nearest preceding lower level section
      section_table$ref_data_id[ii] <- dt_sub$main_data_id[nrow(dt_sub)]
      
      rm(dt_sub)
    }
  }
  
  #####Complete details-------------------------------------------------------------------------
  # Add details for perturbations and other sections
  for(ii in 1:nrow(section_table)){
    
    if(section_table$perturb_method[ii] %in% c("main", "bootstrap", "cross_val", "loocv", "imbalance_part")){
      
      # Create readable string for current data id
      curr_data_id_str <- NULL
      for(jj in which(section_table$main_data_id==section_table$main_data_id[ii])){
        curr_data_id_str <- c(curr_data_id_str, stringi::stri_sub(experimental_design, from=section_table$sect_start[jj], to=section_table$sect_end[jj]))
      }
      curr_data_id_str <- paste0(curr_data_id_str, collapse="")
      
      # Drop parentheses and split string by comma
      curr_data_id_str <- gsub(pattern="\\(|\\)", replacement="", x=curr_data_id_str)
      curr_data_id_str <- stringi::stri_split(str=curr_data_id_str, fixed=",")[[1]]
      
      # Check if feature selection is included in the current section
      if(grepl(pattern="fs", x=curr_data_id_str[1])){
        section_table$feat_sel[ii] <- TRUE
      }
      
      # Check if model building is included in the current section
      if(grepl(pattern="mb", x=curr_data_id_str[1])){
        section_table$model_building[ii] <- TRUE
      }
      
      # Check if external validation is included in the current section
      if(grepl(pattern="ev", x=curr_data_id_str[1])){
        section_table$external_validation[ii] <- TRUE
      }
      
      # Read bootstrap data
      if(section_table$perturb_method[ii]=="bootstrap"){
        if(length(curr_data_id_str) < 2){
          stop(paste("The number of bootstraps should be indicated when using the bt (bootstrap) subsampler.",
                     "None was found."))
        }
        
        # Determine the number of bootstraps
        n_reps <- .perform_type_conversion(x=curr_data_id_str[2], to_type="integer", var_name="The number of bootstraps", req_length=1)
        
        # Check whether the number of bootstraps is at least 1
        .check_number_in_valid_range(x=n_reps, var_name="The number of bootstraps", range=c(1, Inf))

        # Add the number of bootstraps to the section table
        section_table$perturb_n_rep[ii] <- n_reps
      }
      
      # Read cross-validation settings
      if(section_table$perturb_method[ii]=="cross_val"){
        if(length(curr_data_id_str) < 2){
          stop(paste("The number of folds should be indicated when using the cv (cross-validation) subsampler.",
                     "None was found."))
        }
        
        # Determine the number of folds
        n_folds <- .perform_type_conversion(x=curr_data_id_str[2], to_type="integer",
                                            var_name="The number of cross-validation folds", req_length=1)
        
        # Check whether the number of folds is at least 2
        .check_number_in_valid_range(x=n_folds, var_name="The number of cross-validations folds", range=c(2, Inf))
        
        # Add number of folds to the section_table
        section_table$perturb_n_folds[ii] <- n_folds
        
        # Check the number of repetitions
        if(length(curr_data_id_str) >= 3){
          
          n_reps <- .perform_type_conversion(x=curr_data_id_str[3], to_type="integer",
                                             var_name="The number of cross-validation repetitions", req_length=1)
          
          # Check whether the number of CV repetitions is at least 1
          .check_number_in_valid_range(x=n_folds, var_name="The number of cross-validations repetitions", range=c(1, Inf))
          
          # Add number of repetitions to the section table
          section_table$perturb_n_rep[ii] <- n_reps
        } else {
          
          # Set the number of repetitions to one
          section_table$perturb_n_rep[ii] <- 1L
        }
      }
      
      # Read leave-one-out-cross-validation settings
      if(section_table$perturb_method[ii]=="loocv"){
        section_table$perturb_n_folds[ii] <- -1L
        section_table$perturb_n_rep[ii]   <- 1L
      }
      
      rm(curr_data_id_str, jj)
    }
  }
  
  # Remove unnessary rows and columns
  section_table <- section_table[perturb_method!="none", ]
  section_table[, ":="("exp_level_id"=NULL, "sect_end"=NULL, "sect_start"=NULL)]
  
  return(section_table)
}


.report_experimental_design <- function(section_table){
  # Suppress NOTES due to non-standard evaluation in data.table
  feat_sel <- model_building <- main_data_id <- NULL
  
  # Report on validation data:
  if(any(section_table$external_validation)){
    logger.message("Setup report: Validation is external.")
  } else{
    logger.message("Setup report: Validation is internal only.")
  }
  
  # Report on model building and feature selection
  if(any(section_table$feat_sel * section_table$model_building)){
    main_message <- "Setup report: Feature selection and model building on"
    
    # Iteratively append message
    dt_sub <- section_table[feat_sel==TRUE & model_building==TRUE, ]
    curr_ref_data_id <- dt_sub$main_data_id[1]
    while(curr_ref_data_id > 0){
      dt_sub <- section_table[main_data_id==curr_ref_data_id, ]
      if(dt_sub$perturb_method[1]=="main")           { main_message <- c(main_message, "the training data.") }
      if(dt_sub$perturb_method[1]=="bootstrap")      { main_message <- c(main_message, paste0(dt_sub$perturb_n_rep[1], " bootstraps of")) }
      if(dt_sub$perturb_method[1]=="cross_val")      { main_message <- c(main_message, paste0(dt_sub$perturb_n_rep[1], " repetitions of ", dt_sub$perturb_n_folds, "-fold cross validation of")) }
      if(dt_sub$perturb_method[1]=="loocv")          { main_message <- c(main_message, "folds of leave-one-out-cross-validation of") }
      if(dt_sub$perturb_method[1]=="imbalance_part") { main_message <- c(main_message, "class-balanced partitions of")}
      
      curr_ref_data_id <- dt_sub$ref_data_id[1]
    }
    
    logger.message(paste0(main_message, collapse=" "))
    
  } else {
    # Feature selection first
    main_message <- "Setup report: Feature selection on"
    
    # Iteratively append message
    dt_sub <- section_table[feat_sel==TRUE, ]
    curr_ref_data_id <- dt_sub$main_data_id[1]
    while(curr_ref_data_id > 0){
      dt_sub <- section_table[main_data_id==curr_ref_data_id, ]
      if(dt_sub$perturb_method[1]=="main")     { main_message <- c(main_message, "the training data.") }
      if(dt_sub$perturb_method[1]=="bootstrap"){ main_message <- c(main_message, paste0(dt_sub$perturb_n_rep[1], " bootstraps of")) }
      if(dt_sub$perturb_method[1]=="cross_val"){ main_message <- c(main_message, paste0(dt_sub$perturb_n_rep[1], " repetitions of ", dt_sub$perturb_n_folds, "-fold cross validation of")) }
      if(dt_sub$perturb_method[1]=="loocv")    { main_message <- c(main_message, "folds of leave-one-out-cross-validation of") }
      if(dt_sub$perturb_method[1]=="imbalance_part") { main_message <- c(main_message, "class-balanced partitions of")}
      
      curr_ref_data_id <- dt_sub$ref_data_id[1]
    }
    
    logger.message(paste0(main_message, collapse=" "))
    
    # Model building second
    main_message <- "Setup report: Model building on"
    
    # Iteratively append message
    dt_sub <- section_table[model_building==TRUE, ]
    curr_ref_data_id <- dt_sub$main_data_id[1]
    while(curr_ref_data_id > 0){
      dt_sub <- section_table[main_data_id==curr_ref_data_id, ]
      if(dt_sub$perturb_method[1]=="main")     { main_message <- c(main_message, "the training data.") }
      if(dt_sub$perturb_method[1]=="bootstrap"){ main_message <- c(main_message, paste0(dt_sub$perturb_n_rep[1], " bootstraps of")) }
      if(dt_sub$perturb_method[1]=="cross_val"){ main_message <- c(main_message, paste0(dt_sub$perturb_n_rep[1], " repetitions of ", dt_sub$perturb_n_folds, "-fold cross validation of")) }
      if(dt_sub$perturb_method[1]=="loocv")    { main_message <- c(main_message, "folds of leave-one-out-cross-validation of") }
      if(dt_sub$perturb_method[1]=="imbalance_part") { main_message <- c(main_message, "class-balanced partitions of")}
      
      curr_ref_data_id <- dt_sub$ref_data_id[1]
    }
    
    logger.message(paste0(main_message, collapse=" "))
  }
}


.check_experimental_design_section_table <- function(section_table){
  if(sum(section_table$feat_sel) > 1){
    stop("The fs component for feature selection may only be used once in the experimental design.")
  }
  
  if(sum(section_table$feat_sel) == 0){
    stop("The fs component for feature selection must appear in the experimental design. It was not found.")
  }
  
  if(sum(section_table$model_building) > 1){
    stop("The mb component for model building may only be used once in the experimental design.")
  }
  
  if(sum(section_table$model_building) == 0){
    stop("The mb component for model building must appear in the experimental design. It was not found.")
  }
  
  if(sum(section_table$external_validation) > 1){
    stop("The ev component for external validation can only appear once in the experimental design.")
  }
  
}

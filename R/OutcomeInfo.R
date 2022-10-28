#' @include FamiliarS4Generics.R
#' @include FamiliarS4Classes.R


create_outcome_info <- function(settings){
  
  outcome_info <- methods::new("outcomeInfo",
                               name = settings$data$outcome_name,
                               outcome_type = settings$data$outcome_type,
                               outcome_column = settings$data$outcome_col)
  
  if(outcome_info@outcome_type %in% c("binomial", "multinomial")){
    # Set class levels
    outcome_info@levels <- settings$data$class_levels
    
    # Set flag indicating that the outcome is ordinal (currently not enabled)
    outcome_info@ordered <- FALSE
    
    # Set reference level of the outcome
    outcome_info@reference <- settings$data$class_levels[1]
  }
  
  if(outcome_info@outcome_type %in% c("survival", "competing_risk")){
    # Set maximum time
    outcome_info@time <- ifelse(is.numeric(settings$eval$time_max), settings$eval$time_max, Inf)
    
    # Set indicator for censoring
    outcome_info@censored <- as.character(settings$data$censoring_indicator)
    
    # Set indicator for events
    outcome_info@event <- as.character(settings$data$event_indicator)
  }
  
  if(outcome_info@outcome_type %in% c("competing_risk")){
    # Set indicator for competing risks
    outcome_info@competing_risk <- as.character(settings$data$competing_risk_indicator)
  }
  
  return(outcome_info)
}


create_outcome_info_from_data <- function(data){
  # This is typically an outcomeInfo object created at runtime, without access
  # to outcome_info in the global backend, or attached to an object.
  
  outcome_info <- methods::new("outcomeInfo",
                               name = character(0L),
                               outcome_type = data@outcome_type,
                               outcome_column = get_outcome_columns(x=data))
  
  if(outcome_info@outcome_type %in% c("binomial", "multinomial")){
    # Set class levels
    outcome_info@levels <- get_outcome_class_levels(x=data)
    
    # Set flag indicating that the outcome is ordinal (currently not enabled)
    outcome_info@ordered <- is.ordered(data@data[[outcome_info@outcome_column]])
    
    # Set reference level of the outcome
    outcome_info@reference <- outcome_info@levels[1]
  }
  
  if(outcome_info@outcome_type %in% c("survival", "competing_risk")){
    # Set maximum time
    outcome_info@time <- Inf
    
    # Set indicator for censoring
    outcome_info@censored <- "0"
    
    # Set indicator for events
    outcome_info@event <- "1"
  }
  
  if(outcome_info@outcome_type %in% c("competing_risk")){
    # Set indicator for competing risks
    outcome_info@competing_risk <- as.character(setdiff(unique_na(data@data[[outcome_info@outcome_column[2]]]), c(0, 1)))
  }
  
  return(outcome_info)
}



.assign_outcome_info_to_global <- function(outcome_info){
  # Assign outcome_info in the familiar global environment
  assign("outcome_info", outcome_info, envir=familiar_global_env)
}



get_outcome_info_from_backend <- function(){
  
  # Retrieve the paths to files and directories
  if(exists("familiar_global_env")){
    if(exists("outcome_info", where=familiar_global_env)){
      data_env <- familiar_global_env
    } else if (exists("outcome_info", where=.GlobalEnv)){
      data_env <- .GlobalEnv
    } else {
      stop("The outcomeInfo object was not found in backend environment.")
    }
  } else if (exists("outcome_info", where=.GlobalEnv)){
    data_env <- .GlobalEnv
  } else {
    stop("The outcomeInfo object was not found in backend environment.")
  }
  
  return(get("outcome_info", envir=data_env))
}



.get_outcome_info <- function(x=NULL){
  # Function to retrieve outcome_info in a generic manner.
  
  # Placeholder outcome_info
  outcome_info <- NULL
  
  # First, attempt to obtain from familiarModel and similar objects.
  if(inherits(x, c("familiarModel", "familiarEnsemble", "familiarData", "familiarCollection"))){
    if(!is.null(x@outcome_info)) return(x@outcome_info)
  }
  
  # Second attempt to get from backend
  outcome_info <- tryCatch(get_outcome_info_from_backend(),
                           error=function(err) return(NULL))
  
  if(!is.null(outcome_info)) return(outcome_info)
  
  # Third, attempt to infer from settings.
  settings <- tryCatch(get_settings(),
                       error=function(err) return(NULL))
  
  if(!is.null(settings)){
    return(create_outcome_info(settings=settings))
  }
  
  # Finally, attempt to infer from dataObject
  if(inherits(x, "dataObject")){
    return(create_outcome_info_from_data(data=x))
  }
  
  if(is.null(outcome_info)){
    stop("The requested outcomeInfo object could not be read or created on the fly.")
  }
}



.aggregate_outcome_info <- function(x){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  min <- Q1 <- median <- Q3 <- max <- count <- incidence <- survival_probability <- survival_probability <- NULL
  
  # Copy the first outcome info object
  outcome_info <- x[[1]]
  
  # Find distribution items
  distribution_items <- names(outcome_info@distribution)
  
  if(!is.null(distribution_items)){
    
    # Placeholder list
    distr_list <- list()
    
    # Iterate over items in the distribution list
    for(item in distribution_items){
      
      if(grepl(pattern="fivenum", x=item, fixed=TRUE)){
        
        # Aggregate from list
        fivenum_values <- lapply(x, function(outcome_info_object, item) (outcome_info_object@distribution[[item]]), item=item)
        
        # Combine all the data.tables
        fivenum_values <- data.table::rbindlist(fivenum_values)
        
        # Check for zero-length lists.
        if(is_empty(fivenum_values)) next()
        
        # Summarise
        fivenum_values <- fivenum_values[, list("min"=min(min),
                                                "Q1"=mean(Q1),
                                                "median"=mean(median),
                                                "Q3"=mean(Q3),
                                                "max"=max(max)), ]
        
        # Add to list
        distr_list[[item]] <- fivenum_values
        
      } else if(grepl(pattern="frequency", x=item, fixed=TRUE)){
        
        # Aggregate from list
        frequency_values <- lapply(x, function(outcome_info_object, item) (outcome_info_object@distribution[[item]]), item=item)
        
        # Combine all the data.tables
        frequency_values <- data.table::rbindlist(frequency_values)
        
        if(is_empty(frequency_values)) next()
        
        # Summarise and add to list
        distr_list[[item]] <- frequency_values[, list("count"=mean(count)), by="outcome"]
        
      } else if(grepl(pattern="incidence", x=item, fixed=TRUE)){
        
        # Identify unique times.
        unique_times <- sort(unique(unlist(lapply(x, function(outcome_info_object, item) (outcome_info_object@distribution[[item]]$time), item=item))))
        
        # Interpolate at the unique times.
        incidence_table <- lapply(x, function(outcome_info_object, item, unique_times){
          
          # Check that the object is not empty.
          if(is_empty(outcome_info_object@distribution[[item]])) return(NULL)
          
          # Check that there is at least unique times.
          if(data.table::uniqueN(outcome_info_object@distribution[[item]]$time) < 2) return(NULL)
          
          # Interpolate the data at the unique time points.
          incidence <- stats::approx(x=outcome_info_object@distribution[[item]]$time,
                                     y=outcome_info_object@distribution[[item]][[item]],
                                     xout=unique_times,
                                     method="linear",
                                     rule=2,
                                     yleft=0.0)$y
          
          return(data.table::data.table("time"=unique_times,
                                        "incidence"=incidence))
        },
        item=item,
        unique_times=unique_times)
        
        # Aggregate the incidence table.
        incidence_table <- data.table::rbindlist(incidence_table)
        
        # Compute the mean incidence by each time point.
        incidence_table <- incidence_table[, list("incidence"=mean(incidence)), by="time"]
        
        # Update the column name.
        data.table::setnames(incidence_table, old="incidence", new=item)
        
        # Add to list
        distr_list[[item]] <- incidence_table
      
      } else if(grepl(pattern="survival_probability", x=item, fixed=TRUE)) {
        
        # Identify unique times.
        unique_times <- sort(unique(unlist(lapply(x, function(outcome_info_object, item) (outcome_info_object@distribution[[item]]$time), item=item))))
        
        # Interpolate at the unique times.
        survival_table <- lapply(x, function(outcome_info_object, item, unique_times){
          
          # Check that the object is not empty.
          if(is_empty(outcome_info_object@distribution[[item]])) return(NULL)
          
          # Check that there is at least unique times.
          if(data.table::uniqueN(outcome_info_object@distribution[[item]]$time) < 2) return(NULL)
          
          # Interpolate the data at the unique time points.
          survival_probability <- stats::approx(x=outcome_info_object@distribution[[item]]$time,
                                                y=outcome_info_object@distribution[[item]][[item]],
                                                xout=unique_times,
                                                method="linear",
                                                rule=2,
                                                yleft=1.0)$y
          
          return(data.table::data.table("time"=unique_times,
                                        "survival_probability"=survival_probability))
        },
        item=item,
        unique_times=unique_times)
        
        # Aggregate the survival probability table.
        survival_table <- data.table::rbindlist(survival_table)
        
        # Compute the mean survival prob by each time point.
        survival_table <- survival_table[, list("survival_probability"=mean(survival_probability)), by="time"]
        
        # Update the column name.
        data.table::setnames(survival_table, old="survival_probability", new=item)
        
        # Add to list
        distr_list[[item]] <- survival_table
        
      } else {
        # Find mean value
        distr_list[[item]] <- mean(unlist(extract_from_slot(x, "distribution", item, na.rm=TRUE)))
      }
      
    }
    
    # Update distribution slot
    outcome_info@distribution <- distr_list
  }
  
  # Update transformation parameters
  if(!is.null(outcome_info@transformation_parameters)){
    transform_method <- get_mode(extract_from_slot(object_list=x, slot_name="transformation_parameters", slot_element="transform_method"))
    transform_lambda <- get_mode(extract_from_slot(object_list=x, slot_name="transformation_parameters", slot_element="transform_lambda"))
    
    outcome_info@transformation_parameters <- list("transform_method" = transform_method,
                                                   "transform_lambda" = transform_lambda)
  }
  
  # Update normalisation parameters
  if(!is.null(outcome_info@normalisation_parameters)){
    normalisation_method <- get_mode(extract_from_slot(object_list=x, slot_name="normalisation_parameters", slot_element="norm_method"))
    normalisation_shift  <- mean(extract_from_slot(object_list=x, slot_name="normalisation_parameters", slot_element="norm_shift"))
    normalisation_scale  <- mean(extract_from_slot(object_list=x, slot_name="normalisation_parameters", slot_element="norm_scale"))
    
    outcome_info@normalisation_parameters <- list("norm_method" = normalisation_method,
                                                  "norm_shift" = normalisation_shift,
                                                  "norm_scale" = normalisation_scale)
  }
  
  return(outcome_info)
}



.compute_outcome_distribution_data <- function(object, data){

  # Suppress NOTES due to non-standard evaluation in data.table
  repetition_id <- event <- censored <- competing <- NULL
  interval_survival <- interval_incidence_rate <- interval_incidence_rate <- NULL
  outcome_event <- outcome_time <- n <- interval_non_censoring_rate <- survival_probability <- NULL
  
  # Get standard outcome columns
  outcome_columns <- get_outcome_columns(x=data)
  
  # Check for empty datasets, and return without setting distribution info.
  if(is_empty(data, allow_no_features=TRUE)) return(object)
  
  # Placeholder distribution list
  distr_list <- list()
  
  # Find outcome data
  x <- data.table::copy(unique(data@data, by=get_id_columns(id_depth="series")))

  if(object@outcome_type %in% c("binomial", "multinomial")){
    
    # Number of samples
    distr_list[["n"]] <- nrow(x)
    
    # Number of instances for each class
    distr_list[["frequency"]] <- x[, list("count"=.N), by="outcome"]
    
  } else if(object@outcome_type %in% c("continuous", "count")){
    
    # Number of samples
    distr_list[["n"]] <- nrow(x)
    
    # Five-number summary of outcome values
    distr_list[["fivenum"]] <- fivenum_summary(x$outcome, na.rm=TRUE)
    
    # Mean value
    distr_list[["mean"]] <- mean(x$outcome, na.rm=TRUE)
    
    distr_list[["median"]] <- stats::median(x$outcome, na.rm=TRUE)
    
  } else if(object@outcome_type %in% c("survival", "competing_risk")){
    
    # Baseline survival
    surv_group <- data.table::copy(x)
    
    # Count events and censoring at each time point.
    surv_group <- surv_group[, list("event"=sum(outcome_event == 1),
                                    "censored"=sum(outcome_event == 0),
                                    "competing"=sum(outcome_event > 1)),
                             by="outcome_time"][order(outcome_time)]
    
    # Add group sizes at the start of each interval.
    surv_group[, "n":=nrow(x) - data.table::shift(cumsum(event + censored + competing), n=1, fill=0, type="lag")]
    
    # Compute the incidence and censoring rates in the interval
    surv_group[, ":="("interval_survival"= 1.0 - (event + competing) / n,
                      "interval_incidence_rate"=event/ n,
                      "interval_non_censoring_rate"=1.0 - censored / n)]
    
    # Compute the Kaplan-Meier survival estimator and censoring estimator.
    surv_group[, ":="("survival_probability"=cumprod(interval_survival),
                      "cumulative_censoring"=1.0 - cumprod(interval_non_censoring_rate))]
    
    # Compute cumulative incidence and censoring rates.
    surv_group[, ":="("cumulative_incidence"=cumsum(data.table::shift(survival_probability, n=1L, fill=1.0, type="lag") * interval_incidence_rate))]
    
    # Rename the outcome_time column.
    data.table::setnames(surv_group, old="outcome_time", new="time")
    
    # Retain only import
    surv_group <- surv_group[, c("time", "survival_probability", "cumulative_incidence", "cumulative_censoring")]
    
    if(!any(surv_group$time == 0.0)){
      surv_group <- rbind(data.table::data.table("time"=0.0,
                                                 "survival_probability"=1.0,
                                                 "cumulative_incidence"=0.0,
                                                 "cumulative_censoring"=0.0),
                          surv_group)
    }
    
    # Number of samples
    distr_list[["n"]] <- nrow(x)
    
    # Number of events
    distr_list[["n_event"]] <- sum(x$outcome_event == 1, na.rm=TRUE)
    
    # Five-number summary of follow-up
    distr_list[["follow_up_fivenum"]] <- fivenum_summary(x$outcome_time, na.rm=TRUE)
    
    # Five-number summary of event
    distr_list[["event_fivenum"]] <- fivenum_summary(x[outcome_event == 1, ]$outcome_time, na.rm=TRUE)

    # Survival probability
    distr_list[["survival_probability"]] <- unique(surv_group[, c("time", "survival_probability")])
    
    # Cumulative incidence
    distr_list[["cumulative_incidence"]] <- unique(surv_group[, c("time", "cumulative_incidence")])
    
    # Censoring rate
    distr_list[["censoring_incidence"]] <- unique(surv_group[ , c("time", "cumulative_censoring")])
    
  } else {
    ..error_no_known_outcome_type(object@outcome_type)
  }
  
  object@distribution <- distr_list
  
  return(object)
}



#####show (outcomeInfo)#####
setMethod("show", signature(object="outcomeInfo"),
          function(object){
            # Basic outcome information.
            outcome_name <- object@name
            if(length(outcome_name) == 0) outcome_name <- "outcome"
            
            # Form basic string.
            outcome_str <- paste0(outcome_name, " (", object@outcome_type, ")")
            
            # Adapt string per outcome-type
            if(object@outcome_type %in% c("binomial")){
              
              # Show the reference class.
              outcome_classes <- object@levels
              outcome_classes[1] <- paste0(outcome_classes[1], " (reference)")
              
              # Add to the outcome string.
              outcome_str <- paste0(outcome_str, ", with classes: ", paste_s(outcome_classes), ".\n")
              
            } else if(object@outcome_type %in% c("multinomial")){
              # Show the outcome classes.
              outcome_str <- paste0(outcome_str, ", with classes: ", paste_s(object@levels), ".\n")
              
            } else if(object@outcome_type %in% c("count", "continuous")){
              # No further details provided.
              outcome_str <- paste0(outcome_str, ".\n")
              
            } else if(object@outcome_type %in% c("survival")){
              # Show the censoring and event values.
              if(length(object@censored) > 0){
                censoring_str <- paste0("censoring: ", paste_s(object@censored))
                event_str <- paste0("event: ", paste_s(object@event))
                
                # Add to outcome string.
                outcome_str <- paste0(outcome_str, ", with ", censoring_str, "; and ", event_str, ".\n")
                
              } else {
                event_str <- paste0("event: ", paste_s(object@event))
                
                # Add to outcome string.
                outcome_str <- paste0(outcome_str, ", without censoring; and ", event_str, ".\n")
              }
              
            } else if(object@outcome_type %in% c("competing_risk")){
              if(length(object@censored) > 0){
                censoring_str <- paste0("with censoring: ", paste_s(object@censored))
               
              } else {
                censoring_str <- "without censoring"
              }
              
              if(length(object@competing_risk) > 0){
                competing_risk_str <- paste0(ifelse(length(object@competing_risk) > 1, "competing risks: ", "competing risk: "),
                                             paste_s(object@competing_risk))
              } else {
                competing_risk_str <- "no competing risks"
              }
              
              event_str <- paste0("event: ", paste_s(object@event))
              
              # Add to outcome string.
              outcome_str <- paste0(outcome_str, ", ", censoring_str, "; ", event_str, "; and ", competing_risk_str, ".\n")
              
            } else {
              ..error_no_known_outcome_type(object@outcome_type)
            }
            
            # Print to terminal.
            cat(paste0(outcome_str))
          })

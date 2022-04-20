.start_cluster <- function(n_cores=NULL, cluster_type=NULL){
  # Start a cluster of workers
  
  # Load from familiar global environment if unset.
  if(is.null(n_cores)) n_cores <- .get_desired_n_cores()
  if(is.null(cluster_type)) cluster_type <- .get_selected_cluster_type()
  
  if(cluster_type == "none") return(NULL)
  
  # Check the number of available connections
  n_available_connections <- .available_connections()
  
  # Find the number of available cores.
  n_available_cores <- parallel::detectCores()
  n_cores <- max(c(1, min(c(n_available_cores-1, n_cores))))
  
  if(n_available_connections < 2 & n_cores > 2 & cluster_type %in% c("psock", "sock", "fork")){
    stop(paste0("R has insufficient available connections to perform parallel processing. ",
                "You may close connections using closeAllConnections() to free up connections. ",
                "If this does not resolve the issue, disable parallelisation for familiar."))
    
  } else if(n_cores > 2 & n_available_connections < n_cores){
    
    warning(paste0("R has insufficient available connections to use all available cores for parallel processing. ",
                   n_available_connections, " are used instead."))
    
    # Update the number of cores.
    n_cores <- n_available_connections
  }
  
  # Return NULL if the number of cores is 1 or less.
  if(n_cores <= 1) return(NULL)
  
  # Start cluster.
  if(cluster_type == "psock"){
    cl <- parallel::makePSOCKcluster(n_cores)
    
  } else if(cluster_type == "fork"){
    cl <- parallel::makeForkCluster(n_cores)
    
  } else if(cluster_type %in% c("mpi", "nws", "sock")){
    cl <- parallel::makeCluster(type=toupper(cluster_type), 2)
  }
  
  return(cl)
}


.terminate_cluster <- function(cl){
  # Stops a cluster and releases workers
  
  # Do not stop a cluster if it is external.
  if(.is_external_cluster()) return(cl)

  if(inherits(cl, "cluster")) parallel::stopCluster(cl)

  return(NULL)
}


.available_connections <- function(){
  # TODO Add check for R version in case the number of available connections
  # changes.
  
  # The standard is that R allows 128 connections. 3 are in standard use by R.
  # Another socket is used by the default socket_server data backend.
  R_available <- 124
  
  # Find the number of currently available connections.
  n_available_connections <- R_available - nrow(showConnections())
  
  if(n_available_connections < 0) n_available_connections <- 0
  
  return(n_available_connections)
}


.check_cluster_type_availability <- function(cluster_type){
  # Check if the provided cluster type is available.
  
  if(get_os() == "windows" & cluster_type == "fork") {
    stop("FORK clusters are not available for windows OS.")
  }
  
  installed_libs <- utils::installed.packages()[,1]
  
  if(!"snow" %in% installed_libs & cluster_type %in% c("mpi", "nws", "sock")){
    stop("The parallel package requires the snow package to work with MPI, NWS or SOCK clusters.")
  }
  
  if(!"Rmpi" %in% installed_libs & cluster_type == "mpi"){
    stop("The parallel packages requires the Rmpi package to work with MPI clusters.")
  }
}


.assign_parallel_options_to_global <- function(is_external_cluster=FALSE,
                                               restart_cluster=FALSE,
                                               n_cores=NULL,
                                               cluster_type=NULL){
  
  # We don't need a backend if the cluster is external.
  if(is.null(cluster_type) & !is_external_cluster) cluster_type <- "psock"
  
  # Assign parallelisation options to the familiar global environment.
  assign("is_external_cluster", is_external_cluster, envir=familiar_global_env)
  assign("restart_cluster", restart_cluster, envir=familiar_global_env)
  assign("n_cores", n_cores, envir=familiar_global_env)
  assign("cluster_type", cluster_type, envir=familiar_global_env)
}



.needs_cluster_restart <- function(){
  
  if(exists("familiar_global_env")){
    if(exists("restart_cluster", where=familiar_global_env)){
      data_env <- familiar_global_env
      
    } else if (exists("restart_cluster", where=.GlobalEnv)){
      data_env <- .GlobalEnv
      
    } else {
      return(FALSE)
    }
    
  } else if (exists("restart_cluster", where=.GlobalEnv)){
    data_env <- .GlobalEnv
    
  } else {
    return(FALSE)
  }
  
  return(get("restart_cluster", envir=data_env))
}



.is_external_cluster <- function(){
  
  if(exists("familiar_global_env")){
    if(exists("is_external_cluster", where=familiar_global_env)){
      data_env <- familiar_global_env
      
    } else if (exists("is_external_cluster", where=.GlobalEnv)){
      data_env <- .GlobalEnv
      
    } else {
      return(TRUE)
    }
    
  } else if (exists("is_external_cluster", where=.GlobalEnv)){
    data_env <- .GlobalEnv
    
  } else {
    return(TRUE)
  }
  
  return(get("is_external_cluster", envir=data_env))
}



.get_desired_n_cores <- function(){
  return(get("n_cores", envir=familiar_global_env))
}



.get_selected_cluster_type <- function(){
  return(get("cluster_type", envir=familiar_global_env))
}



.restart_cluster <- function(cl=NULL, assign=NULL, n_nodes=NULL){
  
  # Determine the number of nodes that we need to assign.
  if(inherits(cl, "cluster") & is.null(n_nodes)) n_nodes <- length(cl)
  
  # Terminate old cluster (if necessary)
  cl <- .terminate_cluster(cl=cl)
  
  # Start a new cluster
  cl <- .start_cluster(n_cores=n_nodes)
  
  # If the cluster doesn't start, return a NULL
  if(is.null(cl)) return(NULL)
  
  # Set library paths to avoid issues with non-standard library locations.
  libs <- .libPaths()
  parallel::clusterExport(cl=cl, varlist="libs", envir=environment())
  parallel::clusterEvalQ(cl=cl, .libPaths(libs))
  
  # Load familiar and data.table libraries to each cluster node.
  parallel::clusterEvalQ(cl=cl, library(familiar))
  parallel::clusterEvalQ(cl=cl, library(data.table))
  
  # Set options on each cluster node.
  parallel::clusterEvalQ(cl=cl, options(rf.cores=as.integer(1)))
  parallel::clusterEvalQ(cl=cl, data.table::setDTthreads(1L))
  
  # Check if anything needs to be loaded
  if(any(c("all", "data") %in% assign)){
    
    # Only add master data to the global environment of the clusters when
    # running a non-rserve data backend.
    if(.get_selected_backend_type() == "none"){
      parallel::clusterExport(cl=cl, varlist="master_data", envir=familiar_global_env)
    }
  }
  
  # Export the feature_info list
  if(any(c("all", "feature_info") %in% assign)){
    if(.get_selected_backend_type() == "none"){
      if(exists("master_feature_info_list", envir=familiar_global_env)){
        parallel::clusterExport(cl=cl, varlist="master_feature_info_list", envir=familiar_global_env)
      }
    }
  }
  
  # Export the project_info list
  if(any(c("all", "project_info") %in% assign)){
    parallel::clusterExport(cl=cl, varlist="project_info_list", envir=familiar_global_env)
  }
    
  # Export smaller objects directly, so that we don't have to worry about them.
  parallel::clusterExport(cl=cl, varlist="settings", envir=familiar_global_env)
  parallel::clusterExport(cl=cl, varlist="file_paths", envir=familiar_global_env)
  parallel::clusterExport(cl=cl, varlist="outcome_info", envir=familiar_global_env)
  parallel::clusterExport(cl=cl, varlist="server_port", envir=familiar_global_env)
  parallel::clusterExport(cl=cl, varlist="backend_type", envir=familiar_global_env)
  
  return(cl)
}


.update_info_on_cluster <- function(cl, assign, backend_type=NULL){
  # Not all variables in the familiar global environment are static. For
  # example, the feature_info_list may be updated by pre-processing. This
  # function updates the clusters with the latest version of such variables.
  
  if(is.null(backend_type)) backend_type <- .get_selected_backend_type()
  
  # Re-export the feature_info list.
  if(any(c("all", "feature_info") %in% assign)){
    if(backend_type == "none" & exists("master_feature_info_list", envir=familiar_global_env)){
      parallel::clusterExport(cl=cl, varlist="master_feature_info_list", envir=familiar_global_env)
    }
  }
  
  return(cl)
}



fam_sapply <- function(cl=NULL,
                       assign=NULL,
                       X,
                       FUN,
                       ...,
                       progress_bar=FALSE,
                       MoreArgs=NULL,
                       SIMPLIFY=TRUE,
                       USE.NAMES=TRUE,
                       chopchop=FALSE,
                       overhead_time=NULL,
                       process_time=NULL,
                       MEASURE.TIME=FALSE){
  
  # Determine the output format.
  output_format <- ifelse(SIMPLIFY, "vector", "list")
  
  # Get the name of the initial argument.
  initial_argument_name <- head(names(formals(FUN)), n=1)
  
  # Upgrade X to a named list.
  X <- list(X)
  names(X) <- initial_argument_name
  
  # Make call to .fam_apply.
  y <- do.call(.fam_apply,
               args=c(list("cl"=cl,
                           "assign"=assign,
                           "FUN"=FUN,
                           "additional_arguments"=list(...),
                           "progress_bar"=progress_bar,
                           "chopchop"=chopchop,
                           "overhead_time"=overhead_time,
                           "process_time"=process_time,
                           "measure_time"=MEASURE.TIME,
                           "process_scheduling"="static",
                           "output_format"=output_format,
                           "use_names"=USE.NAMES),
                      X))
  
  return(y)
}



fam_lapply <- function(cl=NULL,
                       assign=NULL,
                       X,
                       FUN,
                       ...,
                       progress_bar=FALSE,
                       MoreArgs=NULL,
                       SIMPLIFY=FALSE,
                       chopchop=FALSE,
                       overhead_time=NULL,
                       process_time=NULL,
                       MEASURE.TIME=FALSE){
  
  # Determine the output format.
  output_format <- ifelse(SIMPLIFY, "vector", "list")
  
  # Get the name of the initial argument.
  initial_argument_name <- head(names(formals(FUN)), n=1)
  
  # Upgrade X to a named list.
  X <- list(X)
  names(X) <- initial_argument_name
  
  # Make call to .fam_apply.
  y <- do.call(.fam_apply,
               args=c(list("cl"=cl,
                           "assign"=assign,
                           "FUN"=FUN,
                           "additional_arguments"=list(...),
                           "progress_bar"=progress_bar,
                           "chopchop"=chopchop,
                           "overhead_time"=overhead_time,
                           "process_time"=process_time,
                           "measure_time"=MEASURE.TIME,
                           "process_scheduling"="static",
                           "output_format"=output_format),
                      X))
  
  return(y)
}




fam_mapply <- function(cl=NULL,
                       assign=NULL,
                       FUN,
                       ...,
                       progress_bar=FALSE,
                       MoreArgs=NULL,
                       SIMPLIFY=FALSE,
                       USE.NAMES=TRUE,
                       chopchop=FALSE,
                       overhead_time=NULL,
                       process_time=NULL,
                       MEASURE.TIME=FALSE){
  
  # Determine the output format.
  output_format <- ifelse(SIMPLIFY, "vector", "list")
  
  # Make call to .fam_apply.
  y <- do.call(.fam_apply,
               args=c(list("cl"=cl,
                           "assign"=assign,
                           "FUN"=FUN,
                           "additional_arguments"=MoreArgs,
                           "progress_bar"=progress_bar,
                           "chopchop"=chopchop,
                           "overhead_time"=overhead_time,
                           "process_time"=process_time,
                           "measure_time"=MEASURE.TIME,
                           "process_scheduling"="static",
                           "output_format"=output_format,
                           "use_names"=USE.NAMES),
                      list(...)))
  
  return(y)
}



fam_sapply_lb <- function(cl=NULL,
                          assign=NULL,
                          X,
                          FUN,
                          ...,
                          progress_bar=FALSE,
                          SIMPLIFY=TRUE,
                          USE.NAMES=TRUE,
                          MEASURE.TIME=FALSE){
  
  # Determine the output format.
  output_format <- ifelse(SIMPLIFY, "vector", "list")
  
  # Get the name of the initial argument.
  initial_argument_name <- head(names(formals(FUN)), n=1)
  
  # Upgrade X to a named list.
  X <- list(X)
  names(X) <- initial_argument_name
  
  # Make call to .fam_apply.
  y <- do.call(.fam_apply,
               args=c(list("cl"=cl,
                           "assign"=assign,
                           "FUN"=FUN,
                           "additional_arguments"=list(...),
                           "progress_bar"=progress_bar,
                           "process_scheduling"="dynamic",
                           "output_format"=output_format,
                           "use_names"=USE.NAMES,
                           "measure_time"=MEASURE.TIME),
                      X))
  
  return(y)
}



fam_lapply_lb <- function(cl=NULL,
                          assign=NULL,
                          X,
                          FUN,
                          ...,
                          progress_bar=FALSE,
                          SIMPLIFY=FALSE,
                          MEASURE.TIME=FALSE){
  
  # Determine the output format.
  output_format <- ifelse(SIMPLIFY, "vector", "list")
  
  # Get the name of the initial argument.
  initial_argument_name <- head(names(formals(FUN)), n=1)
  
  # Upgrade X to a named list.
  X <- list(X)
  names(X) <- initial_argument_name
  
  # Make call to .fam_apply.
  y <- do.call(.fam_apply,
               args=c(list("cl"=cl,
                           "assign"=assign,
                           "FUN"=FUN,
                           "additional_arguments"=list(...),
                           "progress_bar"=progress_bar,
                           "process_scheduling"="dynamic",
                           "output_format"=output_format,
                           "measure_time"=MEASURE.TIME),
                      X))
  return(y)
}



fam_mapply_lb <- function(cl=NULL,
                          assign=NULL,
                          FUN,
                          ...,
                          progress_bar=FALSE,
                          MoreArgs=NULL,
                          SIMPLIFY=FALSE,
                          USE.NAMES=TRUE,
                          MEASURE.TIME=FALSE){
  
  # Determine the output format.
  output_format <- ifelse(SIMPLIFY, "vector", "list")
  
  # Make call to .fam_apply.
  y <- do.call(.fam_apply,
               args=c(list("cl"=cl,
                           "assign"=assign,
                           "FUN"=FUN,
                           "additional_arguments"=MoreArgs,
                           "progress_bar"=progress_bar,
                           "process_scheduling"="dynamic",
                           "output_format"=output_format,
                           "use_names"=USE.NAMES,
                           "measure_time"=MEASURE.TIME),
                      list(...)))
  
  return(y)
}



.fam_apply <- function(cl=NULL,
                       assign=NULL,
                       FUN,
                       ...,
                       additional_arguments=NULL,
                       progress_bar=FALSE,
                       chopchop=FALSE,
                       overhead_time=NULL,
                       process_time=NULL,
                       measure_time=FALSE,
                       process_scheduling="static",
                       output_format="list",
                       use_names=TRUE){
  
  .check_parameter_value_is_valid(process_scheduling,
                                  var_name="process_scheduling",
                                  values=c("static", "dynamic"))
  
  .check_parameter_value_is_valid(output_format,
                                  var_name="output_format",
                                  values=c("list", "vector"))
  
  # Check the length of the dots argument.
  n_x <- .dots_arg_length(...)
  if(n_x == 0) return(list())
  
  # Adapt cl to the iterable data in ...
  if(inherits(cl, "cluster")){
    if(n_x == 1) cl <- NULL
  }
  
  # Check that there is more than one node.
  if(inherits(cl, "cluster")){
    if(length(cl) == 1) cl <- NULL
  }
  
  if((chopchop | measure_time) & inherits(cl, "cluster")){
    if(!require_package(x="microbenchmark",
                        purpose="to make use of optimised parallel processing",
                        as_error=FALSE)){
      
      # If package is not installed, do not do anything fancy.
      chopchop <- measure_time <- FALSE
    }
  }
  
  # Check whether optimisation of the apply distribution is required and useful.
  require_optimisation <- chopchop & inherits(cl, "cluster") & (is.null(overhead_time) | is.null(process_time))
  require_process_stacking <- chopchop & inherits(cl, "cluster") & length(process_time) == n_x & !is.null(overhead_time)

  # Set initial values.
  y_initial <- NULL
  skip_element <- NULL
  stacking_data <- NULL
  
  # Adapt cl based on optimised overhead times.
  if(chopchop & inherits(cl, "cluster")){
    if(require_optimisation){
      
      # Send to a chunking function for chopping into a list with up to n_cl
      # elements. The first element may be skipped, because we already analysed
      # it.
      dots <- .chop_args(args=list(...),
                         n_cl=1L,
                         n_x=1L)
      
      # Measure process start
      overhead_start <- microbenchmark::get_nanotime()
      
      # Setup initial cluster.
      if(.needs_cluster_restart() & !.is_external_cluster()){
        cl_test <- .restart_cluster(cl=cl[1],
                                    assign=assign,
                                    n_nodes=1L)
        
      } else {
        cl_test <- .update_info_on_cluster(cl=cl[1],
                                           assign=assign)
      }
      
      # # Only for testing
      # y_test <- do.call(mapply,
      #                   args=list("FUN"=.chopped_apply,
      #                             "dots"=dots,
      #                             "SIMPLIFY"=FALSE,
      #                             "MoreArgs"=list("wrapper_fun"=.wrapper_fun_time,
      #                                             "FUN2"=FUN,
      #                                             "measure_time"=TRUE,
      #                                             "additional_arguments"=additional_arguments)))
      
      # Iterate over dots and perform analysis.
      y <- do.call(parallel::clusterMap,
                   args=list("fun"=.chopped_apply,
                             "cl"=cl_test,
                             "dots"=dots,
                             "SIMPLIFY"=FALSE,
                             "MoreArgs"=list("wrapper_fun"=.wrapper_fun_time,
                                             "FUN2"=FUN,
                                             "measure_time"=TRUE,
                                             "additional_arguments"=additional_arguments)))

      # Measure process end
      overhead_end <- microbenchmark::get_nanotime()
      
      # Collect data and concatenate to a flat list.
      y <- unlist(y, recursive=FALSE)
      y <- .flatten_nested_fam_apply(y)
      
      # Compute overhead and process times. Overhead time is computed from the
      # time passed minus the time of the longest subprocess.
      overhead_time <- (overhead_end - overhead_start) / 1E9 - y$process_time_total
      
      # Obtain initial
      y_initial <- y
      
      # Skip analysis of the first iterable element.
      skip_element <- 1L
      n_x <- n_x - 1
      
      # Update the first node that was used to run the test.
      cl[1] <- cl_test
      
    } else if(require_process_stacking){
      # Determine how to optimally stack the processes.
      stacking_data <- .auto_stacker(n_nodes=length(cl),
                                     overhead_time=overhead_time,
                                     process_time=process_time)
    }
    
    # Set the optimal number of nodes.
    if(require_process_stacking){
      if(is.null(stacking_data)){
        n_nodes_optimal <- 0L
        
      } else {
        n_nodes_optimal <- max(stacking_data$node_id)
      }
      
    } else {
      # Compute the optimal number of nodes. This is based on computing the
      # derivative of the process time curve, and determining the optima. Here t
      # = f(n, m) = m * overhead_time + n / m * process_time, with n the number
      # of processes and m the number of available nodes.
      n_nodes_optimal <- floor(sqrt(n_x * stats::median(c(process_time, y$process_time)) / overhead_time))
    }
    
    # Select the required nodes.
    if(n_nodes_optimal <= 1){
      cl <- NULL

    } else if(n_nodes_optimal < length(cl)){
      cl <- cl[1:n_nodes_optimal]
    }
  }
  
  # Adapt cl based on the number of iterable elements.
  if(inherits(cl, "cluster")) {
    if(n_x < length(cl)) cl <- cl[1:n_x]
    
    if(measure_time) startup_overhead_start <- microbenchmark::get_nanotime()
    
    # Update and restart clusters.
    if(.needs_cluster_restart() & !.is_external_cluster()){
      if(require_optimisation){
        cl[-1] <- .restart_cluster(cl=cl[-1],
                                   assign=assign)
        
      } else {
        cl <- .restart_cluster(cl=cl,
                               assign=assign)
      }
      
      # Close cluster on exit.
      on.exit(.terminate_cluster(cl))
      
    } else {
      if(require_optimisation){
        cl[-1] <- .update_info_on_cluster(cl=cl[-1],
                                          assign=assign)
        
      } else {
        cl <- .update_info_on_cluster(cl=cl,
                                      assign=assign)
        
      }
    }
    
    if(measure_time){
      startup_overhead_end <- microbenchmark::get_nanotime()
      startup_overhead_time <- (startup_overhead_end - startup_overhead_start) / 1E9
      
    } else {
      startup_overhead_time <- 0.0
    }
  }
  
  # Set up the wrapper.
  wrapper_fun <- .wrapper_fun
  if(measure_time) wrapper_fun <- .wrapper_fun_time
  if(progress_bar & !measure_time & !inherits(cl, "cluster")) wrapper_fun <- .wrapper_fun_progress
  if(progress_bar & measure_time & !inherits(cl, "cluster")) wrapper_fun <- .wrapper_fun_time_progress
  
  # Start progress bar.
  if(progress_bar & !inherits(cl, "cluster")){
    pb_conn <- utils::txtProgressBar(min=0, max=n_x, style=3)
    
  } else {
    pb_conn <- NULL
  }
  
  if(!inherits(cl, "cluster")) {
    
    # Check if sequential processing was selected despite cluster nodes being
    # present initially. For simplicity, we recompute the first element, as the
    # lack of cluster indicates that the time spent by the process is
    # negligible.
    if(require_optimisation) n_x <- n_x + 1
    
    # Perform the sequential apply as mapply.
    y <- do.call(mapply,
                 args=c(list("FUN"=wrapper_fun),
                        list(...),
                        list("II"=seq_len(n_x),
                             "SIMPLIFY"=FALSE,
                             "MoreArgs"=list("FUN2"=FUN,
                                             "pb_conn"=pb_conn,
                                             "MoreArgs"=additional_arguments))))
    
    # Collect data and concatenate to a flat list.
    y <- .flatten_nested_fam_apply(y)
    
  } else if(inherits(cl, "cluster") & !chopchop){
    
    if(measure_time) overhead_start <- microbenchmark::get_nanotime()
    
    # Perform the sequential apply as clusterMap.
    y <- do.call(parallel::clusterMap,
                 args=c(list("cl"=cl,
                             "fun"=wrapper_fun),
                        list(...),
                        list("SIMPLIFY"=FALSE,
                             ".scheduling"=process_scheduling,
                             "MoreArgs"=list("FUN2"=FUN,
                                             "MoreArgs"=additional_arguments))))
    
    if(measure_time) overhead_end <- microbenchmark::get_nanotime()
    
    # Collect data and concatenate to a flat list.
    y <- .flatten_nested_fam_apply(y)
    
    if(measure_time){
      # Compute overhead and process times. Overhead time is computed from the
      # time passed minus the time of the longest subprocess.
      overhead_time <- (startup_overhead_time + (overhead_end - overhead_start) / 1E9 - sum(y$process_time) / length(cl)) / length(cl)
    }
    
  } else if(inherits(cl, "cluster") & chopchop){
    # Send to a chunking function for chopping into a list with up to n_cl
    # elements. The first element may be skipped, because we already analysed
    # it.
    dots <- .chop_args(args=list(...),
                       n_cl=length(cl),
                       stacking_data=stacking_data,
                       skip_elements=skip_element)
    
    if(measure_time) overhead_start <- microbenchmark::get_nanotime()
    
    # # Test purposes
    # y_test <- do.call(mapply,
    #                   args=list("FUN"=.chopped_apply,
    #                             "dots"=dots,
    #                             "SIMPLIFY"=FALSE,
    #                             "MoreArgs"=list("wrapper_fun"=wrapper_fun,
    #                                             "FUN2"=FUN,
    #                                             "measure_time"=measure_time,
    #                                             "additional_arguments"=additional_arguments)))
    
    # Iterate over dots and perform analysis.
    y <- do.call(parallel::clusterMap,
                 args=list("fun"=.chopped_apply,
                           "cl"=cl,
                           "dots"=dots,
                           "MoreArgs"=list("wrapper_fun"=wrapper_fun,
                                           "FUN2"=FUN,
                                           "measure_time"=measure_time,
                                           "additional_arguments"=additional_arguments)))
    
    if(measure_time) overhead_end <- microbenchmark::get_nanotime()
    
    # Collect data and concatenate to a flat list.
    y <- unlist(y, recursive=FALSE)
    y <- .flatten_nested_fam_apply(y)
    
    # Reorder results to original order. This is because the stacking algorithm
    # will assign based on expected time passed.
    if(!is.null(stacking_data)){
      
      # Reorder results to input order.
      y$results[stacking_data$original_index] <- y$results
      
      # Reorder process time to input order.
      if(!is.null(y$process_time)) y$process_time[stacking_data$original_index] <- y$process_time
    }
    
    if(measure_time){
      # Compute overhead and process times. Overhead time is computed from the
      # time passed minus the time of the longest subprocess.
      overhead_time <- (startup_overhead_time + (overhead_end - overhead_start) / 1E9 - max(y$process_time_total)) / length(cl)
    }
    
    # Add in initial list.
    y <- .flatten_nested_list(c(list(y_initial), list(y)), flatten=TRUE)
    
  } else {
    ..error_reached_unreachable_code(".fam_apply: no correct combination found.")
  }
  
  # Simplify to output format.
  if(output_format == "vector") y$results <- simplify2array(y$results)
  
  # Set names.
  if(use_names) {
    dots <- list(...)
    
    # Try to get names.
    element_names <- names(dots[[1L]])
    if(is.null(element_names) & is.character(dots[[1L]])){
      names(y$results) <- dots[[1L]]
      
    } else if(!is.null(element_names)){
      names(y$results) <- element_names
    }
  }
  
  # Parse the return data.
  if(measure_time){
    
    y <- list("results"=y$results,
              "overhead_time"=overhead_time,
              "process_time"=y$process_time)
    
  } else {
    y <- y$results
  }
  
  if(!is.null(pb_conn)) close(pb_conn)
  
  return(y)
}



.wrapper_fun <- function(FUN2, II=NULL, pb_conn=NULL, ..., MoreArgs=NULL){
  # Execute function
  y <- do.call(FUN2, args=c(list(...),
                            MoreArgs))
  
  return(list("results"=y))
}



.wrapper_fun_time <- function(FUN2, II=NULL, pb_conn=NULL, ..., MoreArgs=NULL){
  # Get process start time.
  process_start <- microbenchmark::get_nanotime()
  
  # Execute function
  y <- do.call(FUN2, args=c(list(...),
                            MoreArgs))
  
  # Get process end time.
  process_end <- microbenchmark::get_nanotime()
  
  return(list("results"=y,
              "process_time"=(process_end - process_start) / 1E9))
}



.wrapper_fun_progress <- function(FUN2, II=NULL, pb_conn=NULL, ..., MoreArgs=NULL){
  # Execute function
  y <- do.call(FUN2, args=c(list(...),
                            MoreArgs))
  
  # Update the progress bar
  utils::setTxtProgressBar(pb=pb_conn, value=II)
  
  return(list("results"=y))
}



.wrapper_fun_time_progress <- function(FUN2, II=NULL, pb_conn=NULL, ..., MoreArgs=NULL){
  # Get process start time.
  process_start <- microbenchmark::get_nanotime()
  
  # Execute function
  y <- do.call(FUN2, args=c(list(...),
                            MoreArgs))
  
  # Get process end time.
  process_end <- microbenchmark::get_nanotime()
  
  # Update the progress bar
  utils::setTxtProgressBar(pb=pb_conn, value=II)
  
  return(list("results"=y,
              "process_time"=(process_end - process_start) / 1E9))
}


.dots_arg_length <- function(...){
  # Get the length of the dots argument.
  n_x <-  unique(sapply(list(...), length))
  
  if(length(n_x)!=1) ..error_reached_unreachable_code(".dots_arg_length: iterable arguments do not have the same length.")
  
  return(n_x)
}



.chopped_apply <- function(wrapper_fun, FUN2, dots, additional_arguments=NULL, measure_time=FALSE){
  
  if(measure_time) process_total_start <- microbenchmark::get_nanotime()
  
  # Perform the sequential apply as mapply.
  y <- do.call(mapply,
               args=c(list("FUN"=wrapper_fun),
                      dots,
                      list("SIMPLIFY"=FALSE,
                           "USE.NAMES"=FALSE,
                           "MoreArgs"=list("FUN2"=FUN2,
                                           "MoreArgs"=additional_arguments))))
  
  if(measure_time) process_total_end <- microbenchmark::get_nanotime()
  
  # Set total process time (which may vary between nodes.)
  if(measure_time) y[[1]]$process_time_total <- (process_total_end - process_total_start) / 1E9
  
  return(y)
}



.flatten_nested_fam_apply <- function(x){
  
  x <- .flatten_nested_list(x)
  
  if(!is.null(x$process_time)) x$process_time <- unlist(x$process_time)
  if(!is.null(x$process_time_total)) x$process_time_total <- unlist(x$process_time_total)
  
  return(x)
}



.chop_args <- function(args, n_cl, n_x=NULL, skip_elements=NULL, stacking_data=NULL){
  
  # Determine n_x if not provided. n_x may be provided when we attempt to
  # determine process and overhead time.
  if(is.null(n_x)) n_x <- unique(sapply(args, length))
  
  if(length(n_x)!=1) ..error_reached_unreachable_code(".chop_args: arguments do not have the same length.")
  
  # Set up the sequence of values. 
  n_x <- seq_len(n_x)
  
  # Skip if there elements that should be skipped. This is useful if, for
  # example, the optimisation process needs to determine overhead time.
  if(!is.null(skip_elements)) n_x <- setdiff(n_x, skip_elements)
    
  # Make sure not to generate empty entries. Therefore the number of nodes can
  # only be up to the number of available elements.
  if(n_cl > length(n_x)) n_cl <- length(n_x)
  
  if(length(n_x) == 0 | n_cl == 0){
    index <- list()
    
  } else if(length(n_x) == 1 | n_cl == 1){
    index <- list(n_x)
    
  } else if(!is.null(stacking_data)){
    index <- unname(lapply(split(stacking_data, by="node_id", sorted=TRUE), function(list_element) (list_element$original_index)))
    
  } else {
    index <- structure(split(n_x,
                             cut(n_x, n_cl)),
                       names=NULL)
  }
  
  # Split arguments internally.
  chopped_args <- lapply(index, function(ii, x){
    return(lapply(x, function(x, ii){
      if(data.table::is.data.table(x)){
        column_names <- colnames(x)[ii]
        return(x[, mget(column_names)])
        
      } else {
        return(x[ii])
      }
    }, ii=ii))
  },
  x=args)
  
  return(chopped_args)
}



.auto_stacker <- function(n_nodes, overhead_time, process_time, min_decrease=0.05){
  
  # Check if there are fewer processes than nodes.
  if(n_nodes > length(process_time)) n_nodes <- length(process_time)
  
  # Check if there is any reason to parallellise.
  if(n_nodes < 2) return(NULL)
  
  # Determine the baseline time.
  baseline_time <- sum(process_time)
  
  # Compute the minimal decrease.
  min_decrease <- min_decrease * baseline_time
  
  # Initialise parameters for the while loop.
  previous_time <- baseline_time
  previous_stacking_data <- NULL
  
  # Initial nodes
  current_nodes <- 2L
  
  while(current_nodes <= n_nodes){
    
    # Check if the overhead time does not exceed the previous time.
    if(current_nodes * overhead_time >= previous_time) break()
    
    # Get current stacking data
    current_stacking_data <- ..auto_stack(n_nodes=current_nodes,
                                          process_time=process_time,
                                          overhead_time=overhead_time)
    
    # Check that an increase in nodes does not negatively affect the time spent
    # processing. Also, ensure that the achieved decrease is at least
    # min_decrease with regard to the previous time.
    if(previous_time <= current_stacking_data$node_time | previous_time - current_stacking_data$node_time < min_decrease) break()
    
    # Replace stacking data for the next iteration.
    previous_stacking_data <- current_stacking_data
    
    # Update other variables.
    previous_time <- current_stacking_data$node_time
    current_nodes <- current_nodes + 1L
  }
  
  # Return process data belonging to the process prior to 
  return(previous_stacking_data$process_data)
}



..auto_stack <- function(n_nodes, process_time, overhead_time){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  node_id <- NULL
  
  # Initiate a placeholder for the time allocated to each node.
  node_time <- numeric(n_nodes)
  
  # Sort process times. Assign the longest processes first.
  process_data <- data.table::data.table("process_time"=process_time)
  process_data[, ":="("original_index"=.I, "node_id"=0L)]
  process_data <- process_data[order(process_time, decreasing=TRUE)]
  
  for(ii in seq_along(process_time)){
    # Select the node that has the least process time assigned to it.
    selected_node <- which.min(node_time)
    
    # Assign node to process
    process_data[ii, "node_id":=selected_node]
    
    # Update time on the node
    node_time[selected_node] <- node_time[selected_node] + process_data[ii, ]$process_time
  }
  
  # Set element order.
  process_data <- process_data[order(node_id)]
  process_data[, "process_index":=.I]
  
  # Return the maximum node time (which is of course)
  return(list("node_time"=max(node_time) + overhead_time * n_nodes,
              "process_data"=process_data))
}

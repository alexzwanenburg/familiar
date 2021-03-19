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



.restart_cluster <- function(cl, assign=NULL){
  
  # Terminate old cluster (if necessary)
  cl <- .terminate_cluster(cl=cl)
  
  # Start a new cluster
  cl <- .start_cluster()
  
  # If the cluster doesn't start, return a NULL
  if(is.null(cl)) return(NULL)
  
  # Set library paths to avoid issues with non-standard library locations.
  libs <- .libPaths()
  parallel::clusterExport(cl=cl, varlist="libs", envir=environment())
  parallel::clusterEvalQ(cl=cl, .libPaths(libs))
  
  # Load familiar and data.table libraries to each cluster node.
  parallel::clusterEvalQ(cl=cl, library(familiar))
  
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
      parallel::clusterExport(cl=cl, varlist="master_feature_info_list", envir=familiar_global_env)
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
}



fam_lapply <- function(cl=NULL, assign=NULL, X, FUN, progress_bar=FALSE, ..., .scheduling="static"){
  # lapply. Reverts to sequential lapply if cl is NULL.
  
  # Restart cluster if specified. This also means that we should terminate the
  # cluster after use. If clusters already exist, update the cluster with the
  # latest versions of variables in the global familiar environment.
  if(!is.null(cl) & .needs_cluster_restart() & !.is_external_cluster()){
    cl <- .restart_cluster(cl=cl, assign=assign)
    terminate_cluster_on_exit <- TRUE
    
  } else if(inherits(cl, "cluster")) {
    .update_info_on_cluster(cl=cl, assign=assign)
    terminate_cluster_on_exit <- FALSE
    
  } else {
    terminate_cluster_on_exit <- FALSE
  }
  
  if(is.null(cl) & !progress_bar){
    # Simple sequential lapply.
    y <- do.call(lapply, args=append(list("X" = X,
                                          "FUN" = FUN),
                                     list(...)))
    
  } else if(is.null(cl) & progress_bar){
    # Start progress bar
    pb_conn <- utils::txtProgressBar(min=0, max=length(X), style=3)
    
    # Determine the length of the argument.
    arg_length <- length(X)
    
    # Perform the sequential apply as mapply.
    y <- do.call(mapply, args=c(list("FUN"=.fun_with_progress_map),
                                list(X),
                                list("II"=seq_len(arg_length),
                                     "MoreArgs"=list("FUN2"=FUN,
                                                     "pb_conn"=pb_conn,
                                                     "MoreArgs"=list(...)),
                                     "SIMPLIFY"=FALSE,
                                     "USE.NAMES"=TRUE)))
    
    # Close the progress bar connection.
    close(pb_conn)
    
  } else if(inherits(cl, "cluster")){
    
    # Determine the type of scheduling.
    if(.scheduling == "static") FUN_par <- parallel::parLapply
    else FUN_par <- parallel::parLapplyLB
    
    # Parallel lapply without load balancing.
    y <- do.call(FUN_par, args=c(list("cl" = cl,
                                      "X" = X,
                                      "fun" = FUN),                                                                                                                                                  
                                 list(...)))
    
  } else {
    ..error_reached_unreachable_code("fam_lapply: the cluster object is neither NULL nor a cluster.")
  }
  
  if(terminate_cluster_on_exit){
    cl <- .terminate_cluster(cl=cl)
  }
  
  return(y)
}



fam_lapply_lb <- function(cl=NULL, assign=NULL, X, FUN, progress_bar=FALSE, ...){
  # Call fam_lapply, with dynamic scheduling.
  y <- do.call(fam_lapply, args=c(list("cl"=cl,
                                       "assign"=assign,
                                       "X"=X,
                                       "FUN"=FUN,
                                       "progress_bar"=progress_bar,
                                       ".scheduling"="dynamic"),
                                  list(...)))
  
  return(y)
}



fam_sapply <- function(cl=NULL, assign=NULL, X, FUN, progress_bar=FALSE, ..., SIMPLIFY=TRUE, USE.NAMES=TRUE, .scheduling="static"){
  # sapply. Reverts to sequential sapply if cl is NULL.
  
  # Restart cluster if specified. This also means that we should terminate the
  # cluster after use. If clusters already exist, update the cluster with the
  # latest versions of variables in the global familiar environment.
  if(!is.null(cl) & .needs_cluster_restart() & !.is_external_cluster()){
    cl <- .restart_cluster(cl=cl, assign=assign)
    terminate_cluster_on_exit <- TRUE
    
  } else if(inherits(cl, "cluster")) {
    .update_info_on_cluster(cl=cl, assign=assign)
    terminate_cluster_on_exit <- FALSE
    
  } else {
    terminate_cluster_on_exit <- FALSE
  }
  
  if(is.null(cl) & !progress_bar){
    # Simple sequential sapply.
    y <- do.call(sapply, args=append(list("X" = X,
                                          "FUN" = FUN,
                                          "simplify" = SIMPLIFY,
                                          "USE.NAMES" = USE.NAMES),
                                     list(...)))
    
  } else if(is.null(cl) & progress_bar){
    # Start progress bar
    pb_conn <- utils::txtProgressBar(min=0, max=length(X), style=3)
    
    # Determine the length of the argument.
    arg_length <- length(X)
    
    # Perform the sequential apply as mapply.
    y <- do.call(mapply, args=c(list("FUN"=.fun_with_progress_map),
                                list(X),
                                list("II"=seq_len(arg_length),
                                     "MoreArgs"=list("FUN2"=FUN,
                                                     "pb_conn"=pb_conn,
                                                     "MoreArgs"=list(...)),
                                     "SIMPLIFY"=SIMPLIFY,
                                     "USE.NAMES"=USE.NAMES)))
    
    # Close the progress bar connection.
    close(pb_conn)
    
  } else if(inherits(cl, "cluster")){
    
    # Determine the type of scheduling.
    if(.scheduling == "static") FUN_par <- parallel::parSapply
    else FUN_par <- parallel::parSapplyLB
    
    # Parallel sapply without load balancing.
    y <- do.call(FUN_par, args=append(list("cl" = cl,
                                           "X" = X,
                                           "FUN" = FUN,
                                           "simplify" = SIMPLIFY,
                                           "USE.NAMES" = USE.NAMES),
                                      list(...)))
    
  } else {
    ..error_reached_unreachable_code("fam_sapply: the cluster object is neither NULL nor a cluster.")
  }
  
  if(terminate_cluster_on_exit){
    cl <- .terminate_cluster(cl=cl)
  }
  
  return(y)
}


fam_sapply_lb <- function(cl=NULL, assign=NULL, X, FUN, progress_bar=FALSE, ..., SIMPLIFY=TRUE, USE.NAMES=TRUE){
  # Call fam_sapply, with dynamic scheduling.
  y <- do.call(fam_sapply, args=c(list("cl"=cl,
                                       "assign"=assign,
                                       "X"=X,
                                       "FUN"=FUN,
                                       "progress_bar"=progress_bar,
                                       "SIMPLIFY"=SIMPLIFY,
                                       "USE.NAMES"=USE.NAMES,
                                       ".scheduling"="dynamic"),
                                  list(...)))
  
  return(y)
}



fam_mapply <- function(cl=NULL, assign=NULL, FUN, ...,  progress_bar=FALSE, MoreArgs=NULL, SIMPLIFY=FALSE, USE.NAMES=TRUE, .scheduling="static", .chopchop=FALSE){
  # mapply. Reverts to sequential mapply if cl is NULL.
  
  # Restart cluster if specified. This also means that we should terminate the
  # cluster after use. If clusters already exist, update the cluster with the
  # latest versions of variables in the global familiar environment.
  if(!is.null(cl) & .needs_cluster_restart() & !.is_external_cluster()){
    cl <- .restart_cluster(cl=cl, assign=assign)
    terminate_cluster_on_exit <- TRUE
    
  } else if(inherits(cl, "cluster")) {
    .update_info_on_cluster(cl=cl, assign=assign)
    terminate_cluster_on_exit <- FALSE
    
  } else {
    terminate_cluster_on_exit <- FALSE
  }
  
  if(is.null(cl) & !progress_bar){
    # Simple sequential lapply.
    y <- do.call(mapply, args=c(list("FUN" = FUN),
                                list(...),
                                list("MoreArgs"=MoreArgs,
                                     "SIMPLIFY"=SIMPLIFY,
                                     "USE.NAMES"=USE.NAMES)))
    
  } else if(is.null(cl) & progress_bar){
    
    # Define length of dots
    dots <- list(...)
    arg_length <- max(sapply(dots, length))
    
    # Start progress bar
    pb_conn <- utils::txtProgressBar(min=0, max=arg_length, style=3)
    
    # Perform the sequential apply as mapply.
    y <- do.call(mapply, args=c(list("FUN"=.fun_with_progress_map),
                                list(...),
                                list("II"=seq_len(arg_length),
                                     "MoreArgs"=list("FUN2"=FUN,
                                                     "pb_conn"=pb_conn,
                                                     "MoreArgs"=MoreArgs),
                                     "SIMPLIFY"=SIMPLIFY,
                                     "USE.NAMES"=USE.NAMES)))
    
    # Close the progress bar connection.
    close(pb_conn)
    
  } else if(inherits(cl, "cluster")){
    
    if(.chopchop){
      # Send to a chunking function for chopping into a list with up to n_cl
      # elements.
      dots <- .chop_args(args=list(...),
                         n_cl=length(cl))
      
      # Limit the number of clusters.
      if(length(dots) < length(cl)) cl[1:length(dots)]
      
      # Iterate over cluster nodes for assignment using .chopped_mapply.
      y <- do.call(parallel::clusterMap, args=list("fun"=.chopped_mapply,
                                                   "cl"=cl,
                                                   "dots"=dots,
                                                   "MoreArgs"=list("FUN2"=FUN,
                                                                   "MoreArgs"=MoreArgs,
                                                                   "SIMPLIFY2"=SIMPLIFY,
                                                                   "USE.NAMES2"=USE.NAMES),
                                                   "SIMPLIFY"=SIMPLIFY,
                                                   "USE.NAMES"=USE.NAMES))
      
      # Flatten the list.
      y <- unlist(y, recursive=FALSE)
      
    } else {
      # Parallel mapply using clustermap
      y <- do.call(parallel::clusterMap, args=c(list("cl"=cl,
                                                     "fun"=FUN),
                                                list(...),
                                                list("MoreArgs"=MoreArgs,
                                                     "SIMPLIFY"=SIMPLIFY,
                                                     "USE.NAMES"=USE.NAMES,
                                                     ".scheduling"=.scheduling)))
    }
    
  } else {
    ..error_reached_unreachable_code("fam_lapply: the cluster object is neither NULL nor a cluster.")
  }
  
  if(terminate_cluster_on_exit){
    cl <- .terminate_cluster(cl=cl)
  }
  
  return(y)
}



fam_mapply_lb <- function(cl=NULL, assign=NULL, FUN, ...,  progress_bar=FALSE, MoreArgs=NULL, SIMPLIFY=FALSE, USE.NAMES=TRUE){
  # Call fam_sapply, with dynamic scheduling.
  y <- do.call(fam_mapply, args=c(list("cl"=cl,
                                       "assign"=assign,
                                       "FUN"=FUN,
                                       "progress_bar"=progress_bar,
                                       "MoreArgs"=MoreArgs,
                                       "SIMPLIFY"=SIMPLIFY,
                                       "USE.NAMES"=USE.NAMES,
                                       ".scheduling"="dynamic"),
                                  list(...)))
  
  return(y)
}



.fun_with_progress_map <- function(FUN2, II, pb_conn, ..., MoreArgs=NULL){
  
  # Execute function
  y <- do.call(FUN2, args=c(list(...),
                            MoreArgs))
  
  # Update the progress bar
  utils::setTxtProgressBar(pb=pb_conn, value=II)
  
  return(y)
}



.chopped_mapply <- function(FUN2, dots, MoreArgs=NULL, SIMPLIFY2, USE.NAMES2){
  
  # Execute function on cluster
  y <- do.call(mapply, args=c(list("FUN"=FUN2,
                                   "SIMPLIFY"=SIMPLIFY2,
                                   "USE.NAMES"=USE.NAMES2,
                                   "MoreArgs"=MoreArgs),
                              dots))
  return(y)
}



.chop_args <- function(args, n_cl){
  
  n_x <- unique(sapply(args, length))
  if(length(n_x)!=1) ..error_reached_unreachable_code(".chop_args: arguments do not have the same length.")
  
  if(n_cl == 0){
    index <- list()
    
  } else if(n_x == 1 | n_cl == 1){
    index <- list(seq_len(n_x))
    
  } else {
    index <- structure(split(seq_len(n_x),
                             cut(seq_len(n_x), n_cl)),
                       names=NULL)
  }
  
  # Split arguments internally.
  chopped_args <- lapply(index, function(ii, x){
    return(lapply(x, function(x, ii) (x[ii]), ii=ii))
  },
  x=args)

  return(chopped_args)
}

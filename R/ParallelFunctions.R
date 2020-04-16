.start_cluster <- function(n_cores=NULL, parallel_backend=NULL){
  # Start a cluster of workers
  
  # Load from familiar global environment if unset.
  if(is.null(n_cores)) n_cores <- .get_desired_n_cores()
  if(is.null(parallel_backend)) parallel_backend <- .get_selected_parallel_backend()
  
  # Check the number of available connections
  n_available_connections <- .available_connections()
  
  # Set the number of available cores
  if(is.null(n_cores)){
    
    # Find the number of available cores.
    n_cores <- parallel::detectCores()
    if(n_cores>1) n_cores <- n_cores-1
    
    if(n_available_connections < 2 & n_cores > 2){
      stop(paste0("R has insufficient available connections to perform parallel processing. ",
                  "You may close connections using closeAllConnections() to free up connections. ",
                  "If this does not resolve the issue, disable parallelisation for familiar."))
      
    } else if(n_cores > 2 & n_available_connections < n_cores){
      
      warning(paste0("R has insufficient available connections to use all available cores for parallel processing. ",
                     n_available_connections, " are used instead."))
      
      # Update the number of cores.
      n_cores <- n_available_connections
    }
    
  } else {
    
    # Find the number of available cores.
    n_available_cores <- parallel::detectCores()
    n_cores <- max(c(1, min(c(n_available_cores-1, n_cores))))
    
    if(n_available_connections < 2 & n_cores > 2){
      stop(paste0("R has insufficient available connections to perform parallel processing. ",
                  "You may close connections using closeAllConnections() to free up connections. ",
                  "If this does not resolve the issue, disable parallelisation for familiar."))

    } else if(n_cores > 2 & n_available_connections < n_cores){
      
      warning(paste0("R has insufficient available connections to use all available cores for parallel processing. ",
                     n_available_connections, " are used instead."))
      
      # Update the number of cores.
      n_cores <- n_available_connections
    }
  }
  
  # Return NULL if the number of cores is 1 or less.
  if(n_cores <= 1) return(NULL)
  
  # Start CPU cluster
  if(parallel_backend == "fork"){
    cl <- parallel::makeForkCluster(n_cores)
  } else {
    cl <- parallel::makeCluster(n_cores)
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
  
  R_available <- 125
  
  # Find the number of currently available connections.
  n_available_connections <- R_available - nrow(showConnections())
  
  if(n_available_connections < 0) n_available_connections <- 0
  
  return(n_available_connections)
}


.assign_parallel_options_to_global <- function(is_external_cluster=FALSE,
                                               restart_cluster=FALSE,
                                               n_cores=NULL,
                                               parallel_backend=NULL){
  
  # We don't need a backend if the cluster is external.
  if(is.null(parallel_backend) & !is_external_cluster) parallel_backend <- .get_default_backend()
  
  # Assign parallelisation options to the familiar global environment.
  assign("is_external_cluster", is_external_cluster, envir=familiar_global_env)
  assign("restart_cluster", restart_cluster, envir=familiar_global_env)
  assign("n_cores", n_cores, envir=familiar_global_env)
  assign("parallel_backend", parallel_backend, envir=familiar_global_env)
}


.needs_cluster_restart <- function(){
  return(get("restart_cluster", envir=familiar_global_env))
}

.is_external_cluster <- function(){
  return(get("is_external_cluster", envir=familiar_global_env))
}

.get_desired_n_cores <- function(){
  return(get("n_cores", envir=familiar_global_env))
}

.get_selected_parallel_backend <- function(){
  return(get("parallel_backend", envir=familiar_global_env))
}

.start_cluster <- function(nr_cores=NULL, backend){
  # Start a cluster of workers
  
  # Check the number of available connections
  n_available_connections <- .available_connections()
  
  # Set the number of available cores
  if(is.null(nr_cores)){
    nr_cores <- parallel::detectCores()
    if(nr_cores>1) nr_cores <- nr_cores-1
    
    if(n_available_connections < 2 & nr_cores > 2){
      stop(paste0("R has insufficient available connections to perform parallel processing. ",
                  "You may close connections using closeAllConnections() to free up connections. ",
                  "If this does not resolve the issue, disable parallelisation for familiar."))
    } else if(nr_cores > 2 & n_available_connections < nr_cores){
      
      warning(paste0("R has insufficient available connections to use all available cores for parallel processing. ",
                     n_available_connections, " are used instead."))
      
      # Update the number of cores.
      nr_cores <- n_available_connections
    }
    
  } else {
    nrAvailableCores <- parallel::detectCores()
    nr_cores <- max(c(1, min(c(nrAvailableCores-1, nr_cores))))
    
    if(n_available_connections < 2 & nr_cores > 2){
      stop(paste0("R has insufficient available connections to perform parallel processing. ",
                  "You may close connections using closeAllConnections() to free up connections. ",
                  "If this does not resolve the issue, disable parallelisation for familiar."))

    } else if(nr_cores > 2 & n_available_connections < nr_cores){
      
      warning(paste0("R has insufficient available connections to use all available cores for parallel processing. ",
                     n_available_connections, " are used instead."))
      
      # Update the number of cores.
      nr_cores <- n_available_connections
    }
  }
  
  if(nr_cores <= 1) return(NULL)
  
  # Start CPU cluster
  if(backend=="fork"){
    cl <- parallel::makeForkCluster(nr_cores)
  } else {
    cl <- parallel::makeCluster(nr_cores)
  }

  return(cl)
}


.terminate_cluster <- function(cl){
  # Stops a cluster and releases workers

  parallel::stopCluster(cl)

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

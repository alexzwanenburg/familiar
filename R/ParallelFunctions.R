.start_cluster <- function(nr_cores=NULL, backend){
  # Start a cluster of workers

  # Set the number of available cores
  if(is.null(nr_cores)){
    nr_cores <- parallel::detectCores()
    if(nr_cores>1) { nr_cores <- nr_cores-1}
  } else {
    nrAvailableCores <- parallel::detectCores()
    nr_cores <- max(c(1, min(c(nrAvailableCores-1, nr_cores))))
  }

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


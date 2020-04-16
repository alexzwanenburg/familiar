.check_backend_availability <- function(backend_option){
  # Checks backend configuration

  # Get all installed packages
  installed_libs <- utils::installed.packages()[,1]

  if(backend_option=="rserve_coop"){
    # Rservecoop is not CRAN, and may require installation
    if(!"Rservecoop" %in% installed_libs){
      # Stop and have the package installed by the user
      stop(paste("rserve_coop is not a valid option for backend. The Rservecoop package is missing. Please install this package manually:",
                 "\tdevtools::install_github(\"alexzwanenburg/Rserve_coop\", ref=\"1.7\")",
                 "If you are behind a proxy, please configure the connection first:",
                 "\trequire(httr)",
                 "\tset_config(use_proxy(url=\"your_proxy\", port=proxy_port, username=\"proxy_user_name\", password=\"proxy_password\"))",
                 sep="\n"))
    }
  } else if(backend_option=="rserve"){
    # Check for RServe and windows OS
    if(getOS() != "windows"){
      stop(paste("rserve is not a valid option for backend. The Rserve package",
                 "cannot be used in cooperative mode for non-windows systems."))
    }

    if(!"Rserve" %in% installed_libs){
      stop(paste("rserve is not a valid option for backend. The Rserve package is missing. Please install this package from CRAN:",
                 "\tinstall.packages(\"RServe\")", sep="\n"))
    }
  } else if(backend_option=="fork"){
    # Check for windows OS
    if(getOS() == "windows"){
      stop("fork is not a valid option for backend. Forking is not allowed on Windows OS.")
    }

  } else if(backend_option=="non_fork"){
    # Nothing really
  } else {
    ..error_value_not_allowed(x=backend_option, var_name="backend",
                              values=c("non_fork", "fork", "rserve", "rserve_coop"))
  }

}


.get_default_backend <- function(){
  # Get all installed packages
  installed_libs <- utils::installed.packages()[,1]

  if("Rservecoop" %in% installed_libs) {
    return("rserve_coop")
    
  } else if(getOS() != "windows" & !interactive()) {
    # The documentation of parallel::makeCluster recommends against forking when
    # running from a GUI such as RStudio. We use the interactive() function to
    # figure out if the R session is interactive or not.
    return("fork")
    
  } else if("Rserve" %in% installed_libs) {
    return("rserve")
    
  } else {
    return("non_fork")
  }
}


.assign_data_to_backend <- function(cl, backend_data, backend, server_port){

  if(backend %in% c("fork", "non_fork")){
    # Put dt_backend_data in global environment
    assign(x="dt_backend_data", value=backend_data, envir=familiar_global_env)
    
  } else if(backend %in% c("rserve", "rserve_coop")){
    # Start server and open connection
    startRServe(port=server_port, backend=backend)

    # Open connection
    rcon <- RSclient::RS.connect(port=server_port)

    # Load data.table pacakge on server
    RSclient::RS.eval(rsc=rcon, library(familiar))

    # Send data.table to server
    RSclient::RS.assign(rsc=rcon, name="dt_backend_data", value=backend_data, wait=TRUE)

    # Close server connection
    RSclient::RS.close(rcon)
    
  } else {
    ..error_reached_unreachable_code("assign_data_to_backed_unknown_backend")
  }
  
  # Only add backend data when running a non-rserve backend
  if(!is.null(cl) & backend %in% c("fork", "non_fork")){
    parallel::clusterExport(cl=cl, varlist="dt_backend_data", envir=familiar_global_env)
  }
}


getDataFromBackend <- function(subj_id=NULL, col_names=NULL, settings){

  # Suppress NOTES due to non-standard evaluation in data.table
  subject_id <- NULL

  if(settings$run$backend %in% c("rserve", "rserve_coop")){
    # Open connection. If no connection is available, the process will sleep until a connection can be made
    connectionAvailable <- FALSE
    while(connectionAvailable==FALSE){
      tryCatch({
        rcon <- RSclient::RS.connect(port=settings$run$server_port) # Try to connect to loop
        break()                                 # Break from loop if connection is succesful
      }, error=function(err){
        Sys.sleep(0.01)
      })
    }

    # Get slice of data.table from server
    if( is.null(subj_id) &  is.null(col_names)){
      dt <- RSclient::RS.eval(rsc=rcon, substitute(dt_backend_data[!subject_id %in% r_vars, ], list(r_vars=character(0))), lazy=FALSE) }
    if( is.null(subj_id) & !is.null(col_names)){
      dt <- RSclient::RS.eval(rsc=rcon, substitute(dt_backend_data[, c_vars], list(c_vars=col_names)), lazy=FALSE) }
    if(!is.null(subj_id) &  is.null(col_names)){
      dt <- RSclient::RS.eval(rsc=rcon, substitute(dt_backend_data[subject_id %in% r_vars, ], list(r_vars=subj_id)), lazy=FALSE) }
    if(!is.null(subj_id) & !is.null(col_names)){
      dt <- RSclient::RS.eval(rsc=rcon, substitute(dt_backend_data[subject_id %in% r_vars, c_vars], list(r_vars=subj_id, c_vars=col_names)), lazy=FALSE) }

    # Close connection
    RSclient::RS.close(rsc=rcon)

    return(dt)
  }

  if(settings$run$backend %in% c("fork", "non_fork")){
    # Data is stored in the familiar_global_env environment by default, but may be assigned to the global environment on clusters
    if(exists("familiar_global_env")){
      if(exists("dt_backend_data", where=familiar_global_env)){
        data_env <- familiar_global_env
      } else if (exists("dt_backend_data", where=.GlobalEnv)){
        data_env <- .GlobalEnv
      } else {
        stop("Data not found in backend.")
      }
    } else if (exists("dt_backend_data", where=.GlobalEnv)){
      data_env <- .GlobalEnv
    } else {
      stop("Data not found in backend.")
    }


    if( is.null(subj_id) &  is.null(col_names)){
      dt <- data.table::copy(get("dt_backend_data", envir=data_env)) }
    if( is.null(subj_id) & !is.null(col_names)){
      dt <- data.table::copy(get("dt_backend_data", envir=data_env)[, col_names, with=FALSE]) }
    if(!is.null(subj_id) &  is.null(col_names)){
      dt <- data.table::copy(get("dt_backend_data", envir=data_env)[subject_id %in% subj_id, ]) }
    if(!is.null(subj_id) & !is.null(col_names)){
      dt <- data.table::copy(get("dt_backend_data", envir=data_env)[subject_id %in% subj_id, col_names, with=FALSE]) }

    return(dt)
  }
}



getOS <- function(){
  # Gets the operating system name
  system_info <- Sys.info()

  os_name <- system_info["sysname"]
  if(os_name=="Darwin"){
    os_name <- "mac_os"
  }
  os_name <- tolower(os_name)
  return(os_name)

}



startRServe <- function(port, backend){
  # Starts an Rserve server daemon process. This server will hold the main data table.
  # The function checks whether a server is already running at the specified port and starts a new server if it isn't

  # Check if RServe is already running
  tryCatch({
    rcon <- RSclient::RS.connect(port=port)
    RSclient::RS.close(rcon)
    message("Data server: Rserve already running.")
  }, error=function(err) {
    message("Data server: Starting Rserve in cooperative mode.")
    if(backend=="rserve_coop"){
      Rservecoop::Rserve(debug=FALSE, port=port, args=paste0("--no-save ","--RS-enable-control"))
    } else if(backend=="rserve") {
      Rserve::Rserve(debug=FALSE, port=port, args=paste0("--no-save ","--RS-enable-control"))
    }
  })
}


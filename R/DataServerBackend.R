.required_packages_backend <- function(backend_type) {
  
  backend_packages <- NULL
  if (backend_type == "socket_server") {
    backend_packages <- "callr"
  }
  
  return(backend_packages)
}


.get_available_backend_types <- function() {
  
  # All types of backend.
  all_backend_types <- c("none", "socket_server")
  
  return(all_backend_types)
}



.get_selected_backend_type <- function() {
  # Get the type of backend server from the familiar global environment or the
  # global environment. The latter happens for parallel processes.
  if (exists("familiar_global_env")) {
    if (exists("backend_type", where = familiar_global_env)) {
      data_env <- familiar_global_env
      
    } else if (exists("backend_type", where = .GlobalEnv)) {
      data_env <- .GlobalEnv
    }
    
  } else if (exists("backend_type", where = .GlobalEnv)) {
    data_env <- .GlobalEnv
    
  } else {
    ..error_reached_unreachable_code(
      ".get_selected_backend_type: backend_type was not found in familiar_global_env or .GlobalEnv"
    )
  }
  
  backend_type <- tryCatch(
    get("backend_type", envir = data_env),
    error = identity
  )
  
  if (inherits(backend_type, "error")) {
    .assign_backend_options_to_global(
      backend_type = "none",
      server_port = NULL
    )
    
    backend_type <- "none"
  }
  
  return(backend_type)
}



.get_backend_server_port <- function() {
  # Get the port for the backend server from the familiar global environment or
  # the global environment. The latter happens for parallel processes.
  if (exists("familiar_global_env")) {
    if (exists("server_port", where = familiar_global_env)) {
      data_env <- familiar_global_env
 
    } else if (exists("server_port", where = .GlobalEnv)) {
      data_env <- .GlobalEnv
    }
    
  } else if (exists("server_port", where = .GlobalEnv)) {
    data_env <- .GlobalEnv
    
  } else {
    ..error_reached_unreachable_code(
      ".get_backend_server_port: server_port was not found in familiar_global_env or .GlobalEnv"
    )
  }

  return(get("server_port", envir = data_env))
}



.assign_backend_options_to_global <- function(backend_type, server_port) {
  assign(
    "backend_type",
    value = backend_type,
    envir = familiar_global_env
  )
  assign(
    "server_port",
    value = server_port,
    envir = familiar_global_env
  )
  
  return(invisible(TRUE))
}



.assign_data_to_backend <- function(
    data, 
    backend_type = NULL, 
    server_port = NULL
) {

  # Find the server port and backend_type variables if not directly provided.
  if (is.null(server_port)) server_port <- .get_backend_server_port()
  if (is.null(backend_type)) backend_type <- .get_selected_backend_type()
  
  # Check if required packages are installed.
  require_package(
    x = .required_packages_backend(backend_type = backend_type),
    purpose = paste0("to use the requested backend (", backend_type, ")")
  )
  
  if (backend_type %in% c("none")) {
    # Put master_data in global environment
    assign(
      x = "master_data",
      value = data,
      envir = familiar_global_env
    )
    
  } else if (backend_type %in% c("socket_server")) {
    # Start the separate r session thread that will run the socket server.
    start_socket_server_process(server_port = server_port)
    
    # Obtain the handle.
    socket_server_process <- .get_socket_server_process_handle()
    
    # Assign the data to socket server process so that its available later.
    socket_server_process$run(
      function(data) {
        assign(
          x = "master_data",
          value = data,
          envir = familiar_global_env
        )
      },
      args = list("data" = data),
      package = TRUE
    )
    
    # Finally, activate the server subroutine.
    .activate_socket_server_routine(server_port = server_port)
    
  } else {
    ..error_reached_unreachable_code(".assign_data_to_backend: unknown backend encountered")
  }
  
  return(invisible(TRUE))
}



get_data_from_backend <- function(
    backend_type = NULL,
    server_port = NULL,
    sample_identifiers = NULL,
    column_names = NULL
) {

  # Find the server port and backend_type variables if not directly provided.
  if (is.null(server_port)) server_port <- .get_backend_server_port()
  if (is.null(backend_type)) backend_type <- .get_selected_backend_type()
  
  # Check if required packages are installed.
  require_package(
    x = .required_packages_backend(backend_type = backend_type),
    purpose = paste0("to use the requested backend (", backend_type, ")")
  )
  
  if (backend_type == "none") {
    # Absence of backend.
    x <- .get_data_from_backend(
      sample_identifiers = sample_identifiers,
      column_names = column_names
    )
    
  } else if (backend_type == "socket_server") {
    # Socket server backend.
    
    # Open connection to socket server.
    con <- socket_client_open_connection(port = server_port)
    on.exit(socket_client_close_connection(con))
    
    # Execute .get_data_back_end on the server side, and obtain the data view.
    x <- socket_client_do_call(
      con,
      .get_data_from_backend,
      args = list(
        "sample_identifiers" = sample_identifiers,
        "column_names" = column_names
      )
    )
    
  } else {
    ..error_reached_unreachable_code(paste0(
      "get_data_from_backend: encountered unknown type of backend: ", backend_type
    ))
  }
  
  return(x)
}



.get_data_from_backend <- function(sample_identifiers = NULL, column_names = NULL) {
  # This is the support function that operates on the processes 
  
  # Suppress NOTES due to non-standard evaluation in data.table
  .NATURAL <- NULL
  
  # Identify the environment where the master_data object lives.
  if (exists("familiar_global_env")) {
    if (exists("master_data", where = familiar_global_env)) {
      data_env <- familiar_global_env
      
    } else if (exists("master_data", where = .GlobalEnv)) {
      data_env <- .GlobalEnv
    }
    
  } else if (exists("master_data", where = .GlobalEnv)) {
    data_env <- .GlobalEnv
    
  } else {
    ..error_reached_unreachable_code(
      ".get_data_from_backend: master_data was not found in familiar_global_env or .GlobalEnv"
    )
  }
  
  # Obtain and export views of the master data set to the calling function.
  if (is.null(sample_identifiers) && is.null(column_names)) {
    # Get entire dataset.
    x <- data.table::copy(get("master_data", envir = data_env))
    
  } else if (is.null(sample_identifiers) && !is.null(column_names)) {
    # Get entire columns from the dataset.
    x <- data.table::copy(get("master_data", envir = data_env)[, mget(column_names)])
    
  } else if (!is.null(sample_identifiers) && is.null(column_names)) {
    # Get entire rows from the dataset.
    x <- data.table::copy(get("master_data", envir = data_env)[
      sample_identifiers, on = .NATURAL
    ])
    
  } else {
    # Get certain columns and rows from the dataset.
    x <- data.table::copy(get("master_data", envir = data_env)[
      sample_identifiers, mget(column_names), on = .NATURAL
    ])
  }
  
  return(x)
}



.assign_feature_info_to_backend <- function(
    feature_info_list,
    backend_type = NULL,
    server_port = NULL
) {

  # Find the server port and backend_type variables if not directly provided.
  if (is.null(server_port)) server_port <- .get_backend_server_port()
  if (is.null(backend_type)) backend_type <- .get_selected_backend_type()
  
  # Check if required packages are installed.
  require_package(
    x = .required_packages_backend(backend_type = backend_type),
    purpose = paste0("to use the requested backend (", backend_type, ")")
  )
  
  # Put master_feature_info_list in the familiar global environment.
  assign(
    x = "master_feature_info_list",
    value = feature_info_list,
    envir = familiar_global_env
  )
  
  if (backend_type %in% c("socket_server")) {
    # Stop the socket_server subroutine so that we can add or update the list of
    # feature information.
    .shutdown_active_socket_server_routine(server_port = server_port)
    
    # Obtain the handle.
    socket_server_process <- .get_socket_server_process_handle()
    
    # Assign the list of feature information to the socket server process so
    # that its accessible later.
    socket_server_process$run(
      function(feature_info_list) {
        # Assign locally.
        assign(
          x = "master_feature_info_list",
          value = feature_info_list,
          envir = familiar_global_env
        )
      },
      args = list("feature_info_list" = feature_info_list),
      package = TRUE
    )
    
    # Reactivate the server subroutine.
    .activate_socket_server_routine(server_port = server_port)
    
  } else if (backend_type != "none") {
    ..error_reached_unreachable_code(
      ".assign_feature_info_to_backend: unknown backend encountered"
    )
  }
}


get_feature_info_from_backend <- function(
    backend_type = NULL,
    server_port = NULL,
    data_id = NULL,
    run_id = NULL
) {
  
  # Find the server port and backend_type variables if not directly provided.
  if (is.null(server_port)) server_port <- .get_backend_server_port()
  if (is.null(backend_type)) backend_type <- .get_selected_backend_type()
  
  # Check if required packages are installed.
  require_package(
    x = .required_packages_backend(backend_type = backend_type),
    purpose = paste0("to use the requested backend (", backend_type, ")")
  )
  
  if (backend_type == "none") {
    # Absence of backend.
    x <- .get_feature_info_from_backend(data_id = data_id, run_id = run_id)
    
  } else if (backend_type == "socket_server") {
    # Socket server backend.
    
    # Open connection to socket server.
    con <- socket_client_open_connection(port = server_port)
    on.exit(socket_client_close_connection(con))
    
    # Execute .get_feature_info_from_backend on the server side, and obtain the
    # requested list of feature information.
    x <- socket_client_do_call(
      con,
      FUN = .get_feature_info_from_backend,
      args = list(
        "data_id" = data_id,
        "run_id" = run_id
      )
    )
    
  } else {
    ..error_reached_unreachable_code(paste0(
      "get_feature_info_from_backend: encountered unknown type of backend: ", backend_type
    ))
  }
  
  return(x)
}



.get_feature_info_from_backend <- function(data_id = NULL, run_id = NULL) {
  # This function is executed locally in the thread where
  # master_feature_info_list resides.
  
  # Identify the environment where the master_feature_info_list object lives.
  if (exists("familiar_global_env")) {
    if (exists("master_feature_info_list", where = familiar_global_env)) {
      data_env <- familiar_global_env
      
    } else if (exists("master_feature_info_list", where = .GlobalEnv)) {
      data_env <- .GlobalEnv
    }
    
  } else if (exists("master_feature_info_list", where = .GlobalEnv)) {
    data_env <- .GlobalEnv
    
  } else {
    ..error_reached_unreachable_code(paste0(
      ".get_feature_info_from_backend: master_feature_info_list was not found ", 
      "in familiar_global_env or .GlobalEnv."
    ))
  }
  
  # Now read the requested element.
  if (is.null(data_id) && is.null(run_id)) {
    # Retrieve the generic feature information.
    x <- get("master_feature_info_list", envir = data_env)[["generic"]]
    
  } else if (is.waive(data_id) && is.waive(run_id)) {
    # Retrieve all feature information, i.e. the complete list.
    x <- get("master_feature_info_list", envir = data_env)
    
  } else if (!is.null(data_id) && !is.null(run_id)) {
    # Retrieve run-specific feature information.
    x <- get("master_feature_info_list", envir = data_env)[[
      .get_feature_info_list_name(data_id = data_id, run_id = run_id)
    ]]
    
  } else {
    # data_id and run_id are either both provided, or both NULL.
    ..error_reached_unreachable_code(paste0(
      ".get_feature_info_from_backend: one of data_id and run_id is NULL ", 
      "instead of both NULL or both not NULL."
    ))
  }
  
  # The feature info list should not be empty.
  if (is.null(x)) {
    ..error_reached_unreachable_code(paste0(
      ".get_feature_info_from_backend: the requested feature information is empty. ",
      "data_id = ", data_id,
      "; run_id = ", run_id
    ))
  }
  
  return(x)
}



get_os <- function() {
  # Gets the operating system name
  system_info <- Sys.info()

  os_name <- system_info["sysname"]
  if (os_name == "Darwin") {
    os_name <- "mac_os"
  }
  
  return(tolower(os_name))
}



start_socket_server_process <- function(server_port = NULL) {
  
  # Find the server port if not directly provided.
  if (is.null(server_port)) server_port <- .get_backend_server_port()
  
  # Check if required packages are installed.
  require_package(
    x = .required_packages_backend(backend_type = "socket_server"),
    purpose = paste0("to use the requested backend (socket_server)")
  )
  
  # Check if the process is already running.
  if (!.is_socket_server_process_started()) {
    # Start the socket_server process in a separate persistent thread using
    # the callr package.
    socket_server_process <- callr::r_session$new()
    
    # Assign initial data to the 
    socket_server_process$run(
      function(server_port) {
        # Load the familiar package locally.
        library(familiar)
        
        # Assign the socket server port to the familiar global environment.
        assign(
          x = "server_port",
          value = server_port,
          envir = familiar_global_env
        )
      },
      args = list("server_port" = server_port),
      package = TRUE
    )
    
    # Export socket_server_process to the familiar global environment.
    assign(
      "socket_server_process",
      socket_server_process,
      envir = familiar_global_env
    )
  }
  
  return(invisible(TRUE))
}



.get_socket_server_process_handle <- function() {
  # Return the R6 callr::r_session object that serves as the handle for the
  # socket server process thread. This object is normally stored in the familiar
  # global environment.
  if (!exists("socket_server_process", where = familiar_global_env)) {
    ..error_reached_unreachable_code(paste0(
      ".get_socket_server_process_handle: socket_server_process ",
      "does not exist in familiar_global_env"
    ))
  }
  
  return(get("socket_server_process", envir = familiar_global_env))
}



.is_socket_server_process_started <- function() {
  # Check if the process exists
  if (!exists("socket_server_process", where = familiar_global_env)) return(FALSE)
  
  # Get handle
  socket_server_process <- .get_socket_server_process_handle()
  
  # Check if the process is alive
  is_alive <- socket_server_process$is_alive()
  
  return(is_alive)
}



.activate_socket_server_routine <- function(server_port = NULL) {
  
  # Get handle
  socket_server_process <- .get_socket_server_process_handle()
  
  # Check whether the socket server process is running in a separate thread.
  if (!socket_server_process$is_alive()) {
    ..error_reached_unreachable_code(paste0(
      ".activate_socket_server: the socket_server_process that hosts the ",
      "socket_server routine is not active."
    ))
  }
  
  # Check if the socket_server_process is idle.
  if (!socket_server_process$get_state() == "idle") {
    ..error_reached_unreachable_code(paste0(
      ".activate_socket_server: the socket_server_process that hosts the ",
      "socket_server routine is busy."
    ))
  }
  
  # Find the server port if not directly provided.
  if (is.null(server_port)) server_port <- .get_backend_server_port()
  
  # Start subroutine.
  socket_server_process$call(
    socket_server,
    args = list("port" = server_port),
    package = TRUE
  )
  
  return()
}



.shutdown_active_socket_server_routine <- function(server_port = NULL) {
  # Shuts down the socket_server routine running on the separate
  # socket_server_process callr::r_session thread.
  
  # Check if the server thread is active prior to shutting down the
  # socket_server subroutine.
  if (.is_socket_server_process_started()) {
    
    # Get handle
    socket_server_process <- .get_socket_server_process_handle()
    
    # Check the state of the process (idle or busy)
    process_state <- socket_server_process$get_state()
    
    # The process state will be "busy" when the socket_server subroutine is
    # active, because it loops. If its "idle" the subroutine is not longer
    # running.
    if (process_state == "busy") {
      
      # Find the server port if not directly provided.
      if (is.null(server_port)) server_port <- .get_backend_server_port()
      
      # Close the socket_server routine
      socket_client_server_shutdown(port = server_port)

      # The process should be responsive within a few milliseconds after sending
      # the command to stop the subroutine. We leave the process 5 seconds to
      # respond.      
      process_ready <- socket_server_process$poll_process(5000.0)
      
      # Read from the socket_server_process to free up the process again,
      # thereby finalising the shutdown of the socket_server subroutine and
      # returning the socket_server_process to its idle state.
      output <- socket_server_process$read()
    }
  }
  
  return(invisible(TRUE))
}



shutdown_backend_server <- function(backend_type = NULL, server_port = NULL) {
  
  # Find the server port and backend_type variables if not directly provided.
  if (is.null(server_port)) server_port <- .get_backend_server_port()
  if (is.null(backend_type)) backend_type <- .get_selected_backend_type()
  
  # Check if required packages are installed.
  require_package(
    x = .required_packages_backend(backend_type = backend_type),
    purpose = paste0("to use the requested backend (", backend_type, ")")
  )
  
  if (backend_type %in% c("socket_server")) {
    
    # Check if the server thread is active prior to shutting it down.
    if (.is_socket_server_process_started()) {
      
      # Attempt to shutdown the socket_server subroutine itself, if it is
      # running.
      .shutdown_active_socket_server_routine(server_port = server_port)
      
      # Get handle
      socket_server_process <- .get_socket_server_process_handle()
      
      # Stop the socket_server_process thread.
      socket_server_process$close()
    }
  }
  
  return()
}

socket_server <- function(host="local_host", port=6311, timeout=2678400){
  
  # The socket server basically runs until a client gives a shutdown command.
  server_status <- "active"
  
  # Activate server
  socket <- serverSocket(port = port)
  on.exit(close(socket))
  
  while(!server_status == "shutdown"){
    
    # Reset server status, unless shutdown
    server_status <- "active"
    
    # Check if connection to server can be accepted from a client.
    repeat{
      connection_available <- socketSelect(list(socket), FALSE, NULL)
      
      if(connection_available){
        con <- socketAccept(socket=socket, blocking=TRUE, open="a+b")
        break()
      }
    }
    
    # Try handshake by serializing to connecting client?
    if(socket_server_handshake(con)){
      
      # Run execution loop. server_status may be set to close_connection or
      # shutdown by the client. Both options close the connection at the server
      # side, with shutdown additionaly breaking the server loop.
      while(server_status == "active"){
        server_status <- socket_server_execution_loop(con=con)
      }
    }
    
    # Close existing connection.
    close(con)
  }
}



socket_server_handshake <- function(con){
  
  # Attempt to send instructions to obtain the process id of the client process.
  success <- tryCatch(serialize(list("FUN"=eval, "args"=list(quote(Sys.getpid()))),
                                connection=con),
                      error=identity)
  
  # Check if the attempt was successful, otherwise return FALSe, which closes
  # the connection.
  if(inherits(success, "error")) return(FALSE)
  
  # Attempt to read the process id of the client.
  pid <- tryCatch(unserialize(con),
                  error=identity)
  
  # Return TRUE or FALSE. TRUE indicates that the connection is accepted and
  # works. FALSE closes the connection.
  return(is.integer(pid))
}



socket_server_execution_loop <- function(con){
  
  # Attempt to read serialised data that has been pushed to the socket.
  input <- tryCatch(unserialize(connection=con),
                    warning=identity,
                    error=identity)
  
  # Break from loop in case a warning or error is given.
  if(inherits(input, "warning") | inherits(input, "error")) return("active")
  
  if(input$type == "EXEC"){
    # Execute a function
    
    # Execute the function
    output <- do.call(input$FUN, args=input$args)
    
    # Send back output of the function.
    output_send <- tryCatch(serialize(object=output, connection=con),
                            error=identity)
    
    return("active")
    
  } else if(input$type == "CLOSE"){
    # Signal that the socket connection should be closed.
    return("close_connection")
    
  } else if(input$type == "SHUTDOWN") {
    # Signal that the server itself should shutdown.
    return("shutdown")
  }
}


socket_client_open_connection <- function(host="localhost", port=6311){
  # Open a client-side connection.
  
  repeat{
    
    # Run a socketConnection with the shortest possible timeout, so that in case
    # of asynchronous activation (we attempt to start the client before a
    # server is listening), we will not be stuck for long.
    con <- tryCatch(socketConnection(host=host, port=port, blocking=TRUE,
                                     server=FALSE, open="a+b", timeout=1),
                    warning=identity,
                    error=identity)
    
    if(inherits(con, "sockconn")){
      # Try to perform the handshake with the server process. The connection is
      # closed if this is not successful and it will attempt to start a new
      # connection.
      if(socket_client_handshake(con)){
        break()
        
      } else {
        close(con)
      }
    }
  }
  
  return(con)
}



socket_client_handshake <- function(con){
  
  # The server will send a simple funnction through serialisation that request
  # the client pid.
  received <- tryCatch(unserialize(con), error=identity)
  
  # If the socket connection cannot be read, return a FALSE. This closes the
  # connection in the socket_client_open_connection.
  if(inherits(received, "error")) return(FALSE)
  
  # Execute the function received from the server process, and find the process
  # id.
  pid <- do.call(received$FUN, args=received$args)
  
  # Try to send the pid to the server.
  success <- tryCatch(serialize(pid, connection=con), error=identity)
  
  return(!inherits(success, "error"))
}



socket_client_do_call <- function(con, FUN, args=NULL){
  
  if(!inherits(con, "sockconn")){
    stop("Connection is not a socket connection.")
  }
  
  # Send call to server to execute the function.
  serialize(object=list("type"="EXEC",
                        "FUN"=FUN,
                        "args"=args),
            connection=con)
  
  # Wait for output to be received from server.
  repeat{
    output <- tryCatch(unserialize(connection=con),
                       error=identity)
    
    # Break from the loop if the output is not an error.
    if(!inherits(output, "error")) break()
  }
  
  return(output)
}


socket_client_close_connection <- function(con){
  if(!inherits(con, "sockconn")){
    stop("Connection is not a socket connection.")
  }
  
  # Skip if already closed.
  if(isOpen(con)){
    
    serialize(object=list("type"="CLOSE"), connection=con)
    
    # Close connection from the client side.
    close(con)
  }
}



socket_client_server_shutdown <- function(con=NULL, host="localhost", port=6311){
  if(!inherits(con, "sockconn")){
    
    # Open a connection
    con <- socket_client_open_connection(host=host, port=port)
    on.exit(close(con))
    
  } else if(!isOpen(con)){
    # Attempt to shut client-side connection and re-open it.
    close(con)
    
    # Open new connection
    con <- socket_client_open_connection(host=host, port=port)
    on.exit(close(con))
    
  } else {
    on.exit(close(con))
  }
  
  # Signal the server to close the connection and shutdown.
  serialize(list("type"="SHUTDOWN"), connection=con)
}

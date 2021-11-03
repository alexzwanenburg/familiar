## R CMD check results

0 errors | 0 warnings | 2 notes

This is the first release of the package.

* checking package dependencies ... NOTE
    Packages suggested but not available for checking: 'Rservecoop', 'qvalue'
  
    Imports includes 27 non-default packages.
    Importing from so many packages makes the package vulnerable to any of
    them becoming unavailable.  Move as many as possible to Suggests and
    use conditionally.

The 'Rservecoop' package can be installed from a GitHub repository, but its
absence does not limit package functionality.

The 'qvalue' package can be installed from BioConductor, and can be used to
generate specific output.

Non-default packages primarily are involved in plotting, job dispatch and
machine learning. The latter could be moved to Suggests, but this would require
prompting users to install additional packages.

* checking R code for possible problems ... NOTE
    Found the following assignments to the global environment:
      File 'familiar/R/DataServerBackend.R':
      assign(x = "master_data", value = data, envir = .GlobalEnv)
      assign(x = "master_feature_info_list", value = feature_info_list, 
        envir = .GlobalEnv)
      assign(x = "server_port", value = server_port, envir = .GlobalEnv)
      assign(x = "socket_server_execution_loop", value = FUN[[1]], 
        envir = .GlobalEnv)
      assign(x = "socket_server_handshake", value = FUN[[2]],
        envir = .GlobalEnv)
        
The assignments to the global environment are made in a separate R process
spawned using the 'callr' package. These are not visible to the user, and do not
affect the global environment of the user.

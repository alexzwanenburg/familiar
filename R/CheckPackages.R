#' @include FamiliarS4Generics.R
NULL


#####require_package (character)#####
setMethod("require_package", signature(x="character"),
          function(x, purpose=NULL, as_error=TRUE){
            
            # Attempt to load required packages.
            package_loaded <- sapply(x, requireNamespace, quietly=TRUE)
            
            # Skip further analysis if all packages could be loaded.
            if(all(package_loaded)) return(invisible(TRUE))
            
            # Find all packages that are missing.
            missing_packages <- x[!package_loaded]
            
            if(as_error){
              # Throw an error.
              ..error_package_not_installed(x=missing_packages,
                                            purpose=purpose)
            } else {
              # Raise a warning
              ..warning_package_not_installed(x=missing_packages,
                                              purpose=purpose)
            }
            
            return(invisible(FALSE))
          })


#####require_package (NULL)#####
setMethod("require_package", signature(x="NULL"),
          function(x, purpose=NULL, as_error=TRUE){
            
            return(invisible(TRUE))
          })



is_package_installed <- function(name){
  
  if(length(name) == 0) return(TRUE)
  
  # Try to obtain the package version. This perhaps the cleanest way to check
  # whether a package exists. require and requireNameSpace attach and load
  # packages, which is not required here. The find.package documentation
  # actively discourages its use to identify whether a package is installed.
  installed_version <- tryCatch(utils::packageVersion(name),
                                error=identity)
  
  return(!inherits(installed_version, "error"))
}



is_package_outdated <- function(name, version){
  
  if(length(name) == 0) return(TRUE)
  
  # Obtain the installed version of the package.
  installed_version <- tryCatch(utils::packageVersion(name),
                                error=identity)
  
  if(inherits(installed_version, "error")){
    ..error_package_not_installed(name)
  }
  
  # Make sure version is a package version.
  version <- as.package_version(version)
  
  return(version > installed_version)
} 



is_package_newer <- function(name, version){
  
  if(length(name) == 0) return(TRUE)
  
  # Obtain the installed version of the package.
  installed_version <- tryCatch(utils::packageVersion(name),
                                error=identity)
  
  if(inherits(installed_version, "error")){
    ..error_package_not_installed(name)
  }
  
  # Make sure version is a package version.
  version <- as.package_version(version)
  
  return(version < installed_version)
}



.bioconductor_packages <- function(){
  return("qvalue")
}



..message_package_not_installed_to_backend <- function(x, purpose, message_type){
  
  # Generate message string.
  message_str <- ..message_missing_package(x=x,
                                           purpose=purpose)
  
  missing_packages <- NULL
  if(exists("missing_packages", where=familiar_global_env)){
    # Get existing missing packages.
    missing_packages <- get("missing_packages",
                            envir=familiar_global_env)
  }
  
  # Append missing packages.
  missing_packages <- c(missing_packages, x)
  
  # Assign to backend.
  assign("missing_packages",
         value=missing_packages,
         envir=familiar_global_env)
  
  
  missing_package_messages <- NULL
  if(exists("missing_package_messages", where=familiar_global_env)){
    # Get existing package messages.
    missing_package_messages <- get("missing_package_messages",
                                    envir=familiar_global_env)
  }
  
  # Append package messages.
  missing_package_messages <- c(missing_package_messages, message_str)
  
  # Assign to backend.
  assign("missing_package_messages",
         value=missing_package_messages,
         envir=familiar_global_env)
  
  
  missing_package_message_type <- NULL
  if(exists("missing_package_message_type", where=familiar_global_env)){
    # Get existing missing packages.
    missing_package_message_type <- get("missing_package_message_type",
                                        envir=familiar_global_env)
  }
  
  # Append missing packages.
  missing_package_message_type <- c(missing_package_message_type, message_type)
  
  # Assign to backend.
  assign("missing_package_message_type",
         value=missing_package_message_type,
         envir=familiar_global_env)
}



..message_missing_package <- function(x, purpose=NULL){
  
  # Select unique packages.
  x <- unique(x)
  
  message_str <- paste0("The following package",
                        ifelse(length(x) > 1, "s have", " has"),
                        " to be installed",
                        ifelse(is.null(purpose), ": ", paste0(" ", purpose, ": ")),
                        paste_s(x), ".")
  
  return(message_str)
}



..message_install_from_cran <- function(x){
  
  # Select unique packages.
  x <- unique(x)
  
  x_cran <- setdiff(x, .bioconductor_packages())
  if(length(x_cran) > 0){
    message_str <- paste0("\n\nInstall from CRAN: ",
                          "\n\n\tinstall.packages(c(",
                          paste0("\"", x_cran, "\"", collapse=", "), "))")
  } else {
    message_str <- NULL
  }
}



..message_install_from_bioconductor <- function(x){
  
  # Select unique packages.
  x <- unique(x)
  
  x_bioc <- intersect(x, .bioconductor_packages())
  if(length(x_bioc) > 0){
    message_str <- paste0("\n\nInstall from Bioconductor: ",
                          ifelse(is_package_installed("BiocManager"), "", "\n\n\tinstall.packages(\"BiocManager\")"),
                          "\n\tBiocManager::install(c(",
                          paste0("\"", x_bioc, "\"", collapse=", "), "))")
  } else {
    message_str <- NULL
  }
  
  return(message_str)
}



..message_package_version <- function(x, version){
  return(paste0(x, " (v", as.character(version), ")"))
}

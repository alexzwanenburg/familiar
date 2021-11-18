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

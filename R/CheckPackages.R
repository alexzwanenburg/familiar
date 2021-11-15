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
  
  installed_version <- tryCatch(utils::packageVersion(name),
                                error=identity)
  
  if(inherits(installed_version, "error")){
    ..error_package_not_installed(name)
  }
  
  return(version > installed_version)
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

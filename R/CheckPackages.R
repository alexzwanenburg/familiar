#' @include FamiliarS4Generics.R
NULL


# require_package (character) --------------------------------------------------
setMethod(
  "require_package",
  signature(x = "character"),
  function(
    x,
    purpose = NULL,
    message_type = "error",
    ...) {
    
    # Pass to .require_package
    return(invisible(.require_package(
      x = x,
      purpose = purpose,
      message_type = message_type)))
  }
)


# require_package (NULL) -------------------------------------------------------
setMethod(
  "require_package",
  signature(x = "NULL"),
  function(x, purpose = NULL, ...) {
    
    return(invisible(TRUE))
  }
)


.require_package <- function(
    x,
    purpose = NULL,
    message_type = "error",
    ...) {
  
  if (message_type %in% c("error", "warning")) {
    # Attempt to load required packages.
    package_loaded <- sapply(x, requireNamespace, quietly = TRUE)
    
  } else {
    # Check whether packages are installed, without loading the packages.
    package_loaded <- sapply(x, is_package_installed)
  }
  
  # Skip further analysis if all packages could be loaded.
  if (all(package_loaded)) return(TRUE)
  
  # Find all packages that are missing.
  missing_packages <- x[!package_loaded]
  
  if (message_type == "error") {
    # Throw an error.
    ..error_package_not_installed(
      x = missing_packages,
      purpose = purpose)
    
  } else if (message_type == "warning") {
    # Raise a warning
    ..warning_package_not_installed(
      x = missing_packages,
      purpose = purpose)
    
  } else if (message_type == "backend_error") {
    # Add message to backend.
    ..message_package_not_installed_to_backend(
      x = missing_packages,
      purpose = purpose,
      message_type = "error")
    
  } else if (message_type == "backend_warning") {
    # Add message to backend.
    ..message_package_not_installed_to_backend(
      x = missing_packages,
      purpose = purpose,
      message_type = "warning")
  }
  
  return(FALSE)
}



# Check whether installed packages are outdated or newer.
.check_package_version <- function(
    name,
    version,
    when = NULL) {
  
  # Do not check if package versions are missing completely.
  if (is_empty(version)) return(invisible(NULL))
  
  # Check for outdated packages.
  package_outdated <- mapply(
    is_package_outdated,
    name = name,
    version = version)
  
  # Check for newer packages.
  package_newer <- mapply(
    is_package_newer,
    name = name,
    version = version)
  
  # Skip if no packages are outdated.
  if (!any(package_outdated) && !any(package_newer)) return(invisible(NULL))
  
  # Check whether one or more packages do not have the correct
  # version.
  multiple_packages <- sum(package_outdated + package_newer)
  
  # Form "when" string to describe when and for what the packages were initially
  # used.
  if (!is.null(when)) {
    when_str <- ifelse(
      multiple_packages,
      paste0(" from those ", when),
      paste0(" from that ", when))
    
  } else {
    when_str <- NULL
  }
  
  # Initial string.
  message_str <- paste0(
    "The following installed package",
    ifelse(
      multiple_packages,
      paste0("s have versions that differ", when_str, ":"),
      paste0(" has a version that differs", when_str, ":")))
  
  
  for (ii in seq_along(name)) {
    
    # Skip if package is not newer or outdated.
    if (!package_outdated[ii] && !package_newer[ii]) next
    
    # Parse package information.
    message_str <- c(
      message_str,
      paste0(
        name[ii],
        ": ",
        as.character(utils::packageVersion(name[ii])),
        ifelse(package_outdated[ii], " < ", " > "),
        as.character(version[ii]),
        ifelse(package_outdated[ii], " (outdated)", " (newer)")))
  }
  
  # Show as warning.
  warning(paste(message_str, sep = "\n"))
  
  return(invisible(TRUE))
}



is_package_installed <- function(name) {
  
  if (length(name) == 0) return(TRUE)
  
  # Try to obtain the package version. This perhaps the cleanest way to check
  # whether a package exists. require and requireNameSpace attach and load
  # packages, which is not required here. The find.package documentation
  # actively discourages its use to identify whether a package is installed.
  installed_version <- tryCatch(
    utils::packageVersion(name),
    error = identity)
  
  return(!inherits(installed_version, "error"))
}



is_package_outdated <- function(name, version) {
  
  if (length(name) == 0) return(TRUE)
  
  # Obtain the installed version of the package.
  installed_version <- tryCatch(
    utils::packageVersion(name),
    error = identity)
  
  if (inherits(installed_version, "error")) {
    ..error_package_not_installed(name)
  }
  
  # Make sure version is a package version.
  version <- as.package_version(version)
  
  return(version > installed_version)
} 



is_package_newer <- function(name, version) {
  
  if (length(name) == 0) return(TRUE)
  
  # Obtain the installed version of the package.
  installed_version <- tryCatch(
    utils::packageVersion(name),
    error = identity)
  
  if (inherits(installed_version, "error")) {
    ..error_package_not_installed(name)
  }
  
  # Make sure version is a package version.
  version <- as.package_version(version)
  
  return(version < installed_version)
}



.bioconductor_packages <- function() {
  return(NULL)
}



..message_package_not_installed_to_backend <- function(
    x,
    purpose,
    message_type) {
  
  # Generate message string.
  message_str <- ..message_missing_package(
    x = x,
    purpose = purpose)
  
  missing_packages <- NULL
  
  # Get existing missing package, if any.
  if (exists("missing_packages", where = familiar_global_env)) {
    missing_packages <- get(
      "missing_packages",
      envir = familiar_global_env)
  }
  
  # Append missing packages.
  missing_packages <- c(missing_packages, x)
  
  # Assign to backend.
  assign(
    "missing_packages",
    value = missing_packages,
    envir = familiar_global_env)
  
  
  missing_package_messages <- NULL
  
  # Get existing package messages, if any.
  if (exists("missing_package_messages", where = familiar_global_env)) {
    
    missing_package_messages <- get(
      "missing_package_messages",
      envir = familiar_global_env)
  }
  
  # Append package messages.
  missing_package_messages <- c(missing_package_messages, message_str)
  
  # Assign to backend.
  assign(
    "missing_package_messages",
    value = missing_package_messages,
    envir = familiar_global_env)
  
  missing_package_message_type <- NULL
  
  # Get existing message types for missing package.
  if (exists("missing_package_message_type", where = familiar_global_env)) {
    missing_package_message_type <- get(
      "missing_package_message_type",
      envir = familiar_global_env)
  }
  
  # Append missing packages.
  missing_package_message_type <- c(missing_package_message_type, message_type)
  
  # Assign to backend.
  assign(
    "missing_package_message_type",
    value = missing_package_message_type,
    envir = familiar_global_env)
  
  return(invisible(TRUE))
}


.report_missing_package_messages <- function() {
  
  # Do not report if either messages or message types are missing.
  if (!exists("missing_packages", where = familiar_global_env)) return()
  if (!exists("missing_package_message_type", where = familiar_global_env)) return()
  if (!exists("missing_package_messages", where = familiar_global_env)) return()
  
  # Get all missing packages.
  x <- get(
    "missing_packages",
    envir = familiar_global_env)
  
  # Only unique packages.
  x <- unique(x)
  
  # Retrieve package messages.
  err_message <- get(
    "missing_package_messages",
    envir = familiar_global_env)
  
  # Determine number of messages.
  n_messages <- length(err_message)
  
  # Combine error messages.
  if (n_messages > 1) err_message <- paste0(err_message, collapse = "\n")
  
  # Add basic message to install packages.
  if (n_messages > 1) err_message <- c(
    err_message, "\n",
    ..message_missing_package(x = x))
  
  # Instructions for CRAN packages.
  err_message <- c(
    err_message,
    ..message_install_from_cran(x = x))
  
  # Instructions for Bioconductor packages.
  err_message <- c(
    err_message,
    ..message_install_from_bioconductor(x = x))
  
  # Obtain message types.
  message_type <- get(
    "missing_package_message_type",
    envir = familiar_global_env)
  
  # Throw error or warning.
  if (any(message_type == "error")) {
    stop(paste0(err_message, collapse = ""))
    
  } else {
    warning(paste0(err_message, collapse = ""))
  }
  
  # Clean up
  rm(
    "missing_packages",
    "missing_package_message_type",
    "missing_package_messages",
    envir = familiar_global_env)
  
  return(invisible(TRUE))
}



..message_missing_package <- function(x, purpose = NULL) {
  
  # Select unique packages.
  x <- unique(x)
  
  # Check whether packages are not actually installed.
  all_missing <- all(!sapply(x, is_package_installed))
  
  if (all_missing) {
    message_str <- paste0(
      "The following package",
      ifelse(length(x) > 1, "s have", " has"),
      " to be installed",
      ifelse(is.null(purpose), ": ", paste0(" ", purpose, ": ")),
      paste_s(x), ".")
    
  } else {
    message_str <- paste0(
      "The following package",
      ifelse(length(x) > 1, "s have", " has"),
      " to be installed, or installed again to update dependencies",
      ifelse(is.null(purpose), ": ", paste0(" ", purpose, ": ")),
      paste_s(x), ".")
  }
  
  return(message_str)
}



..message_install_from_cran <- function(x) {
  
  message_str <- NULL
  
  # Select unique packages.
  x <- unique(x)
  x_cran <- setdiff(x, .bioconductor_packages())
  
  if (length(x_cran) > 0) {
    message_str <- paste0(
      "\n\nInstall from CRAN: ",
      "\n\n\tinstall.packages(c(",
      paste0("\"", x_cran, "\"", collapse = ", "), "))")
  }
  
  return(message_str)
}



..message_install_from_bioconductor <- function(x) {
  
  message_str <- NULL
  
  # Select unique packages.
  x <- unique(x)
  x_bioc <- intersect(x, .bioconductor_packages())
  
  if (length(x_bioc) > 0) {
    message_str <- paste0(
      "\n\nInstall from Bioconductor: ",
      ifelse(is_package_installed("BiocManager"), "", "\n\n\tinstall.packages(\"BiocManager\")"),
      "\n\tBiocManager::install(c(",
      paste0("\"", x_bioc, "\"", collapse = ", "), "))")
  }
  
  return(message_str)
}



..message_package_version <- function(x, version) {
  return(paste0(x, " (v", as.character(version), ")"))
}

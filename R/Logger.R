logger.message <- function(mess_str, file_name=NULL, indent=0L, verbose=TRUE){
  # Write message to console and file

  if (is.null(file_name)) file_name <- .get_log_file()

  # Derive an indent string using two spaces for each indentation.
  indent_str <- paste0(rep("  ", indent), collapse = "")

  # Date and time string
  date_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Parse date and time with message string to log string
  log_str <- paste0(date_str, "\tMESSAGE\t", indent_str, mess_str)

  # Write message to log file
  if (!is.null(file_name)) {
    tryCatch(write(
      x = log_str,
      file = file_name,
      append = TRUE))
  }

  # Write message to console
  if (verbose) message(paste0(indent_str, mess_str))
  
  return(invisible(NULL))
}



logger.warning <- function(
    warn_str,
    file_name = NULL) {
  # Write warning to console and file

  if (is.null(file_name)) file_name <- .get_log_file()

  # Date and time string
  date_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Parse date and time with warning string to log string
  log_str <- paste0(date_str, "\tWARNING\t", warn_str)

  # Write warning to log file
  if (!is.null(file_name)) {
    tryCatch(write(
      x = log_str,
      file = file_name,
      append = TRUE))
  }

  # Write warning to console
  warning(warn_str)
  
  return(invisible(NULL))
}



logger.stop <- function(
    err_str,
    file_name = NULL) {
  # Write error to console and file

  if (is.null(file_name)) file_name <- .get_log_file()

  # Date and time string
  date_str <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Parse date and time with error string to log string
  log_str <- paste0(date_str, "\tERROR\t", err_str)

  # Write error to log file
  if (!is.null(file_name)) {
    tryCatch(write(x = log_str, file = file_name, append = TRUE))
  }

  # Write error to console
  stop(err_str)
}



.get_log_file <- function() {
  # If the log file cannot be found return NULL.
  log_file <- tryCatch(
    get("log_file", envir = familiar_global_env),
    error = function(err) (NULL)
  )

  return(log_file)
}

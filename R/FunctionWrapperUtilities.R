do.call_strict <- function(
    what,
    args,
    quote = FALSE,
    envir = parent.frame()) {
  # Only pass fully matching arguments. Side effect is that ... is always empty.
  # Use with care in case arguments need to be passed to another function.

  # Get arguments that can be passed.
  passable_argument_names <- intersect(
    names(formals(what)),
    names(args))

  return(do.call(
    what, 
    args = args[passable_argument_names],
    quote = quote, 
    envir = envir))
}



do.call_with_timeout <- function(
    what, 
    args, 
    timeout, 
    quote = FALSE, 
    envir = parent.frame(),
    muffle = TRUE, 
    additional_packages = NULL) {
  
  # Set up the muffler that captures all output.
  muffle_fun <- identity
  if (muffle) muffle_fun <- quiet

  if (!require_package(
    "callr",
    purpose = "to allow for executing functions with timeout",
    message_type = "warning")) {
    # If callr cannot be loaded, execute the function as is.
    muffle_fun(value <- do.call(
      what = what,
      args = args, 
      envir = envir))

    # Return data.
    return(list(
      "value" = value,
      "timeout" = FALSE))
    
  } else if (!is.finite(timeout)) {
    # If there is no finite timeout, execute the function as is.
    muffle_fun(value <- do.call(
      what = what, 
      args = args, 
      envir = envir))

    # Return data.
    return(list(
      "value" = value,
      "timeout" = FALSE))
    
  } else {
    bg_function_wrapper <- function(what, args, additional_packages) {
      if (!is.null(bg_function_wrapper)) {
        for (package in additional_packages) {
          requireNamespace(package)
        }
      }

      do.call(what = what, args = args)
    }

    # Launch as background to allow for non-blocking timeout checks.
    bg_process <- callr::r_bg(
      func = bg_function_wrapper,
      args = list(
        "what" = what,
        "args" = args,
        "additional_packages" = additional_packages),
      package = TRUE)

    # Wait until the timeout, or until the background process completes,
    # whichever happens first.
    bg_process$wait(timeout)

    # If the process is still running past the timeout, kill it.
    if (bg_process$is_alive()) {
      bg_process$kill()

      return(list("value" = NULL, "timeout" = TRUE))
      
    } else {
      return(list("value" = bg_process$get_result(), "timeout" = FALSE))
    }
  }
}



do.call_with_handlers_timeout <- function(
    what, 
    args, 
    timeout, 
    quote = FALSE, 
    envir = parent.frame(), 
    muffle = TRUE, 
    additional_packages = NULL) {
  # Pass to do.call_with_timeout, which then calls do.call_with_handlers on
  # "what" with specified arguments "args".
  results <- do.call_with_timeout(
    what = do.call_with_handlers,
    args = list(
      "what" = what,
      "args" = args,
      "quote" = quote,
      "envir" = envir,
      "muffle" = muffle),
    timeout = timeout,
    muffle = muffle,
    additional_packages = additional_packages)

  # Flatten results.
  if (!is.null(results$value)) {
    results$error <- results$value$error
    results$warning <- results$value$warning
    results$value <- results$value$value
  }

  return(results)
}



do.call_with_handlers <- function(
    what, 
    args, 
    quote = FALSE, 
    envir = parent.frame(),
    muffle = TRUE) {
  # Set up the muffler that captures all output.
  muffle_fun <- identity
  if (muffle) muffle_fun <- quiet

  # Set up initial warning and error logs. Note that the error log is only
  # written after the tryCatch.
  warn_logs <- error_logs <- NULL

  muffle_fun(value <- tryCatch(
    withCallingHandlers(
      do.call(what = what, args = args, envir = envir),
      warning = function(w) {
        # Since warn_logs is found in the enclosing frame, we either have to use
        # <<- to access and update this variable. However, this causes CRAN
        # notes, because <<- can be a global assignment, i.e. in the user space.
        # Hence we explicitly use get and assign to update the variable.
        warn_logs <- assign(
          "warn_logs",
          append(get("warn_logs"), condition_parser(w)),
          inherits = TRUE)
        
        # This prevents any warnings from being generated outside the function.
        invokeRestart("muffleWarning")
      }),
    error = identity))
  
  if (inherits(value, "error")) {
    error_logs <- condition_parser(value)
    value <- NULL
  }

  return(list(
    "value" = value,
    "warning" = warn_logs,
    "error" = error_logs))
}



condition_parser <- function(x, ...) {
  # Based on print.condition

  # Find information on the condition message and call.
  condition_message <- conditionMessage(x)
  condition_call <- conditionCall(x)

  # Parse condition to string.
  if (!is.null(condition_call)) {
    # Get the condition call. Also, just in case, select only the first section
    # that contains the function itself.
    deparsed_condition_call <- deparse1(condition_call)[1]

    # We remove the function arguments from the call to prevent leaking data, if
    # any. Identify the first parenthesis that opens the argument section.
    parenthesis_position <- regexpr(
      text = deparsed_condition_call,
      pattern = "(",
      fixed = TRUE
    )[1]

    # Check if a parenthesis appears in the initial section of the call.
    if (parenthesis_position > -1) {
      # Select the function name which appears prior to the parenthesis.
      deparsed_condition_call <- substr(deparsed_condition_call,
        start = 1L,
        stop = parenthesis_position - 1L)
      
    } else {
      # If no parenthesis appears, the function call was likely cut off prior to
      # the argument section. We mark this abbreviation.
      deparsed_condition_call <- paste0(deparsed_condition_call, "[...]")
    }

    if (nchar(deparsed_condition_call) > 0) {
      # Add in the deparsed condition call, and combine.
      parsed_condition <- paste0(
        deparsed_condition_call, ": ",
        condition_message)
      
    } else {
      parsed_condition <- condition_message
    }
    
  } else {
    # Set message.
    parsed_condition <- condition_message
  }

  return(parsed_condition)
}



condition_summary <- function(x) {
  # Convert x to a data.table.
  data <- data.table::data.table("condition_message" = x)
  
  # Count the number of times each unique condition message appears.
  data <- data[, list("n" = .N, "id" = .GRP), by = "condition_message"]
  
  # Parse each line.
  summary_vector <- sapply(
    split(data, by = "condition_message"),
    function(x) {
      # Initialise using the id, e.g. "3. "
      message_string <- paste0(as.character(x$id), ". ")
      
      # Show the number of times the condition has appeared, but ignore if it
      # only appeared once.
      if (x$n > 1) {
        message_string <- c(message_string, paste0("(", x$n, "x) "))
      }
      
      # Add the condition message itself.
      message_string <- c(message_string, x$condition_message)
      
      return(paste0(message_string, collapse = ""))
    },
    USE.NAMES = FALSE)
  
  return(unname(summary_vector))
}



suppress_warnings <- function(
    expr,
    classes = "warning",
    regexp = NULL
) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      # Attempt to silence a warning. A warning is silenced if it is one of the
      # provided classes. In addition, if regexp is set, it is silenced only
      # when the warning matches one of the classes, and has the regexp in its
      # message.
      if (rlang::inherits_any(w, classes)) {
        if (is.null(regexp)) {
          tryInvokeRestart("muffleWarning")
          
        } else if (any(sapply(regexp, grepl, x = conditionMessage(w)))) {
          tryInvokeRestart("muffleWarning")
        }
      }
    }
  )
}



quiet <- function(x) {
  # Removes all output to console.

  sink(nullfile())
  on.exit(sink())

  invisible(utils::capture.output(x, file = nullfile(), type = "message"))
}

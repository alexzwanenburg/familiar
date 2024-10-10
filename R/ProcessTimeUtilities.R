# Clock for process timing.
ProcessClock <- setRefClass(
  "processClock",
  fields = list(
    clock_process = "ANY",
    start_time = "numeric",
    interrupt_time = "ANY"
  ),
  methods = list(
    "initialize" = function() {
      # Make sure that the packages are installed and can their namespaces can
      # be attached.
      require_package(c("callr", "microbenchmark"),
        purpose = "to measure process time"
      )

      # Set-up fields.
      .self$initFields(
        clock_process = NULL,
        start_time = 0.0,
        interrupt_time = NULL
      )

      # Start the clock process to look for interrupts, such as by
      # hibernating the system.
      .self$clock_process <- callr::r_bg(function() {
        time_start <- microbenchmark::get_nanotime()
        while (TRUE) {
          # Sleep for some time (hypothetically, 50ms, but realistically
          # anywhere between 50 and 90ms). Sys.sleep is not really accurate.
          # Luckily accuracy is not really required. Its just that most process
          # time should be spend asleep.
          Sys.sleep(0.05)

          # Measure the current time.
          time_current <- microbenchmark::get_nanotime()

          # Warn for interrupts if, for some reason (e.g. process suspension, OS
          # hibernation, etc.) the process slept for at least 1 second.
          if ((time_current - time_start) > 1.0E9) {
            cat(format(time_start, scientific = FALSE),
              ":",
              format(time_current, scientific = FALSE),
              "\n",
              sep = ""
            )

            # Correct for longer writing times.
            time_start <- microbenchmark::get_nanotime()
          } else {
            # Assume that the if-statement completes instantly.
            time_start <- time_current
          }
        }
      })

      # Set the start time. This is set after creating the clock process,
      # because launching the callr::r_bg asynchronous R background environment
      # takes a non-trivial amount of time.
      .self$start_time <- microbenchmark::get_nanotime()
    },
    
    "update_interrupt" = function() {
      # Suppress NOTES due to non-standard evaluation in data.table
      time_end <- time_start <- NULL

      # Read any interrupted time from the clock process. For some unobvious
      # reason the first read may not generate anything, but the second will.
      interrupt <- c(
        .self$clock_process$read_output_lines(),
        .self$clock_process$read_output_lines()
      )

      if (length(interrupt) > 0L) {
        # Split into start and endtimes.
        interrupt <- strsplit(x = interrupt, split = ":")

        interrupt_data <- data.table::data.table(
          "time_start" = as.numeric(sapply(interrupt, function(x) x[1L], USE.NAMES = FALSE)),
          "time_end" = as.numeric(sapply(interrupt, function(x) x[2L], USE.NAMES = FALSE))
        )

        # Update interrupt_time field.
        .self$interrupt_time <- rbind(.self$interrupt_time, interrupt_data)
      }
    },
    
    "accumulate_interrupt" = function(ref_start_time, ref_end_time) {
      # Suppress NOTES due to non-standard evaluation in data.table
      time_end <- time_start <- NULL

      # If there are no recorded interrupts, rejoice and return 0.
      if (is.null(.self$interrupt_time)) return(0.0)

      # Select only data that contains (part of) the reference time interval.
      # Work on a copy of the data to avoid updating by reference.
      interrupt_data <- data.table::copy(
        .self$interrupt_time[ref_start_time < time_end & ref_end_time > time_start]
      )

      # If all interrupts fall outside the reference time interval, return 0.
      if (nrow(interrupt_data) == 0L) return(0.0)

      # Update partial interrupt windows.
      interrupt_data[ref_start_time > time_start, "time_start" := ref_start_time]
      interrupt_data[ref_end_time < time_end, "time_end" := ref_end_time]

      # Set measured interrupt durations.
      interrupt_data[, "duration" := time_end - time_start]

      # Sum durations.
      total_interrupt_duration <- sum(interrupt_data$duration)

      # Return 0 if the total interrupt was smaller than 1s.
      if (total_interrupt_duration < 1.0E9) return(0.0)

      return(total_interrupt_duration)
    },
    
    "time" = function(units = "s", reference_time = NULL) {
      # Find current time.
      current_time <- microbenchmark::get_nanotime()

      # If the reference time is not provided, use the process start time field
      # instead.
      if (is.null(reference_time)) reference_time <- .self$start_time

      # Update tables with interrupt times, if any.
      .self$update_interrupt()

      # Accumulate interrupt time.
      sum_interrupt_time <- .self$accumulate_interrupt(
        ref_start_time = reference_time,
        ref_end_time = current_time
      )

      conversion_factor <- switch(units,
        "secs" = 1.0E9,
        "sec" = 1.0E9,
        "s" = 1.0E9,
        "mins" = 60.0E9,
        "min" = 60.0E9,
        "m" = 60.0E9,
        "hours" = 3600.0E9,
        "hour" = 3600.0E9,
        "h" = 3600.0E9,
        "millisecs" = 1.0E6,
        "millisec" = 1.0E6,
        "milli" = 1.0E6,
        "ms" = 1.0E6,
        "microsecs" = 1.0E3,
        "microsec" = 1.0E3,
        "micro" = 1.0E3,
        "us" = 1.0E3,
        "nanosecs" = 1.0,
        "nanosec" = 1.0,
        "nano" = 1.0,
        "ns" = 1.0
      )

      return((current_time - sum_interrupt_time - reference_time) / conversion_factor)
    },
    
    "close" = function() {
      .self$clock_process$kill()
    },
    
    "suspend" = function() {
      .self$clock_process$suspend()
      invisible(TRUE)
    },
    
    "resume" = function() {
      .self$clock_process$resume()
      invisible(TRUE)
    }
  )
)

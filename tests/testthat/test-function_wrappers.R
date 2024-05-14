testthat::skip_on_cran()
testthat::skip_on_ci()

test_fun_normal <- function(x = "default") {
  Sys.sleep(1)
  return(x)
}

test_fun_warning <- function(x = "default") {
  warning("warning")
  Sys.sleep(1)
  return(x)
}

test_fun_error <- function(x = "default") {
  warning("warning")
  Sys.sleep(1)
  stop("error")
  return(x)
}

testthat::test_that("Processes that complete within the timeout function correctly", {
  results_normal <- familiar:::do.call_with_timeout(
    test_fun_normal, 
    list("x" = "done"),
    timeout = 10000)
  
  testthat::expect_equal(results_normal$value, "done")
  testthat::expect_equal(results_normal$timeout, FALSE)
  
  results_warning <- familiar:::do.call_with_timeout(
    test_fun_warning, 
    list("x" = "done"),
    timeout = 10000)
  
  testthat::expect_equal(results_warning$value, "done")
  testthat::expect_equal(results_warning$timeout, FALSE)
  
  testthat::expect_error(
    familiar:::do.call_with_timeout(
      test_fun_error, 
      list("x" = "done"), 
      timeout = 10000), 
    "error")
})

testthat::test_that("Processes that complete outside the timeout fail", {
  results_normal <- familiar:::do.call_with_timeout(
    test_fun_normal, 
    list("x" = "done"),
    timeout = 500)

  testthat::expect_equal(is.null(results_normal$value), TRUE)
  testthat::expect_equal(results_normal$timeout, TRUE)

  results_warning <- familiar:::do.call_with_timeout(
    test_fun_warning, 
    list("x" = "done"),
    timeout = 500)

  testthat::expect_equal(is.null(results_warning$value), TRUE)
  testthat::expect_equal(results_warning$timeout, TRUE)

  results_error <- familiar:::do.call_with_timeout(
    test_fun_error, 
    list("x" = "done"),
    timeout = 500)

  testthat::expect_equal(is.null(results_error$value), TRUE)
  testthat::expect_equal(results_error$timeout, TRUE)
})


testthat::test_that("Processes that are called with handles function correctly", {
  results_normal <- familiar:::do.call_with_handlers(
    test_fun_normal, 
    list("x" = "done"))

  testthat::expect_equal(results_normal$value, "done")
  testthat::expect_equal(is.null(results_normal$warning), TRUE)
  testthat::expect_equal(is.null(results_normal$error), TRUE)

  results_warning <- familiar:::do.call_with_handlers(
    test_fun_warning, 
    list("x" = "done"))

  testthat::expect_equal(results_warning$value, "done")
  testthat::expect_equal(results_warning$warning, "warning")
  testthat::expect_equal(is.null(results_warning$error), TRUE)

  results_error <- familiar:::do.call_with_handlers(
    test_fun_error, 
    list("x" = "done"))

  testthat::expect_equal(is.null(results_error$value), TRUE)
  testthat::expect_equal(results_error$warning, "warning")
  testthat::expect_equal(results_error$error, "error")
})

testthat::test_that("Processes that are called with handles and timeout function correctly", {
  results_normal <- familiar:::do.call_with_handlers_timeout(
    test_fun_normal, 
    list("x" = "done"),
    timeout = 10000)

  testthat::expect_equal(results_normal$value, "done")
  testthat::expect_equal(is.null(results_normal$warning), TRUE)
  testthat::expect_equal(is.null(results_normal$error), TRUE)
  testthat::expect_equal(results_normal$timeout, FALSE)

  results_warning <- familiar:::do.call_with_handlers_timeout(
    test_fun_warning, 
    list("x" = "done"), 
    timeout = 10000)

  testthat::expect_equal(results_warning$value, "done")
  testthat::expect_equal(results_warning$warning, "warning")
  testthat::expect_equal(is.null(results_warning$error), TRUE)
  testthat::expect_equal(results_warning$timeout, FALSE)

  results_error <- familiar:::do.call_with_handlers_timeout(
    test_fun_error, 
    list("x" = "done"), 
    timeout = 10000)

  testthat::expect_equal(is.null(results_error$value), TRUE)
  testthat::expect_equal(results_error$warning, "warning")
  testthat::expect_equal(results_error$error, "error")
  testthat::expect_equal(results_error$timeout, FALSE)
})

testthat::test_that("Processes that are called with handles and infinite timeout function correctly", {
  results_normal <- familiar:::do.call_with_handlers_timeout(
    test_fun_normal,
    list("x" = "done"),
    timeout = Inf)

  testthat::expect_equal(results_normal$value, "done")
  testthat::expect_equal(is.null(results_normal$warning), TRUE)
  testthat::expect_equal(is.null(results_normal$error), TRUE)
  testthat::expect_equal(results_normal$timeout, FALSE)

  results_warning <- familiar:::do.call_with_handlers_timeout(
    test_fun_warning, 
    list("x" = "done"),
    timeout = Inf)

  testthat::expect_equal(results_warning$value, "done")
  testthat::expect_equal(results_warning$warning, "warning")
  testthat::expect_equal(is.null(results_warning$error), TRUE)
  testthat::expect_equal(results_warning$timeout, FALSE)

  results_error <- familiar:::do.call_with_handlers_timeout(
    test_fun_error, 
    list("x" = "done"), 
    timeout = Inf)

  testthat::expect_equal(is.null(results_error$value), TRUE)
  testthat::expect_equal(results_error$warning, "warning")
  testthat::expect_equal(results_error$error, "error")
  testthat::expect_equal(results_error$timeout, FALSE)
})



testthat::test_that("Processes that complete outside timeout fail", {
  results_normal <- familiar:::do.call_with_handlers_timeout(
    test_fun_normal, 
    list("x" = "done"),
    timeout = 500)

  testthat::expect_equal(is.null(results_normal$value), TRUE)
  testthat::expect_equal(is.null(results_normal$warning), TRUE)
  testthat::expect_equal(is.null(results_normal$error), TRUE)
  testthat::expect_equal(results_normal$timeout, TRUE)

  results_warning <- familiar:::do.call_with_handlers_timeout(
    test_fun_warning, 
    list("x" = "done"), 
    timeout = 500)

  testthat::expect_equal(is.null(results_warning$value), TRUE)
  testthat::expect_equal(is.null(results_warning$warning), TRUE)
  testthat::expect_equal(is.null(results_warning$error), TRUE)
  testthat::expect_equal(results_warning$timeout, TRUE)

  results_error <- familiar:::do.call_with_handlers_timeout(
    test_fun_error, 
    list("x" = "done"), 
    timeout = 500)

  testthat::expect_equal(is.null(results_error$value), TRUE)
  testthat::expect_equal(is.null(results_error$warning), TRUE)
  testthat::expect_equal(is.null(results_error$error), TRUE)
  testthat::expect_equal(results_error$timeout, TRUE)
})

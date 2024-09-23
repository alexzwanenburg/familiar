# Don't perform any further tests on CRAN due to parallelisation issues.
testthat::skip_on_cran()
testthat::skip_on_ci()

apply_vector_fun <- function(x) {
  Sys.sleep(x)

  return(x)
}

apply_vector_fun_2 <- function(x, y) {
  Sys.sleep(x)

  return(y)
}

apply_list_fun <- function(x) {
  Sys.sleep(x)

  return(list("x" = x))
}

apply_list_fun_2 <- function(x, y) {
  Sys.sleep(x)

  return(list("x" = x, "y" = y))
}

apply_list_fun_3 <- function(x, y, z) {
  Sys.sleep(x)

  return(list("x" = x, "y" = y, "z" = z))
}

# Start local cluster in the overall process.
cl <- familiar:::.test_start_cluster(2L)


# Generate random values.
x_sequential <- stats::rnorm(5, mean = 1.0, sd = 0.2)
x_sequential[x_sequential < 0.2] <- 0.2

x_sequential_named <- x_sequential
names(x_sequential_named) <- letters[seq_along(x_sequential_named)]

x_single <- x_sequential[1]

x_parallel <- stats::rnorm(20, mean = 1.0, sd = 0.2)
x_parallel[x_parallel < 0.2] <- 0.2

x_parallel_named <- x_parallel
names(x_parallel_named) <- letters[seq_along(x_parallel_named)]

# fam_sapply -------------------------------------------------------------------

testthat::test_that("fam_sapply works correctly.", {
  # Simple sequential fam_sapply
  x <- familiar:::fam_sapply(
    cl = NULL,
    assign = NULL,
    X = x_sequential,
    FUN = apply_vector_fun)

  testthat::expect_equal(x, x_sequential)

  # Simple sequential fam_sapply with names
  x <- familiar:::fam_sapply(
    cl = NULL,
    assign = NULL,
    X = x_sequential_named,
    FUN = apply_vector_fun)

  testthat::expect_equal(x, x_sequential_named)

  # Sequential fam_sapply with extra argument.
  x <- familiar:::fam_sapply(
    cl = NULL,
    assign = NULL,
    X = x_sequential,
    FUN = apply_vector_fun_2,
    y = "A")

  testthat::expect_length(x, length(x_sequential))
  testthat::expect_setequal(x, "A")

  # Sequential fam_sapply with mini-batching (does nothing)
  x <- familiar:::fam_sapply(
    cl = NULL,
    assign = NULL,
    X = x_sequential,
    FUN = apply_vector_fun,
    chopchop = TRUE)

  testthat::expect_equal(x, x_sequential)

  # Simple parallel fam_sapply.
  x <- familiar:::fam_sapply(
    cl = cl,
    assign = NULL,
    X = x_sequential,
    FUN = apply_vector_fun)

  testthat::expect_equal(x, x_sequential)

  # Simple parallel fam_sapply with names.
  x <- familiar:::fam_sapply(
    cl = cl,
    assign = NULL,
    X = x_sequential_named,
    FUN = apply_vector_fun)

  testthat::expect_equal(x, x_sequential_named)

  # Parallel fam_sapply with extra argument.
  x <- familiar:::fam_sapply(
    cl = cl,
    assign = NULL,
    X = x_sequential,
    FUN = apply_vector_fun_2,
    y = "A")

  testthat::expect_length(x, length(x_sequential))
  testthat::expect_setequal(x, "A")

  # Parallel fam_sapply with mini-batching.
  x <- familiar:::fam_sapply(
    cl = cl,
    assign = NULL,
    X = x_parallel,
    FUN = apply_vector_fun,
    chopchop = TRUE)

  testthat::expect_equal(x, x_parallel)

  # Parallel fam_sapply with mini-batching and "predicted" process times.
  x <- familiar:::fam_sapply(
    cl = cl,
    assign = NULL,
    X = x_parallel,
    FUN = apply_vector_fun,
    chopchop = TRUE,
    process_time = x_parallel,
    overhead_time = 0.1)

  testthat::expect_equal(x, x_parallel)

  # Parallel fam_sapply with mini-batching and "predicted" process times.
  x <- familiar:::fam_sapply(
    cl = cl,
    assign = NULL,
    X = x_parallel_named,
    FUN = apply_vector_fun,
    chopchop = TRUE,
    process_time = x_parallel,
    overhead_time = 0.1)

  testthat::expect_equal(x, x_parallel_named)

  # Parallel, load-balanced sapply.
  x <- familiar:::fam_sapply_lb(
    cl = cl,
    assign = NULL,
    X = x_sequential,
    FUN = apply_vector_fun)

  testthat::expect_equal(x, x_sequential)

  # Parallel, load-balanced sapply with time measurement.
  x <- familiar:::fam_sapply_lb(
    cl = cl,
    assign = NULL,
    X = x_sequential,
    FUN = apply_vector_fun_2,
    y = "A",
    MEASURE.TIME = TRUE)

  testthat::expect_length(x$results, length(x_sequential))
  testthat::expect_setequal(x$results, "A")

  testthat::expect_equal(x$process_time, x_sequential, tolerance = 0.1)

  # Sequential sapply with NULL
  x <- familiar:::fam_sapply(
    cl = NULL,
    assign = NULL,
    X = NULL,
    FUN = apply_vector_fun)

  testthat::expect_equal(x, list())

  # Parallel sapply with NULL
  x <- familiar:::fam_sapply(
    cl = cl,
    assign = NULL,
    X = NULL,
    FUN = apply_vector_fun)

  testthat::expect_equal(x, list())

  # Parallel sapply with mini-batching and NULL
  x <- familiar:::fam_sapply(
    cl = cl,
    assign = NULL,
    X = NULL,
    FUN = apply_vector_fun,
    chopchop = TRUE)

  testthat::expect_equal(x, list())

  # Sequential sapply with single value
  x <- familiar:::fam_sapply(
    cl = NULL,
    assign = NULL,
    X = x_single,
    FUN = apply_vector_fun)

  testthat::expect_equal(x, x_single)

  # Parallel sapply with single value
  x <- familiar:::fam_sapply(
    cl = cl,
    assign = NULL,
    X = x_single,
    FUN = apply_vector_fun)

  testthat::expect_equal(x, x_single)

  # Parallel sapply with mini-batching and single value.
  x <- familiar:::fam_sapply(
    cl = cl,
    assign = NULL,
    X = x_single,
    FUN = apply_vector_fun,
    chopchop = TRUE)

  testthat::expect_equal(x, x_single)
})

# fam_lapply -------------------------------------------------------------------

testthat::test_that("fam_lapply works correctly.", {
  # Simple sequential lapply.
  x <- familiar:::fam_lapply(
    cl = NULL,
    assign = NULL,
    X = x_sequential,
    FUN = apply_list_fun)

  testthat::expect_equal(unname(unlist(x)), x_sequential)

  # Simple sequential fam_sapply with names
  x <- familiar:::fam_lapply(
    cl = NULL,
    assign = NULL,
    X = x_sequential_named,
    FUN = apply_list_fun)

  testthat::expect_equal(names(x), names(x_sequential_named))
  testthat::expect_equal(unname(unlist(x)), x_sequential)

  # Simple sequential lapply with extra argument.
  x <- familiar:::fam_lapply(
    cl = NULL,
    assign = NULL,
    X = x_sequential,
    FUN = apply_list_fun_2,
    y = "A")

  testthat::expect_equal(sapply(x, function(x) (x$x)), x_sequential)
  testthat::expect_setequal(sapply(x, function(x) (x$y)), "A")

  # Sequential lapply with mini-batching (no effect).
  x <- familiar:::fam_lapply(
    cl = NULL,
    assign = NULL,
    X = x_sequential,
    FUN = apply_list_fun,
    chopchop = TRUE)

  testthat::expect_equal(unname(unlist(x)), x_sequential)

  # Parallel lapply.
  x <- familiar:::fam_lapply(
    cl = cl,
    assign = NULL,
    X = x_sequential,
    FUN = apply_list_fun_2,
    y = "A")

  testthat::expect_equal(sapply(x, function(x) (x$x)), x_sequential)
  testthat::expect_setequal(sapply(x, function(x) (x$y)), "A")

  # Parallel lapply with mini-batching.
  x <- familiar:::fam_lapply(
    cl = cl,
    assign = NULL,
    X = x_parallel,
    FUN = apply_list_fun,
    chopchop = TRUE)

  testthat::expect_equal(unname(unlist(x)), x_parallel)

  # Parallel lapply with mini-batching and named elements.
  x <- familiar:::fam_lapply(
    cl = cl,
    assign = NULL,
    X = x_parallel_named,
    FUN = apply_list_fun,
    chopchop = TRUE)

  testthat::expect_equal(names(x), names(x_parallel_named))
  testthat::expect_equal(unname(unlist(x)), x_parallel)

  # Parallel lapply with mini-batching and "predicted" process times.
  x <- familiar:::fam_lapply(
    cl = cl,
    assign = NULL,
    X = x_parallel,
    FUN = apply_list_fun,
    chopchop = TRUE,
    process_time = x_parallel,
    overhead_time = 0.1)

  testthat::expect_equal(unname(unlist(x)), x_parallel)

  # Parallel lapply with mini-batching and "predicted" process times.
  x <- familiar:::fam_lapply(
    cl = cl,
    assign = NULL,
    X = x_parallel_named,
    FUN = apply_list_fun,
    chopchop = TRUE,
    process_time = x_parallel,
    overhead_time = 0.1)

  testthat::expect_equal(names(x), names(x_parallel_named))
  testthat::expect_equal(unname(unlist(x)), x_parallel)

  # Parallel lapply with mini-batching, named elements and vector output.
  x <- familiar:::fam_lapply(
    cl = cl,
    assign = NULL,
    X = x_parallel_named,
    FUN = apply_vector_fun,
    chopchop = TRUE)

  testthat::expect_equal(names(x), names(x_parallel_named))
  testthat::expect_equal(unlist(x), x_parallel_named)

  # Parallel lapply with load balancing.
  x <- familiar:::fam_lapply_lb(
    cl = cl,
    assign = NULL,
    X = x_sequential,
    FUN = apply_list_fun)

  testthat::expect_equal(unname(unlist(x)), x_sequential)

  # Parallel lapply with load balancing and time measurement.
  x <- familiar:::fam_lapply_lb(
    cl = cl,
    assign = NULL,
    X = x_sequential,
    FUN = apply_list_fun_2,
    y = "A",
    MEASURE.TIME = TRUE)

  testthat::expect_equal(sapply(x$results, function(x) (x$x)), x_sequential)
  testthat::expect_setequal(sapply(x$results, function(x) (x$y)), "A")

  testthat::expect_equal(x$process_time, x_sequential, tolerance = 0.1)

  # Sequential lapply with NULL
  x <- familiar:::fam_lapply(
    cl = NULL,
    assign = NULL,
    X = NULL,
    FUN = apply_list_fun)

  testthat::expect_equal(x, list())

  # Parallel lapply with NULL
  x <- familiar:::fam_lapply(
    cl = cl,
    assign = NULL,
    X = NULL,
    FUN = apply_list_fun)

  testthat::expect_equal(x, list())

  # Parallel lapply with mini-batching and NULL
  x <- familiar:::fam_lapply(
    cl = cl,
    assign = NULL,
    X = NULL,
    FUN = apply_list_fun,
    chopchop = TRUE)

  testthat::expect_equal(x, list())

  # Sequential lapply with single value
  x <- familiar:::fam_lapply(
    cl = NULL,
    assign = NULL,
    X = x_single,
    FUN = apply_list_fun)

  testthat::expect_equal(unname(unlist(x)), x_single)

  # Parallel lapply with single value
  x <- familiar:::fam_lapply(
    cl = cl,
    assign = NULL,
    X = x_single,
    FUN = apply_list_fun)

  testthat::expect_equal(unname(unlist(x)), x_single)

  # Parallel lapply with mini-batching and single value.
  x <- familiar:::fam_lapply(
    cl = cl,
    assign = NULL,
    X = x_single,
    FUN = apply_list_fun,
    chopchop = TRUE)

  testthat::expect_equal(unname(unlist(x)), x_single)
})



# fam_mapply -------------------------------------------------------------------

testthat::test_that("fam_mapply works correctly.", {
  # Simple sequential mapply,
  x <- familiar:::fam_mapply(
    cl = NULL,
    assign = NULL,
    FUN = apply_list_fun,
    x = x_sequential)

  testthat::expect_equal(unname(unlist(x)), x_sequential)

  # Simple sequential mapply, with additional argument.
  x <- familiar:::fam_mapply(
    cl = NULL,
    assign = NULL,
    FUN = apply_list_fun_2,
    x = x_sequential,
    y = seq_along(x_sequential))

  testthat::expect_equal(sapply(x, function(x) (x$x)), x_sequential)
  testthat::expect_equal(sapply(x, function(x) (x$y)), seq_along(x_sequential))

  # Simple sequential mapply, with two additional arguments.
  x <- familiar:::fam_mapply(
    cl = NULL,
    assign = NULL,
    FUN = apply_list_fun_3,
    x = x_sequential,
    y = seq_along(x_sequential),
    MoreArgs = list("z" = "A"))

  testthat::expect_equal(sapply(x, function(x) (x$x)), x_sequential)
  testthat::expect_equal(sapply(x, function(x) (x$y)), seq_along(x_sequential))
  testthat::expect_setequal(sapply(x, function(x) (x$z)), "A")

  # Simple sequential mapply, with two additional arguments, and named input.
  x <- familiar:::fam_mapply(
    cl = NULL,
    assign = NULL,
    FUN = apply_list_fun_3,
    x = x_sequential_named,
    y = seq_along(x_sequential_named),
    MoreArgs = list("z" = "A"))

  testthat::expect_equal(names(x), names(x_sequential_named))
  testthat::expect_equal(unname(sapply(x, function(x) (x$x))), x_sequential)
  testthat::expect_equal(unname(sapply(x, function(x) (x$y))), seq_along(x_sequential))
  testthat::expect_setequal(unname(sapply(x, function(x) (x$z))), "A")

  # Sequential mapply with mini-batching (no effect)
  x <- familiar:::fam_mapply(
    cl = NULL,
    assign = NULL,
    FUN = apply_list_fun_2,
    x = x_sequential,
    y = seq_along(x_sequential),
    chopchop = TRUE)

  testthat::expect_equal(sapply(x, function(x) (x$x)), x_sequential)
  testthat::expect_equal(sapply(x, function(x) (x$y)), seq_along(x_sequential))

  # Simple parallel mapply.
  x <- familiar:::fam_mapply(
    cl = cl,
    assign = NULL,
    FUN = apply_list_fun_2,
    x = x_sequential,
    y = seq_along(x_sequential))

  testthat::expect_equal(sapply(x, function(x) (x$x)), x_sequential)
  testthat::expect_equal(sapply(x, function(x) (x$y)), seq_along(x_sequential))

  # Parallel mapply with mini-batching.
  x <- familiar:::fam_mapply(
    cl = cl,
    assign = NULL,
    FUN = apply_list_fun_2,
    x = x_parallel,
    y = seq_along(x_parallel),
    chopchop = TRUE)

  testthat::expect_equal(sapply(x, function(x) (x$x)), x_parallel)
  testthat::expect_equal(sapply(x, function(x) (x$y)), seq_along(x_parallel))

  # Parallel mapply with mini-batching and "predicted" process times.
  x <- familiar:::fam_mapply(
    cl = cl,
    assign = NULL,
    FUN = apply_list_fun_2,
    x = x_parallel,
    y = seq_along(x_parallel),
    process_time = x_parallel,
    overhead_time = 0.1,
    chopchop = TRUE)

  testthat::expect_equal(sapply(x, function(x) (x$x)), x_parallel)
  testthat::expect_equal(sapply(x, function(x) (x$y)), seq_along(x_parallel))

  # Parallel mapply with mini-batching, "predicted" process times and additional
  # arguments.
  x <- familiar:::fam_mapply(
    cl = cl,
    assign = NULL,
    FUN = apply_list_fun_3,
    x = x_parallel,
    y = seq_along(x_parallel),
    process_time = x_parallel,
    overhead_time = 0.1,
    MoreArgs = list("z" = "A"),
    chopchop = TRUE)

  testthat::expect_equal(sapply(x, function(x) (x$x)), x_parallel)
  testthat::expect_equal(sapply(x, function(x) (x$y)), seq_along(x_parallel))
  testthat::expect_setequal(sapply(x, function(x) (x$z)), "A")

  # Parallel mapply with load balancing.
  x <- familiar:::fam_mapply_lb(
    cl = cl,
    assign = NULL,
    FUN = apply_list_fun_2,
    x = x_sequential,
    y = seq_along(x_sequential))

  testthat::expect_equal(sapply(x, function(x) (x$x)), x_sequential)
  testthat::expect_equal(sapply(x, function(x) (x$y)), seq_along(x_sequential))

  # Parallel mapply with load balancing and time measurement.
  x <- familiar:::fam_mapply_lb(
    cl = cl,
    assign = NULL,
    FUN = apply_list_fun_2,
    x = x_sequential,
    y = seq_along(x_sequential),
    MEASURE.TIME = TRUE)

  testthat::expect_equal(sapply(x$results, function(x) (x$x)), x_sequential)
  testthat::expect_equal(sapply(x$results, function(x) (x$y)), seq_along(x_sequential))

  testthat::expect_equal(x$process_time, x_sequential, tolerance = 0.1)

  # Sequential mapply with NULL
  x <- familiar:::fam_mapply(
    cl = NULL,
    assign = NULL,
    FUN = apply_list_fun,
    x = NULL)

  testthat::expect_equal(x, list())

  # Parallel mapply with NULL
  x <- familiar:::fam_mapply(
    cl = cl,
    assign = NULL,
    FUN = apply_list_fun,
    x = NULL)

  testthat::expect_equal(x, list())

  # Parallel mapply with mini-batching and NULL
  x <- familiar:::fam_mapply(
    cl = cl,
    assign = NULL,
    FUN = apply_list_fun,
    x = NULL,
    chopchop = TRUE)

  testthat::expect_equal(x, list())

  # Sequential mapply with single value
  x <- familiar:::fam_mapply(
    cl = NULL,
    assign = NULL,
    FUN = apply_list_fun,
    x = x_single)

  testthat::expect_equal(unname(unlist(x)), x_single)

  # Parallel mapply with single value
  x <- familiar:::fam_mapply(
    cl = cl,
    assign = NULL,
    FUN = apply_list_fun,
    x = x_single)

  testthat::expect_equal(unname(unlist(x)), x_single)

  # Parallel mapply with mini-batching and single value.
  x <- familiar:::fam_mapply(
    cl = cl,
    assign = NULL,
    FUN = apply_list_fun,
    x = x_single,
    chopchop = TRUE)

  testthat::expect_equal(unname(unlist(x)), x_single)
})

familiar:::.terminate_cluster(cl)

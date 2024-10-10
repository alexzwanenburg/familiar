# Environment holding global variables and settings.
familiar_global_env <- new.env(parent = emptyenv())

# Prevents notes on co-routines using the coro package. The coro developers may
# be able to fix this issue at some point. See
# https://github.com/r-lib/coro/issues/40
utils::globalVariables(c(
  "generator_env",
  "exits"
))

.onLoad <- function(libname, pkgname){
  # Add an empty package environment
  assign("familiar_global_env", new.env(parent=emptyenv()), parent.env(environment()))

}

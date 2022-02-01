.start_random_number_stream <- function(seed){
  
  if(is.null(seed)) return(NULL)
  
  r <- methods::new("rstream.runif", seed=seed)
  
  return(r)
}


..fam_sample <- function(x, size, replace, seed=NULL, rstream_object=NULL){
  # Create a random generator stream to avoid touching seeds globally.
  
  if(is.null(seed) & is.null(rstream_object)){
    ..error_reached_unreachable_code("..fam_sample: seed and rstream_object cannot both be NULL")
  }
  
  if(!is.null(seed) & is.null(rstream_object)){
    rstream_object <- .start_random_number_stream(seed=seed)
  }
  
  if(!is(rstream_object, "rstream.runif")) ..error_reached_unreachable_code("..fam_sample: rstream_object is not an rstream.runif object.")
  
  # Determine the number of random values to generate. When samples are
  # replaced, "size" is sufficient. Otherwise we need to draw a number of random
  # values up to x.
  n_random_values <- ifelse(replace, size, length(x))
  
  # Obtain random values from the interval [0, 1].
  random_values <- rstream::rstream.sample(rstream_object, n_random_values)
  
  if(replace){
    # Generate sample indices.
    x_indices <- ceiling(random_values * length(x))
    
    # Replace index 0, if present.
    x_indices[x_indices == 0] <- 1
    
  } else {
    # Create sample indices by ranking indices.
    x_indices <- data.table::frank(random_values, ties.method="first")
    
    # Select the first x_indices (up to size)
    x_indices <- head(x_indices, n=size)
  }
  
  # Return sampled values.
  return(x[x_indices])
}


fam_rnorm <- function(n, mean=0.0, sd=1.0, seed=NULL, rstream_object=NULL){
  # Based on Marsaglia polar method.
  
  if(is.null(seed) & is.null(rstream_object)) return(stats::rnorm(n=n, mean=mean, sd=sd))
  
  return(.fam_rnorm(n=n, mean=mean, sd=sd, seed=seed, rstream_object=rstream_object))
}


.fam_rnorm <- function(n, mean=0.0, sd=1.0, seed=NULL, rstream_object=NULL){
  # Based on Marsaglia polar method.
  
  if(is.null(seed) & is.null(rstream_object)){
    ..error_reached_unreachable_code("..fam_rnorm: seed and rstream_object cannot both be NULL")
  }
  
  if(!is.null(seed) & is.null(rstream_object)){
    rstream_object <- .start_random_number_stream(seed=seed)
  }
  
  if(!is(rstream_object, "rstream.runif")) ..error_reached_unreachable_code("..fam_rnorm: rstream_object is not an rstream.runif object.")
  
  x <- sapply(seq_len(n), ..fam_rnorm, rstream_object=rstream_object)
  
  return(mean + sd * x)
}


..fam_rnorm <- function(ii, rstream_object){
  # Based on Marsaglia polar method.
  
  while(TRUE){
    x_1 <- rstream::rstream.sample(rstream_object, 1L) * 2.0 - 1.0
    x_2 <- rstream::rstream.sample(rstream_object, 1L) * 2.0 - 1.0
    
    s <- x_1^2 + x_2^2
    
    if(s < 1) break()
  }
  
  if(s == 0) return(0)
  
  return(x_1 * sqrt(-2.0 * log(s)/s))
}


fam_runif <- function(n, min=0.0, max=1.0, seed=NULL, rstream_object=NULL){
  
  if(is.null(seed) & is.null(rstream_object)) return(stats::runif(n=n, min=min, max=max))
  
  return(.fam_runif(n=n, min=min, max=max, seed=seed, rstream_object=rstream_object))
}


.fam_runif <- function(n, min=0.0, max=1.0, seed=NULL, rstream_object=NULL){
  
  if(is.null(seed) & is.null(rstream_object)){
    ..error_reached_unreachable_code("..fam_runif: seed and rstream_object cannot both be NULL")
  }
  
  if(!is.null(seed) & is.null(rstream_object)){
    rstream_object <- .start_random_number_stream(seed=seed)
  }
  
  if(!is(rstream_object, "rstream.runif")) ..error_reached_unreachable_code("..fam_runif: rstream_object is not an rstream.runif object.")
  
  # Create uniformly sampled data.
  x <- rstream::rstream.sample(rstream_object, n)
  
  return(min + x * (max - min))
}

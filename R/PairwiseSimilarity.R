similarity.compute_similarity <- function(x, y, x_categorical, y_categorical, similarity_metric){

  # Strip some information from similarity_metric
  similarity_metric <- gsub(x=similarity_metric, pattern="_trim", replacement="", fixed=TRUE)
  similarity_metric <- gsub(x=similarity_metric, pattern="_winsor", replacement="", fixed=TRUE)
  
  if(similarity_metric %in% c("cox_snell_r2", "nagelkerke_r2", "mcfadden_r2")){
    # Pseudo-R2 similarity measures
    similarity <- suppressWarnings(similarity.pseudo_r2(x=x,
                                                        y=y,
                                                        x_categorical=x_categorical,
                                                        y_categorical=y_categorical,
                                                        similarity_metric=similarity_metric))
    
  } else if(similarity_metric %in% c("pearson", "kendall", "spearman")){
    # Correlation-based similarity measures
    similarity <- suppressWarnings(similarity.correlation(x=x,
                                                          y=y,
                                                          x_categorical=x_categorical,
                                                          y_categorical=y_categorical,
                                                          similarity_metric=similarity_metric))
    
  } else if(similarity_metric %in% c("gower", "euclidean", "manhattan", "chebyshev", "cosine", "canberra", "bray_curtis")){
    # Distance-based similarity measures
    similarity <- suppressWarnings(similarity.distance_based(x=x,
                                                             y=y,
                                                             x_categorical=x_categorical,
                                                             y_categorical=y_categorical,
                                                             similarity_metric=similarity_metric))
    
  } else {
    ..error_reached_unreachable_code("similarity.compute_similarity: encountered unknown similarity metric")
  }
  
  return(similarity)
}



similarity.pseudo_r2 <- function(x, y, x_categorical, y_categorical, similarity_metric){


  # Check categorical flag for x
  if(length(x_categorical) != 1){
    ..error_variable_has_too_many_values(x=x_categorical, var_name="x_categorical", req_length=1)
  }
  
  # Check categorical flag for y
  if(length(y_categorical) != 1){
    ..error_variable_has_too_many_values(x=y_categorical, var_name="y_categorical", req_length=1)
  }
  
  # Find analysis type and whether x and y should be swapped in the models,
  # based on information content.
  analysis_info <- similarity.pseudo_r2.get_analysis_type(x=x,
                                                          y=y,
                                                          x_categorical=x_categorical,
                                                          y_categorical=y_categorical)
  
  # Determine model formulae
  model_formula <- stats::as.formula(ifelse(analysis_info$swap, "x ~ y", "y ~ x"))
  null_formula <- stats::as.formula(ifelse(analysis_info$swap, "x ~ 1", "y ~ 1"))
  
  # Create data.table to perform the regression
  data <- data.table::data.table("x"=x, "y"=y)
  
  # Only allow complete observations. This check should not be required in
  # practice.
  data <- data[is.finite(x) & is.finite(y)]
  
  # Check if there is sufficient data left over.
  if(nrow(data) <= 1) return(as.double(NA))
  
  # Check if there are more than one unique values in x and or y.
  if(length(unique(x)) == 1 & length(unique(y)) == 1) return(1.0)
  
  # Compute log-likelihoods so that the pseudo-R^2 measures can be computed.
  if(analysis_info$type == "gaussian"){
    # Numerical y variable
    model_obj <- stats::glm(model_formula, data=data, family=stats::gaussian)
    null_obj  <- stats::glm(null_formula, data=data, family=stats::gaussian)
    
    # Compute log-likelihoods
    model_loglik <- stats::logLik(model_obj)[1]
    null_loglik <- stats::logLik(null_obj)[1]
    
  } else if(analysis_info$type == "binomial"){
    # Categorical y variable with two levels
    model_obj <- stats::glm(model_formula, data=data, family=stats::binomial)
    null_obj  <- stats::glm(null_formula, data=data, family=stats::binomial)
    
    # Compute log-likelihoods
    model_loglik <- stats::logLik(model_obj)[1]
    null_loglik <- stats::logLik(null_obj)[1]
    
  } else if(analysis_info$type == "multinomial") {
    require_package(x="VGAM",
                    purpose=paste0("to compute log-likelihood pseudo R2 similarity using the ", similarity_metric, " metric"))
    
    # Categorical y variable with over two levels
    model_obj <- VGAM::vglm(model_formula, family=VGAM::multinomial, data=data)
    null_obj  <- VGAM::vglm(null_formula, family=VGAM::multinomial, data=data)
    
    # Compute log-likelihoods
    model_loglik <- VGAM::logLik.vlm(model_obj)[1]
    null_loglik <- VGAM::logLik.vlm(null_obj)[1]
    
  } else {
    ..error_reached_unreachable_code("similarity.pseudo_r2: encountered unknown analysis type")
  }
  
  # Compute pseudo R-squared values from log-likelihoods
  if(similarity_metric == "mcfadden_r2"){
    # Compute McFadden's R-squared
    similarity <- 1.0 -  model_loglik / null_loglik
    
  } else if(similarity_metric == "cox_snell_r2"){
    # Compute Cox and Snell's R-squared
    similarity <- 1.0 - (exp(null_loglik) / exp(model_loglik)) ^ (2.0 / length(x))
    
  } else if(similarity_metric == "nagelkerke_r2"){
    # Compute Nagelkerke's R-squared
    similarity <- (1.0 - (exp(null_loglik) / exp(model_loglik)) ^ (2.0 / length(x))) /
      (1.0 - exp(null_loglik) ^ (2.0 / length(x)))
    
  } else {
    ..error_reached_unreachable_code("similarity.pseudo_r2: encountered unknown similarity metric")
  }
  
  # Check if similarity is NaN or infinite.
  if(!is.finite(similarity)){
    similarity <- 0.0
  }
  
  # Limit to range [0,1]
  if(similarity < 0.0){
    similarity <- 0.0
    
  } else if(similarity > 1.0){
    similarity <- 1.0
  }

  return(similarity)
}



similarity.pseudo_r2.get_analysis_type <- function(x, y, x_categorical, y_categorical){
  # Determine analysis types: note that we expect the outcome to be y: hence it
  # is recommended that predictor x contains the widest range of variables, i.e.
  # continuous or multinomial variables.
  #
  # This function checks the analysis type and whether x and y should be
  # swapped in the analysis.

  # Determine number of levels of categorical features.
  n_x <- ifelse(x_categorical, nlevels(x), length(x))
  n_y <- ifelse(y_categorical, nlevels(y), length(y))
  
  if(x_categorical & y_categorical){
    # Case 1: Both x and y are categorical variables.
    
    # Determine analysis type.
    analysis_type <- ifelse(min(c(n_x, n_y)) <= 2, "binomial", "multinomial")
    
    # Determine if swapping is required.
    requires_swap <- n_x < n_y
    
  } else if(x_categorical) {
    # Case 2: x is a categorical variable, and y numerical.
    
    # Determine analysis type.
    analysis_type <- ifelse(n_x <= 2, "binomial", "multinomial")
    
    # Swapping is required.
    requires_swap <- TRUE
    
  } else if(y_categorical) {
    # Case 3: x is a numerical variable, and y categorical.
    
    # Determine analysis type.
    analysis_type <- ifelse(n_y <= 2, "binomial", "multinomial")
    
    # Swapping is not required.
    requires_swap <- FALSE
    
  } else {
    # Case 4: both x and y are numerical variables.
    analysis_type <- "gaussian"
    requires_swap <- FALSE
  }
  
  return(list("type"=analysis_type, "swap"=requires_swap))
}



similarity.correlation <- function(x, y, x_categorical, y_categorical, similarity_metric){
  
  ..dummy_encode <- function(x){
    # Dummy encoding of categorical variable.
    level_names <- levels(x)
    level_count <- nlevels(x)
    
    return(lapply(level_names[2:level_count], function(ii, x) (as.numeric(x==ii)), x=x))
  }
  
  ..encode_variable_to_list <- function(x, is_categorical){
    if(is_categorical) {
      # Categorical variables are encoded as numeric levels (ordinal), or using
      # one-hot-encoding (nominal).
      if(is.ordered(x)){
        x <- list(as.numeric(x))
        
      } else {
        x <- ..dummy_encode(x)
      }
      
    } else {
      # Numeric variables are only stored as a list.
      x <- list(x)
    }
    
    return(x)
  }
  
  # Check categorical flag for x
  if(length(x_categorical) != 1){
    ..error_variable_has_too_many_values(x=x_categorical, var_name="x_categorical", req_length=1)
  }
  
  # Check categorical flag for y
  if(length(y_categorical) != 1){
    ..error_variable_has_too_many_values(x=y_categorical, var_name="y_categorical", req_length=1)
  }
 
  # Cast x and y to lists. Categorical variables are encoded.
  x <- ..encode_variable_to_list(x=x, is_categorical=x_categorical)
  y <- ..encode_variable_to_list(x=y, is_categorical=y_categorical)
  
  # Get all list combinations; for numeric x and y this is always 1, but can be more if x and/or y are factors
  combinations <- expand.grid(ii=seq_along(x), jj=seq_along(y), KEEP.OUT.ATTRS=FALSE, stringsAsFactors=FALSE)
  
  # Calculate correlation coefficients
  correlation_coef <- sapply(seq_len(nrow(combinations)),
                             function(kk, combinations, x, y, method){
                               stats::cor(x=x[[combinations$ii[kk]]],
                                          y=y[[combinations$jj[kk]]],
                                          use="na.or.complete",
                                          method=method)
                             },
                             combinations=combinations,
                             x=x,
                             y=y,
                             method=similarity_metric)
  
  if(x_categorical | y_categorical){
    correlation_coef <- abs(correlation_coef)
  }
  
  # Compute similarity as the mean correlation coefficient.
  similarity <- mean(correlation_coef, na.rm=TRUE)
  
  # Check for NaN or infinite values.
  if(!is.finite(similarity)){
    similarity <- 0.0
  }
  
  # Return similarity
  return(similarity)
}


similarity.distance_based <- function(x, y, x_categorical, y_categorical, similarity_metric){
  
  # Check categorical feature mask for x.
  if(length(x_categorical) != length(x)){
    ..error_value_outside_allowed_range(x=x_categorical, var_name="x_categorical", range=c(length(x), length(x)))
  }
  
  # Check categorical feature mask for y. Note that x_categorical and
  # y_categorical are identical in practice.
  if(length(y_categorical) != length(y)){
    ..error_value_outside_allowed_range(x=y_categorical, var_name="y_categorical", range=c(length(y), length(y)))
  }
  
  # Handle categorical variables so that distance = 0 if they are the same, and
  # distance = 1 if they are not.
  if(any(x_categorical)){
    # Determine where values match.
    matching_values <- x[x_categorical] == y[y_categorical]
    
    # Assign the same value in x and y in case of matching categorical values.
    x[x_categorical][matching_values] <- 1.0
    y[y_categorical][matching_values] <- 1.0
    
    # Assign different values in x and y in case of mismatching categorical
    # values.
    x[x_categorical][!matching_values] <- 0.0
    y[y_categorical][!matching_values] <- 1.0
  }
  
  if(similarity_metric == "gower"){
    distance <- sum(abs(x - y)) / length(x)
    
  } else if(similarity_metric == "euclidean"){
    distance <- sqrt(sum((x - y)^2))
    
  } else if(similarity_metric == "manhattan"){
    distance <- sum(abs(x - y))
    
  } else if(similarity_metric == "chebyshev"){
    distance <- max(abs(x - y))
    
  } else if(similarity_metric == "cosine"){
    distance <- 1 - sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
    
  } else if(similarity_metric == "canberra"){
    distance <- sum(abs(x - y) / (abs(x) + abs(y)))
    
  } else if(similarity_metric == "bray_curtis"){
    distance <- sum(abs(x - y)) / sum(abs(x + y))
  }
  
  return(distance)
}


similarity.to_distance <- function(x, similarity_metric){
  # From similarity to distance. Note that some metrics represent distance (such
  # as Gower distance and Euclidean distance).
  
  if(similarity_metric %in% c("mcfadden_r2", "cox_snell_r2", "nagelkerke_r2")){
    # (pseudo) R^2 similarity metrics
    return(1-sqrt(x))
    
  } else if(similarity_metric %in% c("spearman", "kendall", "pearson")) {
    # Correlation-based metrics
    return(1-abs(x))
    
  } else {
    # Distance metrics (e.g. Gower's distance). These are natural distances.
    return(x)
  }
}



similarity.to_similarity <- function(x, similarity_metric){
  # From distance back to similarity.
  if(similarity_metric %in% c("mcfadden_r2", "cox_snell_r2", "nagelkerke_r2")){
    # (pseudo) R^2 similarity metrics.
    return((1-x)^2)
    
  } else if(similarity_metric %in% c("spearman", "kendall", "pearson")) {
    # Correlation-based metrics. Note that the sign is lost at conversion.
    return(1-abs(x))
    
  } else {
    # Distance metrics (e.g. Gower's distance). These do not generally have a
    # similarity representation, and are not converted.
    return(x)
  }
}



similarity.message_similarity_metric <- function(similarity_metric){
  
  # Strip some information from similarity_metric
  similarity_metric <- gsub(x=similarity_metric, pattern="_trim", replacement="", fixed=TRUE)
  similarity_metric <- gsub(x=similarity_metric, pattern="_winsor", replacement="", fixed=TRUE)
  
  if(similarity_metric == "mcfadden_r2"){
    return("McFadden\'s pseudo-R2")
    
  } else if(similarity_metric == "cox_snell_r2") {
    return("Cox and Snell\'s pseudo-R2")
    
  } else if(similarity_metric == "nagelkerke_r2") {
    return("Nagelkerke\'s pseudo-R2")
    
  } else if(similarity_metric == "pearson"){
    return("absolute Pearson correlation coefficient")
    
  } else if(similarity_metric == "spearman"){
    return("absolute Spearman\'s rank correlation coefficient")
    
  } else if(similarity_metric == "kendall"){
    return("absolute Kendall rank correlation coefficient")
    
  } else if(similarity_metric == "gower"){
    return("Gower\'s distance")
    
  } else if(similarity_metric == "euclidean"){
    return("Euclidean distance")
    
  } else {
    ..error_reached_unreachable_code("similarity.message_similarity_metric: encountered unknown similarity metric")
  }
}



similarity.requires_normalisation <- function(similarity_metric){
  
  # Strip some information from similarity_metric
  similarity_metric <- gsub(x=similarity_metric, pattern="_trim", replacement="", fixed=TRUE)
  similarity_metric <- gsub(x=similarity_metric, pattern="_winsor", replacement="", fixed=TRUE)
  
  if(similarity_metric %in% c("gower", "euclidean")){
    return(TRUE)
    
  } else {
    return(FALSE)
  }
}



.get_available_similarity_metrics <- function(data_type="feature"){
  if(data_type %in% c("feature", "cluster")){
    # Pair-wise comparison between features.
    return(c("mcfadden_r2", "cox_snell_r2", "nagelkerke_r2", "spearman", "kendall", "pearson"))
    
  } else {
    # Pair-wise comparison between samples.
    return(c("gower", "gower_trim", "gower_winsor",
             "euclidean", "euclidean_trim", "euclidean_winsor"))
  }
}

similarity.metric_range <- function(similarity_metric, as_distance=FALSE){
  
  # Strip some information from similarity_metric
  similarity_metric <- gsub(x=similarity_metric, pattern="_trim", replacement="", fixed=TRUE)
  similarity_metric <- gsub(x=similarity_metric, pattern="_winsor", replacement="", fixed=TRUE)
  
  if(similarity_metric %in% c("gower", "mcfadden_r2", "cox_snell_r2", "nagelkerke_r2")){
    return(c(0.0, 1.0))
    
  } else if(similarity_metric %in% c("spearman", "kendall", "pearson") & !as_distance){
    return(c(-1.0, 0.0, 1.0))
    
  } else if(similarity_metric %in% c("spearman", "kendall", "pearson") & as_distance){
    return(c(0.0, 1.0))
    
  } else {
    # For example, Euclidean distance.
    return(c(0.0, Inf))
  }
}


similarity.default_is_distance <- function(similarity_metric){
  # Returns if the metric is a distance-metric by default.
  if(similarity_metric %in% c("mcfadden_r2", "cox_snell_r2", "nagelkerke_r2", "spearman", "kendall", "pearson")){
    return(FALSE)
    
  } else {
    return(TRUE)
  }
}

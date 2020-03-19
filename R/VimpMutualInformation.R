vimp.mutual_information.learner <- function(method, outcome_type){
  return(NULL)
}



vimp.mutual_information.outcome <- function(method, outcome_type){

  if(outcome_type %in% c("binomial", "multinomial", "continuous", "count", "survival")){
    return(TRUE)
  } else {
    return(FALSE)
  }
}



vimp.mutual_information.param <- function(data_obj, method){
  return(list())
}



vimp.mutual_information.vimp <- function(data_obj, method){
  # Mutual information-based calculation
  # For continuous and survival data approximations based on spearman correlation and the concordance index are used respectively.

  if(method=="mim"){
    dt_vimp <- vimp.mutual_information.mim(data_obj=data_obj)
  } else if(method %in% c("mrmr", "mifs")){
    dt_vimp <- vimp.mutual_information.mi_multivar(data_obj=data_obj, method=method)
  }


  return(dt_vimp)

}



vimp.mutual_information.mim <- function(data_obj){

  # Internal
  outcome_type       <- data_obj@outcome_type

  # Drop missing levels from the data table
  dt                 <- droplevels(data_obj@data)

  # Find feature columns in data table
  feature_cols       <- get_feature_columns(x=data_obj)

  # Calculate mutual information
  mutual_information <- vimp.mutual_information.get_mi(dt=dt, outcome_type=outcome_type, feature_cols=feature_cols)

  # Generate variable importance data table
  dt_vimp            <- data.table::data.table("score"=mutual_information, "name"=feature_cols)

  # Update vimp data table with rank and multi_var
  dt_vimp$rank       <- data.table::frank(-(dt_vimp$score), ties.method="min")
  dt_vimp$multi_var  <- FALSE

  return(dt_vimp)
}



vimp.mutual_information.mi_multivar <- function(data_obj, method){
  # mifs: Mutual information feature selection using greedy search (Battiti 1994)
  # mrmr: Minimum Redundancy Maximum Relevance feature selection using greedy search (Peng 2005)

  # Suppress NOTES due to non-standard evaluation in data.table
  optim_score <- available <- name <- mi_redundancy <- mi <- selected <- NULL

  # Internal: set outcome_type
  outcome_type <- data_obj@outcome_type

  # Drop missing levels from the data table
  dt                     <- droplevels(data_obj@data)

  # Find feature columns in data table
  feature_cols           <- get_feature_columns(x=dt, outcome_type=outcome_type)

  # Calculate mutual information
  mutual_information     <- vimp.mutual_information.get_mi(dt=dt, outcome_type=outcome_type, feature_cols=feature_cols)

  # Setup mutual information table - select only those with finite values and positive mutual information
  dt_mi                  <- data.table::data.table("name"=feature_cols, "mi"=mutual_information, "mi_redundancy"=0, "optim_score"=mutual_information,
                                                   "selected"=FALSE, "available"=TRUE, "sel_step"=0)
  dt_mi                  <- dt_mi[!is.finite(optim_score) | optim_score<0, "available":=FALSE]
  rm(mutual_information)

  if(nrow(dt_mi)==0  | !any(dt_mi$available)){
    # If no valid mutual information is found, return an empty vimp table
    return(getEmptyVimp())
  } else if(nrow(dt_mi)==1){
    # If there is only one feature (with valid mutual information), select this feature
    dt_mi[, ":="("selected"=TRUE, "available"=FALSE, "sel_step"=1)]
  } else {

    # Select initial feature and update dt_mi
    max_score            <- max(dt_mi[available==TRUE, ]$optim_score)
    selected_feature     <- dt_mi[available==TRUE & optim_score==max_score, ]$name[1]
    dt_mi[name==selected_feature, ":="("selected"=TRUE, "available"=FALSE, "sel_step"=1)]

    for(jj in 2:nrow(dt_mi)){
      # Break if there are no more features available to compare with
      if(!any(dt_mi$available)){ break() }

      # Determine available features
      available_features <- dt_mi[available==TRUE, ]$name

      # Calculate mutual information of available features and the selected feature
      curr_redundancy_mi <- vimp.mutual_information.get_mi(dt=dt, feature_cols=available_features, selected_feature=selected_feature)

      # Update redundancy
      dt_mi[available==TRUE, "mi_redundancy":=mi_redundancy + curr_redundancy_mi]

      # Calculate optimisation score based on mutual information and redundancy
      if(method=="mifs"){

        ##### MIFS #######################################################

        # Calculate optimisation score. MIFS uses beta=1 (Battiti 1994)
        dt_mi[available==TRUE, "optim_score":= mi - mi_redundancy]

        # Only consider features with a valid, non-negative optimisation score, as redundancy is strictly increasing
        dt_mi              <- dt_mi[!is.finite(optim_score) | optim_score<0, "available":=FALSE]

        # Break if there are no more features available to compare with
        if(!any(dt_mi$available)){ break() }

        # Select the feature with maximum optimisation score and update dt_mi
        max_score          <- max(dt_mi[available==TRUE, ]$optim_score)
        selected_feature   <- dt_mi[available==TRUE & optim_score==max_score, ]$name[1]
        dt_mi[name==selected_feature, ":="("selected"=TRUE, "available"=FALSE, "sel_step"=jj)]

      } else if(method=="mrmr") {

        ##### MRMR ############################################################

        # Calculate optimisation score. MRMR uses beta=1/(jj-1), with jj-1 the size of the selected feature set
        dt_mi[available==TRUE, "optim_score":= mi - 1/(jj-1)*mi_redundancy]

        # Only consider features with a valid optimisation score
        dt_mi              <- dt_mi[!is.finite(optim_score), "available":=FALSE]

        # Break if there are no more features available to compare with, or no features with positive optimisation score
        if(!any(dt_mi$available)){ break() }
        if(!any(dt_mi[available==TRUE, ]$optim_score>=0)) { break() }

        # Select the feature with maximum optimisation score >0 and update dt_mi
        max_score          <- max(dt_mi[available==TRUE & optim_score>=0, ]$optim_score)
        selected_feature   <- dt_mi[available==TRUE & optim_score==max_score, ]$name[1]
        dt_mi[name==selected_feature, ":="("selected"=TRUE, "available"=FALSE, "sel_step"=jj)]

        # Break if there are no more features available to compare with, or no features with positive optimisation score
        if(!any(dt_mi$available)){ break() }

        # The minimum relevance criterion is not necessarily always increasing, i.e. negative optimisation scores
        # are allowed for features other than the selected feature. However, shrinkage of the feature set is feasible because
        # it is improbable that new features will add far less redundancy compared to what has already been added.
        # We therefore recalculate the optimisation score as if the feature set was 3 features larger than it was at the start of the
        # iteration (jj+2), and omit features that have a negative optimisation score.
        dt_mi[available==TRUE, "optim_score":= mi - 1/(jj+2)*mi_redundancy]
        dt_mi              <- dt_mi[!is.finite(optim_score) | optim_score<0, "available":=FALSE]
      }
    }
  }

  # Select relevant columns for variable importance data frame
  dt_vimp                <- dt_mi[selected==TRUE, c("optim_score", "name", "sel_step")]

  # Update column names to standard naming scheme
  setnames(dt_vimp, c("optim_score", "sel_step"), c("score", "rank"))

  # Add multivariate flag
  dt_vimp$multi_var      <- TRUE

  return(dt_vimp)
}



vimp.mutual_information.get_mi <- function(dt, feature_cols, selected_feature=NULL, outcome_type=NULL) {

  # Infer selected_feature from outcome_type in the case it is unset. The outcome columns are used in this case
  if(is.null(selected_feature)){
    if(outcome_type=="survival"){
      selected_feature <- c("outcome_time", "outcome_event")
    } else {
      selected_feature <- "outcome"
    }
  }

  if(is.null(outcome_type)){
    outcome_type <- "dummy"
  }

  # For survival data use the concordance index approximation
  if(outcome_type=="survival"){

    # Calculate approximate mutual information
    mi <- sapply(feature_cols, function(ii, dt, sel_feat) (vimp.mutual_information.mi_(x=dt[[ii]], y=cbind(dt[[sel_feat[1]]], dt[[sel_feat[2]]]))),
                 dt=dt, sel_feat=selected_feature)
  } else {
    mi <- sapply(feature_cols, function(ii, dt, sel_feat) (vimp.mutual_information.mi_(x=dt[[ii]], y=dt[[sel_feat]])),
                 dt=dt, sel_feat=selected_feature)
  }

  return(mi)
}



vimp.mutual_information.mi_ <- function(x, y) {
  # Main script for mutual information calculation

  # Determine if x should be a factor (character or logical)
  if(!is.factor(x)){
    if(is.character(x) | is.logical(x)){
      x <- factor(x)
    }
  }

  # Do the same for y, but include numeric data with few unique values
  if(!is.factor(y) & !is.matrix(y)){
    if(is.character(y) | is.logical(y)){
      y <- factor(y)
    } else if(is.numeric(y)) {
      if(length(unique(y))<=ceiling(sqrt(length(y)))){
        y <- factor(y)
      }
    }
  }

  # Check the number of levels in x and y, if they are factors. If there is one level only, cast to a simple numeric vector
  if(is.factor(x)){
    if(nlevels(x)==1){
      x <- vector(mode="numeric", length=length(x))
    }
  }

  # Do the same for y
  if(is.factor(y)){
    if(nlevels(y)==1){
      y <- vector(mode="numeric", length=length(y))
    }
  }

  # Determine method from x and y
  if(is.matrix(y)) {
    method <- "concordance_index"
  } else if(is.factor(y)) {
    method <- "probability"
  } else {
    method <- "spearman"
  }

  if(method %in% c("spearman", "pearson", "kendall")) {
    # Continuous y
    # MI is calculated based on linear approximation by De Jay et al. 2013; doi:10.1093/bioinformatics/btt383
    # and Gel'fand, I.M.; Yaglom, A.M. (1957). "Calculation of amount of information about a random function
    # contained in another such function". American Mathematical Society Translations: Series 2. 12: 199-246

    if(is.factor(x)){
      # Expand x by using dummy coding
      x  <- stats::model.matrix(~.-1, data.frame(x))

      # Calculate mutual information using correlation and select the maximum mutual information
      mi <- apply(x, 2, function(x_data, y_data, method) (-0.5*log(1-stats::cor(x=x_data, y=y_data, method=method)^2 + 1E-10)),
                  y_data=y, method=method)
      mi <- max(mi)
    } else {
      # Calculate mutual information using correlation
      mi <- -0.5*log(1-stats::cor(x=x, y=y, method=method)^2 + 1E-10)
    }
  } else if(method == "concordance_index") {
    # Survival outcomes (with y being a matrix)
    # MI is calculated based on linear approximation by De Jay et al. 2013; doi:10.1093/bioinformatics/btt383
    # and Gel'fand, I.M.; Yaglom, A.M. (1957). "Calculation of amount of information about a random function
    # contained in another such function". American Mathematical Society Translations: Series 2. 12: 199-246

    if(is.factor(x)){
      # Expand x by using dummy coding
      x  <- stats::model.matrix(~.-1,data.frame(x))

      # Calculate concordance indices
      ci <- apply(x, 2, function(x_data, y_data) (metric.concordance_index.cindex_(x=x_data, y=y_data)), y_data=y)

      # Calculate mutual information from the concordance indices
      mi <- -0.5*log(1-(2*(ci-0.5))^2 + 1E-10)
      mi <- max(mi)
    } else {
      ci <- metric.concordance_index.cindex_(x=x, y=y)
      mi <- -0.5*log(1-(2*(ci-0.5))^2 + 1E-10)
    }
  } else if(method == "probability") {
    # Direct, exact, calculation of mutual information for binomial and multinomial outcomes
    # Mutual information is defined as: I(X,Y) = H(X) - H(X|Y) = H(Y) - H(Y|X) = H(X) + H(Y) - H(X,Y), and we calculate entropies

    # Calculate probability distributions
    p        <- vimp.mutual_information.conditional_prob_(x=x, y=y)

    # Calculate entropies for marginal and joint distributions
    h_j      <- -sum(p$prob_j$prob_j * log2(p$prob_j$prob_j))
    h_k      <- -sum(p$prob_k$prob_k * log2(p$prob_k$prob_k))
    h_jk     <- -sum(p$prob_kj$prob_kj * log2(p$prob_kj$prob_kj))

    # Calcate mutual information
    mi       <- h_j + h_k - h_jk
  }

  # Return mutual information
  if (!is.finite(mi)){
    return(NA)
  } else {
    return(mi)
  }
}



vimp.mutual_information.conditional_prob_ <- function(x,y){
  # Suppress NOTES due to non-standard evaluation in data.table
  value <- NULL

  # Total number of training instances
  n      <- length(x)

  # Treat x as factors - bin using the Rice rule if required.
  if(!is.factor(x)){
    n_uniq <- length(unique(x))
    if(n_uniq <= ceiling(sqrt(n))){
      x <- factor(x)
    } else {
      x <- cut(x, breaks=ceiling(2*n^(1/3)))
    }
  }

  # Create data table from data
  dt <- data.table::data.table("value"=x, "class"=y)

  # p(k,j)   joint probability for the combination of class k and value j
  dt_p_kj <- dt[, list(prob_kj=.N/n), by=list(value, class)]

  # p(k)     marginal probability for class k
  dt_p_k  <- dt[, list(prob_k=.N/n), by=class]

  # p(j)     marginal probability for value j
  dt_p_j  <- dt[, list(prob_j=.N/n), by=value]

  return(list("prob_kj"=dt_p_kj, "prob_k"=dt_p_k, "prob_j"=dt_p_j))
}

#' Create randomised groups
#' Creates randomised groups, e.g. for tests that depend on splitting (continuous) data into groups, such as the Hosmer-Lemeshow test
#'
#' The default fast mode is based on random sampling, whereas the slow mode is based on probabilistic joining of adjacent groups. As
#' the name suggests, fast mode operates considerably more efficient.
#'
#' @param x Vector with data used for sorting. Groups are formed based on adjacent values.
#' @param y Vector with markers, e.g. the events. Should be 0 or 1 (for an event).
#' @param sample_id Vector with sample_ids. If provided, lists of sample_ids groups will be returned, and integers otherwise.
#' @param n_max_groups Maximum number of groups that need to be formed.
#' @param n_min_groups Minimum number of groups that need to be formed.
#' @param n_min_y_in_group Minimum number of y=1 in each group for a valid group.
#' @param n_groups_init Number of initial groups (default: 30)
#' @param fast_mode Enables fast randomised grouping mode (default: TRUE)
#'
#' @return List of group sample ids or indices.
#' @md
#' @keywords internal
create_randomised_groups <- function(x, y=NULL, sample_id=NULL, n_max_groups=NULL, n_min_groups=NULL, n_min_y_in_group=NULL, n_groups_init=30, fast_mode=TRUE){
  # Creates randomised groups, e.g. for tests that depend on splitting (continuous) data into groups, such as the Hosmer-Lemeshow test
  # - Determine maximum number of groups: either 10 or number so that each group has 5 events (if smaller).
  # - Determine minimum number of groups (half the maximum, or 2). Groups cannot the exceed corresponding group size
  # - Start with 50 very small groups.
  # - Iterate while the maximum number of groups has not been reached.
  #   - Selection probability is 1/n_j
  #   - If a group exceeds the maximum group size, selection probability is 0.
  #     - Break if all groups have exceeded the maximum size.
  #   - Get cumulative probability and normalise by total.
  #   - Draw random number between 0 and 1.
  #   - Select the group which has a cumulative probability range that contains the random number.
  #   - Draw a random number to decide whether to join the group with right or left adjacent group, and assign
  #     the group number to the adjacent group. Probability depends on the size of adjacent groups. Smaller sizes
  #     have greater probability of being joined. No joining with groups already exceeding the maximum group size.
  #     If surrounded on both sides, force selection probability for current group to 0. If joining is possible,
  #     update group size, and selection probability for the new group.
  # - Check that 5 events are present in each group. For each group with < 5 events, try to join with neighbours.
  # - Start over if the number of groups is smaller than the minimum number.
  #
  #

  # Suppress NOTES due to non-standard evaluation in data.table
  group_id <- cum_y <- weight <- cum_prob_lower <- cum_prob_upper <- exclude <- y_in_group <- NULL

  ##### Initial parsing #####
  # - Get number of x
  n_x <- length(x)

  # - Get number of y
  if(is.null(y)){
    y <- rep(0, n_x)
  }
  n_y <- sum(y)

  # - Get the maximum of groups
  if(is.null(n_max_groups)){
    n_max_groups <- ceiling(2.5 * n_x^(1/3))
  }

  # - Update maximum number of groups
  if(n_y > 0 & !is.null(n_min_y_in_group)){
    n_max_groups <- min(c(n_max_groups, floor(n_y / n_min_y_in_group)))
  }

  # - Update mininum number of groups based on n_max_groups
  if(is.null(n_min_groups)){
    n_min_groups <- min(c(n_max_groups, ceiling(1.0 * n_x^(1/3))))
  }

  # - Generate sample_id if not provided
  if(is.null(sample_id)){
    sample_id <- seq_len(n_x)
  }

  # Some checks
  if(n_max_groups < n_min_groups | n_max_groups > n_x | n_max_groups < 2){
    return(NULL)
  }

  if(fast_mode){

    ###############################################################
    # Fast group generation mode
    ##############################################################

    # Create table
    dt_agg <- data.table::data.table("sample_id"=sample_id, "x"=x, "y"=y)[order(x)]

    # Initial loop counter
    loop_iter <- 0
    
    while(TRUE){
      # Draw a random number of groups between n_min_groups and n_max_groups
      if(n_min_groups == n_max_groups){
        n_group_draw <- n_max_groups
      } else {
        n_group_draw <- sample(x=seq.int(from=n_min_groups, to=n_max_groups, by=1L), size=1, replace=FALSE)
      }

      # Draw a randomised groups assignment
      random_group_id <- sort(sample(seq_len(n_group_draw), size=n_x, replace=TRUE))

      # Assign group id
      dt_agg[, "group_id":=random_group_id]

      # Check if all groups have the minimum required y (e.g. events/group size)
      if(n_y > 0 & !is.null(n_min_y_in_group)){
        dt_y <- dt_agg[, list(y_in_group=sum(y)), by=group_id]
        if(all(dt_y$y_in_group >= n_min_y_in_group)){
          break
        }
      } else {
        # If there is no minimum number of events required, only run one iteration
        break
      }

      # Update loop_iter in case the sample was not good enough
      loop_iter <- loop_iter + 1

      # Break from outer loop after 10 unsuccessful random samplings and create a static split instead
      if(loop_iter >= 10){
        # Cumulative sum over y
        dt_agg[, "cum_y":=cumsum(y)]

        # Assign static group ids
        dt_agg[, "group_id":=findInterval(x=cum_y, vec=c(-Inf, stats::quantile(x=cum_y, (1:n_min_groups-1)/n_min_groups), Inf))]

        # Break from loop
        break
      }
    }

  } else {

    ###############################################################
    # Slow group generation mode
    ##############################################################

    # - Update maximum number of groups based on n_min_groups
    if(!is.null(n_min_groups)){
      if(n_max_groups < n_min_groups + 2){
        n_max_groups <- n_min_groups + 2
      }
    }

    # - Update number of initial groups so that each group size is one or more
    if(n_groups_init > n_x){
      n_groups_init <- n_x
    }

    # - Maximum group size, which corresponds to n_min_groups
    max_group_size <- ceiling(n_x / n_min_groups)

    ##### Setup #####
    # Create table
    dt <- data.table::data.table("sample_id"=sample_id, "x"=x, "y"=y)[order(x)]

    # Create initial groups
    init_group_size <- ceiling(n_x/n_groups_init)

    # Create oversampled groups
    group_size <- rep(init_group_size, n_groups_init)

    # Randomly reduce group sizes by 1
    n_samples_subtract <- init_group_size * n_groups_init - n_x
    if(n_samples_subtract > 0){
      group_ind <- sample(x=seq_len(n_groups_init), size=n_samples_subtract, replace=FALSE)
      group_size[group_ind] <- group_size[group_ind] - 1
      rm("group_ind")
    }

    # Add group_id to table
    dt[, "group_id":=rep(seq_len(n_groups_init), times=group_size)]
    rm("init_group_size", "n_samples_subtract", "group_size", "n_groups_init")

    loop_iter <- 0

    # Outer loop that checks whether the final number of groups is not smaller than n_min_groups
    while(TRUE){

      dt_agg <- data.table::copy(dt)
      n_groups <- data.table::uniqueN(dt_agg, by="group_id")

      # Get a summary table
      # - group size is the number of samples in the group
      # - weight is the unnormalised selection probability
      # - exclude is a flag to set weights to 0 for groups that would otherwise grow too large
      dt_s <- dt_agg[, list(group_size=.N, exclude=FALSE, weight=1/.N), by=group_id]

      # Determine the total weight for normalizing weights into probabilities
      total_weight <- sum(dt_s$weight)
      dt_s[, "cum_prob_upper":=cumsum(weight)/(total_weight)]
      dt_s[, "cum_prob_lower":=c(0, dt_s$cum_prob_upper[1:length(dt_s$cum_prob_upper)-1])]

      # Inner loop that agglomerates groups
      while(n_groups > n_max_groups){
        # Draw two numbers uniformly from [0,1]
        r_prob <- stats::runif(n=2)

        # Get the group id that contains r_prob[1]
        sel_group_id <- dt_s[data.table::between(r_prob[1], lower=cum_prob_lower, upper=cum_prob_upper)]$group_id[1]

        # Get neighbouring group_ids
        group_left  <- tail(dt_s[group_id < sel_group_id], n=1)
        group_right <- head(dt_s[group_id > sel_group_id], n=1)

        # Get join probabilities
        if(nrow(group_left)==0){ left_prob <- 0 }
        else {                   left_prob <- group_left$weight }
        if(nrow(group_right)==0){ right_prob <- 0 }
        else {                    right_prob <- group_right$weight }
        cum_join_prob <- left_prob + right_prob

        # Only join if the combined joining probability is not 0
        if(cum_join_prob > 0){
          # Normalise probability
          left_prob <- left_prob / cum_join_prob

          # Determine join
          if(r_prob[2] < left_prob){
            join_id <- group_left$group_id
          } else {
            join_id <- group_right$group_id
          }

          # Apply join
          dt_s[group_id==join_id, "group_id":=sel_group_id]
          dt_agg[group_id==join_id, "group_id":=sel_group_id]
          rm("join_id")
        } else {
          # Force exclusion of the current group
          dt_s[group_id==sel_group_id, "exclude":=TRUE]
        }
        rm("r_prob", "sel_group_id", "group_left", "group_right", "left_prob", "right_prob", "cum_join_prob")

        # Update summary table by determining new group sizes. Group sizes are summed for entries with the same
        # group_id (i.e. those just joined), whereas an OR operation is applied to exclude.
        dt_s <- dt_s[, list(group_size=sum(group_size), exclude=as.logical(max(exclude))), by=group_id]

        # Exclude groups that exceed the max_group_size in size
        dt_s[group_size >= max_group_size, "exclude":=TRUE]

        # Set weights
        dt_s[, "weight":=1/group_size]
        dt_s[exclude==TRUE, "weight":=0]

        # Get total weight for normalisation
        total_weight <- sum(dt_s$weight)

        # Break from inner loop if there are no more weights to set
        if(total_weight == 0){
          dt_s[, "cum_prob_upper":=0]
          dt_s[, "cum_prob_lower":=0]

          break
        }

        # Set probabilities
        dt_s[, "cum_prob_upper":=cumsum(weight)/(total_weight)]
        dt_s[, "cum_prob_lower":=c(0, dt_s$cum_prob_upper[1:length(dt_s$cum_prob_upper)-1])]

        # Finally, determine n_groups
        n_groups <- data.table::uniqueN(dt_s, by="group_id")
      }
      rm("total_weight")

      # Check if all groups have the minimum required y (e.g. events/group size)
      if(n_y > 0 & !is.null(n_min_y_in_group)){
        # Count y in each group
        dt_y <- dt_agg[, list(y_in_group=sum(y)), by=group_id]
        dt_y <- merge(dt_y, dt_s, by="group_id")[order(group_id)]

        # Inner loop to resolve all y
        dt_y[, "exclude":=FALSE]
        dt_y[y_in_group >= n_min_y_in_group, "exclude":=TRUE]

        # Combine data until every group is valid.
        while(!all(dt_y$exclude)){

          # Find the smallest group, and add this to a neighbour
          sel_group_id <- dt_y[exclude==FALSE][group_size==min(dt_y$group_size)]$group_id[1]

          # Get neighbouring group_ids
          group_left  <- tail(dt_y[group_id < sel_group_id], n=1)
          group_right <- head(dt_y[group_id > sel_group_id], n=1)

          # Preferentially add to a neighbour without sufficient y, but where addition would allow the combined group to exist
          # Break if there is just one group.
          if(nrow(group_left)==0 & nrow(group_right)==0){ break }

          if(nrow(group_left)==0){ join <- "right" }
          else if(nrow(group_right)==0){ join <- "left" }
          else {
            # This requires some heuristics. General principle is to combine the smallest groups,
            # unless a new group with at least n_min_y_in_group can be produced
            n_y_left <- group_left$y_in_group
            n_y_right <- group_right$y_in_group
            n_y_sel <- dt_y[group_id==sel_group_id]$y_in_group

            c_y_left  <- n_y_left + n_y_sel
            c_y_right <- n_y_right + n_y_sel
            if(c_y_left >= n_min_y_in_group & c_y_right >= n_min_y_in_group){
              # All joins would produce good groups: choose the combined smallest
              if(c_y_left > c_y_right){ join <- "right" }
              else                    { join <- "left" }
            } else if(c_y_left >= n_min_y_in_group & n_y_left < n_min_y_in_group){
              # Left join would produce a new good group: choose left
              join <- "left"
            } else if(c_y_right >= n_min_y_in_group & n_y_right < n_min_y_in_group){
              # Right join would produce a new good group: choose right
              join <- "right"
            } else if(n_y_left < n_min_y_in_group & n_y_right >= n_min_y_in_group){
              # Add to the smaller left group, as the right group is already fine.
              join <- "left"
            } else if(n_y_right < n_min_y_in_group & n_y_left >= n_min_y_in_group){
              # Add to the smaller right group, as the left group is already fine.
              join <- "right"
            } else if(n_y_left < n_y_right){
              # Add to the smaller left group
              join <- "left"
            } else if(n_y_left > n_y_right){
              # Add to the smaller right group
              join <- "right"
            } else {
              # It's a toss-up
              join <- sample(c("left", "right"), size=1)
            }
            rm("n_y_left", "n_y_right", "n_y_sel", "c_y_left", "c_y_right")
          }

          # Get join id
          if(join=="left"){ join_id <- group_left$group_id }
          else            { join_id <- group_right$group_id }
          rm("group_left", "group_right")

          # Apply join
          dt_y[group_id==join_id, "group_id":=sel_group_id]
          dt_agg[group_id==join_id, "group_id":=sel_group_id]
          rm("join_id", "sel_group_id")

          # Update table
          dt_y <- dt_y[, list(group_size=sum(group_size), y_in_group=sum(y_in_group), exclude=as.logical(max(exclude))), by=group_id]
          dt_y[y_in_group >= n_min_y_in_group, "exclude":=TRUE]
        }

        rm("dt_y")
      }

      # Determine n_groups
      n_groups <- data.table::uniqueN(dt_agg, by="group_id")

      # Break from outer loop
      if(n_groups >= n_min_groups) { break }

      # Update loop_iter in case efforts were not successful
      loop_iter <- loop_iter + 1

      # Break from outer loop after 5 unsuccessful iterations and create a static split instead
      if(loop_iter >= 5){
        if(n_y > 0 & !is.null(n_min_y_in_group)){
          # Cumulative sum over y
          dt_agg[, "cum_y":=cumsum(y)]

          # Assign static group ids
          dt_agg[, "group_id":=findInterval(x=cum_y, vec=c(-Inf, stats::quantile(x=cum_y, (1:n_min_groups-1)/n_min_groups), Inf))]
        } else {

          # Assign static group ids
          dt_agg[, "group_id":=findInterval(x=seq_len(n_x), vec=c(-Inf, stats::quantile(x=seq_len(n_x), (1:n_min_groups-1)/n_min_groups), Inf))]
        }

        # Break from loop
        break
      }
    }
  }

  # Get sample ids for each group
  out_groups <- lapply(unique(dt_agg$group_id), function(sel_group_id, dt) (dt[group_id==sel_group_id]$sample_id), dt=dt_agg)

  return(out_groups)
}

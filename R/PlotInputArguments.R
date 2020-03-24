#' Internal checks on common plot input arguments
#'
#' @param x_range (*optional*) Value range for the x-axis.
#' @param y_range (*optional*) Value range for the y-axis.
#' @param x_n_breaks (*optional*) Number of breaks to show on the x-axis of the
#'   plot. `x_n_breaks` is used to determine the `x_breaks` argument in case it
#'   is unset.
#' @param y_n_breaks (*optional*) Number of breaks to show on the y-axis of the
#'   plot. `y_n_breaks` is used to determine the `y_breaks` argument in case it
#'   is unset.
#' @param x_breaks (*optional*) Break points on the x-axis of the plot.
#' @param y_breaks (*optional*) Break points on the y-axis of the plot.
#' @param conf_int (*optional*)
#' @param conf_int_alpha (*optional*) Alpha value to determine transparancy of
#'   `ribbon`-style confidence intervals in a plot. Only values between 0.0
#'   (fully transparent) and 1.0 (fully opaque) are allowed.
#' @param conf_int_style (*optional*)
#' @param facet_wrap_cols (*optional*) Number of columns to generate when facet
#'   wrapping. If NULL, a facet grid is produced instead.
#' @param x_label (*optional*) Label to provide to the x-axis. If NULL, no label
#'   is shown.
#' @param y_label (*optional*) Label to provide to the y-axis. If NULL, no label
#'   is shown.
#' @param legend_label (*optional*) Label to provide to the legend. If NULL, the
#'   legend will not have a name.
#' @param combine_legend (*optional*) Flag to indicate whether the same legend
#'   is to be shared by multiple aesthetics, such as those specified by
#'   `color_by` and `linetype_by` arguments.
#' @param plot_title (*optional*) Label to provide as figure title. If NULL, no
#'   title is shown.
#' @param plot_sub_title (*optional*) Label to provide as figure subtitle. If
#'   NULL, no subtitle is shown.
#' @param caption (*optional*) Label to provide as figure caption. If NULL, no
#'   caption is shown.
#' @param x_label_shared (*optional*) Sharing of x-axis labels between facets.
#'   One of three values:
#'
#'   * `overall`: A single label is placed at the bottom of the figure. Tick
#'   text (but not the ticks themselves) is removed for all but the bottom facet
#'   plot(s).
#'
#'   * `column`: A label is placed at the bottom of each column. Tick text (but
#'   not the ticks themselves) is removed for all but the bottom facet plot(s).
#'
#'   * `individual`: A label is placed below each facet plot. Tick text is kept.
#'
#' @param y_label_shared (*optional*) Sharing of y-axis labels between facets.
#'   One of three values:
#'
#'   * `overall`: A single label is placed to the left of the figure. Tick text
#'   (but not the ticks themselves) is removed for all but the left-most facet
#'   plot(s).
#'
#'   * `row`: A label is placed to the left of each row. Tick text (but not the
#'   ticks themselves) is removed for all but the left-most facet plot(s).
#'
#'   * `individual`: A label is placed below each facet plot. Tick text is kept.
#'
#' @param rotate_x_tick_labels (*optional*) Rotate tick labels on the x-axis by
#'   90 degrees. Defaults to `TRUE`. Rotation of x-axis tick labels may also be
#'   controlled through the `ggtheme`. In this case, `FALSE` should be provided
#'   explicitly.
#'
#' @param rotate_y_tick_labels (*optional*) Rotate tick labels on the y-axis by
#'   45 degrees.
#' @md
#' @keywords internal
plotting.check_input_args <- function(x_range=waiver(), y_range=waiver(), x_n_breaks=waiver(), y_n_breaks=waiver(),
                                      x_breaks=waiver(), y_breaks=waiver(), conf_int=waiver(),
                                      conf_int_alpha=waiver(), conf_int_style=waiver(), facet_wrap_cols=waiver(),
                                      x_label=waiver(), y_label=waiver(),
                                      x_label_shared=waiver(), y_label_shared=waiver(),
                                      rotate_x_tick_labels=waiver(), rotate_y_tick_labels=waiver(),
                                      legend_label=waiver(), combine_legend=waiver(), plot_title=waiver(),
                                      plot_sub_title=waiver(), caption=waiver()){
  
  # x and y ranges
  if(!is.waive(x_range)){ plotting.check_input_range(range_var=x_range, var_name="x_range") }
  if(!is.waive(y_range)){ plotting.check_input_range(range_var=y_range, var_name="y_range") }
  
  # x and y number of breaks
  if(!is.waive(x_n_breaks)){ plotting.check_input_n_breaks(n_break_var=x_n_breaks, var_name="x_n_breaks") }
  if(!is.waive(y_n_breaks)){ plotting.check_input_n_breaks(n_break_var=y_n_breaks, var_name="y_n_breaks") }
  
  # x and y breaks
  if(!is.waive(x_breaks)){ plotting.check_input_break(break_var=x_breaks, var_name="x_breaks") }
  if(!is.waive(y_breaks)){ plotting.check_input_break(break_var=y_breaks, var_name="y_breaks") }
  
  # labels
  if(!is.waive(x_label)){ plotting.check_input_label(label_var=x_label, var_name="x_label") }
  if(!is.waive(y_label)){ plotting.check_input_label(label_var=y_label, var_name="y_label") }
  if(!is.waive(legend_label)){ plotting.check_input_legend(label_var=legend_label, var_name="legend_label") }
  if(!is.waive(plot_title)){ plotting.check_input_label(label_var=plot_title, var_name="plot_title") }
  if(!is.waive(plot_sub_title)){ plotting.check_input_label(label_var=plot_sub_title, var_name="plot_sub_title") }
  if(!is.waive(caption)){ plotting.check_input_label(label_var=caption, var_name="caption") }

  # Legend combination flag
  if(!is.waive(combine_legend)) {
    .check_parameter_value_is_valid(x=combine_legend, var_name="combine_legend", values=c(FALSE, TRUE))
  }
  
  # Transparency values
  if(!is.waive(conf_int_alpha)){
    .check_number_in_valid_range(x=conf_int_alpha, var_name="conf_int", range=c(0, 1), closed=c(TRUE, TRUE))
  }
  
  # Confidence intervals. Note that a confidence interval of 0.0 (equals FALSE) is allowed.
  if(!is.waive(conf_int)){
    .check_number_in_valid_range(x=conf_int, var_name="conf_int", range=c(0, 1), closed=c(TRUE, FALSE))
  }
  
  # Facet wrap cols
  if(!is.waive(facet_wrap_cols) & !is.null(facet_wrap_cols)){
    .check_number_in_valid_range(x=facet_wrap_cols, var_name="facet_wrap_cols", range=c(1, Inf), closed=c(TRUE, TRUE))
  }
  
  # Style of confidence intervals
  if(!is.waive(conf_int_style)){
    .check_parameter_value_is_valid(x=conf_int_style, var_name="conf_int_style", values=c("step", "ribbon", "none"))
  }
  
  # Sharing of x-axis labels
  if(!is.waive(x_label_shared)){
    .check_parameter_value_is_valid(x=x_label_shared, var_name="x_label_shared", values=c("overall", "column", "individual"))
  }
  
  # Sharing of y-axis labels
  if(!is.waive(y_label_shared)){
    .check_parameter_value_is_valid(x=y_label_shared, var_name="y_label_shared", values=c("overall", "row", "individual"))
  }
  
  # Rotation of tick labels on the x-axis by 90 degrees
  if(!is.waive(rotate_x_tick_labels)){
    .check_parameter_value_is_valid(x=rotate_x_tick_labels, var_name="rotate_x_tick_labels", values=c(FALSE, TRUE))
  }
  
  # Rotation of tick labels on the y-axis by 45 degrees
  if(!is.waive(rotate_y_tick_labels)){
    .check_parameter_value_is_valid(x=rotate_y_tick_labels, var_name="rotate_y_tick_labels", values=c(FALSE, TRUE))
  }
  
}


plotting.check_input_range <- function(range_var, var_name){
  # Generic range check
  if(!is.numeric(range_var)){
    stop(paste("The", var_name, "argument should be a numerical vector of length 2, indicating min and max of the range.",
               "Use NA to indicate an unset upper or lower boundary.", sep=" "))
    
  } else if(length(range_var) !=2){
    stop(paste("The", var_name, "argument should be a numerical vector of length 2, indicating min and max of the range.",
               "Use NA to indicate an unset upper or lower boundary.", sep=" "))
    
  } else if(any(is.infinite(range_var))){
    stop(paste("The range provided by the", var_name, "argument cannot have Inf or -Inf values.",
               "Use NA to indicate an unset upper or lower boundary.", sep=" "))
    
  } else if(!any(is.na(range_var))){
    if(range_var[1] == range_var[2]){
      stop(paste("The range provided by the", var_name,
                 "argument cannot have the same value for the lower and upper boundary.", sep=" "))
    } else if(range_var[1] > range_var[2]){
      stop(paste("The range provided by the", var_name,
                 "argument cannot have a lower boundary with a higher value than the upper boundary.", sep=" "))
    }
  }
}


plotting.check_input_n_breaks <- function(n_break_var, var_name){
  # Generic number of breaks check
  if(!is.numeric(n_break_var)){
    stop(paste("The", var_name, "argument should be a single integer value of 2 or larger."), sep=" ")
    
  } else if(length(n_break_var) !=1){
    stop(paste("The", var_name, "argument should be a single integer value of 2 or larger."), sep=" ")
    
  } else if(!is.finite(n_break_var)){
    stop(paste("The", var_name, "argument should be a single integer value of 2 or larger. It cannot be infinite or NA."), sep=" ")
    
  } else if(n_break_var < 2.0){
    stop(paste("The", var_name, "argument should be a single integer value of 2 or larger."), sep=" ")
    
  }
}


plotting.check_input_break <- function(break_var, var_name){
  # Generic break check
  if(is.null(break_var)){ return() }
  
  if(is.numeric(break_var) & !all(is.finite(break_var))){
    stop(paste("The", var_name, "argument contains NA or infite values.", sep=" "))
    
  } else if(any(is.na(break_var))){
    stop(paste("The", var_name, "argument contains NA or infite values.", sep=" "))
    
  } else if(any(duplicated(break_var))){
    stop(paste("Some breaks in the", var_name, "argument are duplicates.", sep=" "))
    
  }
}


plotting.check_input_label <- function(label_var, var_name){
  # Generic label check
  if(is.null(label_var) | is.call(label_var) | is.expression(label_var)) { return() }
  
  if(!(is.numeric(label_var) | is.character(label_var))){
    stop(paste("The", var_name, "argument should be NULL, a single number, a single string,",
               "or a call or expression interpreted by grDevices::plotmath.", sep=" "))
    
  } else if(length(label_var) != 1){
    stop(paste("The", var_name, "argument contains multiple labels, whereas a single label is expected.", sep=" "))
    
  }
}


plotting.check_input_legend <- function(label_var, var_name){
  # Generic label check
  if(is.null(label_var) | is.call(label_var) | is.expression(label_var)) { return() }
  
  # Legend labels can be a list
  if(is.list(label_var)) {
    sapply(label_var, plotting.check_input_label, var_name=var_name)
    
    return()
  }
  
  if(!(is.numeric(label_var) | is.character(label_var))){
    stop(paste("The", var_name, "argument should be NULL, a single number, a single string,",
               "or a call or expression interpreted by grDevices::plotmath.", sep=" "))
    
  } else if(length(label_var) != 1){
    stop(paste("The", var_name, "argument contains multiple labels, whereas a single label is expected.", sep=" "))
    
  }
  
}


plotting.check_grid_unit <- function(x, var_name){
  if(!grid::is.unit(x)){
    stop(paste("The", var_name, "argument should be a grid unit (see ?grid::unit), but instead is", class(x), sep=" "))
  }
}

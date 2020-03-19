
plotting.get_theme <- function(use_theme=NULL){

  if(is.null(use_theme) & is_package_installed("cowplot")){
    # Get cowplot theme.
    ggtheme <- cowplot::theme_cowplot(font_size=9)
    
    # Update theme to reduce plot margins
    ggtheme$plot.margin <- grid::unit(c(1.0, 1.0, 1.0, 1.0), "pt")
    
    return(ggtheme)
    
  } else {
    # Get ggplot light theme
    ggtheme <- ggplot2::theme_light(base_size=9)
    
    # Update theme to reduce plot margins
    ggtheme$plot.margin <- grid::unit(c(1.0, 1.0, 1.0, 1.0), "pt")
    
    return(ggtheme)
  }
  
  if(use_theme == "cowplot" & is_package_installed("cowplot")){
    return(cowplot::theme_cowplot(font_size=9))
  }
  
  if(use_theme %in% c("theme_grey", "theme_gray")){
    return(ggplot2::theme_grey(base_size=9))
  }
  
  if(use_theme == "theme_bw"){
    return(ggplot2::theme_bw(base_size=9))
  }
  
  if(use_theme == "theme_linedraw"){
    return(ggplot2::theme_linedraw(base_size=9))
  }
  
  if(use_theme == "theme_light"){
    return(ggplot2::theme_light(base_size=9))
  }
  
  if(use_theme == "theme_dark"){
    return(ggplot2::theme_dark(base_size=9))
  }
  
  if(use_theme == "theme_minimal"){
    return(ggplot2::theme_minimal(base_size=9))
  }
  
  if(use_theme == "theme_classic"){
    return(ggplot2::theme_classic(base_size=9))
  }
  
  stop("The requested theme could not be found.")
}


#' Checks and sanitizes spliting variables for plotting.
#'
#' @param x data.table or data.frame containing the data used for splitting.
#' @param split_by (*optional*) Splitting variables. This refers to column names
#'   on which datasets are split. A separate figure is created for each split.
#'   See details for available variables.
#' @param color_by (*optional*) Variables used to determine fill colour of plot
#'   objects. The variables cannot overlap with those provided to the `split_by`
#'   argument, but may overlap with other arguments. See details for available
#'   variables.
#' @param linetype_by (*optional*) Variables that are used to determine the
#'   linetype of lines in a plot. The variables cannot overlap with those
#'   provided to the `split_by` argument, but may overlap with other arguments.
#'   Sett details for available variables.
#' @param facet_by (*optional*) Variables used to determine how and if facets of
#'   each figure appear. In case the `facet_wrap_cols` argument is `NULL`, the
#'   first variable is used to define columns, and the remaing variables are
#'   used to define rows of facets. The variables cannot overlap with those
#'   provided to the `split_by` argument, but may overlap with other arguments.
#'   See details for available variables.
#' @param x_axis_by (*optional*) Variable plotted along the x-axis of a plot.
#'   The variable cannot overlap with variables provided to the `split_by` and
#'   `y_axis_by` arguments (if used), but may overlap with other arguments. Only
#'   one variable is allowed for this argument. See details for available
#'   variables.
#' @param y_axis_by (*optional*) Variable plotted along the y-axis of a plot.
#'   The variable cannot overlap with variables provided to the `split_by` and
#'   `x_axis_by` arguments (if used), but may overlap with other arguments. Only
#'   one variable is allowed for this argument. See details for available
#'   variables.
#' @param available Names of columns available for splitting.
#'
#' @details This internal function allows some flexibility regarding the exact
#'   input. Allowed splitting variables should be defined by the available
#'   argument.
#'
#' @return A sanitized list of splitting variables.
#' @md
#' @keywords internal
plotting.check_data_handling <- function(x, split_by=NULL, color_by=NULL, linetype_by=NULL, facet_by=NULL, x_axis_by=NULL, y_axis_by=NULL, available=NULL){

  # Find unique variables
  splitting_vars <- c(split_by, color_by, linetype_by, facet_by, x_axis_by, y_axis_by)
  
  if(is.null(available) & length(splitting_vars) == 0){
    return(list())
    
  } else if(is.null(available) & length(splitting_vars) > 0){
    stop(paste0("The current plot has no required splitting variables defined, but ",
                paste0(splitting_vars, collapse=", "),
                ifelse(length(splitting_vars) == 1, " was assigned.", " were assigned.")))
    
  }

  # Filter available down to those that have more than one variable
  filter_available <- available[sapply(available, function(ii, x) (data.table::uniqueN(x=x, by=ii) > 1), x=x)]
  
  if(is.null(filter_available)){
    return(list())
    
  } else if(!all(filter_available %in% splitting_vars)){
    missing_vars <- filter_available[!filter_available %in% splitting_vars]
    stop(paste0("The current plot requires ",
                paste0(filter_available, collapse=", "),
                " as splitting variables, but ",
                paste0(missing_vars, collapse=", "),
                ifelse(length(missing_vars) == 1, " was not assigned.", " were not assigned.")))
  }
  
  # Update available
  available <- filter_available
  
  # Generate output
  output_list <- list()
  
  # Update split_by
  if(!is.null(split_by) & any(split_by %in% available)){
    output_list$split_by <- intersect(split_by, available)
  }
  
  # Update color_by
  if(!is.null(color_by) & any(color_by %in% available)){
    output_list$color_by <- intersect(color_by, available)
  }

  # update linetype_by
  if(!is.null(linetype_by) & any(linetype_by %in% available)){
    output_list$linetype_by <- intersect(linetype_by, available)
  }
  
  # update facet_by
  if(!is.null(facet_by) & any(facet_by %in% available)){
    output_list$facet_by <- intersect(facet_by, available)
  }

  # update x_axis_by
  if(!is.null(x_axis_by) & any(x_axis_by %in% available)){
    output_list$x_axis_by <- intersect(x_axis_by, available)
  }
  
  # update y_axis_by
  if(!is.null(y_axis_by) & any(y_axis_by %in% available)){
    output_list$y_axis_by <- intersect(y_axis_by, available)
  }
  
  # Check split_by variable.
  .check_value_not_shared(output_list$split_by, output_list$color_by, "split_by", "color_by")
  .check_value_not_shared(output_list$split_by, output_list$linetype_by, "split_by", "linetype_by")
  .check_value_not_shared(output_list$split_by, output_list$facet_by, "split_by", "facet_by")
  .check_value_not_shared(output_list$split_by, output_list$x_axis_by, "split_by", "x_axis_by")
  .check_value_not_shared(output_list$split_by, output_list$y_axis_by, "split_by", "y_axis_by")
  
  # Check x_axis_by variable and y_axis_by variables.
  .check_value_not_shared(output_list$x_axis_by, output_list$y_axis_by, "x_axis_by", "y_axis_by")
  
  # Check length of x_axis_by and y_axis_by variables.
  .check_argument_length(output_list$x_axis_by, "x_axis_by", min=0, max=1)
  .check_argument_length(output_list$y_axis_by, "y_axis_by", min=0, max=1)
  
  return(output_list)
}


plotting.parse_color_by <- function(x, color_by){
  if(is.null(color_by)){
    return(list("data"=x))
  } else if(length(color_by)==1){
    return(list("data"=x, "color_by"=color_by))
  } else {
    # Expand grid
    y <- data.table::data.table(expand.grid(lapply(color_by, function(ii, x) (levels(x[[ii]])), x=x)))
    
    # Set names
    data.table::setnames(y, color_by)
    
    # Create new color_by_col column from y
    y[, "color_by_col":=apply(y, 1, paste, collapse=", ")]
    
    # Determine levels
    y$color_by_col <- factor(y$color_by_col, levels=y$color_by_col)
    
    # Merge with x
    x <- merge(x=x, y=y, by=color_by, all.x=TRUE, all.y=FALSE)
    
    return(list("data"=x, "color_by"="color_by_col"))
    
  }
}


plotting.parse_linetype_by <- function(x, linetype_by){
  if(is.null(linetype_by)){
    return(list("data"=x))
  } else if(length(linetype_by)==1){
    return(list("data"=x, "linetype_by"=linetype_by))
  } else {
    # Expand grid
    y <- data.table::data.table(expand.grid(lapply(linetype_by, function(ii, x) (levels(x[[ii]])), x=x)))
    
    # Set names
    data.table::setnames(y, linetype_by)
    
    # Create new color_by_col column from y
    y[, "linetype_by_col":=apply(y, 1, paste, collapse=", ")]
    
    # Determine levels
    y$linetype_by_col <- factor(y$linetype_by_col, levels=y$linetype_by_col)
    
    # Merge with x
    x <- merge(x=x, y=y, by=linetype_by, all.x=TRUE, all.y=FALSE)
    
    return(list("data"=x, "linetype_by"="linetype_by_col"))
    
  }
}


plotting.parse_facet_by <- function(x, facet_by, facet_wrap_cols){
  if(is.null(facet_by)){
    return(list())
    
  } else if(length(facet_by) == 1){
    if(is.null(facet_wrap_cols)){
      return(list("facet_cols"=quos(!!ensym(facet_by))))
    } else {
      return(list("facet_by"=quos(!!ensym(facet_by))))
    }
    
  } else {
    if(is.null(facet_wrap_cols)){
      facet_col <- facet_by[1]
      facet_rows <- facet_by[2:length(facet_by)]
      return(list("facet_cols"=quos(!!ensym(facet_col)),
                  "facet_rows"=quos(!!!parse_exprs(facet_rows))))
    } else {
      return(list("facet_by"=quos(!!!parse_exprs(facet_by))))
    }
  }
}


plotting.add_cluster_name <- function(x, color_by=NULL, facet_by=NULL, singular_cluster_character="\u2014"){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  cluster_size <- cluster_id <- name <- new_cluster_id <- cluster_name <- NULL
  
  ..integer_to_char <- function(x){
    # Initialise placeholders
    x_remain <- x
    new_string <- character(0)
    
    while(ceiling(x_remain/26) > 0){
      
      # Determine the modulo.
      mod <- x_remain %% 26
      
      # Find if mod is equal to 0, which would indicate Z.
      mod <- ifelse(mod==0, 26, mod)
      
      # Add letter
      new_string <- c(new_string, LETTERS[mod])
      
      # Update the remain variable
      x_remain <- (x_remain - mod)/26
    }
    
    return(paste(rev(new_string), collapse=""))
  }
  
  # Identify splitting variables
  splitting_vars <- unique(c(color_by, facet_by))
  
  # Split x by splitting variables.
  if(length(splitting_vars) > 0){
    x <- split(x, by=splitting_vars)
    
  } else {
    x <- list(x)
  }
  
  # Iterate and add cluster names.
  x <- lapply(x, function(y){
    
    # Check if x is empty.
    if(is_empty(y)){
      return(y)
    }
    
    # This is for backward compatibility.
    if(!all(c("cluster_id", "cluster_size") %in% colnames(y))){
      
      # Add cluster name
      y[, "cluster_name":=singular_cluster_character]
      
      return(y)
    }
    
    # Only determine cluster_name for those clusters that have cluster_size > 1.
    # Also, the most important features should receive a higher replacement
    # cluster_id.
    y_short <- y[cluster_size > 1, c("name", "cluster_id"), with=FALSE]
    
    if(!is_empty(y_short)){
      # Remove unused levels for the name column. The levels of name are
      # ordered according to importance.
      y_short <- droplevels(y_short)
      
      # Set placeholder cluster id
      y_short[, "new_cluster_id":=NA_integer_]
      
      new_id <- 1L
      for(feature in levels(y_short$name)){
        # Provide new cluster id in case none exists.
        if(is.na(y_short[name == feature, ]$new_cluster_id[1])){
          # Find the old cluster id.
          old_cluster_id <- y_short[name == feature, ]$cluster_id[1]
          
          # Update all entries with the same old cluster id.
          y_short[cluster_id == old_cluster_id, "new_cluster_id":=new_id]
          
          # Increment new cluster id.
          new_id <- new_id + 1L
        }
      }
      
      # Determine cluster name based on id.
      y_short[, "cluster_name":=..integer_to_char(new_cluster_id), by="name"]
      
      # Drop redundant columns
      y_short[, ":="("cluster_id"=NULL, "new_cluster_id"=NULL)]
      
      # Merge with y.
      y <- merge(x=y, y=y_short, by="name", all=TRUE)
      
      # Mark singular clusters
      y[is.na(cluster_name), "cluster_name":=singular_cluster_character]
      
    } else {
      # Mark singular clusters
      y[, "cluster_name":=singular_cluster_character]
    }
    
    return(y)
  })
  
  x <- data.table::rbindlist(x, use.names=TRUE)
  
  return(x)
}


plotting.split_data_by_facet <- function(x, plot_layout_table=NULL, ...){
  
  if(is_empty(x)){
    return(NULL)
  }
  
  if(is.null(plot_layout_table)){
    plot_layout_table <- do.call(plotting.get_plot_layout_table, args=append(list("x"=x), list(...)))
  }
  
  # Derive facet_by
  facet_by <- setdiff(colnames(plot_layout_table), c("col_id", "row_id", "has_strip_x", "has_strip_y",
                                                     "has_guide", "has_axis_title_x", "has_axis_title_y"))
  
  if(length(facet_by > 0)){
    # Merge the plot_layout_table into x. This will keep things in order.
    x <- merge(x=x, y=plot_layout_table, by=facet_by)
    
  } else {
    x <- cbind(x, plot_layout_table)
  }
  
  # Split data by row, then column
  split_data <- split(x, by=c("row_id", "col_id"), sorted=TRUE)

  return(split_data)
}



plotting.get_plot_layout_dims <- function(plot_layout_table=NULL, ...){
  
  # Create the plot_layout_table if it is not provided.
  if(is.null(plot_layout_table)){
    plot_layout_table <- do.call(plotting.get_plot_layout_table, args=list(...))
  }

  # Return (nrows, ncols)
  return(c(max(plot_layout_table$row_id), max(plot_layout_table$col_id)))
}



plotting.get_plot_layout_table <- function(x, facet_by, facet_wrap_cols,
                                           x_label_shared="overall", y_label_shared="overall"){
  # Suppress NOTES due to non-standard evaluation in data.table
  col_id <- row_id <- NULL
  
  if(is.null(facet_by)){
    # Simple 1x1 layout without facets.
    plot_layout_table <- data.table::data.table("col_id"=1L, "row_id"=1L, "has_guide"=TRUE,
                                          "has_axis_title_x"=TRUE, "has_axis_title_y"=TRUE,
                                          "has_strip_x"=FALSE, "has_strip_y"=FALSE)
    
  } else if(is.null(facet_wrap_cols)){
    
    # Generate a plot_layout_table, and order
    plot_layout_table <- unique(x[, (facet_by), with=FALSE], by=facet_by)
    data.table::setorderv(x=plot_layout_table, cols=facet_by)
    
    # Find the number of columns
    n_cols <- length(unique(x[[facet_by[1]]]))
    
    # Add column id to the plot_layout_table
    plot_layout_table[, "col_id":=.GRP, by=get(facet_by[1])]
    
    if(length(facet_by) > 1){
      # Find the number of rows
      n_levels <- sapply(facet_by[2:length(facet_by)], function(ii, x){
        if(is.factor(x[[ii]])){
          return(nlevels(x[[ii]]))
        } else {
          return(length(unique(x[[ii]])))
        }
      }, x=x)
      n_rows <- prod(n_levels)
      
      # Add row id to the plot_layout_table
      facet_row_cols <- facet_by[2:length(facet_by)]
      plot_layout_table[, "row_id":=.GRP, by=mget(facet_row_cols)]
      
    } else {
      # There is only one row
      n_rows <- 1
      plot_layout_table[, "row_id":=1L]
    }
    
    # Set default elements
    plot_layout_table[, ":="("has_strip_x"=FALSE, "has_strip_y"=FALSE, "has_guide"=FALSE,
                       "has_axis_title_x"=FALSE, "has_axis_title_y"=FALSE)]
    
    # Update default elements based on layout
    if(n_cols > 1){
      plot_layout_table[row_id==1, "has_strip_x":=TRUE]
    }
    
    if(n_rows > 1){
      plot_layout_table[col_id==n_cols, "has_strip_y":=TRUE]
    }
    
    plot_layout_table[col_id==1, "has_axis_title_y":=TRUE]
    plot_layout_table[row_id==n_rows, "has_axis_title_x":=TRUE]
    
    # Set the plot with the guide
    plot_layout_table[col_id==n_cols & row_id==1, "has_guide":=TRUE]
    
  } else {
    
    # Generate a plot_layout_table, and order
    plot_layout_table <- unique(x[, (facet_by), with=FALSE], by=facet_by)
    data.table::setorderv(x=plot_layout_table, cols=facet_by)
    
    # Number of columns is provided using facet_wrap_cols.
    len_table <- nrow(plot_layout_table)
    n_cols <- facet_wrap_cols
    n_rows <- ceiling(len_table / n_cols)
    
    # Generate the column and row positions.
    col_ids <- rep(seq_len(n_cols), times=n_rows)[seq_len(len_table)]
    row_ids <- rep(seq_len(n_rows), each=n_cols)[seq_len(len_table)]
    
    # Add column and row ids to the plot_layout_table.
    plot_layout_table[, ":="("col_id"=col_ids, "row_id"=row_ids, "has_strip_x"=TRUE, "has_strip_y"=FALSE,
                       "has_guide"=FALSE, "has_axis_title_x"=FALSE, "has_axis_title_y"=FALSE)]
    
    plot_layout_table[col_id==1, "has_axis_title_y":=TRUE]
    plot_layout_table[(len_table-n_cols+1):len_table, "has_axis_title_x":=TRUE]
    
    # Set the plot with the guide
    plot_layout_table[col_id==n_cols & row_id==1, "has_guide":=TRUE]
  }
  
  # If x_label_shared is individual, all subplots should retain their axes.
  if(x_label_shared == "individual"){
    plot_layout_table[, "has_axis_title_x":=TRUE]
  }
  
  # If y_label_shared is individual, all subplots should retain their axes.
  if(y_label_shared == "individual"){
    plot_layout_table[, "has_axis_title_y":=TRUE]
  }
  
  return(plot_layout_table)
}



plotting.update_facet_plot_elements <- function(p, x, keep_axis_labels_x=FALSE, keep_axis_labels_y=FALSE){
  
  # Check whether the table containing the data has columns related to external
  # facetting.

  if(!all(c("col_id", "row_id", "has_strip_x", "has_strip_y", "has_guide", "has_axis_title_x", "has_axis_title_y") %in% colnames(x))){
    return(p)
  }
  
  if(x[["has_strip_x"]][1] == FALSE){
    p <- p + ggplot2::theme(strip.background.x=ggplot2::element_blank(),
                            strip.text.x=ggplot2::element_blank())
  }
  
  if(x[["has_strip_y"]][1] == FALSE){
    p <- p + ggplot2::theme(strip.background.y=ggplot2::element_blank(),
                            strip.text.y=ggplot2::element_blank())
  }
  
  if(x[["has_guide"]][1] == FALSE){
    p <- p + ggplot2::theme(legend.position="none")
  }
  
  if(x[["has_axis_title_x"]][1] == FALSE){
    p <- p + ggplot2::theme(axis.title.x=ggplot2::element_blank())
    
    # Check if axis tick labels should be kept, e.g. for feature similarity
    # heatmaps where the content may be different.
    if(!keep_axis_labels_x) p <- p + ggplot2::theme(axis.text.x=ggplot2::element_blank())
  }
  
  if(x[["has_axis_title_y"]][1] == FALSE){
    p <- p + ggplot2::theme(axis.title.y=ggplot2::element_blank())
    
    # Check if axis tick labels should be kept, e.g. for feature similarity
    # heatmaps where the content may be different.
    if(!keep_axis_labels_y) p <- p + ggplot2::theme(axis.text.y=ggplot2::element_blank())
  }
  
  return(p)
}


plotting.get_panel_spacing <- function(ggtheme=NULL, axis){
  # Obtain spacing between panels. This determines distance between facets.
  
  # Import default ggtheme in case none is provided.
  if(!any(class(ggtheme) == "theme")) {
    ggtheme <- plotting.get_theme(use_theme=ggtheme)
  }
  
  if(axis == "x"){
    # Obtain spacing from ggtheme.
    panel_spacing <- ggtheme$panel.spacing.x
    
    # From more general element.
    if(is.null(panel_spacing)){
      panel_spacing <- ggtheme$panel.spacing
    }
    
  } else if(axis == "y"){
    # Obtain spacing from ggtheme.
    panel_spacing <- ggtheme$panel.spacing.y
    
    # From more general element.
    if(is.null(panel_spacing)){
      panel_spacing <- ggtheme$panel.spacing
    }
  }
  
  # If no panel spacing is provided, produce 0.0 length spacing
  if(!grid::is.unit(panel_spacing)){
    panel_spacing <- grid::unit(0.0, "pt")
  }
  
  return(panel_spacing)
}


plotting.get_geom_text_font_size <- function(ggtheme=NULL){
  
  # Import default ggtheme in case none is provided.
  if(!any(class(ggtheme) == "theme")) {
    ggtheme <- plotting.get_theme(use_theme=ggtheme)
  }
  
  # Find the text size for the table. This is based on text sizes in the ggtheme.
  fontsize <- ggtheme$text$size
  fontsize_rel <- 1.0
  
  # Attempt to base the text size on the general axis.text attribute.
  if(!is.null(ggtheme$axis.text$size)){
    if(any(class(ggtheme$axis.text$size) == "rel")){
      # Find the relative text size of axis text.
      fontsize_rel <- as.numeric(ggtheme$axis.text$size)
      
    } else {
      # Set absolute text size.
      fontsize <- ggtheme$axis.text$size
      fontsize_rel <- 1.0
    }
  }
  
  # Attempt to refine the text size using the axis.text.y attribute in
  # particular.
  if(!is.null(ggtheme$axis.text.y$size)){
    if(any(class(ggtheme$axis.text.y$size) == "rel")){
      # Set relative text size of axis text
      fontsize_rel <- as.numeric(ggtheme$axis.text.y$size)
      
    } else {
      # Set absolute text size.
      fontsize <- as.numeric(ggtheme$axis.text.y$size)
      fontsize_rel <- 1.0
    }
  }
  
  # Obtain lineheight
  lineheight <- ggtheme$text$lineheight
  if(!is.null(ggtheme$axis.text$lineheight)) { lineheight <- ggtheme$axis.text$lineheight }
  if(!is.null(ggtheme$axis.text.y$lineheight)) { lineheight <- ggtheme$axis.text.y$lineheight }
  
  # Update the text size using the magical ggplot2 point size (ggplot2:::.pt).
  geom_text_size <- fontsize * fontsize_rel / 2.845276
  
  return(list("geom_text_size"=geom_text_size, "fontsize"=fontsize, "fontsize_rel"=fontsize_rel, "lineheight"=lineheight))
}



plotting.arrange_figures <- function(grobs, n_rows, n_cols, elements, element_grobs, ggtheme){
  
  # Iterator
  figure_id <- 1L
  
  # Placeholder for final figure
  g <- NULL
  
  # Iterate over rows and columns.
  for(ii in seq_len(n_rows)){
    
    # Create a placeholder.
    g_current_row <- NULL
    
    # Populate the current row with figures.
    for(jj in seq_len(n_cols)){
      
      # Check if the iterator exceeds the maximum number of available figures.
      if(figure_id > length(grobs)) break()
      
      if(is.null(g_current_row)){
        # If the current row is still empty, copy the first figure.
        g_current_row <- grobs[[figure_id]]
        
      } else {
        # If the current row is not empty, use cbind.gtable to combine figures
        # column-wise.
        
        # First insert a column that spaces the facets.
        g_current_row <- gtable::gtable_add_cols(g_current_row,
                                                 widths=plotting.get_panel_spacing(ggtheme=ggtheme, axis="x"))
        
        # Add the figure to the current figure.
        g_current_row <- cbind(g_current_row, grobs[[figure_id]])
      }
      
      # Increment iterator
      figure_id <- figure_id + 1L
    }
    
    # Populate the figure.
    if(is.null(g)){
      # If the figure is still empty, copy the current row.
      g <- g_current_row
      
    } else {
      # Use rbind.gtable to combine rows.
      g <- rbind(g, g_current_row)
    }
    
    # Check if the iterator exceeds the maximum number of available figures.
    if(figure_id > length(grobs)) break()
  }
  
  # Re-insert elements.
  g <- plotting.reinsert_plot_elements(g=g,
                                       elements=elements,
                                       grob_list=element_grobs,
                                       ggtheme=ggtheme)
  
  return(g)
}


plotting.extract_plot_elements <- function(p, elements){
  
  element_list <- list()

  # Convert to grobs
  g <- plotting.to_grob(p)
  
  if("guide" %in% elements){
    element_list$guide <- .gtable_extract(g=g, element="guide-box", drop_empty=TRUE)
  }
  
  if("axis_title_x" %in% elements){
    element_list$axis_title_x <- .gtable_extract(g=g, element="xlab-b", drop_empty=TRUE)
  }
  
  if("axis_title_y" %in% elements){
    element_list$axis_title_y <- .gtable_extract(g=g, element="ylab-l", drop_empty=TRUE)
  }
  
  if("strip_x" %in% elements){
    element_list$strip_x <- .gtable_extract(g=g, element="strip-t-", partial_match=TRUE, drop_empty=TRUE)
  }
  
  if("strip_y" %in% elements){
    element_list$strip_y <- .gtable_extract(g=g, element="strip-r-", partial_match=TRUE, drop_empty=TRUE)
  }

  return(element_list)
}


plotting.remove_plot_elements <- function(p, elements){
  # Remove elements that were extracted as a grob from plots.
  
  if("guide" %in% elements){
    p <- p + ggplot2::theme(legend.position="none")
  }
  
  if("axis_title_x" %in% elements){
    p <- p + ggplot2::theme(axis.title.x=ggplot2::element_blank())
  }
  
  if("axis_title_y" %in% elements){
    p <- p + ggplot2::theme(axis.title.y=ggplot2::element_blank())
  }
  
  if("strip_x" %in% elements){
    p <- p + ggplot2::theme(strip.background.x=ggplot2::element_blank(),
                            strip.text.x=ggplot2::element_blank())
  }
  
  if("strip_y" %in% elements){
    p <- p + ggplot2::theme(strip.background.y=ggplot2::element_blank(),
                            strip.text.y=ggplot2::element_blank())
  }
  
  return(p)
}


plotting.reinsert_plot_elements <- function(g, elements=NULL, grob_list, ggtheme){
  
  # Re-insert guides.
  if("guide" %in% elements & !is.null(grob_list$guide)){
    # Find legend position
    legend_position <- ggtheme$legend.position
    
    if(legend_position == "right"){
      # Align to right of the plot, and iterate inward to find valid reference elements.
      for(ref_element in c("strip-r", "ylab-r", "axis-r", "panel")){
        if(.gtable_element_in_layout(g=g, element=ref_element, partial_match=TRUE)){
          
          # If the reference element exists, add and align along background.
          g <- .gtable_insert_along(g=g,
                                    g_new=grob_list$guide,
                                    ref_element=ref_element,
                                    along_element="panel",
                                    where=legend_position,
                                    partial_match_ref=TRUE,
                                    partial_match_along=TRUE)
          
          break()
        }
      }
      
    } else if(legend_position == "left"){
      # Align to left of the plot, and iterate inward to find valid reference
      # elements.
      for(ref_element in c("strip-l", "ylab-l", "axis-l", "panel")){
        if(.gtable_element_in_layout(g=g, element=ref_element, partial_match=TRUE)){
          
          # If the reference element exists, add and align along background.
          g <- .gtable_insert_along(g=g,
                                    g_new=grob_list$guide,
                                    ref_element=ref_element,
                                    along_element="panel",
                                    where=legend_position,
                                    partial_match_ref=TRUE,
                                    partial_match_along=TRUE)
          
          break()
        }
      }
      
    } else if(legend_position == "bottom"){
      # Align to bottom of the plot, and iterate inward to find valid reference
      # elements.
      for(ref_element in c("strip-b", "xlab-b", "axis-b", "panel")){
        if(.gtable_element_in_layout(g=g, element=ref_element, partial_match=TRUE)){
          
          # If the reference element exists, add and align along background.
          g <- .gtable_insert_along(g=g,
                                    g_new=grob_list$guide,
                                    ref_element=ref_element,
                                    along_element="panel",
                                    where=legend_position,
                                    partial_match_ref=TRUE,
                                    partial_match_along=TRUE)
          
          break()
        }
      }
      
    } else if(legend_position == "top"){
      # Align to top of the plot, and iterate inward to find valid reference
      # elements.
      for(ref_element in c("strip-t", "xlab-t", "axis-t", "panel")){
        if(.gtable_element_in_layout(g=g, element=ref_element, partial_match=TRUE)){
          
          # If the reference element exists, add and align along background.
          g <- .gtable_insert_along(g=g,
                                    g_new=grob_list$guide,
                                    ref_element=ref_element,
                                    along_element="panel",
                                    where=legend_position,
                                    partial_match_ref=TRUE,
                                    partial_match_along=TRUE)
          
          break()
        }
      }
    }
  }
  
  
  # Insert strip with facet text (for columns)
  if("strip_x" %in% elements & !is.null(grob_list$strip_x)){
    
    # Align top of the plot, and iterate inward to find valid reference elements.
    for(ref_element in c("xlab-t", "axis-t", "panel")){
      if(.gtable_element_in_layout(g=g, element=ref_element, partial_match=TRUE)){
        
        # If the reference element exists, add and align along the panel(s).
        g <- .gtable_insert_along(g=g,
                                  g_new=grob_list$strip_x,
                                  ref_element=ref_element,
                                  along_element="panel",
                                  where="top",
                                  partial_match_ref=TRUE,
                                  partial_match_along=TRUE)
        
        break()
      }
    }
  }
  
  
  # Insert strip with facet text (for rows)
  if("strip_y" %in% elements & !is.null(grob_list$strip_y)){
    
    # Align to right of the plot, and iterate inward to find valid reference
    # elements.
    for(ref_element in c("ylab-r", "axis-r", "panel")){
      if(.gtable_element_in_layout(g=g, element=ref_element, partial_match=TRUE)){
        
        # If the reference element exists, add and align along the panel(s).
        g <- .gtable_insert_along(g=g,
                                  g_new=grob_list$strip_y,
                                  ref_element=ref_element,
                                  along_element="panel",
                                  where="right",
                                  partial_match_ref=TRUE,
                                  partial_match_along=TRUE)
        
        break()
      }
    }
  }
  
  
  # Insert x-axis label
  if("axis_title_x" %in% elements & !is.null(grob_list$axis_title_x)){
    
    # Align to bottom of the plot, and iterate inward to find valid reference
    # elements.
    for(ref_element in c("axis-b", "panel")){
      if(.gtable_element_in_layout(g=g, element=ref_element, partial_match=TRUE)){
        
        # If the reference element exists, add and align along the panel(s).
        g <- .gtable_insert_along(g=g,
                                  g_new=grob_list$axis_title_x,
                                  ref_element=ref_element,
                                  along_element="panel",
                                  where="bottom",
                                  partial_match_ref=TRUE,
                                  partial_match_along=TRUE)
        
        break()
      }
    }
  }
  
  
  # Insert y-axis label
  if("axis_title_y" %in% elements & !is.null(grob_list$axis_title_y)){
    
    # Align to left of the plot, and iterate inward to find valid reference elements.
    for(ref_element in c("axis-l", "panel")){
      if(.gtable_element_in_layout(g=g, element=ref_element, partial_match=TRUE)){
        
        # If the reference element exists, add and align along the panel(s).
        g <- .gtable_insert_along(g=g,
                                  g_new=grob_list$axis_title_y,
                                  ref_element=ref_element,
                                  along_element="panel",
                                  where="left",
                                  partial_match_ref=TRUE,
                                  partial_match_along=TRUE)
        
        break()
      }
    }
  }
  
  return(g)
}



plotting.to_grob <- function(plots_or_grobs){

  # Convert to list if the input is a single grob or 
  if(grid::is.grob(plots_or_grobs) | ggplot2::is.ggplot(plots_or_grobs)){
    plots_or_grobs <- list(plots_or_grobs)
    
    # Set a flag so that we unlist the results after conversion.
    unlist_grobs <- TRUE
    
  } else{
    unlist_grobs <- FALSE
  }
  
  # Initialise list of grobs
  grobs <- list()
  
  for(p in plots_or_grobs){
    if(any(class(p) == "familiar_ggplot")){
      
      # Convert to grob
      g <- ggplot2::ggplotGrob(p)

      # Make changes to g according to p$custom_grob.
      if(!is.null(p$custom_grob)){
        if(!is.null(p$custom_grob$heights)){
          
          # Iterate over the elements that need to be updated.
          for(ii in seq_len(length(p$custom_grob$heights$name))){
            # Extract name and height
            name <- p$custom_grob$heights$name[ii]
            height <- p$custom_grob$heights$height[ii]
            
            # Find the row containing the grob cell indicated by the name and
            # update the heights.
            g$heights[g$layout[g$layout$name == name, "t"]] <- height
          }
        }
        
        if(!is.null(p$custom_grob$widths)){
          
          # Iterate over the elements that need to be updated.
          for(ii in seq_len(length(p$custom_grob$widths$name))){
            name <- p$custom_grob$widths$name[ii]
            width <- p$custom_grob$widths$width[ii]
            
            # Find the column containing the grob cell indicated by the name and
            # update the widths.
            g$widths[g$layout[g$layout$name == name, "l"]] <- width
          }
        }
      }
    } else if(any(class(p) == "ggplot")){
      
      # Convert to grob
      g <- ggplot2::ggplotGrob(p)
      
    } else if(any(class(p) == "grob")){
      
      # Assign grob
      g <- p
      
    } else {
      warning(paste0("Could not convert an object of class ", class(p), " to a grob."))
      g <- NULL
    }
    
    grobs <- append(grobs, list(g))
  }
  
  if(unlist_grobs){
    grobs <- grobs[[1]]
  }
  
  return(grobs)
}


plotting.create_legend_label <- function(user_label, color_by=NULL, linetype_by=NULL, combine_legend=FALSE){
  
  # Sent for inspection
  plotting.check_input_args(legend_label=user_label,
                            combine_legend=combine_legend)
  
  if(is.null(color_by) & is.null(linetype_by)){
    # No splitting variables are used
    return(list("guide_color"=NULL, "guide_linetype"=NULL))
    
  }

  # Collect required list entries
  req_entries <- character(0)
  if(!is.null(color_by)){
    req_entries <- append(req_entries, "guide_color")
  }
  
  if(!is.null(linetype_by)){
    req_entries <- append(req_entries, "guide_linetype")
  }
  
  # Waiver: no user input
  if(is.waive(user_label)){
    if(combine_legend){
      legend_label <- gsub(x=paste0(unique(c(color_by, linetype_by)), collapse=" & "), pattern="_", replacement=" ")
      
      return(list("guide_color"=legend_label, "guide_linetype"=legend_label))
      
    } else {
      
      # Colour labels
      if(!is.null(color_by)){
        color_guide_label <- sub(x=paste0(color_by, collapse=" & "), pattern="_", replacement=" ")
      } else {
        color_guide_label <- NULL
      }
      
      # Linetype labels
      if(!is.null(linetype_by)){
        linetype_guide_label <- sub(x=paste0(linetype_by, collapse=" & "), pattern="_", replacement=" ")
      } else {
        linetype_guide_label <- NULL
      }
      
      return(list("guide_color"=color_guide_label, "guide_linetype"=linetype_guide_label))
    }
    
  } else if(is.null(user_label)){
    #NULL input
    
    return(list("guide_color"=NULL, "guide_linetype"=NULL))
    
  } else if(is.list(user_label)){
    # List input
    
    # Check entries for existence
    for(current_entry in req_entries){
      if(!current_entry %in% names(user_label)){
        stop(paste("A legend name is missing for ", current_entry, ". Please set this name to a \"", current_entry, "\" list element, e.g. list(\"",
                   current_entry,"\"=\"some name\", ...).", sep=""))
      }
    }
    
    # Select required entries
    user_label <- user_label[names(user_label) %in% req_entries]
    
    # Check that all entries are the same
    if(combine_legend & length(req_entries) >= 2){
      if(!all(sapply(user_label[2:length(user_label)], identical, user_label[[1]]))){
        stop(paste("Not all provided legend names are identical, but identical legend names are required for combining the legend."))
      }
    }
    
    return(user_label)
  } else if(length(req_entries) >= 2 & !combine_legend){
    # Single input where multiple is required
    
    stop(paste("Multiple legend names are required, but only one is provided. Please return a list with",
               paste0("\"", req_entries, "\"", collapse=", "), "elements.", sep=" "))
    
  } else {
    # Single input
    
    return(list("guide_color"=user_label, "guide_linetype"=user_label))
  }
}



plotting.create_guide_table <- function(x, color_by=NULL, linetype_by=NULL, discrete_palette=NULL, combine_legend=TRUE){
  
  #####.get_guide_tables -------------------------------------------------------------------------------------
  .get_guide_tables <- function(x, color_by, linetype_by, discrete_palette){
  
    # Suppress NOTES due to non-standard evaluation in data.table
    color_id <- linetype_id <- NULL
    
    # Select unique variables
    unique_vars <- unique(c(color_by, linetype_by))
    
    # Check whether there are any unique splitting variables
    if(is.null(unique_vars)){
      return(NULL)
    }
    
    # Generate a guide table
    guide_table <- data.table::data.table(expand.grid(lapply(rev(unique_vars), function(ii, x) (levels(x[[ii]])), x=x)))
    
    # Rename variables
    data.table::setnames(x=guide_table, rev(unique_vars))
    
    # Convert to factors
    for(ii in unique_vars){
      guide_table[[ii]] <- factor(guide_table[[ii]], levels=levels(x[[ii]]))
    }
    
    # Order columns according to unique_vars
    data.table::setcolorder(x=guide_table, neworder=unique_vars)
    
    # Order data set by columns
    data.table::setorderv(x=guide_table, cols=unique_vars)
    
    # Set breaks
    breaks <- apply(guide_table, 1, paste, collapse=", ")

    # Extend guide table
    if(!is.null(color_by)){
      
      # Generate breaks
      guide_table$color_breaks <- factor(breaks, levels=breaks)
      
      # Define colour groups
      guide_table[, "color_id":=.GRP, by=color_by]
      
      # Get the palette to use.
      discr_palette <- plotting.get_palette(x=discrete_palette,
                                            n=max(guide_table$color_id),
                                            palette_type="qualitative")

      # Assign colour values
      guide_table[, "color_values":=discr_palette[color_id]]
    } 
    
    if(!is.null(linetype_by)){
      
      # Generate breaks
      guide_table$linetype_breaks <- factor(breaks, levels=breaks)
      
      # Define linetype groups
      guide_table[, "linetype_id":=.GRP, by=linetype_by]
      
      # Get the palette to use
      line_palette <- scales::linetype_pal()(max(guide_table$linetype_id))
    
      # Assign linetypes
      guide_table[, "linetype_values":=line_palette[linetype_id]]
    }
    
    return(guide_table)
  }
  
  #####Main--------------------------------------------------------------------------------------------------
  
  # Extract guide tables
  if(combine_legend){
    guide_list <- list("guide_color"    = .get_guide_tables(x=x, color_by=color_by, linetype_by=linetype_by, discrete_palette=discrete_palette),
                       "guide_linetype" = .get_guide_tables(x=x, color_by=color_by, linetype_by=linetype_by, discrete_palette=discrete_palette))
  } else {
    guide_list <- list("guide_color"    = .get_guide_tables(x=x, color_by=color_by, linetype_by=NULL, discrete_palette=discrete_palette),
                       "guide_linetype" = .get_guide_tables(x=x, color_by=NULL, linetype_by=linetype_by, discrete_palette=discrete_palette))
  }

  # Filter out lists corresponding to missing split variables
  guide_list <- guide_list[!sapply(list(color_by, linetype_by), is.null)]
  
  if(length(guide_list) == 0){
    return(list("data"=x))
  }
  
  # Initialise return list
  return_list <- list()
  
  # Add break column of the remaining lists to x
  for(guide_type in names(guide_list)){
    
    if(guide_type == "guide_color"){
      # Add color_breaks to x
      if(combine_legend){
        x <- merge(x=x, y=guide_list[[guide_type]][, c(unique(c(color_by, linetype_by)), "color_breaks"), with=FALSE],
                   by=unique(c(color_by, linetype_by)), all.x=TRUE, all.y=FALSE)
      } else {
        x <- merge(x=x, y=guide_list[[guide_type]][, c(color_by, "color_breaks"), with=FALSE],
                   by=color_by, all.x=TRUE, all.y=FALSE)
      }
      
      # Return guide_color
      return_list[[guide_type]] <- guide_list[[guide_type]]
      
    } else if(guide_type == "guide_linetype"){
      # Add linetype_breaks to x
      if(combine_legend){
        x <- merge(x=x, y=guide_list[[guide_type]][, c(unique(c(color_by, linetype_by)), "linetype_breaks"), with=FALSE],
                   by=unique(c(color_by, linetype_by)), all.x=TRUE, all.y=FALSE)
      } else {
        x <- merge(x=x, y=guide_list[[guide_type]][, c(linetype_by, "linetype_breaks"), with=FALSE],
                   by=linetype_by, all.x=TRUE, all.y=FALSE)
      }
      
      # Return guide_linetype
      return_list[[guide_type]] <- guide_list[[guide_type]]
    }
  }
  
  # Add updated data
  return_list$data <- x

  return(return_list)
}


plotting.draw <- function(plot_or_grob){
  
  if(ggplot2::is.ggplot(plot_or_grob)){
    show(plot_or_grob)
    
  } else if(grid::is.grob(plot_or_grob)){
    grid::grid.newpage()
    grid::grid.draw(plot_or_grob)
    
  } else {
    stop("Plot could not be drawn.")
  }
  
}


plotting.save_plot_to_file <- function(plot_obj, object, dir_path, type, subtype=NULL, filename=NULL, device="png", ...){
  ### ... are passed to ggplot2::ggsave
  
  # Check if the plot object exists
  if(is.null(plot_obj)) { return(NULL) }

  # Check if directory exists
  if(is.encapsulated_path(dir_path)){
    file_dir <- normalizePath(file.path(dir_path, object@collection_name, type), mustWork=FALSE)
    
  } else {
    file_dir <- normalizePath(dir_path, mustWork=FALSE)
  }
  
  if(!dir.exists(file_dir)) { dir.create(file_dir, recursive=TRUE) }

  for(current_device in device){
    if(is.null(filename)){
      filename <- paste0(type, ifelse(is.null(subtype), "", paste0("_", subtype)), ".", current_device)
    }
    
    # There may be an issue with a cold RStudio where the plotting devices have not started.
    tryCatch({
      # Call ggsave to save the data
      suppressMessages(do.call(ggplot2::ggsave, args=append(list("filename"=filename, "plot"=plot_obj, "device"=device, "path"=file_dir), list(...))))
    }, error = function(err){
      logger.warning(paste0("Could not create plot ", filename, ". The OS may not allow long file names."))
    })
    
  }
  invisible()
}



plotting.format_number <- function(x, digits=3){
  
  # Find the base-10 integer of the data.
  x_base <- floor(log10(abs(x)))
  x_base <- x_base[is.finite(x_base)]
  
  # Determine the largest base.
  common_base <- ifelse(length(x_base) > 0, max(x_base), 0)

  # Round numbers.
  x <- round(x / 10^(1 + common_base - digits)) * 10^(1 + common_base - digits)
  
  # Format output.
  return(format(x, digits=digits, trim=TRUE))
}



plotting.dendrogram_as_table <- function(h, similarity_metric){
  
  # Suppress NOTES due to non-standard evaluation in data.table
  x_1 <- y_1 <- x_2 <- NULL
  
  # Convert to dendogram
  h <- stats::as.dendrogram(h)
  
  # Determine the metric range.
  metric_range <- similarity.metric_range(similarity_metric=similarity_metric, as_distance=TRUE)
  
  # Convert dendogram to a list of connectors that can later be used for
  # plotting. Note that we do not know where the origin should be located on the
  # x-axis. We will correct for that later.
  connectors <- .decompose_dendogram(h=h,
                                     parent_height=max(metric_range),
                                     parent_x=0.0)
  
  # Combine into single data.table.
  connectors <- data.table::rbindlist(connectors)
  
  # Keep only nodes with finite parent height (y_1). Depending on the metric,
  # this means that the connector with the origin may not be drawn.
  connectors <- connectors[is.finite(y_1)]
  
  # Return null if there are no connectors to be drawn.
  if(is_empty(connectors)) return(NULL)
  
  # Reposition the left-most leaf to 0.0.
  min_leaf_pos <- min(c(connectors$x_1, connectors$x_2))
  connectors[, ":="("x_1"=x_1 - min_leaf_pos,
                    "x_2"=x_2 - min_leaf_pos)]
  
  return(connectors)
}


.decompose_dendogram <- function(h, parent_height=Inf, parent_x=0.0){
  # Decompose dendogram. The function is designed to iterate through a
  # dendogram, and obtain the connector between node (h) and its parent, as well
  # as the connectors between the node and its children h[[1]] and h[[2]],
  # unless it has no children.
  
  dend_attr <- attributes(h)
  
  if(is.null(dend_attr$midpoint)){
    # This indicates that the node has no children.
      
    # Connector from parent.
    conn_parent_child <- data.table::data.table("x_1"=parent_x,
                                                "y_1"=parent_height,
                                                "x_2"=parent_x,
                                                "y_2"=dend_attr$height,
                                                "feature"=dend_attr$label)
    
    return(list(conn_parent_child))
  }
  
  # Connector from parent.
  conn_parent_child <- data.table::data.table("x_1"=parent_x,
                                              "y_1"=parent_height,
                                              "x_2"=parent_x,
                                              "y_2"=dend_attr$height,
                                              "feature"=NA_character_)
  
  # Connector to left leaf.
  conn_child_l_leaf <- data.table::data.table("x_1"=parent_x,
                                              "y_1"=dend_attr$height,
                                              "x_2"=parent_x - dend_attr$midpoint,
                                              "y_2"=dend_attr$height,
                                              "feature"=NA_character_)
  
  # Connector to right leaf.
  conn_child_r_leaf <- data.table::data.table("x_1"=parent_x,
                                              "y_1"=dend_attr$height,
                                              "x_2"=parent_x + dend_attr$midpoint,
                                              "y_2"=dend_attr$height,
                                              "feature"=NA_character_)
  
  # Add data.tables as list elements.
  connector_list <- list(conn_parent_child, conn_child_l_leaf, conn_child_r_leaf)
  
  # Left leaf
  if(!is.null(h[[1]])){
    left_leaf_connectors <- .decompose_dendogram(h=h[[1]],
                                                 parent_height=dend_attr$height,
                                                 parent_x=parent_x - dend_attr$midpoint)
    
    # Append to list
    connector_list <- append(connector_list, left_leaf_connectors)
  }
  
  # Right leaf
  if(!is.null(h[[2]])){
    right_leaf_connectors <- .decompose_dendogram(h=h[[2]],
                                                  parent_height=dend_attr$height,
                                                  parent_x=parent_x + dend_attr$midpoint)
    
    # Append to list
    connector_list <- append(connector_list, right_leaf_connectors)
  }
  
  return(connector_list)
}

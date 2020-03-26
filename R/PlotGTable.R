.gtable_element_in_layout <- function(g, element, partial_match=FALSE){
  
  if(partial_match){
    return(any(grepl(pattern=element, x=g$layout$name)))
    
  } else {
    return(any(element %in% g$layout$name))
  }
}



.gtable_get_position <- function(g, element, where=NULL, partial_match=FALSE){
  
  # Find position.
  if(partial_match){
    # Based on partial matching.
    position <- g$layout[grepl(pattern=element, x=g$layout$name), c("t", "l", "b", "r")]
    
  } else {
    # Based on exact matching.
    position <- g$layout[g$layout$name == element, c("t", "l", "b", "r")]
  }
  
  if(nrow(position) == 0){
    ..error_reached_unreachable_code(".gtable_get_position: element not found in layout table.")
  }
  
  if(is.null(where)){
    if(nrow(position) != 1){
      stop(paste0("Multiple matches, please set where attribute."))
    }
    
  } else if(where == "top"){
    # Select the uppermost element.
    position <- position[position$t == min(position$t), ][1, ]
    
  } else if(where == "bottom"){
    # Select the bottommost element.
    position <- position[position$b == max(position$b), ][1, ]
    
  } else if(where == "left"){
    # Select the leftmost element
    position <- position[position$l == min(position$l), ][1, ]
    
  } else if(where == "right"){
    # Select the rightmost element
    position <- position[position$r == max(position$r), ][1, ]
    
  } else {
    stop(..error_value_not_allowed(x=where, var_name="where",
                                   values=c("top", "bottom", "left", "right")))
  }
  
  # Return as array.
  position <- simplify2array(position)
  
  return(position)
}


.gtable_get_extent <- function(g, element, partial_match=FALSE){
  
  # Find position.
  if(partial_match){
    # Based on partial matching.
    position <- g$layout[grepl(pattern=element, x=g$layout$name), c("t", "l", "b", "r")]
    
  } else {
    # Based on exact matching.
    position <- g$layout[g$layout$name == element, c("t", "l", "b", "r")]
  }
  
  if(nrow(position) == 0){
    ..error_reached_unreachable_code(".gtable_get_extent: element not found in layout table.")
  }
  
  # Find extent by deriving the bounding box of the elements.
  extent <- list()
  extent$t <- min(position$t)
  extent$b <- max(position$b)
  extent$l <- min(position$l)
  extent$r <- max(position$r)
  
  # Return as array.
  extent <- simplify2array(extent)
  
  return(extent)
}


.gtable_extract <- function(g, element, partial_match=FALSE, drop_empty=FALSE){
  
  # Extract partially matching elements
  if(partial_match){
    extracted_table <- gtable::gtable_filter(x=g, pattern=paste0(element, collapse="|"))
    
  } else {
    # Extract exactly matching elements
    extracted_table <- .gtable_filter_exact(g=g, element=element)
  }
  
  # Drop empty elements
  if(drop_empty){
    extracted_table <- .gtable_drop_empty(g=extracted_table)
  }
  
  if(length(extracted_table) == 0){
    extracted_table <- NULL
  }
  
  return(extracted_table)
}


.gtable_drop_empty <- function(g, trim=TRUE){
  
  # Find grob classes
  grob_classes <- lapply(g$grobs, class)
  
  # Find zeroGrob and nullGrob classes, which represent empty elements.
  matches <- sapply(grob_classes, function(ii) any(ii %in% c("zeroGrob", "nullGrob")))
  
  # Filter layout and grobs of the gtable g by keeping non-empty elements.
  g$layout <- g$layout[!matches, , drop=FALSE]
  g$grobs <- g$grobs[!matches]
  
  if(trim) g <- gtable::gtable_trim(g)
  
  return(g)
}


.gtable_filter_exact <- function(g, element, trim=TRUE, invert=FALSE){
  # Similar to gtable::gtable_filter, but with exact matching.
  
  # Find exact matches
  matches <- g$layout$name %in% element
  
  # If invert is TRUE, select only non-matching entries.
  if(invert) matches <- !matches
  
  # Filter layout and grobs of the gtable g.
  g$layout <- g$layout[matches, , drop=FALSE]
  g$grobs <- g$grobs[matches]
  
  if(trim) g <- gtable::gtable_trim(g)
  
  return(g)
}



.gtable_insert <- function(g, g_new, where="top", ref_element="panel", spacer=NULL, partial_match=FALSE){
  
  if(length(g_new) == 0){
    return(g)
  }

  # Create an offset.
  offset <- integer(4)
  names(offset) <- c("t", "l", "b", "r")
  
  # Find position to insert this element
  ref_position <- .gtable_get_position(g=g, element=ref_element, where=where, partial_match=partial_match)

  # Add spacing
  if(where %in% c("top", "bottom")){
    
    # Add space to top.
    if(!is.null(spacer$t)){
      g_new <- gtable::gtable_add_rows(g_new, heights=spacer$t, pos=0)
    }
    
    # Add space to bottom.
    if(!is.null(spacer$b)){
      g_new <- gtable::gtable_add_rows(g_new, heights=spacer$b, pos=-1)
    }
    
  } else {
    
    # Add space to left.
    if(!is.null(spacer$l)){
      g_new <- gtable::gtable_add_cols(g_new, widths=spacer$l, pos=0)
    }
    
    # Add space to right.
    if(!is.null(spacer$r)){
      g_new <- gtable::gtable_add_cols(g_new, widths=spacer$r, pos=-1)
    }
  }
  
  
  # Make room to insert the stuff.
  if(where == "top"){
    # Add row below t-1 (i.e. at t, and move existing rows down).
    g <- gtable::gtable_add_rows(g, heights=g_new$heights, pos=ref_position[["t"]]-1)
    
    # This shifts the rest of the elements (including the reference element)
    # down by a number of rows, which means that we need an offset.
    offset[["t"]] <- offset[["b"]] <- length(g_new$heights)
    
  } else if(where == "bottom"){
    # Add row below b (i.e. at b+1, and move existing rows down).
    g <- gtable::gtable_add_rows(g, heights=g_new$heights, pos=ref_position[["b"]])
    
    # This does not shift the reference element down, which means that the
    # offset is -1L.
    offset[["t"]] <- offset[["b"]] <- -1L
    
  } else if(where == "left"){
    # Add column at l-1 (i.e. at l, and move existing columns to right)
    g <- gtable::gtable_add_cols(g, widths=g_new$widths, pos=ref_position[["l"]]-1)
    
    # This shifts the rest of the elements (including the reference element) to
    # the right by a number of rows, which means that we need an offset.
    offset[["l"]] <- offset[["r"]] <- length(g_new$widths)
    
  } else if(where == "right"){
    # Add column at r (i.e. at r+1, and move existing columns to the right).
    g <- gtable::gtable_add_cols(g, widths=g_new$widths, pos=ref_position[["r"]])
    
    # This does not shift the reference element to the right, which means that
    # the offset can is -1L.
    offset[["l"]] <- offset[["r"]] <- -1L
    
  } else {
    stop("Unknown where argument.")
  }

  # Re-establish position of reference element.
  ref_position <- .gtable_get_position(g=g, element=ref_element, where=where, partial_match=partial_match)

  for(ii in seq_len(nrow(g_new$layout))){
    
    # Find element name
    element_name <- g_new$layout$name[ii]
    
    # Find the element position.
    element_position <- .gtable_get_position(g=g_new, element=element_name)
    
    # Find the reference position of the similar-named element in g.
    sim_position <- .gtable_get_position(g=g, element=element_name, where=where,
                                         partial_match=partial_match)
    
    # Set new position. Note that element_position receives an offset of 1
    # because, position starts at 1, not 0, and we are only interest in the
    # internal shift for element_position with regard to the origin.
    new_position <- ref_position - offset - (element_position - 1L)
    
    # Align with similarly-named element in g. This only refers to vertical or
    # horizontal placement.
    if(where %in% c("top", "bottom")){
      new_position[["l"]] <- sim_position[["l"]]
      new_position[["r"]] <- sim_position[["r"]]
      
    } else {
      new_position[["t"]] <- sim_position[["t"]]
      new_position[["b"]] <- sim_position[["b"]]
    }
    
    # Update width (for inserted rows) or height (for inserted height).
    if(where %in% c("top", "bottom")){
      
      # Rows are inserted, and we need to update column width for new elements
      # that span a single column.
      if(new_position[["l"]] == new_position[["r"]]){
        if(!is.null(g_new$grobs[[ii]]$width)){
          g$widths[new_position[["l"]]] <- max(grid::unit.c(g$widths[new_position[["l"]]], g_new$grobs[[ii]]$width))
        }
      }
      
    } else {
      
      # Columns are inserted, and we need to update row heights for new elements
      # that span a single row.
      if(new_position[["t"]] == new_position[["b"]]){
        if(!is.null(g_new$grobs[[ii]]$height)){
          g$heights[new_position[["t"]]] <- max(grid::unit.c(g$heights[new_position[["t"]]], g_new$grobs[[ii]]$height))
        }
      }
    }
    
    # Add element to g.
    g <- gtable::gtable_add_grob(g,
                                 grobs=g_new$grobs[[ii]],
                                 t=new_position[["t"]],
                                 l=new_position[["l"]],
                                 b=new_position[["b"]],
                                 r=new_position[["r"]],
                                 name=element_name,
                                 clip=g_new$layout$clip[ii])
  }
  
  return(g)
}


.gtable_insert_along <- function(g, g_new, where="top", ref_element="panel",
                                 along_element=ref_element, spacer=NULL,
                                 partial_match_ref=FALSE,
                                 partial_match_along=FALSE){
  
  if(length(g_new) == 0){
    return(g)
    
  } else if(length(g_new) > 1){
    ..error_variable_has_too_many_values(x=g_new, var_name="g_new", req_length=1)
  }
  
  # Create an offset.
  offset <- integer(4)
  names(offset) <- c("t", "l", "b", "r")
  
  # Find position to insert this element
  ref_position <- .gtable_get_position(g=g, element=ref_element, where=where, partial_match=partial_match_ref)
  
  # Add spacing
  if(where %in% c("top", "bottom")){
    
    # Add space to top.
    if(!is.null(spacer$t)){
      g_new <- gtable::gtable_add_rows(g_new, heights=spacer$t, pos=0)
    }
    
    # Add space to bottom.
    if(!is.null(spacer$b)){
      g_new <- gtable::gtable_add_rows(g_new, heights=spacer$b, pos=-1)
    }
    
  } else {
    
    # Add space to left.
    if(!is.null(spacer$l)){
      g_new <- gtable::gtable_add_cols(g_new, widths=spacer$l, pos=0)
    }
    
    # Add space to right.
    if(!is.null(spacer$r)){
      g_new <- gtable::gtable_add_cols(g_new, widths=spacer$r, pos=-1)
    }
  }
  
  
  # Make room to insert the stuff.
  if(where == "top"){
    # Add row below t-1 (i.e. at t, and move existing rows down).
    g <- gtable::gtable_add_rows(g, heights=g_new$heights, pos=ref_position[["t"]]-1)
    
    # This shifts the rest of the elements (including the reference element)
    # down by a number of rows, which means that we need an offset.
    offset[["t"]] <- offset[["b"]] <- length(g_new$heights)
    
  } else if(where == "bottom"){
    # Add row below b (i.e. at b+1, and move existing rows down).
    g <- gtable::gtable_add_rows(g, heights=g_new$heights, pos=ref_position[["b"]])
    
    # This does not shift the reference element down, which means that the
    # offset is -1L.
    offset[["t"]] <- offset[["b"]] <- -1L
    
  } else if(where == "left"){
    # Add column at l-1 (i.e. at l, and move existing columns to right)
    g <- gtable::gtable_add_cols(g, widths=g_new$widths, pos=ref_position[["l"]]-1)
    
    # This shifts the rest of the elements (including the reference element) to
    # the right by a number of rows, which means that we need an offset.
    offset[["l"]] <- offset[["r"]] <- length(g_new$widths)
    
  } else if(where == "right"){
    # Add column at r (i.e. at r+1, and move existing columns to the right).
    g <- gtable::gtable_add_cols(g, widths=g_new$widths, pos=ref_position[["r"]])
    
    # This does not shift the reference element to the right, which means that
    # the offset can is -1L.
    offset[["l"]] <- offset[["r"]] <- -1L
    
  } else {
    stop("Unknown where argument.")
  }
  
  # Re-establish position of reference element.
  ref_position <- .gtable_get_position(g=g, element=ref_element, where=where, partial_match=partial_match_ref)
  
  # Find element name
  element_name <- g_new$layout$name[1]
  
  # Find the extent of the along_elements
  extent <- .gtable_get_extent(g=g, element=along_element, partial_match=partial_match_along)
  
  # Set new position
  new_position <- ref_position - offset
  
  if(where %in% c("top", "bottom")){
    new_position[["l"]] <- extent[["l"]]
    new_position[["r"]] <- extent[["r"]]
    
  } else {
    new_position[["t"]] <- extent[["t"]]
    new_position[["b"]] <- extent[["b"]]
  }
  
  # Update width (for inserted rows) or height (for inserted height).
  if(where %in% c("top", "bottom")){
    
    # Rows are inserted, and we need to update column width for new elements
    # that span a single column.
    if(new_position[["l"]] == new_position[["r"]]){
      if(!is.null(g_new$grobs[[1]]$width)){
        g$widths[new_position[["l"]]] <- max(grid::unit.c(g$widths[new_position[["l"]]], g_new$grobs[[1]]$width))
      }
    }
    
  } else {
    
    # Columns are inserted, and we need to update row heights for new elements
    # that span a single row.
    if(new_position[["t"]] == new_position[["b"]]){
      if(!is.null(g_new$grobs[[1]]$height)){
        g$heights[new_position[["t"]]] <- max(grid::unit.c(g$heights[new_position[["t"]]], g_new$grobs[[1]]$height))
      }
    }
  }
  
  
  # Add element to g.
  g <- gtable::gtable_add_grob(g,
                               grobs=g_new$grobs[[1]],
                               t=new_position[["t"]],
                               l=new_position[["l"]],
                               b=new_position[["b"]],
                               r=new_position[["r"]],
                               name=element_name,
                               clip=g_new$layout$clip[1])
  
  return(g)
}


.gtable_rename_element <- function(g, old, new){
  if(!.gtable_element_in_layout(g=g, element=old, partial_match=FALSE)){
    ..error_reached_unreachable_code(".gtable_rename_element: element not found in layout table.")
  }
  
  g$layout$name[g$layout$name == old] <- new
  
  return(g)
}

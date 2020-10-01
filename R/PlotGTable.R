.gtable_element_in_layout <- function(g, element, partial_match=FALSE){
  
  if(partial_match){
    return(any(grepl(pattern=element, x=g$layout$name)))
    
  } else {
    return(any(element %in% g$layout$name))
  }
}



.gtable_get_position <- function(g, element, where=NULL, partial_match=FALSE, allow_multiple=FALSE){
  
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
      
      if(allow_multiple){
        browser()
        
      } else {
        stop(paste0("Multiple matches, please set where attribute."))
      }
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
  
  if(length(g_new) == 0) return(g)

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


.gtable_insert_along <- function(g,
                                 g_new,
                                 where="top",
                                 ref_element="panel",
                                 along_element=ref_element,
                                 spacer=NULL,
                                 attempt_replace=FALSE,
                                 partial_match_ref=FALSE,
                                 partial_match_along=FALSE){
  
  # Intended for inserting elements that stretch multiple along_elements. It can
  # also be used for inserting elements directly (without along_elements) and/or
  # replacing existing elements (attempt_replace=TRUE)
  
  if(length(g_new) == 0){
    return(g)
    
  } else if(length(g_new) > 1){
    ..error_variable_has_too_many_values(x=g_new, var_name="g_new", req_length=1)
  }
  
  # Create an offset.
  offset <- integer(4)
  names(offset) <- c("t", "l", "b", "r")
  
  spacer_offset <- integer(4)
  names(spacer_offset) <- c("t", "l", "b", "r")
  
  # Find position to insert this element
  ref_position <- .gtable_get_position(g=g, element=ref_element, where=where, partial_match=partial_match_ref)
  
  # Add spacing
  if(where %in% c("top", "bottom")){
    
    # Add space to top.
    if(!is.null(spacer$t)){
      g_new <- gtable::gtable_add_rows(g_new, heights=spacer$t, pos=0)
      
      # This shifts the actual element downward.
      spacer_offset[["t"]] <- spacer_offset[["b"]] <- 1L
    }
    
    # Add space to bottom.
    if(!is.null(spacer$b)){
      g_new <- gtable::gtable_add_rows(g_new, heights=spacer$b, pos=-1)
    }
    
  } else {
    
    # Add space to left.
    if(!is.null(spacer$l)){
      g_new <- gtable::gtable_add_cols(g_new, widths=spacer$l, pos=0)
      
      # This shifts the actual element to the right.
      spacer_offset[["r"]] <- spacer_offset[["l"]] <- 1L
    }
    
    # Add space to right.
    if(!is.null(spacer$r)){
      g_new <- gtable::gtable_add_cols(g_new, widths=spacer$r, pos=-1)
    }
  }
  
  if(attempt_replace){

    # Find if there is a grob with the same name at the intended position.
    g_index <- .gtable_which_aligned(g,
                                     element=g_new$layout$name,
                                     ref_element=ref_element,
                                     where=where,
                                     partial_match_ref=partial_match_ref)
    
    if(!is.null(g_index)){
      # Replace the grob.
      g$grobs[[g_index]] <- g_new
      
      # Update heights and widths to get the accurate figures.
      g <- .gtable_update_layout(g)
      
      return(g)
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
  new_position <- ref_position + spacer_offset - offset
  
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



.gtable_which_aligned <- function(g,
                                  element,
                                  ref_element,
                                  where,
                                  partial_match_ref=FALSE,
                                  only_nearby=TRUE){
  
  # Identify the element that is located as close as possible to the reference
  # element, and is aligned with it.
  
  # Find position of the reference element.
  ref_position <- .gtable_get_position(g=g,
                                       element=ref_element,
                                       where=where,
                                       partial_match=partial_match_ref)
  
  # As a list
  ref_position <- as.list(ref_position)
  
  # Identify candidates
  if(where == "top"){
    # Any candidates should span the left-right extent of the reference element,
    # and be entirely above it.
    candidates <- which(g$layout$name == element &
                          g$layout$l == ref_position$l &
                          g$layout$r == ref_position$r &
                          g$layout$t < ref_position$t &
                          g$layout$b < ref_position$t)
    
  } else if(where == "bottom"){
    # Any candidates should span the left-right extent of the reference element,
    # and be entirely below it.
    candidates <- which(g$layout$name == element &
                          g$layout$l == ref_position$l &
                          g$layout$r == ref_position$r &
                          g$layout$t > ref_position$b &
                          g$layout$b > ref_position$b)
    
    
  } else if(where == "left"){
    # Any candidates should span the top-bottom extent of the reference element,
    # and be entirely to the left it.
    candidates <- which(g$layout$name == element &
                          g$layout$l < ref_position$l &
                          g$layout$r < ref_position$l &
                          g$layout$t == ref_position$t &
                          g$layout$b == ref_position$b)
    
    
  } else if(where == "right"){
    # Any candidates should span the top-bottom extent of the reference element,
    # and be entirely to the right it.
    candidates <- which(g$layout$name == element &
                          g$layout$l > ref_position$r &
                          g$layout$r > ref_position$r &
                          g$layout$t == ref_position$t &
                          g$layout$b == ref_position$b)
    
  } else {
    stop("Unknown where argument.")
  }
  
  if(length(candidates) == 0) return(NULL)
  
  if(length(candidates) > 1 & only_nearby){
    # Identify the candidate that is located nearest to the reference element.
    layout_table <- g$layout[candidates, ]
    
    if(where == "top"){
      distance <- ref_position$t - layout_table$t
      
    } else if(where == "bottom"){
      distance <- layout_table$b - ref_position$b
      
    } else if(where == "left") {
      distance <- ref_position$l - layout_table$l
      
    } else if(where == "right") {
      distance <- layout_table$r - ref_position$r
    }
    
    # Select the candidate with minimal distance.
    candidates <- candidates[which.min(distance)[1]]
  }
  
  return(candidates)
}
  


.gtable_rename_element <- function(g, old, new, partial_match=FALSE, allow_missing=FALSE){
  if(!.gtable_element_in_layout(g=g, element=old, partial_match=partial_match)){
    
    if(allow_missing) return(g)
    
    stop(".gtable_rename_element: element not found in layout table.")
  }
  
  if(partial_match){
    updated_element <- grepl(pattern=old, x=g$layout$name)
    
  } else {
    updated_element <- g$layout$name == old
  }
  
  if(sum(updated_element) > 1){
    warning(".gtable_rename_element: multiple elements will be updated.")
  }
  
  g$layout$name[updated_element] <- new
  
  return(g)
}



.gtable_update_layout <- function(g){
  
  ..get_height <- function(grob_id, g){
    
    # Identify the name of the grob.
    grob_name <- g$layout$name[grob_id]
    
    # Identify the height of the grob.
    if(grid::is.unit(g$grobs[[grob_id]]$heights)){
      grob_height <- g$grobs[[grob_id]]$heights
      
    } else if(grid::is.unit(g$grobs[[grob_id]]$height)){
      grob_height <- g$grobs[[grob_id]]$height
      
    } else {
      grob_height <- NULL
    }
    
    if(grid::is.unit(grob_height)){
      
      if(is.list(grob_height)){
        
        grob_height <- lapply(grob_height, function(current_grob_height){
          if(grid::unitType(current_grob_height) == "npc") current_grob_height <- grid::unit(as.numeric(current_grob_height), "null")
          return(current_grob_height)
        })
        
        # Sum the heights.
        if(length(grob_height) == 1){
          grob_height <- grob_height[[1]]
          
        } else {
          grob_height <- sum(do.call(grid::unit.c, args=grob_height))
        }
        
      } else if(grid::unitType(grob_height) == "npc"){
        grob_height <- grid::unit(as.numeric(grob_height), "null")
      }
    }
    
    return(grob_height)
  }
  
  
  ..get_width <- function(grob_id, g){
    
    # Identify the name of the grob.
    grob_name <- g$layout$name[grob_id]
    
    # Identify the width of the grob.
    if(grid::is.unit(g$grobs[[grob_id]]$widths)){
      grob_width <- g$grobs[[grob_id]]$widths
      
    } else if(grid::is.unit(g$grobs[[grob_id]]$width)){
      grob_width <- g$grobs[[grob_id]]$width
      
    } else {
      grob_width <- NULL
    }
    
    if(grid::is.unit(grob_width)){
      
      if(is.list(grob_width)){
        
        grob_width <- lapply(grob_width, function(current_grob_width){
          if(grid::unitType(current_grob_width) == "npc") current_grob_width <- grid::unit(as.numeric(current_grob_width), "null")
          return(current_grob_width)
        })
        
        # Sum the widths.
        if(length(grob_width) == 1){
          grob_width <- grob_width[[1]]
          
        } else {
          grob_width <- sum(do.call(grid::unit.c, args=grob_width))
        }
        
      } else if(grid::unitType(grob_width) == "npc"){
        grob_width <- grid::unit(as.numeric(grob_width), "null")
      } 
      
    }
    
    return(grob_width)
  }
  
  # Update heights
  new_heights <- replicate(grid::unit(0, "cm"), n=nrow(g), simplify=FALSE)
  new_heights <- do.call(grid::unit.c, args=new_heights)
  new_heights[1] <- grid::unit(1, "points")
  new_heights[nrow(g)] <- grid::unit(1, "points")
  
  for(ii in seq_len(nrow(g))){
    # Select candidates.
    candidates <- which(g$layout$t == ii & g$layout$b == ii)
    
    # Skip if there are no candidates.
    if(length(candidates) == 0) next()
    
    # Identify the height of the grobs.
    grob_heights <- lapply(candidates, ..get_height, g=g)
    
    # Remove all empty heights.
    grob_heights <- grob_heights[sapply(grob_heights, grid::is.unit)]
    if(length(grob_heights) == 0) next()
    
    # Remove all heights equal to 0.0.
    grob_heights <- grob_heights[sapply(grob_heights, function(grob_height) (as.numeric(grob_height) != 0.0))]
    if(length(grob_heights) == 0) next()
    
    # Select unique heights.
    grob_heights <- unique(grob_heights)
    
    # If the grob_heights are a mix of implicit and explicit heights, keep only
    # explicit.
    explicit_grob_heights <- sapply(grob_heights, grid::unitType) != "null"
    implicit_grob_heights <- sapply(grob_heights, grid::unitType) == "null"
    if(any(implicit_grob_heights) & !all(implicit_grob_heights)){
      grob_heights <- grob_heights[explicit_grob_heights]
    }
    
    if(length(grob_heights) == 1){
      # Set the grob height.
      new_heights[ii] <- grob_heights[[1]]
      
    } else if(length(grob_heights) > 1){
      # Take the max height of the row.
      grob_heights <- do.call(grid::unit.c, args=grob_heights)
      grob_heights <- max(grob_heights)
      
      # Set the grob height.
      new_heights[ii] <- grob_heights
    }
  }
  
  # Update heights
  new_widths <- replicate(grid::unit(0, "cm"), n=ncol(g), simplify=FALSE)
  new_widths <- do.call(grid::unit.c, args=new_widths)
  new_widths[1] <- grid::unit(1, "points")
  new_widths[ncol(g)] <- grid::unit(1, "points")
  
  for(ii in seq_len(ncol(g))){
    # Select candidates.
    candidates <- which(g$layout$l == ii & g$layout$r == ii)
    
    # Skip if there are no candidates.
    if(length(candidates) == 0) next()
    
    # Identify the width of the grobs.
    grob_widths <- lapply(candidates, ..get_width, g=g)
    
    # Remove all empty widths.
    grob_widths <- grob_widths[sapply(grob_widths, grid::is.unit)]
    if(length(grob_widths) == 0) next()
    
    # Remove all heights equal to 0.0.
    grob_widths <- grob_widths[sapply(grob_widths, function(grob_width) (as.numeric(grob_width) != 0.0))]
    if(length(grob_widths) == 0) next()
    
    # Select unique widths.
    grob_widths <- unique(grob_widths)
    
    # If the grob_widths are a mix of implicit and explicit width, keep only
    # explicit.
    explicit_grob_widths <- sapply(grob_widths, grid::unitType) != "null"
    implicit_grob_widths <- sapply(grob_widths, grid::unitType) == "null"
    if(any(implicit_grob_widths) & !all(implicit_grob_widths)){
      grob_widths <- grob_widths[explicit_grob_widths]
    }
    
    if(length(grob_widths) == 1){
      # Set the grob width.
      new_widths[ii] <- grob_widths[[1]]
      
    } else if(length(grob_widths) > 1){
      # Take the max width of the column.
      grob_widths <- do.call(grid::unit.c, args=grob_widths)
      grob_widths <- max(grob_widths)
      
      # Set the grob width
      new_widths[ii] <- grob_widths
    }
  }
  
  # Update heights and widths in the table.
  g$heights <- new_heights
  g$widths <- new_widths
  
  return(g)
}

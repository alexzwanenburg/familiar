.get_palette <- function(
    x = NULL,
    palette_type,
    n = 5L, 
    invert = FALSE,
    use_alternative = FALSE,
    diverge_to_white = FALSE
) {
  # Check whether the provided palette is known, a set of colours, or a default.
  if (is.null(x)) {
    colours <- .get_default_palette(
      n = n,
      palette_type = palette_type,
      invert = invert,
      use_alternative = use_alternative,
      diverge_to_white = diverge_to_white
    )
    
  } else if (all(is.character(x))) {
    if (length(x) > 1L) {
      # Check that all elements are colours.
      valid_colours <- sapply(x, .is_colour)

      if (!all(valid_colours)) {
        ..error(paste0(
          "The following palette colours could not be interpreted: ",
          paste_s(x[!valid_colours]),
          " . A valid colour is either a hexadecimal string (e.g. \"#4F94CD\"), ",
          "a colour specified in grDevices::colors() (e.g. \"steelblue3\"), ",
          "or \"transparant\". Alternatively, a palette can be specified by name."
        ))
      }

      colours <- x
      
    } else if (length(x) == 1L) {
      # Obtain colours from a predefined palette.
      colours <- .palette_to_colour(x = x, n = n)
    }
    
  } else {
    ..error(paste0(
      "The requested palette are neither colours nor a palette: ",
      paste_s(x)
    ))
  }

  return(colours)
}



.is_colour <- function(x) {
  return(
    x %in% grDevices::colors() ||
      x == "transparant" ||
      grepl(pattern = "^#(\\d|[a-f]){6,8}$", x, ignore.case = TRUE)
  )
}



.palette_to_colour <- function(x, n = 5L) {
  # Determine if the string ends with _, _r or _rev.
  invert_colours <- grepl(pattern = "_$|_r$|_rev$", x, ignore.case = TRUE)

  # Strip from x
  x <- gsub(pattern = "_$|_r$|_rev$", replacement = "", x)

  # Try grDevices::palette (requires R version >= 4.0.0)
  colours <- tryCatch(
    grDevices::palette.colors(n = n, palette = x),
    error = function(err) (NULL)
  )

  # Try grDevices::hcl.colors (requires R version >= 3.6.0)
  if (is.null(colours)) {
    colours <- tryCatch(
      grDevices::hcl.colors(n = n, palette = x),
      error = function(err) (NULL)
    )
  }

  # Palettes that are always available.
  if (is.null(colours)) {
    if (x == "default") {
      colours <- grDevices::palette()
    } else if (x == "rainbow") {
      colours <- grDevices::rainbow(n = n)
    } else if (x == "heat.colors") {
      colours <- grDevices::heat.colors(n = n)
    } else if (x == "terrain.colors") {
      colours <- grDevices::terrain.colors(n = n)
    } else if (x == "topo.colors") {
      colours <- grDevices::topo.colors(n = n)
    } else if (x == "cm.colors") {
      colours <- grDevices::cm.colors(n = n)
    }
  }

  if (is.null(colours)) {
    ..error(paste0(
      "The palette was not recognised: ", x,
      ". Please check the spelling. Note that some options may not be available prior ",
      "to R 4.0.0 (grDevices::palette.pals(), and R 3.6.0 (grDevices::hcl.pals()))."
    ))
  }

  if (invert_colours) {
    colours <- rev(colours)
  }

  return(colours)
}


.get_default_palette <- function(
    n,
    palette_type,
    invert,
    use_alternative = FALSE,
    diverge_to_white = FALSE
) {
  
  .check_parameter_value_is_valid(
    x = palette_type, var_name = "palette_type",
    values = c("qualitative", "sequential", "divergent")
  )

  if (palette_type == "qualitative") {
    # Default qualitative palettes are based on the Tableau 10 palette by
    # Maureen Stone, Cristy Miller and Jeffrey Heer.
    #
    # * Maureen Stone, Designing Colors for Data, International Symposium on
    # Computational Aesthetics in Graphics, Visualization, and Imaging, Banff,
    # AB, Canada, June 22, 2007.
    #
    # * Jeffrey Heer and Maureen Stone, Color Naming Models for Color Selection,
    # Image Editing and Palette Design, ACM Human Factors in Computing Systems,
    # 2012.
    #
    # * https://www.tableau.com/about/blog/2016/7/colors-upgrade-tableau-10-56782
    # 
    if (!use_alternative) {
      if (n <= 10L) {
        colours <- c(
          "#4e79a7",
          "#f28e2b",
          "#e15759",
          "#76b7b2",
          "#59a14f",
          "#edc948",
          "#b07aa1",
          "#ff9da7",
          "#9c755f",
          "#bab0ac"
        )[1L:n]
        
      } else if (n <= 20L) {
        colours <- c(
          "#4e79a7",
          "#a0cbe8",
          "#f28e2b",
          "#ffbe7d",
          "#59a14f",
          "#8cd17d",
          "#b6992d",
          "#f1ce63",
          "#499894",
          "#86bcb6",
          "#e15759",
          "#ff9d9a",
          "#79706e",
          "#bab0ac",
          "#d37295",
          "#fabfd2",
          "#b07aa1",
          "#d4a6c8",
          "#9d7660",
          "#d7b5a6"
        )[1L:n]
        
      } else {
        ..error(paste0(
          "The required number (", n, ") of discrete colors is too large for the ",
          "default qualitative score (max 20). "
        ))
      }
    } else {
      # Alternative colour schemes were the blue and orange colours come last.
      # This is to avoid confusion with other gradients that may be used in the
      # plot.
      if (n <= 10L) {
        colours <- c(
          "#e15759",
          "#76b7b2",
          "#59a14f",
          "#edc948",
          "#b07aa1",
          "#ff9da7",
          "#9c755f",
          "#bab0ac",
          "#4e79a7",
          "#f28e2b"
        )[1L:n]
        
      } else if (n <= 20L) {
        colours <- c(
          "#59a14f",
          "#8cd17d",
          "#b6992d",
          "#f1ce63",
          "#499894",
          "#86bcb6",
          "#e15759",
          "#ff9d9a",
          "#79706e",
          "#bab0ac",
          "#d37295",
          "#fabfd2",
          "#b07aa1",
          "#d4a6c8",
          "#9d7660",
          "#d7b5a6",
          "#4e79a7",
          "#a0cbe8",
          "#f28e2b",
          "#ffbe7d"
        )[1L:n]
        
      } else {
        ..error(paste0(
          "The required number (", n, ") of discrete colors is too large for ",
          "the default qualitative score (max 20). "
        ))
      }
    }
  } else if (palette_type == "sequential") {
    if (!use_alternative) {
      # A palette with the same hue (blue) as the first color of the qualitative
      # palette. Based on an advanced single-hue sequential palette created
      # using colorspace::hcl_wizard, with Hue 1=245, Chroma 1 = 15, Max Chroma
      # = 75, Lumin. 1 = 20, Lumin 2 = 98, Power 1 = 0.8, Power 2 = 1.4, without
      # color correction.
      colours <- c(
        "#233143", "#243950", "#26415D", "#27496A", "#285177", "#295A85",
        "#296393", "#2A6CA2", "#2A76B1", "#297FC0", "#2D89CF", "#4D93D2",
        "#649CD6", "#79A7DA", "#8CB1DF", "#9FBDE3", "#B2C9E8", "#C6D6ED",
        "#DCE5F2", "#F9F9F9"
      )
    } else {
      # Alternative reddish colour scheme that avoids the use of blues and
      # orange tones that may have been used as a primary palette. Based on an
      # advanced single-hue sequential palette created using
      # colorspace::hcl_wizard, with Hue=12, Chroma 1 = 40, Max Chroma = 120,
      # Lumin. 1 = 20, Lumin 2 = 98, Power 1 = 0.8, Power 2 = 1.4, without color
      # correction.
      colours <- c(
        "#581B1C", "#661F1F", "#732323", "#812727", "#8F2B2C", "#9E2F30",
        "#AC3434", "#BC3839", "#CC3D3E", "#DC4243", "#E25354", "#E76364",
        "#EC7374", "#F18383", "#F59394", "#F8A4A4", "#FBB5B6", "#FCC8C8",
        "#FCDDDD", "#F9F9F9"
      )
    }

    if (invert) colours <- rev(colours)
    
  } else if (palette_type == "divergent") {
    if (!use_alternative) {
      # A palette with the same hues (blue and orange) as the first two colors of
      # the qualitative palette. Based on a combination of two advanced single-hue
      # sequential palettes created using colorspace::hcl_wizard:
      #
      # Blues: Hue 1=245, Chroma 1 = 15, Max Chroma = 75, Lumin. 1 = 20, Lumin 2 =
      # 98, Power 1 = 0.8, Power 2 = 1.4, without color correction. Oranges: Hue
      # 1=36, Chroma 1 = 20, Max Chroma = 100, Lumin. 1 = 35, Lumin 2 = 98, Power
      # 1 = 0.8, Power 2 = 1.4, without color correction, in reverse order.
      if (!diverge_to_white) {
        # Centre colour is white.
        colours <- c(
          "#233143", "#243950", "#26415D", "#27496A", "#285177", "#295A85",
          "#296393", "#2A6CA2", "#2A76B1", "#297FC0", "#2D89CF", "#4D93D2",
          "#649CD6", "#79A7DA", "#8CB1DF", "#9FBDE3", "#B2C9E8", "#C6D6ED", "#DCE5F2",
          "#F9F9F9",
          "#FAE4DA", "#F9D4C3", "#F8C6AE", "#F5B999", "#F2AD85", "#EFA170",
          "#EB965A", "#E78C40", "#E28118", "#D67B1E", "#C87528", "#BC702F",
          "#AF6A34", "#A36537", "#96603A", "#8A5B3C", "#7E563E", "#725240", "#664D41"
        )
      } else {
        # Centre colour is dark.
        colours <- c(
          "#FAE4DA", "#F9D4C3", "#F8C6AE", "#F5B999", "#F2AD85", "#EFA170",
          "#EB965A", "#E78C40", "#E28118", "#D67B1E", "#C87528", "#BC702F",
          "#AF6A34", "#A36537", "#96603A", "#8A5B3C", "#7E563E", "#725240", "#664D41",
          "#050505",
          "#233143", "#243950", "#26415D", "#27496A", "#285177", "#295A85",
          "#296393", "#2A6CA2", "#2A76B1", "#297FC0", "#2D89CF", "#4D93D2",
          "#649CD6", "#79A7DA", "#8CB1DF", "#9FBDE3", "#B2C9E8", "#C6D6ED", "#DCE5F2"
        )
      }
    } else {
      # A palette based on the same hues the first two colours of the
      # alternative qualitative palette. Based on a combination of two advanced
      # single-hue sequential palettes created using colorspace::hcl_wizard:
      #
      # Reds: Hue=12, Chroma 1 = 40, Max Chroma = 120, Lumin. 1 = 20, Lumin 2 =
      # 98, Power 1 = 0.8, Power 2 = 1.4, without color correction. Cyan:
      # Hue=185, Chroma 1 = 0, Max Chroma = 55, Lumin. 1 = 40, Lumin 2 = 98,
      # Power 1 = 1.0, Power 2 = 1.4, without color correction.
      if (!diverge_to_white) {
        # Centre colour is white.
        colours <- c(
          "#581B1C", "#661F1F", "#732323", "#812727", "#8F2B2C", "#9E2F30",
          "#AC3434", "#BC3839", "#CC3D3E", "#DC4243", "#E25354", "#E76364",
          "#EC7374", "#F18383", "#F59394", "#F8A4A4", "#FBB5B6", "#FCC8C8", "#FCDDDD",
          "#F9F9F9",
          "#E8F3F2", "#D7ECEA", "#C5E6E3", "#B2DFDC", "#9FD9D4", "#8AD3CD",
          "#72CCC6", "#56C6BF", "#2DC0B8", "#16B7B0", "#30ADA6", "#3EA39D",
          "#489994", "#4F8F8B", "#548582", "#587B79", "#5B7270", "#5D6867", "#5E5E5E"
        )
      } else {
        # Centre colour is black.
        colours <- c(
          rev(c(
            "#E8F3F2", "#D7ECEA", "#C5E6E3", "#B2DFDC", "#9FD9D4", "#8AD3CD",
            "#72CCC6", "#56C6BF", "#2DC0B8", "#16B7B0", "#30ADA6", "#3EA39D",
            "#489994", "#4F8F8B", "#548582", "#587B79", "#5B7270", "#5D6867", "#5E5E5E"
          )),
          "#050505",
          rev(c(
            "#581B1C", "#661F1F", "#732323", "#812727", "#8F2B2C", "#9E2F30",
            "#AC3434", "#BC3839", "#CC3D3E", "#DC4243", "#E25354", "#E76364",
            "#EC7374", "#F18383", "#F59394", "#F8A4A4", "#FBB5B6", "#FCC8C8", "#FCDDDD"
          ))
        )
      }
    }

    if (invert) colours <- rev(colours)
  }

  return(colours)
}

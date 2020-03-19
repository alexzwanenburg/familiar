plotting.get_palette <- function(x=NULL, palette_type, n=5, invert=FALSE){
                                  
  # Check whether the provided palette is known, a set of colors, or a default.
  if(is.null(x)){
    colours <- plotting.get_default_palette(n=n, palette_type=palette_type, invert=invert)
    
  } else if(all(is.character(x))){
    if(length(x) > 1){
      # Check that all elements are colours.
      valid_colours <- sapply(x, plotting.is_color)
      
      if(any(!valid_colours)){
        stop(paste0("The following palette colours could not be interpreted: ",
                    paste0(x[!valid_colours], collapse=", "),
                    " . A valid colour is either a hexadecimal string (e.g. \"#4F94CD\"), ",
                    "a colour specified in grDevices::colors() (e.g. \"steelblue3\"), ",
                    "or \"transparant\". Alternatively, a palette can be specified by name."))
      }
      
      colours <- x
      
    } else if(length(x) == 1){
      
      # Obtain colours from a predefined palette.
      colours <- plotting.palette_to_colour(x=x, n=n)
    }
    
  } else {
    stop(paste0("The requested palette are neither colours nor a palette: ",
                paste0(x, collapse=", ")))
  }
  
  return(colours)
}



plotting.is_color <- function(x) {
  return(x %in% grDevices::colors() | x=="transparant" | grepl("^#(\\d|[a-f]){6,8}$", x, ignore.case=TRUE))
}



plotting.palette_to_colour <- function(x, n=5){
  
  # Determine if the string ends with _, _r or _rev.
  invert_colours <- grepl(pattern="_$|_r$|_rev$", x, ignore.case=TRUE)
  
  # Strip from x
  x <- gsub(pattern="_$|_r$|_rev$", replacement="", x)
  
  # Try grDevices::palette (requires R version >= 4.0.0)
  colours <- tryCatch(grDevices::palette.colors(n=n, palette=x),
                      error=function(err) return(NULL))
  
  # Try grDevices::hcl.colors (requires R version >= 3.6.0)
  if(is.null(colours)){
    colours <- tryCatch(grDevices::hcl.colors(n=n, palette=x),
                        error=function(err) return(NULL))
  }
  
  # Palettes that are always available.
  if(is.null(colours)){
    if(x == "default"){
      colours <- grDevices::palette()
      
    } else if(x == "rainbow"){
      colours <- grDevices::rainbow(n=n)
      
    } else if(x == "heat.colors"){
      colours <- grDevices::heat.colors(n=n)
      
    } else if(x == "terrain.colors"){
      colours <- grDevices::terrain.colors(n=n)
      
    } else if(x == "topo.colors"){
      colours <- grDevices::topo.colors(n=n)
      
    } else if(x == "cm.colors"){
      colours <- grDevices::cm.colors(n=n)
    }
  }
  
  if(is.null(colours)){
    stop(paste0("The palette was not recognised: ", x,
                ". Please check the spelling. Note that some options may not be available prior ",
                "to R 4.0.0 (grDevices::palette.pals(), and R 3.6.0 (grDevices::hcl.pals()))."))
  }
  
  if(invert_colours){
    colours <- rev(colours)
  }
  
  return(colours)
}


plotting.get_default_palette <- function(n, palette_type, invert){
  
  .check_parameter_value_is_valid(x=palette_type, var_name="palette_type",
                                  values=c("qualitative", "sequential", "divergent"))
  
  if(palette_type == "qualitative"){
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
    
    if(n <= 10){
      colours <- c("#4e79a7",
                   "#f28e2b",
                   "#e15759",
                   "#76b7b2",
                   "#59a14f",
                   "#edc948",
                   "#b07aa1",
                   "#ff9da7",
                   "#9c755f",
                   "#bab0ac")[1:n]
      
    } else if(n <= 20){
      colours <- c("#4e79a7",
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
                   "#d7b5a6")[1:n]
      
    } else {
      stop(paste0("The required number (",n ,") of discrete colors is too large for the default qualitative score (max 20). "))
    }
    
  } else if(palette_type == "sequential"){
    # A palette with the same hue (blue) as the first color of the qualitative
    # palette. Based on an advanced single-hue sequential palette created using
    # colorspace::hcl_wizard, with Hue 1=245, Chroma 1 = 15, Max Chroma = 75,
    # Lumin. 1 = 20, Lumin 2 = 98, Power 1 = 0.8, Power 2 = 1.4, without color correction.
    colours <- c("#233143", "#243950", "#26415D", "#27496A", "#285177", "#295A85",
                 "#296393", "#2A6CA2", "#2A76B1", "#297FC0", "#2D89CF", "#4D93D2",
                 "#649CD6", "#79A7DA", "#8CB1DF", "#9FBDE3", "#B2C9E8", "#C6D6ED",
                 "#DCE5F2", "#F9F9F9")
    
    if(invert) colours <- rev(colours)
    
  } else if(palette_type == "divergent"){
    # A palette with the same hues (blue and orange) as the first two colors of
    # the qualitative palette. Based on a combination of two advanced single-hue
    # sequential palettes created using colorspace::hcl_wizard:
    #
    # Blues: Hue 1=245, Chroma 1 = 15, Max Chroma = 75, Lumin. 1 = 20, Lumin 2 =
    # 98, Power 1 = 0.8, Power 2 = 1.4, without color correction. Oranges: Hue
    # 1=36, Chroma 1 = 20, Max Chroma = 100, Lumin. 1 = 35, Lumin 2 = 98, Power
    # 1 = 0.8, Power 2 = 1.4, without color correction, in reverse order.
    colours <- c("#233143", "#243950", "#26415D", "#27496A", "#285177", "#295A85",
                 "#296393", "#2A6CA2", "#2A76B1", "#297FC0", "#2D89CF", "#4D93D2",
                 "#649CD6", "#79A7DA", "#8CB1DF", "#9FBDE3", "#B2C9E8", "#C6D6ED", "#DCE5F2",
                 "#F9F9F9",
                 "#FAE4DA", "#F9D4C3", "#F8C6AE", "#F5B999", "#F2AD85", "#EFA170",
                 "#EB965A", "#E78C40", "#E28118", "#D67B1E", "#C87528", "#BC702F",
                 "#AF6A34", "#A36537", "#96603A", "#8A5B3C", "#7E563E", "#725240", "#664D41")
    
    if(invert) colours <- rev(colours)
  }
  
  return(colours)
}


# plotting.get_pallette <- function(use_palette=NULL, n, discrete=TRUE){
#   
#   if(is.null(use_palette) & discrete & n <= 8){
#     # Return default discrete palette
#     return(c("#666666", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")[1:n])
#     
#   } else if(is.null(use_palette) & (!discrete | (discrete & n > 8))){
#     # Return default continuous palette
#     return(grDevices::heat.colors(n))
#   }
#   
#   if(!is.character(use_palette)){
#     stop("Please use hexadecimal strings or color names to specify the palette.")
#   }
#   
#   # Brewer scales
#   if(length(use_palette)==1 & is_package_installed(name="RColorBrewer")){
#     available_palettes <- rownames(RColorBrewer::brewer.pal.info)
#     if(use_palette %in% available_palettes){
#       return(RColorBrewer::brewer.pal(n=n, name=use_palette))
#     }
#   }
#   
#   # Wes Anderson scales
#   if(length(use_palette)==1 & is_package_installed(name="wesanderson")){
#     available_palettes <- names(wesanderson::wes_palettes)
#     if(use_palette %in% available_palettes){
#       return(wesanderson::wes_palette(use_palette, n, type=ifelse(discrete, "discrete", "continuous")))
#     }
#   }
#   
#   # Viridis scales (viridisLite)
#   if(length(use_palette)==1 & is_package_installed(name="viridisLite", verbose=FALSE)){
#     if(use_palette %in% c("viridis_a", "viridis_b", "viridis_c", "viridis_d", "viridis_e")){
#       return(viridisLite::viridis(n=n, option=toupper(gsub(pattern="viridis", replacement="", x=use_palette))))
#     } else if(use_palette == "magma"){
#       return(viridisLite::magma(n=n))
#     } else if(use_palette == "inferno"){
#       return(viridisLite::inferno(n=n))
#     } else if(use_palette == "plasma"){
#       return(viridisLite::plasma(n=n))
#     } else if(use_palette == "viridis"){
#       return(viridisLite::viridis(n=n))
#     } else if(use_palette == "cividis"){
#       return(viridisLite::cividis(n=n))
#     }
#   }
#   
#   # Discrete ggsci palettes
#   if(length(use_palette)==1 & is_package_installed(name="ggsci") & discrete){
#     if(use_palette == "npg"){
#       return(ggsci::pal_npg()(n))
#     } else if(use_palette == "aaas"){
#       return(ggsci::pal_aaas()(n))
#     } else if(use_palette == "nejm"){
#       return(ggsci::pal_nejm()(n))
#     } else if(use_palette == "lancet"){
#       return(ggsci::pal_lancet()(n))
#     } else if(use_palette == "jama"){
#       return(ggsci::pal_jama()(n))
#     } else if(use_palette == "jco"){
#       return(ggsci::pal_jco()(n))
#     } else if(use_palette == "ucscgb"){
#       return(ggsci::pal_ucscgb()(n))
#     } else if(use_palette == "d3_category10"){
#       return(ggsci::pal_d3("category10")(n))
#     } else if(use_palette == "d3_category20"){
#       return(ggsci::pal_d3("category20")(n))
#     } else if(use_palette == "d3_category20b"){
#       return(ggsci::pal_d3("category20b")(n))
#     } else if(use_palette == "d3_category20c"){
#       return(ggsci::pal_d3("category20c")(n))
#     } else if(use_palette == "locuszoom"){
#       return(ggsci::pal_locuszoom()(n))
#     } else if(use_palette %in% c("igv", "igv_default")){
#       return(ggsci::pal_igv()(n))
#     } else if(use_palette == "igv_alternating"){
#       return(ggsci::pal_igv("alternating")(n))
#     } else if(use_palette %in% c("uchicago", "uchicago_default")){
#       return(ggsci::pal_uchicago()(n))
#     } else if(use_palette == "uchicago_light"){
#       return(ggsci::pal_uchicago("light")(n))
#     } else if(use_palette == "uchicago_dark"){
#       return(ggsci::pal_uchicago("dark")(n))
#     } else if(use_palette == "startrek"){
#       return(ggsci::pal_startrek()(n))
#     } else if(use_palette == "tron"){
#       return(ggsci::pal_tron(n))
#     } else if(use_palette == "futurama"){
#       return(ggsci::pal_futurama()(n))
#     } else if(use_palette == "rickandmorty"){
#       return(ggsci::pal_rickandmorty()(n))
#     } else if(use_palette == "simpsons"){
#       return(ggsci::pal_simpsons()(n))
#     }
#   }
#   
#   # Continuous ggsci palettes
#   if(length(use_palette)==1 & is_package_installed(name="ggsci")){
#     if(use_palette == "gsea"){
#       return(ggsci::pal_gsea(n=n)(n))
#     } else if(use_palette == "material_red"){
#       return(ggsci::pal_material("red", n=n)(n))
#     } else if(use_palette == "material_pink"){
#       return(ggsci::pal_material("pink", n=n)(n))
#     } else if(use_palette == "material_purple"){
#       return(ggsci::pal_material("purple", n=n)(n))
#     } else if(use_palette == "material_deep_purple"){
#       return(ggsci::pal_material("deep-purple", n=n)(n))
#     } else if(use_palette == "material_indigo"){
#       return(ggsci::pal_material("indigo", n=n)(n))
#     } else if(use_palette == "material_blue"){
#       return(ggsci::pal_material("blue", n=n)(n))
#     } else if(use_palette == "material_light_blue"){
#       return(ggsci::pal_material("light-blue", n=n)(n))
#     } else if(use_palette == "material_cyan"){
#       return(ggsci::pal_material("cyan", n=n)(n))
#     } else if(use_palette == "material_teal"){
#       return(ggsci::pal_material("teal", n=n)(n))
#     } else if(use_palette == "material_green"){
#       return(ggsci::pal_material("green", n=n)(n))
#     } else if(use_palette == "material_light_green"){
#       return(ggsci::pal_material("light-green", n=n)(n))
#     } else if(use_palette == "material_lime"){
#       return(ggsci::pal_material("lime", n=n)(n))
#     } else if(use_palette == "material_yellow"){
#       return(ggsci::pal_material("yellow", n=n)(n))
#     } else if(use_palette == "material_amber"){
#       return(ggsci::pal_material("amber", n=n)(n))
#     } else if(use_palette == "material_orange"){
#       return(ggsci::pal_material("orange", n=n)(n))
#     } else if(use_palette == "material_deep_orange"){
#       return(ggsci::pal_material("deep-orange", n=n)(n))
#     } else if(use_palette == "material_brown"){
#       return(ggsci::pal_material("brown", n=n)(n))
#     } else if(use_palette %in% c("material_grey", "material_gray")){
#       return(ggsci::pal_material("grey", n=n)(n))
#     } else if(use_palette %in% c("material_blue_grey", "material_blue_gray")){
#       return(ggsci::pal_material("blue-grey", n=n)(n))
#     }
#   }
#   
#   # Dichromatic scales (dichromat)
#   if(length(use_palette)==1 & is_package_installed(name="dichromat", verbose=FALSE)){
#     available_palettes <- names(dichromat::colorschemes)
#     if(use_palette %in% available_palettes){
#       return(dichromat::colorschemes[[use_palette]])
#     }
#   }
#   
#   # grDevice scales
#   if(length(use_palette)==1){
#     if(use_palette=="rainbow"){
#       return(grDevices::rainbow(n=n))
#     } else if(use_palette=="heat.colors"){
#       return(grDevices::heat.colors(n=n))
#     } else if(use_palette=="terrain.colors"){
#       return(grDevices::terrain.colors(n=n))
#     } else if(use_palette=="topo.colors"){
#       return(grDevices::topo.colors(n=n))
#     } else if(use_palette=="cm.colors"){
#       return(grDevices::cm.colors(n=n))
#     }
#   }
#   
#   # Check for user-defined palette
#   if(plotting.is_color(x=use_palette)){
#     return(use_palette)
#   }
#   
#   stop("The requested palette could not be found.")
# }

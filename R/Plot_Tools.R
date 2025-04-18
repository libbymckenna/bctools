# This script contains code for plot themes and the BC color palette

########################################################################################################################*

# BC COLOR LIST ----
#'
#' @export
#'
# These are all the colors from the brand guide.
bc_colors <- c(purple = "#332a86",
               blue = "#009ed1",
               teal = "#27bdbe",
               green = "#76b043",
               cool_dark_gray = "#43525a",
               warm_gray = "#8f9e96",
               cool_gray = "#c0cac7",
               navy = "#0a3049",
               dark_blue = "#2e83b7",
               cyan = "#6dcff6",
               light_blue = "#c7eafb",
               dark_green = "#478a57",
               light_green = "#bed73b",
               warm_light_gray = "#f1f2f2",
               light_yellow = "#fffcd5",
               bright_red = "#ef4123",
               red = "#b84626",
               orange = "#f58220",
               yellow = "#ffc233",
               brown = "#472a14",
               light_brown = "#b38f6b",
               charcoal = "#444d3e",
               # These are other tints of some colors for use in plots. (To find tints: https://www.colorhexa.com/)
               light_purple = "#aba5e3",
               lighter_green = "#A8D085",
               light_red = "#de7d62")

########################################################################################################################*

# Custom color palette code from: https://www.r-bloggers.com/2018/02/creating-corporate-colour-palettes-for-ggplot2/
# Function to pull the hex codes from the list for use below
extract_hex <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (bc_colors)
  bc_colors[cols]
}

########################################################################################################################*

# PALETTE SELECTION ----
# Additional color palettes can be added here using a combo of the color names above. Try to ensure all palettes are
# color blind friendly using this website:
# https://projects.susielu.com/viz-palette?colors=[%22#332a86%22,%22#ffc233%22]&backgroundColor=%22white%22&fontColor=%22black%22&mode=%22normal%22
bc_color_palettes <- list(primary = extract_hex("purple", "orange", "dark_green", "teal",
                                                        "light_purple", "yellow", "lighter_green", "dark_blue",
                                                        "light_red", "cool_gray"),
                          fourcolor = extract_hex("purple", "orange", "dark_green", "teal"),
                          rainbow = extract_hex("bright_red", "orange", "yellow", "dark_green", "teal", "purple"),
                          bigrainbow = extract_hex("light_red", "bright_red", "orange", "yellow", "lighter_green",
                                                   "dark_green", "teal", "dark_blue", "light_purple", "purple", "cool_gray"))

########################################################################################################################*

# Use code from stack overflow to make a color palette that can interpolate additional colors but maintain the order of
# the color palette: https://stackoverflow.com/questions/61674217/custom-discrete-color-scale-in-ggplot-does-not-respect-order
# See answer from linog (https://stackoverflow.com/users/9197726/linog)
# Need to add ",drop=FALSE" to the line "if (n<nrow(colors)) colors <- colors[1:n,]" for it to work for one color

colorRamp_d <- function (colors, n,
                         bias = 1,
                         space = c("rgb", "Lab"),
                         interpolate = c("linear",
                                         "spline"),
                         alpha = FALSE){

  # PRELIMINARY STEPS
  if (bias <= 0)
    stop("'bias' must be positive")
  if (!missing(space) && alpha)
    stop("'alpha' must be false if 'space' is specified")
  colors <- t(col2rgb(colors, alpha = alpha)/255)
  space <- match.arg(space)
  interpolate <- match.arg(interpolate)

  # CUT THE COLOR VECTOR

  if (space == "Lab")
    colors <- convertColor(colors, from = "sRGB", to = "Lab")
  interpolate <- switch(interpolate, linear = stats::approxfun,
                        spline = stats::splinefun)

  # RESPECT ORDER IF NCLASSES<NCOLORS
  if (n<nrow(colors)) colors <- colors[1:n,,drop=FALSE]

  if ((nc <- nrow(colors)) == 1L) {
    colors <- colors[c(1L, 1L), ]
    nc <- 2L
  }
  x <- seq.int(0, 1, length.out = nc)^bias
  palette <- c(interpolate(x, colors[, 1L]), interpolate(x,
                                                         colors[, 2L]), interpolate(x, colors[, 3L]), if (alpha) interpolate(x,
                                                                                                                             colors[, 4L]))
  roundcolor <- function(rgb) pmax(pmin(rgb, 1), 0)
  if (space == "Lab")
    function(x) roundcolor(convertColor(cbind(palette[[1L]](x),
                                              palette[[2L]](x), palette[[3L]](x), if (alpha)
                                                palette[[4L]](x)), from = "Lab", to = "sRGB")) *
    255
  else function(x) roundcolor(cbind(palette[[1L]](x), palette[[2L]](x),
                                    palette[[3L]](x), if (alpha)
                                      palette[[4L]](x))) * 255
}


colorRampPalette_d <- function (colors, ...){
  # n: number of classes
  function(n) {
    ramp <- colorRamp_d(colors, n, ...)
    x <- ramp(seq.int(0, 1, length.out = n))
    if (ncol(x) == 4L)
      rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
    else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
  }
}

# continued from the Rbloggers link, but use the new function to fix the color ramp

bc_pal <- function(palette = "primary", reverse = FALSE, ...) {
  pal <- bc_color_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  colorRampPalette_d(pal, ...)
}

########################################################################################################################*
# GGPLOT COLOR FUNCTIONS ----
#' @title BC Color Palette with scale_color
#'
#' @param palette Name of color palette to use. Current options: "primary", "rainbow"
#' @param discrete Set equal to false if gradient is desired
#' @param reverse Set equal to TRUE if you want the colors in the opposite order
#'
#' @export
#'
scale_color_bc <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bc_pal(palette = palette, reverse = reverse)
  if (discrete) {
    # the paste0 argument is for error messages
    discrete_scale("colour", paste0("bc_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# GGPLOT CORROSION FUNCTIONS ----
#' @title Corrosion Index Ranges
#' @description
#' Automatically add typical corrosion ranges to plots. Note: MUST use data argument, MUST use index_column argument. See example.
#' Ranges are based on this corrosion write up, already QC'd by Damon Roth (access to doc for BC employees only)
#' https://brwncald-my.sharepoint.com/:w:/p/lmckenna/Ebhc5hMxdZpHq5aKyoaGijQBheBR0EYLVtEEMJFaXpnARw?e=kRv8dX
#' @param data Data frame containing a column of named corrosion/scaling indices
#' @param alpha Set the opacity of the rectangles, default is 0.3
#' @param fill Set the color of the rectangles, default is "cyan"
#' @param index_column Name of the column in the plot data frame with corrosion/scaling indices names (should be the column used for facetting)
#'
#' @examples
#' library(tidywater)
#' corr_plot <- water_df %>%
#'   define_water_chain() %>%
#'   calculate_corrosion_once() %>%
#'   pivot_longer(aggressive:csmr, names_to = "index", values_to = "result")
#'
#' ggplot(corr_plot, aes(x = ph, y = result)) +
#'   geom_point() +
#'   facet_wrap(~index) +
#'   geom_corrosion_ranges(data = corr_plot, index_column = "index")
#'
#' @export
#'
geom_corrosion_ranges <- function(data,
                              alpha = 0.3,
                              fill = "cyan",
                              xmin = -Inf,
                              xmax = Inf,
                              index_column, ...) {

  plot_data <- data %>%
    select(!!sym(index_column)) %>%
    unique() %>% # this makes sure there is only one rectangle per index. Otherwise multiple rectangles are drawn and alpha doesn't work well
    mutate( ymin= case_when( grepl("lange|Lange|LSI", !!sym(index_column)) ~ -0.5,
                             grepl("ryz|Ryz|RI",  !!sym(index_column)) ~ 6,
                             grepl("ccpp|CCPP|Calcium C",  !!sym(index_column)) ~ 4,
                             grepl("larso|Lars|LI",  !!sym(index_column)) ~ -Inf,
                             grepl("csmr|CSMR|Chloride",  !!sym(index_column)) ~ -Inf,
                             grepl("agg|Agg|AI",  !!sym(index_column)) ~ 12),
            ymax =case_when( grepl("lange|Lange|LSI",  !!sym(index_column)) ~ 0.5,
                             grepl("ryz|Ryz|RI",  !!sym(index_column)) ~ 7,
                             grepl("ccpp|CCPP|Calcium C",  !!sym(index_column)) ~ 10,
                             grepl("larso|Lars|LI",  !!sym(index_column)) ~ 5,
                             grepl("csmr|CSMR|Chloride",  !!sym(index_column)) ~ 0.6,
                             grepl("agg|Agg|AI",  !!sym(index_column)) ~ Inf))

return(geom_rect(data = plot_data, aes(ymin = ymin, ymax = ymax),
                 xmin = xmin, xmax = xmax, alpha = alpha, fill = fill, inherit.aes = FALSE))

}

#######################################################################################################################*
#' BC Color Palette with scale_fill
#'
#' @param palette Name of color palette to use. Current options: "primary", "rainbow"
#' @param discrete Set equal to false if gradient is desired
#' @param reverse Set equal to TRUE if you want the colors in the opposite order
#'
#' @export
#'
scale_fill_bc <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bc_pal(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("fill", paste0("bc_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


########################################################################################################################*
########################################################################################################################*
########################################################################################################################*

# BOX AND WHISKER ----

#' Box and Whisker Plot function that uses 25th and 75th percentile for the box and 5th and 95th percentiles for whiskers.
#' The middle point can be at the mean or 90th percentile.
#' Outlier points can be included or excluded.  To set fill to a single color, include the argument fill =.
#'
#'
#' @param outlier Can be set to FALSE to remove points <5th percentile and >95th percentile.
#' @param count Can be set to FALSE to remove count from below the whisker.
#' @param middlepoint Can be equal to "mean" (default) or "90th" to select location, can be set to FALSE to remove.
#' @param whiskerbar Can be set to FALSE to remove horizontal bars on the ends of the whiskers.
#' @param alpha Can be set to a different transparency or NA if an alpha scale is needed (not recommended).
#' @param width Can be set to a number between 0 and 1, with lower numbers narrowing boxes.
#' @param fontsize Can be used to adjust the size of the count. Use a number equivalent to a standard size in points.
#' @param whiskerloc Can be used to adjust the percentile that the whiskers extend to, use the low value, high will be calculated.
#' @param countlabel Can be set to TRUE if you want the count to appear as n=#
#' @param preserve Can be set to "single" or "total". See position_dodge documentation for the difference.
#' @param padding Sets the spacing between boxes. Works best between 0.1 and 0.5
#'
#' @export
#'
geom_boxandwhisker <- function (outlier = TRUE, count = TRUE, middlepoint = "mean", whiskerbar = TRUE,
                                alpha = .8, width = .9, fontsize = 9, whiskerloc = .05, countlabel = FALSE,
                                preserve = "single", padding = .1,
                                whiskerlabel = FALSE, boxedgelabel = FALSE, medianlabel = FALSE, ...) { # haven't added this functionality yet

  # Box and Whiskers - these functions set up for the stat_summary functions
  lowwhisker <- whiskerloc
  hiwhisker <- 1 - whiskerloc

  # Quantiles for the boxplot function
  boxplot_info <- function(x) {
    r <- quantile(x, probs = c(lowwhisker, 0.25, 0.5, 0.75, hiwhisker))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
  }

  # For the horizontal bars on the ends of the whiskers (errorbar)
  low_bar <- function(x) {
    range <- quantile(x, probs = c(lowwhisker, 0.25))
    names(range) <- c("ymin", "ymax")
    range
  }
  high_bar <- function(x) {
    range <- quantile(x, probs = c(0.75, hiwhisker))
    names(range) <- c("ymin", "ymax")
    range
  }

  # For the outlier points (point)
  outlier_points <- function(x) {
    outliers <- subset(x, x < quantile(x,lowwhisker) | quantile(x,hiwhisker) < x)
    fillpoint <- unname(quantile(x, .5))
    if(length(outliers) == 0){
      data.frame(y = fillpoint, colour = NA)
    } else {
      data.frame(y = outliers, colour = rep("black", length(outliers)))
    }
  }


  # For the count (text). Location changes depending on whether outlier points are included.
  if (outlier == TRUE){
    ncount <- function(x){
      if (countlabel == TRUE) {
        return(data.frame(y = min(x), label = paste0("n=",length(x))))
      } else {
        return(data.frame(y = min(x), label = length(x)))
      }
    }
  } else if (outlier == FALSE){
    ncount <- function(x){
      if (countlabel == TRUE) {
        return(data.frame(y = as.numeric(quantile(x, lowwhisker)), label = paste0("n=",length(x))))
      } else {
        return(data.frame(y = as.numeric(quantile(x, lowwhisker)), label = length(x)))
      }
    }
  }




  # Returns a list of stat_summary for the full plot
  list(
    # This makes the actual box and whisker
    if(!is.na(alpha))
    ggplot2::stat_summary(fun.data = boxplot_info, geom = "boxplot",
                          position = position_dodge2(width = width,preserve=preserve, padding = padding),
                          alpha = alpha, width = width, ...),
    if(is.na(alpha))
      ggplot2::stat_summary(fun.data = boxplot_info, geom = "boxplot",
                            position = position_dodge2(width = width,preserve=preserve, padding = padding),
                            width = width, ...),

    # This adds the outlier points
    if (outlier)
      ggplot2::stat_summary(fun.data = outlier_points, geom= "point", position = position_dodge(width), ...),

    # This adds the count (vjust spaces it away from the plot)
    if (count)
      ggplot2::stat_summary(fun.data = ncount, geom = "text", vjust = 1.5, position = position_dodge2(width = width, preserve=preserve),
                            size = fontsize / ggplot2::.pt, ...),

    # This makes the point at the mean or 90th percentile
    if (middlepoint == "mean")
      ggplot2::stat_summary(fun = mean, geom = "point", shape = 8, colour = "black",
                            position = position_dodge2(width = width, preserve=preserve), ...),
    if (middlepoint == "90th")
      ggplot2::stat_summary(fun = quantile, fun.args = list(probs = 0.9),  geom = "point", shape = 8, colour = "black",
                            position = position_dodge2(width = width, preserve=preserve), ...),

    # This adds the horizontal bars on the whiskers
    if (whiskerbar)
      ggplot2::stat_summary(fun.data = low_bar, geom = "errorbar",
                            position = position_dodge2(width = width, preserve=preserve, padding = padding+.2),
                            width = width),
    if (whiskerbar)
      ggplot2::stat_summary(fun.data = high_bar, geom = "errorbar",
                            position = position_dodge2(width = width, preserve=preserve, padding = padding+.2),
                            width = width)
  )

}

########################################################################################################################*
########################################################################################################################*
########################################################################################################################*

#' Theme function for BC defaults
#' Automatically sets BC color palette.
#'
#' @param base_size Can be set to adjust default font sizes
#' @param base_family Can be set to change default font (R isn't great at fonts, so be careful)
#'
#' @export
#'
theme_bc <- function (base_size = 12, base_family = "", ...) {
  theme_bw(base_size = base_size, base_family = base_family, ...) %+replace%
    theme(plot.background = element_rect(fill = "transparent", color = NA),
          panel.border = element_rect(fill = NA, colour = "#43525a"),
          panel.grid = element_line(colour = "#f1f2f2"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#332a86", colour = "#43525a"),
          strip.text = element_text(colour = "white", size = rel(0.9),
                       margin = margin(0.8 * base_size/2, 0.8 * base_size/2, 0.8 * base_size/2, 0.8 * base_size/2)),
          strip.text.y = element_text(angle = 90),
          legend.key = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          legend.box.background = element_rect(fill = "transparent", color = NA),
          axis.title.y.right = element_text(angle = 90, margin = margin(l = base_size/4), vjust = 0, size = base_size),
          axis.text = element_text(size = rel(0.9)))
}


# Test code
# iris <- iris %>%
#   mutate(binpetal = as.factor(round(Petal.Width/.5)*.5))
# ggplot(iris, aes(x = Species, y = Sepal.Width, fill = binpetal)) +
#   geom_boxandwhisker() +
#   theme_bc() +
#   scale_fill_bc()
#
# ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, color = Petal.Length)) +
#   geom_point() +
#   facet_wrap(~Species) +
#   theme_bc() +
#   scale_color_bc(palette = "rainbow", discrete = FALSE)

########################################################################################################################*

# BOX AND WHISKER LEGEND ----
#' Box and Whisker Plot Legend for geom_boxandwhisker
#' Accepts the same arguments as geom_boxandwhisker with some additional for fixing legend appearance
#' You will have to manually add this to your B+W plots using cowplot or gridextra functions
#'
#'
#' @param fill Default setting is BC purple, can specify other hex codes or ggplot colors.
#' @param widthscale Legend spacing is hard, you can mess with this (approx range 0.5-1.5) to try to fit things better.
#' @param outlier Can be set to FALSE to remove points <5th percentile and >95th percentile.
#' @param count Can be set to FALSE to remove count from below the whisker.
#' @param middlepoint Can be equal to "mean" (default) or "90th" to select location, can be set to FALSE to remove.
#' @param whiskerbar Can be set to FALSE to remove horizontal bars on the ends of the whiskers.
#' @param alpha Can be set to a different transparency or NA if an alpha scale is needed (not recommended).
#' @param width Can be set to a number between 0 and 1, with lower numbers increasing space between boxes.
#' @param fontsize Can be used to adjust the size of the count. Use a number equivalent to a standard size in points.
#' @param whiskerloc Can be used to adjust the percentile that the whiskers extend to, use the low value, high will be calculated.
#' @param countlabel Can be set to TRUE if you want the count to appear as n=#
#'
#' @examples
#' legend_only <- boxwhisker_legend()
#' legend_plot <- ggdraw(your_plot) + draw_plot(legend_only, x = .65, y = .6, width = .35, height = .4)
#'
#' @export
#'
boxwhisker_legend <- function(fill = "#332a86", widthscale = 1, meaninside = TRUE,
                              outlier = TRUE, count = TRUE, middlepoint = "mean", whiskerbar = TRUE,
                              alpha = .8, fontsize = 9, whiskerloc = .05, countlabel = FALSE, ...) {
  require(magrittr)

  # xlocations
  nearx = ifelse(meaninside, 1.1, 1.5)
  midx = 1.2
  farx = 1.5
  adjustx <- ifelse(whiskerbar, farx, midx)

  # Whisker locations
  whiskerlow <- whiskerloc
  whiskerhigh <- 1- whiskerloc

  # Legend data
  set.seed(307)
  legend_data <- data.frame(yvalues = c(rnorm(48, mean = 50, sd = 10), 50,50,50,50,50,50,20,21.5,24,30,30,30))

  legend_labels <- legend_data %>%
    dplyr::summarise(P05 = quantile(yvalues, whiskerlow),
                     P25 = quantile(yvalues, .25),
                     P50 = quantile(yvalues, .5),
                     P75 = quantile(yvalues, .75),
                     P95 = quantile(yvalues, whiskerhigh),
                     low = quantile(yvalues, whiskerlow) - 5,
                     high = quantile(yvalues, whiskerhigh) + 10,
                     count = min(yvalues),
                     mean = mean(yvalues),
                     P90 = quantile(yvalues, .9)) %>%
    tidyr::pivot_longer(c(P05, P25, P50, P75, P95, low, high, count, mean, P90), names_to = "ID", values_to = "yloc") %>%
    dplyr::mutate(Label = dplyr::case_when(ID == "P05" ~ paste0(whiskerlow*100, "th percentile"),
                                           ID == "P25" ~ "25th percentile",
                                           ID == "P50" ~ "Median",
                                           ID == "P75" ~ "75th percentile",
                                           ID == "P95" ~ paste0(whiskerhigh*100, "th percentile"),
                                           ID == "low" ~ paste0("<", whiskerlow*100, "th percentile"),
                                           ID == "high" ~ paste0(">", whiskerhigh*100, "th percentile"),
                                           ID == "mean" ~ "Mean",
                                           ID == "P90" ~ "90th percentile",
                                           ID == "count" ~ "Number of values"),
                  xloc = dplyr::case_when(ID == "P25" | ID == "P50" | ID == "P75" ~ farx,
                                          ID == "low" | ID == "high" | ID == "count" | ID == "P90" ~ midx,
                                          ID == "mean" ~ nearx,
                                          ID == "P05" | ID == "P95" ~ adjustx))

  count_lab <-  dplyr::filter(legend_labels, ID == "count")

  legend_labels <- dplyr::filter(legend_labels, ID != "count")

  if(!outlier) {
    legend_labels <- dplyr::filter(legend_labels, ID != "low" & ID != "high")
    count_lab$yloc <- legend_labels$yloc[legend_labels$ID == "P05"]
  }

  if(middlepoint == "mean") {
    legend_labels <- dplyr::filter(legend_labels, ID != "P90")
  } else if (middlepoint == "90th") {
    legend_labels <- dplyr::filter(legend_labels, ID != "mean")
  } else {
    legend_labels <- dplyr::filter(legend_labels, ID != "P90" & ID != "mean")
  }

  ggplot2::ggplot(legend_data, ggplot2::aes(x = 1, y = yvalues)) +
    geom_boxandwhisker(outlier = outlier, count = count, middlepoint = middlepoint, whiskerbar = whiskerbar,
                       alpha = alpha, fontsize = fontsize, whiskerloc = whiskerloc, countlabel = countlabel,
                       whiskerlabel = FALSE, boxedgelabel = FALSE, medianlabel = FALSE, fill = fill, ...) +
    ggplot2::geom_text(data = legend_labels, ggplot2::aes(x = xloc, y = yloc, label = Label), size = fontsize/ggplot2::.pt, vjust = .5, hjust = 0) +
    theme_bc() +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank()) +
    ggplot2::coord_cartesian(xlim = c(.5,2.5 / widthscale)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(mult = c(.12,.05))) +
    if(count & !countlabel)
      ggplot2::geom_text(data = count_lab, ggplot2::aes(x = xloc, y = yloc, label = Label), size = fontsize/ggplot2::.pt, vjust = 1.5, hjust = 0)


}

# boxwhisker_legend(widthscale = .5)

########################################################################################################################*
########################################################################################################################*
########################################################################################################################*
#' Creates a png file of plots with automatic scaling for adding to word documents
#'
#' @param plot Plot to be exported
#' @param filename Desired file name in quotes without file extension.
#' @param vscale Default height is 4", based on typical plot ratios to copy into a word doc.  Increase scale to multiply size.
#' @param hwidth Default width is 6.5", only change if plot is being added to a different page width. Specify in inches.
#'
#' @export
#'
export_plot <- function(plot, filename = "DRAFT R Plot", vscale = 1, hwidth = 6.5){

  ggplot2::ggsave(plot = plot, filename = paste(filename, ".png", sep = ""),
                  width = hwidth, height = 4*vscale, units = "in", dpi = 300, bg = "transparent")

}


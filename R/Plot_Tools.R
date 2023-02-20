# This script contains code for plot themes and the BC color palette

########################################################################################################################*

# BC Color List
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
               charcoal = "#444d3e")



# Custom color palette code from: https://www.r-bloggers.com/2018/02/creating-corporate-colour-palettes-for-ggplot2/
extract_hex() <- function(...) {
  cols <- c(...)
  if (is.null(cols))
    return (bc_colors)
  bc_colors[cols]
}

# Additional color palettes can be added here using a combo of the color names above. Need to check all for
# color blind friendly and make sure they look good on a plot.
bc_color_palettes <- list(primary = extract_hex(purple, green, blue, yellow),
                          rainbow = extract_hex(bright_red, orange, yellow, light_green, blue, purple, warm_gray))

# Color palette checking:
# https://projects.susielu.com/viz-palette?colors=[%22#332a86%22,%22#76b043%22,%22#009ed1%22,%22#ffc233%22,%22#43525a%22,%22#27bdbe%22,%22#c0cac7%22,%22#b84626%22,%22#cab0ff%22,%22#b38f6b%22]&backgroundColor=%22white%22&fontColor=%22black%22&mode=%22normal%22


# Use code from stack overflow to make a color palette that can interpolate additional colors but maintain the order of
# the color palette: https://stackoverflow.com/questions/61674217/custom-discrete-color-scale-in-ggplot-does-not-respect-order

colorRamp_d <- function (colors, n,
                         bias = 1,
                         space = c("rgb", "Lab"),
                         interpolate = c("linear",
                                         "spline"),
                         alpha = FALSE){

  # PRELIMINARY STEPS ----------------
  if (bias <= 0)
    stop("'bias' must be positive")
  if (!missing(space) && alpha)
    stop("'alpha' must be false if 'space' is specified")
  colors <- t(col2rgb(colors, alpha = alpha)/255)
  space <- match.arg(space)
  interpolate <- match.arg(interpolate)

  # CUT THE COLOR VECTOR ----------------------

  if (space == "Lab")
    colors <- convertColor(colors, from = "sRGB", to = "Lab")
  interpolate <- switch(interpolate, linear = stats::approxfun,
                        spline = stats::splinefun)

  # RESPECT ORDER IF NCLASSES<NCOLORS
  if (n<nrow(colors)) colors <- colors[1:n,]

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

# Need to make sure these actually work
scale_color_bc <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bc_pal(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("colour", paste0("bc_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_color_bc <- function(palette = "primary", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bc_pal(palette = palette, reverse = reverse)
  if (discrete) {
    discrete_scale("fill", paste0("bc_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


########################################################################################################################*

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
#' @param width Can be set to a number between 0 and 1, with lower numbers increasing space between boxes.
#' @param fontsize Can be used to adjust the size of the count. Use a number equivalent to a standard size in points.
#'
#' @export
#'
geom_boxandwhisker <- function (outlier = TRUE, count = TRUE, middlepoint = "mean", whiskerbar = TRUE,
                                alpha = .8, width = .9, fontsize = 9, ...) {

  # Box and Whiskers - these functions set up for the stat_summary functions

  # Quantiles for the boxplot function
  boxplot_info <- function(x) {
    r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
  }

  # For the horizontal bars on the ends of the whiskers (errorbar)
  low_bar <- function(x) {
    range <- quantile(x, probs = c(0.05, 0.25))
    names(range) <- c("ymin", "ymax")
    range
  }
  high_bar <- function(x) {
    range <- quantile(x, probs = c(0.75, 0.95))
    names(range) <- c("ymin", "ymax")
    range
  }

  # For the outlier points (point)
  outlier_points <- function(x) {
    subset(x, x < quantile(x,0.05) | quantile(x,0.95) < x)
  }

  # For the count (text). Location changes depending on whether outlier points are included.
  if (outlier == TRUE){
    ncount <- function(x){
      return(c(y = min(x), label = length(x)))
    }
  } else if (outlier == FALSE){
    ncount <- function(x){
      return(c(y = as.numeric(quantile(x, 0.05)), label = length(x)))
    }
  }

  # Returns a list of stat_summary for the full plot
  list(
    # This makes the actual box and whisker
    if(!is.na(alpha))
    ggplot2::stat_summary(fun.data = boxplot_info, geom = "boxplot", position = ggplot2::position_dodge(0.9),
                          alpha = alpha, width = width, ...),
    if(is.na(alpha))
      ggplot2::stat_summary(fun.data = boxplot_info, geom = "boxplot", position = ggplot2::position_dodge(0.9),
                            width = width, ...),

    # This adds the outlier points
    if (outlier)
      ggplot2::stat_summary(fun = outlier_points, geom= "point", position = ggplot2::position_dodge(0.9)),

    # This adds the count (vjust spaces it away from the plot)
    if (count)
      ggplot2::stat_summary(fun.data = ncount, geom = "text", vjust = 1.5, position = ggplot2::position_dodge(0.9),
                            size = fontsize / ggplot2::.pt),

    # This makes the point at the mean or 90th percentile
    if (middlepoint == "mean")
      ggplot2::stat_summary(fun = mean, geom = "point", shape = 8, colour = "black", position = ggplot2::position_dodge(0.9)),
    if (middlepoint == "90th")
      ggplot2::stat_summary(fun = quantile, fun.args = list(probs = 0.9),  geom = "point", shape = 8, colour = "black",
                            position = ggplot2::position_dodge(0.9)),

    # This adds the horizontal bars on the whiskers
    if (whiskerbar)
      ggplot2::stat_summary(fun.data = low_bar, geom = "errorbar", position = ggplot2::position_dodge(0.9),
                            width = 0.9 * width),
    if (whiskerbar)
      ggplot2::stat_summary(fun.data = high_bar, geom = "errorbar", position = ggplot2::position_dodge(0.9),
                            width = 0.9 * width)
  )

}



iris <- iris %>%
  mutate(binpetal = as.factor(round(Petal.Width/.5)*.5))
ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  geom_boxandwhisker(width = .5, fill = "purple", fontsize = 12) +
  theme_bw(base_size = 15)


########################################################################################################################*

#' Theme function for BC defaults
#' Automatically sets BC color palette.
#'
#' @param base_size Can be set to adjust default font sizes
#' @param base_family Can be set to change default font (R isn't great at fonts, so be careful)
#'
#' @export
#'
theme_bc <- function (base_size = 11, base_family = "", ...) {
  theme_bw(base_size = base_size, base_family = base_family, ...) %+replace%
    theme(plot.background = element_rect(fill = "transparent", color = NA),
          panel.border = element_rect(fill = NA, colour = "#43525a"),
          panel.grid = element_line(colour = "#f1f2f2"),
          panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "#332a86", colour = "#43525a"),
          strip.text = element_text(colour = "white", size = rel(0.8),
                       margin = margin(0.8 * base_size/2, 0.8 * base_size/2, 0.8 * base_size/2, 0.8 * base_size/2)),
          legend.key = element_rect(fill = "transparent", colour = NA), complete = TRUE,
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent", color = NA))
}

ggplot(iris, aes(x = Sepal.Width, y = Petal.Width)) +
  geom_point() +
  facet_wrap(~Species) +
  theme_bc()

########################################################################################################################*
#' Creates a png file of plots with automatic scaling for adding to word documents
#'
#' @param plot Plot to be exported
#' @param filename Desired file name in quotes without file extension.
#' @param vscale Default height is 4", based on typical plot ratios to copy into a word doc.  Increase scale to multiply size.
#' @param hscale Default width is 6.5", only change the scale if plot is being added to a different page width.
#'
#' @export
#'
export_plot <- function(plot, filename = "DRAFT R Plot", vscale = 1){

  ggplot2::ggsave(plot = plot, filename = paste(filename, ".png", sep = ""),
                  width = 6.5*hscale, height = 4*vscale, units = "in", dpi = 300, bg = "transparent")

}


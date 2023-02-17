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
#' Outlier points can be included or excluded.
#'
#'
#' @param outlier Can be set to FALSE to remove points <5th percentile and >95th percentile.
#' @param count Can be set to FALSE to remove count from below the whisker.
#' @param middlepoint Can be equal to "mean" (default) or "90th" to select location, can be set to FALSE to remove.
#' @param whiskerbar Can be set to FALSE to remove horizontal bars on the ends of the whiskers.
#'
#' @export
#'
geom_boxandwhisker <- function (outlier = TRUE, count = TRUE, middlepoint = "mean", whiskerbar = TRUE) {

  # Box and Whiskers - these functions set up for the stat_summary functions

  # Quantiles for the boxplot function
  boxplot_info <- function(x) {
    r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
    names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
    r
  }

  # For the horizontal bars on the ends of the whiskers (errorbar)
  low_bar <- function(x) {
    r <- quantile(x, probs = c(0.05, 0.25))
    names(r) <- c("ymin", "ymax")
    r
  }
  high_bar <- function(x) {
    r <- quantile(x, probs = c(0.75, 0.95))
    names(r) <- c("ymin", "ymax")
    r
  }

  # For the outlier points (point)
  outlier_points <- function(x) {
    subset(x, x < quantile(x,0.05) | quantile(x,0.95) < x)
  }

  # For the count (text). Location changes depending on whether outlier points are included.
  if (outlier == TRUE){
    ncount <- function(x){
      # Need to figure out how to space this away from the points
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
    ggplot2::stat_summary(fun.data = boxplot_info, geom = "boxplot"),
    # This adds the outlier points
    if (outlier)
      ggplot2::stat_summary(fun = outlier_points, geom= "point", position = position, size = 1.5 * pointsize),
    # This adds the count
    if (count)
      ggplot2::stat_summary(fun.data = ncount, geom = "text"),
    # This makes the point at the mean or 90th percentile
    if (diamond == "mean")
      ggplot2::stat_summary(fun = mean, geom = "point", shape = 8, colour = "black"),
    if (diamond == "90th")
      ggplot2::stat_summary(fun = quantile, fun.args = list(probs = 0.9),  geom = "point", shape = 8, colour = "black"),
    # This adds the horizontal bars on the whiskers
    if (whiskerbar)
      ggplot2::stat_summary(fun.data = low_bar, geom = "errorbar"),
    if (whiskerbar)
      ggplot2::stat_summary(fun.data = high_bar, geom = "errorbar")
  )

}



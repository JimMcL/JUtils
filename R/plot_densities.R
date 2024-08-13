
#' Plot a list of densities
#'
#' Plots a list of densities (as created by \code{\link[stats]{density}}).
#' Plotting is performed using base graphics. Each density is drawn as a line,
#' and optionally filled. Fill colours may be easily made transparent by
#' specifying a value for \code{fillAlpha}. Attempts to provide sensible
#' defaults while allowing maximum flexibility.
#'
#' @param densities List of densities to be plotted
#' @param lineColours Vector of colours used to draw the lines. Defaults to the
#'   rainbow palette (\code{\link[grDevices]{rainbow}}).
#' @param lty Line style used to draw the lines.
#' @param lwd Line width used to draw the lines.
#' @param fillColours Vector of colours to fill the density polygons. If not
#'   specified or NULL, the polygons are not filled.
#' @param fillAlpha If not NA (the default), should be an alpha value between 0
#'   and 1. The alpha of the fill colours are multiplied by this value.
#' @param fillDensities If not NA (the default), density of shading lines used
#'   to fill polygons (passed as argument \code{density} to
#'   \code{\link[graphics]{polygon}}).
#' @param fillAngles If not NA (the default), slope of shading lines used to
#'   fill polygons (passed as argument \code{angle} to
#'   \code{\link[graphics]{polygon}}).
#' @param meanLty Line style used to draw vertical mean. NA (the default) means don't draw mean lines.
#' @param xlim Defines the graphical extents of the x-axis. Defaults to include
#'   all the density lines.
#' @param ylim Defines the graphical extents of the y-axis. Defaults to include
#'   all the density lines.
#' @param add If TRUE, the densities are added to an existing plot, otherwise a
#'   new plot is created.
#' @param ylab Y label, passed to \code{\link[graphics]{plot}}.
#' @param legendLabels Optional vector of characters. If specified, a legend will be added to the plot.
#' @param legendPos Position of optional legend.
#' @param ... Additional parameters are passed to \code{\link[graphics]{plot}}.
#'
#' @returns \code{NULL} or, if means are displayed, vector of mean values (returned invisibly).
#'
#' @seealso \code{\link[stats]{density}}, \code{\link[grDevices]{rainbow}},
#'   \code{RColorBrewer::brewer.pal}, \code{\link[graphics]{polygon}}
#'
#' @examples
#' data <- list(
#'   normal = rnorm(100),
#'   uniform = runif(50),
#'   exponential = rexp(200))
#' densities <- lapply(data, density)
#' JPlotDensities(densities)
#' # Display legend
#' legend("topleft", c("Normal", "Uniform", "Exponential"), col = rainbow(3), lwd = 2)
#'
#' @export
JPlotDensities <- function(densities, lineColours = NULL, lty = 1, lwd = 2,
                           fillColours = NULL, fillAlpha = NA, fillDensities = NA, fillAngles = NA,
                           meanLty = NA,
                           xlim = NULL, ylim = NULL, ylab = "Density", add = FALSE,
                           legendLabels, legendPos = "topleft", ...) {

  .applyAlpha <- function(colour, alpha) {
    c <- grDevices::col2rgb(colour, alpha = TRUE) / 255
    grDevices::rgb(c[1,], c[2,], c[3,], c[4, ] * alpha, maxColorValue = 1)
  }

  # Recycle parameters if they're not long enough
  lty <- rep(lty, length.out = length(densities))
  lwd <- rep(lwd, length.out = length(densities))
  meanLty <- rep(meanLty, length.out = length(densities))

  # Provide reasonable default line colours
  if (is.null(lineColours)) {
    lineColours <- grDevices::rainbow(length(densities))
  }
  # Repeat colours if necessary
  lineColours <- rep(lineColours, length.out = length(densities))
  # Repeat fill parameters
  fillDensities <- rep(fillDensities, length.out = length(densities))
  fillAngles <- rep(fillAngles, length.out = length(densities))

  # Return value
  r <- NULL

  # Create empty plot
  if (!add) {
    if (is.null(xlim))
      xlim <- range(lapply(densities, function(d) d$x), na.rm = TRUE)
    if (is.null(ylim))
      ylim <- range(lapply(densities, function(d) d$y), na.rm = TRUE)
    graphics::plot(NULL, xlim = xlim, ylim = ylim, ylab = ylab, ...)
  }

  # Optionally fill shapes first
  if (!is.null(fillColours)) {
    if (!is.na(fillAlpha))
      fillColours <- .applyAlpha(fillColours, fillAlpha)
    fillColours <- rep(fillColours, length.out = length(densities))
    i <- 1
    for (d in densities) {
      if (!is.na(fillColours[i]))
        graphics::polygon(d, col = fillColours[i], density = fillDensities[i], angle = fillAngles[i], border = NA)
      i <- i + 1
    }
  }

  # Plot densities as lines
  i <- 1
  for (d in densities) {
    graphics::lines(d, col = lineColours[i], lty = lty[i], lwd = lwd[i])
    i <- i + 1
  }

  # Optional mean lines
  if (!all(is.na(meanLty))) {
    # Derive mean from density
    m <- sapply(densities, function(d) weighted.mean(d$x, d$y))
    graphics::abline(v = m, col = lineColours, lty = meanLty)
    # Rreturn means
    r <- m
  }

  # Optional legend
  if (!missing("legendLabels") && !is.null(legendLabels)) {
    graphics::legend(legendPos, legendLabels, col = lineColours, lty = lty, lwd = lwd)
  }

  invisible(r)
}

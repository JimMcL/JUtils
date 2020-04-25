
#' Plots a list of densities as optionally filled lines using base graphics
#'
#' Attempts to provide sensible defaults while allowing maximum flexibility.
#'
#' @param densities List of densities to be plotted
#' @param lineColours Vector of colours used to draw the lines. Defaults to the
#'   rainbow palette.
#' @param fillColours Vector of colours to fill the densitiy polygons. If not
#'   specified or NULL, the polygons are not filled.
#' @param fillAlpha If not NA (the default), should be an alpha value between 0
#'   and 1. The alpha of the fill colours are multiplied by this value.
#' @param lty Line style used to draw the lines.
#' @param lwd Line width used to draw the lines.
#' @param xlim Defines the graphical extents of the x-axis. Defaults to include all the density lines.
#' @param ylim Defines the graphical extents of the y-axis. Defaults to include all the density lines.
#' @param add If TRUE, the densities are added to an existing plot, otherwse a new plot is created.
#' @param ... Additional parameters are passed to \code{\link[graphics]{plot}}.
#'
#' @seealso \code{\link[stats]{density}}, \code{\link[RColorBrewer]{brewer.pal}}
#'
#' @examples
#' data <- list(
#'   normal = rnorm(100),
#'   uniform = runif(50),
#'   exponential = rexp(200))
#' densities <- lapply(data, density)
#' JPlotDensities(densities)
#'
#' @export
JPlotDensities <- function(densities, lineColours = NULL, fillColours = NULL, fillAlpha = NA, lty = 1, lwd = 2, xlim = NULL, ylim = NULL, add = FALSE, ...) {

  .applyAlpha <- function(colour, alpha) {
    c <- grDevices::col2rgb(colour, alpha = TRUE) / 255
    grDevices::rgb(c[1,], c[2,], c[3,], c[4, ] * alpha, maxColorValue = 1)
  }

  # Recycle parameters if they're not long enough
  lty <- rep(lty, length.out = length(densities))
  lwd <- rep(lwd, length.out = length(densities))

  # Provide reasonable default line colours
  if (is.null(lineColours)) {
    lineColours <- grDevices::rainbow(length(densities))
  }
  # Repeat colours if necessary
  lineColours <- rep(lineColours, length.out = length(densities))

  # Create empty plot
  if (!add) {
    if (is.null(xlim))
      xlim <- range(lapply(densities, function(d) d$x), na.rm = TRUE)
    if (is.null(ylim))
      ylim <- range(lapply(densities, function(d) d$y), na.rm = TRUE)
    graphics::plot(NULL, xlim = xlim, ylim = ylim, ...)
  }

  # Optionally fill shapes first
  if (!is.null(fillColours)) {
    if (!is.na(fillAlpha))
      fillColours <- .applyAlpha(fillColours, fillAlpha)
    fillColours <- rep(fillColours, length.out = length(densities))
    i <- 1
    for(d in densities) {
      if (!is.na(fillColours[i]))
        graphics::polygon(d, col = fillColours[i], border = NA)
      i <- i + 1
    }
  }

  # Plot densities as lines
  i <- 1
  for(d in densities) {
    graphics::lines(d, col = lineColours[i], lty = lty[i], lwd = lwd[i])
    i <- i + 1
  }
}


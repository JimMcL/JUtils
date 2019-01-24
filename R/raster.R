#library(jpeg)
#library(png)


#' Adds a raster image to a plot, preserving the image's aspect ratio.
#'
#' @param img Raster image, possibly created by calling
#'   \code{\link[jpeg]{readJpeg}} or \code{\link[png]{readPNG}}.
#' @param cx, cy Position on plot of the centre of the image in user
#'   coordinates.
#' @param width Width of the image in user coordinates. The height will be
#' calculated from the aspect ratio of the image and the respective scales of
#' the x- and y-axes.
#'
#' @export
JPlotRaster <- function(img, cx, cy, width) {
  # Plot size in user coordinates (x1, x2, y1, y2)
  usr <- par("usr")
  # Plot size in inches
  pin <- par("pin")

  xScale <- (usr[2] - usr[1]) / pin[1]
  yScale <- (usr[4] - usr[3]) / pin[2]

  # Raster aspect ratio
  rasterAR <- dim(img)[2] / dim(img)[1]
  # Work out how to display while preserving aspect ratio
  # Image height in user coords assuming equal user/display scale on both axes
  usrHeight <- width / rasterAR
  # Adjust for different axis scales
  height <- usrHeight / xScale * yScale

  rasterImage(img, cx - width / 2, cy - height / 2, cx + width / 2, cy + height / 2)
}

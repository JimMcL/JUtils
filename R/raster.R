#library(jpeg)
#library(png)


#' Adds a raster image to a plot, preserving the image's aspect ratio.
#'
#' @param img Raster image, possibly created by calling
#'   \code{\link[jpeg]{readJPEG}} or \code{\link[png]{readPNG}}.
#' @param x,y Location on the plot of the image in user coordinates.
#' @param width Width of the image in user coordinates. The height will be
#'   calculated from the aspect ratio of the image and the respective scales of
#'   the x- and y-axes.
#' @param position Specifies the location of \code{x, y} on the image. E.g. if
#'   \code{"centre"}, the image will be centred on \code{x, y}. if
#'   \code{"topleft"}, the top left-hand corner of the image will be located at
#'   \code{x, y}.
#' @param ... Additional arguments are passed on to \code{\link[graphics]{rasterImage}}.
#'
#' @export
JPlotRaster <- function(img, x, y, width,
                        position = c("centre", "center", "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right"),
                        ...) {
  position <- match.arg(position)

  # Plot size in user coordinates (x1, x2, y1, y2)
  usr <- par("usr")
  # Plot size in inches
  pin <- par("pin")

  xScale <- (usr[2] - usr[1]) / pin[1]
  yScale <- (usr[4] - usr[3]) / pin[2]

  # Raster aspect ratio
  rasterAR <- dim(img)[2] / dim(img)[1]
  # Work out how to display while preserving aspect ratio.
  # Start with image height in user coords assuming equal user/display scale on both axes
  usrHeight <- width / rasterAR
  # Now adjust for different axis scales
  height <- usrHeight / xScale * yScale

  # Adjust centre for position
  if (grepl("left", position)) {
    x <- x + width / 2
  }
  if (grepl("right", position)) {
    x <- x - width / 2
  }
  if (grepl("top", position)) {
    y <- y - height / 2
  }
  if (grepl("bottom", position)) {
    y <- y + height / 2
  }

  graphics::rasterImage(img, x - width / 2, y - height / 2, x + width / 2, y + height / 2, ...)
}

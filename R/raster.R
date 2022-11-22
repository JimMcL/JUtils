#library(jpeg)
#library(png)


#' Adds a raster image to a plot, preserving the image's aspect ratio.
#'
#' Exactly one of \code{width} and \code{height} must be specified.
#'
#' @param img Raster image, possibly created by calling
#'   \code{\link[jpeg]{readJPEG}} or \code{\link[png]{readPNG}}.
#' @param x,y Location on the plot of the image in user coordinates.
#' @param width Width of the image in user coordinates. The height will be
#'   calculated from the aspect ratio of the image and the respective scales of
#'   the x- and y-axes.
#' @param height Height of the image in user coordinates. The width will be
#'   calculated from the aspect ratio of the image and the respective scales of
#'   the x- and y-axes.
#' @param position Specifies the location of \code{x, y} on the image. E.g. if
#'   \code{"centre"}, the image will be centred on \code{x, y}. if
#'   \code{"topleft"}, the top left-hand corner of the image will be located at
#'   \code{x, y}.
#' @param offset Vector with length 2. Amount to offset \code{x} and \code{y} by,
#'   as a proportion of the image display width and height, respectively.
#' @param ... Additional arguments are passed on to \code{\link[graphics]{rasterImage}}.
#'
#' @examples
#'\dontrun{
#' plot(...)
#' img <- readJPEG("myjpeg.jpg", native = TRUE)
#' # Draw image centred on (0, 0)
#' JPlotRaster(img, x = 0, y = 0, width = 2)
#'}
#'
#' @export
JPlotRaster <- function(img, x, y, width = NA, height = NA,
                        position = c("centre", "center", "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right"),
                        offset = c(0, 0),
                        ...) {
  position <- match.arg(position)
  if (!xor(is.numeric(width), is.numeric(height)))
    stop("Exactly 1 of width and height must be specified")

  # Plot size in user coordinates (x1, x2, y1, y2)
  usr <- graphics::par("usr")
  # Plot size in inches
  pin <- graphics::par("pin")

  xScale <- (usr[2] - usr[1]) / pin[1]
  yScale <- (usr[4] - usr[3]) / pin[2]

  # Raster aspect ratio
  rasterAR <- dim(img)[2] / dim(img)[1]
  # Work out how to display while preserving aspect ratio.
  if (is.numeric(width)) {
    # Start with image height in user coords assuming equal user/display scale on both axes
    usrHeight <- width / rasterAR
    # Now adjust for different axis scales
    height <- usrHeight / xScale * yScale
  } else {
    # Same as above but calculate width instead of height
    usrWidth <- height * rasterAR
    width <- usrWidth / yScale * xScale
  }

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

  # Adjust position by inset
  x <- x + offset[1] * width
  y <- y + offset[2] * height

  graphics::rasterImage(img, x - width / 2, y - height / 2, x + width / 2, y + height / 2, ...)
}

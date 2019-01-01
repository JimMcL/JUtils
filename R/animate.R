


#' Combine multiple plots into an animated GIF file.
#'
#' Works by writing each plot to a PNG file, then combining them into a GIF file
#' using the ImageMagick utility. Frames are created by calling a custom plot
#' function.
#'
#' You must have \href{https://imagemagick.org/script/download.php}{ImageMagick}
#' installed, and the ImageMagick bin subdirectory must be in your PATH
#' environment variable. If ImageMagick is not installed, or is not in your
#' PATH, \code{JAnimateGIF} will fail with an exception such as:
#'
#' \preformatted{Error in system2("magick", c("convert", "jp*.png", "-delay",
#' 100/frameRate, : '"magick"' not found}.
#'
#' For a general discussion about creating an animation in R, see
#' \url{https://stackoverflow.com/questions/1298100/creating-a-movie-from-a-series-of-plots-in-r},
#' or \url{https://www.r-graph-gallery.com/animation/}.
#'
#' The png to gif conversion can be quite time consuming for a large number of
#' frames, so start with a small number of frames to ensure everything works as
#' expected.
#'
#' @param nFrames Number of frames to be generated. You must specify one of
#'   \code{nFrames} or \code{frameKeys}.
#' @param frameKeys Vector of keys to be passed to `plotFn` to identify the
#'   frame to be plotted. If not specified, `frameKeys` will be set to the
#'   sequence `1:nFrames`.
#' @param gifFileName Name of the GIF file to be created.
#' @param plotFn Function which is called once for each frame. It is called once
#'   for each frame to be generated, with a single argument which is one of the
#'   values from \code{frameKeys}. If it does not generate a plot, the frame
#'   will be silently skipped.
#' @param frameRate Play back frame rate - used to set the frame delay in the
#'   GIF file.
#' @param tmpDir Name of a directory to be used to create temporary files in.
#' @param ... Any additional arguments are passed to the \code{JPlotToPNG}
#'   function.
#'
#' @return The error message (as a character vector) from the ImageMack convert
#'   command, or \code{character(0)} on success.
#'
#' @examples
#'\dontrun{
#' # Number of frames in the animation
#' nFrames <- 50
#'
#' # A function to plot a coloured polygon
#' .plotPoly <- function(frame) {
#'   # Created an empty plot
#'   plot(NULL, xlim = c(-1, 1), ylim = c(-1, 1), asp = 1, xlab = "", ylab = "", axes = FALSE)
#'   # Frame starts from 1, but a 1 cornered polygon is not very visually interesting
#'   nCorners <- frame + 1
#'   angles <- 2 * pi * (1:nCorners) / nCorners
#'   f <- frame / nFrames
#'   # Pick a colour based on the frame number
#'   col <- rgb(sin(pi / 2 * f), cos(pi / 2 * f), sin(pi / 2 * (f + .5)))
#'   # Draw a polygon
#'   polygon(cos(angles), sin(angles), col = col, border = "black", lwd = 4)
#' }
#'
#' JAnimateGIF(nFrames, gifFileName = "poly.gif", plotFn = .plotPoly, frameRate = 10)
#'}
#'
#' @export
JAnimateGIF <- function(nFrames = NULL, frameKeys = 1:nFrames, gifFileName, plotFn, frameRate = 30, tmpDir = tempdir(TRUE), ...) {

  ndigits <- ceiling(log10(length(frameKeys)))
  .tmpFileName <- function(i) sprintf("jp%0*d.png", ndigits, i)

  # Allow for plot commands which don't actually produce a file
  pngs <- character()

  # Generate all of the frames
  for (i in seq_along(frameKeys)) {
    key <- frameKeys[i]
    fname <- .tmpFileName(i)
    fname <- file.path(tmpDir, fname)
    # cat(sprintf("Plotting to %s...\n", fname))
    JPlotToPNG(fname, plotFn(key), ...)
    # Check if the file was created
    if (file.exists(fname))
      pngs <- c(pngs, fname)
  }

  # ImageMagick command to convert multiple pngs to animated gif.
  # By default, the delay is in ticks, where 1 tick = 100th sec.
  # https://imagemagick.org/script/command-line-options.php#delay
  # magick convert jp*.png -delay ? 3d.gif

  oldDir <- getwd()
  result <- tryCatch({
    setwd(tmpDir)
    # Need to specify files with a wildcard rather than explicitly listing them
    # all because with many frames, the command line becomes too long
    system2("magick", c("convert", "jp*.png", "-delay", 100 / frameRate, "3d.gif"), invisible = F, stderr = TRUE)
  },
  finally = setwd(oldDir)
  )

  # Move the GIF file
  file.rename(file.path(tmpDir, "3d.gif"), gifFileName)

  # Delete temporary pngs
  file.remove(pngs)

  result
}



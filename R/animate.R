
# Choose a method for generating a GIF from a sequence of still frames
pickGIFMethod <- function() {
  if (requireNamespace("gifski", quietly = TRUE)) {
    "gifski"
  } else if (requireNamespace("magick", quietly = TRUE)) {
    "magick-r"
  } else {
    "magick"
  }
}

pngToGIFMagickR <- function(pngs, videoFileName, loop, frameRate, optimize) {
  imgs <- magick::image_read(pngs)
  anim <- magick::image_join(imgs)
  anim <- magick::image_animate(anim, delay = 100 / frameRate, loop = loop, optimize = optimize)
  magick::image_write(anim, videoFileName)
  character(0)
}

pngToGIFGifskiR <- function(pngs, videoFileName, loop, frameRate, progress) {
  # Get image size from first frame
  info <- png::readPNG(pngs[1], info = TRUE)
  dim <- attr(info, "dim")
  # Gifski uses a different convention for loop than magick, and also hard-wires GIF size
  gifski::gifski(pngs, videoFileName, width = dim[2], height = dim[1],
                 delay = 1 / frameRate, loop = ifelse(loop <= 0, TRUE, loop), progress = progress)
  character(0)
}

pngToGIFMagick <- function(pngs, videoFileName, loop, frameRate, ext, subDir, tmpDir) {
  # ImageMagick command to convert multiple pngs to animated gif.
  # By default, the delay is in ticks, where 1 tick = 100th sec.
  # https://imagemagick.org/script/command-line-options.php#delay
  # magick convert jp*.png -delay ? 3d.gif

  oldDir <- getwd()
  tmpGif <- paste0("3d.", ext)
  result <- tryCatch({
    setwd(file.path(tmpDir, subDir))
    # Need to specify files with a wildcard rather than explicitly listing them
    # all because with many frames, the command line becomes too long
    system2("magick", c("convert", "-loop", loop, "-delay", 100 / frameRate, "jp*.png", tmpGif), invisible = F, stderr = TRUE)
  },
  finally = setwd(oldDir)
  )

  # Move the GIF file
  if (identical(result, character(0))) {
    if (!dir.exists(dirname(videoFileName))) {
      dir.create(dirname(videoFileName), recursive = TRUE)
    }
    if (!file.rename(file.path(tmpDir, subDir, tmpGif), videoFileName)) {
      stop(sprintf("Unable to create animation file %s", videoFileName))
    }
  }

  result
}

pngToGIFFFMpeg <- function(pngs, videoFileName, loop, frameRate, ext, subDir, tmpDir, ndigits) {
  # FFMpeg command to convert multiple pngs to animated gif.
  # Command based on https://shotstack.io/learn/use-ffmpeg-to-convert-images-to-video/
  # "-y" says answer yes to questions
  # ffmpeg -framerate 1 -i jp%3d.png -c:v libx264 -r ? -y 3d.mp4

  if (loop != 1) {
    stop("loop must be 1 when animating with ffmpeg")
  }
  oldDir <- getwd()
  tmpGif <- paste0("3d.", ext)
  result <- tryCatch({
    setwd(file.path(tmpDir, subDir))
    inp <- c("-framerate", frameRate, "-i", sprintf("jp%%%dd.png", ndigits))
    # Conversion suitable for powerpoint based on https://stackoverflow.com/a/45465730
    format <- c("-c:v", "libx264", "-preset", "slow", "-profile:v", "high", "-level:v", "4.0", "-pix_fmt", "yuv420p", "-crf", "22", "-codec:a", "aac")
    out <- c("-r", frameRate, "-y", tmpGif)
    cmd <- c(inp, format, out)
    #cat(paste(c(cmd, "\n")))
    system2("ffmpeg", cmd, invisible = F, stderr = TRUE)
  },
  finally = setwd(oldDir)
  )

  # Move the output file
  if (!dir.exists(dirname(videoFileName))) {
    dir.create(dirname(videoFileName), recursive = TRUE)
  }
  if (!file.rename(file.path(tmpDir, subDir, tmpGif), videoFileName)) {
    stop(sprintf("Unable to create animation file %s", videoFileName))
  }

  invisible(result)
}

#' Combine multiple plots into an animated GIF file.
#'
#' Writes each frame to a PNG file, then combines them into a GIF
#' file. Frames are created by calling a custom plot function. The PNG to GIF
#' conversion is performed by a 3rd party R package or application.
#'
#' You must have a suitable image conversion tool installed - see the documentation
#' of the argument \code{gifMethod} for details. \href{https://imagemagick.org/script/download.php}{ImageMagick}
#' installed, and the ImageMagick bin subdirectory must be in your PATH
#' environment variable. If ImageMagick is not installed, or is not in your PATH,
#' \code{JAnimateGIF} will fail with an exception such as:
#'
#' \preformatted{Error in system2("magick", c("convert", "jp*.png", "-delay",
#' 100/frameRate, : '"magick"' not found}.
#'
#' For a general discussion about creating an animation in R, see
#' \url{https://stackoverflow.com/questions/1298100/creating-a-movie-from-a-series-of-plots-in-r},
#' or \url{https://r-graph-gallery.com/animation}.
#'
#' The png to gif conversion can be quite time consuming for a large number of
#' frames, so start with a small number of frames to ensure everything works as
#' expected.
#'
#' In theory, magick can create mpeg files, but I can't get it to work, so use
#' \code{gifMethod="ffmpeg"} for file formats other than GIF.
#'
#' @param nFrames Number of frames to be generated. You must specify one of
#'   \code{nFrames} or \code{frameKeys}.
#' @param frameKeys Vector of keys to be passed to \code{plotFn} to identify the frame
#'   to be plotted. If not specified, \code{frameKeys} will be set to the sequence
#'   \code{1:nFrames}.
#' @param videoFileName Name of the video file to be created. The file type is
#'   inferred from the file extension, but must be GIF if \code{gifMethod == "gifski"}.
#' @param plotFn Function which is called once for each frame. It is called once
#'   for each frame to be generated, with a single argument which is one of the
#'   values from \code{frameKeys}. If it does not generate a plot, the frame will
#'   be silently skipped. If no frames are created for the entire animation, an
#'   error is generated.
#' @param frameRate Play back frame rate - used to set the frame delay in the GIF
#'   file.
#' @param loop Number of times animation should be played. 0 means loop
#'   infinitely.
#' @param gifMethod Specify the library/tool used to convert from PNG to GIF:
#' \itemize{
#'   \item{\code{"magick-r"}}{ uses the \href{https://docs.ropensci.org/magick/articles/intro.html}{magick R
#'   package}.}
#'   \item{\code{"magick"}}{ uses the \href{https://imagemagick.org/script/download.php}{ImageMagick command line
#'   application}.}
#'   \item{\code{"gifski"}}{ uses the \href{https://gif.ski/}{gifski R package}.}
#'   \item{\code{"ffmpeg"}}{ uses the \href{https://ffmpeg.org/}{FFMpeg command line application}, which can output \code{mp4} files. The output format is suitable for use by Powerpoint (at least on my version).}
#'   \item{\code{"auto"}}{ uses \code{"gifski"} if it is installed, otherwise uses \code{"magick-r"} if it is installed, otherwise uses \code{"magick"}.}
#' }
#' @param optimize Only used if \code{gifMethod == "magick-r"}. Passed to \link[magick]{image_animate}.
#' @param progress Only used if \code{gifMethod == "gifski"}. Passed to \link[gifski]{gifski}; if TRUE, prints some progress messages.
#' @param tmpDir Name of a directory to be used to create temporary files in.
#' @param ... Any additional arguments are passed to the \code{\link{JPlotToPNG}}
#'   function.
#'
#' @return The error message (as a character vector) from the ImageMack convert
#'   command, or \code{character(0)} (returned invisibly) on success.
#'
#' @seealso \code{\link{JPlotToPNG}}, \code{\link{JAnimateScenes}} for smoothly animating changing parameters.
#'
#' @examples
#' \dontrun{
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
#' JAnimateGIF("poly.gif", nFrames, plotFn = .plotPoly, frameRate = 10)
#' }
#'
#' @export
JAnimateGIF <- function(videoFileName, nFrames = NULL, frameKeys = 1:nFrames,
                        plotFn, frameRate = 30, loop = 0,
                        gifMethod = c("auto", "magick-r", "gifski", "magick", "ffmpeg"),
                        optimize = FALSE, progress = FALSE,
                        tmpDir = tempdir(TRUE),
                        ...) {

  gifMethod <- match.arg(gifMethod)
  if (gifMethod == "auto")
    gifMethod <- pickGIFMethod()

  # Create a new temporary directory to store all the frames.
  # This way, if an animation is interrupted (leaving behind frame files),
  # then another is run, the old frames won't get used in the new animation
  .createSubDir <- function() {
    for (i in 1:100) {
      td <- sprintf("jt%d", i)
      ftd <- file.path(tmpDir, td)
      if (!dir.exists(ftd)) {
        dir.create(ftd)
        return(td)
      }
    }
    stop(sprintf("Unable to create a new temporary directory under %s", tmpDir))
  }
  subDir <- .createSubDir()

  ndigits <- ceiling(log10(length(frameKeys) + 1))
  .tmpFileName <- function(i) sprintf("%s/jp%0*d.png", subDir, ndigits, i)

  # Allow for plot commands which don't actually produce a file
  pngs <- character()

  # Generate all of the frames
  for (i in seq_along(frameKeys)) {
    key <- frameKeys[i]
    fname <- .tmpFileName(i)
    fname <- file.path(tmpDir, fname)
    JPlotToPNG(fname, plotFn(key), ...)
    # Check if the file was created
    if (file.exists(fname))
      pngs <- c(pngs, fname)
  }
  if (length(pngs) == 0) {
    stop("No frames were plotted")
  }

  # Default to GIF output
  ext <- tools::file_ext(videoFileName)
  if (nchar(ext) == 0)
    ext <- "gif"
  # Ensure output directory exists
  if (!dir.exists(dirname(videoFileName))) {
    dir.create(dirname(videoFileName), recursive = TRUE)
  }

  if (gifMethod == "magick-r") {
    result <- pngToGIFMagickR(pngs, videoFileName, loop, frameRate, optimize)
  } else if (gifMethod == "magick") {
    result <- pngToGIFMagick(pngs, videoFileName, loop, frameRate, ext, subDir, tmpDir)
  } else if (gifMethod == "gifski") {
    result <- pngToGIFGifskiR(pngs, videoFileName, loop, frameRate, progress)
  } else if (gifMethod == "ffmpeg") {
    result <- pngToGIFFFMpeg(pngs, videoFileName, loop, frameRate, ext, subDir, tmpDir, ndigits)
  }

  # Delete temporary pngs
  file.remove(pngs)
  unlink(subDir, recursive = TRUE)

  # ffmpeg prints a lot of junk on success, so make it invisible
  if (identical(result, character(0)) || gifMethod == "ffmpeg")
    invisible(result)
  else
    result
}



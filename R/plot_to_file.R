# Functions for plotting to files
#
# V1.0

# "Private" function to run some code with output going somewhere special
.JplotToDevice <- function(filename, plotFn, onlyIfDoesntExist, openDeviceFn, closeDevFn = grDevices::dev.off) {
  if (!onlyIfDoesntExist || !file.exists(filename)) {
    openDeviceFn()
    tryCatch({
      if (is.function(plotFn))
        plotFn()
      else
        invisible(eval(plotFn))
    }, finally = {
      closeDevFn()
    })
  }
}

# Calculates and returns image geometry in inches
.geometry <- function(width, height, aspectRatio, res, inUnits, outUnits) {
  # Use either explicitly specified height, or calculate from width and aspect ratio
  height <- ifelse(is.na(height), width / aspectRatio, height)

  # Get list of choices for units from the default value of units in the calling function.
  # Must be a subset of c("in", "cm", "mm", "px")
  choices <- eval(formals(sys.function(sys.parent()))[["units"]])
  inUnits <- match.arg(inUnits, choices)
  .toInches <- function(n) {
    switch(inUnits,
           "in" = 1,
           "cm" = 1 / 2.54,
           "mm" = 1 / 25.4,
           "px" = 1 / res) * n
  }

  # Check that units are compatible and convert
  if (inUnits %in% outUnits) {
    # No conversion required
    list(width = width, height = height, units = inUnits)
  } else if ("in" %in% outUnits) {
    # Convert to inches
    list(width = .toInches(width), height = .toInches(height), units = "in")
  } else {
    stop(sprintf("Unable to handle image size in units '%s', require one of %s", inUnits, paste(outUnits, collapse = ", ")))
  }
}

########################################################################################
# Public functions

#' Plot to a PNG file
#'
#' Writes the output of a plot to a PNG file. If you are using ggplot, try either
#' using \code{JPlotToPNG(filename, print(<plotting code>))} or else \code{ggsave()}.
#'
#' @param filename The name of the PNG to create or overwrite.
#' @param plot A function or expression which will produce the plot to be
#'   written to the file.
#' @param width The width of the output PNG file in \code{units}.
#' @param height The height of the output PNG file in \code{units}. Defaults to
#'   \code{width / aspectRatio}. If \code{height} is specified,
#'   \code{aspectRatio} will be ignored.
#' @param aspectRatio Aspect ratio (\code{width / height}) of the output PNG
#'   file.
#' @param units Units of \code{width} and \code{height}.
#' @param type Plotting device - defaults to "cairo" if that is an available
#'   device since it produces nicer looking graphics.
#' @param res The nominal resolution in ppi. The value is passed in to
#'   \code{\link[grDevices]{png}}. Increasing the resolution will increase the
#'   size (in pixels) of the text and graph elements.
#' @param onlyIfDoesntExist If TRUE and the output file already exists,
#'   \code{JPlotToPNG} will do nothing.
#' @param ... Any additional arguments are passed to
#'   \code{\link[grDevices]{png}}.
#'
#' @seealso \code{\link[grDevices]{png}}
#'
#' @examples
#' JPlotToPNG("test.png", plot(rnorm(50), type = 'l', main = 'Wiggly line'))
#'
#' # Plot to a PNG file with width 180 mm, height 120 mm
#' # (i.e. height / aspectRatio which defaults to (3 / 2)), resolution 300 ppi.
#' # This results in a PNG file with size 2125x1417 pixels
#' JPlotToPNG("test.png", plot(1:10 + rnorm(10), type = "o"), width = 180, units = "mm", res = 300)
#'
#' @export
JPlotToPNG <- function(filename, plot,
                       width = 180, height = NA, aspectRatio = 3 / 2,
                       units = c("mm", "cm", "px", "in"),
                       type = ifelse(capabilities()["cairo"], 'cairo', NULL),
                       res = 72,
                       onlyIfDoesntExist = F, ...) {
  g <- .geometry(width, height, aspectRatio, res, units, c("mm", "cm", "px", "in"))

  .JplotToDevice(filename, plot, onlyIfDoesntExist, function () {
    grDevices::png(filename, width = g$width, height = g$height, units = g$units, type = type, res = res, ...)
  })
}

#' Plot to a TIFF file
#'
#' Writes the output of a plot to a TIFF file. If you are using ggplot, try either
#' using \code{JPlotToTIFF(filename, print(<plotting code>))} or else \code{ggsave()}.
#'
#' @param filename The name of the TIFF to create or overwrite.
#' @param plot A function or expression which will produce the plot to be
#'   written to the file.
#' @param width The width of the output TIFF file in \code{units}.
#' @param height The height of the output TIFF file in \code{units}. Defaults to
#'   \code{width / aspectRatio}. If \code{height} is specified,
#'   \code{aspectRatio} will be ignored.
#' @param aspectRatio Aspect ratio (\code{width / height}) of the output TIFF
#'   file.
#' @param units Units of \code{width} and \code{height}.
#' @param type Plotting device - defaults to "cairo" if that is an available
#'   device since it produces nicer looking graphics.
#' @param res The nominal resolution in ppi. The value is passed in to
#'   \code{\link[grDevices]{tiff}}. Increasing the resolution will increase the
#'   size (in pixels) of the text and graph elements.
#' @param onlyIfDoesntExist If TRUE and the output file already exists,
#'   \code{JPlotToTIFF} will do nothing.
#' @param ... Any additional arguments are passed to
#'   \code{\link[grDevices]{tiff}}.
#'
#' @seealso \code{\link[grDevices]{tiff}}
#'
#' @export
JPlotToTIFF <- function(filename, plot,
                        width = 180, height = NA, aspectRatio = 3 / 2,
                        units = c("mm", "cm", "px", "in"),
                        type = ifelse(capabilities()["cairo"], 'cairo', NULL),
                        res = 72,
                        onlyIfDoesntExist = F, ...) {
  g <- .geometry(width, height, aspectRatio, res, units, c("mm", "cm", "px", "in"))

  .JplotToDevice(filename, plot, onlyIfDoesntExist, function () {
    grDevices::tiff(filename, width = g$width, height = g$height, units = g$units, type = type, res = res, ...)
  })
}

#' Plot to a PDF file
#'
#' Writes the output of a plot to a PDF file. If you are using ggplot, try
#' either using \code{JPlotToPDF(filename, print(<plotting code>))} or else
#' \code{ggsave()}.
#'
#' @param filename The name of the PDF to create or overwrite.
#' @param plot A function or expression which will produce the plot to be
#'   written to the file.
#' @param width The width of the output PDF file in \code{units}.
#' @param height The height of the output PDF file in \code{units}. Defaults to
#'   \code{width / aspectRatio}. If \code{height} is specified,
#'   \code{aspectRatio} will be ignored.
#' @param units Units of \code{width} and \code{height}.
#' @param aspectRatio Aspect ratio (\code{width / height}) of the output PDF
#'   file.
#' @param bg Background colour - may be "transparent" for no background.
#' @param paper Paper size, defaults to "special" which is the value it must have if
#'   \code{width} and \code{height} are to be used.
#' @param family The font family to be used.
#' @param onlyIfDoesntExist If TRUE and the output file already exists,
#'   \code{JPlotToPDF} will do nothing.
#' @param ... Any additional arguments are passed to
#'   \code{\link[grDevices]{pdf}}.
#'
#' @seealso \code{\link[grDevices]{pdf}}
#'
#' @export
JPlotToPDF <- function(filename, plot,
                       width = 180, height = NA, aspectRatio = 3 / 2,
                       units =  c("mm", "cm", "in"),
                       bg = "white",
                       paper = "special",
                       family = "Helvetica",
                       onlyIfDoesntExist = F, ...) {
  g <- .geometry(width, height, aspectRatio, 1, units, "in")

  .JplotToDevice(filename, plot, onlyIfDoesntExist, function () {
    grDevices::pdf(filename, width = g$width, height = g$height, bg = bg, paper = paper, family = family, ...)
  })
}

#' Plot to an EPS (encapsulated postscript) file
#'
#' Writes the output of a plot to a EPS file. If you are using ggplot, try
#' either using \code{JPlotToEPS(filename, print(<plotting code>))} or else
#' \code{ggsave()}.
#'
#' @param filename The name of the EPS to create or overwrite.
#' @param plot A function or expression which will produce the plot to be
#'   written to the file.
#' @param width The width of the output EPS file in \code{units}.
#' @param height The height of the output EPS file in \code{units}. Defaults to
#'   \code{width / aspectRatio}. If \code{height} is specified,
#'   \code{aspectRatio} will be ignored.
#' @param units Units of \code{width} and \code{height}.
#' @param aspectRatio Aspect ratio (\code{width / height}) of the output EPS
#'   file.
#' @param bg Background colour - may be "transparent" for no background.
#' @param paper Paper size, defaults to "special" which is required if
#'   \code{width} and \code{height} are to be used.
#' @param family The font family to be used.
#' @param onlyIfDoesntExist If TRUE and the output file already exists,
#'   \code{JPlotToEPS} will do nothing.
#' @param ... Any additional arguments are passed to
#'   \code{\link[grDevices]{pdf}}.
#'
#' @seealso \code{\link[grDevices]{setEPS}}, \code{\link[grDevices]{postscript}}
#'
#' @export
JPlotToEPS <- function(filename, plot,
                       width = 180, height = NA, aspectRatio = 3 / 2,
                       units =  c("mm", "cm", "in"),
                       bg = "white",
                       paper = "special",
                       family = "Helvetica",
                       onlyIfDoesntExist = F, ...) {
  g <- .geometry(width, height, aspectRatio, 1, units, "in")

  .JplotToDevice(filename, plot, onlyIfDoesntExist, function () {
    grDevices::setEPS(family = family)
    grDevices::postscript(filename, width = g$width, height = g$height, bg = bg, paper = paper, family = family, ...)
  })
}

#' Plot to a file
#'
#' Writes the output of a plot to a file. The type of file is deduced from the
#' extension of the file name. If you are using ggplot rather than base
#' graphics, try either using \code{JPlotToFile(filename, print(<plotting
#' code>))} or \code{ggsave()}.
#'
#' @param filename The name of the file to create or overwrite.
#' @param plot A function or expression which will produce the plot to be
#'   written to the file.
#' @param ... Any additional arguments are passed to the appropriate function.
#'
#' @seealso \code{\link{JPlotToPNG}}, \code{\link{JPlotToTIFF}},
#'   \code{\link{JPlotToPDF}}, \code{\link{JPlotToEPS}}
#'
#' @export
JPlotToFile <- function(filename, plot, ...) {
  ext <- tolower(tools::file_ext(filename))

  fns <- list(
    eps =  JPlotToEPS,
    ps =   JPlotToEPS,
    pdf =  JPlotToPDF,
    #svg =  ,
    #emf =  ,
    #wmf =  ,
    png =  JPlotToPNG,
    #jpg =  JPlotToJpeg,
    #jpeg = JPlotToJpeg,
    #bmp =  JPlotToBMP,
    tiff = JPlotToTIFF
  )

  fns[[ext]](filename, plot, ...)
}

#' Send console (i.e. text) output to a file
#'
#' Sends the text output of an arbitrary R expression of function to a text file.
#'
#' @param filename Name of the file to write to.
#' @param expr An expression which outputs the text to be written.
#'
#' @examples
#' JReportToFile("test.txt", print("Hello world!"))
#'
#' @export
JReportToFile <- function(filename, expr) {
  oldOptions <- options()
  on.exit(options(oldOptions))
  # Normally, text output is wrapped based on the console window size. This
  # doesn't make sense when writing to a file, so make the width very big
  options(width = 10000)

  .JplotToDevice(filename, expr, onlyIfDoesntExist = FALSE,
                 openDeviceFn = function () { sink(filename) },
                 closeDevFn = sink)
}


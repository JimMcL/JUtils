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
.geometry <- function(width, height, aspectRatio, res, units, choices) {
  # Use either explicitly specified height, or calculate from width and aspect ratio
  height <- ifelse(is.na(height), width / aspectRatio, height)

  # Get list of choices for units from the default value of units in the calling function.
  # Must be a subset of c("in", "cm", "mm", "px")
  choices <- eval(formals(sys.function(sys.parent()))[["units"]])
  units <- match.arg(units, choices)
  .toInches <- function(n) {
    switch(units,
           "in" = 1,
           "cm" = 1 / 2.54,
           "mm" = 1 / 25.4,
           "px" = 1 / res) * n
  }

  list(width = .toInches(width), height = .toInches(height))
}

########################################################################################
# Public functions

#' Plot to a PNG file
#'
#' Writes the output of a plot to a PNG file. If you are using ggplot, try either
#' using \code{JPlotToPng(filename, print(<plotting code>))} or else \code{ggsave()}.
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
#'   \code{JPlotToPng} will do nothing.
#' @param ... Any additional arguments are passed to
#'   \code{\link[grDevices]{png}}.
#'
#' @seealso \code{\link[grDevices]{png}}
#'
#' @examples
#' JPlotToPng("test.png", plot(rnorm(50), type = 'l', main = 'Wiggly line'))
#'
#' @export
JPlotToPng <- function(filename, plot,
                       width = 180, height = NA, aspectRatio = 3 / 2,
                       units = c("mm", "cm", "px", "in"),
                       type = ifelse(capabilities()["cairo"], 'cairo', NULL),
                       res = 72,
                       onlyIfDoesntExist = F, ...) {
  g <- .geometry(width, height, aspectRatio, res, units)

  .JplotToDevice(filename, plot, onlyIfDoesntExist, function () {
    grDevices::png(filename, width = g$width, height = g$height, type = type, res = res, units = "in", ...)
  })
}

#' Plot to a TIFF file
#'
#' Writes the output of a plot to a TIFF file. If you are using ggplot, try either
#' using \code{JPlotToTiff(filename, print(<plotting code>))} or else \code{ggsave()}.
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
JPlotToTiff <- function(filename, plot,
                        width = 180, height = NA, aspectRatio = 3 / 2,
                        units = c("mm", "cm", "px", "in"),
                        type = ifelse(capabilities()["cairo"], 'cairo', NULL),
                        res = 72,
                        onlyIfDoesntExist = F, ...) {
  g <- .geometry(width, height, aspectRatio, res, units)

  .JplotToDevice(filename, plot, onlyIfDoesntExist, function () {
    grDevices::tiff(filename, width = g$width, height = g$height, type = type, res = res, units = "in", ...)
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
#' @param paper Paper size, defaults to "special" which is required if
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
  g <- .geometry(width, height, aspectRatio, 1, units)

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
  g <- .geometry(width, height, aspectRatio, 1, units)

  .JplotToDevice(filename, plot, onlyIfDoesntExist, function () {
    grDevices::setEPS(family = family)
    grDevices::postscript(filename, width = g$width, height = g$height, bg = bg, paper = paper, family = family, ...)
  })
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


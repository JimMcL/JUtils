# Functions for plotting to files
#
# V1.0

# "Private" function to run some code with output going somewhere special
.JplotToDevice <- function(filename, plotExpr, onlyIfDoesntExist, openDeviceFn, closeDevFn = grDevices::dev.off, createDir = TRUE) {
  if (!onlyIfDoesntExist || !file.exists(filename)) {

    # If directory doesn't exist, either create it or stop with an error
    if (!dir.exists(dirname(filename))) {
      if (createDir) {
        # cat(sprintf("\nAttempting to create '%s' (in %s)\n", dirname(filename), getwd()))
        dir.create(dirname(filename), recursive = TRUE)
      } else {
        stop(sprintf("Unable to write file '%s' as directory '%s' does not exist", filename, dirname(filename)))
      }
    }

    openDeviceFn()
    tryCatch({
      if (is.function(plotExpr)) {
        plotExpr()
      }
    }, finally = {
      closeDevFn()
    })
  }
}

# Calculates and returns image geometry in inches
.geometry <- function(width, height, aspectRatio, res, inUnits, outUnits) {
  # Use either explicitly specified height, or calculate from width and aspect ratio
  if (is.na(height) && !is.na(width))
    height <- width / aspectRatio
  # Or calculate width from height
  if (is.na(width) && !is.na(height))
    width <- height * aspectRatio

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
#' Writes the output of a plot to a PNG file. If you are using ggplot, try
#' either using \code{JPlotToPNG(filename, print(<plotting code>))} or else
#' \code{ggsave()}.
#'
#' @param filename The name of the PNG to create or overwrite.
#' @param plotExpr A function or expression which will produce the plot to be
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
#' @param createDirectory If TRUE and \code{filename} is located in a directory
#'   which doesn't exist, the directory will be created.
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
JPlotToPNG <- function(filename, plotExpr,
                       width = 180, height = NA, aspectRatio = 3 / 2,
                       units = c("mm", "cm", "px", "in"),
                       type = ifelse(capabilities()["cairo"], 'cairo', NULL),
                       res = 72,
                       onlyIfDoesntExist = FALSE,
                       createDirectory = TRUE,
                       ...) {
  g <- .geometry(width, height, aspectRatio, res, units, c("mm", "cm", "px", "in"))

  .JplotToDevice(filename, plotExpr, onlyIfDoesntExist, createDir = createDirectory, function () {
    grDevices::png(filename, width = g$width, height = g$height, units = g$units, type = type, res = res, ...)
  })
}

#' Plot to a TIFF file
#'
#' Writes the output of a plot to a TIFF file. If you are using ggplot, try either
#' using \code{JPlotToTIFF(filename, print(<plotting code>))} or else \code{ggsave()}.
#'
#' @param filename The name of the TIFF to create or overwrite.
#' @param plotExpr A function or expression which will produce the plot to be
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
#' @param createDirectory If TRUE and \code{filename} is located in a directory
#'   which doesn't exist, the directory will be created.
#' @param ... Any additional arguments are passed to
#'   \code{\link[grDevices]{tiff}}.
#'
#' @seealso \code{\link[grDevices]{tiff}}
#'
#' @export
JPlotToTIFF <- function(filename, plotExpr,
                        width = 180, height = NA, aspectRatio = 3 / 2,
                        units = c("mm", "cm", "px", "in"),
                        type = ifelse(capabilities()["cairo"], 'cairo', NULL),
                        res = 72,
                        onlyIfDoesntExist = FALSE,
                        createDirectory = TRUE,
                        ...) {
  g <- .geometry(width, height, aspectRatio, res, units, c("mm", "cm", "px", "in"))

  .JplotToDevice(filename, plotExpr, onlyIfDoesntExist, createDir = createDirectory, function () {
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
#' @param plotExpr A function or expression which will produce the plot to be
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
#' @param createDirectory If TRUE and \code{filename} is located in a directory
#'   which doesn't exist, the directory will be created.
#' @param ... Any additional arguments are passed to
#'   \code{\link[grDevices]{pdf}}.
#'
#' @seealso \code{\link[grDevices]{pdf}}
#'
#' @export
JPlotToPDF <- function(filename, plotExpr,
                       width = 180, height = NA, aspectRatio = 3 / 2,
                       units =  c("mm", "cm", "in"),
                       bg = "white",
                       paper = "special",
                       family = "Helvetica",
                       onlyIfDoesntExist = FALSE,
                       createDirectory = TRUE,
                       ...) {
  g <- .geometry(width, height, aspectRatio, 1, units, "in")

  .JplotToDevice(filename, plotExpr, onlyIfDoesntExist, createDir = createDirectory, function () {
    grDevices::pdf(filename, width = g$width, height = g$height, bg = bg, paper = paper, family = family, ...)
  })
}

#' Plot to an EPS (encapsulated postscript) file
#'
#' Writes the output of a plot to a EPS file. If you are using ggplot, try
#' either using \code{JPlotToEPS(filename, print(<plotting code>))} or else
#' \code{ggsave()}.
#'
#' R provides two mechanisms (or 'graphics devices') for writing postscript: the
#' standard device (\code{\link[grDevices]{postscript}}) and the Cairo device
#' (\code{\link[grDevices]{cairo_ps}}). The \code{cairo_ps} device is an
#' alternative implementation for writing postscript which handles transparency,
#' although it may output a raster bitmap rather than vector data to do so.
#' Specify a value for the argument \code{fallback_resolution} to define the
#' resolution of the bitmap. See \code{\link[grDevices]{cairo_ps}} for details.
#'
#' @param filename The name of the EPS to create or overwrite.
#' @param plotExpr A function or expression which will produce the plot to be
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
#' @param family The font family to be used. Passed to
#'   \code{\link[grDevices]{postscript}}.
#' @param onlyIfDoesntExist If TRUE and the output file already exists,
#'   \code{JPlotToEPS} will do nothing.
#' @param createDirectory If TRUE and \code{filename} is located in a directory
#'   which doesn't exist, the directory will be created.
#' @param cairo If TRUE, the \code{\link[grDevices]{cairo_ps}} device is used,
#'   otherwise the \code{\link[grDevices]{postscript}} device is used.
#' @param ... Any additional arguments are passed to
#'   \code{\link[grDevices]{postscript}} or \code{\link[grDevices]{cairo_ps}}.
#'
#' @seealso \code{\link[grDevices]{postscriptFonts}},
#'   \code{\link[grDevices]{postscript}}, \code{\link[grDevices]{cairo_ps}}
#'
#' @export
JPlotToEPS <- function(filename, plotExpr,
                       width = 180, height = NA, aspectRatio = 3 / 2,
                       units =  c("mm", "cm", "in"),
                       bg = "white",
                       paper = "special",
                       family = "Helvetica",
                       onlyIfDoesntExist = FALSE,
                       createDirectory = TRUE,
                       cairo = FALSE,
                       ...) {
  g <- .geometry(width, height, aspectRatio, 1, units, "in")

  .JplotToDevice(filename, plotExpr, onlyIfDoesntExist, createDir = createDirectory, function () {
    if (cairo)
      grDevices::cairo_ps(filename, width = g$width, height = g$height, bg = bg, family = family, ...)
    else
      grDevices::postscript(filename, width = g$width, height = g$height, bg = bg, paper = paper, family = family, ...)
  })
}

#' Plot to files
#'
#' Writes the output of a plot to a file. The type of file is deduced from the
#' extension of the file name. If you are using ggplot rather than base
#' graphics, try either using \code{JPlotToFile(filename, print(<plotting
#' code>))} or \code{ggsave()}.
#'
#' @param filenames The names of one or more files to create or overwrite.
#' @param plotExpr A function or expression which will produce the plot to be
#'   written to the file.
#' @param ... Any additional arguments are passed to the appropriate function.
#'
#' @seealso \code{\link{JPlotToPNG}}, \code{\link{JPlotToTIFF}},
#'   \code{\link{JPlotToPDF}}, \code{\link{JPlotToEPS}}
#'
#' @export
JPlotToFile <- function(filenames, plotExpr, ...) {

  .plotToFile <- function(filename, pe, ...) {
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
      tif = JPlotToTIFF,
      tiff = JPlotToTIFF
    )
    ext <- tolower(tools::file_ext(filename))
    fns[[ext]](filename, pe, ...)
  }

  # Turn the expression into a complicated function using substitute and eval,
  # otherwise weird shit happens the first time it is evaluated, i.e. it works
  # the first time through the loop, but not for subsequent iterations
  plotFn <- eval.parent(substitute(function(...) {
    v <- plotExpr
    if (is.function(v))
      v()
    }
  ))

  for (filename in filenames) {
    .plotToFile(filename, plotFn, ...)
  }
}

#' Send console (i.e. text) output to a file
#'
#' Sends the text output of an arbitrary R expression of function to a text file.
#'
#' @param filename Name of the file to write to.
#' @param expr An expression which outputs the text to be written.
#' @param createDirectory If TRUE and \code{filename} is located in a directory
#'   which doesn't exist, the directory will be created.
#'
#' @examples
#' JReportToFile("test.txt", print("Hello world!"))
#'
#' @export
JReportToFile <- function(filename, expr, createDirectory = TRUE) {
  oldOptions <- options()
  on.exit(options(oldOptions))
  # Normally, text output is wrapped based on the console window size. This
  # doesn't make sense when writing to a file, so make the width very big
  options(width = 10000)

  .JplotToDevice(filename, expr, onlyIfDoesntExist = FALSE, createDir = createDirectory,
                 openDeviceFn = function () { sink(filename) },
                 closeDevFn = sink)
}


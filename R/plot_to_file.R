# Functions for plotting to files
#

# "Private" function to run some code with output going somewhere special.
# The result of evaulating the code is returned invisibly.
.JplotToDevice <- function(filename, plotExpr, onlyIfDoesntExist, openDeviceFn, closeDevFn = grDevices::dev.off, createDir = TRUE) {
  if (!onlyIfDoesntExist || !file.exists(filename)) {

    # If file name is NULL, just plot or print to screen
    if (is.null(filename)) {
      openDeviceFn <- closeDevFn <- function() {}
    } else {
      # If directory doesn't exist, either create it or stop with an error
      if (!dir.exists(dirname(filename))) {
        if (createDir) {
          # cat(sprintf("\nAttempting to create '%s' (in %s)\n", dirname(filename), getwd()))
          dir.create(dirname(filename), recursive = TRUE)
        } else {
          stop(sprintf("Unable to write file '%s' as directory '%s' does not exist", filename, dirname(filename)))
        }
      }
    }

    openDeviceFn()
    result <- tryCatch({
      if (is.function(result <- plotExpr)) {
        plotExpr()
      } else {
        result
      }
    }, finally = {
      closeDevFn()
    })
    invisible(result)
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
#' @param filename The name of the PNG to create or overwrite. If NULL, plot
#'   output goes to the current device.
#' @param plotExpr A function or expression which will produce the plot to be
#'   written to the file.
#' @param width The width of the output PNG file in \code{units}.
#' @param height The height of the output PNG file in \code{units}. Defaults to
#'   \code{width / aspectRatio}. If \code{height} is specified,
#'   \code{aspectRatio} will be ignored.
#' @param aspectRatio Aspect ratio (\code{width / height}) of the output PNG
#'   file.
#' @param units Units of \code{width} and \code{height}. Note that defaults
#'   units are pixels (\code{px}).
#' @param tryToUseRagg If \code{TRUE} (the default) and the \code{ragg} package
#'   is installed, the PNG file will be created using
#'   \code{\link[ragg]{agg_png}}, otherwise \code{\link[grDevices]{png}} will be
#'   used. See \url{https://ragg.r-lib.org} for more information.
#' @param type Plotting device; passed directly to \code{\link[grDevices]{png}}.
#'   Defaults to "cairo" if that is an available device since it produces nicer
#'   looking graphics. Ignored if \code{tryToUseRagg == TRUE} and \code{ragg} is
#'   installed. Note that R can be compiled with Cairo support, but it still
#'   won't work on MacOS if Quartz is not installed.
#' @param res The nominal resolution in ppi. The value is simply passed in to
#'   \code{\link[ragg]{agg_png}} or \code{\link[grDevices]{png}}. Increasing the
#'   resolution will increase the size (in pixels) of the text and graph
#'   elements.
#' @param onlyIfDoesntExist If TRUE and the output file already exists,
#'   \code{JPlotToPNG} will do nothing.
#' @param createDirectory If TRUE and \code{filename} is located in a directory
#'   which doesn't exist, the directory will be created.
#' @param ... Any additional arguments are passed to either
#'   \code{\link[ragg]{agg_png}} or \code{\link[grDevices]{png}}.
#'
#' @return The result of evaluating \code{plotExpr} is returned invisibly (which
#'   means it is not automatically printed).
#'
#' @seealso \code{\link[ragg]{agg_png}}, \code{\link[grDevices]{png}}
#'
#' @examples
#' \dontrun{
#' JPlotToPNG("test.png", plot(rnorm(50), type = 'l', main = 'Wiggly line'))
#'
#' # Plot to a PNG file with width 180 mm, height 120 mm
#' # (i.e. height / aspectRatio which defaults to (3 / 2)), resolution 300 ppi.
#' # This results in a PNG file with size 2125x1417 pixels
#' JPlotToPNG("test.png", plot(1:10 + rnorm(10), type = "o"), width = 180, units = "mm", res = 300)
#' }
#'
#' @export
JPlotToPNG <- function(filename, plotExpr,
                       width = 600, height = NA, aspectRatio = 3 / 2,
                       units = c("px", "mm", "cm", "in"),
                       tryToUseRagg = TRUE,
                       type = ifelse(capabilities()["cairo"], 'cairo', NULL), # Unfortunately, this test can't determine whether Quartz is actually installed!
                       res = 72,
                       onlyIfDoesntExist = FALSE,
                       createDirectory = TRUE,
                       ...) {
  g <- .geometry(width, height, aspectRatio, res, units, c("mm", "cm", "px", "in"))

  if (tryToUseRagg && requireNamespace("ragg", quietly = TRUE)) {
    .JplotToDevice(filename, plotExpr, onlyIfDoesntExist, createDir = createDirectory, function() {
      ragg::agg_png(filename, width = g$width, height = g$height, units = g$units, res = res, ...)
    })
  } else {
    .JplotToDevice(filename, plotExpr, onlyIfDoesntExist, createDir = createDirectory, function() {
      grDevices::png(filename, width = g$width, height = g$height, units = g$units, type = type, res = res, ...)
    })
  }
}

#' Plot to a JPEG file
#'
#' Writes the output of a plot to a JPEG file. If you are using ggplot, try
#' either using \code{JPlotToJPEG(filename, print(<plotting code>))} or else
#' \code{ggsave()}.
#'
#' @param filename The name of the JPEG to create or overwrite. If NULL, plot
#'   output goes to the current device.
#' @param plotExpr A function or expression which will produce the plot to be
#'   written to the file.
#' @param width The width of the output JPEG file in \code{units}.
#' @param height The height of the output JPEG file in \code{units}. Defaults to
#'   \code{width / aspectRatio}. If \code{height} is specified,
#'   \code{aspectRatio} will be ignored.
#' @param aspectRatio Aspect ratio (\code{width / height}) of the output JPEG
#'   file.
#' @param units Units of \code{width} and \code{height}. Note that defaults
#'   units are pixels (\code{px}).
#' @param tryToUseRagg If \code{TRUE} (the default) and the \code{ragg} package
#'   is installed, the JPEG file will be created using
#'   \code{\link[ragg]{agg_jpeg}}, otherwise \code{\link[grDevices]{jpeg}} will
#'   be used. See \url{https://ragg.r-lib.org} for more information.
#' @param type Plotting device; passed directly to \code{\link[grDevices]{jpeg}}.
#'   Defaults to "cairo" if that is an available device since it produces nicer
#'   looking graphics. Ignored if \code{tryToUseRagg == TRUE} and \code{ragg} is
#'   installed.
#' @param res The nominal resolution in ppi. The value is passed unchanged in to
#'   \code{\link[ragg]{agg_jpeg}} or \code{\link[grDevices]{jpeg}}. Increasing the
#'   resolution will increase the size (in pixels) of the text and graph
#'   elements.
#' @param onlyIfDoesntExist If TRUE and the output file already exists,
#'   \code{JPlotToJPEG} will do nothing.
#' @param createDirectory If TRUE and \code{filename} is located in a directory
#'   which doesn't exist, the directory will be created.
#' @param ... Any additional arguments are passed to \code{\link[ragg]{agg_jpeg}} or
#'   \code{\link[grDevices]{jpeg}}.
#'
#' @return The result of evaluating \code{plotExpr} is returned invisibly (which
#'   means it is not automatically printed).
#'
#' @seealso \code{\link[ragg]{agg_jpeg}}, \code{\link[grDevices]{jpeg}}
#'
#' @examples
#' \dontrun{
#' JPlotToJPEG("test.jpg", plot(rnorm(50), type = 'l', main = 'Wiggly line'))
#'
#' # Plot to a JPEG file with width 180 mm, height 120 mm
#' # (i.e. height / aspectRatio which defaults to (3 / 2)), resolution 300 ppi.
#' # This results in a JPEG file with size 2125x1417 pixels
#' JPlotToJPEG("test.jpg", plot(1:10 + rnorm(10), type = "o"), width = 180, units = "mm", res = 300)
#' }
#'
#' @export
JPlotToJPEG <- function(filename, plotExpr,
                       width = 600, height = NA, aspectRatio = 3 / 2,
                       units = c("px", "mm", "cm", "in"),
                       tryToUseRagg = TRUE,
                       type = ifelse(capabilities()["cairo"], 'cairo', NULL),
                       res = 72,
                       onlyIfDoesntExist = FALSE,
                       createDirectory = TRUE,
                       ...) {
  g <- .geometry(width, height, aspectRatio, res, units, c("mm", "cm", "px", "in"))

  if (tryToUseRagg && requireNamespace("ragg", quietly = TRUE)) {
    .JplotToDevice(filename, plotExpr, onlyIfDoesntExist, createDir = createDirectory, function() {
      ragg::agg_jpeg(filename, width = g$width, height = g$height, units = g$units, res = res, ...)
    })
  } else {
    .JplotToDevice(filename, plotExpr, onlyIfDoesntExist, createDir = createDirectory, function() {
      grDevices::jpeg(filename, width = g$width, height = g$height, units = g$units, type = type, res = res, ...)
    })
  }
}

#' Plot to a TIFF file
#'
#' Writes the output of a plot to a TIFF file. If you are using ggplot, try
#' either using \code{JPlotToTIFF(filename, print(<plotting code>))} or else
#' \code{ggsave()}.
#'
#' @param filename The name of the TIFF to create or overwrite. If NULL, plot output goes to the current device.
#' @param plotExpr A function or expression which will produce the plot to be
#'   written to the file.
#' @param width The width of the output TIFF file in \code{units}.
#' @param height The height of the output TIFF file in \code{units}. Defaults to
#'   \code{width / aspectRatio}. If \code{height} is specified,
#'   \code{aspectRatio} will be ignored.
#' @param aspectRatio Aspect ratio (\code{width / height}) of the output TIFF
#'   file.
#' @param units Units of \code{width} and \code{height}.
#' @param tryToUseRagg If \code{TRUE} (the default) and the \code{ragg} package
#'   is installed, the TIFF file will be created using
#'   \code{\link[ragg]{agg_tiff}}, otherwise \code{\link[grDevices]{tiff}} will be
#'   used. See \url{https://ragg.r-lib.org} for more information.
#' @param type Plotting device; passed directly to \code{\link[grDevices]{tiff}}.
#'   Defaults to "cairo" if that is an available device since it produces nicer
#'   looking graphics. Ignored if \code{tryToUseRagg == TRUE} and \code{ragg} is
#'   installed.
#' @param res The nominal resolution in ppi. The value is passed in to
#'   \code{\link[grDevices:png]{grDevices::tiff()}}. Increasing the resolution
#'   will increase the size (in pixels) of the text and graph elements.
#' @param onlyIfDoesntExist If TRUE and the output file already exists,
#'   \code{JPlotToTIFF} will do nothing.
#' @param createDirectory If TRUE and \code{filename} is located in a directory
#'   which doesn't exist, the directory will be created.
#' @param ... Any additional arguments are passed to
#'   \code{\link[ragg]{agg_tiff}} or \code{\link[grDevices]{tiff}}.
#'
#' @return The result of evaluating \code{plotExpr} is returned invisibly (which
#'   means it is not automatically printed).
#'
#' @seealso \code{\link[ragg]{agg_tiff}}, \code{\link[grDevices]{tiff}}
#'
#' @export
JPlotToTIFF <- function(filename, plotExpr,
                        width = 180, height = NA, aspectRatio = 3 / 2,
                        units = c("mm", "cm", "px", "in"),
                        tryToUseRagg = TRUE,
                        type = ifelse(capabilities()["cairo"], 'cairo', NULL),
                        res = 72,
                        onlyIfDoesntExist = FALSE,
                        createDirectory = TRUE,
                        ...) {
  g <- .geometry(width, height, aspectRatio, res, units, c("mm", "cm", "px", "in"))

  if (tryToUseRagg && requireNamespace("ragg", quietly = TRUE)) {
    .JplotToDevice(filename, plotExpr, onlyIfDoesntExist, createDir = createDirectory, function() {
      ragg::agg_tiff(filename, width = g$width, height = g$height, units = g$units, res = res, ...)
    })
  } else {
    .JplotToDevice(filename, plotExpr, onlyIfDoesntExist, createDir = createDirectory, function() {
      grDevices::tiff(filename, width = g$width, height = g$height, units = g$units, type = type, res = res, ...)
    })
  }
}

#' Plot to a PDF file
#'
#' Writes the output of a plot to a PDF file. If you are using ggplot, try
#' either using \code{JPlotToPDF(filename, print(<plotting code>))} or else
#' \code{ggsave()}.
#'
#' The \code{\link[grDevices]{pdf}} argument \code{pointsize} can be specified
#' to control the resolution of the image (as can all other arguments to
#' \code{pdf}.)
#'
#' The pdf device does \emph{not} embed fonts in the pdf file. This probably
#' does not matter if you use one of the device-independent font families,
#' \code{"sans"}, \code{"serif"} and \code{"mono"}, because an appropriate (but
#' not necessarily identical) font will be used by the viewer. Alternatively,
#' specifying \code{cairo = TRUE} \emph{may} embed the font, although the
#' documentation for \code{\link[grDevices]{cairo_pdf}} is confusing.
#'
#' @param filename The name of the PDF to create or overwrite. If NULL, plot
#'   output goes to the current device.
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
#' @param paper Paper size, defaults to "special" which is the value it must
#'   have if \code{width} and \code{height} are to be used. Ignored if
#'   \code{cairo} is \code{TRUE}.
#' @param family The font family to be used. Consider using \code{"sans"},
#'   \code{"serif"} and \code{"mono"} for consistent viewing results across
#'   devices.
#' @param onlyIfDoesntExist If TRUE and the output file already exists,
#'   \code{JPlotToPDF} will do nothing.
#' @param createDirectory If TRUE and \code{filename} is located in a directory
#'   which doesn't exist, the directory will be created.
#' @param cairo If TRUE, the \code{\link[grDevices]{cairo_pdf}} device is used,
#'   otherwise the \code{\link[grDevices]{pdf}} device is used (see Details).
#' @param embedFonts If TRUE, \code{\link[grDevices]{embedFonts}} is called on
#'   the file after creation. This is provided as a convenience. Ghostscript
#'   must be installed and locatable (see \code{\link[grDevices]{embedFonts}}).
#'   I \emph{think} this is unnecessary when using the Cairo PDF device.
#' @param ... Any additional arguments are passed to
#'   \code{\link[grDevices]{pdf}}, for example, text and font control parameters
#'   such as \code{pointsize}.
#'
#' @return The result of evaluating \code{plotExpr} is returned invisibly (which
#'   means it is not automatically printed).
#'
#' @seealso \code{\link[grDevices]{pdf}}, \code{\link[grDevices]{cairo_pdf}},
#'   \code{\link[grDevices]{embedFonts}}
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
                       cairo = FALSE,
                       embedFonts = FALSE,
                       ...) {
  g <- .geometry(width, height, aspectRatio, 1, units, "in")

  r <- .JplotToDevice(filename, plotExpr, onlyIfDoesntExist, createDir = createDirectory, function() {
    if (cairo)
      grDevices::cairo_pdf(filename, width = g$width, height = g$height, bg = bg, family = family, ...)
    else
      grDevices::pdf(filename, width = g$width, height = g$height, bg = bg, paper = paper, family = family, ...)
  })

  # Optionally embed fonts
  if (embedFonts) {
    embedFonts(filename)
  }

  invisible(r)
}

#' Plot to an EPS (encapsulated postscript) file
#'
#' Writes the output of a plot to a EPS file. If you are using ggplot, try
#' either using \code{JPlotToEPS(filename, print(<plotting code>))} or else
#' \code{ggsave()}.
#'
#' R provides two mechanisms (or 'graphics devices') for writing postscript: the
#' standard device (\code{\link[grDevices]{postscript}}) and the Cairo device
#' (\code{\link[grDevices]{cairo}}). The \code{cairo} device is an
#' alternative implementation for writing postscript that handles transparency,
#' although it may output a raster bitmap rather than vector data to do so.
#' Specify a value for the argument \code{fallback_resolution} to define the
#' resolution of the bitmap. See \code{\link[grDevices]{cairo}} for details.
#'
#' @param filename The name of the EPS to create or overwrite. If NULL, plot output goes to the current device.
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
#' @param cairo If TRUE, the \code{\link[grDevices]{cairo}} device is used,
#'   otherwise the \code{\link[grDevices]{postscript}} device is used (see Details).
#' @param ... Any additional arguments are passed to
#'   \code{\link[grDevices]{postscript}} or \code{\link[grDevices]{cairo}}.
#'
#' @return The result of evaluating \code{plotExpr} is returned invisibly (which
#'   means it is not automatically printed).
#'
#' @seealso \code{\link[grDevices]{postscriptFonts}},
#'   \code{\link[grDevices]{postscript}}, \code{\link[grDevices]{cairo}}
#'
#' @examples
#'\dontrun{
#' plotWithAlpha <- function() {
#'     plot(1:3, type = 'l', lwd = 20, col = "#ffcc8888")
#'     lines(3:1, lwd = 20, col = "#88ccff88")
#' }
#'
#' # This will generate a warning and an empty plot due to transparency
#' JPlotToEPS(filename, plotWithAlpha)
#' # This will work (more or less) as expected
#' JPlotToEPS(filename, plotWithAlpha, cairo = TRUE, fallback_resolution = 600)
#'}
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

  .JplotToDevice(filename, plotExpr, onlyIfDoesntExist, createDir = createDirectory, function() {
    if (cairo)
      grDevices::cairo_ps(filename, width = g$width, height = g$height, bg = bg, family = family, ...)
    else
      grDevices::postscript(filename, width = g$width, height = g$height, bg = bg, paper = paper, family = family, ...)
  })
}

#' Plot to an SVG file
#'
#' Writes the output of a plot to a SVG file. If you are using ggplot, try either
#' using \code{JPlotToSVG(filename, print(<plotting code>))} or else
#' \code{ggsave()}.
#'
#' @param filename The name of the SVG file to create or overwrite. If NULL, plot output goes to the current device.
#' @param plotExpr A function or expression which will produce the plot to be
#'  written to the file.
#' @param width The width of the output SVG file in \code{units}.
#' @param height The height of the output SVG file in \code{units}. Defaults to
#'  \code{width / aspectRatio}. If \code{height} is specified,
#'  \code{aspectRatio} will be ignored.
#' @param units Units of \code{width} and \code{height}.
#' @param aspectRatio Aspect ratio (\code{width / height}) of the output EPS
#'  file.
#' @param bg Background colour - may be "transparent" for no background.
#' @param family The font family to be used. Passed to
#'  \code{\link[grDevices]{svg}}.
#' @param onlyIfDoesntExist If TRUE and the output file already exists,
#'  \code{JPlotToEPS} will do nothing.
#' @param createDirectory If TRUE and \code{filename} is located in a directory
#'  which doesn't exist, the directory will be created.
#' @param ... Any additional arguments are passed to
#'  \code{\link[grDevices]{svg}}. These may be used to control text size
#'  (\code{pointsize}), font family (\code{family}) and so on.
#'
#' @return The result of evaluating \code{plotExpr} is returned invisibly (which
#'  means it is not automatically printed).
#'
#' @seealso \code{\link[grDevices]{cairo}}
#'
#' @examples
#'\dontrun{
#' plotWithAlpha <- function() {
#'     plot(1:3, type = 'l', lwd = 20, col = "#ffcc8888")
#'     lines(3:1, lwd = 20, col = "#88ccff88")
#' }
#'
#' JPlotToSVG(filename, plotWithAlpha)
#'}
#'
#' @export
JPlotToSVG <- function(filename, plotExpr,
                       width = 180, height = NA, aspectRatio = 3 / 2,
                       units =  c("mm", "cm", "in"),
                       bg = "white",
                       family = "Helvetica",
                       onlyIfDoesntExist = FALSE,
                       createDirectory = TRUE,
                       ...) {
  g <- .geometry(width, height, aspectRatio, 1, units, "in")

  .JplotToDevice(filename, plotExpr, onlyIfDoesntExist, createDir = createDirectory, function() {
      grDevices::svg(filename, width = g$width, height = g$height, bg = bg, family = family, ...)
  })
}

#' Plot to files
#'
#' Writes the output of a plot to a file. The type of file is deduced from the
#' extension of the file name. If you are using ggplot rather than base
#' graphics, try either using \code{JPlotToFile(filename, print(<plotting
#' code>))} or \code{ggsave()}.
#'
#' @param filenames The names of one or more files to create or overwrite. If NULL, plot output goes to the current device.
#' @param plotExpr A function or expression which will produce the plot to be
#'   written to the file.
#' @param ... Any additional arguments are passed to the appropriate function.
#'
#' @return The result of evaluating \code{plotExpr} when plotting to the last
#'   file in the \code{filenames} vector is returned invisibly (which means it
#'   is not automatically printed).
#'
#' @seealso \code{\link{JPlotToPNG}}, \code{\link{JPlotToTIFF}},
#'   \code{\link{JPlotToPDF}}, \code{\link{JPlotToEPS}}, \code{\link{JPlotToSVG}}
#'
#' @export
JPlotToFile <- function(filenames, plotExpr, ...) {

  .plotToFile <- function(filename, pe, ...) {
    fns <- list(
      eps =  JPlotToEPS,
      ps =   JPlotToEPS,
      pdf =  JPlotToPDF,
      svg =  JPlotToSVG,
      #emf =  ,
      #wmf =  ,
      png =  JPlotToPNG,
      jpg =  JPlotToJPEG,
      jpeg = JPlotToJPEG,
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
    if (is.function(result <- v))
      result <- v()
    result
  }))

  for (filename in filenames) {
    result <- .plotToFile(filename, plotFn, ...)
  }
  result
}

#' Send console (i.e. text) output to a file
#'
#' Sends the text output of an arbitrary R expression or function to a text file.
#'
#' @param filename Name of the file to write to. If NULL, output destination is unaffected by this function.
#' @param expr An expression which outputs the text to be written.
#' @param createDirectory If TRUE and \code{filename} is located in a directory
#'   which doesn't exist, the directory will be created.
#'
#' @return The result of evaluating \code{plotExpr} is returned invisibly (which
#'   means it is not automatically printed).
#'
#' @seealso \code{\link{sink}}, \code{\link[utils]{capture.output}} for saving text output into a variable.
#'
#' @examples
#' \dontrun{
#' JReportToFile("test.txt", print("Hello world!"))
#' }
#'
#' @export
JReportToFile <- function(filename, expr, createDirectory = TRUE) {
  oldOptions <- options()
  on.exit(options(oldOptions), add = TRUE, after = FALSE)
  # Normally, text output is wrapped based on the console window size. This
  # doesn't make sense when writing to a file, so make the width very big
  options(width = 10000)

  .JplotToDevice(filename, expr, onlyIfDoesntExist = FALSE, createDir = createDirectory,
                 openDeviceFn = function() { sink(filename) },
                 closeDevFn = sink)
}


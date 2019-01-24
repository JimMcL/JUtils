[![Travis-CI Build Status](https://travis-ci.org/JimMcL/JUtils.svg?branch=master)](https://travis-ci.org/JimMcL/JUtils)
[![codecov](https://codecov.io/gh/JimMcL/JUtils/branch/master/graph/badge.svg)](https://codecov.io/gh/JimMcL/JUtils)

# JUtils

R utilities to simplify some common operations.

## Getting started

* [Installation](#installation)
* [Functionality](#functionality)
  * [Plotting to a file](#plotting-to-a-file)
  * [Creating an animation](#creating-an-animation)
  * [Adding a raster image to a plot](#adding-a-raster-image-to-a-plot)
  * [Printing text to a file](#printing-text-to-a-file)
  * [Downloading files](#downloading-files)
  * [String functions](#string-functions)

## Installation
JUtils is not available on CRAN, so it must be installed from Github.

    $ install.packages("devtools")
    $ devtools::install_github("JimMcL/JUtils")

### Installation failures

If you are installing on MacOS, and installation fails with messages like
    
    Setting LC_CTYPE failed, using "C"
    
Open a terminal window, enter

    `defaults write org.R-project.R force.LANG en_US.UTF-8` 
    
then restart R, as described [here](https://cran.r-project.org/bin/macosx/RMacOSX-FAQ.html#Internationalization-of-the-R_002eapp).

## Functionality

### Plotting to a file

`JUtils` provides functions which wrap the standard R functions (from the core `grDevices` package) for plotting to files using base graphics. `JUtils`  provides more flexibility in specifying file sizes, and hides some of the more arcane aspects of the standard functions, yet still provides access to all of the underlying functionality. All functions accept any "real world" units (mm, cm & in). Raster functions (`JPlotToPNG` and `JPlotToTIFF`) also accept pixel ("px") units. For a nice discussion about printing issues in R, see (https://blog.revolutionanalytics.com/2009/01/10-tips-for-making-your-r-graphics-look-their-best.html).


    library("JUtils")

    # Plot to a PNG file with width 180 mm, height 120 mm 
    # (i.e. height / aspectRatio which defaults to (3 / 2)), resolution 300 ppi.
    # This results in a PNG file with size 2125x1417 pixels
    JPlotToPNG("test.png", plot(1:10 + rnorm(10), type = "o"), width = 180, units = "mm", res = 300)
    
    # Plot to TIFF, with image dimensions specified in pixels
    JPlotToTIFF("test.tiff", plot(1:10 + rnorm(10), type = "o"), width = 1800, height = 1200, units = "px") 
    
    # Plot to a PDF file, with graphics region 18 cm x 12 cm
    JPlotToPDF("test.pdf", plot(1:10 + rnorm(10), type = "o"), width = 18, units = "cm")
    
    # Generate encapsulated postscript using the Cairo graphics device
    JPlotToPDF("test.eps", plot(1:10 + rnorm(10), type = "o"), cairo = TRUE, width = 18, units = "cm")

    # JPlotToFile selects the file type based on the file name, then passes all of its arguments 
    # on to the appropriate JPlotTo* function. It can also plot to multiple files at once.
    JPlotToFile(c("test.png", "test.eps"), plot(1:10))

### Creating an animation

The "standard" method to create an animation in R is to generate a set of images and then use the 
ImageMagick utility to combine them into an animated GIF file. `JAnimateGIF` provides a relatively simple and 
robust way to do this.

    library("JUtils")

    .plotFrame <- function(angle) plot(x = c(sin(angle), 0), y = c(0, cos(angle)), 
                                       type = 'l', lwd = 4, 
                                       xlim = c(-1, 1), ylim = c(-1, 1), 
                                       axes = FALSE, xlab = "", ylab = "")
    JAnimateGIF(frameKeys = seq(0, pi * 2, .1), gifFileName = "test.gif", plotFn = .plotFrame)


### Adding a raster image to a plot

A raster image (i.e. a GIF of JPEG file) can be drawn to a plot using `graphics::rasterImage`. However, if you want to draw it with the correct aspect ratio, calculating the required position is not straightforward. `JPlotRaster` is a wrapper around `rasterImage` which calculates the correct position of the image, then draws it, based on a point and the desired width of the plotted image. The point defaults to the position of the centre of the image, but can alternatively be a corner or point on the middle of an edge.

    library(jpeg)
    library("JUtils")
    
    plot(...)
    img <- readJPEG("myjpeg.jpg", native = TRUE)
    # Draw image centred on (0, 0)
    JPlotRaster(img, x = 0, y = 0, width = 2)
    # Draw image with top-left corner at (2, 1)
    JPlotRaster(img, x = 2, y = 1, width = 2, position = "topleft")

### Printing text to a file

The function `JReportToFile` runs a function or an expression, and redirects all output to a file.

    library("JUtils")

    # Write "Hello world!" to the file called test.txt
    JReportToFile("test.txt", print("Hello world!"))

### Downloading files

The function `JDownload` is used to download a URL to a local file. The local file is tracked so that a second call to `JDownload` with the same URL will not download it again, but simply return the name of the previously downloaded file. However, it will be downloaded again if it has changed since the last download.

    library("JUtils")

    # Fairly slow the first time
    jpgFile <- JDownload("https://farm5.staticflickr.com/4507/37847388931_959d812490_o_d.jpg")

    # Quick the second time
    jpgFile <- JDownload("https://farm5.staticflickr.com/4507/37847388931_959d812490_o_d.jpg")


### String functions

Ever wanted to convert a vector of values to a human-readable list? Try JToSentence!

    library("JUtils")

    print(JToSentence(c("apple", "banana", "mandarin", "mango")))
    # => [1] "apple, banana, mandarin and mango"
    
Capitalise a string or set of words.

    print(JCapitalise("the quick brown fox"))
    # => [1] "The quick brown fox"
    print(JCapWords("the quick brown fox"))
    # => [1] "The Quick Brown Fox"

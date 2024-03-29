<!-- badges: start -->
[![check-standard](https://github.com/JimMcL/JUtils/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/JimMcL/JUtils/actions/workflows/check-standard.yaml)
[![codecov](https://codecov.io/gh/JimMcL/JUtils/branch/master/graph/badge.svg?token=WIUHIx9qiK)](https://codecov.io/gh/JimMcL/JUtils)
<!-- badges: end -->

# JUtils

R utilities to simplify some common operations.

## Getting started

* [Installation](#installation)
* [Functionality](#functionality)
  * [Plotting to a file](#plotting-to-a-file)
  * [Creating an animation](#creating-an-animation)
  * [Multi-scene animations](#multi-scene-animations)
  * [Adding a raster image to a plot](#adding-a-raster-image-to-a-plot)
  * [Plotting a list of probability densities](#plotting-a-list-of-probability-densities)
  * [Writing text to a file](#writing-text-to-a-file)
  * [Downloading files](#downloading-files)
  * [String functions](#string-functions)
  * [Fill in missing values](#fill-missing-values)
  * [Progress bar](#progress-bar)

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

`JUtils` provides functions which wrap the standard R functions (from the core `grDevices` package) for plotting to files using base graphics. `JUtils`  provides more flexibility in specifying file sizes, and hides some of the more arcane aspects of the standard functions, yet still provides access to all of the underlying functionality. All functions accept any "real world" units (mm, cm & in). Raster functions (`JPlotToPNG`, `JPlotToJPEG` and `JPlotToTIFF`) also accept pixel ("px") units. For a nice discussion about printing issues in R, see https://blog.revolutionanalytics.com/2009/01/10-tips-for-making-your-r-graphics-look-their-best.html.

Be aware that if you use `par(mfrow = ...)` to plot with 2 or more rows or columns, R automatically reduces `cex`. If you are unaware of this behaviour, it can cause major headaches when creating multiple plots for a publication with different numbers of panels. One solution is to explicitly set `par(cex = ...)` _after_ setting `par(mfrow = ...)`.

By default, if the [ragg](https://ragg.r-lib.org/) package is installed, `JUtils` will use it for plotting to PNG, JPEG or TIFF files. If [ragg](https://ragg.r-lib.org/) is not installed, `JUtils` will use the standard `grDevices` functionality. The [ragg](https://ragg.r-lib.org/) package may be faster than `grDevices`, has better font support (e.g. `par(family = "Segoe UI")` works as expected), and may produce better quality plots, although plot quality may sometimes be a subjective matter.

```R
library("JUtils")

# Plot to a PNG file with width 180 mm, height 120 mm 
# (i.e. height / aspectRatio which defaults to (3 / 2)), resolution 300 ppi.
# This results in a PNG file with size 2125x1417 pixels
JPlotToPNG("test.png", plot(1:10 + rnorm(10), type = "o"), width = 180, units = "mm", res = 300)
    
# Plot to JPEG, with image dimensions specified in pixels
JPlotToTIFF("test.jpg", plot(1:10 + rnorm(10), type = "o"), width = 1800, height = 1200, units = "px") 
    
# Plot to TIFF, with image dimensions specified in pixels
JPlotToTIFF("test.tiff", plot(1:10 + rnorm(10), type = "o"), width = 1800, height = 1200, units = "px") 
    
# Plot to a PDF file, with graphics region 18 cm x 12 cm
JPlotToPDF("test.pdf", plot(1:10 + rnorm(10), type = "o"), width = 18, units = "cm")
    
# Generate encapsulated postscript using the Cairo graphics device
JPlotToEPS("test.eps", plot(1:10 + rnorm(10), type = "o"), cairo = TRUE, width = 18, units = "cm")

# Plot to a SVG file with width 180 mm, height 120 mm 
JPlotToSVG("test.svg", plot(1:10 + rnorm(10), type = "o"), width = 180, units = "mm")

# JPlotToFile selects the file type based on the file name, then passes all of its arguments 
# on to the appropriate JPlotTo* function. It can also plot to multiple files at once.
JPlotToFile(c("test.png", "test.eps"), plot(1:10))
```

#### Transparency and Postscript

By default, partial transparency cannot be used in postscript or PDF files. The compromise solution is to specify the arguments `cairo = TRUE, fallback_resolution = 600` to `JPlotToPDF` or `JPlotToEPS`. Use a fallback resolution that is appropriate for your needs. This is a compromise because a raster image is written to the file, so it is no longer infinitely scaleable. See the help for `JPlotToPDF` or `JPlotToEPS` for more information.

#### Postscript and embedded fonts

By default, fonts are not embedded in PDF plot output. This may not matter if you specify a device-independent font family: `"sans"`, `"serif"` or `"mono"`, or if you know where the file will be viewed. If linked fonts are unacceptable, e.g. you are required to produce a [PDF/A](https://en.wikipedia.org/wiki/PDF/A) file, then:

 * you can attempt to embed fonts by passing `embedFonts = TRUE` to `JPlotToPDF`, or equivalently, calling `grDevices::embedFonts` after creating your PDF file (both methods require [Ghostscript](https://www.ghostscript.com/) to be installed);
 * you can specify `cairo = TRUE` when calling `JPlotToPDF` or `JPlotToEPS`, which uses the `grDevices::cairo_pdf`. The `cairo_pdf` device converts to bitmapped text. Obviously this is a compromise solution since bitmapped text is not infinitely scalable, and will only work if the Cairo device is available. For a brief discussion of these issues, see https://hansjoerg.me/2018/02/15/font-embedding-for-latex-and-r-users/.

#### Plotting on macOS

On macOS (and perhaps other Unix-like systems), the Cairo library might not be installed by default, so attempts to plot to TIFF or PNG files will fail with an error message such as '`failed to load cairo DLL`'. The simplest solution is to install and use `ragg` (which prevents the use of Cairo). Alternatively, specify the argument `type = "Xlib"` or `type = "quartz"` to `JPlotToTIFF` or `JPlotToPNG`. This may require you to install XQuartz (or Xlib). See [https://www.xquartz.org/](https://www.xquartz.org/). As far as I am aware, SVG can only be produced if Cairo is available.

---

### Creating an animation

The "standard" method to create an animation in R is to generate a set of images and then use the 
ImageMagick utility to combine them into an animated GIF file. `JAnimateGIF` provides a relatively simple and 
robust way to do this. You must have either the `magick` R package installed, the `gifski` package installed, or else have the `ImageMagick` command line application installed and accessible on the PATH.

You provide a function to plot a single frame given the frame key, and `JAnimateGIF` will write each frame to a PNG file then combine them into a GIF.

```R
library("JUtils")

.plotFrame <- function(angle) plot(x = c(sin(angle), 0), y = c(0, cos(angle)), 
                                   type = 'l', lwd = 4, 
                                   xlim = c(-1, 1), ylim = c(-1, 1), 
                                   axes = FALSE, xlab = "", ylab = "")
JAnimateGIF("test.gif", frameKeys = seq(0, pi * 2, .1), , plotFn = .plotFrame)
```

#### Multi-scene animations

Complex animations can consist of multiple scenes involving smooth transitions. Multi-scene animations build upon `JAnimateGIF`, which means that an appropriate tool for generating GIFs must be installed and accessible.

An animation is constructed by building a list of scenes.  A scene
is a parameterised plotting function, together with a set of
transitions. A transition defines how the values of a parameter
change over time during the scene.

Transitions make use of bezier curves for flexible smooth
transitions.  Transitions are largely inspired by CSS animations. See https://cubic-bezier.com/

The following example creates an animation with two scenes, a pink rectangle that fades into a blue triangle.

```R
library("JUtils")

basePlot <- function() {
  par(mar = c(0, 0, 0, 0))
  plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
}

# 30 frames per second (must be the same in all scenes)
fps <- 30

# Build a list of scenes
scenes <- list(

  # Scene 1, pink square, 
  JScene(1,   # Duration 1 second
         fps,
         
         # Define parameters that will change (transition) within the scene
         width = JTransition(.1, 1, JEaseInOut),
         height = JTransition(1, .1, JEaseInOut),
         alpha = JTransition(1, 0, JEaseIn, times = c(0.6, 1)),
         
         # Define a function that can plot a single frame, 
         # given the approriate parameter values
         plotFn = function(width, height, alpha) {
           # Initialise the plot
           basePlot()
           # Draw the rectangle
           rect(0.5 - width / 2, 0.5 - height / 2, 0.5 + width / 2, 0.5 + height / 2, 
                density = NA, col = rgb(0.8, .05, 0.4, alpha))
         }
  ),
  
  # Scene 2, blue triangle
  JScene(1, # Duration 1 second
         fps, 
         startAfter = -0.4, # start part-way through previous scene
         
         # Animate triangle width and transparency (alpha)
         width = JTransition(.1, 0.8, JEaseInOut),
         alpha = JTransition(0, 1, JEaseIn, times = c(0, 0.4)),
         
         plotFn = function(width, alpha, add) {
           if (!add)
             basePlot()
           polygon(x = c(0.5, 0.5 - width / 2, 0.5 + width / 2),
                   y = c(0.8, 0.2, 0.2),
                   density = NA, col = rgb(66 / 255, 200 / 255, 245 / 255, alpha))
         }
  )
)

# Generate the animation, loop indefinitely
JAnimateScenes("animated.gif", scenes, loop = 0)
```

---

### Adding a raster image to a plot

A raster image (i.e. a GIF of JPEG file) can be drawn to a plot using `graphics::rasterImage`. However, if you want to draw it with the correct aspect ratio, calculating the required position is not straightforward. `JPlotRaster` is a wrapper around `rasterImage` which calculates the correct position of the image, then draws it, based on a point and the desired width of the plotted image. The point defaults to the position of the centre of the image, but can alternatively be a corner or point on the middle of an edge.

```R
library(jpeg)
library("JUtils")
    
plot(...)
img <- readJPEG("myjpeg.jpg", native = TRUE)
# Draw image centred on (0, 0)
JPlotRaster(img, x = 0, y = 0, width = 2)
# Draw image with top-left corner at (2, 1)
JPlotRaster(img, x = 2, y = 1, width = 2, position = "topleft")
```

---

### Plotting a list of probability densities

Combines line plots of multiple densities into a single plot. This is an operation I perform fairly frequently, so this is here to simplify my life.

```R
library("JUtils")
    
# Get a list of densities from somewhere
data <- list(
             normal = rnorm(100),
             uniform = runif(50),
             exponential = rexp(200))
densities <- lapply(data, density)
# Plot them all on a single plot
JPlotDensities(densities, legendLabels = c("Normal", "Uniform", "Exponential"))
```

---

### Writing text to a file

The function `JReportToFile` runs a function or an expression, and redirects all output to a file.

```R
library("JUtils")

# Write "Hello world!" to the file called test.txt
JReportToFile("test.txt", print("Hello world!"))
```

---

### Downloading files

The function `JDownload` is used to download a URL to a local file. The local file is tracked so that a second call to `JDownload` with the same URL will not download it again, but simply return the name of the previously downloaded file. However, it will be downloaded again if it has changed since the last download.

```R
library("JUtils")

# Fairly slow the first time
jpgFile <- JDownload("https://farm5.staticflickr.com/4507/37847388931_959d812490_o_d.jpg")

# Quick the second time
jpgFile <- JDownload("https://farm5.staticflickr.com/4507/37847388931_959d812490_o_d.jpg")
```

---

### String functions

Ever wanted to convert a vector of values to a human-readable list? Try JToSentence!

```R
library("JUtils")

print(JToSentence(c("apple", "banana", "mandarin", "mango")))
# => [1] "apple, banana, mandarin and mango"
```

Capitalise a string or set of words.

```R
print(JCapitalise("the quick brown fox"))
# => [1] "The quick brown fox"
print(JCapWords("the quick brown fox"))
# => [1] "The Quick Brown Fox"
```

---

### Fill missing values

Use `JFill` to replace missing values in a vector with the most recent previous non-missing value. By default, `NA` values are interpreted as missing.

```R
x <- c("", "Group 1", "", "", "", "Group 2", "", "")
 # Specify that empty strings are missing
 JFill(x, x == "")
# [1] NA        "Group 1" "Group 1" "Group 1" "Group 1" "Group 2" "Group 2" "Group 2"
# Get indices of last good value, rather than the value
JFill(x, x == "", indices = TRUE)
# [1] NA  2  2  2  2  6  6  6
```


---

### Progress bar

Displays a progress bar that estimates time to completion based on the timing of tasks so far. The progress bar can be displayed as text written to the console, as a TCL popup or a Windows popup.

```R
n <- 20
# Prepare progress bar
pb <- JBuildProgressBar("win", numItems = n, title = "Progress")
for (i in 1:n) {
  # Execute slow task
  Sys.sleep(runif(1, max = 1))
  # Update progress bar
  pb()
}
# Optionally force close in case there weren't as many items as we expected
pb(close = TRUE)
```

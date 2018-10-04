[![Travis-CI Build Status](https://travis-ci.org/JimMcL/JUtils.svg?branch=master)](https://travis-ci.org/JimMcL/JUtils)
[![codecov](https://codecov.io/gh/JimMcL/JUtils/branch/master/graph/badge.svg)](https://codecov.io/gh/JimMcL/JUtils)

# JUtils

R utilities to simplify some common operations.

## Installation
JUtils is not available on CRAN, so it must be installed from Github.

    $ install.packages("devtools")
    $ devtools::install_github("JimMcL/JUtils")
    
## Examples

### Plotting to a file

`JUtils` provides functions which wrap the standard R functions (from the core `grDevices` package) for plotting to files using base graphics. `JUtils`  provides more flexibility in specifying file sizes, and hides some of the more arcane aspects of the standard functions, yet still provides access to all of the underlying functionality. All functions accept any "real world" units (mm, cm & in). Raster functions (`JPlotToPNG` and `JPlotToTIFF`) also accept pixel ("px") units.


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

### Printing to a file

The function `JReportToFile` runs a function or an expression, and redirects all output to a file.

    library("JUtils")

    # Write "Hello world!" to the file called test.txt
    JReportToFile("test.txt", print("Hello world!"))

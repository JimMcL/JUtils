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

`JUtils` provides functions which wrap the standard R functions for plotting to files. `JUtils` functions provide more flexibility in specifying file sizes, and hide some of the more arcane aspects of the standard functions.


    library("JUtils")

    # Plot to a PNG file with width 180 mm, height 120 mm 
    # (i.e. height / aspectRatio which defaults to (3 / 2)), resolution 300 ppi.
    # This results in a PNG file with size 2125x1417 pixels
    JPlotToPNG("test.png", plot(1:10 + rnorm(10), type = "o"), width = 180, units = "mm", res = 300)

    # Plot to a PDF file, with graphics region 18 cm x 12 cm
    JPlotToPDF("test.pdf", plot(1:10 + rnorm(10), type = "o"), width = 18, units = "cm")

    # JPlotToFile selects the file type based on the file name, then passes all of its arguments 
    # on to the appropriate JPlotTo* function
    JPlotToFile("test.eps", plot(1:10))
`

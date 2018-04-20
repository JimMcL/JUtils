# JUtils

Some R utilities to simplify common operations.

## Installation
    $ install.packages("devtools")
    $ devtools::install_github("JimMcL/JUtils")
    
## Examples
    library("JUtils")
    
    JPlotToFile("test.eps", plot(1:10))
    
    JPlotToPNG("test.png", plot(1:10 + rnorm(10), type = "o"), width = 180, units = "mm", res = 300)
`

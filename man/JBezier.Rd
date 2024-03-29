% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anim-tools.R
\name{JBezier}
\alias{JBezier}
\alias{JStep}
\title{Parameterised transition timings}
\usage{
JBezier(p0, p1, p2, p3)

JStep(time)
}
\arguments{
\item{p0, p1, p2, p3}{Define the two control points (P1 & P2) of a cubic bezier
curve with end points at (0, 0) and (1, 1).}

\item{time}{Time at which to step from initial value to final value. Time is
expressed as a fraction of the scene duration. The scene starts at time 0
and ends at time 1.}
}
\value{
A timing function.
}
\description{
These functions allow you to create custom transition timing functions. See
Examples below for usage.
}
\examples{
\dontrun{
# Replicate the \link{JEaseOut} timing function
JScene(1, 30,
       width = JTransition(.1, 1, timing = JBezier(0, 0, 0.58, 1)),
       plotFn = function(width) {
           # Do something with width
       })
}

# Function to graphically visualise a timing function
VisualiseTimingFun <- function(fn) plot(seq(0, 1, .001), fn(seq(0, 1, .001)), type = "l")

# Visualise a custom step function
VisualiseTimingFun(JStep(0.3))

# Visualise a custom bezier function which begins slowly and ends fast
VisualiseTimingFun(JBezier(0.7, 0.1, 0.95, 0.7))

}
\seealso{
\code{\link{JTransition}}, \code{\link{JEase}}, \code{\link{JScene}}, \code{\link{JLoop}}, \code{\link{JEase}}
}

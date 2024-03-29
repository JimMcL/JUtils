% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anim-tools.R
\name{JLoop}
\alias{JLoop}
\alias{JLoops}
\title{Looping transition timing function}
\usage{
JLoop(x)

JLoops(nCycles)
}
\arguments{
\item{x}{x-value passed to the function automatically. See \code{Examples} below.}

\item{nCycles}{Number of times to loop within the transition.}
}
\value{
A timing function.
}
\description{
Creates a custom transition timing function that loops smoothly, i.e. the
value animates from its initial value to the final value then back to the
initial value. It is implemented as a sin function, so it starts and stops
smoothly. \code{JLoop} is passed by name (i.e. without parentheses), whereas
\code{JLoops} must be invoked with a single argument, \code{nCycles}.
}
\examples{
\dontrun{
# Loop
JScene(1, 30,
       width = JTransition(0, 1, timing = JLoop), # Loop once during the transition
       height = JTransition(0, 1, timing = JLoops(3)), # Loop 3 times
       plotFn = function(width, height) {
           # width values will animate from 0 to 1 then back to 0
           # height values will change from 0 to 1 and back 3 times
       })
}

}
\seealso{
\code{\link{JTransition}}, \code{\link{JEase}},
  \code{\link{JBezier}}, \code{\link{JScene}}
}

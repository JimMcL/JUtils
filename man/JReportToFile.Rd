% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_to_file.R
\name{JReportToFile}
\alias{JReportToFile}
\title{Send console (i.e. text) output to a file}
\usage{
JReportToFile(filename, expr, createDirectory = TRUE)
}
\arguments{
\item{filename}{Name of the file to write to. If NULL, output destination is unaffected by this function.}

\item{expr}{An expression which outputs the text to be written.}

\item{createDirectory}{If TRUE and \code{filename} is located in a directory
which doesn't exist, the directory will be created.}
}
\value{
The result of evaluating \code{plotExpr} is returned invisibly (which
  means it is not automatically printed).
}
\description{
Sends the text output of an arbitrary R expression or function to a text file.
}
\examples{
\dontrun{
JReportToFile("test.txt", print("Hello world!"))
}

}
\seealso{
\code{\link{sink}}, \code{\link[utils]{capture.output}} for saving text output into a variable.
}

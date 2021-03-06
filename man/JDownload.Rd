% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{JDownload}
\alias{JDownload}
\title{Downloads one or more URLs and saves them to local files.}
\usage{
JDownload(
  url,
  tempfileFn = NULL,
  cacheDir = tempdir(),
  filePattern = "jdcache",
  verbose = FALSE,
  debug = FALSE
)
}
\arguments{
\item{url}{Character vector specifying the URLs to be downloaded.}

\item{tempfileFn}{Function to create names of downloaded files. (defaults to
tempfile). Must accept the same arguments as the base R function
\code{tempfile}.}

\item{cacheDir}{Passed to \code{tempFileFn} as the parameter \code{tmpdir}.
By default, files will be created in this directory.}

\item{filePattern}{Passed to \code{tempFileFn} as the parameter \code{pattern}.
By default, this is used as the initial part of the file name.}

\item{verbose}{If TRUE, prints status messages.}

\item{debug}{If TRUE, prints debugging messages.}
}
\value{
Array of names of the downloaded files.
}
\description{
The intent of this function is to save time downloading files by only
downloading them if they haven't previously been downloaded. Files will be
downloaded if they have been modified since the previous download (as
determined by HTTP If-Modified-Since header). On some platforms, caching does
not seem to work correctly. On those platforms, the files are always downloaded.
}
\details{
A file is created within \code{cacheDir} which contains the URL, file name
and modification date of each downloaded file in the directory. It is used to
determine whether a URL needs to be downloaded again. The file is in R
serialised object format (\code{readRDS}, \code{saveRDS}).  The URL is always
fetched (using \code{httr::GET}), however the http header
\code{If-Modified-Since} is specified so that it will not be downloaded again
if it is unmodified.
}
\seealso{
\code{\link[utils]{download.file}}
}

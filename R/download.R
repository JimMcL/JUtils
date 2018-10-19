library(httr)
library(tools)

#' Downloads one or more URLs and saves them to local files.
#'
#' The intent of this function is to save time downloading files by only
#' downloading them if they haven't previously been downloaded. Files will be
#' downloaded if they have been modified since the previous download (as
#' determined by HTTP If-Modified-Since header).
#'
#' A file is created within \code{cacheDir} which contains the URL, file name and
#' modification date of each downloaded file in the directory. It is used to
#' determine whether a URL needs to be downloaded again. The file is in R
#' serialised object format (\code{readRDS}, \code{saveRDS}).  The URL is always fetched
#' (using \code{httr::GET}), however the http header \code{If-Modified-Since} is specified
#' so that it will not be downloaded again if it is unmodified.
#'
#' @param url Character vector specifying the URLs to be downloaded.
#' @param tempfileFn Function to create names of downloaded files. (defaults to
#'   tempfile). Must accept the same arguments as the base R function
#'   \code{tempfile}.
#' @param cacheDir Passed to \code{tempFileFn} as the parameter \code{tmpdir}. By default,
#'   files will be created in this directory.
#' @param verbose If TRUE, prints status messages.
#' @param debug If TRUE, prints debugging messages.
#'
#' @return Array of names of the downloaded files.
#'
#' @export
JDownload <- function(url, tempfileFn = NULL, cacheDir = tempdir(), verbose = FALSE, debug = FALSE) {
  if (!dir.exists(cacheDir))
    dir.create(cacheDir, recursive = TRUE)

  # Prepare index. Index is a list of (url,modified,file)
  indexPath <- file.path(cacheDir, ".JDindex.rds")
  if (file.exists(indexPath)) {
    index <- readRDS(indexPath)
  } else {
    index <- data.frame()
  }

  # Construct file names
  if (is.null(tempfileFn))
    tempfileFn <- tempfile
  f = tempfileFn(pattern = 'jdcache', tmpdir = cacheDir, fileext = paste0('.', tools::file_ext(url)))

  # For each file
  for (i in 1:length(url)) {
    u <- url[i]

    # Get index entry
    entry <- index[u,]
    if (debug)
      cat(sprintf("%d: %s: %s\n", i, u, ifelse(is.null(entry$modified), "NULL", entry$modified)))
    resp <- httr::GET(u, httr::add_headers('If-Modified-Since' = entry$modified))
    if (resp$status_code == 304) {
      # Use cached file
      f[i] <- entry[,"file"]
      if (verbose)
        cat(sprintf("Using cached %s\n", u))
    } else {
      httr::stop_for_status(resp, paste("download", u))         # Ugly
      if (verbose)
        cat(sprintf("Downloading %s\n", u))
      if (resp$status_code == 200) {
        # Save content to the file
        bin <- httr::content(resp, "raw")
        writeBin(bin, f[i])
        # Update the index
        index[u,"modified"] <- httr::headers(resp)$`Last-Modified`
        index[u,"file"] <- f[i]
      }
    }
  }
  # Save potentially modified index
  saveRDS(index, indexPath)

  f
}


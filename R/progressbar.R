durationToS <- function(duration) {
  if (is.na(duration))
    return(NA)
  if (duration >= 3600) {
    duration <- duration / 3600
    units <- "hours"
  } else if (duration >= 60) {
    duration <- duration / 60
    units <- "mins"
  } else {
    duration <- ceiling(duration)
    units <- "secs"
  }
  sprintf("%g %s", signif(duration, 2), units)
}

.formatProgressMsg <- function(n, total, secsElapsed, secsRemaining, sd, finished, showPC) {
  suf <- if (showPC) { sprintf(", %0.1f%% complete", n * 100 / total) } else ""

  # If it's not going to finish for a long time...
  if (secsRemaining > 45 * 60) {
    # Report finish time
    fmt <- if (julian(Sys.Date()) != julian(Sys.time() + secsRemaining))
      "%Y-%m-%d %H:%M:%S"
    else
      "%H:%M:%S"
    sprintf("Est. finish at %s%s", format(Sys.time() + secsRemaining, fmt), suf)
  } else {
    sprintf("Est. time remaining %s%s", durationToS(secsRemaining), suf)
  }
}

buildTxtReportFn <- function(title, newline = "\r", showPC) {
  if (!missing(title) && !is.null(title)) {
    cat(paste0(title, "\n"))
    utils::flush.console()
  }

  function(n, total, secsElapsed, secsRemaining, sd, finished) {
    if (finished)
      cat(sprintf("\nComplete\n"))
    else {
      cat(paste0(.formatProgressMsg(n, total, secsElapsed, secsRemaining, sd, finished, showPC),
             "                                    ", newline))
      utils::flush.console()
    }
  }
}

buildWinReportFn <- function(title, showPC) {
  pb <- utils::winProgressBar(title, "Estimated completion time", min = 0, max = 100)
  function(n, total, secsElapsed, secsRemaining, sd, finished) {
    if (!missing(finished) && finished)
      close(pb)
    else {
      label <- .formatProgressMsg(n, total, secsElapsed, secsRemaining, sd, finished, showPC)
      utils::setWinProgressBar(pb, 100 * secsElapsed / (secsElapsed + secsRemaining), label = label)
    }
  }
}

buildTkReportFn <- function(title, showPC) {
  pb <- tcltk::tkProgressBar(title, "Estimated completion time", min = 0, max = 100)
  function(n, total, secsElapsed, secsRemaining, sd, finished) {
    if (!missing(finished) && finished)
      close(pb)
    else {
      label <- .formatProgressMsg(n, total, secsElapsed, secsRemaining, sd, finished, showPC)
      tcltk::setTkProgressBar(pb, 100 * secsElapsed / (secsElapsed + secsRemaining), label = label)
    }
  }
}

# Attempt to guess the total time required for all tasks to complete
forecastGuess <- function(timesSoFar, n) {
  # Is there a clear trend?
  trendWeight <- 0
  trending <- 0

  # Practical experience suggests meaningful trends are unlikely and simple
  # mean-based estimate is a more reliable guide to expected time

  # if (length(timesSoFar > 1)) {
  #   l <- stats::lm(time ~ x, data = data.frame(x = seq_along(timesSoFar), time = timesSoFar))
  #   sl <- summary(l)
  #   r2 <- abs(sl$r.squared)
  #   c <- stats::coef(sl)
  #   p <- if (nrow(c) < 2) { 1 } else { c[2,4] }
  #   if (!is.na(p) && r2 > 0.5) {
  #     trendWeight <- 1 - p
  #     trending <- sum(stats::predict(l, newdata = data.frame(x = seq_len(n))))
  #   }
  # }

  # Estimate based on each task having independent times
  independent <- mean(timesSoFar) * n

  # Combine trend-based estimate with independent estimate
  trending * trendWeight + independent * (1 - trendWeight)
}


# A general purpose progress bar that reports remaining elapsed time rather than number of items
#
# @param numItems Number of items to be processed
# @param reportFn A function used to report changing progress
#
# @return A function which should be called for each item as it is processed.
ElapsedTimeProgressBarFn <- function(numItems, reportFn) {
  # Function state
  durations <- numeric(numItems)
  index <- 1
  startTime <- proc.time()
  itemStartTime <- proc.time()
  closed <- FALSE

  function(itemNumber, newNumItems, close, printElapsed) {
    # Calculate elapsed time from last item to this
    now <- proc.time()

    # Report total time so far?
    if (!missing(printElapsed) && printElapsed) {
      duration <- (now - startTime)[3]
      cat(sprintf("Elapsed time: %s\n", durationToS(duration)))
      return(invisible(NULL))
    }

    # Already closed?
    if (closed)
      # Do nothing
      return(invisible(NULL))

    # Force close?
    if (!missing(close) && close) {
      reportFn(finished = TRUE)
      closed <<- TRUE
      return(invisible(NULL))
    }

    # Allow caller to override current item index or total number of items
    if (!missing(itemNumber)) {
      index <<- as.numeric(itemNumber)
    }
    if (!missing(newNumItems)) {
      numItems <<- as.numeric(newNumItems)
    }

    # Save this duration
    duration <- (now - itemStartTime)[3]
    durations[index] <<- duration
    # Time remaining
    nRemaining <- numItems - index
    # secsRemaining <- nRemaining * (now - startTime)[3] / index
    total <- forecastGuess(durations[1:index], numItems)
    secsRemaining <- total - sum(durations)
    secsRemaining <- max(0, secsRemaining)

    closed <<- nRemaining == 0
    # To get sd, sum the variances (= sd ^ 2) and take square root
    reportFn(index, numItems, (now - startTime)[3], secsRemaining, sqrt(stats::sd(durations) ^ 2 * nRemaining), finished = closed)

    index <<- index + 1
    itemStartTime <<- now
  }
}

#' Creates a general purpose progress bar that reports remaining elapsed time
#' rather than number of items.
#'
#' This function attempts to guess the total elapsed time based on time so far
#' and \code{numItems}.
#'
#' The progress bar can be displayed as a Windows popup, a TCL popup, text
#' printed to the console, or no progress bar depending on the value of
#' \code{progressBar}.
#'
#' The function (\code{fn}) returned by this function is usually called with no
#' arguments, once for each item to be processed. Call \code{fn(close = TRUE)}
#' to close the progress bar without processing all items. If the number of
#' items to be processed changes dynamically, call \code{fn(numNumItems = x)}.
#'
#' @param progressBar Type of progress bar.
#' @param numItems Number of items to be processed.
#' @param title Optional message displayed on the progress bar.
#' @param showPC If \code{TRUE}, include percent complete in progress message.
#'
#' @return A function which should be called for each item as it is processed.
#'
#' @examples
#' \dontrun{
#' n <- 20
#' pb <- JBuildProgressBar("win", numItems = n, title = "Progress")
#' for (i in 1:n) {
#'   # Execute slow task
#'   Sys.sleep(runif(1, max = 1))
#'   # Update progress bar
#'   pb()
#' }
#'
#' # Optionally force close in case there weren't as many items as we expected
#' pb(close = TRUE)
#' # Optionally report total elapsed time
#' pb(printElapsed = TRUE)
#' }
#'
#' @export
JBuildProgressBar <- function(progressBar = c("text", "win", "tk", "none"), numItems, title = NULL, showPC = FALSE) {
  # Setup the progress bar
  progressBar <- match.arg(progressBar)

  rfn <- switch(progressBar,
         text = buildTxtReportFn(title, showPC = showPC),
         win = buildWinReportFn(title, showPC),
         tk = buildTkReportFn(title, showPC)
  )
  if (is.null(rfn)) {
    function(close){}
  } else {
    ElapsedTimeProgressBarFn(numItems, rfn)
  }
}

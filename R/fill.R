#' Fill "bad" values in a vector with the most recent "good" values
#'
#' @param x Vector to be filled
#' @param badValues Logical vector identifying the "bad" values in `x`, i.e. the
#'   values to be replaced
#' @param indices If TRUE, return indices into `x`, otherwise values from
#'   `x`.
#'
#' @returns description Either a vector of indices into `x` (when
#'   `returnIndices` is `TRUE`), or `x` with bad values replaced by the most
#'   recent good value.
#'
#' @examples
#' JFill(c(NA, 1, 2, 3, NA, 4, NA, NA))
#' ## [1] NA  1  2  3  3  4  4  4
#'
#' @export
JFill <- function(x, badValues = is.na(x), indices = FALSE) {
  goodValues <- !badValues
  # Get indices of good values
  lastGoodIdx <- cumsum(goodValues)
  # Leave leading bad values untouched
  lastGoodIdx[lastGoodIdx == 0] <- NA
  # For each entry in x, determine where to get its value
  idxs <- which(goodValues)[lastGoodIdx]

  if (indices) {
    idxs
  } else {
    x[idxs]
  }
}


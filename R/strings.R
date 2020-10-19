
#' Convert a vector to a human readable list
#'
#' Similar to calling `paste(v, collapse = ", ")` except that the conjuction ("
#' and " by default) is used as the final separator.
#'
#' @param v Vector of values.
#' @param sep Separator to use between the first elements of the list.
#' @param conjunction Value to use before the last element in the list.
#'
#' @examples
#' print(JToSentence(c("apple", "banana", "mandarin", "mango")))
#' # => [1] "apple, banana, mandarin and mango"
#'
#' @export
JToSentence <- function(v, sep = ", ", conjunction = " and ") {
  if (length(v) <= 1) {
    v
  } else {
    paste(
      c(paste(v[1:(length(v)-1)], collapse = sep),
        v[length(v)]),
      collapse = conjunction)
  }
}

#' Capitalises the first letter in a string.
#'
#' Copied (and changed) from the doc for \code{toupper}.
#'
#' @param s Character vector to be capitalised
#' @param strict If \code{TRUE}, the remainder of the string is converted to lower case.
#' @param collapse String used to collapse multiple character strings.
#'
#' @examples
#' print(JCapitalise("the quick brown fox"))
#' # => [1] "The quick brown fox"
#'
#' @export
JCapitalise <- function(s, strict = FALSE, collapse = NULL) {
  s <- as.character(s)
  paste(toupper(substring(s, 1, 1)),
        {s <- substring(s, 2); if(strict) tolower(s) else s},
        sep = "", collapse = collapse)
}

#' Capitalises the first letter in each word in a string.
#'
#' @param s Character vector to be capitalised.
#' @param split Regular expression used to split words to be capitalised.
#' @param sep String used to paste words back together.
#' @param ... Additional arguments passed to \code{\link{JCapitalise}}
#'
#' @examples
#' print(JCapWords("the quick brown fox"))
#' # => [1] "The Quick Brown Fox"
#'
#' @export
JCapWords <- function(s, split = " ", sep = " ", ...) {
  s <- as.character(s)
  sapply(strsplit(s, split = split), JCapitalise, collapse = sep, ...)
}


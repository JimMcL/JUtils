
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

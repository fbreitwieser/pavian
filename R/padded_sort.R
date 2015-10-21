
#' Sort a vector of string, but pad the first numbers with zeros before doing so.
#'
#' Adapted from http://stackoverflow.com/a/15552536
#'
#' @param x vector of strings
#' @param n_pad number of zeros the numbers are padded with
#'
#' @return vector of strings sorted 'alpha-numerically'
#' @export
#'
#' @examples
#' strings <- c("A1","A01","A10","A20","A")
#' paddedsort(strings)
padded_sort <- function(x, n_pad=5) {
  if (length(x) == 0)
    return(x)
  sprintf_pad_string <- paste0("%.",n_pad,"d")

  numeric_matches <- sapply(regmatches(x, gregexpr('\\d+', x)),as.numeric)
  padded_x <- sapply(seq_along(x), function(n) {
    if (length(numeric_matches[[n]]) > 0) {
      sub('\\d+',sprintf(sprintf_pad_string,
                         numeric_matches[[n]][1]),x[n])
    } else {
      x[n]
    }
  })

  ## sort by both padded_x and x to break ties, such as between A1 and A01
  x[order(padded_x,x)]
}

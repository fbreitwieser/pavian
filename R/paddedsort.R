
#' Sort a vector of string, but padd the numbers with zeros before doing so.
#'
#' Adapted from http://stackoverflow.com/a/15552536
#'
#' @param x vector of strings
#' @param pad number of zeros the numbers are padded with
#'
#' @return vector of strings sorted 'alpha-numerically'
#' @export
#'
#' @examples
#' strings <- c("A1","A01","A10","A20")
#' paddedsort(strings)
paddedsort <- function(x, n.pad=5) {
  x.padded <- mapply(gsub, list('\\d+'), sprintf(paste0("%.",n.pad,"d"), as.numeric(regmatches(x, gregexpr('\\d+', x)))), x)
  x[order(x.padded,x)]
}

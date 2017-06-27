
#' Make a number human readable
#'
#' @param x Number.
#' @param base typically 1000 or 2^10.
#' @param suffix Will be concatendated to the end.
#' @param signif_digits Number of significant digits.
#'
#' @return human readable representation of the number
#' @export
#'
#' @examples
#' humanreadable(1234.567)
humanreadable <- function(x, base = 10^3, suffix = "", signif_digits = 3)  {
  base_suffixes <- c("k", "M", "G", "T", "P", "E", "Z", "Y")
  if (length(x) == 0)
    return()

  if (any(x < 0))
    stop("'x' must be positive")

  x_suffixes <- rep("",length(x))

  for (base_suffix in base_suffixes) {
    sel <- floor(x) >= 1000
    x[sel] <- x[sel] / base
    x_suffixes[sel] <- base_suffix
  }
  return(paste0(signif(x,signif_digits),x_suffixes,suffix))
}

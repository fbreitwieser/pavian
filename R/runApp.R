
#' Run Centrifuger web interface
#'
#' @param cache_dir Directory to save temporary files.
#' @param ... Additional arguments to \code{\link[shiny]{runApp}}, such as \code{host} and \code{port}.
#'
#' @export
runApp <- function(cache_dir = "cache",
                   example_dir = system.file("shinyapp", "example-data", package = "pavian"), ...) {

  appDir <- system.file("shinyapp", package = "pavian")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `pavian`.", call. = FALSE)
  }

  options(pavian.cache_dir = cache_dir)
  options(pavian.example_dir = example_dir)

  shiny::runApp(appDir, display.mode="normal", ...)
}


#' Run Pavian web interface
#'
#' @param cache_dir Directory to save temporary files.
#' @param server_dir Directory for sample files.
#'
#' @param ... Additional arguments to \code{\link[shiny]{runApp}}, such as \code{host} and \code{port}.
#'
#' @export
runApp <- function(cache_dir = "cache",
                   server_dir = system.file("shinyapp", "example-data", package = "pavian"),
                   allow_change_server_dir = TRUE,
                   ...) {

  appDir <- system.file("shinyapp", package = "pavian")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `pavian`.", call. = FALSE)
  }

  options(pavian.cache_dir = cache_dir)
  options(pavian.server_dir = server_dir)
  options(pavian.allow_change_server_dir = allow_change_server_dir)

  shiny::runApp(appDir, display.mode="normal", ...)
}

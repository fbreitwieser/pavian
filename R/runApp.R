
#' Run Pavian web interface
#'
#' @param cache_dir Directory to save temporary files.
#' @param server_dir Directory for sample files.
#' @param server_access Allow users to change server directory
#' @param load_server_directory Load server directory.
#' @param load_example_data Load example data.
#'
#' @param ... Additional arguments to \code{\link[shiny]{runApp}}, such as \code{host} and \code{port}.
#'
#' @export
runApp <- function(cache_dir = "cache",
                   server_dir = Sys.glob("~"),
                   server_access = TRUE,
                   load_example_data = FALSE,
                   load_server_directory = FALSE,
                   ...) {

  appDir <- system.file("shinyapp", package = "pavian")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `pavian`.", call. = FALSE)
  }

  options(pavian.cache_dir = cache_dir)
  options(pavian.server_dir = server_dir)
  options(pavian.server_access = server_access)
  options(pavian.load_server_directory = load_server_directory)
  options(pavian.load_example_data = load_example_data)

  shiny::runApp(appDir, display.mode="normal", ...)
}

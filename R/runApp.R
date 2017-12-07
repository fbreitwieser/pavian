
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
  
  pID = 0

  new_options <- list(
    pavian.session_count = 0,
    pavian.running_sessions = 0,
    pavian.cache_dir = cache_dir,
    pavian.server_dir = server_dir,
    pavian.server_access = server_access,
    pavian.load_server_directory = load_server_directory,
    pavian.load_example_data = load_example_data)

  old_options <- options(new_options)
  shiny::runApp(appDir, display.mode="normal", ...)
  
  ## TODO: Restoring options like this does not work - we never get here after shiny::runApp as the server keeps running
  options(old_options)
}

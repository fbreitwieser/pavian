
#' Run Centrifuger web interface
#'
#' @param cache_dir Directory to save temporary files
#'
#' @export
runApp <- function(cache_dir = "cache", ...) {
  appDir <- system.file("shinyapp", package = "pavian")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `pavian`.", call. = FALSE)
  }

  #source(system.file("shinyapp","ui.R", package = "pavian"))
  #source(system.file("shinyapp","server.R", package = "pavian"))

  options(pavian.cache_dir = cache_dir)

  shiny::runApp(appDir, display.mode="normal", ...)
}

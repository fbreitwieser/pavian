
#' Run Centrifuger web interface
#'
#' @param cache_dir Directory to save temporary files
#'
#' @export
runApp <- function(cache_dir = "cache", ...) {
  appDir <- system.file("shinyapp", package = "centrifuger")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `centrifuger`.", call. = FALSE)
  }

  #source(system.file("shinyapp","ui.R", package = "centrifuger"))
  #source(system.file("shinyapp","server.R", package = "centrifuger"))

  options(centrifuger.cache_dir = cache_dir)

  shiny::runApp(appDir, display.mode="normal", ...)
}

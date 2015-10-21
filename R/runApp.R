
#' Run Centrifuger web interface
#'
#' @param cache_dir Directory to save temporary files
#'
#' @export
runApp <- function(cache_dir = "cache") {
  appDir <- system.file("shinyapp", package = "centrifugeR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `centrifugeR`.", call. = FALSE)
  }

  #source(system.file("shinyapp","ui.R", package = "centrifugeR"))
  #source(system.file("shinyapp","server.R", package = "centrifugeR"))

  options(centrifugeR.cache_dir = cache_dir)

  shiny::runApp(appDir, display.mode="normal")
}

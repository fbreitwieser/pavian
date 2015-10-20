
#' Run Centrifuger web interface
#'
#' @export
runCentrifuger <- function() {
  appDir <- system.file("shinyapp", package = "centrifugeR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `centrifugeR`.", call. = FALSE)
  }

  #source(system.file("shinyapp","ui.R", package = "centrifugeR"))
  #source(system.file("shinyapp","server.R", package = "centrifugeR"))

  shiny::runApp(appDir, display.mode="normal")
}

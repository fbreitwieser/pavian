
#' Run Centrifuger web interface
#'
#' @export
runCentrifuger <- function() {
  appDir <- system.file("shinyapp", package = "centrifugeR")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `centrifugeR`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}

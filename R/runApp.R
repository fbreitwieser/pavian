
#' Run Pavian web interface
#'
#' @param cache_dir Directory to save temporary files.
#' @param server_dir Directory for sample files.
#' @param server_access Allow users to change server directory
#' @param load_server_directory Load server directory.
#' @param load_example_data Load example data.
#' @param maxUploadSize Maximum upload size for reports and BAM files.
#' @param enableBookmarking Enable bookmarking? Possible values 'disable' or 'server' (default).
#'
#' @param ... Additional arguments to \code{\link[shiny]{runApp}}, such as \code{host} and \code{port}.
#'
#' @export
runApp <- function(cache_dir = "cache",
                   server_dir = Sys.glob("~"),
                   server_access = TRUE,
                   load_example_data = FALSE,
                   load_server_directory = FALSE,
                   maxUploadSize = NULL,
                   enableBookmarking = "server",
                   ...) {

  appDir <- system.file("shinyapp", package = "pavian")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `pavian`.", call. = FALSE)
  }
  
  if (!is.null(maxUploadSize) && is.character(maxUploadSize)) {
    if (is.character(maxUploadSize)) {
      numMaxUploadSize <- suppressWarnings(as.numeric(maxUploadSize))
      if (!is.na(numMaxUploadSize)) {
        maxUploadSize <- numMaxUploadSize
      } else {
        unit <- toupper(substring(maxUploadSize, nchar(maxUploadSize)))
        val <- as.numeric(substring(maxUploadSize, 1, nchar(maxUploadSize)-1))
        factor <-
          switch(unit,
                 B=1,
                 K=1024,
                 M=1024^2,
                 G=1024^3,
                 -1)
        if (is.na(val) || factor == -1) {
          message("Error parsing maxUploadSize! Allowed extensions are B, K, M and G")
          maxUploadSize <- NULL
        } else {
          maxUploadSize <- factor*val   
        }
      }
    }
  }
  
  pID = 0

  new_options <- list(
    pavian.session_count = 0,
    pavian.running_sessions = 0,
    pavian.cache_dir = cache_dir,
    pavian.server_dir = server_dir,
    pavian.server_access = server_access,
    pavian.load_server_directory = load_server_directory,
    pavian.load_example_data = load_example_data,
    #pavian.maxSubDirs = maxSubDirs,
    shiny.maxRequestSize = maxUploadSize
  )

  old_options <- options(new_options)

  shiny::shinyApp(pavian::dashboardUI, pavian::pavianServer, enableBookmarking=enableBookmarking, options = list(...))

  ## TODO: Restoring options like this does not work - we never get here after shiny::runApp as the server keeps running
  #options(old_options)
}

#' @describeIn runApp Create pavian app (passes arguments to runApp)
#' @export
pavianApp <- function(...) {
    pavian::runApp(...)
}

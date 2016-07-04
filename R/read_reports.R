#' Read report files, with Shiny progress bar
#'
#' @param report_files character vector with files
#' @param report_names character vector with names
#'
#' @return List of reports
#' @export
read_reports <- function(report_files, report_names, cache_dir = "cache") {
  if (length(report_files) == 0) {
    return()
  }
  n_reports <- length(report_files)
  my_reports <-
    withProgress(
      message = paste("Loading", n_reports, "sample reports"),
      detail = 'This may take a while...',
      value = 0,
      min = 0,
      max = n_reports,
      {
        lapply(seq_along(report_files),
               function(i) {
                 setProgress(value = i,
                             detail = paste(n_reports - i, "left ..."))
                 load_or_create(
                   function()
                     read_krakenres(report_files[i]),
                   sprintf("%s.rds", basename(report_files[i])),
                   cache_dir = cache_dir
                 )
               })
      }
    )

  names(my_reports) <- report_names
  lapply(my_reports, function(my_report) {
    #my_report$name <- sub("[a-z-]_", "", my_report$name)
    #my_report$taxonstring <-
    #  gsub("[a-z-]_", "", my_report$taxonstring)
    my_report
  })
}

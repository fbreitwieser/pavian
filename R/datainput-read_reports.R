#' Read metagenomics classification results ('reports'), with Shiny progress bar
#'
#' @param report_files character vector with files, or a data.frame from read_sample_data, or a single directory path
#' @param report_names character vector with names
#' @param cache_dir cache directory path
#'
#' @return List of reports
#' @export
#' @examples
#' report_dir <- system.file("shinyapp", "example-data", "brain-biopsies", package="pavian")
#' reports <- read_reports(report_dir)
read_reports <- function(report_files, report_names = NULL, cache_dir = NULL) {
  if (length(report_files) == 0) {
    return()
  }

  if (length(report_files) == 1 && isTRUE(file.info(report_files)$isdir)) {
    report_files <- read_sample_data(report_files, ext = NULL)
  }

  if (is.data.frame(report_files) && all(c("ReportFilePath", "Name") %in% colnames(report_files))) {
    report_names <- report_files$Name
    report_files <- report_files$ReportFilePath
  }

  if (is.null(report_names)) {
    report_names = basename(report_files)
  }
	  
  if (any(duplicated(report_names))) {
    report_names = report_files
  }

  is_shiny_session <- !is.null(shiny::getDefaultReactiveDomain())
  if (!is_shiny_session) {
    withProgress <- function(expr, env = parent.frame(), ...) { eval(expr, env); }
    setProgress <- function(...) {}
  }

  dmessage("Reading ",length(report_files)," reports ...")
  n_reports <- length(report_files)

  f <- quote({
    lapply(seq_along(report_files),
           function(i) {
             if (is_shiny_session)
              setProgress(value = i,
                           detail = paste(n_reports - i, "left."))
             load_or_create(
               function() {
                 report <- read_report(report_files[i]) %>% shinyTryCatch(message=paste("reading file",report_files[i]))
                 if (is.null(report)|| nrow(report) == 0) {
                   validate(need(FALSE, message=paste("Error reading file",report_files[i])))
                 }
                 report
               },
               sprintf("%s.rds", basename(report_files[i])),
               cache_dir = cache_dir
             )
           })
    })

  my_reports <- withProgress(f,
      message = paste("Reading", n_reports, "sample reports."),
      detail = 'This may take a while...', max = n_reports,
      quoted = T
    )

  names(my_reports) <- report_names

  my_reports[sapply(my_reports, length) > 0]
}

withProgress_ns <- function(expr, env = parent.frame(), ...) {
  if (is.null(shiny::getDefaultReactiveDomain())) {
    return(eval(expr, env))
  } else {
    return(withProgress(eval(expr,env), env=env, ...))
  }
}

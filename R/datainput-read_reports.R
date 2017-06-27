#' Read metagenomics classification results ('reports'), with Shiny progress bar
#'
#' @param report_files character vector with files
#' @param report_names character vector with names
#' @param cache_dir cache directory path
#'
#' @return List of reports
#' @export
read_reports <- function(report_files, report_names = basename(report_files), cache_dir = NULL) {
  if (length(report_files) == 0) {
    return()
  }

  is_shiny_session <- !is.null(shiny::getDefaultReactiveDomain())
  if (!is_shiny_session) {
    withProgress <- function(expr, env = parent.frame(), ...) { eval(expr, env); }
    setProgress <- function(...) {}
  }

  message("Reading reports ...")
  n_reports <- length(report_files)

  f <- quote({
    lapply(seq_along(report_files),
           function(i) {
             if (is_shiny_session)
              setProgress(value = i,
                           detail = paste(n_reports - i, "left ..."))
             load_or_create(
               function() {
                 tryCatch({
                   read_report(report_files[i])
                 }, error = function(e) print(e))
               },
               sprintf("%s.rds", basename(report_files[i])),
               cache_dir = cache_dir
             )
           })
    })

  my_reports <- withProgress(f,
      message = paste("Loading", n_reports, "sample reports"),
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

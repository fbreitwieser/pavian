#' htmlwidgets:::getDependency
#'
#' Copied to circumvent R CMD check warning when using non-exported function from a package
#'
#' @param name name
#' @param package package
#'
#' @return list of dependencies
#' @export
#'
#' @importFrom yaml yaml.load_file
#' @importFrom htmltools htmlDependency
#' @examples
#' getDependency('sparkline')
getDependency <- function (name, package = name) {
  requireNamespace("htmlwidgets")
  requireNamespace("htmltools")
  requireNamespace("yaml")

  name = "sparkline"
  package = "sparkline"
  config = sprintf("htmlwidgets/%s.yaml", name)
  jsfile = sprintf("htmlwidgets/%s.js", name)
  config = yaml::yaml.load_file(system.file(config, package = package))
  widgetDep <- lapply(config$dependencies, function(l) {
    l$src = system.file(l$src, package = package)
    do.call(htmltools::htmlDependency, l)
  })
  bindingDir <- system.file("htmlwidgets", package = package)
  argsDep <- NULL
  copyBindingDir <- getOption("htmlwidgets.copybindingdir",
                              TRUE)
  if (copyBindingDir) {
    if (packageVersion("htmltools") < "0.3.3") {
      bindingDir <- tempfile("widgetbinding")
      dir.create(bindingDir, mode = "0700")
      file.copy(system.file(jsfile, package = package),
                bindingDir)
    }
    else argsDep <- list(all_files = FALSE)
  }
  bindingDep <- do.call(htmltools::htmlDependency, c(list(paste0(name,
                                                      "-binding"), packageVersion(package), bindingDir, script = basename(jsfile)),
                                          argsDep))
  c(list(htmltools::htmlDependency("htmlwidgets", packageVersion("htmlwidgets"),
                        src = system.file("www", package = "htmlwidgets"), script = "htmlwidgets.js")),
    widgetDep, list(bindingDep))
}

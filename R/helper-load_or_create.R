
#' Load a file from a cache directory, or create and save it
#'
#' @param f function to call when the file does not exist.
#' @param name name of cache file (including .rds).
#' @param recreate if TRUE, recreate the file even if it exists.
#' @param cache_dir directory that contains cache files. It is created if it does not exist.
#'
#' @return object created by function f, or stored in cache file
#' @export
#'
#' @examples
#' \dontrun{
#'  load_or_create(function() {
#'    rnorm(10000)
#'  }, "rnorm-results.rds", recreate=FALSE, cache_dir="cache")
#' }
load_or_create <- function(f, name, recreate=FALSE, cache_dir = "cache") {
  if (is.null(cache_dir))
    return(f())

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
    if (!dir.exists(cache_dir)) {
      stop(1)
    }
  }
  sav_file <- sprintf("%s/%s",cache_dir, name)
  if (!file.exists(sav_file) || isTRUE(recreate)) {
    # message('creating ',name," ... ",appendLF=F)
    res <- f()
    saveRDS(res,file=sav_file)
  } else {
    # message('loading ',name," ... ",appendLF=F)
    res <- readRDS(sav_file)
  }
  # message("finished")
  return(res)
}

get_combinations <- function(...) {
  x <- list(...)
  #all.elemens <- Reduce(union,x)
  my_names <- names(x)

  res <- c()
  # look at each combination of 1, 2, ..., n elements
  for (num.elements in length(x):1) {

    apply(utils::combn(my_names,num.elements),2,function(y) {
      my_name <- paste(y,collapse="&")
      comn_elements <- Reduce(intersect,x[y])
      if (length(comn_elements) > 0) {
        res[my_name] <<- length(comn_elements)
        x[y] <<- lapply(x[y], function(z) z[!z %in% comn_elements])
      }
    })
  }
  return(res)
}



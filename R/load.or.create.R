
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
#' \donotrun{
#' load.or.create(function() {
#'   rnorm(10000)
#' }, "rnorm-results.rds", recreate=FALSE, cache_dir="cache")
#' }
load.or.create <- function(f, name, recreate=FALSE, cache_dir = "cache") {
  if (!file.exists(cache_dir)) {
    dir.create(cache_dir)
  }
  sav.file <- sprintf("%s/%s",cache_dir, name)
  if (!file.exists(sav.file) || isTRUE(recreate)) {
    message('creating ',name," ... ",appendLF=F)
    res <- f()
    saveRDS(res,file=sav.file)
  } else {
    message('loading ',name," ... ",appendLF=F)
    res <- readRDS(sav.file)
  }
  message("finished")
  return(res)
}

get.combinations <- function(...) {
  x <- list(...)
  #all.elemens <- Reduce(union,x)
  my.names <- names(x)

  res <- c()
  # look at each combination of 1, 2, ..., n elements
  for (num.elements in length(x):1) {

    apply(combn(my.names,num.elements),2,function(y) {
      print(y)
      my.name <- paste(y,collapse="&")
      combn.elements <- Reduce(intersect,x[y])
      if (length(combn.elements) > 0) {
        res[my.name] <<- length(combn.elements)
        x[y] <<- lapply(x[y], function(z) z[!z %in% combn.elements])
      }
    })
  }
  return(res)
}



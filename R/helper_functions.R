#' @export
info_message <- function(...) {
  message("INFO [",format(Sys.time(), "%m/%d %H:%M"),"]: ",...)
}

## helper functions for loading the files reactively
##   file_glob_pattern contains %s, which is to be replaced by the sample names
#' @export
list_kraken_files <- function(data_dir,file_glob_pattern,sample_name="*") {
  #sample_file_globs <- sapply(sample_name,function(my_sample_name) gsub("\\([^(]*\\)",my_sample_name,file_glob_pattern))
  sample_file_globs <- sapply(sample_name,function(my_sample_name) gsub("%s",my_sample_name,file_glob_pattern))
  info_message("Looking for files with pattern(s) ",paste(sample_file_globs,collapse=","))
  Sys.glob(paste0(data_dir,"/",sample_file_globs))
}

#' @export
get_sample_name <- function(file_names, regex_pattern) {
  sapply(file_names, function(file_name) sub(regex_pattern,"\\1",file_name))
}

#' @export
sorted_kraken_files <- function(data_dir,file_glob_pattern) {
  kraken_files <- sub(paste0(data_dir,"/"),"",list_kraken_files(data_dir, file_glob_pattern),fixed=TRUE)
  #padded_sort(sub(".report$","",kraken_files))
  sort(sub(".report$","",kraken_files))
}

## Helper function to upper-case column names
#' @export
beautify_string <- function(x) {
  x <- gsub("[\\._]"," ",x)
  x <- sub("^([[:alpha:]])", "\\U\\1", x, perl=TRUE)
  x
}

#' @export
beautify_colnames <- function(x) {
  colnames(x) <- beautify_string(colnames(x))
  x
}

## helper function that sets NAs to zeros in a supplied data.frame
#' @export
zero_if_na <- function(df) { df[is.na(df) | df < 0] <- 0; df; }





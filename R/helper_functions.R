#' @export
info_message <- function(...) {
  message("INFO [",format(Sys.time(), "%m/%d %H:%M"),"]: ",...)
}

## helper functions for loading the files reactively
##   file_glob_pattern contains %s, which is to be replaced by the sample names
#' @export
list_kraken_files <- function(data_dir, file_names,file_extension,sample_name=NULL) {

  file_paths <- paste0(data_dir, "/", file_names)
  sample_file_globs <- sapply(sample_name,
                              function(my_sample_name) gsub("%s",my_sample_name,file_glob_pattern)
                              )
  #if (is.null(sample_file_globs))
  info_message("Looking for files with pattern(s) ",paste(sample_file_globs,collapse=","))
  #list.files(data_dir,pattern=sample_file_globs,...)
  Sys.glob(paste0(data_dir,"/",sample_file_globs))
}

#' @export
get_sample_name <- function(file_names, regex_pattern) {
  sapply(file_names, function(file_name) sub(regex_pattern,"\\1",file_name))
}

#' @export
sorted_kraken_files <- function(data_dir, file_glob_pattern) {
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
zero_if_na <- function(df) {
  df[is.na(df) | df < 0] <- 0
  return(df)
}


# get directory listing for shinyTree
get_directory_listing <- function(my_dir) {
      all_dirs <- list.dirs(my_dir, recursive = FALSE, full.names=TRUE)
      all_dirs_short <- sub(".*/","",all_dirs)
      all_files <- list.files(my_dir)
      all_files <- all_files[!all_files %in% all_dirs_short]
      if (length(all_files) == 0 && length(all_dirs) == 0)
          return(structure(''))

      c(setNames(lapply(all_dirs,get_directory_listing),all_dirs_short),
        setNames(rep(structure('',sticon='file'),length(all_files)),all_files))
}



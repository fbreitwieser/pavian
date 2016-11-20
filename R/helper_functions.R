info_message <- function(...) {
  message("INFO [",format(Sys.time(), "%m/%d %H:%M"),"]: ",...)
}

startsWith <- function(x, prefix) {
  if (exists("startsWith", "package:base") ) {
    base::startsWith(x, prefix)
  } else {
    substring(x, 1, nchar(prefix)) == prefix
  }
}

common_buttons <- function(...) {
  opts <- list(...)
  dl_fname <- paste(unlist(c(opts, format(Sys.time(), "%y%m%d"))), collapse="-")
  list('pageLength', list(extend='excel',title=dl_fname), list(extend='csv',title=dl_fname),'print','copy', 'colvis')
}

## helper functions for loading the files reactively
##   file_glob_pattern contains %s, which is to be replaced by the sample names
list_kraken_files <- function(data_dir, file_names,file_extension,sample_name=NULL) {

  #file_paths <- paste0(data_dir, "/", file_names)
  sample_file_globs <- sapply(sample_name,
                              function(my_sample_name) gsub("%s",my_sample_name,file_glob_pattern)
                              )
  #if (is.null(sample_file_globs))
  info_message("Looking for files with pattern(s) ",paste(sample_file_globs,collapse=","))
  #list.files(data_dir,pattern=sample_file_globs,...)
  Sys.glob(paste0(data_dir,"/",sample_file_globs))
}

get_sample_name <- function(file_names, regex_pattern) {
  sapply(file_names, function(file_name) sub(regex_pattern,"\\1",file_name))
}

## Helper function to upper-case column names
beautify_string <- function(x) {
  x <- gsub("[\\._]"," ",x)
  x <- sub("^([[:alpha:]])", "\\U\\1", x, perl=TRUE)
  x
}

beautify_taxonstring <- function(x) {
  x %>%
    sub("^-_root.","", .) %>%
    sub("^._","", .) %>%
    gsub("\\|._", ">", .) %>%
    gsub(" ", "&nbsp;", .) %>%
    gsub("-","&#x2011;", .)
}


#' Beautify colnames
#'
#' @param x data.frame or matrix
#'
#' @return data.frame or matrix with nicer colnames
#' @export
beautify_colnames <- function(x) {
  colnames(x) <- beautify_string(colnames(x))
  x
}

#' Helper function that sets NAs to zeros in a supplied data.frame
#'
#' @param df data.frame or matrix
#'
#' @return data.frame or matrix in which all negative and NA values are set to zero
#' @export
zero_if_na <- function(df) {
  stopifnot(!is.null(df))
  if (nrow(df) > 0) {
    df[is.na(df) | df < 0] <- 0
  }
  return(df)
}

styleColorBar2 = function(data, color, angle=90) {
  rg = range(data, na.rm = TRUE, finite = TRUE)
  r1 = rg[1]; r2 = rg[2]; r = r2 - r1
  htmlwidgets::JS(sprintf(
    "isNaN(parseFloat(value)) || value <= %s ? '' : 'linear-gradient(%sdeg, transparent ' + (%s - value)/%s * 100 + '%%, %s ' + (%s - value)/%s * 100 + '%%)'",
    r1, angle, r2, r, color, r2, r
  ))
}

# get directory listing for shinyTree
get_directory_listing <- function(my_dir) {
      all_dirs <- list.dirs(my_dir, recursive = FALSE, full.names=TRUE)
      all_dirs_short <- sub(".*/","",all_dirs)
      all_files <- list.files(my_dir)
      all_files <- all_files[!all_files %in% all_dirs_short]
      if (length(all_files) == 0 && length(all_dirs) == 0)
          return(structure(''))

      c(stats::setNames(lapply(all_dirs,get_directory_listing),all_dirs_short),
        stats::setNames(rep(structure('',sticon='file'),length(all_files)),all_files))
}

now <- function() proc.time()[[3]]

# from http://stackoverflow.com/questions/11340444/is-there-an-r-function-to-format-number-using-unit-prefix
f2si2<-function (number)
{
  lut <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-09, 1e-06,
           0.001, 1, 1000, 1e+06, 1e+09, 1e+12, 1e+15, 1e+18, 1e+21,
           1e+24)
  pre <- c("y", "z", "a", "f", "p", "n", "u", "m", "", "k",
           "M", "G", "T", "P", "E", "Z", "Y")
  sel <- lut >= 1
  lut <- lut[sel]
  pre <- pre[sel]
  ix <- findInterval(number, lut)
  ix[ix == 0] <- 1


  number2 <- signif(number/lut[ix],3)
  number2[number2 < 1] <- signif(number2[number2 < 1], 2)
  number2[number2 < .1] <- signif(number2[number2 < .1], 1)

  sistring <- sub("^0", "", paste0(number2,pre[ix]))

  return(sistring)
}

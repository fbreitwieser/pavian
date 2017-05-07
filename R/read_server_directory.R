#' Title
#'
#' @param data_dir data directory
#' @param sample_set_name name of sample set
#' @param include_base_dir include base directory
#' @param display_messages display messages?
#'
#' @return resulting sample sets
#' @export
read_server_directory1 <- function(data_dir, sample_set_name = NULL,
                                  include_base_dir = T, display_messages = TRUE) {
  new_sample_sets <- list()
  read_error_msg <- list(val_neg=NULL, val_pos=NULL)

  return1 <- function(msg = NULL) {
    if (!is.null(msg))
      read_error_msg$neg <- msg
    if (isTRUE(display_messages)) {
      if (!is.null(read_error_msg$neg)) { warning(read_error_msg$neg) }
      if (!is.null(read_error_msg$pos)) { message(read_error_msg$pos) }
      return(new_sample_sets)
    } else {
      return(list(
        sample_sets = new_sample_sets,
        error_msg = read_error_msg
      ))
    }
  }

  message("reading files in ", data_dir)
  if (!dir.exists(data_dir)) {
    return1(paste("Directory ", data_dir, "does not exist."))
  }
  if (length(list.files(data_dir)) == 0) {
    return1(paste("No files in directory ", data_dir, "."))
  }
  n_files <- length(list.files(data_dir))
  max_files <- getOption("pavian.maxFiles", 100)
  if (n_files > max_files) {
    return1(paste("There are ",n_files," files in the directory, but the highest allowed number is ",max_files," files ", data_dir, " - please subdivide the data into smaller directories, or set the option 'pavian.maxFiles' to a higher number (e.g. 'options(pavian.maxFiles=250)')."))
  }

  base_name <- ifelse(!is.null(sample_set_name), sample_set_name, basename(data_dir))

  if (include_base_dir) {
    new_sample_sets <- list(read_sample_data(data_dir, ext=NULL))
    names(new_sample_sets) <- base_name
  }

  dirs <- grep("^\\.", list.dirs(data_dir, recursive = FALSE), invert = TRUE, value = TRUE)
  n_dirs <- length(dirs)
  max_dirs <- getOption("pavian.maxSubDirs", 25)
  if (n_dirs > max_dirs) {
    read_error_msg$val_neg <- c(read_error_msg$val_neg, paste("There are ",n_dirs," sub-directories in ", data_dir,
                                                              " but the highest allowed number is ",max_dirs,"  - specify individual directories with reports one at a time to load data or set the option 'pavian.maxSubDirs' to a higher number (e.g. 'options(pavian.maxSubDirs=50)')."))
  } else if (length(dirs) > 0) {
    sub_dir_sets <- lapply(dirs, read_sample_data, ext=NULL)
    names(sub_dir_sets) <- paste0(base_name,"/",basename(dirs))
    new_sample_sets <- c(new_sample_sets, sub_dir_sets)
  }

  bad_files <- unlist(sapply(new_sample_sets, attr, "bad_files"))
  sel_bad_sets <- sapply(new_sample_sets, function(x) is.null(x) || nrow(x) == 0)
  new_sample_sets <- new_sample_sets[!sel_bad_sets]

  if (length(new_sample_sets) > 0) {
    read_error_msg$val_pos <- sprintf("Added sample set%s <b>%s</b> with <b>%s</b> valid reports in total.",
                                      ifelse(length(new_sample_sets) == 1, "", "s"),
                                      paste(names(new_sample_sets), collapse="</b>, <b>"),
                                      sum(unlist(sapply(new_sample_sets, function(x) sum(x$FormatOK)))))
  }
  if (length(bad_files) > 0) {
    read_error_msg$val_neg <- c(read_error_msg$val_neg,
                                sprintf("The following files did not conform the report format: <br/> - <b>%s</b>",
                                        paste(bad_files, collapse="</b><br/> - <b>")))
  }
  return1()
}

#' Search for classification results in a directory and its children
#'
#' @param dirs_or_files data directory
#' @param sample_set_name name of sample set
#' @param existing_sample_set_names names of current sample sets that may be updated
#' @param unique_sample_set_name Require sample set names to be unique?
#' @param include_base_dir include base directory
#' @param display_messages display messages?
#' @param glob_files glob files?
#'
#' @return resulting sample sets
read_server_directory1 <- function(dirs_or_files, sample_set_name = NULL,
                                   existing_sample_set_names = NULL,
                                   unique_sample_set_name = FALSE,
                                  include_base_dir = T, display_messages = TRUE,
                                  glob_files = FALSE) {
  new_sample_sets <- list()
  read_error_msg <- list(val_neg=NULL, val_pos=NULL)

  msg1 <- function(msg) {
      read_error_msg$val_neg <- msg
  }

  get_sampleset_name <- function(data_dir, my_set_name = sample_set_name) {
    if (is.null(my_set_name)) {
      if (isTRUE(glob_files)) {
        my_set_name <- "Server files"
      } else {
        my_set_name <- basename(data_dir)
      }
    }
    if (unique_sample_set_name) {
      counter <- 1
      if (!is.null(existing_sample_set_names)) {
        ## Set a unique name for the uploaded samples 
        while (paste(my_set_name, counter) %in% existing_sample_set_names) {
          counter <- counter + 1
        }
      }
      my_set_name <- paste(my_set_name, counter)
    }
    my_set_name
  }

  file_infos <- file.info(dirs_or_files)
  file_sel <- !is.na(file_infos$isdir) & !file_infos$isdir
  if (any(file_sel)) {
      my_set_name <- get_sampleset_name("Server files")
      new_sample_sets[[my_set_name]] <- read_sample_data(rownames(file_infos)[file_sel], 
                                                         ext=NULL, is_files=TRUE)
  }

  if (any(!file_sel)) {
  for (data_dir in dirs_or_files[!file_sel]) {
    # get name for sample set
    my_set_name <- get_sampleset_name(data_dir, sample_set_name)
    
    bad_files <- c()
    if (isTRUE(glob_files)) {
      new_sample_sets[[my_set_name]] <- read_sample_data(data_dir, ext=NULL, glob_files=TRUE)
    } else {
      dmessage("Reading files in ", data_dir)
      if (!dir.exists(data_dir)) {
        msg1(paste("Directory ", data_dir, "does not exist."))
        next
      }
      if (length(list.files(data_dir)) == 0) {
        msg1(paste("No files in directory ", data_dir, "."))
        next
      }
      n_files <- length(list.files(data_dir))
      max_files <- getOption("pavian.maxFiles", 100)
      if (n_files > max_files) {
        msg1(paste("There are ",n_files," files in the directory, but the highest allowed number is ",max_files," files ", data_dir, " - please subdivide the data into smaller directories, or set the option 'pavian.maxFiles' to a higher number (e.g. 'options(pavian.maxFiles=250)')."))
        next
      }
    
      if (include_base_dir) {
        new_sample_sets[[my_set_name]] <- read_sample_data(data_dir, ext=NULL)
      }
  
      dirs <- grep("^\\.", list.dirs(data_dir, recursive = FALSE), invert = TRUE, value = TRUE)
      n_dirs <- length(dirs)
      max_dirs <- getOption("pavian.maxSubDirs", 10)
      if (n_dirs > max_dirs) {
        read_error_msg$val_neg <- c(read_error_msg$val_neg, paste("There are ",n_dirs," sub-directories in ", data_dir,
                                                                  " but the highest allowed number is ",max_dirs,"  - specify individual directories with reports one at a time to load data or set the option 'pavian.maxSubDirs' to a higher number (e.g. 'options(pavian.maxSubDirs=50)')."))
      } else if (length(dirs) > 0) {
        sub_dir_sets <- lapply(dirs, read_sample_data, ext=NULL)
        names(sub_dir_sets) <- paste0(my_set_name,"/",basename(dirs))
        new_sample_sets <- c(new_sample_sets, sub_dir_sets)
      }
    }
  }
  }
    
    paste_last <- function(x, ..., collapse_last) {
      if (length(x) ==1)
        return(x)
      
      y <- paste(x[-length(x)], ...)
      paste(y, x[length(x)], sep=collapse_last)
    }
  
    sel_bad_sets <- sapply(new_sample_sets, function(x) is.null(x) || nrow(x) == 0)
    bad_files <- unlist(sapply(new_sample_sets, attr, "bad_files"))
    new_sample_sets <- new_sample_sets[!sel_bad_sets]
    
    if (length(new_sample_sets) > 0) {
      read_error_msg$val_pos <- sprintf("Added sample set%s <b>%s</b> with <b>%s</b> valid reports in total.",
                                        ifelse(length(new_sample_sets) == 1, "", "s"),
                                        paste_last(names(new_sample_sets), collapse="</b>, <b>", collapse_last="</b> and <b>"),
                                        sum(unlist(sapply(new_sample_sets, function(x) sum(x$FormatOK)))))
    }
    if (length(bad_files) > 0) {
      read_error_msg$val_neg <- c(read_error_msg$val_neg,
                                  sprintf("The following files did not conform the report format: <br/> - <b>%s</b>",
                                          paste(bad_files, collapse="</b><br/> - <b>")))
    }

    if (isTRUE(display_messages)) {
      if (!is.null(read_error_msg$val_neg)) { warning(read_error_msg$val_neg) }
      if (!is.null(read_error_msg$val_pos)) { dmessage(read_error_msg$val_pos) }
    } 
    return(list(
        sample_sets = new_sample_sets,
        error_msg = read_error_msg))
}

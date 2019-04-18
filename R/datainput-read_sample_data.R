
#' Read sample definitions from a directory
#'
#' @param my_dir directory
#' @param def_filename filename
#' @param ext extension of report files
#' @param glob_files glob files?
#' @param is_files Is input a list of files?
#'
#' @return sample_data data.frame
#' @export
read_sample_data <- function(my_dir, def_filename = "sample_data.csv",
                             ext = c("report", "profile", "report.tsv"), 
                             glob_files = FALSE, is_files = FALSE) {
  gd_sample_data <- FALSE
    
  if (isTRUE(file.exists(file.path(my_dir,def_filename)))) {
    sample_data <- utils::read.delim(file.path(my_dir,def_filename), 
                                     header = TRUE, sep = ";", stringsAsFactors = FALSE)

    if (!"ReportFile" %in% colnames(sample_data)){
      warning("Required column 'ReportFile' not present in ",def_filename)
    } else if (!"Name" %in% colnames(sample_data)){
      warning("Required column 'Name' not present in ",def_filename)
    } else {
      gd_sample_data <- TRUE
    }
  }
  
  if (gd_sample_data) {
    if (!"ReportFilePath" %in% colnames(sample_data))
      sample_data$ReportFilePath <- file.path(my_dir, sample_data$ReportFile)
  } else {
    if (isTRUE(glob_files)) {
      ReportFilePaths <- Sys.glob(my_dir)
    } else if (isTRUE(is_files)) {
      ReportFilePaths <- my_dir
    } else {
      ReportFilePaths <- setdiff(list.files(path = my_dir, full.names = TRUE),list.dirs(path = my_dir, recursive = FALSE, full.names = TRUE))
      #ReportFiles <- ReportFiles[ReportFiles != def_filename]
      if (!is.null(ext))
        ReportFilePaths <- ReportFilePaths[grepl(paste0(ext, "$", collapse="|"), ReportFilePaths)]
    }
    ReportFiles <- basename(ReportFilePaths)
    Name = basename(ReportFiles)
    if (any(duplicated(Name))) {
      Name = ReportFilePaths   
      while(length(unique(substr(Name, nchar(Name), nchar(Name)))) == 1) {
        Name <- substr(Name, 1, nchar(Name) - 1)
      }
    } else {
      if (length(Name) > 1) {
        while(max(nchar(Name)) >= 2 && length(unique(substr(Name, nchar(Name), nchar(Name)))) == 1) {
          Name <- substr(Name, 1, nchar(Name) - 1)
        }
      }
    }

    sample_data <- data.frame(Name,
                              ReportFile = ReportFiles, 
                              ReportFilePath = ReportFilePaths,
                              stringsAsFactors = FALSE)
  }

  #if (length(sample_data) == 0 || nrow(sample_data) == 0) {
  #  return(NULL)
  #}

  #if ("Class" %in% colnames(sample_data))
  #  sample_data$Class <- as.factor(sample_data$Class)

  if (!isTRUE(glob_files) && !isTRUE(is_files)) {
  if ("CentrifugeOutFile" %in% colnames(sample_data) && !"CentrifugeOutFilePath" %in% colnames(sample_data))
    sample_data$CentrifugeOutFilePath <- file.path(my_dir, sample_data$CentrifugeOutFile)

  if ("KrakenFile" %in% colnames(sample_data) && ! "KrakenFilePath" %in% colnames(sample_data))
    sample_data$KrakenFilePath <- file.path(my_dir, sample_data$KrakenFile)

  if ("FastqFile" %in% colnames(sample_data) && ! "FastqFilePath" %in% colnames(sample_data))
    sample_data$FastqFilePath <- file.path(my_dir, sample_data$FastqFile)
  }

  if (!"Include" %in% colnames(sample_data))
    sample_data <- cbind(FormatOK = rep(TRUE, nrow(sample_data)), 
                         Include = rep(TRUE, nrow(sample_data)), sample_data)

  good_files <- sapply(sample_data$ReportFilePath, function(x) length(read_report(x, check_file=T)) != 0)
  bad_files <- sample_data$ReportFile[!good_files]

  if (length(good_files) > 0)
    sample_data <- sample_data[good_files, ]

  attr(sample_data, "bad_files") <- bad_files
  sample_data
}

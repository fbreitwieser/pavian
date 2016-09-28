
#' @export
get_reports_def_df <- function(my_dir, def_filename = "defs.csv", report_extension = ".report$") {

  gd_def_df <- FALSE

  if (file.exists(file.path(my_dir,def_filename))) {
    def_df <- read.delim(file.path(my_dir,def_filename), header = TRUE, sep = ";", stringsAsFactors = FALSE)

    if (!"ReportFile" %in% colnames(def_df)){
      warning("Required column 'ReportFile' not present in defs.csv")
    } else if (!"Name" %in% colnames(def_df)){
      warning("Required column 'Name' not present in defs.csv")
    } else {
      gd_def_df <- TRUE
    }
  }
  if (!gd_def_df) {
    ReportFiles = list.files(path = my_dir, pattern = report_extension )
    def_df <- data.frame(Name = sub(".report$", "", basename(ReportFiles)), ReportFile = ReportFiles, stringsAsFactors = FALSE)
  }

  if (length(def_df) == 0 || nrow(def_df) == 0) {
    return(NULL)
  }

  #if ("Class" %in% colnames(def_df))
  #  def_df$Class <- as.factor(def_df$Class)

  if (!"ReportFilePath" %in% colnames(def_df))
    def_df$ReportFilePath <- file.path(my_dir, def_df$ReportFile)

  if ("CentrifugeOutFile" %in% colnames(def_df) && !"CentrifugeOutFilePath" %in% colnames(def_df))
    def_df$CentrifugeOutFilePath <- file.path(my_dir, def_df$CentrifugeOutFile)

  if ("KrakenFile" %in% colnames(def_df) && ! "KrakenFilePath" %in% colnames(def_df))
    def_df$KrakenFilePath <- file.path(my_dir, def_df$KrakenFile)

  if ("FastqFile" %in% colnames(def_df) && ! "FastqFilePath" %in% colnames(def_df))
    def_df$FastqFilePath <- file.path(my_dir, def_df$FastqFile)

  if (!"Include" %in% colnames(def_df))
    def_df <- cbind(Include = file.exists(def_df$ReportFilePath), def_df)


  def_df
}

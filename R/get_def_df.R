
#' @export
get_reports_def_df <- function(my_dir, def_filename = "defs.csv", report_extension = ".report$") {
  if (file.exists(file.path(my_dir,def_filename))) {
    def_df <- read.delim(file.path(my_dir,def_filename), header = TRUE, sep = ";", stringsAsFactors = FALSE)

    if (!"ReportFile" %in% colnames(def_df)){
      stop("Required column 'ReportFile' not present in defs.csv")
    }
  } else {
    ReportFiles = list.files(path = my_dir, pattern = report_extension )
    def_df <- data.frame(Name = sub(".report$", "", basename(ReportFiles)), ReportFile = ReportFiles, stringsAsFactors = FALSE)
  }

  if (length(def_df) == 0 || nrow(def_df) == 0) {
    return(NULL)
  }

  if (!"Include" %in% colnames(def_df))
    def_df <- cbind(Include = TRUE, def_df)

  #if ("Class" %in% colnames(def_df))
  #  def_df$Class <- as.factor(def_df$Class)

  if (!"ReportFilePath" %in% colnames(def_df))
    def_df$ReportFilePath <- file.path(my_dir, def_df$ReportFile)

  def_df <- def_df[file.exists(def_df$ReportFilePath),]
}

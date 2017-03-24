REPORTATTR_vec <- c(
  data_column_start = "data_column_start",
  data_columns = "data_columns",
  reads_columns = "read_columns",
  reads_stay_columns = "reads_stay_columns",
  stat_column = "stat_column",
  taxonid_column = "taxonid_column"
)

REPORTATTR <- function(name) {
  if (!name %in% REPORTATTR_vec) {
    stop(name, " is not a report attribute")
  }
  REPORTATTR_vec[[name, exact=TRUE]]
}

req_attr <- function(mydf, myattr) {
  validate(need(myattr), "Need valid attribute definition!")
  validate(need(attr(mydf,myattr, exact=TRUE)), paste("Attribute", myattr, "is not truthy!"))
  return(attr(mydf,myattr, exact=TRUE))
}

normalize_data_cols <- function(merged_reports, normalize_col = REPORTATTR("reads_stay_columns"), sum_reads = NULL) {
  data_columns <- req_attr(merged_reports, REPORTATTR("data_columns"))
  normalize_columns <- req_attr(merged_reports, normalize_col)

  validate(need(data_columns, message="data_columns is NULL"),
           need(normalize_columns, message=paste(normalize_col,"is NULL")))

  if (is.null(sum_reads)) {
    sum_reads <- colSums(merged_reports[, normalize_columns, drop=F], na.rm = T)
  }

  if(length(sum_reads) != length(normalize_columns))
    stop("length(sum_reads) != length(normalize_columns)")

  sum_reads <- rep(sum_reads, each = length(data_columns) / length(normalize_columns))

  merged_reports[, data_columns] <- 100*scale(merged_reports[, data_columns],
                                                 center = rep(0, length(sum_reads)),
                                                 scale = sum_reads)

  merged_reports
}

log_data_cols <- function(merged_reports) {
  data_columns <- req_attr(merged_reports, REPORTATTR("data_columns"))
  merged_reports[, data_columns] <- log10(merged_reports[, data_columns, drop=F] + 1)

  merged_reports
}

calc_robust_zscore <- function(merged_reports, min_scale = 1) {
  data_columns <- req_attr(merged_reports, REPORTATTR("data_columns"))
  stopifnot(!is.null(data_columns))

  dp2 <- merged_reports[, data_columns, drop=F]
  dp2[is.na(dp2)] <- 0
  merged_reports[, data_columns] <- t(scale(t(merged_reports[, data_columns, drop=F]),
                                           center = apply(dp2,1,median),
                                           scale  = pmax(apply(dp2,1,stats::mad), min_scale)))

  merged_reports
}

filter_reports_to_rank <- function(my_reports, classification_rank) {
  lapply(my_reports, function(my_report) {

    ## subset report to the requested rank
    if (!is.null(classification_rank) && !(any(classification_rank == "-"))) {
      my_report[my_report$Rank == classification_rank, ]
    }
  })
}





#' Merge report files into a wide format, with column(s) for each
#' report.
#'
#' @param my_reports Report data.frames.
#' @param numeric_col Numeric columns to keep.
#'
#' @return Combined data.frame
#' @export
merge_reports <- function(my_reports, numeric_col = c("reads","reads_stay")) {
  ## generate data.frame which has a name column (species name) and a further reads column for each sample
  id_cols_before <- c("name", "rank")

  if (all(sapply(my_reports, function(x) "taxonid" %in% colnames(x) ))) {
    id_cols_before <- c(id_cols_before, "taxonid")
  }

  id_cols_after <- c("taxonstring")
  id_cols <- c(id_cols_before, id_cols_after)

  if (is.null(my_reports) || length(my_reports) == 0) {
    return(NULL)
  }
  idx_of_numeric_col <- seq(from = length(id_cols) + 1, to = length(id_cols) + length(numeric_col))

  my_reports <- lapply(names(my_reports), function(report_name) {

    my_report <- my_reports[[report_name]][ ,c(id_cols, numeric_col), drop=FALSE]
    my_report <- my_report[rowSums(my_report[, numeric_col, drop=F], na.rm=T)>0, , drop=FALSE]

    if (nrow(my_report) > 0) {
      ## set zeros to NA
      my_report[numeric_col][!is.na(my_report[numeric_col]) & my_report[numeric_col] == 0] <- NA
    }

    ## set the basename of the report file as name for the numeric column
    colnames(my_report)[idx_of_numeric_col] <- sub(".*/(.*).report", "\\1", report_name)
    if (length(numeric_col) > 1) {
      colnames(my_report)[idx_of_numeric_col] <-
        paste(colnames(my_report)[idx_of_numeric_col], numeric_col, sep = "\n")
    }
    my_report
  })

  idx_of_numeric_col_merged <- seq(from = length(id_cols) + 1, length.out = length(my_reports)*length(numeric_col))

  ## merge all the data.frames in the my_reports list, and add additional info (sparkline and mean)
  merged_reports <-
    Reduce(function(x, y)
      merge(x, y, all = TRUE, by = id_cols), my_reports)

  merged_reports <- cbind(
        beautify_colnames(merged_reports[, id_cols_before, drop = FALSE]),
        OVERVIEW = rep(NA, nrow(merged_reports)),
        STAT = rep(NA, nrow(merged_reports)),  ## Placeholder
        merged_reports[, idx_of_numeric_col_merged, drop = FALSE],
        beautify_colnames(merged_reports[, id_cols_after, drop = FALSE])
      )

  ## make a link to NCBI genome browser in the taxonID column
  if (!"Taxonid" %in% colnames(merged_reports)) {
    taxonid_column <- NA
  } else {
    taxonid_column <- which(colnames(merged_reports) == "Taxonid")
    stopifnot(length(taxonid_column) == 1)
    merged_reports[, taxonid_column] <- sub("^  *", "", merged_reports[, taxonid_column])
  }
  ## remove s_, g_, etc
  merged_reports[, 1] <- sub("^[a-z-]_", "", merged_reports[, 1])

  for (col in numeric_col) {
    attr(merged_reports, paste0(col,"_columns")) <-
      seq(from = length(id_cols) + which(numeric_col == col),
          length.out = length(my_reports), by = length(numeric_col)) + 1
  }


  attr(merged_reports, REPORTATTR("data_column_start")) <- length(id_cols) + 2
  attr(merged_reports, REPORTATTR("data_columns")) <- idx_of_numeric_col_merged + 1
  attr(merged_reports, REPORTATTR("stat_column")) <- which(colnames(merged_reports) == "STAT")
  attr(merged_reports, REPORTATTR("taxonid_column")) <- taxonid_column

  class(merged_reports) <- append(class(merged_reports),"merged_reports")

  merged_reports
}

assayData <- function(x, ...) UseMethod("assayData", x)
assayData.merged_reports <- function(x)
  list("reads"=x[, req_attr(x, REPORTATTR("reads_columns"))],
       "reads_stay"=x[, req_attr(x, REPORTATTR("reads_stay_columns"))])


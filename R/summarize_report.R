
normalize_data_cols <- function(summarized_report, normalize_col = "reads_stay_columns") {
  data_columns <- attr(summarized_report, "data_columns")
  normalize_columns <- attr(summarized_report, normalize_col)

  validate(need(data_columns, message="data_columns is NULL"),
           need(normalize_columns, message=paste(normalize_col,"is NULL")))

  sum_reads <- colSums(summarized_report[, normalize_columns, drop=F], na.rm = T)
  sum_reads <- rep(sum_reads, each = length(data_columns) / length(normalize_columns))

  summarized_report[, data_columns] <- 100*scale(summarized_report[, data_columns],
                                                 center = rep(0, length(sum_reads)),
                                                 scale = sum_reads)

  summarized_report
}

log_data_cols <- function(summarized_report) {
  data_columns <- attr(summarized_report, "data_columns")
  summarized_report[, data_columns] <- log10(summarized_report[, data_columns, drop=F] + 1)

  summarized_report
}

calc_robust_zscore <- function(summarized_report, min_scale = 1) {
  data_columns <- attr(summarized_report, "data_columns")
  stopifnot(!is.null(data_columns))

  dp2 <- summarized_report[, data_columns, drop=F]
  dp2[is.na(dp2)] <- 0
  summarized_report[, data_columns] <- t(scale(t(summarized_report[, data_columns, drop=F]),
                                           center = apply(dp2,1,median),
                                           scale  = pmax(apply(dp2,1,stats::mad), min_scale)))

  summarized_report
}

filter_reports_to_rank <- function(my_reports, classification_rank) {
  lapply(my_reports, function(my_report) {

    ## subset report to the requested rank
    if (!is.null(classification_rank) && !(any(classification_rank == "-"))) {
      my_report[my_report$Rank == classification_rank, ]
    }
  })
}

# numeric_col <- c("reads", "reads_stay")
get_summarized_report2 <- function(my_reports, numeric_col = "reads_stay") {
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
  summarized_report <-
    Reduce(function(x, y)
      merge(x, y, all = TRUE, by = id_cols), my_reports)

  summarized_report <- cbind(
        beautify_colnames(summarized_report[, id_cols_before, drop = FALSE]),
        OVERVIEW = rep(NA, nrow(summarized_report)),
        STAT = rep(NA, nrow(summarized_report)),  ## Placeholder
        summarized_report[, idx_of_numeric_col_merged, drop = FALSE],
        beautify_colnames(summarized_report[, id_cols_after, drop = FALSE])
      )



  ## make a link to NCBI genome browser in the taxonID column
  if (!"Taxonid" %in% colnames(summarized_report)) {
    taxonid_column <- NA
  } else {
    taxonid_column <- which(colnames(summarized_report) == "Taxonid")
    stopifnot(length(taxonid_column) == 1)
    summarized_report[, taxonid_column] <- sub("^  *", "", summarized_report[, taxonid_column])
  }
  ## remove s_, g_, etc
  summarized_report[, 1] <- sub("^[a-z-]_", "", summarized_report[, 1])

  for (col in numeric_col) {
    attr(summarized_report, paste0(col,"_columns")) <-
      seq(from = length(id_cols) + which(numeric_col == col),
          length.out = length(my_reports), by = length(numeric_col)) + 1
  }


  attr(summarized_report, "data_column_start") <- length(id_cols) + 2
  attr(summarized_report, "data_columns") <- idx_of_numeric_col_merged + 1
  attr(summarized_report, "stat_column") <- which(colnames(summarized_report) == "STAT")
  attr(summarized_report, "taxonid_column") <- taxonid_column

  summarized_report
}


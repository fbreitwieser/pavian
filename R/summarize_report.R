

get_summarized_report <- function(
  my_reports,
  display_percentage = FALSE,
  show_zscore = FALSE,
  show_reads_stay = "reads_stay",
  classification_level = NULL,
  summary_stat = "Mean",
  as_matrix = FALSE) {
  ## generate data.frame which has a name column (species name) and a further reads column for each sample
  id_cols_before <- c("name", "level", "taxonid")
  numeric_col <- c("reads", "reads_stay")
  id_cols_after <- c("taxonstring")
  id_cols <- c(id_cols_before, id_cols_after)

  if (is.null(my_reports) || length(my_reports) == 0) {
    return(NULL)
  }
  my_reports <- lapply(names(my_reports), function(report_name) {
    my_report <- my_reports[[report_name]]
    if (length(my_report) == 0)
      stop("Report ",report_name," is empty")
    ## filter contaminants if defined

    ## subset report to the requested level
    if (!is.null(classification_level) &&
        !(any(classification_level == "-"))) {
      my_report <- my_report[my_report$level == classification_level, ]
    }
    my_report <- my_report[, c(id_cols, numeric_col)]



    ## set the basename of the report file as name for the numeric column
    idx_of_numeric_col <-
      seq(from = length(id_cols) + 1, to = ncol(my_report))
    colnames(my_report)[idx_of_numeric_col] <-
      sub(".*/(.*)(-PT.*)?.report", "\\1", report_name)
    if (length(numeric_col) > 1) {
      colnames(my_report)[idx_of_numeric_col] <-
        paste(colnames(my_report)[idx_of_numeric_col], numeric_col, sep = "\n")
    }
    my_report
  })

  ## merge all the data.frames in the my_reports list, and add additional info (sparkline and mean)
  summarized_report <-
    Reduce(function(x, y)
      merge(x, y, all = TRUE, by = id_cols), my_reports)

  ## transform to percent
  data_portion <-
    summarized_report[, seq(from = length(id_cols) + 1, to = ncol(summarized_report)), drop = FALSE]

  reads_idx <-      seq(from = 1, to = ncol(data_portion) - 1, by = 2)
  reads_stay_idx <- seq(from = 2, to = ncol(data_portion), by = 2)

  if (show_reads_stay == "reads") {
    data_portion <- data_portion[, reads_idx, drop = FALSE]
    colnames(data_portion) <-
      sub("\nreads", "", colnames(data_portion))
  } else if (show_reads_stay == "reads_stay") {
    data_portion <- data_portion[, reads_stay_idx, drop = FALSE]
    colnames(data_portion) <-
      sub("\nreads_stay", "", colnames(data_portion))
  }

  if (isTRUE(display_percentage)) {
    sum_reads <- colSums(data_portion, na.rm = T)
    #if (show_reads_stay == "both")
    #  sum_reads <- rep(sum_reads, each = 2)
    data_portion <- 100 * t(t(data_portion) / sum_reads)
  }



  if (isTRUE(show_zscore)) {
    ## The minimum scaling factor is pretty arbitrarily chosen
    min_scale <- ifelse(isTRUE(display_percentage), 0.001, 1)
    min_one <- function(x) max(min_scale, x)

    dp2 <- data_portion
    sel.na <- is.na(dp2)
    dp2[sel.na] <- 0
    data_portion <- t(scale(t(data_portion), center = apply(dp2,1,median), scale=min_one(apply(dp2,1,stats::mad))))
    #data_portion[sel.na] <- NA
    #dimnames(data_portion) <- dimnames(dp2)
    rm(dp2)
  }


  if (as_matrix) {
    row_names <- summarized_report[, 1]
    summarized_report <- as.matrix(data_portion)
    rownames(summarized_report) <- gsub("^[a-z-]_", "", row_names)
  } else {
    if (isTRUE(display_percentage) || isTRUE(show_zscore)) {
      data_portion <- signif(data_portion, 3)
    }

    stat_column_data <-
      signif(apply(zero_if_na(data_portion), 1, stat_name_to_f[[summary_stat]]), 3)

    round_digits <- ifelse(isTRUE(display_percentage), 3, 1)
    summarized_report <-
      cbind(
        beautify_colnames(summarized_report[, id_cols_before, drop = FALSE]),
        Overview = apply(round(zero_if_na(data_portion), round_digits), 1, paste0, collapse = ","),
        STAT = stat_column_data,
        data_portion,
        beautify_colnames(summarized_report[, id_cols_after, drop = FALSE])
      )

    stat_column <- which(colnames(summarized_report) == "STAT")

    colnames(summarized_report)[stat_column] <- summary_stat

    ## that's the last column before the data, and the one which we sort for

    stopifnot(length(stat_column) == 1)
    data_columns <- seq(from = stat_column + 1,
                        to = ncol(summarized_report) - length(id_cols_after))

    ## make a link to NCBI genome browser in the taxonID column
    taxonid_column <-
      which(colnames(summarized_report) == "Taxonid")
    summarized_report[, taxonid_column] <-
      gsub("^  *", "", summarized_report[, taxonid_column])
    stopifnot(length(taxonid_column) == 1)

    ## remove s_, g_, etc
    summarized_report[, 1] <-
      gsub("^[a-z-]_", "", summarized_report[, 1])
    attr(summarized_report, "stat_column") <- stat_column
    attr(summarized_report, "taxonid_column") <- taxonid_column
    attr(summarized_report, "data_columns") <- data_columns
  }
  summarized_report
}


normalize_data_cols <- function(summarized_report) {
  data_columns <- attr(summarized_report, "data_columns")
  sum_reads <- colSums(summarized_report[, data_columns, drop=F], na.rm = T)
  summarized_report[, data_columns] <- 100 * t(t(summarized_report[, data_columns, drop=F]) / sum_reads)

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

filter_reports_to_level <- function(my_reports, classification_level) {
  lapply(my_reports, function(my_report) {

    ## subset report to the requested level
    if (!is.null(classification_level) && !(any(classification_level == "-"))) {
      my_report[my_report$Level == classification_level, ]
    }
  })
}

# numeric_col <- c("reads", "reads_stay")
get_summarized_report2 <- function(my_reports, numeric_col = "reads_stay") {
  ## generate data.frame which has a name column (species name) and a further reads column for each sample
  id_cols_before <- c("name", "level", "taxonid")

  id_cols_after <- c("taxonstring")
  id_cols <- c(id_cols_before, id_cols_after)

  if (is.null(my_reports) || length(my_reports) == 0) {
    return(NULL)
  }
  my_reports <- lapply(names(my_reports), function(report_name) {
    my_report <- my_reports[[report_name]]
    if (length(my_report) == 0)
      stop("Report ",report_name," is empty")

    sel_val <- apply(my_report[,numeric_col, drop=FALSE] > 0, 1, any)
    my_report <- my_report[sel_val, c(id_cols, numeric_col), drop=FALSE]

    ## set the basename of the report file as name for the numeric column
    idx_of_numeric_col <- seq(from = length(id_cols) + 1, to = ncol(my_report))
    colnames(my_report)[idx_of_numeric_col] <- sub(".*/(.*).report", "\\1", report_name)
    if (length(numeric_col) > 1) {
      colnames(my_report)[idx_of_numeric_col] <-
        paste(colnames(my_report)[idx_of_numeric_col], numeric_col, sep = "\n")
    }
    my_report
  })

  ## merge all the data.frames in the my_reports list, and add additional info (sparkline and mean)
  summarized_report <-
    Reduce(function(x, y)
      merge(x, y, all = TRUE, by = id_cols), my_reports)


  data_portion_names <- colnames(summarized_report[, seq(from = length(id_cols) + 1, to = ncol(summarized_report)), drop = FALSE])

  summarized_report <- cbind(
        beautify_colnames(summarized_report[, id_cols_before, drop = FALSE]),
        OVERVIEW = NA,
        STAT = NA,  ## Placeholder
        summarized_report[, seq(from = length(id_cols) + 1, to = ncol(summarized_report)), drop = FALSE],
        beautify_colnames(summarized_report[, id_cols_after, drop = FALSE])
      )



  ## make a link to NCBI genome browser in the taxonID column
  taxonid_column <- which(colnames(summarized_report) == "Taxonid")
  stopifnot(length(taxonid_column) == 1)
  summarized_report[, taxonid_column] <- sub("^  *", "", summarized_report[, taxonid_column])

  ## remove s_, g_, etc
  summarized_report[, 1] <- sub("^[a-z-]_", "", summarized_report[, 1])
  attr(summarized_report, "data_columns") <- which(colnames(summarized_report) %in% data_portion_names)
  attr(summarized_report, "stat_column") <- which(colnames(summarized_report) == "STAT")
  attr(summarized_report, "taxonid_column") <- taxonid_column

  summarized_report
}


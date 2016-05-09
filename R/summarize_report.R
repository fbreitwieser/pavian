
get_summarized_report <- function(
  my_reports,
  filter_contaminants,
  display_percentage,
  show_reads_stay,
  classification_level = NULL,
  remove_root_hits = FALSE,
  as_matrix = FALSE) {
  ## generate data.frame which has a name column (species name) and a further reads column for each sample
  id_cols_before <- c("name", "level", "taxonid")
  numeric_col <- c("reads", "reads_stay")
  id_cols_after <- c("taxonstring")
  id_cols <- c(id_cols_before, id_cols_after)

  if (is.null(my_reports)) {
    return(NULL)
  }
  my_reports <- lapply(names(my_reports), function(report_name) {
    my_report <- my_reports[[report_name]]
    ## filter contaminants if defined
    for (c in filter_contaminants)
      my_report <- filter_taxon(my_report, c)

    ## subset report to the requested level
    if (!is.null(classification_level) &&
        !(any(classification_level == "-"))) {
      my_report <- my_report[my_report$level == classification_level, ]
    }
    my_report <- my_report[, c(id_cols, numeric_col)]

    if (isTRUE(remove_root_hits))
      my_report <- my_report[my_report$name != "-_root", ]


    ## set the basename of the report file as name for the numeric column
    idx_of_numeric_col <-
      seq(from = length(id_cols) + 1, ncol(my_report))
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
    summarized_report[, seq(from = length(id_cols) + 1,
                            to = ncol(summarized_report))]

  reads_idx <- seq(from = 1,
                   to = ncol(data_portion) - 1,
                   by = 2)
  reads_stay_idx <- seq(from = 2,
                        to = ncol(data_portion),
                        by = 2)

  if (isTRUE(display_percentage)) {
    sum_reads <- colSums(data_portion[, reads_stay_idx], na.rm = T)
  }
  if (show_reads_stay == "reads") {
    data_portion <- data_portion[, reads_idx]
    colnames(data_portion) <-
      sub("\nreads", "", colnames(data_portion))
  } else if (show_reads_stay == "reads_stay") {
    data_portion <- data_portion[, reads_stay_idx]
    colnames(data_portion) <-
      sub("\nreads_stay", "", colnames(data_portion))
  }

  if (isTRUE(display_percentage)) {
    if (show_reads_stay == "both")
      sum_reads <- rep(sum_reads, each = 2)
    data_portion <- signif(100 * t(t(data_portion) / sum_reads), 3)
  }

  if (as_matrix) {
    row_names <- summarized_report[, 1]
    summarized_report <- as.matrix(data_portion)
    rownames(summarized_report) <- gsub("^[a-z-]_", "", row_names)
  } else {
    if (isTRUE(display_percentage)) {
      mean_column <-
        signif(rowSums(data_portion, na.rm = TRUE) / ncol(data_portion),
               3)
    } else {
      mean_column <-
        signif(rowSums(data_portion, na.rm = TRUE) / ncol(data_portion),
               3)
    }
    round_digits <- ifelse(isTRUE(display_percentage), 3, 1)
    summarized_report <-
      cbind(
        beautify_colnames(summarized_report[, id_cols_before, drop = FALSE]),
        Overview = apply(round(zero_if_na(
          log10(10 * data_portion)
        ), round_digits), 1, paste0, collapse = ","),
        Mean = mean_column,
        data_portion,
        beautify_colnames(summarized_report[, id_cols_after, drop =
                                              FALSE])
      )

    ## that's the last column before the data, and the one which we sort for
    mean_column <- which(colnames(summarized_report) == "Mean")
    stopifnot(length(mean_column) == 1)
    data_columns <- seq(from = mean_column + 1,
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
    attr(summarized_report, "mean_column") <- mean_column
    attr(summarized_report, "taxonid_column") <- taxonid_column
    attr(summarized_report, "data_columns") <- data_columns
  }
  summarized_report
}


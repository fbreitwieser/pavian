formatSummaryDT <- function(dt, sample_data=NULL,
			     display_percent = TRUE, ...) {
    start_color_bar_at <- 2  ## length of extra_cols + 1
    samples_summary <- dt$x$data
    number_range <-  c(0, max(samples_summary[, start_color_bar_at], na.rm = TRUE))
    stopifnot(is.numeric(number_range))

    if (isTRUE(display_percent)) {
      ## add a custom renderer.
      start_color_bar_at <- start_color_bar_at + 1
      number_range <- c(0, 100)
    }

    microbial_col <- 7

  n_data_cols <- ncol(samples_summary)
  max_microbial_reads <- max(samples_summary[, microbial_col], na.rm = TRUE)
  if (isTRUE(display_percent)) {
    
    dt <- dt %>%
      DT::formatStyle(
        colnames(samples_summary)[2],
        background = styleColorBar2(c(0,max(samples_summary[[2]],na.rm=T)), 'lightsalmon')
      ) %>%
      DT::formatStyle(
        colnames(samples_summary)[seq(from=start_color_bar_at, to=microbial_col-1)],
        background = styleColorBar2(number_range, 'lightblue')
      ) %>%
      DT::formatStyle(colnames(samples_summary)[seq(from=microbial_col,to=n_data_cols)],
                      background = styleColorBar2(c(0, max_microbial_reads), 'lightgreen')) %>%
      DT::formatCurrency(start_color_bar_at - 1, currency = '', digits = 0) %>%
      DT::formatString(seq(from=start_color_bar_at, to=n_data_cols), suffix = '%')  ## TODO: display as percent
    #   ## not implemented for now as formatPercentage enforces a certain number of digits, but I like to round
    #   ## with signif.
  } else {
    dt <-
      dt %>% DT::formatStyle(
        colnames(samples_summary)[seq(from=start_color_bar_at, to=microbial_col-1)],
        background = styleColorBar2(number_range, 'lightsalmon')
      ) %>%
      DT::formatStyle(colnames(samples_summary)[seq(from=microbial_col,to=n_data_cols)],
                      background = styleColorBar2(c(0, max_microbial_reads), 'lightgreen'))

      dt <- dt %>% DT::formatCurrency(seq(from=start_color_bar_at, to=n_data_cols),
                                      currency = '', digits = 0)
  }

  dt
}

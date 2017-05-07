#' UI part of report overview module
#'
#' @param id Shiny namespace id.
#'
#' @return UI elements for report overview module.
#' @export
#' @import shiny
reportOverviewModuleUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    HTML("This page shows the summary of the classifications in the selected sample set.
         The table cells have a barchart that shows the relation of the cell value to other cell values in the same category, with the microbiota columns being a separate category from the rest."),
    checkboxInput(ns("opt_samples_overview_percent"), label = "Show percentages instead of number of cladeReads", value = FALSE),
    DT::dataTableOutput(ns('dt_samples_overview')),
    br(),
    HTML("If the table does not display at first, double-click the checkbox to reload it.")

  )

}

## TODO: use replacedata to replace the data https://github.com/rstudio/DT/issues/168
## this stops flickering in the when displaying percent - but might not work with changing the formatting

#' Shiny modules to display an overview of metagenomics reports
#'
#' @param input Shiny input object.
#' @param output Shiyn output object.
#' @param session Shiny session object.
#' @param sample_data Samples \code{data.frame}.
#' @param reports List of reports.
#' @param datatable_opts Additional datatable opts (mostly $class)
#'
#' @return Report overview module server functionality.
#' @export
#' @import shiny
reportOverviewModule <- function(input, output, session, sample_data, reports, datatable_opts = NULL) {
  #r_state <- list()

  get_samples_summary <- reactive( {
    validate(need(reports(), message = "No data available."))

    withProgress({
    samples_summary <- do.call(rbind, lapply(reports(), summarize_report))

    ## Add a 'Name' column in front
    samples_summary$Name <- rownames(samples_summary)
    extra_cols <- c("Name")
    samples_summary <- samples_summary[,c(extra_cols, setdiff(colnames(samples_summary),extra_cols))]
    colnames(samples_summary) <- beautify_string(colnames(samples_summary))

    samples_summary
    }, message = "Summarizing sample contents ... ")
  })



  ## Samples overview output
  output$dt_samples_overview <- DT::renderDataTable({

    samples_summary <- get_samples_summary()
    validate(need(samples_summary, message = "Error in getting samples summary - please re-select the sample set."))
    validate(need(sample_data(), message = "No data available."))
    validate(need(nrow(samples_summary)==nrow(sample_data()), message = "Different number of rows in summary and sample data"))

    format_datatable(samples_summary, sample_data=sample_data(),
                     datatable_opts=datatable_opts,
                     display_percent=input$opt_samples_overview_percent)
  })

}


format_datatable <- function(samples_summary, sample_data, datatable_opts, display_percent) {
     start_color_bar_at <- 2  ## length of extra_cols + 1
    number_range <-  c(0, max(samples_summary[, 2], na.rm = TRUE))

    if (isTRUE(display_percent)) {
      ## add a custom renderer.
      start_color_bar_at <- start_color_bar_at + 1
      number_range <- c(0, 100)
      samples_summary[, start_color_bar_at:ncol(samples_summary)] <-
        100 * signif(sweep(samples_summary[, start_color_bar_at:ncol(samples_summary)], 1, samples_summary[, 2], `/`), 4)

      ## TODO: Define columnDefs and give read counts on mouse-over
    }

    microbial_col <- 7

    if (!max(samples_summary[,2]) > 1000) {
      ## For Metaphlen reports - all numbers are percent
      samples_summary[,seq(from=start_color_bar_at, to=ncol(samples_summary))] <- signif(samples_summary[,seq(from=start_color_bar_at, to=ncol(samples_summary))], 4)
    }

    add_columns <- setdiff(colnames(sample_data), colnames(samples_summary))
    n_data_cols <- ncol(samples_summary)
    samples_summary <- cbind(samples_summary, sample_data[,add_columns])

  dt <- DT::datatable(
    samples_summary,
    rownames = FALSE,
    selection = 'single',
    extensions = datatable_opts$extensions,
    options(buttons = common_buttons(basename(attr(sample_data, "set_name")), "summary"), processing = FALSE,
            columnDefs=list(list(targets = seq(from=n_data_cols, to=ncol(samples_summary)-1), visible = FALSE))),
    escape = FALSE,
    class = datatable_opts$class
  )
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
                      background = DT::styleColorBar(c(0, max(
                        samples_summary[, microbial_col], na.rm = TRUE
                      )), 'lightgreen')) %>%
      DT::formatCurrency(start_color_bar_at - 1, currency = '', digits = 0) %>%
      DT::formatString(seq(from=start_color_bar_at, to=n_data_cols), suffix = '%')  ## TODO: display as percent
    #   ## not implemented for now as formatPercentage enforces a certain number of digits, but I like to round
    #   ## with signif.
  } else {
    dt <-
      dt %>%   DT::formatStyle(
        colnames(samples_summary)[seq(from=start_color_bar_at, to=microbial_col-1)],
        background = styleColorBar2(number_range, 'lightblue')
      ) %>%
      DT::formatStyle(colnames(samples_summary)[seq(from=microbial_col,to=n_data_cols)],
                      background = DT::styleColorBar(c(0, max(
                        samples_summary[, microbial_col], na.rm = TRUE
                      )), 'lightgreen'))

    if (max(samples_summary[,2]) > 1000) {
      dt <- dt %>% DT::formatCurrency(seq(from=start_color_bar_at, to=n_data_cols),
                                      currency = '', digits = 0)
    }
  }

  dt
}

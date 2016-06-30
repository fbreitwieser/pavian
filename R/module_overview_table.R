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
    checkboxInput(ns("opt_samples_overview_percent"), label = "Show percentages instead of number of reads", value = TRUE),
    div(style = 'overflow-x: scroll',
        DT::dataTableOutput(ns('dt_samples_overview')))
  )

}

#' Shiny modules to display an overview of metagenomics reports
#'
#' @param input Shiny input object.
#' @param output Shiyn output object.
#' @param session Shiny session object.
#' @param samples_df Samples \code{data.frame}.
#' @param reports List of reports.
#' @param datatable_opts Additional options for datatable.
#'
#' @return Report overview module server functionality.
#' @export
#' @import shiny
reportOverviewModule <- function(input, output, session, samples_df, reports, datatable_opts = NULL) {
  #r_state <- list()

  observeEvent(input$opt_samples_overview_percent, {
    ## save state of table
    #r_state <<- list(
    #  search_columns = input$dt_samples_overview_search_columns,
    #  state = input$dt_samples_overview_state
    #  )
    utils::str(input$dt_samples_overview_state)
  })

  ## Samples overview output
  output$dt_samples_overview <- DT::renderDataTable({

    validate(need(samples_df(), message = "No data available."))
    validate(need(reports(), message = "No data available."))

    samples_summary <- do.call(rbind, lapply(reports(), summarize_report))
    #rownames(samples_summary) <- basename(rownames(samples_summary))
    colnames(samples_summary) <-
      beautify_string(colnames(samples_summary))

    number_range <-  c(0, max(samples_summary[, 1], na.rm = TRUE))
    start_color_bar_at <- 1

    columnDefs <- list()
    rowCallback <- NULL

    if (isTRUE(input$opt_samples_overview_percent)) {
      ## add a custom renderer.
      start_color_bar_at <- 2
      number_range <- c(0, 100)
      samples_summary[, 2:ncol(samples_summary)] <-
        100 * signif(sweep(samples_summary[, 2:ncol(samples_summary)], 1, samples_summary[, 1], `/`), 2)
      columnDefs = list(list(
        targets = 2:(ncol(samples_summary)),
        render = htmlwidgets::JS(
          "function(data, type, row, meta) {",
          "value = (100 * data / row[1]).toPrecision(3)",
          "backgroundValue =",DT::styleColorBar(c(0,100), 'lightblue')[1],
          "return type === 'display' ?",
          "'<span title=\"' + data + ' reads\">' + value + '</span>' : data;",
          "}")))

      rowCallback = htmlwidgets::JS(
        "function(row, data) {",
        sprintf(" for (i = 1; i < %s; i++) { ",ncol(samples_summary)-1),
        " value = data[i]",
        " perc = (100 * data / data[1]).toPrecision(3)",
        " backgroundValue =",DT::styleColorBar(c(0,100), 'lightblue')[1],
        " $('td', row).eq(i).css('background',backgroundValue); ",
        " $('td', row).eq(i).css('background-repeat','no-repeat'); ",
        " $('td', row).eq(i).css('background-position','center'); ",
        " $('td', row).eq(i).css('background-size','98% 88%') }",
        " var span = $('<span title=\"'+value+' reads\">some text</span>');",
        " $('td', row).eq(i).html('ABC')",
        #" $('td', row).eq(i).text('<span title=\"'+value+' reads\">' + perc + '</span>')",
        "}"
      )
    }

    styleColorBar2 = function(data, color, angle=90) {
      rg = range(data, na.rm = TRUE, finite = TRUE)
      r1 = rg[1]; r2 = rg[2]; r = r2 - r1
      htmlwidgets::JS(sprintf(
        "isNaN(parseFloat(value)) || value <= %s ? '' : 'linear-gradient(%sdeg, transparent ' + (%s - value)/%s * 100 + '%%, %s ' + (%s - value)/%s * 100 + '%%)'",
        r1, angle, r2, r, color, r2, r
      ))
    }


    dt <- DT::datatable(
      samples_summary,
      selection = 'single'
      ,extensions = c('Buttons')
      , options = list(
        dom = 'Bfrtip'
        , buttons = c('pageLength','pdf', 'excel' , 'csv', 'copy')
        , lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100', 'All'))
        , pageLength = 25
        , options = c(datatable_opts, list(stateSave = TRUE))
      )
    ) %>%
      DT::formatStyle(
        colnames(samples_summary)[start_color_bar_at:5],
        background = styleColorBar2(number_range, 'lightblue')
      ) %>%
       DT::formatStyle(colnames(samples_summary)[6:ncol(samples_summary)],
                       background = DT::styleColorBar(c(0, max(
                         samples_summary[, 6], na.rm = TRUE
                     )), 'lightgreen'))

    #formatString <- function(table, columns, before="", after="") {
    #  DT:::formatColumns(table, columns, function(col, before, after)
    #    sprintf("$(this.api().cell(row, %s).node()).html((%s + data[%d]) + %s);  ",col, before, col, after),
    #    before, after
    #  )
    #}

     if (isTRUE(input$opt_samples_overview_percent)) {
       dt <- dt %>%
         DT::formatCurrency(1, currency = '', digits = 0) %>%
         DT::formatString(2:ncol(samples_summary),
                      suffix = '%')  ## TODO: display as percent
    #   ## not implemented for now as formatPercentage enforces a certain number of digits, but I like to round
    #   ## with signif.
     } else {
       dt <-
         dt %>% DT::formatCurrency(1:ncol(samples_summary),
                               currency = '',
                               digits = 0)
     }

    dt
  })

}

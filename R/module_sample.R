
#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
sampleModuleUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    fluidRow(
      box(width = 6, title = "Select sample", background = "green",
        selectInput(
          ns('sample_selector'), label = "",
          choices = NULL, multiple = FALSE,
          width = '100%'
        )),
      box(width=6,
          selectizeInput(
            ns('contaminant_selector'), label = "",
            allcontaminants, selected = commoncontaminants,
            multiple = TRUE,
            options = list(
              maxItems = 25, create = TRUE, placeholder = 'Filter clade'
            ),
            width = "100%"
          ),
          checkboxInput(ns("opt_remove_root_hits"),
                        label = "Remove reads that stay at root", value = FALSE))
    ),
    fluidRow(
      box(width=12,
          tabsetPanel(
            tabPanel("Flow diagram",
                     networkD3::sankeyNetworkOutput(ns("sample_view_sankey"), width = "100%")
            ),
            tabPanel("Sunburst",
                     sunburstR::sunburstOutput(ns("sample_view_sunburst"), width = "100%")
            ),
            tabPanel("Table",
                     div(style = 'overflow-x: scroll', DT::dataTableOutput(ns('dt_sample_view')))
            ),
            uiOutput(ns("view_in_samples_comparison"))
          )
      )
    )
  )
}

#' Title
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
sampleModule <- function(input, output, session, samples_df, reports,
                         datatable_opts = NULL) {
  sample_view_report <- reactive({

    validate(need(reports(),
                  "No reports"))

    updateSelectInput(session, 'sample_selector',
                        choices = names(reports()),
                        selected = names(reports())[1])

    my_report <- reports()[[input$sample_selector]]
    if (is.null(my_report))
      stop("No sample with that name")

    ## filter contaminants
    for (c in input$contaminant_selector)
      my_report <- filter_taxon(my_report, c)

    if (input$opt_remove_root_hits)
      my_report <- my_report[my_report$name != "-_root", ]

    my_report
  })

  #############################################################################
  ##  Sample viewer outputs

  output$sample_view_sunburst <- sunburstR::renderSunburst({
    my_report <- sample_view_report()
    if (length(my_report) == 0)
      return()

    # filter report with rows as selected in the table
    if (isTRUE(input$synchronize_table) &&
        length(input$sample_view_rows_all) > 0)
      my_report <- my_report[sort(input$sample_view_rows_all), ]

    kraken_sunburst(my_report)
  })

  output$sample_view_sankey <- networkD3::renderSankeyNetwork({
    my_report <- sample_view_report()
    if (length(my_report) == 0)
      return()

    # filter report with rows as selected in the table
    if (isTRUE(input$synchronize_table) &&
        length(input$sample_view_rows_all) > 0)
      my_report <- my_report[sort(input$sample_view_rows_all), ]

    #my_report$name <- sub("._", "", my_report$name)
    my_report <- my_report[, c("depth", "reads", "name")]
    #my_report$name <- sub("^._","",my_report$name)
    eng <- get_nodes_and_links(my_report, 10)
    nodes <- eng[[1]]
    links <- eng[[2]]
    max.reads <- max(links[, "value"])

    #print(links)

    #output$maxReads <- renderUI({
    #  helpText(sprintf("max.reads: %s",max.reads))
    #})

    #updateSliderInput(session,inputId = "min.reads",min = 1,max = min(1000,max.reads))
    links$source_name <- nodes$name[links$source + 1]


    if (!is.null(links))
      networkD3::sankeyNetwork(
        Links = links,
        Nodes = nodes,
        Source = "source",
        Target = "target",
        Value = "value",
        NodeID = "name",
        nodeWidth = 3,
        LinkGroup = "source_name",
        fontSize = 12,
        moveNodesRight = FALSE
      )
  })

  output$dt_sample_view <- DT::renderDataTable({
    my_report <- sample_view_report()

    my_report$taxonstring <-
      gsub("|", ">", my_report$taxonstring, fixed = TRUE)
    my_report$level <- as.factor(my_report$level)
    my_report$Percent <-
      100 * signif(my_report$reads / sum(my_report$reads_stay, na.rm = TRUE), 3)
    my_report$coverage <- NULL
    my_report$rankperc <- NULL

    colnames(my_report) <- beautify_string(colnames(my_report))
    DT::datatable(
      my_report,
      filter = 'top',
      selection = 'single',
      options = common_datatable_opts
    ) %>%
      formatString("Percent", suffix = "%") %>%
      formatCurrency(c("Reads", "Reads stay"),
                     digits = 0,
                     currency = "")

  }, server = TRUE)

  ## When a row (i.e. a taxonomical entry) gets selected in the sample view table, an action button appears to view the species in the overview
  output$view_in_samples_comparison <- renderUI({
    selected_row <- input$sample_view_rows_selected
    if (length(selected_row) != 1)
      return()

    my_report <- sample_view_report()
    selected_sample <-
      my_report[input$sample_view_rows_selected, "name"]
    selected_sample <- sub("^u_", "", selected_sample)
    selected_sample <- sub("^-_", "", selected_sample)
    selected_sample <- sub("^d_", "domain ", selected_sample)
    selected_sample <- sub("^k_", "kingdom ", selected_sample)
    selected_sample <- sub("^p_", "phylum ", selected_sample)
    selected_sample <- sub("^o_", "order ", selected_sample)
    selected_sample <- sub("^f_", "family ", selected_sample)
    selected_sample <- sub("^g_", "genus ", selected_sample)
    selected_sample <- sub("^s_", "species ", selected_sample)

    ## TODO: Add a custom search functionality
    #actionButton("view_selected_in_samples_comparison",paste("--> View abundances of ",selected_sample,"across samples"))
  })

  observeEvent(input$view_selected_in_samples_comparison, {
    my_report <- sample_view_report()
    selected_sample <-
      my_report[input$sample_view_rows_selected, "name"]

    updateTabsetPanel(session, "main_page", selected = "Sample comparison")
  }, ignoreNULL = TRUE)

}

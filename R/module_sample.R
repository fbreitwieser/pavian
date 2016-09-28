
tax_levels <- c("D","K","P","C","O","F","G","S")

#' UI part for sample module
#'
#' @param id Shiny namespace id
#'
#' @return UI elements for sample module
#' @export
#' @import shiny
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
            ns('contaminant_selector'), label = "Filter taxons",
            allcontaminants, selected = c("synthetic construct", "Homo sapiens"),
            multiple = TRUE,
            options = list(
              maxItems = 25, create = TRUE, placeholder = 'Filter clade'
            ),
            width = "100%"
          ),
          shinyjs::hidden(checkboxInput(ns("opt_remove_root_hits"),
                        label = "Do not show reads that stay at the root", value = FALSE))
          )
    ),
    fluidRow(
      box(width=12,
        #tabsetPanel(position="left",
        #  tabPanel("Flow diagram",
                   sliderInput(ns("sankey_maxn"), "Number of taxons to show", 10, 100, value = 50, step = 5),
                   checkboxGroupInput(ns("levels"),"",tax_levels,setdiff(tax_levels,"O"), inline = TRUE),
                   #shinyjs::hidden(sliderInput(ns("iterations"), "Number of iterations", 50, 1000, value = 250, step = 50)),
                   div(style = 'overflow-x: scroll', networkD3::sankeyNetworkOutput(ns("sample_view_sankey"), width = "100%")),
        #)
        #  tabPanel("Sunburst", sunburstR::sunburstOutput(ns("sample_view_sunburst"), width = "100%"))
        #),
        verbatimTextOutput(ns("blu")),
        div(style = 'overflow-x: scroll', DT::dataTableOutput(ns('dt_sample_view'))),
        uiOutput(ns("view_in_samples_comparison"))
      )
    )
  )
}

#' Server part for sample module
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @param sample_data Samples \code{data.frame}
#' @param reports List of reports
#' @param datatable_opts Additional options for datatable
#'
#' @return Sample module server functionality
#' @export
sampleModule <- function(input, output, session, sample_data, reports,
                         datatable_opts = NULL) {
  sample_view_report <- reactive({

    validate(need(reports(),
                  "No reports"))

    if (!input$sample_selector %in% names(reports())) {
      updateSelectInput(session, 'sample_selector',
                          choices = names(reports()),
                          selected = names(reports())[1])
    }

    my_report <- reports()[[input$sample_selector]]
    validate(need(my_report, "No sample with that name"))

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

    kraken_sunburst(my_report, colors = list(domain=all_names()))
  })

  observeEvent(input$sample_view_sankey_clicked, {
    #update(session, "blu", input$sample_view_sankey_clicked)
    req(dt_sample_view_proxy)

    message(input$sample_view_sankey_clicked)
    DT::updateSearch(dt_sample_view_proxy, list(global=input$sample_view_sankey_clicked))
  })

  tbx <- reactive({
    dat <- sample_data()
    if (!"CentrifugeOutFilePath" %in% colnames(dat))
      return()

    cf_out <- dat[dat$Name == input$sample_selector,"CentrifugeOutFilePath"]
    if (!file.exists(cf_out) || !file.exists(paste0(cf_out,".tbi")))
      return()

    return(Rsamtools::TabixFile(cf_out))
  })

  output$blu <- renderText({
    req(tbx())
    scanTabix(tbx(), GRanges(c("561"), IRanges(c(50), width=100000)))
    #input$sample_view_sankey_clicked
  })

  all_names <- reactive ({
    sub("^._","", sort(unique(unlist(sapply(reports(), function(x) x$name)))))
  })

  colourScale <- reactive({
    colourScale <- networkD3::JS(sprintf("d3.scale.category20().domain([%s])",
                              paste0('"',c(all_names(),"other"),'"',collapse=",")))
  })

  output$sample_view_sankey <- networkD3::renderSankeyNetwork({
    my_report <- sample_view_report()
    req(my_report)

    # filter report with rows as selected in the table
    if (isTRUE(input$synchronize_table) &&
        length(input$sample_view_rows_all) > 0)
      my_report <- my_report[sort(input$sample_view_rows_all), ]

    #my_report$name <- sub("._", "", my_report$name)
    #my_report <- my_report[, c("depth", "reads", "name")]
    #my_report$name <- sub("^._","",my_report$name)
    #eng <- get_nodes_and_links(my_report, 10)

    my_report <- my_report[utils::tail(order(my_report$reads,-my_report$depth), n=input$sankey_maxn), ]
    my_report <- subset(my_report, level %in% input$levels)
    my_report <- my_report[, c("name","taxonstring","reads_stay", "reads","depth", "level")]

    my_report <- my_report[!my_report$name %in% c('-_root','u_unclassified'), ]
    #my_report$name <- sub("^-_root.", "", my_report$name)

    splits <- strsplit(my_report$taxonstring, "\\|")

    ## for the root nodes, we'll have to add an 'other' link to account for all reads
    root_nodes <- sapply(splits[sapply(splits, length) ==2], function(x) x[2])

    sel <- sapply(splits, length) >= 3
    splits <- splits[sel]

    links <- data.frame(do.call(rbind,
                                lapply(splits, function(x) utils::tail(x[x %in% my_report$name], n=2))), stringsAsFactors = FALSE)
    colnames(links) <- c("source","target")
    links$value <- my_report[sel,"reads"]

    my_levels <- input$levels[input$levels %in% my_report$level]
    level_to_depth <- setNames(seq_along(my_levels)-1, my_levels)

    nodes <- data.frame(name=my_report$name,
                        depth=level_to_depth[my_report$level],
                        value=my_report$reads,
                        stringsAsFactors=FALSE)

    for (node_name in root_nodes) {
      diff_sum_vs_all <- my_report[my_report$name == node_name, "reads"] - sum(links$value[links$source == node_name])
      if (diff_sum_vs_all > 0) {
        nname <- paste("other", sub("^._","",node_name))
        #nname <- node_name
        #links <- rbind(links, data.frame(source=node_name, target=nname, value=diff_sum_vs_all, stringsAsFactors = FALSE))
        #nodes <- rbind(nodes, nname)
      }
    }

    names_id = stats::setNames(seq_len(nrow(nodes)) - 1, nodes[,1])
    links$source <- names_id[links$source]
    links$target <- names_id[links$target]
    links <- links[links$source != links$target, ]

    nodes$name <- sub("^._","", nodes$name)
    links$source_name <- nodes$name[links$source + 1]

    if (!is.null(links))
      networkD3::sankeyNetwork(
        Links = links,
        Nodes = nodes,
        Source = "source",
        Target = "target",
        Value = "value",
        NodeID = "name",
        NodeGroup = "name",
        NodeDepth = "depth",
        NodeValue = "value",
        bezierLink = FALSE,
        colourScale = colourScale(),
        nodeWidth = 14,
        units = "reads",
        LinkGroup = "source_name",
        fontSize = 12,

        iterations = ifelse(is.null(input$iterations), 50, input$iterations),
        sinksRight = FALSE,
        nodeStrokeWidth = 0,
        zoom = T
      )
  })

  dt_sample_view_proxy <- DT::dataTableProxy('sample-dt_sample_view')
  output$dt_sample_view <- DT::renderDataTable({
    my_report <- sample_view_report()

    my_report$taxonstring <- sub("^-_root.","", my_report$taxonstring)
    my_report$taxonstring <- sub("^._","", my_report$taxonstring)
    my_report$taxonstring <-
      gsub("\\|._", ">", my_report$taxonstring)
    my_report$level <- as.factor(my_report$level)
    #my_report$Percent <-
    #  100 * signif(my_report$reads / sum(my_report$reads_stay, na.rm = TRUE), 3)
    my_report$coverage <- NULL
    my_report$rankperc <- NULL
    my_report$percentage <- NULL
    my_report$name <- sub("^._", "", my_report$name)

    colnames(my_report) <- beautify_string(colnames(my_report))
    DT::datatable(
      my_report,
      filter = 'none',
      selection = 'single',
      options = datatable_opts,
      rownames = FALSE
    ) %>%
      #DT::formatString("Percent", suffix = "%") %>%
      DT::formatCurrency(c("Reads", "Reads stay"),
                     digits = 0, currency = "")

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

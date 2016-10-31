
## TODOL Make Sankey work for '-' levels
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
      column(width=6,
          selectizeInput(
            ns('contaminant_selector'), label = "Filter taxons",
            allcontaminants, selected = c("artificial sequences", "Homo sapiens"),
            multiple = TRUE, options = list(maxItems = 25, create = TRUE, placeholder = 'Filter clade'),
            width = "100%"
          ),
          shinyjs::hidden(checkboxInput(ns("opt_remove_root_hits"),
                        label = "Do not show reads that stay at the root", value = FALSE)),
          column(4,checkboxInput(ns("side_by_side"),"Show reads across samples on hover", value = TRUE)),
          column(4, column(2,(checkboxInput(ns("show_numbers"),"Show Numbers", value = TRUE)))),
          column(4,actionLink(ns("sankey_opts"), "Hide Sankey options"))
      )
    ),
    fluidRow(
      column(3,checkboxGroupInput(ns("levels"),"Taxonomical levels",tax_levels,setdiff(tax_levels,c("O","-")), inline = TRUE)),
      column(2,(sliderInput(ns("sankey_maxn"), "Number of taxons", 1, 25, value = 10, step = 1))),
      column(2,(sliderInput(ns("curvature"),"Curvature", value = .5, min = 0, max = 1, step=.01))),
      column(2,(sliderInput(ns("height"),"Height", value = 600, min = 300, max = 1200, step=50))),
      column(2,(sliderInput(ns("scalingFactor"),"Scaling", value = 1, min = .5, max = 1.5, step=.1))),
      #column(2,(checkboxInput(ns("sync_table"),"Synchronize table", value = TRUE))),
      column(2,shinyjs::hidden(radioButtons(ns("linkType"), "linkType", selected = "path2", choices = c("bezier", "l-bezier", "trapez", "path1","path2"), inline = TRUE)))
    ),
    fluidRow(
      uiOutput(ns("dynamic_sankey")),
      #uiOutput(ns("view_in_samples_comparison")),
      uiOutput(ns("blastn"))
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

  sankey_opts_state <- reactiveValues(visible = TRUE)

  observeEvent(input$sankey_opts, {
    sankey_opts_state$visible <- !sankey_opts_state$visible
    toggle_elems <- c("scalingFactor","curvature","sankey_maxn","height","levels","sync_table")

    if (sankey_opts_state$visible) {
      lapply(toggle_elems, shinyjs::show)
      updateActionButton(session, "sankey_opts", label = "Hide Sankey options")
    } else {
      lapply(toggle_elems, shinyjs::hide)
      updateActionButton(session, "sankey_opts", label = "Show Sankey options ...")
    }
  })

  observeEvent(reports(), {
    #shinyjs::show("iterations")
    updateSelectInput(session, 'sample_selector',
                      choices = names(reports()),
                      selected = names(reports())[1])
  })

  sample_view_report <- reactive({

    validate(need(reports(),
                  "No reports"))

    req(input$sample_selector)

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


  observeEvent(input$sankey_clicked, {
    #update(session, "txt_selected_name", input$sankey_clicked)
    req(input$sync_table)
    req(dt_sample_view_proxy)
    DT::updateSearch(dt_sample_view_proxy, list(global=input$sankey_clicked))
  })

  tbx <- reactive({
    dat <- sample_data()
    if (!"CentrifugeOutFilePath" %in% colnames(dat))
      return()

    cf_out <- dat[dat$Name == input$sample_selector,"CentrifugeOutFilePath"]
    if (!file.exists(cf_out) || !file.exists(paste0(cf_out,".tbi")))
      return()

    req(requireNamespace("Rsamtools"))
    return(Rsamtools::TabixFile(cf_out, yieldSize = 100))
  })

  tbx_results <- reactive({
    req(tbx)
    req(sample_view_report())
    req(input$dt_sample_view_rows_selected)
    #str(sample_view_report())

    scanTabix(tbx(),
              GRanges(sample_view_report()[input$dt_sample_view_rows_selected, "taxonid"], IRanges(c(50), width=100000)))[[1]]
  })

  tbx_results_df <- reactive({
    req(tbx_results())
    read.delim(tbx_results(), header=F,
               col.names = c("readID","seqID","taxID","score","2ndBestScore","hitLength","queryLength","numMatches","readSeq"))
  })

  output$txt_selected_name <- renderText({
    input$sankey_clicked
  })

  all_names <- reactive ({
    sub("^._","", sort(unique(unlist(sapply(reports(), function(x) x$name)))))
  })

  colourScale <- reactive({
    colourScale <- sankeyD3::JS(sprintf("d3.scaleOrdinal().range(d3.schemeCategory20b).domain([%s])",
                              paste0('"',c(all_names(),"other"),'"',collapse=",")))
  })

  output$save_sankey <- downloadHandler(filename = function() { paste0("sankey-",input$sample_selector,".html") },
                                        content = function(con) { sankey_network() %>%
                                            htmlwidgets::saveWidget(file=con) })


  sankey_network <- reactive({

    my_report <- sample_view_report()
    req(my_report)

    # filter report with rows as selected in the table
    if (isTRUE(input$synchronize_table) &&
        length(input$dt_sample_view_rows_all) > 0)
      my_report <- my_report[sort(input$dt_sample_view_rows_all), ]

    #my_report$name <- sub("._", "", my_report$name)
    #my_report <- my_report[, c("depth", "reads", "name")]
    #my_report$name <- sub("^._","",my_report$name)
    #eng <- get_nodes_and_links(my_report, 10)

    my_report <- subset(my_report, level %in% input$levels)
    #my_report <- my_report[utils::tail(order(my_report$reads,-my_report$depth), n=input$sankey_maxn), , drop = FALSE]
    my_report <- plyr::ddply(my_report, "level", function(x) x[utils::tail(order(x$reads,-x$depth), n=input$sankey_maxn), , drop = FALSE])

    #my_report <- subset(my_report, level %in% input$levels)
    my_report <- my_report[, c("name","taxonstring","reads_stay", "reads","depth", "level")]

    my_report <- my_report[!my_report$name %in% c('-_root'), ]
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
    level_to_depth <- stats::setNames(seq_along(my_levels)-1, my_levels)


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
      sankeyD3::sankeyNetwork(
        Links = links,
        Nodes = nodes,
        Source = "source",
        Target = "target",
        Value = "value",
        NodeID = "name",
        NodeGroup = "name",
        NodePosX = "depth",
        NodeValue = "value",
        colourScale = colourScale(),
        xAxisDomain = my_levels,
        xScalingFactor = input$scalingFactor,
        numberFormat = "pavian",
        title = input$sample_selector,
        nodeWidth = 15,
        nodePadding = ifelse(input$show_numbers, 13, 8),
        height = input$height,
        nodeCornerRadius = 5,
        showNodeValues = input$show_numbers,
        units = "reads",
        linkType = input$linkType,
        curvature = input$curvature,
        LinkGroup = "source_name",
        fontSize = 12,
        iterations = input$sankey_maxn * 100,
        align = "none",
        nodeStrokeWidth = 1,
        highlightChildLinks = TRUE,
        orderByPath = TRUE,
        scaleNodeBreadthsByString = TRUE,
        zoom = T
      )
  })

  sankey_hover_plots <- reactive({
    ns <- session$ns
    shiny::tagList(h3(input$sankey_hover),
                   strong(textOutput(ns("header1"))),
                   plotOutput(ns("plot1"), height=paste0(input$height/2-75,"px"), click = ns("plot_click")),
                   strong(textOutput(ns("header2"))),
                   plotOutput(ns("plot2"), height=paste0(input$height/2-75,"px"), click = ns("plot_click")),
                   conditionalPanel("typeof input.sankey_hover != 'undefined'", actionLink(ns("show_in_comparison"),"Show in comparison table"))
                   )
  })

  output$dynamic_sankey <- renderUI({
    ns <- session$ns
    if (input$side_by_side) {
      tagList(
        fluidRow(
        column(8,
               sankeyD3::sankeyNetworkOutput(session$ns("sankey"),
                                             width = "100%",
                                             height = paste0(ifelse(is.null(input$height), 500, input$height),"px")),
               downloadLink(ns("save_sankey"),"Save Network")
          ),
          column(4,
                 br(),
                 sankey_hover_plots()
          )
        ),
        fluidRow(
          div(style = 'overflow-x: scroll',DT::dataTableOutput(session$ns('dt_sample_view')))
        )
        )
    } else {
      tagList(
        sankeyD3::sankeyNetworkOutput(session$ns("sankey"), width = "100%", height = paste0(ifelse(is.null(input$height), 500, input$height),"px")),
        downloadLink(ns("save_sankey"),"Save Network"),
        div(style = 'overflow-x: scroll', DT::dataTableOutput(session$ns('dt_sample_view')))
      )
    }


  })

  sum_reads <- reactive({
    sapply(reports(), function(x) {
      sel_rows <- sub("^._","",x$name) %in% input$contaminant_selector
      ## select also child rows - as we always remove the whole clade here
      for (ts in x$taxonstring[sel_rows])
        sel_rows <- sel_rows | startsWith(x$taxonstring,ts)

      sel_rows <- sel_rows | x$name %in% c("-_root","u_unclassified")
      sum(x$reads_stay[!sel_rows], na.rm=T)
    }
  )
  })

  hover_reads <- reactive({
    res <- lapply(reports(), function(x) x$reads[sub("^._", "", x$name) == input$sankey_hover])
    sapply(res, function(x) ifelse(length(x) == 0, 0, sum(x)))
  })

  hover_reads_stay <- reactive({
    res <- lapply(reports(), function(x) x$reads_stay[sub("^._", "", x$name) == input$sankey_hover])
    sapply(res, function(x) ifelse(length(x) == 0, 0, sum(x)))
  })

  plot_it <- function(normalize) {
    req(input$sankey_hover)
    req(sum_reads())
    len <- length(sum_reads())
    stopifnot(length(hover_reads()) == len)
    stopifnot(length(hover_reads_stay()) == len)
    mydf <- data.frame(reads=c(hover_reads() - hover_reads_stay(),hover_reads_stay()),
                       type=rep(c("reads", "reads_stay"), each = len),
                       sample=rep(names(sum_reads()),2))

    mydf <- mydf[mydf$type == "reads" || mydf$reads > 0, ]

    my_names <- names(sum_reads())
    colvec <- ifelse(my_names == input$sample_selector, "red","black")

    mydf$sample <- factor(mydf$sample, names(sum_reads()) ,my_names)

    mydf$type <- factor(mydf$type, levels = c("reads_stay", "reads"),
                        labels = c("at taxon", "at children"))

    mydf <- mydf[order(mydf$sample, mydf$type),]
    if (normalize)
      mydf$reads <- 100*mydf$reads / rep(sum_reads(), each = 2)

    mydf$pos <- unlist(tapply(mydf$reads, mydf$sample, function(reads) cumsum(reads)))

    ## TODO: Replace by D3 graph?
    ##   See e.g. http://eyeseast.github.io/visible-data/2013/08/28/responsive-charts-with-d3/
    #str(names(sum_reads()))
    ggplot(mydf, aes(x=sample)) +
      geom_bar(aes(y=reads,fill=type), stat="identity", position="stack") +
      xlab("") + ylab("") +
      scale_y_continuous(limits=c(0,max(mydf$pos)*1.1), expand=c(0,0)) +
      theme_bw() +
      theme(
        panel.grid.major.y = element_line(colour = "black"),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_blank(),
        axis.text.x = element_text(colour=colvec, angle=90, vjust=1,hjust=1),
        legend.position = "none"
      )
  }


  output$header1 <- renderText({
    req(input$sankey_hover)
    #paste("Number of reads for ", input$sankey_hover, "across all samples")
    paste("Number of reads across all samples")
  })
  output$plot1 <- renderPlot({
    plot_it(FALSE) +
      geom_text(aes(label = f2si2(pos), y=pos), hjust = 0.5, vjust = -.1)
  },  bg="transparent")

  output$header2 <- renderText({
    req(input$sankey_hover)
    #paste0("Percent of reads for ", input$sankey_hover, " (excluding filtered clades)")
    paste0("Percent of reads (excluding filtered clades)")
  })
  output$plot2 <- renderPlot({
    plot_it(TRUE) +
      geom_text(aes(label = f2si2(pos), y=pos),  hjust = 0.5, vjust = -.1)
  },  bg="transparent")


  observeEvent(input$plot_click, {
    str(input$plot_click)
    if (round(input$plot_click$x) %in% seq_along(names(reports())))
      updateSelectizeInput(session, "sample_selector", selected = names(reports())[round(input$plot_click$x)])
  })

  output$sankey <- sankeyD3::renderSankeyNetwork({
    sankey_network()
  })

  dt_sample_view_proxy <- DT::dataTableProxy('dt_sample_view', session = session)
  output$dt_sample_view <- DT::renderDataTable({
    my_report <- sample_view_report()

    my_report$taxonstring <- beautify_taxonstring(my_report$taxonstring)

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
      options = c(datatable_opts,
                  pagelength = 25,
                  lengthMenu = c(10, 25, 50, 100),
                  scrollX = TRUE
                  ),
      rownames = FALSE
    ) %>%
      #DT::formatString("Percent", suffix = "%") %>%
      DT::formatCurrency(c("Reads", "Reads stay"),
                     digits = 0, currency = "")

  }, server = TRUE)

  ## When a row (i.e. a taxonomical entry) gets selected in the sample view table, an action button appears to view the species in the overview
  output$view_in_samples_comparison <- renderUI({
    req(input$dt_sample_view_rows_selected)
    selected_row <- input$dt_sample_view_rows_selected

    my_report <- sample_view_report()
    selected_sample <-
      my_report[input$dt_sample_view_rows_selected, "name"]
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

  output$blastn <- renderUI({
    req(tbx_results_df())

    #str(tbx_results_df())

    ## TODO: Add a custom search functionality
    #actionButton("view_selected_in_samples_comparison",paste("--> View abundances of ",selected_sample,"across samples"))
  })


  observeEvent(input$view_selected_in_samples_comparison, {
    my_report <- sample_view_report()
    selected_sample <-
      my_report[input$dt_sample_view_rows_selected, "name"]

    updateTabsetPanel(session, "main_page", selected = "Sample comparison")
  }, ignoreNULL = TRUE)


  reactive({
    req(input$show_in_comparison)
    return(isolate(input$sankey_hover));
  })

}

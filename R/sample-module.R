library(shiny)
library(shinydashboard)
library(shinyjs)
library(magrittr)


## TODOL Make Sankey work for '-' taxRanks
tax_taxRanks <- c("D","K","P","C","O","F","G","S")
tax_taxRank_names <- c("D"="Domain","K"="Kingdom","P"="Phylum","C"="Clade","O"="Order","F"="Family","G"="Genus","S"="Species", "-"="-", "T"="Strain")

figure_options <- function(ns) {
  shiny::tagList(checkboxGroupInput(ns("taxRanks"),"Taxonomical ranks to display",tax_taxRanks,setdiff(tax_taxRanks,c("O","C","-")), inline = TRUE),
                 sliderInput(ns("sankey_maxn"), "Number of taxa at each level", 1, 25, value = 10, step = 1),
                 sliderInput(ns("scalingFactor"),"Scale distance between nodes", value = .9, min = .5, max = 1.5, step=.05),
                 sliderInput(ns("height"),"Figure height", value = 400, min = 300, max = 1200, step=50),
                 sliderInput(ns("nodeStrokeWidth"),"Node border width", value = 0, min = 0, max = 5, step=1),
                 sliderInput(ns("linkOpacity"),"Opacity of links", value = .6, min = .1, max = 1, step=.1),
                 sliderInput(ns("textXPos"),"Node label margins", value = 1, min = 0, max = 5, step=.5),
                 checkboxInput(ns("show_numbers"),"Show numbers above nodes", value = TRUE),
                 checkboxInput(ns("color_links"),"Color the flow", value = TRUE),
                 shinyjs::hidden(sliderInput(ns("curvature"),"Curvature", value = .5, min = 0, max = 1, step=.01)),
                 shinyjs::hidden(radioButtons(ns("linkType"), "linkType", selected = "path2", choices = c("bezier", "l-bezier", "trapez", "path1","path2"), inline = TRUE)))
}


#' UI part for sample module
#'
#' @param id Shiny namespace id
#'
#' @return UI elements for sample module
#' @export
#' @import shiny
sampleModuleUI <- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("UI"))  
}

sampleModuleUI_function <- function(ns, samples, selected_sample = NULL) {
  if (!is.null(selected_sample) && is.null(selected_sample)) {
    selected_sample = selected_sample()
  } else {
    selected_sample = samples[1]
  }
  shiny::tagList(
    fluidRow(
      div(class="col-lg-5 col-md-6 col-sm-6 col-xs-12",
          div(class="col-lg-4 col-md-5 col-sm-12 lessPadding", style="font-size: 18px;", HTML("<label>Select sample<label>")),
          div(class="col-lg-8 col-md-7 col-sm-12 lessPadding", style="font-size: 18px;",
              selectInput(
                ns('sample_selector'), label = NULL,
                choices = samples, selected = selected_sample,
                multiple = getOption("pavian.devel", FALSE), selectize = TRUE,
                width = '100%'
              ))),
      div(class="col-lg-7 col-md-6 col-sm-6 col-xs-12",
          div(class="col-lg-2 col-md-4 col-sm-12 lessPadding", HTML("<label>Filter taxa</label>")),
          div(class="col-lg-10 col-md-8 col-sm-12 lessPadding",
              selectizeInput(
                ns('contaminant_selector'), label = NULL,
                allcontaminants, selected = c("artificial sequences", "Chordata", "unclassified"),
                multiple = TRUE, options = list(maxItems = 25, create = TRUE, placeholder = 'Filter clade'),
                width = "100%"
              )
          )
      )
    ),
    fluidRow(
      
      div(class = "col-lg-8 col-md-12", style = "padding: 0px",
          tabBox(width=12,
                 tabPanel("Sankey visualization",
                          div(style="display:inline-block", "Hover over a node to see the abundance of the taxon in other samples."),
                          div(style="display:inline-block", shinyWidgets::dropdownButton(figure_options(ns), circle = FALSE, label = "Configure Sankey ...")),
                          div(uiOutput(ns("dynamic_sankey")), style="border: 1px solid lightgray"),
                          br(),
                          downloadLink(ns("save_sankey"),"Save Network")
                  ),
                 #tabPanel("Partition visualization",
                #          selectInput(ns("d3part_charttype"),
                #                      label = "Chart type", choices = c('sunburst','treemap','circle_treemap','partition_chart','icicle'), selected="sunburst"),
                #         D3partitionR::D3partitionROutput(ns("D3partionR"))
                #         ),
                 tabPanel("Table",
                          "Click a row to see the abundance of the taxon in other samples.",
                          div(style = 'overflow-x: scroll', DT::dataTableOutput(ns('dt_sample_view'))),
                          br(),
                          downloadButton(ns('downloadData'), 'Download full table in tab-separated value format')),
                 tabPanel("Text",
                          shinyjs::hidden(sliderInput(ns("quantile"),"Amount of data to show", value = 100, min = 1, max = 100, step = 1)),
                          numericInput(ns("min_reads"),"Minimum number of reads", value = 1, step = 1),
                          htmlOutput(ns('text')))
          )),
      div(class = "col-lg-4 col-md-12", style = "padding: 0px",
          tabBox(width=12, id = ns("across_sample_tabbox"),
                 tabPanel("Across samples", status = "success",
                          uiOutput(ns("sankey_hover_plots")))
          )
      ),
      box(width=12,uiOutput(ns("selected_lineage")))
    )
  )
}

#' Server part for sample module
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param sample_data Samples \code{data.frame}.
#' @param reports List of reports.
#' @param tax_data Taxonomy data.
#' @param clade_reads Clade reads.
#' @param taxon_reads Taxon reads.
#' @param selected_sample Pre-selected sample.
#' @param datatable_opts Additional datatable opts (mostly $class).
#'
#' @return Sample module server functionality
#' @export
sampleModule <- function(input, output, session, sample_data, reports,
                         tax_data, clade_reads, taxon_reads,
                         selected_sample = NULL, datatable_opts = NULL) {
  
  output$UI <- renderUI({
    req(reports())
    sampleModuleUI_function(session$ns, names(reports()), selected_sample)
  })
  
  sankey_opts_state <- reactiveValues(visible = TRUE)
  
  hover_plots <- reactiveValues(taxon = NULL, report = 1)
  
  observeEvent(input$sankey_opts, {
    sankey_opts_state$visible <- !sankey_opts_state$visible
    toggle_elems <- c("ext_sankey_opts")
    
    if (sankey_opts_state$visible) {
      lapply(toggle_elems, shinyjs::show)
      updateActionButton(session, "sankey_opts", label = "Hide options")
    } else {
      lapply(toggle_elems, shinyjs::hide)
      updateActionButton(session, "sankey_opts", label = "Show options")
    }
  })
  
  #observe({
    #shinyjs::show("iterations")
  #  ss <- 1 
  #  if (!is.null(selected_sample) && !is.null(selected_sample())){
  #    ss <- selected_sample() 
  #  }
  #  updateSelectInput(session, 'sample_selector',
  #                    choices = names(reports()),
  #                    selected = names(reports())[ss])
  #})
  
  
  #observeEvent(reports(), {
  #  updateSelectInput(session, 'sample_selector',
  #                    choices = names(reports()),
  #                    selected = names(reports())[1])
  #})
  
  #observeEvent(selected_sample, {
  #  validate(need(selected_sample <= length(reports()), message = "Selected sample number too high."))
  #  updateSelectInput(session, 'sample_selector',
  #                    choices = names(reports()),
  #                    selected = names(reports())[selected_sample])
  #})
  
  sample_view_report <- reactive({
    validate(need(input$sample_selector,message="Select sample sets with reports"))
    
    my_report <- reports()[[input$sample_selector[1]]]
    validate(need(my_report, "No sample with that name"))
    
    ## filter contaminants
    for (c in input$contaminant_selector)
      my_report <- filter_taxon(my_report, c)
    
    my_report
  })
  
  sample_view_reports <- reactive ({
    validate(need(input$sample_selector,message="Select sample sets with reports"))
    lapply(input$sample_selector, function(i) {
      my_report <- reports()[[i]]
      validate(need(my_report, "No sample with that name"))
    
      ## filter contaminants
      for (c in input$contaminant_selector)
        my_report <- filter_taxon(my_report, c)
    
      my_report
    })
  })
  
  #############################################################################
  ##  Sample viewer outputs
  
  output$selected_lineage <- renderUI({
    req(hover_plots$taxon)
    my_report <- sample_view_reports()[[hover_plots$report]]
    my_report$name <- sub("^._","",my_report$name)
    sel_row <- my_report[which(my_report$name == hover_plots$taxon)[1], , drop=FALSE]
    shiny::tagList(
      HTML("Selected taxon lineage: "),
      HTML(beautify_taxLineage(sel_row$taxLineage, remove_last=FALSE, break_it_up=TRUE)),
      p(HTML(paste0("Rank ", strong(tax_taxRank_names[sel_row$taxRank]),
                    {if ("taxID" %in% colnames(sel_row)) {
                      HTML(paste0(", TaxID ", sel_row$taxID, " [",
                                  a(href=sprintf("https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=%s", sel_row$taxID, target="_blank"), "NCBI Taxonomy"), ", ",
                                  a(href=sprintf("https://www.ncbi.nlm.nih.gov/assembly/?term=txid%s[Organism:exp]", sel_row$taxID, target="_blank"), "Assemblies"),", "))
                    } else { HTML("[")}},"",
                    a(href=sprintf("https://www.ncbi.nlm.nih.gov/pubmed/?term=%s", sel_row$name, target="_blank"), "PubMed"),", ",
                    a(href=sprintf("https://scholar.google.at/scholar?q=%s", sel_row$name, target="_blank"), "Google Scholar"),"]")))
    )
  })
  
  output$dynamic_sankey <- renderUI({
    ns <- session$ns
    #sankeyD3::sankeyNetworkOutput(ns("sankey"), width = "100%",
    #                                height = paste0(ifelse(is.null(input$height), 500, input$height),"px"))
    
    do.call(shiny::tagList,
            lapply(seq_along(input$sample_selector), function(i) {
      sankeyD3::sankeyNetworkOutput(ns(paste0("sankey",i)), width = "100%",
                                    height = paste0(ifelse(is.null(input$height), 500, input$height),"px"))
      }))
  })
  
  output$sankey_hover_plots <- renderUI({
    validate(need(length(reports()) > 1, message="Need more than one sample in sample set.")) 
    validate(need(hover_plots$taxon, message="Hover or select a taxon to display reads across samples."))
    ns <- session$ns
    my_report <- sample_view_report()
    my_report$name <- sub("^._","",my_report$name)
    sel_row <- my_report[which(my_report$name == hover_plots$taxon)[1], , drop=FALSE]
    req(sel_row)
    shinyjs::runjs(sprintf("$('ul#sample-across_sample_tabbox>li>a').text('%s across samples')", hover_plots$taxon))
    
    shiny::tagList(
      "Number of reads across all samples ","(",downloadLink(ns("save_plot1"),"PDF"),")",
      plotOutput(ns("plot1"), height=paste0(max(200,input$height/1.5-75),"px"), click = ns("plot_click")),
      #"Percent of reads (after filtering) (",downloadLink(ns("save_plot2"),"PDF"),")",
      #plotOutput(ns("plot2"), height=paste0(max(200,input$height/2-75),"px"), click = ns("plot_click")),
      "Legend: The turqoise bar shows the number of reads that are identified at the specific taxon; the orange bar shows the number of reads identified at children of the specified taxon.",
      conditionalPanel("typeof input.sankey_hover != 'undefined'", actionLink(ns("show_in_comparison"),"Show in comparison table"))
    )
  })
  
  plot_it <- function(normalize = FALSE) {
    requireNamespace("ggplot2")
    #requireNamespace("cowplot")
    req(hover_plots$taxon)
    selected_taxon <- hover_plots$taxon
    taxIndex <- which(tax_data()$name == selected_taxon)[1]
    clade_reads_m <- na0(clade_reads()[taxIndex, ]) - na0(taxon_reads()[taxIndex,])
    short_name <- substr(sample_data()$Name, 1, 10)
    for (si in unique(short_name)) {
      sel <- si %in% short_name
      if (sum(sel) > 1) {
        short_name[sel] <- sprintf("%s-%s", short_name[sel], seq_len(sum(sel)))
      }
    }
    if (any(duplicated(short_name))) {
      short_name[duplicated(short_name)]
    }
    mydf <- data.frame(sample=rep(factor(short_name, levels=short_name),2), 
                       type=factor(rep(c("in total", "at taxon"), each=ncol(clade_reads())), levels = c("in total", "at taxon")), 
                       reads=c(clade_reads_m, taxon_reads()[taxIndex,]),
                       pos=c(clade_reads()[taxIndex,], taxon_reads()[taxIndex,]))
    colvec <- ifelse(colnames(clade_reads()) %in% input$sample_selector, "red","black")
    #if (normalize) {
    #  mydf$reads <- 100*mydf$reads / rep(sum_clade_reads(), each = 2)
    #}
    
    ## TODO: Replace by D3 graph?
    ##   See e.g. http://eyeseast.github.io/visible-data/2013/08/28/responsive-charts-with-d3/
    ggplot(mydf, aes(x=sample)) +
      geom_bar(aes(y=reads, fill=type), stat="identity", position="stack") +
      xlab("") + ylab("") +
      scale_y_continuous(limits=c(0,max(mydf$pos, na.rm=T)*1.1), expand=c(0,0)) +
      scale_fill_manual("", values = c("in total"="#fc8d62", "at taxon"="#66c2a5")) +
      my_gg_theme(12) +
      theme(
        axis.ticks = element_blank(),
        axis.text.x = element_text(colour=colvec, angle=90, vjust=1,hjust=1, size = 10),
        legend.position = "none"
      )
  }
  
  output$save_plot1 <- downloadHandler(
    filename = function() {
      paste("plot1-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      ggsave(file, plot_it(FALSE) +
               geom_text(aes(label = f2si2(pos), y=pos), hjust = 0.5, vjust = -.1))
    }
  )
  
  output$plot1 <- renderPlot({
    plot_it(FALSE) +
      geom_text(aes(label = f2si2(pos), y=pos), hjust = 0.5, vjust = -.1)
  },  bg="transparent")
  
  output$save_plot2 <- downloadHandler(
    filename = function() {
      paste("plot2-", Sys.Date(), ".pdf", sep="")
    },
    content = function(file) {
      ggsave(file, plot_it(TRUE) +
               geom_text(aes(label = f2si2(pos), y=pos),  hjust = 0.5, vjust = -.1))
    }
  )
  
  output$plot2 <- renderPlot({
    plot_it(TRUE) +
      geom_text(aes(label = f2si2(pos), y=pos),  hjust = 0.5, vjust = -.1)
  },  bg="transparent")
  
  
  
  observeEvent(input$plot_click, {
    if (round(input$plot_click$x) %in% seq_along(names(reports())))
      updateSelectizeInput(session, "sample_selector", selected = names(reports())[round(input$plot_click$x)])
  })
  
  output$sankey <- sankeyD3::renderSankeyNetwork({ sankey_network() })
  output$sankey1 <- sankeyD3::renderSankeyNetwork({ sankey_networks()[[1]] })
  output$sankey2 <- sankeyD3::renderSankeyNetwork({ sankey_networks()[[2]] })
  output$sankey3 <- sankeyD3::renderSankeyNetwork({ sankey_networks()[[3]] })
  output$sankey4 <- sankeyD3::renderSankeyNetwork({ sankey_networks()[[4]] })
  output$sankey5 <- sankeyD3::renderSankeyNetwork({ sankey_networks()[[5]] })
  output$sankey6 <- sankeyD3::renderSankeyNetwork({ sankey_networks()[[6]] })
  output$sankey7 <- sankeyD3::renderSankeyNetwork({ sankey_networks()[[7]] })
  output$sankey8 <- sankeyD3::renderSankeyNetwork({ sankey_networks()[[8]] })
  output$sankey9 <- sankeyD3::renderSankeyNetwork({ sankey_networks()[[9]] })
  output$sankey10 <- sankeyD3::renderSankeyNetwork({ sankey_networks()[[10]] })
  
  observeEvent(input$sankey_hover, { hover_plots$taxon <- input$sankey_hover })
  observeEvent(input$sankey1_hover, { hover_plots$taxon <- input$sankey1_hover; hover_plots$report <- 1; })
  observeEvent(input$sankey2_hover, { hover_plots$taxon <- input$sankey2_hover; hover_plots$report <- 2; })
  observeEvent(input$sankey3_hover, { hover_plots$taxon <- input$sankey3_hover; hover_plots$report <- 3; })
  observeEvent(input$sankey4_hover, { hover_plots$taxon <- input$sankey4_hover; hover_plots$report <- 4; })
  observeEvent(input$sankey5_hover, { hover_plots$taxon <- input$sankey5_hover; hover_plots$report <- 5; })
  observeEvent(input$sankey6_hover, { hover_plots$taxon <- input$sankey6_hover; hover_plots$report <- 6; })
  observeEvent(input$sankey7_hover, { hover_plots$taxon <- input$sankey7_hover; hover_plots$report <- 7; })
  observeEvent(input$sankey8_hover, { hover_plots$taxon <- input$sankey8_hover; hover_plots$report <- 8; })
  observeEvent(input$sankey9_hover, { hover_plots$taxon <- input$sankey9_hover; hover_plots$report <- 9; })
  observeEvent(input$sankey10_hover, { hover_plots$taxon <- input$sankey10_hover; hover_plots$report <- 10; })
  
  observeEvent(input$dt_sample_view_rows_selected, {
    hover_plots$taxon <- sub("^._", "", sample_view_report()[input$dt_sample_view_rows_selected,"name"])
  })
  
  observeEvent(input$sankey_hover, {
    req(dt_sample_view_proxy)
    DT::updateSearch(dt_sample_view_proxy, list(global=input$sankey_hover))
    DT::updateSearch(dt_sample_view_proxy1, list(global=input$sankey_hover))
  })
  
  tbx <- reactive({
    dat <- sample_data()
    if (!"CentrifugeOutFilePath" %in% colnames(dat))
      return()
    
    cf_out <- dat[dat$Name == input$sample_selector[1],"CentrifugeOutFilePath"]
    if (!file.exists(cf_out) || !file.exists(paste0(cf_out,".tbi")))
      return()
    
    req(requireNamespace("Rsamtools"))
    return(Rsamtools::TabixFile(cf_out, yieldSize = 100))
  })
  
  tbx_results <- reactive({
    req(tbx)
    req(sample_view_report())
    req(input$dt_sample_view_rows_selected)
    
    #scanTabix(tbx(),
    #          GRanges(sample_view_report()[input$dt_sample_view_rows_selected, "taxID"], IRanges(c(50), width=100000)))[[1]]
  })
  
  tbx_results_df <- reactive({
    req(tbx_results())
    #read.delim(tbx_results(), header=F,
    #           col.names = c("readID","seqID","taxID","score","2ndBestScore","hitLength","queryLength","numMatches","readSeq"))
  })
  
  output$txt_selected_name <- renderText({
    input$sankey_clicked
  })
  
  all_names <- reactive ({
    sub("^._","", sort(unique(unlist(sapply(reports(), function(x) x$name[x$taxRank != "-"])))))
  })
  
  colourScale <- reactive({
    colourScale <- sankeyD3::JS(sprintf("d3.scaleOrdinal().range(d3.schemeCategory20b).domain([%s])",
                                        paste0('"',c(all_names(),"other"),'"',collapse=",")))
  })
  
  output$save_sankey <- downloadHandler(filename = function() { paste0("sankey-",input$sample_selector[1],".html") },
                                        content = function(con) {
                                          a <- sankey_networks()[[1]]
                                          a$sizingPolicy$defaultHeight <- 1080
                                          a$sizingPolicy$defaultWidth <- 1920
                                          htmlwidgets::saveWidget(a, file=con)
                                        })
  
  
  sankey_network <- reactive( {
    
    my_report <- sample_view_report()
    req(my_report)
    
    # filter report with rows as selected in the table
    if (isTRUE(input$synchronize_table) &&
        length(input$dt_sample_view_rows_all) > 0)
      my_report <- my_report[sort(input$dt_sample_view_rows_all), ]
    
    #my_report$name <- sub("._", "", my_report$name)
    #my_report <- my_report[, c("depth", "cladeReads", "name")]
    #my_report$name <- sub("^._","",my_report$name)
    #eng <- get_nodes_and_links(my_report, 10)
    validate(need(any(input$taxRanks %in% my_report$taxRank), message = "Report does not have required taxonomy ranks"))
    build_sankey_network(my_report, taxRanks=input$taxRanks, maxn=input$sankey_maxn,
                         # Sankey options
                         xScalingFactor = input$scalingFactor,
                         nodePadding = ifelse(input$show_numbers, 13, 8),
                         nodeStrokeWidth = input$nodeStrokeWidth,
                         showNodeValues = input$show_numbers,
                         linkOpacity = input$linkOpacity,
                         linkType = input$linkType,
                         nodeLabelMargin = input$textXPos,
                         height = input$height,
                         curvature = input$curvature,
                         colourScale = colourScale(),
                         LinkGroup = ifelse(input$color_links, "source_name", NA)
    ) %>% shinyTryCatch(message="building Sankey network")
  })
  
  sankey_networks <- reactive({
    lapply(seq_along(input$sample_selector), function(i) {
    my_report <- sample_view_reports()[[i]]
    req(my_report)
    
    # filter report with rows as selected in the table
    if (isTRUE(input$synchronize_table) &&
        length(input$dt_sample_view_rows_all) > 0)
      my_report <- my_report[sort(input$dt_sample_view_rows_all), ]
    
    #my_report$name <- sub("._", "", my_report$name)
    #my_report <- my_report[, c("depth", "cladeReads", "name")]
    #my_report$name <- sub("^._","",my_report$name)
    #eng <- get_nodes_and_links(my_report, 10)
    validate(need(any(input$taxRanks %in% my_report$taxRank), message = "Report does not have required taxonomy ranks"))
    build_sankey_network(my_report, taxRanks=input$taxRanks, maxn=input$sankey_maxn,
                         #title = input$sample_selector[i],
                         # Sankey options
                         xScalingFactor = input$scalingFactor,
                         nodePadding = ifelse(input$show_numbers, 13, 8),
                         nodeStrokeWidth = input$nodeStrokeWidth,
                         showNodeValues = input$show_numbers,
                         linkOpacity = input$linkOpacity,
                         linkType = input$linkType,
                         nodeLabelMargin = input$textXPos,
                         height = input$height,
                         curvature = input$curvature,
                         colourScale = colourScale(),
                         LinkGroup = ifelse(input$color_links, "source_name", NA)
    ) %>% shinyTryCatch(message="building Sankey network")
    })
  })
  
  output$text <- renderUI({
    my_report <- sample_view_report()
    name_format <- function(node_name) {
      paste(sprintf("<a href='#' onclick=\"Shiny.onInputChange('%s','%s');\" class='name-link'><nobr>%s</nobr></a>",
                    session$ns("sankey_hover"),node_name, node_name), collapse = "<wbr>>")
    }
    
    #    HTML(
    quant1 <- stats::quantile(my_report$cladeReads[my_report$taxonReads > 0], probs=1-input$quantile/100)
    div(style="line-height: 1;",
        text_representation(my_report,
                            name_format = name_format,
                            reads_format = function(x, color) sprintf("<span style='background-color:%s;'>%s</span>", color, x),
                            min_reads = input$min_reads, collapse = "<br/>\n")
    )
    #    )
    
    
  })
  
  dt_sample_view_proxy <- DT::dataTableProxy('dt_sample_view')
  dt_sample_view_proxy1 <- DT::dataTableProxy(session$ns('dt_sample_view'))
  output$dt_sample_view <- DT::renderDataTable({
    my_report <- sample_view_report()
    
    my_report$taxLineage <- beautify_taxLineage(my_report$taxLineage, TRUE)
    
    
    my_report$taxRank <- as.factor(my_report$taxRank)
    #my_report$Percent <-
    #  100 * signif(my_report$cladeReads / sum(my_report$taxonReads, na.rm = TRUE), 3)
    my_report$coverage <- NULL
    my_report$taxRankperc <- NULL
    my_report$percentage <- signif(100*my_report$cladeReads/max(my_report$cladeReads),4)
    my_report$depth <- NULL
    my_report$name <- my_report$name %>% sub("^._", "", .) %>% gsub(" ", "&nbsp;", .)
    #my_report <- my_report[my_report$taxonReads > 0, ]
    
    colnames(my_report) <- beautify_string(colnames(my_report))
    
    if (!max(my_report$CladeReads) > 1000) {
      my_report[,c("CladeReads", "TaxonReads")] <- signif(my_report[,c("CladeReads", "TaxonReads")], digits = 4)
    }
    
    dt <- DT::datatable(my_report,
                        filter = 'bottom', selection = 'single',
                        class = datatable_opts$class,
                        extensions = datatable_opts$extensions,
                        escape = FALSE, rownames = FALSE,
                        options = list(buttons = common_buttons(input$sample_selector[1], "results"),
                                       columnDefs=list(list(targets = seq(from=, to=2), visible=TRUE, orderSequence = c('desc','asc'))))
    )
    if (max(my_report$CladeReads) > 1000) {
      dt <- dt %>% DT::formatCurrency(c("CladeReads", "TaxonReads"), digits = 0, currency = "")
    }
    
    dt %>% 
      DT::formatStyle(c("CladeReads","TaxonReads"),
                      background = styleColorBar2(c(0,my_report$CladeReads,na.rm=T), 'lightblue')) %>%
      DT::formatString(c("Percentage"), suffix = '%') %>%
      DT::formatStyle(c("Percentage"),
                      background = styleColorBar2(c(0,100,na.rm=T), 'lightblue'))
    
    
  }, server = TRUE)
  
  output$downloadData <- downloadHandler(
    filename = function() { sprintf("%s-report-%s.tsv", input$sample_selector[1], format(Sys.time(), "%y%m%d")) },
    content = function(file) {
      utils::write.table(beautify_colnames(sample_view_report()), file, row.names = FALSE, sep = "\t")
    }
  )
  
  
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
  
  
  
  ## D3partitionR does not display - might be added back at a later point in time
#  output$D3partionR <- D3partitionR::renderD3partitionR({
#    library(D3partitionR)
#    my_report <- sample_view_report()
#    my_report <- my_report[my_report$cladeReads > 0 & my_report$taxonReads > 10 & grepl("^._root",my_report$taxLineage), ]
#    #taxLineage <- sub("^._root.","",my_report$taxLineage) %>% gsub("._","", .)
#    strsplit(my_report$taxLineage, "|", fixed=T) %>% lapply(grep, pattern= "^-_", invert = T, value = T)
#    
#    domains <- c("d", "p", "c", "f", "g", "s")
#    pattern <- sprintf("^[%s]_", paste(domains, collapse=""))
#    res <- strsplit(my_report$taxLineage, "|", fixed=T) %>% 
#      lapply(function(x) {
#        y <- grep(pattern, x, value = T)
#        d <- sub("_.*", "", y)
#        n <- sub("^._", "", y)
#        names(n) <- d
#        #xx <- n[domains]
#        #xx[is.na(xx)] <- "NA"
#        #xx
#        n[domains]
#      }) %>% do.call(rbind, .)
#    colnames(res) <- domains
#    res <- data.frame(res, stringsAsFactors = FALSE)
#    sel <- !apply(is.na(res), 1, any) & my_report$taxonReads > 0
#    #sel <- !apply(is.na(res), 1, all)
#    #sel <- my_report$cladeReads
#    res$N <- my_report$taxonRead
#    res <- res[sel, ]
#    library(plyr)
#    res <- ddply(res, domains, function(x) {
#      a = x[1, , drop=F]
#      a$N = sum(x$N)
#      a
#    } )
#    
#    D3partitionR::D3partitionR() %>% 
#      D3partitionR::add_data(res, count = 'N', steps = domains, color='N') %>%
#      #D3partitionR::add_title(list(text=input$sample_selector)) %>%
#      set_chart_type(input$d3part_charttype) %>%
#      plot
#  })
  
  #reactive({
  #  req(input$show_in_comparison)
  #  return(isolate(hover_plots$taxon));
  #})
}

taxon_levels <- c(
  "Any" = "-",
  "Species" = "S",
  "Genus" = "G",
  "Family" = "F",
  "Order" = "O",
  "Class" = "C",
  "Phylum" = "P",
  "Domain" = "D"
)

#' UI part of the comparison module
#'
#' @param id Shiny namespace id.
#'
#' @return UI elements of the comparison module.
#' @export
#' @import shiny
comparisonModuleUI <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    fluidRow(box(width=12,collapsible=TRUE,collapsed=TRUE,title="Select samples",
                 selectizeInput(ns("select_samples"),label="", multiple=TRUE, choices=NULL,selected=NULL, options=list(create = TRUE)))),
    fluidRow(
      box(width=6, background = "green",
          column(6,
                 selectizeInput(ns("opt_statistic"),
                                label = " Summarization statistic",
                                choices = c("Mean", "Median", "Max", "Sd",
                                            "Maximum absolute deviation", "Max Z-score"),
                                selected = "Mean"),
                 checkboxInput(ns("opt_display_percentage"),
                               label = "Normalize by sample reads",
                               value = FALSE),
                 checkboxInput(ns("opt_log_data"),
                               label = "Apply variance-stabilizing transformation",
                               value = FALSE),
                 checkboxInput(ns("opt_zscore"),
                               label = " Robust z-score",
                               value = FALSE)
                 ),
          column(6,
                 selectizeInput(

                   ns("opt_classification_level"),
                   label = "Taxon level",
                   choices = taxon_levels,
                   selected = "-"
                 ),
                 radioButtons(
                   ns("opt_show_reads_stay"),
                   label = "",
                   choices = c(
                     "Reads by taxon" = "reads_stay",
                     "Reads by taxon and children" = "reads",
                     "both"
                   ))#,
                 #checkboxInput(ns("opt_remove_root_hits"),
                #               label = "Do not show reads that stay at the root",
                #               value = TRUE)

          )
      ),
      box(
        width = 6,
        title = "Filter contaminant reads",
        selectizeInput(
          ns('contaminant_selector'),
          allcontaminants,
          label = "from taxon",
          selected = c("synthetic construct", "unclassified", "Homo sapiens"),
          multiple = TRUE,
          options = list(
            maxItems = 25,
            create = TRUE,
            placeholder = 'Filter clade'
          ),
          width = "100%"
        ),
        selectizeInput(
          ns('contaminant_selector_clade'),
          label = "from taxon and its children",
          allcontaminants,
          selected = c(""),
          multiple = TRUE,
          options = list(
            maxItems = 25,
            create = TRUE,
            placeholder = 'Filter clade'
          ),
          width = "100%"
        )
      )
    ),
    tabBox(
      width = 12,
      tabPanel(
        "Table",
        div(style = 'overflow-x: scroll',
            DT::dataTableOutput(ns('dt_samples_comparison'))),
        actionButton(ns("btn_sc_filter"), "Filter"),
        actionButton(ns("btn_sc_gointo"), "Go Into")#,
        #shiny::htmlOutput(ns("txt_samples_comparison")),
        #DT::dataTableOutput(ns("row_details_table"))
      ),
      tabPanel("Heatmap",
               fluidRow(
                 column(width = 8, uiOutput(ns("d3heatmap_samples_comparison"))),
                 column(
                   width = 4,
                   radioButtons(
                     ns("heatmap_scale"),
                     'Scale',
                     c("none", "row", "column"),
                     selected = "column",
                     inline = TRUE
                   ),
                   checkboxGroupInput(
                     ns("heatmap_cluster"),
                     "Cluster",
                     choices = c('row', 'column'),
                     selected = c('row', 'column'),
                     inline = TRUE
                   )
                 )
               )),
      tabPanel("Samples Clustering",
               fluidRow(shiny::plotOutput(ns("cluster_plot")))) ## end tabPanel Clustering
    )
  )
}


stat_name_to_f <- list(
  "Mean"=mean,
  "Median"=median,
  "Max"=max,
  "Standard deviation"=sd,
  "Sd"=sd,
  "Maximum absolute deviation"=function(x) { max(x - median(x)) },
  "Max Z-score"=function(x) { (max(x) - median(x))/max(1,mad(x)) }
)



#' Server part of comparison module
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session.
#' @param samples_df A \code{data.frame} specifying sample names and file paths (read from a defs.csv file).
#' @param reports A list with report \code{data.frame}s.
#' @param datatable_opts Additional options for creating the datatable.
#' @param filter_func If not NULL, \code{filter_func} is applied to every data.frame in \code{reports}.
#'
#' @return Comparison module server functionality
#' @export
comparisonModule <- function(input, output, session, samples_df, reports,
                             datatable_opts = NULL, filter_func = NULL) {
  filtered_reports <- reactive({
    my_reports <- reports()
    withProgress(message="Loading sample reports ...",{
      stats::setNames(lapply(names(my_reports), function(my_report_n) {
        setProgress(detail=my_report_n)
        r1 <- filter_taxon(my_reports[[my_report_n]], input$contaminant_selector, rm_clade = FALSE)
        filter_taxon(r1, input$contaminant_selector_clade, rm_clade = TRUE)
      }),names(my_reports))}
    )
  })

  observe({
    updateSelectizeInput(session, "select_samples",
                         choices=samples_df()[,"Name"], selected=samples_df()[,"Name"])
  })

  selected_reports <- reactive({
    selected <- unlist(lapply(input$select_samples,function(s) grep(paste0("^",s,"$"),samples_df()[,"Name"])))
    #updateSelectizeInput(session, "select_samples",
    #                     selected=samples_df()[selected,"Name"])
    filtered_reports()[unique(sort(selected))]
  })

  get_summarized_report_reads_stay <- reactive({
    get_summarized_report2(reports_filtered(), "reads_stay")
  })

  get_summarized_report_reads_clade <- reactive({
    get_summarized_report2(reports_filtered(), "reads")
  })

  reports_filtered <- reactive({
    ## filter reports, if a filter function is given to the module
    if (!is.null(filter_func)) {
      lapply(selected_reports(), filter_func)
    } else {
      selected_reports()
    }
  })

  get_summarized_report1 <- reactive({
    if (input$opt_show_reads_stay == "reads") {
      get_summarized_report_reads_clade()
    } else {
      get_summarized_report_reads_stay()
    }
  })

  get_summarized_reportc <- reactive({
    summarized_report <- withProgress(message="Combining sample reports ...", { get_summarized_report1() })

    str(summarized_report)
    if (req(input$opt_classification_level) != "-") {
      summarized_report <- summarized_report[summarized_report[["Level"]] %in% input$opt_classification_level,]
    }
    summarized_report
  })

  get_summarized_reportp <- reactive({
    withProgress(message="Normalizing samples ...", { get_summarized_reportc() %>% normalize_data_cols() })
  })


  r_summarized_report <- reactive({

    if (isTRUE(input$opt_display_percentage)) {
      summarized_report <- get_summarized_reportp()
    } else {
      summarized_report <- get_summarized_reportc()
    }

    if (isTRUE(input$opt_log_data)) {
      summarized_report <- withProgress(message="Logging data ...", { summarized_report %>% log_data_cols() })
    }

    if (isTRUE(input$opt_zscore)) {
      summarized_report <- withProgress(message="Calculating z-score ...", {
        summarized_report %>% calc_robust_zscore(min_scale=ifelse(isTRUE(input$opt_display_percentage), 0.001, 1))
        })
    }

    validate(need(attr(summarized_report, 'data_columns'), message = "data_columns NULL"))
    data_cols <- attr(summarized_report, "data_columns")
    round_digits <- ifelse(isTRUE(input$opt_display_percentage), 3, 1)
    summarized_report$STAT <- signif(apply(zero_if_na(summarized_report[,data_cols]), 1, stat_name_to_f[[input$opt_statistic]]), 3)

    colnames(summarized_report)[colnames(summarized_report) == "STAT"] <- input$opt_statistic
    summarized_report$OVERVIEW = apply(round(zero_if_na(summarized_report[,data_cols]), round_digits), 1, paste0, collapse = ",")
    colnames(summarized_report)[colnames(summarized_report) == "OVERVIEW"] <- "Overview"

    if (isTRUE(input$opt_display_percentage) || isTRUE(input$opt_zscore)) {
      summarized_report[,data_cols] <- signif(summarized_report[,data_cols], 3)
    }

    summarized_report

  })

  observeEvent(samples_df, {
    updateSelectizeInput(session, "select_samples",
                         choices=samples_df()[,"Name"], selected=samples_df()[,"Name"]
    )
  })

  ## TODO: Consider working around heatmap issue w outputOptions
  ##  works globally, though, and not in modules
  ##  outputOptions(output, "dt_samples_comparison", suspendWhenHidden = FALSE)

  output$dt_samples_comparison <- DT::renderDataTable({

    summarized_report <- r_summarized_report()
    validate(need(summarized_report, message = "No data"),
             need(attr(summarized_report, 'data_columns'), message = "data_columns NULL"),
             need(attr(summarized_report, 'taxonid_column'), message = "taxonid_data_columns NULL"),
             need(attr(summarized_report, 'stat_column'), message = "stat_columns NULL"))

    idx_data_columns <- attr(summarized_report, 'data_columns')

    show_rownames <- FALSE
    zero_col <- ifelse(show_rownames, 0, 1)

    columnDefs <- list(
      ## Make taxonid column link-able
      list(
        targets = attr(summarized_report, 'taxonid_column') - zero_col,
        render = htmlwidgets::JS(
          "function(data, type, full){
          return '<a href=\"http://www.ncbi.nlm.nih.gov/genome/?term=txid'+data+'[Organism:exp]\" target=\"_blank\">' + data + '</a>'
  }"
        )
      ),
      list(targets = attr(summarized_report, 'stat_column') - zero_col, orderSequence = c('desc', 'asc'), width = "80px" ),       ## Stat column
      list(targets = attr(summarized_report, 'data_columns') - zero_col, orderSequence = c('desc', 'asc'), searchable = FALSE ),  ## Data columns shouldn't be searchable
      list(
        targets = which(colnames(summarized_report) == "Overview") - zero_col,
        searchable = FALSE,
        render = htmlwidgets::JS(
          "function(data, type, full){
    return '<span class=spark>' + data + '</span>'
  }"
        )
      )
    )

    ## parse search term from query
    query <- parseQueryString(session$clientData$url_search)

    ## define a callback that initializes a sparkline on elements which have not been initialized before
    ##   this is essential for pagination
    sparklineDrawCallback = htmlwidgets::JS(
      "function (oSettings, json) {
        $('.spark:not(:has(canvas))').sparkline('html', {
          type: 'bar',
          highlightColor: 'orange',
          chartRangeMin: 0
        });
      }"
    )

    dt <- DT::datatable(summarized_report,
                         filter = "top",
                         escape = FALSE,
                         rownames = show_rownames,
                         selection = "single",
                         extensions = c('Buttons'),
                         options = list(columnDefs = columnDefs,
                                        dom = 'Bfrtip',
                                        buttons = c('pageLength', 'colvis', 'pdf', 'excel' , 'csv', 'copy'),
                                        lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100', 'All')),
                                        pageLength = 10,
                                        #autoWidth = TRUE,
                                        drawCallback = sparklineDrawCallback,
                                        order = list(attr(summarized_report, 'stat_column') - zero_col, "desc"),
                                        search = list(
                                          search = ifelse("search" %in% names(query), query['search'], ""),
                                          regex = TRUE, caseInsensitive = FALSE
                                        ))
                         )


    ## TODO: Consider adding more information in child rows: https://rstudio.github.io/DT/002-rowdetails.html
    ##  For example: taxonomy ID, links to assemblies (e.g. www.ncbi.nlm.nih.gov/assembly/organism/821)
    ##   and organism overview http://www.ncbi.nlm.nih.gov/genome/?term=txid821[Organism:noexp]


    ## Give the correct format to the columns: thousands separators for numbers, and percent sign for percents
    if (!isTRUE(input$opt_display_percentage) && !isTRUE(input$opt_zscore)) {
      dt <- dt %>%
        DT::formatCurrency(attr(summarized_report, 'stat_column'), currency = '', digits = 1 ) %>%
        DT::formatCurrency(attr(summarized_report, 'data_columns'), currency = '', digits = 0 )
    } else {
      suffix <- ifelse(isTRUE(input$opt_zscore),"","%")
      dt <- dt %>%
        DT::formatString(attr(summarized_report, 'stat_column'), suffix = suffix) %>%
        DT::formatString(attr(summarized_report, 'data_columns'), suffix = suffix)
    }

    ## Add color bar
    str(summarized_report[,attr(summarized_report, 'data_columns')])
    dt <- dt %>% DT::formatStyle(
      attr(summarized_report, 'data_columns'),
      background = DT::styleColorBar(range(summarized_report[,attr(summarized_report, 'stat_column')],na.rm=TRUE), 'lightblue'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )

    ## use the sparkline package and the getDependencies function in htmlwidgets to get the
    ## dependencies required for constructing sparklines and then inject it into the dependencies
    ## needed by datatable
    dt$dependencies <-
      append(dt$dependencies,
             htmlwidgets:::getDependency('sparkline'))

    dt
  })

  ns <- session$ns

  output$d3heatmap_samples_comparison <- renderUI({
    #req(input$dt_samples_comparison_rows_current)
    selected_rows <- input$dt_samples_comparison_rows_current
    #selected_rows <- 1:50
    d3heatmap::d3heatmapOutput(ns('my_d3heatmap'),
                               width = "100%",
                               height = paste0(
                                 200 + length(selected_rows) * 15,
                                 "px"
                               ))
    plotOutput(ns("my_pheatmap"))
  })


  output$my_d3heatmap <- d3heatmap::renderD3heatmap({
    #req(input$dt_samples_comparison_rows_current)
    selected_rows <- input$dt_samples_comparison_rows_current

    #selected_rows <- 1:50

    sr <- summarized_report()
    report_mat <- as.matrix(sr[, attr(sr, "data_columns")])
    rownames(report_mat) <- gsub("^[a-z-]_", "", sr[, 1])


    report_mat <-
      zero_if_na(report_mat[selected_rows, ])
    report_mat[report_mat < 0] <- 0
    d3heatmap::d3heatmap(
      report_mat,
      Rowv = "row" %in% input$heatmap_cluster,
      Colv = "column" %in% input$heatmap_cluster,
      # No Column reordering
      scale = input$heatmap_scale,
      yaxis_width = 300,
      xaxis_height = 200,
      xaxis_font_size = "10pt",
      yaxis_font_size = "10pt",
      colors = grDevices::colorRampPalette(rev(RColorBrewer::brewer.pal(n=7,name="RdYlBu")))(100)
    )
  })

  output$my_pheatmap <- renderPlot({
    #req(input$dt_samples_comparison_rows_current)
    selected_rows <- input$dt_samples_comparison_rows_current

    #selected_rows <- 1:50

    sr <- r_summarized_report()
    report_mat <- as.matrix(sr[, attr(sr, "data_columns")])
    rownames(report_mat) <- gsub("^[a-z-]_", "", sr[, 1])


    report_mat <-
      zero_if_na(report_mat[selected_rows, ])
    report_mat[report_mat < 0] <- 0

    pheatmap::pheatmap(
      report_mat,
      cluster_rows = "row" %in% input$heatmap_cluster,
      cluster_cols = "column" %in% input$heatmap_cluster,
      scale = input$heatmap_scale
      )
  })


  output$cluster_plot <- renderPlot({
    my_reports <- selected_reports()
    if (length(my_reports) == 0)
      return()

    #idvar=".id"; timevar="name"
    idvar = "name"
    timevar = ".id"

    all.s.reads <-
      stats::reshape(
        get_level_reads(my_reports, level == "S", min_perc = 0.01)[, c(idvar, timevar, "reads")],
        timevar = timevar,
        idvar = idvar,
        direction = "wide"
      )
    rownames(all.s.reads) <- all.s.reads$NAME
    all.s.reads$name <- NULL
    colnames(all.s.reads) <-
      sub("reads.(.*)", "\\1", colnames(all.s.reads))

    eucl.dist <- stats::dist(t(all.s.reads))
    hc <- stats::hclust(eucl.dist)
    dend <- stats::as.dendrogram(hc)

    gapmap::gapmap(
      m = as.matrix(eucl.dist),
      d_row = rev(dend),
      d_col = dend,
      h_ratio = c(0.2, 0.5, 0.3),
      v_ratio = c(0.2, 0.5, 0.3)
    )
  })

  output$row_details_table <- DT::renderDataTable({
    req(input$dt_samples_comparison_rows_selected)
    sel <- input$dt_samples_comparison_rows_selected

    data_columns <- attr(get_summarized_reportc(), "data_columns")

    res <- data.frame(
      Sample = colnames(get_summarized_reportc())[data_columns],
      "Number of reads" = as.numeric(get_summarized_reportc()[sel, data_columns]),
      "Percent of reads in sample" = signif(as.numeric(get_summarized_reportp()[sel, data_columns]),5),
      check.names = FALSE
    )

    DT::datatable(res, rownames = FALSE, options=list(autoWidth = FALSE)) %>%
      DT::formatCurrency(2, currency = '', digits = 0 ) %>%
      DT::formatString(3, suffix = "%")
  })

  output$txt_samples_comparison <- renderUI({
    req(input$dt_samples_comparison_rows_selected)
    selected_row <-
      zero_if_na(r_summarized_report()[input$dt_samples_comparison_rows_selected, ])
    #is_domain <- input$opt_classification_level == "D"
    is_domain <- FALSE
    data_columns <- attr(selected_row, "data_columns")
    taxid <- selected_row[, "Taxonid"]
    tagList(
      h3(selected_row$Name),
      "NCBI links: ",
      a(href=sprintf('http://www.ncbi.nlm.nih.gov/genome/?term=txid%s[Organism:noexp]', taxid),"Organism overview"),", ",
      a(href=sprintf('http://www.ncbi.nlm.nih.gov/assembly/organism/%s/latest', taxid), "Assemblies"),
      selected_row$Taxonstring)

  })

  observeEvent(input$btn_sc_filter, {
    req(input$dt_samples_comparison_rows_selected)
    ## TODO: How to get current choices from selectizeInput?
    taxonstring <- r_summarized_report()[input$dt_samples_comparison_rows_selected, "Taxonstring"]
    selected_path <- strsplit(taxonstring, "|", fixed = TRUE)[[1]]
    selected_name <- selected_path[length(selected_path)]
    message("filtering ", selected_path[length(selected_path)])

    updateSelectizeInput(
      session,
      "contaminant_selector",
      selected = unique(c(
        input$contaminant_selector, selected_name
      )),
      choices = unique(c(allcontaminants, selected_name))
    )
  })

  observeEvent(input$btn_sc_gointo, {
    req(input$dt_samples_comparison_rows_selected)
    taxonstring <- r_summarized_report()[input$dt_samples_comparison_rows_selected, "Taxonstring"]
    selected_path <- strsplit(taxonstring, "|", fixed = TRUE)[[1]]
    selected_name <- selected_path[length(selected_path)]

    #input$dt_samples_comparison_search <- selected_name
  })


}

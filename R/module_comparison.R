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
                 radioButtons(ns("comparison_type"), label = "",
                              choices = c("Comparison table"="table", "Pairwise comparison scatterplot"="scatter"),
                              selected = "table"),
                 selectizeInput(ns("sample_selector"),label="Samples to show in table", multiple=TRUE, choices=NULL,selected=NULL,
                                options=list(create = TRUE)),
                 column(6, shinyjs::hidden(selectizeInput(ns("sample_selector1"),label="Sample 1", multiple=FALSE, choices=NULL,selected=NULL,
                                                options=list(create = TRUE)))),
                 column(6, shinyjs::hidden(selectizeInput(ns("sample_selector2"),label="Sample 2", multiple=FALSE, choices=NULL,selected=NULL,
                                                options=list(create = TRUE))))
                 )),
    fluidRow(
      box(width=6, background = "green",
          column(6,
                 selectizeInput(ns("opt_statistic"),
                                label = " Statistic",
                                choices = c("Mean", "Median", "Max", "Sd",
                                            "Maximum absolute deviation", "Max Z-score"),
                                selected = "Mean"),
                 shinyBS::bsTooltip(id=ns("opt_statistic"),
                                    title = "Select summarization statistic used in fifth column of data table.",
                           placement = "left", trigger = "hover"),
                 checkboxGroupInput(ns("opts_normalization"), label = "",
                                    choices = c("Normalize by total # of reads"="opt_display_percentage",
                                                "Apply VST"="opt_vst_data",
                                                "Robust z-score"="opt_zscore"),
                                    inline = TRUE)
                 ),
          column(6,
                 radioButtons(
                   ns("opt_classification_level"),
                   label = "Taxon level",
                   choices = taxon_levels,
                   selected = "-",
                   inline = TRUE
                 ),
                 radioButtons(
                   ns("opt_show_reads_stay"),
                   label = "",
                   choices = c(
                     "Reads by taxon" = "reads_stay",
                     "Reads by taxon and children" = "reads",
                     "both"
                   ), inline = TRUE)

          )
      ),
      box(
        width = 6,
        selectizeInput(
          ns('contaminant_selector'),
          allcontaminants,
          label = "Filter reads from taxon",
          selected = c("unclassified", "Homo sapiens", "root"),
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
          label = "Filter reads from taxon and its children",
          allcontaminants,
          selected = c("artificial sequences"),
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
    div(id=ns("table_div"),
      div(style = 'overflow-x: scroll',
          DT::dataTableOutput(ns('dt_samples_comparison'))),
      actionButton(ns("btn_sc_filter"), "Filter taxon"),
      actionButton(ns("btn_sc_filter_clade"), "Filter taxon and children"),
      actionButton(ns("btn_sc_gointo"), "Go Into")
    ),
    shinyjs::hidden(
      div(id=ns("scatter_div"),
          column(6, scatterD3::scatterD3Output(ns("scatter_plot"))),
          column(6, scatterD3::scatterD3Output(ns("ma_plot")))
          )
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
#' @param sample_data A \code{data.frame} specifying sample names and file paths (read from a defs.csv file).
#' @param reports A list with report \code{data.frame}s.
#' @param datatable_opts Additional options for creating the datatable.
#' @param filter_func If not NULL, \code{filter_func} is applied to every data.frame in \code{reports}.
#' @param search Search term to initialize table with.
#'
#' @return Comparison module server functionality
#' @export
comparisonModule <- function(input, output, session, sample_data, reports,
                             datatable_opts = NULL, filter_func = NULL, search = NULL) {

  observe({
    updateSelectizeInput(session, "sample_selector",
                         choices=sample_data()[,"Name"], selected=sample_data()[,"Name"])
    })

  observeEvent(input$comparison_type, {
    if (input$comparison_type == "table") {
      shinyjs::hide("sample_selector1")
      shinyjs::hide("sample_selector2")
      shinyjs::hide("scatter_div")
      shinyjs::show("sample_selector")
      shinyjs::show("table_div")
    } else {
      shinyjs::show("sample_selector1")
      shinyjs::show("sample_selector2")
      shinyjs::show("scatter_div")
      shinyjs::hide("sample_selector")
      shinyjs::hide("table_div")
    }
  })

  observeEvent(r_summarized_report(), {
    report <- r_summarized_report()
    data_cols <- colnames(report)[attr(report, "data_columns")]
    names(data_cols) <- data_cols
    updateSelectizeInput(session, "sample_selector1",
                         choices=data_cols, selected=data_cols[1])
    updateSelectizeInput(session, "sample_selector2",
                         choices=data_cols,
                         selected=data_cols[ifelse(length(data_cols) > 1, 2,1)])

  })

  small_report <- reactive({
    report <- r_summarized_report()
    req(all(c(input$sample_selector1,input$sample_selector2) %in% colnames(report)))
    report <- zero_if_na(report[,c(input$sample_selector1,input$sample_selector2, "Name"), drop = F])
    report <- report[report[,1] > 0 | report[,2] > 0, , drop=FALSE]

    max_r <- ifelse(report[,1] > report[,2], report[,1], report[,2])
    report <- report[head(order(max_r, decreasing=TRUE), n=50), , drop=FALSE]
    #apply(report[,c(input$sample_selector1,input$sample_selector2)], 1, max, na.rm=T)
    report$tooltip <-
      sprintf("<b>%s</b>: %s</br><b>%s</b>: %s",
              input$sample_selector1, report[,1],
              input$sample_selector2, report[,2])

    report
  })

  output$scatter_plot <- scatterD3::renderScatterD3({
    report <- small_report()
    req(all(c(input$sample_selector1,input$sample_selector2) %in% colnames(report)))
    scatterD3::scatterD3(
              x = report[,input$sample_selector1], y = report[,input$sample_selector2],
              lab = report$Name, tooltip_text = report$tooltip,
              xlab = input$sample_selector1, ylab = input$sample_selector2,
              lines = data.frame(slope = 1, intercept = 0))
  })

  output$ma_plot <- scatterD3::renderScatterD3({
    report <- small_report()
    log10_mean <- log10((report[,input$sample_selector1]+report[,input$sample_selector2])/2)
    log2_ratio <- log2(report[,input$sample_selector1]/report[,input$sample_selector2])
    log2_ratio[log2_ratio > 5] <- 5
    log2_ratio[log2_ratio < -5] <- -5

    scatterD3::scatterD3(
              x = log10_mean, y = log2_ratio, lab = report$Name,
              tooltip_text = report$tooltip,
              xlab = "log10 mean", ylab = "log2 ratio")
  })




  ## parse search term from query
  #query <- parseQueryString(session$clientData$url_search)
  #ifelse("search" %in% names(query), query['search'], ""))
  dt_options <- reactiveValues(search = "")
  #observeEvent(search(), {
  #  dt_options$search <-search()
  #})

  observeEvent(input$dt_samples_comparison_search, {
    dt_options$search <- input$dt_samples_comparison_search
  })

  selected_reports <- reactive({
    selected <- unlist(lapply(input$sample_selector,function(s) grep(paste0("^",s,"$"),sample_data()[,"Name"])))
    #updateSelectizeInput(session, "sample_selector",
    #                     selected=sample_data()[selected,"Name"])
    reports()[unique(sort(selected))]
  })

  get_summarized_report_reads_stay <- reactive({
    req(reports_filtered())
    get_summarized_report2(reports_filtered(), "reads_stay")
  })

  get_summarized_report_reads_clade <- reactive({
    req(reports_filtered())
    get_summarized_report2(reports_filtered(), "reads")
  })

  get_summarized_report_reads_both <- reactive({
    req(reports_filtered())
    get_summarized_report2(reports_filtered(), c("reads", "reads_stay"))
  })

  reports_filtered <- reactive({
    req(selected_reports())
    ## filter reports, if a filter function is given to the module
    if (!is.null(filter_func)) {
      lapply(selected_reports(), filter_func)
    } else {
      selected_reports()
    }
  })

  get_summarized_report1 <- reactive({
    withProgress(message="Combining sample reports ...", { get_summarized_report_reads_both() })
  })

  and <- function(x) {
    if (length(x) == 1) {
      return(x[[1]])
    }

    x[[2]] <- x[[1]] & x[[2]]

    return (and(x[-1]))
  }

  get_summarized_reportc <- reactive({
    requireNamespace("dplyr")
    summarized_report <-  get_summarized_report1()

    ## Remove taxons or clades
    sel_rm_clades <- summarized_report$Name %in% input$contaminant_selector_clade
    sel_rm_taxons <- sel_rm_clades | summarized_report$Name %in% input$contaminant_selector
    taxonstrings <- summarized_report$Taxonstring

    clade_reads_col <- attr(summarized_report, "reads_columns")
    if (!is.null(clade_reads_col) && sum(sel_rm_taxons) > 0) {
      ## Update all the parent numbers
      for (contaminant_i in which(sel_rm_taxons)) {
        reads_clade <- as.numeric(summarized_report[contaminant_i, clade_reads_col])
        tax_string <- taxonstrings[contaminant_i]
        update_indices <- summarized_report$Taxonstring == tax_string
        repeat {
          new_tax_string <- sub("(.*)\\|.*", "\\1",tax_string)
          if (tax_string == new_tax_string) break;
          update_indices <- update_indices | taxonstrings == new_tax_string
          tax_string <- new_tax_string
        }
        summarized_report[update_indices,clade_reads_col] <-
          summarized_report[update_indices,clade_reads_col] - rep(reads_clade, each=sum(update_indices))
      }
    }
    if (sum(sel_rm_clades) > 0) {

      for (taxonstring in taxonstrings[sel_rm_clades]) {
        sel_rm_taxons <- sel_rm_taxons | startsWith(taxonstrings, taxonstring)
      }
      stopifnot(length(sel_rm_taxons) == nrow(summarized_report))
    }

    summarized_report <- summarized_report[!sel_rm_taxons, ]

    if (req(input$opt_classification_level) != "-") {
      summarized_report <- summarized_report[summarized_report[["Level"]] %in% input$opt_classification_level,]
    }

    summarized_report
  })

  get_summarized_reportp <- reactive({
    withProgress(message="Normalizing samples ...", { get_summarized_reportc() %>% normalize_data_cols() })
  })


  r_summarized_report <- reactive({

    if ("opt_display_percentage" %in% input$opts_normalization) {
      summarized_report <- get_summarized_reportp()
    } else {
      summarized_report <- get_summarized_reportc()
    }
    sav_attr <- attributes(summarized_report)

    if ("opt_vst_data" %in% input$opts_normalization) {
      summarized_report <- withProgress(message="Applying variance-stabilizing transformation ...", {
        summarized_report %>% log_data_cols()
      })
    }

    if ("opt_zscore" %in% input$opts_normalization) {
      summarized_report <- withProgress(message="Calculating z-score ...", {
        summarized_report %>% calc_robust_zscore(min_scale=ifelse("opt_display_percentage" %in% input$opts_normalization, 0.001, 1))
      })
    }

    if (input$opt_show_reads_stay != "both") {
      req(sav_attr[["data_column_start"]])
      ## remove the columns that are not wanted
      sav_col <- paste0(input$opt_show_reads_stay, "_columns")
      rm_col <- switch(input$opt_show_reads_stay, reads="reads_stay_columns", reads_stay="reads_columns")
      validate(need(attr(summarized_report,rm_col),
                    message=summarized_report,paste0(rm_col," attribute of summarized report is empty!")))
      sav_attr[["data_columns"]] <- seq(from=sav_attr[["data_column_start"]], length.out = length(sav_attr[[sav_col]]))
      sav_attr[[sav_col]] <- sav_attr[["data_columns"]]
      sav_attr[[rm_col]] <- NULL
      sav_attr[["names"]] <- sav_attr[["names"]][-attr(summarized_report,rm_col)]


      summarized_report <- summarized_report[, -attr(summarized_report,rm_col), drop = FALSE]
      sav_attr[["names"]][sav_attr[["data_columns"]]] <- sub(paste0(".",input$opt_show_reads_stay), "",sav_attr[["names"]][sav_attr[["data_columns"]]])

    }
    mostattributes(summarized_report) <- sav_attr

    validate(
      need(length(summarized_report) > 0, message = "summarized report is empty"),
      need(colnames(summarized_report), message = "summarized report has no column names"),
      need(attr(summarized_report, 'data_columns'), message = "data_columns NULL"))

    data_cols <- attr(summarized_report, "data_columns")
    validate(need(all(sapply(summarized_report[data_cols], is.numeric)),
                  message = "Not all data columns are numeric?!"))
    round_digits <- ifelse(isTRUE("opt_display_percentage" %in% input$opts_normalization), 3, 1)

    summarized_report$STAT <- signif(apply(zero_if_na(summarized_report[,data_cols, drop=F]), 1, stat_name_to_f[[input$opt_statistic]]), 3)

    colnames(summarized_report)[colnames(summarized_report) == "STAT"] <- input$opt_statistic
    summarized_report$OVERVIEW = apply(round(zero_if_na(summarized_report[,data_cols, drop=F]), round_digits), 1, paste0, collapse = ",")
    colnames(summarized_report)[colnames(summarized_report) == "OVERVIEW"] <- "Overview"

    if (any(c("opt_display_percentage","opt_zscore", "opt_vst_data") %in% input$opts_normalization)) {
      summarized_report[,data_cols] <- signif(summarized_report[,data_cols, drop=F], 4)
    }

    summarized_report

  })

  observeEvent(sample_data, {
    updateSelectizeInput(session, "sample_selector",
                         choices=sample_data()[,"Name"], selected=sample_data()[,"Name"]
    )
  })

  dt_proxy <- DT::dataTableProxy('dt_samples_comparison', session = session)
  output$dt_samples_comparison <- DT::renderDataTable({

    summarized_report <- r_summarized_report()
    validate(need(summarized_report, message = "No data"),
             need(attr(summarized_report, 'data_columns'), message = "data_columns NULL"),
             need(attr(summarized_report, 'taxonid_column'), message = "taxonid_data_columns NULL"),
             need(attr(summarized_report, 'stat_column'), message = "stat_columns NULL"))

    summarized_report$Taxonstring <- beautify_taxonstring(summarized_report$Taxonstring)

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
                         #filter = "top",
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
                                          search = dt_options$search,
                                          regex = TRUE, caseInsensitive = FALSE
                                        ))
                         )


    ## TODO: Consider adding more information in child rows: https://rstudio.github.io/DT/002-rowdetails.html
    ##  For example: taxonomy ID, links to assemblies (e.g. www.ncbi.nlm.nih.gov/assembly/organism/821)
    ##   and organism overview http://www.ncbi.nlm.nih.gov/genome/?term=txid821[Organism:noexp]


    ## Give the correct format to the columns: thousands separators for numbers, and percent sign for percents
    if (!any(c("opt_display_percentage","opt_zscore", "opt_vst_data") %in% input$opts_normalization)) {
      dt <- dt %>%
        DT::formatCurrency(attr(summarized_report, 'stat_column'), currency = '', digits = 1 ) %>%
        DT::formatCurrency(attr(summarized_report, 'data_columns'), currency = '', digits = 0 )
    } else {
      suffix <- ifelse(any(c("opt_zscore", "opt_vst_data") %in% input$opts_normalization),"","%")
      dt <- dt %>%
        DT::formatString(attr(summarized_report, 'stat_column'), suffix = suffix) %>%
        DT::formatString(attr(summarized_report, 'data_columns'), suffix = suffix)
    }

    ## Add color bar
    #str(summarized_report[,attr(summarized_report, 'data_columns')])
    if (0) {
    dt <- dt %>% DT::formatStyle(
      attr(summarized_report, 'data_columns'),
      background = DT::styleColorBar(range(summarized_report[,attr(summarized_report, 'stat_column')],na.rm=TRUE), 'lightblue'),
      backgroundSize = '90% 80%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
    }

    if (nrow(summarized_report) > 0) {
      #brks <- quantile(summarized_report[,attr(summarized_report, 'data_columns')], probs = seq(.5, .95, .05), na.rm = TRUE)
      #brks <- quantile(summarized_report[,attr(summarized_report, 'stat_column')], probs = seq(.05, .95, .05), na.rm = TRUE)
      #brks <- seq(from=0, to=max(summarized_report[,attr(summarized_report, 'stat_column')], na.rm=T), length.out = 20)
      brks <- quantile(summarized_report[,attr(summarized_report, 'data_columns')], probs = cumsum(1/2^(1:20)), na.rm =TRUE)
      clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>% {paste0("rgb(255,", ., ",", ., ")")}
      dt <- dt %>% DT::formatStyle(attr(summarized_report, 'data_columns'), backgroundColor = DT::styleInterval(brks, clrs))
    }


    if (requireNamespace("sparkline")) {
      ## use the sparkline package and the getDependencies function in htmlwidgets to get the
      ## dependencies required for constructing sparklines and then inject it into the dependencies
      ## needed by datatable
      dt$dependencies <-
        append(dt$dependencies,
               getDependency('sparkline'))
    }

    dt
  }, options = list(initComplete = JS(
                      "function(settings, json) {",
                      "$(this.api().table().header()).addClass('rotate');",
                      "}")))

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

  selected_row <- reactive({
    r_summarized_report()[input$dt_samples_comparison_rows_selected,]
  })

  observeEvent(input$btn_sc_filter, {
    req(selected_row())

    current_selection <- input$contaminant_selector
    selected_name <- selected_row()[["Name"]]

    updateSelectizeInput(
      session, "contaminant_selector",
      selected = unique(c(current_selection, selected_name)),
      choices = unique(c(allcontaminants, current_selection, selected_name))
    )
  })

  observeEvent(input$btn_sc_filter_clade, {
    req(selected_row())
    current_selection <- input$contaminant_selector_clade
    selected_name <- selected_row()[["Name"]]

    updateSelectizeInput(
      session, "contaminant_selector_clade",
      selected = unique(c(current_selection, selected_name)),
      choices = unique(c(allcontaminants, current_selection, selected_name))
    )
  })

  observeEvent(input$btn_sc_gointo, {
    req(dt_proxy)
    req(selected_row())

    DT::updateSearch(dt_proxy, list(global=selected_row()[["Name"]]
))

    #input$dt_samples_comparison_search <- selected_name
  })


}

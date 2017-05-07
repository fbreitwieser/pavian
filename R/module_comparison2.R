taxRanks <- c(
  "Any" = "-",
  "Species" = "S",
  "Genus" = "G",
  "Family" = "F",
  "Order" = "O",
  "Class" = "C",
  "Phylum" = "P",
  "Domain" = "D"
)

show_rownames <- FALSE

## define a callback that
##  (a) makes the background colors of number cells '!important'
##       otherwise the row-selection overwrites those values
##       see https://github.com/rstudio/DT/issues/364
##  (b) center the numeric cells
##  (c) initializes a sparkline on elements which have not been initialized before
##       this is essential for pagination
drawCallback = htmlwidgets::JS(
  "function (oSettings, json) {
        $('td.dt-right').each(function(i) {
          var color = $(this).css('background-color');
          if (color != 'rgba(0, 0, 0, 0)') {
            $(this).attr('style', 'text-align: center; background-color: '+color+'!important;' );
          }})
        $('.spark:not(:has(canvas))').sparkline('html', {
          type: 'bar',
          highlightColor: 'orange',
          chartRangeMin: 0
        });
      }"
)

taxRanks <- c(
  "All taxonomic ranks" = "-",
  "- Domain" = "D",
  "-- Phylum" = "P",
  "--- Class" = "C",
  "---- Order" = "O",
  "----- Family" = "F",
  "------ Genus" = "G",
  "------- Species" = "S"
)



#taxRanks <- list(
#  "At taxon" = taxRanks,
#  "At taxon and children" = setNames(paste("C",taxRanks),paste(names(taxRanks),"clade"))
#)

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
    #shinyjs::extendShinyjs(text = taxRankSliderJS),
    fluidRow(box(width=12,collapsible=TRUE,collapsed=TRUE,title="Expand for further options -->",
                 radioButtons(ns("comparison_type"), label = NULL,
                              choices = c("Comparison table"="table", "Pairwise comparison scatterplot"="scatter"),
                              selected = "table"),
                 #selectizeInput(ns("sample_selector"),label="Samples to show in table", multiple=TRUE, choices=NULL,selected=NULL,
                 #              options=list(create = TRUE)),
                 column(6, shinyjs::hidden(selectizeInput(ns("sample_selector1"),label="Sample 1", multiple=FALSE, choices=NULL,selected=NULL,
                                                          options=list(create = TRUE)))),
                 column(6, shinyjs::hidden(selectizeInput(ns("sample_selector2"),label="Sample 2", multiple=FALSE, choices=NULL,selected=NULL,
                                                          options=list(create = TRUE)))),
                 checkboxInput(ns("dont_wrap_name"), label = "Don't wrap name in table", value = FALSE),
                 checkboxGroupInput(ns("table_columns"), label = "Table columns", choices = c("Taxonid","Overview","Taxonstring"), selected = c("Taxonstring"), inline = TRUE)
    )),
    fluidRow(
      box(width=7, background = "green",
          div(class="col-sm-4 lessPadding lessMargin",
                     selectizeInput(
                       ns("opt_taxRank"), label = "Select taxonomic rank",
                       choices = taxRanks, selected = "-"#,inline = TRUE
                     )
          ),
          div(class="col-sm-4 lessPadding lessMargin",
              selectizeInput(
                ns("opt_numericCols"), label = "Data", multiple = TRUE,
                options = list(placeholder = "Select at least one kind of data to display"),
                choices = c("taxonReads","cladeReads", "taxonReads %","cladeReads %","taxonReads z-score","cladeReads z-score"), selected = "taxonReads"#,inline = TRUE
              )
          ),
          div(class="col-lg-4 col-md-4 col-sm-4 lessPadding lessMargin",
              selectizeInput(ns("opt_statistic"), label = " Summary statistics", multiple = TRUE,
                             options = list(placeholder = "Add column(s) with summary statistics of taxa's data"),
                             choices = c("Mean", "Median", "Max", "Min", "Sd",
                                         "Maximum absolute deviation", "Max Z-score"),
                             selected = c("Mean","Sd","Max Z-score"))#,
              #shinyBS::bsTooltip(id=ns("opt_statistic"),
              #                   title = "Select summarization statistic used in fifth column of data table.",
              #                   placement = "left", trigger = "hover")
          )
      ),
      box(
        width = 5,
        div(class="col-lg-6 col-md-12 lessPadding",
            selectizeInput(
              ns('contaminant_selector'),
              allcontaminants, selected = c("unclassified", "Homo sapiens", "root"),
              label = "Filter taxon", multiple = TRUE,
              options = list(maxItems = 25, create = TRUE, placeholder = 'Filter taxa'),
              width = "100%"
            )),
        div(class="col-lg-6 col-md-12 lessPadding",
            selectizeInput(
              ns('contaminant_selector_clade'),
              allcontaminants, selected = c("artificial sequences"),
              label = "Filter taxon and its children", multiple = TRUE,
              options = list( maxItems = 25, create = TRUE, placeholder = 'Filter taxa with children' ),
              width = "100%"
            )
        )
      )
    ),
    htmlOutput(ns("messages")),
    box(width=12,
    div(id=ns("table_div"),
        DT::dataTableOutput(ns('dt_samples_comparison')),
        downloadButton(ns('downloadData'), 'Download full table in tab-separated value format'),
        uiOutput(ns("filter_buttons"))
    ),
    shinyjs::hidden(
      div(id=ns("scatter_div"),
          column(6, scatterD3::scatterD3Output(ns("scatter_plot"))),
          column(6, scatterD3::scatterD3Output(ns("ma_plot")))
      )
    )
  ))
}


stat_name_to_f <- list(
  "Mean"=mean,
  "Median"=median,
  "Max"=max,
  "Min"=min,
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
#' @param tax_data TODO
#' @param clade_reads TODO
#' @param taxon_reads TODO
#' @param datatable_opts Additional options for creating the datatable.
#' @param filter_func If not NULL, \code{filter_func} is applied to every data.frame in \code{reports}.
#' @param tax_data_add TODO
#' @param search Search term to initialize table with.
#'
#' @return Comparison module server functionality
#' @export
comparisonModule <- function(input, output, session, sample_data, tax_data, clade_reads, taxon_reads,
                             datatable_opts = NULL, filter_func = NULL, tax_data_add = NULL, search = NULL) {

  dt_options <- reactiveValues(search = "", order = NULL, colnames = NULL)

  observeEvent(input$comparison_type, {
    if (input$comparison_type == "table") {
      shinyjs::hide("sample_selector1")
      shinyjs::hide("sample_selector2")
      shinyjs::hide("scatter_div")
      #shinyjs::show("sample_selector")
      shinyjs::show("table_div")
    } else {
      shinyjs::show("sample_selector1")
      shinyjs::show("sample_selector2")
      shinyjs::show("scatter_div")
      #shinyjs::hide("sample_selector")
      shinyjs::hide("table_div")
    }
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

  base_set_name <- reactive({
    validate(need(attr(sample_data(), "set_name"), message = "Attribute set_name not set for sample_data"))
    basename(attr(sample_data(), "set_name"))
  })

  output$messages <- renderUI({
    if (!any(grepl("clade", input$opt_numericCols)) && input$opt_taxRank != "-") {
      return(tags$p("Warning: A specific taxonomic rank is selected, but data is not on the clade level. Thus any data from the taxa's children are not visible. Consider adding clade-level data."))
    }
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

  output$downloadData <- downloadHandler(
    filename = function() { sprintf("%s-matrix-all-%s.tsv", base_set_name(), format(Sys.time(), "%y%m%d")) },
    content = function(file) {
      write.table(r_summarized_report(), file, row.names = FALSE, sep = "\t")
    }
  )

  observeEvent(input$dt_samples_comparison_state, {
    dt_options$order <- input$dt_samples_comparison_state$order
  })

  observeEvent(input$dt_samples_comparison_search, {
    dt_options$search <- input$dt_samples_comparison_search
  })

  #selected_reports <- reactive({
  #  selected <- unlist(lapply(input$sample_selector,function(s) grep(paste0("^",s,"$"),sample_data()[,"Name"])))
  #  #updateSelectizeInput(session, "sample_selector",
  #  #                     selected=sample_data()[selected,"Name"])
  #  reports()[unique(sort(selected))]
  #})

  and <- function(x) {
    if (length(x) == 1) {
      return(x[[1]])
    }

    x[[2]] <- x[[1]] & x[[2]]

    return (and(x[-1]))
  }

  rm_taxons <- reactive({
    tax_data()$name %in% input$contaminant_selector_clade | tax_data()$name %in% input$contaminant_selector
  })


  shown_rows <- reactive({
    filter_taxa(tax_data(),
                rm_clades = input$contaminant_selector_clade,
                rm_taxa = input$contaminant_selector,
                taxRank = input$opt_taxRank) })

  filtered_clade_reads <- reactive({
    filter_cladeReads(clade_reads(), tax_data(), rm_taxa = c(input$contaminant_selector,input$contaminant_selector_clade))
  })

  summarized_report_df <- reactive({

    req(input$opt_numericCols)
    #if (length(input$opt_numericCols) == 0)
    #  return()
    #validate(need(nrow(mydata) == nrow(tax_data()),
    #              message = sprintf("Number of rows of data (%s) and tax_data (%s) different!",
    #                                nrow(mydata), nrow(tax_data()))))

    summarized_report <- data.frame(Name=get_col(tax_data(),"name"),TaxID=get_col(tax_data(),"taxID"), stringsAsFactors = FALSE)

    ## Add columns from tax_data_add
    if (!is.null(tax_data_add)) {
      validate(
        need(is.data.frame(tax_data_add), message="tax_data_add is no data.frame"),
        need(ncol(tax_data_add) > 1, message="Need more than two columns in tax_data_add"))

      tax_ids <- as.character(summarized_report[,"TaxID"])
      upd_rows1 <- tax_ids[!tax_ids %in% rownames(tax_data_add)]
      tax_data_add[upd_rows1,] <- NA
      summarized_report <- cbind(summarized_report, tax_data_add[tax_ids, ])
    }


    taxColumns = colnames(summarized_report)
    numericCols = input$opt_numericCols
    samples = c(input$opt_statistic, get_col(sample_data(), "Name"))

    dt_container = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          lapply(taxColumns, th, rowspan=2),
          lapply(numericCols, th, colspan=length(samples)),
          th(rowspan = 2, 'Lineage')
        ),
        tr(
          lapply(rep(samples, length(numericCols)), th)
        )
      )
    ))


    add_names_to_columns <- length(input$opt_numericCols) > 1
    numericCols2 <- strsplit(input$opt_numericCols, " ")
    for (column in numericCols2) {
      if (isTRUE(column[1] == "cladeReads")) {
        mydata <- filtered_clade_reads()
      } else if (isTRUE(column[1] == "taxonReads")) {
        mydata <- taxon_reads()
      } else {
        message("column ", column, "??")
      }

      if (length(column) == 2) {
        normalize_by_colSums <- column[1] == "taxReads" || input$opt_taxRank != "-"

        # TODO: Normalize by library size
        #if (isTRUE(input$opt_display_percentage_lib)) {
        #  validate(need_attr(reports(), "LibrarySize"))
        #  sum_reads <- attr(reports(), "LibrarySize")
        #}
        if (normalize_by_colSums) {
          sum_reads <- colSums(mydata[shown_rows(), ], na.rm=TRUE)
        } else {
          ## For clade reads (when all are displayed), normalize by the sum of all taxon reads
          sum_reads <- colSums(taxon_reads()[shown_rows(), ], na.rm=TRUE)
        }

        if (column[2] == "%")
          mydata <- signif(normalize(mydata,sum_reads),4)
      }

      colnames(mydata) <- get_col(sample_data(), "Name")
      if (add_names_to_columns)
        colnames(mydata) <- paste(colnames(mydata),column,sep="\n")

      for (stat in input$opt_statistic) {
        summarized_report[[length(summarized_report)+1]] <- apply(mydata, 1, function(x) {
          x[is.na(x)] <- 0
          signif(stat_name_to_f[[stat]](x), 5)
          })
      }

      summarized_report <- cbind(summarized_report, mydata)
    }
    summarized_report <- cbind(summarized_report, TaxLineage = get_col(tax_data(),"taxLineage"))
    list(summarized_report[shown_rows(), ], dt_container)
  })

  ## Different DT version handle namespaces differently. Make sure it works in all of them.
  dt_proxy <- DT::dataTableProxy(session$ns('dt_samples_comparison'))
  dt_proxy1 <- DT::dataTableProxy('dt_samples_comparison')
  observe({
    ## replaceData does not work with modules, currently
    ##  see https://github.com/rstudio/DT/issues/359
    req(summarized_report_df())
    dataTableAjax(session, summarized_report_df()[[1]], rownames = FALSE, outputId = 'dt_samples_comparison')
    dataTableAjax(session, summarized_report_df()[[1]], rownames = FALSE,
                  outputId = session$ns('dt_samples_comparison'))
    reloadData(dt_proxy, resetPaging=TRUE, clearSelection = "row")
    reloadData(dt_proxy1, resetPaging=TRUE, clearSelection = "row")
  })

  output$dt_samples_comparison <- DT::renderDataTable({
    req(input$opt_numericCols)

    #filtered_clade_reads()
    dt <- DT::datatable(isolate({summarized_report_df()[[1]]}),
                        container=summarized_report_df()[[2]],
                        filter = "bottom",
                        escape = FALSE,
                        rownames = show_rownames,
                        selection = "single",
                        extensions = datatable_opts$extensions,
                        class=datatable_opts$class,
                        options = list(
                          stateSave = TRUE,
                          #columnDefs = get_columnDefs(summarized_report),
                          #autoWidth = TRUE,
                          buttons = common_buttons(base_set_name(), "matrix-view"),
                          drawCallback = drawCallback,
                          #order = my_order,
                          search = list(
                            search = isolate(dt_options$search)
                          )
                        ),
                          callback = JS('
              table.on("dblclick.dt","tr", function() {
              var data=table.row(this).data();
              alert("You clicked on "+data[4]+"\'s row");}
              )
              ')
    )
  })


}

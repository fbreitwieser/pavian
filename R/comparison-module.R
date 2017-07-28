taxRanks1 <- c(
  "Any Rank" = "-",
  "Domain" = "D",
  "Phylum" = "P",
  "Class" = "C",
  "Order" = "O",
  "Family" = "F",
  "Genus" = "G",
  "Species" = "S"
)

stat_name_to_f <- list(
  "Mean"=function(x) sum(x,na.rm=T)/length(x),
  "Median"=function(x) stats::median(na0(x)),
  "Max"=function(x) max(x, na.rm=T),
  "Min"=function(x) min(x, na.rm=T),
  "Sd"=sd,
  "MAD"=function(x) { x[is.na(x)] <- 0; stats::mad(x) },
  "Max Z-score"=function(x) { x[is.na(x)] <- 0; max( (x - stats::median(x))/max(1,stats::mad(x)) ) }
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

dropdown_options <- function(ns) {
  shiny::tagList(
    #div(class="col-lg-6 col-md-6 lessPadding",
    selectizeInput(
      ns('contaminant_selector'),
      allcontaminants, selected = c("unclassified", "Homo sapiens", "root"),
      label = "Filter taxon", multiple = TRUE,
      options = list(maxItems = 25, create = TRUE, placeholder = 'Filter taxa'),
      width = "100%"
      #)
    ),
    #div(class="col-lg-6 col-md-6 lessPadding",
    selectizeInput(
      ns('contaminant_selector_clade'),
      allcontaminants, selected = c("artificial sequences"),
      label = "Filter taxon and its children", multiple = TRUE,
      options = list( maxItems = 25, create = TRUE, placeholder = 'Filter taxa with children' ),
      width = "100%"
      #   )
    ),
    selectizeInput(ns("opt_statsColumns"), label = " Row summary", multiple = TRUE,
                   options = list(placeholder = "Add column(s) with summary statistics of taxa's data"),
                   choices = names(stat_name_to_f),
                   selected = c("Max")), 
    numericInput(ns("opt_min_scale_reads"), "Minimum scale for reads z-score", value = 1, min = 0),
    numericInput(ns("opt_min_scale_percent"), "Minimum scale for percent z-score", value = 0.001, min = 0),
    numericInput(ns("opt_min_clade_reads"), "Minimum number of reads to display", value = 1, min = 1, step = 1),
    checkboxInput(ns("opt_groupSamples"),"Group samples", value = TRUE)
    )
}


#taxRanks <- list(
#  "At taxon" = taxRanks,
#  "At taxon and children" = stats::setNames(paste("C",taxRanks),paste(names(taxRanks),"clade"))
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
    box(width=12,
        div(style="display:inline-block",
            shinyWidgets::radioGroupButtons(inputId = ns("opt_taxRank"), 
                                            label = NULL, choices = taxRanks1, status = "success")),
        div(style="display:inline-block",
            shinyWidgets::checkboxGroupButtons(inputId = ns("opt_numericColumns"),  label = NULL, 
				   choices = c("Reads"="cladeReads","%"="cladeReads %",
					       "Rank"="cladeReads rank","Z-score (reads)"="cladeReads z-score","Z-score (%)"="cladeReads % z-score"), justified = FALSE, 
                                               status = "primary",
                                               checkIcon = list(yes = icon("ok", lib = "glyphicon")), selected = "cladeReads")),
        div(style="display:inline-block", shinyWidgets::dropdownButton(dropdown_options(ns),#icon=icon("gear"),
                                                                       circle = FALSE, label = "more options ..."
                                                                       #,tooltip = shinyWidgets::tooltipOptions(title = "Click to see more options."
                                                                       )),
        htmlOutput(ns("messages")),
        div(id=ns("table_div"), style = 'overflow-x: scroll', DT::dataTableOutput(ns('dt_samples_comparison'))),
        downloadButton(ns('downloadData'), 'Download full table in tab-separated value format')
        #uiOutput(ns("filter_buttons"))
    ))
}

na0 <- function(x) {
  x[is.na(x)] <- 0
  x
}

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
  
  get_input <- function(x) {
    ni <- isolate(names(input))
    validate(need(x %in% ni, message = sprintf("Error: Expect %s in input! Available: %s", x, paste0(sort(ni), collapse = ", "))))
    input[[x]]
  }
  
  base_set_name <- reactive({
    validate(need(attr(sample_data(), "set_name"), message = "Attribute set_name not set for sample_data"))
    basename(attr(sample_data(), "set_name"))
  })
  
  output$messages <- renderUI({
    if (!any(grepl("clade", input$opt_numericColumns)) && input$opt_taxRank != "-") {
      return(tags$p("Warning: A specific taxonomic rank is selected, but data is not on the clade level. Thus any data from the taxa's children are not visible. Consider adding clade-level data."))
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { sprintf("%s-matrix-all-%s.tsv", base_set_name(), format(Sys.time(), "%y%m%d")) },
    content = function(file) {
      utils::write.table(r_summarized_report(), file, row.names = FALSE, sep = "\t")
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
  
  na_false <- function(x) {
    x[is.na(x)] <- FALSE
    x
  }
  
  shown_rows <- reactive({
    fmax <- !apply(is.na(filtered_clade_reads()),1,all) & apply(filtered_clade_reads(),1,max,na.rm=T) >= get_input("opt_min_clade_reads")
    res <- filter_taxa(tax_data(),
                rm_clades = input$contaminant_selector_clade,
                rm_taxa = input$contaminant_selector,
                taxRank = input$opt_taxRank) & fmax %>% na_false
    stopifnot(all(!is.na(res)))
    print(summary(res))
    res
  })
  
  filtered_clade_reads <- reactive({
    filter_cladeReads(clade_reads(), tax_data(), 
                      rm_taxa = c(input$contaminant_selector,input$contaminant_selector_clade)) %>%
      shinyTryCatch(message="filtering clade reads")
  })
  
  get_tax_columns <- reactive({
    tax_columns <- data.frame(Name=get_col(tax_data(),"name"), stringsAsFactors = FALSE)
    if ("taxID" %in% colnames(tax_data())) {
      tax_columns$TaxID=tax_data()[,"taxID"]
    }
    
    ## Add columns from tax_data_add
    if (!is.null(tax_data_add)) {
      validate(
        need(is.data.frame(tax_data_add), message="tax_data_add is no data.frame"),
        need(ncol(tax_data_add) > 1, message="Need more than two columns in tax_data_add"),
        need("TaxID" %in% colnames(tax_columns), message="TaxID not available"))
      
      tax_ids <- as.character(tax_columns[,"TaxID"])
      upd_rows1 <- tax_ids[!tax_ids %in% rownames(tax_data_add)]
      tax_data_add[upd_rows1,] <- NA
      tax_columns <- cbind(tax_columns, tax_data_add[tax_ids, ])
    }
    tax_columns
  })
  
  numericColumns <- reactive({
    #paste0("clade",input$opt_numericColumns)
    input$opt_numericColumns
  })
  
  summarized_report_df <- reactive({
    req(input$opt_numericColumns)
    
    sel_rows <- shown_rows()
    one_df(filtered_clade_reads()[sel_rows,,drop=F], taxon_reads()[sel_rows,,drop=F], tax_data()[sel_rows,,drop=F], 
           sample_data(),
           numericColumns = numericColumns(), statsColumns = input$opt_statsColumns, sum_reads = NULL,
           groupSampleColumns = input$opt_groupSamples, specific_tax_rank = input$opt_taxRank != "-",
           min_scale_reads = get_input("opt_min_scale_reads"), min_scale_percent = get_input("opt_min_scale_percent"),
           min_clade_reads = get_input("opt_min_clade_reads"))
  })
  
  ## Different DT version handle namespaces differently. Make sure it works in all of them.
  dt_proxy <- DT::dataTableProxy(session$ns('dt_samples_comparison'))
  dt_proxy1 <- DT::dataTableProxy('dt_samples_comparison')
  observe({
    ## replaceData does not work with modules, currently
    ##  see https://github.com/rstudio/DT/issues/359
    req(summarized_report_df())
    DT::dataTableAjax(session, summarized_report_df()[[1]], rownames = FALSE, outputId = 'dt_samples_comparison')
    DT::dataTableAjax(session, summarized_report_df()[[1]], rownames = FALSE,
                      outputId = session$ns('dt_samples_comparison'))
    DT::reloadData(dt_proxy, resetPaging=TRUE, clearSelection = "row")
    DT::reloadData(dt_proxy1, resetPaging=TRUE, clearSelection = "row")
  })
  
  
  output$dt_samples_comparison <- DT::renderDataTable({
    ## require columns that update the number of columns in the table
    req(input$opt_numericColumns)
    input$opt_groupSamples
    input$opt_statsColumns
    
    dataframe_container_and_formatFunction <- isolate({summarized_report_df()})
    myDf <- dataframe_container_and_formatFunction[[1]]
    myContainer <- dataframe_container_and_formatFunction[[2]]
    
    #filtered_clade_reads()
    DT::datatable(myDf,
                  container=myContainer,
                  filter = "bottom", selection = "single", escape = FALSE,
                  rownames = show_rownames,
                  extensions = datatable_opts$extensions,
                  class=datatable_opts$class,
                  options = list(
                    stateSave = TRUE,
                    columnDefs = get_columnDefs(myDf, nColumnsBefore = 2, nColumnsAfter = 1),
                    #autoWidth = TRUE,
                    buttons = common_buttons(base_set_name(), "matrix-view"),
                    drawCallback = drawCallback,
                    #order = my_order,
                    rowCallback = htmlwidgets::JS(
                      "function(row, data, index) {",
                      "var full_text = 'Full path: ' + data[data.length - 1]",
                      "$('td:eq(0)', row).attr('title', full_text);",
                      "}"),
                    search = list(
                      search = isolate(dt_options$search)
                    )
                  ),
                  callback = htmlwidgets::JS('
              table.on("dblclick.dt","tr", function() {
              var data=table.row(this).data();
              alert("You clicked on "+data[4]+"\'s row");}
              )
              ')
    ) %>% formatDT(nSamples = nrow(sample_data())+length(input$opt_statsColumns),
                   numericColumns = numericColumns(),
                   nColumnsBefore = ncol(tax_data()) - 1,
                   groupSampleColumns = input$opt_groupSamples)
  })
  
  get_columnDefs <- function(myDf, nColumnsBefore, nColumnsAfter) {
    zero_col <- ifelse(show_rownames, 0, 1)
    
    columnDefs <- list(
      list(targets = seq(from=nColumnsBefore, to=ncol(myDf)-nColumnsAfter-1), orderSequence = c('desc', 'asc'), searchable = FALSE)  ## Data columns shouldn't be searchable
    )
    if ('taxID' %in% colnames(myDf)) {
      columnDefs[length(columnDefs) + 1] <-
        list(list(targets = which(colnames(myDf) == "taxID") - zero_col,
                  render = htmlwidgets::JS("function(data, type, full) {
                return '<a href=\"https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id='+data+'\" target=\"_blank\">' + data + '</a>'; }")
        ))
    }
    
    columnDefs
  }
  
  
  formatDT <- function(dt, nSamples, numericColumns, nColumnsBefore, groupSampleColumns = TRUE) {
    ## Give the correct format to the columns: thousands separators for numbers, and percent sign for percents
    ## numericColumns ending with "Reads" are Integer
    
    by_seq <- ifelse(isTRUE(groupSampleColumns), length(numericColumns), 1)
    col_seq <- function(i) {
      if (isTRUE(groupSampleColumns))
        seq(i, by=length(numericColumns), length.out=nSamples) + nColumnsBefore
      else
        seq((i-1)*nSamples+1, by=1, length.out=nSamples) + nColumnsBefore
    }
    
    extendColumns <- function(columns) {
      lapply(columns, col_seq) %>% unlist %>% sort
    }
    
    integerColumns <- grep("Reads$", numericColumns)
    if (length(integerColumns) > 0)
      dt <- dt %>% DT::formatCurrency(integerColumns %>% extendColumns, currency = '', digits = 0 )
    
    percentColumns <- grep("%$", numericColumns)
    if (length(percentColumns) > 0)
      dt <- dt %>% DT::formatString(percentColumns %>% extendColumns, suffix = "%")
    
    #numericColumns <- setdiff(seq(from=0, to=nc*nSamples), c(integerColumns, percentColumns))
    #if (length(numericColumns) > 0)
    #   ## format with X digits after comma
    
    if (0) {
      ## Add color bar (heatmap colors instead)
      dt <- dt %>% DT::formatStyle(
        attr(summarized_report, 'data_columns'),
        background = DT::styleColorBar(range(summarized_report[,attr(summarized_report, 'stat_column')],na.rm=TRUE), 'lightblue'),
        backgroundSize = '90% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    }
    
    cols <- c("255,0,0","0,255,0","0,0,255")
    cols1 <- colorspace::rainbow_hcl(8, start=30,end=300)
    cols1 <- RColorBrewer::brewer.pal(8, "Set1")
    cols <- apply(colorspace::hex2RGB(cols1)@coords*255,1,paste,collapse=",")
    cols <- c(cols,cols,cols,cols)
    
    for (i in seq_along(numericColumns)) {
      columns <- col_seq(i)
      #rg <- range(dt[[1]]$data[,columns], na.rm = TRUE)
      #brks <- seq(from=rg[1],to=rg[2], length.out=20)
      brks <- unique(stats::quantile(dt[[1]]$data[,columns], probs = cumsum(1/2^(1:20)), na.rm =TRUE))
      clrs <- round(seq(0, 1, length.out = length(brks) + 1),4) %>% {sprintf("rgba(%s,%s)", cols[i],.)}
      dt <- dt %>% DT::formatStyle(columns, backgroundColor = DT::styleInterval(brks, clrs))
    }
    
    dt
  }
  
}

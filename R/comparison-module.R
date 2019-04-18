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
    shinyjs::hidden(
    selectizeInput(
      ns('contaminant_selector'),
      allcontaminants,
      label = "Filter taxon", multiple = TRUE,
      options = list(maxItems = 25, create = TRUE, placeholder = 'Filter taxa'),
      width = "100%"
      #)
    )),
    #div(class="col-lg-6 col-md-6 lessPadding",
    checkboxGroupInput(ns("opt_statsColumns"), label = " Row summary", #multiple = TRUE,
                   #options = list(placeholder = "Add column(s) with summary statistics of taxa's data"),
                   choices = names(stat_name_to_f),
                   selected = c("Max"),inline=TRUE), 
    numericInput(ns("opt_min_scale_reads"), "Minimum scale for reads z-score", value = 1, min = 0),
    numericInput(ns("opt_min_scale_percent"), "Minimum scale for percent z-score", value = 0.001, min = 0),
    numericInput(ns("opt_min_clade_reads"), "Hide clades with less than X reads to taxon or children", value = 1, min = 1, step = 1)
    #numericInput(ns("opt_min_taxon_reads"), "Hide taxa with less than X reads", value = 1, min = 0, step = 1),
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
  uiOutput(ns("UI"))
}

comparisonModuleUI_function <- function(ns) {
  shiny::tagList(
    box(width=12,
        div(style="display:inline-block",
            shinyWidgets::radioGroupButtons(inputId = ns("opt_taxRank"), 
                                            label = NULL, choices = taxRanks1, status = "success")),
        div(style="display:inline-block",
            shinyWidgets::checkboxGroupButtons(inputId = ns("opt_numericColumns2"),  label = NULL, 
				   choices = c("Reads"="identity", "%"="%",
					       "Rank"="rank","Z-score (reads)"="z-score","Z-score (%)"="% z-score"), 
                                               justified = FALSE, 
                                               status = "primary",
                                               checkIcon = list(yes = icon("ok", lib = "glyphicon")), 
                                selected = "identity")),
        div(style="display:inline-block",
            shinyWidgets::checkboxGroupButtons(inputId = ns("opt_numericColumns1"), 
                label = NULL, choices = c("Clade"="cladeReads", "Taxon"="taxonReads"), 
                selected = "cladeReads",
                status = "warning")),
        div(style="display:inline-block",selectizeInput(
            ns('contaminant_selector_clade'),
            allcontaminants, #selected = c("artificial sequences"),
            label = NULL, multiple = TRUE,
            options = list( maxItems = 25, create = TRUE, placeholder = 'Filter taxa' ),
            width = "100%")), 
        div(style="display:inline-block",title="Filter clade of selected taxon (click on a table row, first)",
            actionButton(ns("btn_filter_row"), label=NULL, icon=icon("filter"))),
        div(style="display:inline-block", shinyWidgets::dropdownButton(dropdown_options(ns),#icon=icon("gear"),
                                                                       circle = FALSE, label = "more options ..."
                                                                       #,tooltip = shinyWidgets::tooltipOptions(title = "Click to see more options."
                                                                       )),
        div(style="display:inline-block", 
            shinyWidgets::prettySwitch(ns("opt_hide_zero_taxa"),
                                       "Collapse taxa with no reads", value=T, slim=T)),
        div(style="display:inline-block", shinyjs::hidden(
            shinyWidgets::prettySwitch(ns("opt_groupSamples"),"Group samples", value = FALSE, slim=T))),
        htmlOutput(ns("messages")),
        htmlOutput(ns("taxLineage")),
        div(id=ns("table_div"), style = 'overflow-x: scroll', DT::dataTableOutput(ns('dt_samples_comparison'))),
        shinyWidgets::searchInput(ns("search_filter_freetext"), label=NULL, placeholder = "Filter data with formula, e.g $2 > $1",
                                  btnSearch = icon("filter"), btnReset = icon("remove")),
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
  
  output$UI <- renderUI({
    req(sample_data())
    comparisonModuleUI_function(session$ns)
  })
  
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
  
  taxLineage <- reactiveValues(val=NULL)

  output$taxLineage <- renderUI({
    req(taxLineage$val)

    res <- paste(actionLink(session$ns("btn_x_lin"),"[x]"), " ")
    if (length(taxLineage$val) > 1) {
      vali <- taxLineage$val[1:(length(taxLineage$val)-1)]
      valii <- lapply(seq_along(vali), function(i) paste0(actionLink(session$ns(paste0("btn_lin_",i)), vali[i]), "><wbr>"))
      res <- do.call(paste0,c(res,valii))
    }
    return (HTML(paste0(res, taxLineage$val[length(taxLineage$val)])))
  })

  output$messages <- renderUI({
    req(numericColumns1())
    if (!any(grepl("clade", numericColumns1())) && input$opt_taxRank != "-") {
      return(tags$p("Warning: A specific taxonomic rank is selected, but data is not on the clade level. Thus any data from the taxa's children are not visible. Consider adding clade-level data."))
    }
  })

  observeEvent(input$btn_x_lin, { taxLineage$val <- NULL } )

  observeEvent(input$btn_lin_1, { taxLineage$val <- taxLineage$val[1] })
  observeEvent(input$btn_lin_2, { taxLineage$val <- taxLineage$val[1:2] })
  observeEvent(input$btn_lin_3, { taxLineage$val <- taxLineage$val[1:3] })
  observeEvent(input$btn_lin_4, { taxLineage$val <- taxLineage$val[1:4] })
  observeEvent(input$btn_lin_5, { taxLineage$val <- taxLineage$val[1:5] })
  observeEvent(input$btn_lin_6, { taxLineage$val <- taxLineage$val[1:6] })
  observeEvent(input$btn_lin_7, { taxLineage$val <- taxLineage$val[1:7] })
  observeEvent(input$btn_lin_8, { taxLineage$val <- taxLineage$val[1:8] })
  observeEvent(input$btn_lin_9, { taxLineage$val <- taxLineage$val[1:9] })
  observeEvent(input$btn_lin_10, { taxLineage$val <- taxLineage$val[1:10] })
  observeEvent(input$btn_lin_11, { taxLineage$val <- taxLineage$val[1:11] })
  observeEvent(input$btn_lin_12, { taxLineage$val <- taxLineage$val[1:12] })
  observeEvent(input$btn_lin_13, { taxLineage$val <- taxLineage$val[1:13] })
  observeEvent(input$btn_lin_14, { taxLineage$val <- taxLineage$val[1:14] })
  observeEvent(input$btn_lin_15, { taxLineage$val <- taxLineage$val[1:15] })
  observeEvent(input$btn_lin_16, { taxLineage$val <- taxLineage$val[1:16] })
  observeEvent(input$btn_lin_17, { taxLineage$val <- taxLineage$val[1:17] })
  observeEvent(input$btn_lin_18, { taxLineage$val <- taxLineage$val[1:18] })
  observeEvent(input$btn_lin_19, { taxLineage$val <- taxLineage$val[1:19] })
  observeEvent(input$btn_lin_20, { taxLineage$val <- taxLineage$val[1:20] })
  observeEvent(input$btn_lin_21, { taxLineage$val <- taxLineage$val[1:21] })
  
  output$downloadData <- downloadHandler(
    filename = function() { sprintf("%s-matrix-all-%s.tsv", base_set_name(), format(Sys.time(), "%y%m%d")) },
    content = function(file) {
      utils::write.table(summarized_report_for_download(), file, row.names = FALSE, sep = "\t")
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

  my_tax_data <- reactive({
    td <- tax_data()
    cur_names = list()
    taxReads0 = apply(is.na(taxon_reads()) | taxon_reads() == 0, 1, all)
    for (i in seq_len(nrow(td))) {
      tl = td[i, "taxLineage"]
      tparent = sub("\\|[^\\|]*$", "", tl)
      cur_name <- cur_names[[tparent]]
      if (taxReads0[i]) {
        if (!is.null(cur_name))
          cur_names[[tl]] = c(cur_name, td[i, "name"])
        else
          cur_names[[tl]] = td[i, "name"]
      } else {
        if (length(cur_name) > 0) {
          if (length(cur_name) > 2) 
            td[i,"name"] <- paste(cur_name[1], "...", td[i,"name"], sep="><wbr>")
          else
            td[i,"name"] <- paste(c(cur_name,td[i,"name"]), collapse="><wbr>")
        }
      }
    }
    td
  })

  rm_taxons <- reactive({
    tax_data()$name %in% input$contaminant_selector_clade | tax_data()$name %in% input$contaminant_selector
  })
  
  na_false <- function(x) {
    x[is.na(x)] <- FALSE
    x
  }
  
  get_filter_string <- function(filter_string, name, max_col = NULL) {
    if (!is.null(max_col)) {
      for (i in 1:max_col)
        filter_string <- gsub(paste0("$",i), sprintf("%s[,%s]", name, i), filter_string, fixed=F)
    } else {
      filter_string <- gsub("\\$([0-9]+)", paste0(name,"[,\\1]"), filter_string, fixed=F)
    }
    message(filter_string)
    filter_string
  }
  
  filter_df_with_string <- function(dataframe, filter_string) {
    filter_string <- get_filter_string(filter_string, "dataframe")
    message(filter_string)
    tryCatch(
      eval(parse(text =  filter_string)),
      error = function(e) { message(e) }
    )
  }
  
  
  shown_rows <- reactive({
    clade_reads <- filtered_clade_reads()
    res <- !apply(is.na(clade_reads),1,all) &
           filter_taxa(tax_data(),
                rm_clades = input$contaminant_selector_clade,
                rm_taxa = input$contaminant_selector,
                taxRank = input$opt_taxRank) 
    if (!is.null(taxLineage$val)) {
      sel_tl = tax_data()[tax_data()[["name"]] == taxLineage$val[length(taxLineage$val)], "taxLineage"][1]
      dmessage("Selected lineage ", sel_tl)
      if (isTRUE(!is.null(sel_tl) && nchar(sel_tl) > 0))
        res <- res & substr(tax_data()$taxLineage, 0, nchar(sel_tl)) == sel_tl

    }
    #if (input$opt_taxRank == "-" && input$opt_min_taxon_reads > 0) {
    #  res <- res & apply(taxon_reads(),1,max,na.rm=T) >= get_input("opt_min_taxon_reads")
    #}
    if (input$opt_taxRank == "-" && input$opt_hide_zero_taxa) {
      res <- res & apply(taxon_reads(),1,max,na.rm=T) >= 1
    }
    if (input$opt_min_clade_reads > 0) {
      res <- res & apply(clade_reads,1,max,na.rm=T) >= get_input("opt_min_clade_reads")
      
    }
    if (!is.null(input$search_filter_freetext) && nchar(input$search_filter_freetext) > 4 && grepl("$", input$search_filter_freetext, fixed=T)) {
      ## TODO: Try-catch doesn't catch errors - why??
      tryCatch({
          res <- res & eval(parse(text=get_filter_string(input$search_filter_freetext, "clade_reads")))
        },
        error = function(e) message(e)
      )
    }
    res <- res %>% na_false
    #validate(need(any(!is.na(res)), message = "Filtered all rows to NA!"),
    #         need(sum(res,na.rm=T)>0, message = "Filtered all data."))
    res
  })
  
  filtered_clade_reads <- reactive({
    filter_cladeReads(clade_reads(), tax_data(), 
                      rm_taxa = c(input$contaminant_selector,input$contaminant_selector_clade)) %>%
      shinyTryCatch(message="filtering clade reads")
  })
  
  get_tax_columns <- reactive({
    tax_columns <- data.frame(Name=get_col(my_tax_data(),"name"), stringsAsFactors = FALSE)
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
    req(numericColumns1())
    req(numericColumns2())
    paste(rep(numericColumns1(), times=length(numericColumns2())),
          rep(numericColumns2(), each=length(numericColumns1())))
  })

  summarized_report_df <- reactive({
    req(numericColumns())
    
    sel_rows <- shown_rows()
    validate(need(any(sel_rows,na.rm=T), message = "Filtered all data"))
    #message(sum(sel_rows, na.rm=T))
    td <- tax_data()[sel_rows,,drop=F]
    #if (input$opt_taxRank == "-" && input$opt_min_taxon_reads > 0) {
    if (input$opt_taxRank == "-" && input$opt_hide_zero_taxa) {
      td <- my_tax_data()[sel_rows,,drop=F]
    }
    one_df(filtered_clade_reads()[sel_rows,,drop=F], taxon_reads()[sel_rows,,drop=F], td, 
           sample_data(),
           numericColumns = numericColumns(), statsColumns = input$opt_statsColumns, sum_reads = NULL,
           groupSampleColumns = input$opt_groupSamples, specific_tax_rank = input$opt_taxRank != "-",
           min_scale_reads = get_input("opt_min_scale_reads"), min_scale_percent = get_input("opt_min_scale_percent"))
  })
  
  summarized_report_for_download <- reactive({
    summarized_report <- summarized_report_df()
    req(summarized_report)
    summarized_report[[1]]
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

  observeEvent(input$btn_filter_row,  {
    current_selected <- input$contaminant_selector_clade
    sel_row <- input$dt_samples_comparison_rows_selected
    if (is.null(sel_row) || length(sel_row) == 0) {
      shinyjs::info("Either write a taxon name directly in the box to the left to filter its clade, or select a row in the table and press this button again.")
    } else {
      sel_rows <- shown_rows()
      sel_name <- tax_data()[which(sel_rows)[sel_row],"name"]
      dmessage("Filtering ",sel_name)
      req(sel_name)

      if (!sel_name %in% current_selected)
        updateSelectizeInput(session, "contaminant_selector_clade", selected = c(current_selected, sel_name), choices = c(current_selected, sel_name))
    }
  })

  observeEvent(input$contaminant_selector_clade, {
  })

  numericColumns1 <- reactive({
    if (is.null(input$opt_numericColumns1)) {
        shinyWidgets::updateCheckboxGroupButtons(session, ("opt_numericColumns1"), selected = "cladeReads")
    }
    input$opt_numericColumns1
  })

  numericColumns2 <- reactive({
    if (is.null(input$opt_numericColumns2)) {
        shinyWidgets::updateCheckboxGroupButtons(session, ("opt_numericColumns2"), selected = "identity")
    }
    input$opt_numericColumns2
  })
  
  output$dt_samples_comparison <- DT::renderDataTable({
    req(numericColumns())
    ## require columns that update the number of columns in the table
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
                      "var full_text = 'Lineage: ' + data[data.length - 1].replace(/&nbsp;/g,' ')",
                      "$('td:eq(0)', row).attr('title', full_text);",
                      "}"),
                    search = list(
                      search = isolate(dt_options$search)
                    )
                  ),
                  callback = htmlwidgets::JS('
              table.on("dblclick.dt","tr", function() {
              var data=table.row(this).data();
              var data1=data[0].replace(/.*>/,"")
              Shiny.onInputChange("comparison-double_clicked_row", data[data.length - 1] + ">" +data1)
              //alert("You clicked on "+data[0]+"\'s row");
              }
              )
              ')
    ) %>% formatDT(nSamples = nrow(sample_data()),
                   numericColumns = numericColumns(),
                   statsColumns = input$opt_statsColumns,
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
                  render = htmlwidgets::JS("function(data, type, row) {
                if (data < 1000000000) {
                  return '<a href=\"https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id='+data+'\" target=\"_blank\">' + data + '</a>'; 
                } else {
                  name = row[0]
                  name = name.replace(/.*>/,'')
                  name = name.replace(/ .*/,'')
                  if (name.startsWith('GCA_') || name.startsWith('GCF_')) {
                    return '<a href=\"https://www.ncbi.nlm.nih.gov/assembly/'+name+'\" target=\"_blank\">asm</a>'; 
                  } else if (name.startsWith('NC') || name.match(/^[A-Z]{1,2}[0-9]{5,6}/)) {
                    return '<a href=\"https://www.ncbi.nlm.nih.gov/nuccore/'+name+'\" target=\"_blank\">nuc</a>'; 
                  }
                  return '';
                }
              }")
        ))
    }
    
    columnDefs
  }

  observeEvent(input$opt_taxRank, {
    if (input$opt_taxRank == "-") {
      shinyjs::show("opt_hide_zero_taxa")
    } else {
      shinyjs::hide("opt_hide_zero_taxa")
    }
  })


  observeEvent(numericColumns(), {
    if (length(numericColumns()) > 1) {
      shinyjs::show("opt_groupSamples")
    } else {
      shinyjs::hide("opt_groupSamples")
    }
  })

  observeEvent(input$double_clicked_row, {
    #dmessage("double-clicked a row!")
    #dmessage(input$double_clicked_row)
    shinyWidgets::updateRadioGroupButtons(session, "opt_taxRank", selected = "-")
    #search = sub(".*<wbr>","", input$double_clicked_row)
    search = gsub("&nbsp;"," ", input$double_clicked_row)
    dmessage("Search is now: ",search)
    #search = gsub(">","><wbr>", search)
    taxLineage$val <- strsplit(search, ">")[[1]]
    #DT::updateSearch(dt_proxy, keywords = list(global=search))
    #DT::updateSearch(dt_proxy1, keywords = list(global=search))
  })
  
  formatDT <- function(dt, nSamples, numericColumns, statsColumns, nColumnsBefore, groupSampleColumns = TRUE) {
    ## Give the correct format to the columns: thousands separators for numbers, and percent sign for percents
    ## numericColumns ending with "Reads" are Integer
    nSSamples = nSamples + length(statsColumns)
    by_seq <- ifelse(isTRUE(groupSampleColumns), length(numericColumns), 1)
    col_seq <- function(i) {
      if (isTRUE(groupSampleColumns))
        #seq(i, by=length(numericColumns), length.out=nSamples) + nColumnsBefore + length(numericColumns)*length(statsColumns)
        # show colors when grouping samples
	seq(i, by=length(numericColumns), length.out=nSamples) + nColumnsBefore + length(numericColumns)*length(statsColumns)
      else
        seq((i-1)*nSamples+1+length(statsColumns)*i, by=1, length.out=nSamples) + nColumnsBefore
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

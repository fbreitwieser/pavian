

# Common datatable options. It's reactive so that we can add the compact class to tables on demand
datatable_opts <- reactiveValues(
    rownames = FALSE,
    selection = 'single',
    extensions = c('Buttons'),
    # Maybe use 'Scroller' for some tables
    # 'ColReorder' isn't really useful
    # 'FixedColumns' is too buggy
    # Consider adding 'Responsive' / see https://datatables.net/extensions/responsive/priority
    class = "stripe hover row-border"
  )


#' Pavian server function
#'
#' @param input Input object
#' @param output Output object
#' @param session Session object
#' @export
pavianServer <- function(input, output, session) {
  
  # The cache directory stores Rds files for read reports
  #cache_dir <- tempdir()
  cache_dir <- NULL

  ## Global Pavian options
  pavian_options <- reactiveValues(server_dir = getOption("pavian.server_dir"))

  pID <- session$token
  ID <- getOption("pavian.session_count",0) +1
  options(pavian.session_count=ID)
  options(pavian.running_sessions=getOption("pavian.running_sessions")+1)
  dmessage("Started new shiny session #",ID, " (",getOption("pavian.running_sessions",0), " session(s) running)")
  
  onSessionEnded(function(...) {
    dmessage("Exiting session #", ID)
    options(pavian.running_sessions=getOption("pavian.running_sessions")-1)
  }, session = getDefaultReactiveDomain())

  
  ## Observe URL parameters and set pavian options accordingly
  observeEvent(session$clientData$url_search, {
    ## TODO: Implement back/forward navigation and tab selection 
    ## https://github.com/daattali/advanced-shiny/blob/master/navigate-history/app.R
    ## or with bookmarking
    req(nchar(session$clientData$url_search) > 1)
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['server.dir']])) {
      dmessage("Setting server directory to ",query[['server.dir']]," (specified in URL).")
      pavian_options$server_dir = query[['server.dir']]
    }
    if (!is.null(query[['load.dir']])) {
      dmessage("Loading server directory (specified in URL).")
      options(pavian.load_server_dir = TRUE)
    }
    if (!is.null(query[['page']])) {
      dmessage("Setting page to ",query[['page']]," (specified in URL).")
      updateTabItems(session, "tabs", selected = query[['page']])
    }
    if (!is.null(query[['load_example_data']])) {
      dmessage("Loading example data (specified in URL).")
      options(pavian.load_example_data = TRUE)
    }
  })
  
  # Trigger bookmarking
  setBookmarkExclude("bookmark_btn")
  observeEvent(input$bookmark_btn, {
    session$doBookmark()
  })
  
  # Compact styling for datatables, and in the future maybe more elements
  observeEvent(input$compact_format, {
    if (isTRUE(input$compact_format)) {
      #shinyjs::addClass(class="lineheight1", selector = "body")
      datatable_opts$class <- "stripe hover row-border compact"
    } else {
      #shinyjs::removeClass(class="lineheight1", selector = "body")
      datatable_opts$class <- "stripe hover row-border"
    }
  })
  
  ######################
  ## Data input module
  sample_sets <- callModule(dataInputModule, "datafile", pavian_options=pavian_options)
  observeEvent(sample_sets$val,{
    if (length(sample_sets$val) > 0) {
      sample_set_names <- sort(names(sample_sets$val))
      #sample_set_names["Upload data ..."] <- "upload_files"
      #shinyjs::enable("btn_remove_cache_files")
      shinyjs::show("sample_set_names")
      code <- sprintf("$('#sample_set_names').attr('size', %s)", min(length(sample_set_names), 5))
      shinyjs::runjs(code)
      updateSelectInput(session, "sample_set_names", choices = sample_set_names, selected = sample_sets$selected)
    } else {
      updateSelectInput(session, "sample_set_names", choices = character(0), selected= character(0))
      code <- sprintf("$('span.logo').text('')")
      shinyjs::runjs(code)
      shinyjs::hide("sample_set_names")
      #shinyjs::disable("btn_remove_cache_files")
    }
  })

  
  ##########################
  ## Render dynamic menus
  output$dynamic_sidebar_menu_overview <- shinydashboard::renderMenu({
    req(sample_sets$val)
    shinydashboard::menuItem("Results Overview", tabName="Overview", icon = icon("table"))
  })
  
  output$dynamic_sidebar_menu_sample <- shinydashboard::renderMenu({
    req(sample_sets$val)
    shinydashboard::menuItem("Sample", tabName="Sample", icon = icon("sun-o"))
  })
  
  output$dynamic_sidebar_menu_comparison <- shinydashboard::renderMenu({
    req(sample_sets$val)
    shiny::tagList(
      shinydashboard::menuItem("Comparison", icon = icon("line-chart"), tabName = "Alldata",
                               shinydashboard::menuSubItem("All data", tabName="Comparison"),
                               shinydashboard::menuSubItem("Bacteria and Archaea", tabName="Bacteria"),
                               shinydashboard::menuSubItem("Viruses", tabName="Viruses"),
                               shinydashboard::menuSubItem("Eukaryotes", tabName="Eukaryotes"),
                               shinydashboard::menuSubItem("Eukaryotes/Fungi", tabName="Fungi"),
                               shinydashboard::menuSubItem("Eukaryotes/Protists", tabName="Protists")
      )
    )
  })
  
  sample_set_names_combined_str <- reactive({
    paste(input$sample_set_names, collapse=" & ")
  })
  
  sample_set_names_combined <- reactive({
    res <- sapply(input$sample_set_names, basename)
    res <- paste(res, collapse="_")
    res <- gsub("[^A-Za-z\\-_]","_", res)
    if (nchar(res) == 0){
      return("Set1")
    } else {
      return(res)
    }
  })
  
  output$session_info <- renderPrint({
    sessionInfo()
  })
  
  output$session_info1 <- renderPrint({
    #str(session$clientData)
  })
  
  
  observeEvent(input$sample_set_names,{
    if (isTRUE(input$sample_set_names[1] == "upload_files")) {
      shinydashboard::updateTabItems(session,"tabs","Data Selection")
    } else {
      req(reports())
      #updateTabItems(session,"tabs","Overview")
      code <- sprintf("$('span.logo').text('%s')",sample_set_names_combined_str())
      shinyjs::runjs(code)
    }
  })
  
  ## sample data for selected sample set
  ## TODO: Change to reactiveValues (or use makeReactiveBinding?) at some point in time,
  ## then I can call it in observeEvent
  sample_data <- reactive({
    validate(need(sample_sets$val, message="Upload samples or select sample set."))
    validate(need(input$sample_set_names, message="Upload samples or select sample set."))
    res <- sample_sets$val[[input$sample_set_names[1]]]
    if (length(input$sample_set_names) > 1) {
      for (set_name in input$sample_set_names[2:length(input$sample_set_names)]) {
        cols <- intersect(colnames(res), colnames(sample_sets$val[[set_name]]))
        res <- rbind(res[,cols], sample_sets$val[[set_name]][, cols])
      }
    }
    #res <- isolate(sample_sets$val)[[input$sample_set_names]]
    if ("Include" %in% colnames(res)) {
      res <- res[res$Include, ]
    }
    attr(res, "set_name") <- sample_set_names_combined()
    res
  })
  
  output$bookmarkBtnUI <- renderUI ({
    req(input$sample_set_names)
    shiny::tagList(
      bookmarkButton(id="bookmark_btn",label = "Bookmark state ...", title = "Bookmark this Pavian's state and get a URL for sharing."),
      actionLink("link_generate_report", "Generate HTML report ..."),
      shinyjs::hidden(checkboxInput("compact_format", "Compact tables", value = T))
    )
  })
  
  ## contains the classification results ('reports') of the selected sample set
  reports <- reactive({
    validate(need(sample_data(), message="Upload samples or select sample set."))
    validate(need(length(sample_data()) > 0, message="Upload samples or select sample set."))
    validate(
      need("ReportFilePath" %in% colnames(sample_data()), "ReportFilePath not available!"),
      need("Name" %in% colnames(sample_data()), "Name not available!")
    )
    res <- read_reports(sample_data()$ReportFilePath, sample_data()$Name, cache_dir = cache_dir)
    if ("LibrarySize" %in% colnames(sample_data())) {
      dmessage("Getting lib size from sample data")
      attr(res,"library_size") <- sample_data()$LibrarySize
    } else {
      attr(res,"library_size") <- sapply(res, function(x) sum(x$taxonReads))
    }
    validate(need(length(res) > 0, message = "There are no valid reports in this sample set!"))
    res
  })
  
  ############################
  ## Results overview module
  overview_res <- callModule(reportOverviewModule, "overview", sample_data, reports, datatable_opts = datatable_opts)
  observeEvent(overview_res$selected_sample,{
    req(overview_res$selected_sample)
    print("Switching to Sample tab")
    updateTabItems(session, "tabs", selected = "Sample")
  })
  
  observeEvent(overview_res$go_to_sample_comparison,{
    req(overview_res$go_to_sample_comparison)
    print("Switching to Comparison tab")
    updateTabItems(session, "tabs", selected = "Comparison")
  })
  
  ##################
  ## Sample module
  selected_sample <- reactive({overview_res$selected_sample})
  callModule(sampleModule, "sample", sample_data, reports,
             tax_data, clade_reads, taxon_reads,
             selected_sample = selected_sample, datatable_opts=datatable_opts)
  
  
  ######################
  ## Comparison module
  summarized_report <- reactive({
    withProgress(message="Merging samples reports.",
                 detail = "This may take a while ...", 
                 max = length(reports()), { 
      merge_reports2(reports(), col_names = sample_data()[["Name"]], update_progress=T) })
  })
  tax_data <- reactive({ summarized_report()[[1]] })
  clade_reads <- reactive({ summarized_report()[[2]] })
  taxon_reads <- reactive({ summarized_report()[[3]] })
  
  callModule(comparisonModule, "comparison", sample_data, tax_data, clade_reads, taxon_reads,
             reports, datatable_opts = datatable_opts)#, search = sample_module_selected)
  
  #####################
  ## Alignment module
  callModule(alignmentModule, "alignment", sample_data, datatable_opts = datatable_opts)
  
  #####################
  ## Generate report
  generate_report_modal <- function() {
    ns <- session$ns
    if (rmarkdown::pandoc_available("1.12.3")) {
      modalDialog(
        title="Generate sample report",
        textInput(ns("report_title"), "Title", sprintf("Classification report for %s",sample_set_names_combined_str()), width="100%"),
        textInput(ns("report_author"), "Author", sprintf("Pavian R package v%s", utils::packageVersion("pavian")), width="100%"),
        textInput(ns("report_date"), "Date", date(), width="100%"),
        checkboxInput(ns("report_include_sankey"),"Include sankey flow charts for each sample", value = TRUE),
        selectizeInput(ns("report_filter_taxa"), "Filter taxa in sankey", selected=c("Chordata","artificial sequences"), choices=allcontaminants,multiple=TRUE, options(create=TRUE)),
        footer = tagList(
          modalButton("Cancel"),
          downloadButton("dl_report", "Generate HTML report")
        )
      )
    } else {
      modalDialog(titel="Pandoc was not found",
                  "A recent version of pandoc (>= 1.12.3) is required for generating HTML reports. See the ",
                  a(href="https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md","pandoc installation instructions", target="_blank"), 
                  " for details on installing pandoc on your platform"
                  )
    }
  }
  
  
  
  observeEvent(input$link_generate_report, {
    showModal(generate_report_modal())
  })
  output$dl_report <- downloadHandler(
    filename = function() { sprintf("%s-report.html", sample_set_names_combined()) },
    content = function(file) {
      req(input$sample_set_names)
      rmd_file <- system.file("pavian-report.Rmd",package="pavian")
      if (!file.exists(rmd_file)) {
        writeLines("Error in generating the report - didn't find Rmd file")
      	return()
      }
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), sprintf("%s-report.Rmd", sample_set_names_combined()))
      file.copy(rmd_file, tempReport, overwrite = TRUE)
      dmessage("Writing RMD to ",tempReport)
      
      # Set up parameters to pass to Rmd document
      params <- list(doc_title=input$report_title,
                     doc_author=input$report_author,
                     doc_date=input$report_date,
                     set_name=sample_set_names_combined(),
                     all_data_loaded=TRUE,
                     sample_data=sample_data(),
                     reports=reports(),
                     include_sankey=input$report_include_sankey,
                     filter_taxa=input$report_filter_taxa)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      withProgress({
	tryCatch(rmarkdown::render(tempReport, output_file = file,
                        params = params, output_format = "html_document",
                        envir = new.env()),
                        #envir = new.env(parent = globalenv())),
                 error = function(e) writeLines(paste("Error in generating the report:",conditionMessage(e)), con=file))
	}, message="Rendering report ...")
      removeModal()
      #shinyjs::alert("Report generated!")
      
    }
  )
}

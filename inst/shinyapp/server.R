library(shiny)

shinyServer(function(input, output, clientData, session) {

  ## helper functions for loading the files reactively
  list_kraken_files <- function(data_dir,sample_name="*") {
    Sys.glob(paste0(data_dir,"/",sample_name,"/report/*.report"))
  }

  kraken_reports <- reactive({
    kraken_files <- list_kraken_files(input$data_dir, input$sample_selector2)
    print(kraken_files)
    my_kraken_reports <- lapply(kraken_files,
           function(x) {
             load.or.create(function() {
               read.krakenres(x)
             }, basename(x))
           })
    names(my_kraken_reports) <- sub(paste0(input$data_dir,"/"),"",kraken_files,fixed=TRUE)
    my_kraken_reports
  })

  ## Observe the directory textInput to update sample selectors when it changed
  observe( {

    my.samples <- paddedsort(list.dirs(input$data_dir, full.names=FALSE, recursive = FALSE))
    kraken_files <- sub(paste0(input$data_dir,"/"),"",list_kraken_files(input$data_dir),fixed=FALSE)
    updateTextInput(session, 'data_dir',
                    label=paste0("Data directory on server (",length(my.samples)," samples in selected directory)"))
    updateSelectInput(session, 'sample_selector',
                         label=paste(length(kraken_files),"sample reports in directory",input$data_dir),
                         choices=kraken_files, selected=kraken_files[1])
    for (sample_selector in c('sample_selector2','sample_selector3'))
      updateSelectizeInput(session, sample_selector,
                           label=paste(length(my.samples),"samples in directory",input$data_dir),
                           choices=my.samples, selected=my.samples)

  })

  ## keep sample_selector2 and sample_selector3 synced
  observeEvent(input$sample_selector2, {
    updateSelectizeInput(session,"sample_selector3",selected=input$sample_selector2)
  })
  observeEvent(input$sample_selector3, {
    updateSelectizeInput(session,"sample_selector2",selected=input$sample_selector3)
  })

  beautify_string <- function(x) {
    x <- gsub("[\\._]"," ",x)
    x <- sub("^([[:alpha:]])", "\\U\\1", x, perl=TRUE)
    x
  }


  ##------------------------------------------------------------------------------------------------
  ## OUTPUTS

  sample_view_report <- reactive({
    my_report <- kraken_reports()[[input$sample_selector]]
    if (is.null(my_report))
      stop("No sample with that name")

    ## filter contaminants
    for (c in input$contaminant_selector2)
      my_report <- filter_taxon(my_report, c)

    my_report
  })

  ##----------------------
  ## Sample viewer outputs
  output$sunburst <- sunburstR::renderSunburst({

    # get reports with rows as selected in the table
    my_report <- sample_view_report()[sort(input$sample_view_rows_all),]

    # update reads_stay - which is used to display the sunburst values, if the node is a leaf node
    end.nodes <- my_report[,"depth"] >= c(my_report[,"depth"][-1],0)
    my_report[end.nodes,"reads_stay"] <- my_report[end.nodes,"reads"]

    kraken_sunburst(my_report)
  })

  output$sample_view <- DT::renderDataTable({
    my_report <- sample_view_report()

    my_report$taxonstring <- gsub("[a-z-]_","",my_report$taxonstring)
    my_report$taxonstring <- gsub("|",">",my_report$taxonstring,fixed=TRUE)
    my_report$level <- as.factor(my_report$level)

    colnames(my_report) <- beautify_string(colnames(my_report))
    DT::datatable(my_report, filter='top')
  }, server=TRUE)

  ##-------------------------
  ## Samples overview output
  output$samples_overview <- DT::renderDataTable({

    # TODO: Display sample names as a column such that they can be sorted or filtered (not as row names)
    samples_summary <- do.call(rbind,lapply(kraken_reports(), summarize.kraken.report))
    rownames(samples_summary) <- basename(rownames(samples_summary))
    colnames(samples_summary) <- beautify_string(colnames(samples_summary))

    DT::datatable(samples_summary)
  })

  ##---------------------------
  ## Samples comparison output
  output$samples_comparison <- DT::renderDataTable({

    ## generate data.frame which has a name column (species name) and a further reads column for each sample
    id.col <- c("name")
    numeric.col <- c("reads")
    my_reports <- kraken_reports()
    my_reports <- lapply(names(my_reports),function(report_name) {
      ## subset report to the requested level
      report <- my_reports[[report_name]][my_reports[[report_name]]$level==input$classification_level,
                                          c(id.col,numeric.col)]
      ## set the basename of the report file as name for the numeric column
      colnames(report)[2] <- sub(".*/(.*)(-PT.*)?.report","\\1",report_name)
      report
    })

    ## helper function that sets NAs to zeros in a supplied data.frame
    zero_if_na <- function(df) { df[is.na(df)] <- 0; df; }

    ## merge all the data.frames in the my_reports list, and add additional info (sparkline and mean)
    summarized_report <- Reduce(function(x,y) merge(x,y,all=TRUE,by=c(id.col)), my_reports)

    ## filter contaminants if defined
    if (length(input$contaminant_selector3) > 0 ) {
      summarized_report <- summarized_report[!summarized_report$name %in% input$contaminant_selector3,]
    }

    ## transform to percent
    data_portion <- summarized_report[,2:ncol(summarized_report)]
    if (input$numeric_display == "percentage") {
      data_portion <- round(100*t(t(data_portion)/colSums(data_portion,na.rm=T)),3)
    }

    summarized_report <- cbind(Name=summarized_report[,1],
                               Overview=apply(round(zero_if_na(data_portion),2),1,paste0,collapse=","),
                               Mean=round(rowMeans(data_portion,na.rm=TRUE),
                                          ifelse(input$numeric_display == "percentage", 3, 1)),
                               data_portion)

    data_cols <- 4:ncol(summarized_report)

    ## use columnDefs to convert column 2 (1 in javascript) into span elements with the class spark
    sparklineColumnDefs <- list(
      list(targets=0, searchable=TRUE),
      list(targets=data_cols-1, searchable=FALSE),
      list(targets = c(1),searchable=FALSE,
           render = htmlwidgets::JS("function(data, type, full){
              return '<span class=spark>' + data + '</span>'
            }")))

    ## define a callback that initializes a sparkline on elements which have not been initialized before
    ##   this is essential for pagination
    sparklineDrawCallback = htmlwidgets::JS("function (oSettings, json) {
      $('.spark:not(:has(canvas))').sparkline('html', {
        type: 'bar',
        highlightColor: 'orange'
      });
    }")

    ## TODO: Consider adding more information in child rows: https://rstudio.github.io/DT/002-rowdetails.html
    ##  For example: taxonomy ID, links to assemblies (e.g. www.ncbi.nlm.nih.gov/assembly/organism/821)
    ##   and organism overview http://www.ncbi.nlm.nih.gov/genome/?term=txid821[Organism:noexp]

    dt <- DT::datatable(summarized_report, options=list(
      columnDefs = sparklineColumnDefs,
      drawCallback = sparklineDrawCallback,
      order = list(2,"desc"),
      search = list(regex = TRUE, caseInsensitive = FALSE)   ## add regular expression search
      ),rownames=FALSE)

    ## use the sparkline package and the getDependencies function in htmlwidgets to get the
    ## dependencies required for constructing sparklines and then inject it into the dependencies
    ## needed by datatable
    dt$dependencies <- append(dt$dependencies, htmlwidgets:::getDependency('sparkline'))

    dt
  })

})

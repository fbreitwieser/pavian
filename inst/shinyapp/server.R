library(shiny)
library(centrifugeR)

info_message <- function(...) {
  message("INFO [",format(Sys.time(), "%m/%d %H:%M"),"]: ",...)
}

shinyServer(function(input, output, clientData, session) {

  if (is.null(getOption("centrifugeR.cache_dir")))
    options(centrifugeR.cache_dir="cache")

  info_message("cache dir: ",getOption("centrifugeR.cache_dir"))

  ## helper functions for loading the files reactively
  ##   file_glob_pattern contains %s, which is to be replaced by the sample names
  list_kraken_files <- function(data_dir,file_glob_pattern,sample_name="*") {
    #sample_file_globs <- sapply(sample_name,function(my_sample_name) gsub("\\([^(]*\\)",my_sample_name,file_glob_pattern))
    sample_file_globs <- sapply(sample_name,function(my_sample_name) gsub("%s",my_sample_name,file_glob_pattern))
    info_message("Looking for files with pattern(s) ",paste(sample_file_globs,collapse=","))
    Sys.glob(paste0(data_dir,"/",sample_file_globs))
  }

  get_sample_name <- function(file_names, regex_pattern) {
    sapply(file_names, function(file_name) sub(regex_pattern,"\\1",file_name))
  }

  ## load kraken_reports based on input$data_dir and input$sample_selector2
  kraken_reports <- reactive({
    kraken_files <- list_kraken_files(input$data_dir, input$file_glob_pattern, input$sample_selector2)
    my_kraken_reports <- lapply(kraken_files,
           function(x) {
             load_or_create(function() {
               read_krakenres(x)
             }, basename(x), cache_dir = getOption("centrifugeR.cache_dir"))
           })
    names(my_kraken_reports) <- sub(paste0(input$data_dir,"/"),"",kraken_files,fixed=TRUE)
	  names(my_kraken_reports) <- sub(".report$","",names(my_kraken_reports))
    info_message("Kraken report names: ",paste(names(my_kraken_reports),collapse="\n\t"))
    my_kraken_reports
  })

  ## Observe the directory textInput to update sample selectors when it changed
  observe( {
    info_message("data_dir set to ",input$data_dir)
    if (length(input$data_dir) == 0 || !dir.exists(input$data_dir)) {
      updateTextInput(session, 'data_dir', label = "Choose a valid directory")
      updateSelectInput(session, 'sample_selector', label = "No valid directory", choices = c(), selected = c())
      for (sample_selector in c('sample_selector2','sample_selector3')) {
        updateSelectizeInput(session, sample_selector, label = "No valid directory", choices = c(), selected = c())
      }
    } else {
      kraken_files <- sub(paste0(input$data_dir,"/"),"",list_kraken_files(input$data_dir, input$file_glob_pattern),fixed=TRUE)
	    kraken_files <- padded_sort(sub(".report$","",kraken_files))
      my_samples <- get_sample_name(kraken_files,input$regex_pattern)
      info_message("Found ",length(kraken_files)," files: \n\t",paste0(kraken_files," [sample ",my_samples,"]",collapse="\n\t"))
      updateTextInput(session, 'data_dir',
                      label=paste0("Data directory on server (",length(my_samples)," samples in selected directory)"))
      updateSelectInput(session, 'sample_selector',
                           label=paste(length(kraken_files),"sample reports in directory",input$data_dir),
                           choices=kraken_files, selected=kraken_files[1])

      ## update sample_selector2 (sample_selector3 gets updated by another observeEvent)
      for (sample_selector in c('sample_selector2','sample_selector3')) {
        updateSelectizeInput(session, sample_selector,
                             label=paste(length(my_samples),"samples in directory",input$data_dir),
                             choices=my_samples, selected=my_samples)
      }
    }
  })

  ## keep sample_selector2 and sample_selector3 synced
  observeEvent(input$sample_selector2, {
    updateSelectizeInput(session,"sample_selector3",selected=input$sample_selector2)
  })
  observeEvent(input$sample_selector3, {
    updateSelectizeInput(session,"sample_selector2",selected=input$sample_selector3)
  })

  ## Helper function to upper-case column names
  beautify_string <- function(x) {
    x <- gsub("[\\._]"," ",x)
    x <- sub("^([[:alpha:]])", "\\U\\1", x, perl=TRUE)
    x
  }
  beautify_colnames <- function(x) {
    colnames(x) <- beautify_string(colnames(x))
    x
  }


  ## helper function that sets NAs to zeros in a supplied data.frame
  zero_if_na <- function(df) { df[is.na(df)] <- 0; df; }

  ##

  get_summarized_report <- function(classification_level, filter_contaminants, numeric_display, as_matrix=FALSE) {
    ## generate data.frame which has a name column (species name) and a further reads column for each sample
    id_cols <- c("name","taxonid")
    numeric_col <- c("reads")

    my_reports <- kraken_reports()
    my_reports <- lapply(names(my_reports),function(report_name) {
      ## subset report to the requested level
      report <- my_reports[[report_name]][my_reports[[report_name]]$level==classification_level,
                                          c(id_cols,numeric_col)]

      ## set the basename of the report file as name for the numeric column
      idx_of_numeric_col <- length(id_cols)+1
      colnames(report)[idx_of_numeric_col] <- sub(".*/(.*)(-PT.*)?.report","\\1",report_name)
      report
    })


    ## merge all the data.frames in the my_reports list, and add additional info (sparkline and mean)
    summarized_report <- Reduce(function(x,y) merge(x,y,all=TRUE,by=id_cols), my_reports)

    ## filter contaminants if defined
    if (length(filter_contaminants) > 0 ) {
      summarized_report <- summarized_report[!summarized_report[,"name"] %in% filter_contaminants,]
    }

    ## transform to percent
    data_portion <- summarized_report[,seq(from=length(id_cols)+1, to=ncol(summarized_report))]

    if (numeric_display == "percentage") {
      data_portion <- round(100*t(t(data_portion)/colSums(data_portion,na.rm=T)),3)
    }

    if (as_matrix) {
      row_names <- summarized_report[,1]
      summarized_report <- as.matrix(data_portion)
      row.names(summarized_report) <- row_names
    } else {
      round_digits <- ifelse(numeric_display == "percentage", 3, 1)
      summarized_report <- cbind(beautify_colnames(summarized_report[,id_cols,drop=FALSE]),
                                 Overview=apply(round(zero_if_na(data_portion),round_digits),1,paste0,collapse=","),
                                 Mean=round(rowMeans(data_portion,na.rm=TRUE),round_digits),
                                 data_portion)

      ## that's the last column before the data, and the one which we sort for
      mean_column <- which(colnames(summarized_report)=="Mean")
      stopifnot(length(mean_column) == 1)
      data_columns <- seq(from=mean_column+1,
                       to=ncol(summarized_report))

      ## make a link to NCBI genome browser in the taxonID column
      taxonid_column <- which(colnames(summarized_report)=="Taxonid")
      summarized_report[,taxonid_column] <- gsub("^  *","",summarized_report[,taxonid_column])
      stopifnot(length(taxonid_column) == 1)

      ## remove s_, g_, etc
      summarized_report[,1] <- gsub("^[a-z-]_","",summarized_report[,1])
      attr(summarized_report,"mean_column") <- mean_column
      attr(summarized_report,"taxonid_column") <- taxonid_column
      attr(summarized_report,"data_columns") <- data_columns
    }
    summarized_report
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

    if (input$remove_root_hits)
      my_report <- my_report[my_report$name!="-_root",]

    my_report
  })

  ##----------------------
  ## Sample viewer outputs
  output$sunburst <- sunburstR::renderSunburst({

    my_report <- sample_view_report()
	if (length(my_report) == 0)
		return()

    # filter report with rows as selected in the table
    if (input$synchonize_sampleview_table_and_sunburst &&
		length(input$sample_view_rows_all) > 0)
      my_report <- my_report[sort(input$sample_view_rows_all),]

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
	my_reports <- kraken_reports()
	if (length(my_reports) == 0)
		return()

    # TODO: Display sample names as a column such that they can be sorted or filtered (not as row names)
    samples_summary <- do.call(rbind,lapply(my_reports, summarize_kraken_report))
    rownames(samples_summary) <- basename(rownames(samples_summary))
    colnames(samples_summary) <- beautify_string(colnames(samples_summary))

    DT::datatable(samples_summary)
  })

  ##---------------------------
  ## Samples comparison output
  output$samples_comparison <- DT::renderDataTable({

    summarized_report <- get_summarized_report(input$classification_level, input$contaminant_selector3, input$numeric_display)
	if (length(summarized_report) == 0)
		return()

    ## use columnDefs to convert column 2 (1 in javascript) into span elements with the class spark
    sparklineColumnDefs <- list(
      list(targets = 0, searchable=TRUE,
           render = htmlwidgets::JS(ifelse(input$classification_level=="S",
                           "function(data, type, full){ return '<i>'+data+'</i>'; }",  ## layout species names in italic
                           "function(data, type, full){ return data; }"
                           ))),
      list(targets = attr(summarized_report,'taxonid_column')-1,
           render = htmlwidgets::JS("function(data, type, full){
              return '<a href=\"http://www.ncbi.nlm.nih.gov/genome/?term=txid'+data+'[Organism:exp]\" target=\"_blank\">' + data + '</a>'
            }")),
      list(targets = attr(summarized_report,'data_columns')-1, searchable=FALSE),
      list(targets = which(colnames(summarized_report)=="Overview")-1,searchable=FALSE,
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
      order = list(attr(summarized_report,'mean_column')-1,"desc"),
      filter = "top",
      selection = "none",
      search = list(regex = TRUE, caseInsensitive = FALSE)   ## add regular expression search
      ),rownames=FALSE)

    ## use the sparkline package and the getDependencies function in htmlwidgets to get the
    ## dependencies required for constructing sparklines and then inject it into the dependencies
    ## needed by datatable
    dt$dependencies <- append(dt$dependencies, htmlwidgets:::getDependency('sparkline'))

    dt
  })

  output$samples_comparison_heatmap <- d3heatmap::renderD3heatmap({
    if (input$display_heatmap) {
      library(d3heatmap)
      report_mat <- get_summarized_report(input$classification_level,input$contaminant_selector3, input$numeric_display, as_matrix = TRUE)
      report_mat <- zero_if_na(report_mat[input$samples_comparison_rows_current,])
      report_mat[report_mat < 0] <- 0
      print(input$heatmap_cluster)
      d3heatmap::d3heatmap(report_mat,
                           Rowv="row" %in% input$heatmap_cluster,
                           Colv="column" %in% input$heatmap_cluster, # No Column reordering
                           scale=input$heatmap_scale,
                           yaxis_width = 400,
                           xaxis_height = 200,
                           colors=colorRampPalette(c("blue", "white", "red"))(100)
                           #colors=colorRampPalette(rev(RColorBrewer::brewer.pal(n = 7, name ="RdYlBu")))(100)
                           )
    }
  })

})

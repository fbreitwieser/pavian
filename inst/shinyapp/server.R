library(shiny)
library(shinydashboard)
library(centrifuger)
library(shinyFileTree)
library(magrittr)
library(DT)
shinyServer(function(input, output, clientData, session) {

  #if (is.null(getOption("centrifuger.cache_dir")))
    options(centrifuger.cache_dir="cache")

  info_message("cache dir: ",getOption("centrifuger.cache_dir"))

  kraken_reports <- reactive({
    kraken_files <- report_files()
    if (length(kraken_files) == 0) {
      return()
    }
    n_reports <- length(kraken_files)
    my_kraken_reports <-
      withProgress(message = paste("Loading",n_reports,"sample reports"),
                   detail = 'This may take a while...',
                   value = 0, min = 0,max = length(kraken_files), {
       lapply(seq_along(kraken_files),
             function(i) {
               kraken_file <- kraken_files[i]
               setProgress(value=i, detail = paste(n_reports-i, "left ..."))
               load_or_create(function() read_krakenres(kraken_file),
                              sprintf("%s.rds",basename(kraken_file)),
                              cache_dir = getOption("centrifuger.cache_dir"))
             })
    })

    names(my_kraken_reports) <- sub(paste0(input$cbo_data_dir,"/"),"",kraken_files,fixed=TRUE)
    names(my_kraken_reports) <- sub(".report$","",names(my_kraken_reports))
    info_message("Kraken report names: ",paste(names(my_kraken_reports),collapse="\n\t"))
    my_kraken_reports
  })

  ## Observe the directory textInput to update sample selectors when it changed
  observeEvent(input$btn_set_data_dir,  {
    info_message("data_dir set to ",input$cbo_data_dir)
    if (length(input$cbo_data_dir) == 0 || !dir.exists(input$cbo_data_dir)) {
      updateTextInput(session, 'cbo_data_dir', label = "Choose a valid directory")
      updateSelectInput(session, 'sample_selector', label = "No valid directory", choices = c(), selected = c())
    } else {
      my_samples <- get_sample_name(report_file_names(),input$regex_pattern)
      info_message("Found ",length(report_file_names())," files: \n\t",paste0(report_file_names()," [sample ",my_samples,"]",collapse="\n\t"))
      updateTextInput(session, 'cbo_data_dir',
                      label=sprintf("Select reports (%s selected)",length(my_samples)))
      updateSelectInput(session, 'sample_selector',
                           #label=paste(length(report_file_names()),"sample reports in directory",input$cbo_data_dir),
                           choices=report_file_names(), selected=report_file_names()[1])
    }
  })

  report_files <- reactive({
    validate(need(input$cbo_data_dir,message="No report directory selected", label="Report directory"))
    validate(need(input$files_tree_selected,message="Please select at least on report file.", label="Report files"))

    all_files <- paste0(input$cbo_data_dir, "/", input$files_tree_selected)
    sel_report_files <- grepl(paste0(input$txt_file_ext,"$"), all_files) &
      file.exists(all_files) & !dir.exists(all_files)
    all_files[sel_report_files]
  })

  report_file_names <- reactive({
    # strip base directory name
    report_files_no_dir <- sub(paste0(input$cbo_data_dir,"/"), "", report_files(), fixed = TRUE)

    # strip extension
    report_files_basename <- sub(paste0(input$txt_file_ext,"$"), "", report_files_no_dir)

    return(report_files_basename)
  })

  selected_report_files <- reactive({

  })

  get_summarized_report <- function(filter_contaminants, display_percentage, show_reads_stay, classification_level=NULL, as_matrix=FALSE) {
    ## generate data.frame which has a name column (species name) and a further reads column for each sample
    id_cols_before <- c("name","level","taxonid")
    numeric_col <- c("reads","reads_stay")
    id_cols_after <- c("taxonstring")
    id_cols <- c(id_cols_before,id_cols_after)

    my_reports <- kraken_reports()
    if (is.null(my_reports)) {
      return(NULL)
    }
    my_reports <- lapply(names(my_reports),function(report_name) {

      my_report <- my_reports[[report_name]]
      ## filter contaminants if defined
      for (c in filter_contaminants)
        my_report <- filter_taxon(my_report, c)

      ## subset report to the requested level
      if (!is.null(classification_level) && !(any(classification_level == "-"))) {
        my_report <- my_report[my_report$level==classification_level,]
      }
      my_report <- my_report[,c(id_cols,numeric_col)]

    if (input$opt_remove_root_hits)
      my_report <- my_report[my_report$name!="-_root",]


      ## set the basename of the report file as name for the numeric column
      idx_of_numeric_col <- seq(from=length(id_cols)+1,ncol(my_report))
      colnames(my_report)[idx_of_numeric_col] <- sub(".*/(.*)(-PT.*)?.report","\\1",report_name)
      if (length(numeric_col) > 1) {
        colnames(my_report)[idx_of_numeric_col] <- paste(colnames(my_report)[idx_of_numeric_col],numeric_col,sep="\n")
      }
      my_report
    })

    ## merge all the data.frames in the my_reports list, and add additional info (sparkline and mean)
    summarized_report <- Reduce(function(x,y) merge(x,y,all=TRUE,by=id_cols), my_reports)

    ## transform to percent
    data_portion <- summarized_report[,seq(from=length(id_cols)+1, to=ncol(summarized_report))]

    reads_idx <- seq(from=1,to=ncol(data_portion)-1, by=2)
    reads_stay_idx <- seq(from=2,to=ncol(data_portion), by=2)

    if (isTRUE(display_percentage)) {
      sum_reads <- colSums(data_portion[,reads_stay_idx],na.rm=T)
    }
    if (show_reads_stay == "reads") {
      data_portion <- data_portion[,reads_idx]
      colnames(data_portion) <- sub("\nreads","",colnames(data_portion))
    } else if (show_reads_stay == "reads_stay") {
      data_portion <- data_portion[,reads_stay_idx]
      colnames(data_portion) <- sub("\nreads_stay","",colnames(data_portion))
    }

    if (isTRUE(display_percentage)) {
      if (show_reads_stay == "both")
        sum_reads <- rep(sum_reads, each=2)
      data_portion <- signif(100*t(t(data_portion)/sum_reads),3)
    }

    if (as_matrix) {
      row_names <- summarized_report[,1]
      summarized_report <- as.matrix(data_portion)
      rownames(summarized_report) <- gsub("^[a-z-]_","",row_names)
    } else {
      if (isTRUE(display_percentage)) {
        mean_column <- signif(rowSums(data_portion,na.rm=TRUE)/ncol(data_portion),3)
      } else {
        mean_column <- signif(rowSums(data_portion,na.rm=TRUE)/ncol(data_portion),3)
      }
      round_digits <- ifelse(isTRUE(display_percentage), 3, 1)
      summarized_report <- cbind(beautify_colnames(summarized_report[,id_cols_before,drop=FALSE]),
                                 Overview=apply(round(zero_if_na(log10(10*data_portion)),round_digits),1,paste0,collapse=","),
                                 Mean=mean_column,
                                 data_portion,
                                 beautify_colnames(summarized_report[,id_cols_after,drop=FALSE]))

      ## that's the last column before the data, and the one which we sort for
      mean_column <- which(colnames(summarized_report)=="Mean")
      stopifnot(length(mean_column) == 1)
      data_columns <- seq(from=mean_column+1,
                       to=ncol(summarized_report)-length(id_cols_after))

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


  #############################################################################
  ##  OUTPUTS

  sample_view_report <- reactive({
    my_report <- kraken_reports()[[input$sample_selector]]
    if (is.null(my_report))
      stop("No sample with that name")

    ## filter contaminants
    for (c in input$contaminant_selector)
      my_report <- filter_taxon(my_report, c)

    if (input$opt_remove_root_hits)
      my_report <- my_report[my_report$name!="-_root",]

    my_report
  })

  #############################################################################
  ##  Sample viewer outputs

  output$sample_view_sunburst <- sunburstR::renderSunburst({

    my_report <- sample_view_report()
    if (length(my_report) == 0)
        return()

    # filter report with rows as selected in the table
    if (input$synchonize_sampleview_table_and_sunburst &&
        length(input$sample_view_rows_all) > 0)
      my_report <- my_report[sort(input$sample_view_rows_all),]

    kraken_sunburst(my_report)
  })

  output$sample_view_sankey <- networkD3::renderSankeyNetwork({
    my_report <- sample_view_report()
    if (length(my_report) == 0)
          return()

    # filter report with rows as selected in the table
    if (input$synchonize_sampleview_table_and_sunburst &&
        length(input$sample_view_rows_all) > 0)
      my_report <- my_report[sort(input$sample_view_rows_all),]

    #my_report$name <- sub("._", "", my_report$name)
    print(head(my_report))
    my_report <- my_report[,c("depth","reads","name")]
    #my_report$name <- sub("^._","",my_report$name)
    eng <- get_nodes_and_links(my_report,10)
    nodes <- eng[[1]]
    links <- eng[[2]]
    max.reads <- max(links[,"value"])

    #print(links)

    #output$maxReads <- renderUI({
    #  helpText(sprintf("max.reads: %s",max.reads))
    #})

    #updateSliderInput(session,inputId = "min.reads",min = 1,max = min(1000,max.reads))
    links$source_name <- nodes$name[links$source+1]


    if (!is.null(links))
    networkD3::sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  nodeWidth = 3, LinkGroup = "source_name",
                  fontSize = 12, moveNodesRight = FALSE)
  })

  output$dt_sample_view <- DT::renderDataTable({
    my_report <- sample_view_report()

    my_report$name <- sub("[a-z-]_","",my_report$name)
    my_report$taxonstring <- gsub("[a-z-]_","",my_report$taxonstring)
    my_report$taxonstring <- gsub("|",">",my_report$taxonstring,fixed=TRUE)
    my_report$level <- as.factor(my_report$level)
    my_report$Percent <- 100*signif(my_report$reads/sum(my_report$reads_stay,na.rm = TRUE), 3)
    my_report$coverage <- NULL
    my_report$rankperc <- NULL

    colnames(my_report) <- beautify_string(colnames(my_report))
    DT::datatable(my_report, filter='top',selection='single') %>%
      formatString("Percent", suffix = "%") %>%
      formatCurrency(c("Reads","Reads stay"),digits=0,currency="")

  }, server=TRUE)

  ## When a row (i.e. a taxonomical entry) gets selected in the sample view table, an action button appears to view the species in the overview
  output$view_in_samples_comparison <- renderUI({
    selected_row <- input$sample_view_rows_selected
    if (length(selected_row) != 1)
      return()

    my_report <- sample_view_report()
    selected_sample <- my_report[input$sample_view_rows_selected,"name"]
    selected_sample <- sub("^u_","",selected_sample)
    selected_sample <- sub("^-_","",selected_sample)
    selected_sample <- sub("^d_","domain ",selected_sample)
    selected_sample <- sub("^k_","kingdom ",selected_sample)
    selected_sample <- sub("^p_","phylum ",selected_sample)
    selected_sample <- sub("^o_","order ",selected_sample)
    selected_sample <- sub("^f_","family ",selected_sample)
    selected_sample <- sub("^g_","genus ",selected_sample)
    selected_sample <- sub("^s_","species ",selected_sample)

    ## TODO: Add a custom search functionality
    #actionButton("view_selected_in_samples_comparison",paste("--> View abundances of ",selected_sample,"across samples"))
  })

  observeEvent(input$view_selected_in_samples_comparison,{
    my_report <- sample_view_report()
    selected_sample <- my_report[input$sample_view_rows_selected,"name"]

    updateTabsetPanel(session,"main_page",selected="Sample comparison")
  }, ignoreNULL = TRUE)


  #############################################################################
  ## Samples overview output
  output$dt_samples_overview <- DT::renderDataTable({
    my_reports <- kraken_reports()
    if (length(my_reports) == 0)
        return()

    # TODO: Display sample names as a column such that they can be sorted or filtered (not as row names)
    samples_summary <- do.call(rbind,lapply(my_reports, summarize_kraken_report))
    rownames(samples_summary) <- basename(rownames(samples_summary))
    colnames(samples_summary) <- beautify_string(colnames(samples_summary))

    number_range <-  c(0,max(samples_summary[,1], na.rm=TRUE))
    start_color_bar_at <- 1

    if (isTRUE(input$opt_samples_overview_percent)) {
      start_color_bar_at <- 2
      number_range <- c(0,100)
        samples_summary[,2:ncol(samples_summary)] <- 100*signif(sweep(samples_summary[,2:ncol(samples_summary)],1,samples_summary[,1],`/`),2)
    }

    dt <- DT::datatable(samples_summary,selection='single',
                  options=list(pagelength=25)) %>%
      DT::formatStyle(colnames(samples_summary)[start_color_bar_at:5],
                  background = DT::styleColorBar(number_range, 'lightblue')) %>%
      DT::formatStyle(colnames(samples_summary)[6:ncol(samples_summary)],
                      background = DT::styleColorBar(c(0,max(samples_summary[,6], na.rm=TRUE)), 'lightgreen'))

    #formatString <- function(table, columns, before="", after="") {
    #  DT:::formatColumns(table, columns, function(col, before, after)
    #    sprintf("$(this.api().cell(row, %s).node()).html((%s + data[%d]) + %s);  ",col, before, col, after),
    #    before, after
    #  )
    #}

    if (isTRUE(input$opt_samples_overview_percent)) {
      dt <- dt %>%
        formatCurrency(1,currency='',digits = 0) %>%
        formatString(2:ncol(samples_summary), suffix='%', prefix = '')  ## TODO: display as percent
        ## not implemented for now as formatPercentage enforces a certain number of digits, but I like to round
        ## with signif.
    } else {
      dt <- dt %>% formatCurrency(1:ncol(samples_summary),currency='',digits = 0)
    }
    dt
  })


  ## When a row (i.e. a sample gets selected in the samples) an action button appears to view in the sample viewer
  output$view_in_sample_viewer <- renderUI({
    selected_row <- input$dt_samples_overview_rows_selected[length(input$dt_samples_overview_rows_selected)]
    if (length(selected_row) < 1)
      return()

    message(input$dt_samples_overview_rows_selected)
    selected_sample <- report_file_names()[input$dt_samples_overview_rows_selected]
    actionButton("btn_view_selected_in_sample_viewer",paste("--> View details of sample",selected_sample))
  })

  observeEvent(input$btn_view_selected_in_sample_viewer,{
    selected_sample <- report_file_names()[input$dt_samples_overview_rows_selected]
    updateSelectInput(session,'sample_selector',selected=selected_sample)
    updateTabsetPanel(session,"tabsetPanel_main",selected="Sample viewer")
  }, ignoreNULL = TRUE)

  #############################################################################
  ## Samples comparison output

  summarized_report <- reactive({
    get_summarized_report(input$contaminant_selector, input$opt_display_percentage, input$opt_show_reads_stay, input$opt_classification_level)
  })

  output$dt_samples_comparison <- DT::renderDataTable({
    #if (!isTRUE(input$display_table))
    #  return()

    summarized_report <- summarized_report()
    req(summarized_report)

    idx_data_columns <- attr(summarized_report,'data_columns')
    colnames(summarized_report)[idx_data_columns] <- sub(".*/","",colnames(summarized_report)[idx_data_columns])

    ## use columnDefs to convert column 2 (1 in javascript) into span elements with the class spark
    sparklineColumnDefs <- list(
      #list(targets = 0, searchable=TRUE,
      #     render = htmlwidgets::JS(ifelse(input$opt_classification_level=="S",
      #                     "function(data, type, full){ return '<i>'+data+'</i>'; }",  ## layout species names in italic
      #                     "function(data, type, full){ return data; }"
      #                     ))),
      list(targets = attr(summarized_report,'taxonid_column')-1,
           render = htmlwidgets::JS("function(data, type, full){
              return '<a href=\"http://www.ncbi.nlm.nih.gov/genome/?term=txid'+data+'[Organism:exp]\" target=\"_blank\">' + data + '</a>'
            }")),
      list(targets = attr(summarized_report,'mean_column')-1,width="80px"),
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

    #colnames(summarized_report) <- gsub("_","_<wbr>",colnames(summarized_report))

    query <- parseQueryString(session$clientData$url_search)

    # Return a string with key-value pairs

    dt <- DT::datatable(summarized_report,
      filter = "top",options=list(
      columnDefs = sparklineColumnDefs,
      drawCallback = sparklineDrawCallback,
      order = list(attr(summarized_report,'mean_column')-1,"desc"),
      search = list(search=ifelse("search" %in% names(query), query['search'],""),
                    regex = TRUE, caseInsensitive = FALSE)   ## add regular expression search
      ),rownames=FALSE, selection='single')

    if (!isTRUE(input$opt_display_percentage)) {
      dt <- dt %>% formatCurrency(attr(summarized_report,'mean_column'),currency='',digits=1) %>%
                   formatCurrency(attr(summarized_report,'data_columns'),currency='',digits=0)
    } else {
      dt <- dt %>% formatString(attr(summarized_report,'mean_column'),postfix='%') %>%
                   formatString(attr(summarized_report,'data_columns'),postfix='%')
    }

    ## use the sparkline package and the getDependencies function in htmlwidgets to get the
    ## dependencies required for constructing sparklines and then inject it into the dependencies
    ## needed by datatable
    dt$dependencies <- append(dt$dependencies, htmlwidgets:::getDependency('sparkline'))

    dt
  })

  output$d3heatmap_samples_comparison <- renderUI({
    req(input$dt_samples_comparison_rows_current)
    d3heatmap::d3heatmapOutput('my_d3heatmap',width="100%",
                               height=paste0(200+length(input$dt_samples_comparison_rows_current)*15,"px"))
  })

  output$my_d3heatmap <- d3heatmap::renderD3heatmap({
    req(input$dt_samples_comparison_rows_current)

    sr <- summarized_report()
    report_mat <- as.matrix(sr[,attr(sr,"data_columns")])
    rownames(report_mat) <- gsub("^[a-z-]_","",sr[,1])


    report_mat <- zero_if_na(report_mat[input$dt_samples_comparison_rows_current,])
    report_mat[report_mat < 0] <- 0
    d3heatmap::d3heatmap(report_mat,
                         Rowv="row" %in% input$heatmap_cluster,
                         Colv="column" %in% input$heatmap_cluster, # No Column reordering
                         scale=input$heatmap_scale,
                         yaxis_width =300,
                         xaxis_height = 200,
                         xaxis_font_size = "10pt",
                         yaxis_font_size = "10pt",
                         colors=colorRampPalette(c("blue", "white", "red"))(100)
                         )
  })

  output$cluster_plot <- renderPlot({
      my_reports <- kraken_reports()
      if (length(my_reports) == 0)
          return()

      #idvar=".id"; timevar="name"
      idvar="name"; timevar=".id"

      all.s.reads <- reshape(get_level_reads(my_reports,level=="S",min.perc=0.01)[,c(idvar,timevar,"reads")],
                             timevar=timevar,
                             idvar=idvar,
                             direction="wide")
      rownames(all.s.reads) <- all.s.reads$NAME
      all.s.reads$name <- NULL
      colnames(all.s.reads) <- sub("reads.(.*)","\\1",colnames(all.s.reads))

      eucl.dist <- dist(t(all.s.reads))
      hc <- hclust(eucl.dist)
      dend <- as.dendrogram(hc)

      gapmap::gapmap(m = as.matrix(eucl.dist), d_row= rev(dend), d_col=dend,
                     h_ratio=c(0.2,0.5,0.3),v_ratio=c(0.2,0.5,0.3))
  })

  output$txt_samples_comparison <- reactive({
    req(input$dt_samples_comparison_rows_selected)
    selected_row <- zero_if_na(summarized_report()[input$dt_samples_comparison_rows_selected,])
    str(selected_row)
    #is_domain <- input$opt_classification_level == "D"
    is_domain <- FALSE
    data_columns <- attr(selected_row,"data_columns")
    taxid <- selected_row[,"Taxonid"]
    sprintf("
<h3>%s</h3>
NCBI links: <a href='http://www.ncbi.nlm.nih.gov/genome/?term=txid%s[Organism:noexp]'>Organism overview</a>, <a href='www.ncbi.nlm.nih.gov/assembly/organism/%s/latest'>Assemblies</a>
 (taxid %s)
<br/>
Lineage: %s
</br>
<p>
<table>
<thead>
<tr>Sample<th></th><th>Number of reads</th></tr>
</thead>
%s
</table>
</p>
            ",selected_row$Name,
            taxid,taxid,taxid,
            ifelse(is_domain,"",gsub("\\|._","; ",sub("-_root\\|","",selected_row$Taxonstring))),
            paste0(sprintf("<tr><th>%s</th><td align='right'>%s</td><td><a href='%s'>⇨  align</a></td></tr>",
                           colnames(selected_row)[data_columns],
                           selected_row[,data_columns],
                           "test"
                           ),
                   collapse="\n")
            )
  })

  observeEvent(input$btn_sc_filter, {
    req(input$dt_samples_comparison_rows_selected)
    ## TODO: How to get current choices from selectizeInput?
    taxonstring <- summarized_report()[input$dt_samples_comparison_rows_selected,"Taxonstring"]
    selected_path <- strsplit(taxonstring,"|",fixed = TRUE)[[1]]
    selected_name <- selected_path[length(selected_path)]
    message("filtering ", selected_path[length(selected_path)])

    updateSelectizeInput(session,"contaminant_selector",
                         selected = unique(c(input$contaminant_selector,selected_name)),
                         choices = unique(c(allcontaminants,selected_name)))
  })

  observeEvent(input$btn_sc_gointo, {
    req(input$dt_samples_comparison_rows_selected)
    taxonstring <- summarized_report()[input$dt_samples_comparison_rows_selected,"Taxonstring"]
    selected_path <- strsplit(taxonstring,"|",fixed = TRUE)[[1]]
    selected_name <- selected_path[length(selected_path)]

    #input$dt_samples_comparison_search <- selected_name
  })

  #############################################################################
  ## Alignment output
  pileup <- eventReactive(input$btn_get_alignment, {
    req(input$bam_file)
    get_pileup(input$bam_file,input$align_moving_avg)
  })

  seq_lengths <- reactive({
    get_seq_lengths(input$bam_file)
  })

  output$files_tree <- shinyFileTree::renderShinyFileTree({
    ## TODO: Consider pre-selecting brain-biospies directory when application is first loaded
    shinyFileTree(shinyFileTree::get_list_from_directory(input$cbo_data_dir),plugins = c("checkbox","types"))
  })

  nreads <- reactive({
    get_n_reads(input$bam_file)
  })

  output$sample_align <- renderPlot({
    req(input$bam_file)

    plot_pileup(pileup(), nreads())

  })

  output$txt_align_brush <- shiny::renderPrint({
    #brushedPoints(pileup(), input$align_brush)
    input$txt_align_brush
  })

  assembly_info <- eventReactive("btn_load_assembly_info",{
    dir.create(getOption("centrifuger.cache_dir"))
    assembly_info_f <- paste0(getOption("centrifuger.cache_dir"),"/assembly_summary_refseq.txt")
    assembly_info_f <- "/home/fbreitwieser/ai.txt"
    if (!file.exists(assembly_info_f)) {
      withProgress({
        download.file("ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/assembly_summary_refseq.txt", assembly_info_f)
      }, message="Getting RefSeq assembly summary")
    }
    read.delim(assembly_info_f)
  })


  output$dt_assembly_info <- DT::renderDataTable({
    ai <- assembly_info()

    req(ai)
    ai$seq_rel_date <- as.Date(ai$seq_rel_date)

    colnames(ai)[1] <- "AC"
    ai$organism_name <- as.character(ai$organism_name)
    ai$infraspecific_name <- sub("^strain=","",ai$infraspecific_name)
    ends_with_infraspecific_name <- substr(ai$organism_name,nchar(ai$organism_name) - nchar(ai$infraspecific_name)+1,nchar(ai$organism_name)) == ai$infraspecific_name
    ai$organism_name[!ends_with_infraspecific_name] <- paste(ai$organism_name,ai$infraspecific_name)[!ends_with_infraspecific_name]

    columns <- c("AC", "taxid", "organism_name", "assembly_level", "seq_rel_date", "submitter")
    DT::datatable(ai[,columns], filter='top', options=list(pagelength=10),selection = 'single')
  })





})

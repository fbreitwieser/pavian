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
                 selectizeInput(
                   ns("opt_classification_level"),
                   label = "Taxon level",
                   choices = taxon_levels,
                   selected = "-"
                 ),
                 checkboxInput(ns("opt_display_percentage"),
                               label = "Show percentages",
                               value = FALSE),
                 checkboxInput(ns("opt_zscore"),
                               label = "z-score",
                               value = FALSE)),
          column(6,
                 radioButtons(
                   ns("opt_show_reads_stay"),
                   label = "",
                   choices = c(
                     "Reads at taxon" = "reads_stay",
                     "Reads at taxon or lower" = "reads",
                     "both"
                   )),
                 checkboxInput(ns("opt_remove_root_hits"),
                               label = "Do not show reads that stay at the root",
                               value = TRUE)

          )
      ),
      box(
        width = 6,
        title = "Filter contaminants",
        selectizeInput(
          ns('contaminant_selector'),
          allcontaminants,
          label = "At level",
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
          label = "At level and below",
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
        actionButton(ns("btn_sc_gointo"), "Go Into"),
        shiny::htmlOutput(ns("txt_samples_comparison"))
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

  summarized_report <- reactive({
    my_reports <- selected_reports()
    if (!is.null(filter_func)) {
      my_reports <- lapply(my_reports, filter_func)
    }
    if (input$opt_remove_root_hits) {
      for (nn in names(my_reports)) {
        sel <- my_reports[[nn]]$name == "-_root"
        if (any(sel))
          my_reports[[nn]][sel,"reads_stay"] <- 0
      }
    }
    withProgress(message="Combining sample reports ...",{
      get_summarized_report(
        my_reports,
        input$opt_display_percentage,
        input$opt_zscore,
        input$opt_show_reads_stay,
        input$opt_classification_level#,
        #input$opt_remove_root_hits  ## TODO: Consider adding it back in
      )
    })
  })

  observeEvent(samples_df, {
    updateSelectizeInput(session, "select_samples",
                         choices=samples_df()[,"Name"], selected=samples_df()[,"Name"]
    )
  })

  output$dt_samples_comparison <- DT::renderDataTable({

    summarized_report <- summarized_report()
    validate(need(summarized_report, message = "No data"))

    idx_data_columns <- attr(summarized_report, 'data_columns')
    colnames(summarized_report)[idx_data_columns] <-
      sub(".*/", "", colnames(summarized_report)[idx_data_columns])

    ## use columnDefs to convert column 2 (1 in javascript) into span elements with the class spark
    sparklineColumnDefs <- list(
      #list(targets = 0, searchable=TRUE,
      #     render = htmlwidgets::JS(ifelse(input$opt_classification_level=="S",
      #                     "function(data, type, full){ return '<i>'+data+'</i>'; }",  ## layout species names in italic
      #                     "function(data, type, full){ return data; }"
      #                     ))),
      list(
        targets = attr(summarized_report, 'taxonid_column') - 1,
        render = htmlwidgets::JS(
          "function(data, type, full){
          return '<a href=\"http://www.ncbi.nlm.nih.gov/genome/?term=txid'+data+'[Organism:exp]\" target=\"_blank\">' + data + '</a>'
  }"
        )
      ),
      list(
        targets = attr(summarized_report, 'mean_column') - 1,
        width = "80px"
      ),
      list(
        targets = attr(summarized_report, 'data_columns') - 1,
        searchable = FALSE
      ),
      list(
        targets = which(colnames(summarized_report) == "Overview") - 1,
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
      highlightColor: 'orange'
      });
  }"
    )

    ## TODO: Consider adding more information in child rows: https://rstudio.github.io/DT/002-rowdetails.html
    ##  For example: taxonomy ID, links to assemblies (e.g. www.ncbi.nlm.nih.gov/assembly/organism/821)
    ##   and organism overview http://www.ncbi.nlm.nih.gov/genome/?term=txid821[Organism:noexp]

    #colnames(summarized_report) <- gsub("_","_<wbr>",colnames(summarized_report))

    query <- parseQueryString(session$clientData$url_search)

    # Return a string with key-value pairs

    dt <-
      DT::datatable(
        #cbind(
        #  Delete = shinyInput(
        #    actionButton,
        #    nrow(summarized_report),
        #    'button_',
        #    label = "Delete",
        #    onclick = 'Shiny.onInputChange(\"select_button\",  this.id)'
        #  ),
        summarized_report,
        #),
        filter = "top",
        escape = FALSE,
        extensions = c('Buttons'),
        options = c(
          datatable_opts,
          list(

            dom = 'Bfrtip'
            , buttons = c('pageLength','pdf', 'excel' , 'csv', 'copy')
            #, buttons = c('pageLength', 'colvis', 'excel', 'pdf')                             # pageLength / colvis / excel / pdf
            , lengthMenu = list(c(10, 25, 100, -1), c('10', '25', '100', 'All'))
            , pageLength = 10,
            columnDefs = sparklineColumnDefs,
            autoWidth = TRUE,
            drawCallback = sparklineDrawCallback,
            order = list(attr(summarized_report, 'mean_column') - 1, "desc"),
            search = list(
              search = ifelse("search" %in% names(query), query['search'], ""),
              regex = TRUE,
              caseInsensitive = FALSE
            )   ## add regular expression search
          )
        ),
        rownames = FALSE,
        selection = 'single'
      )

    if (!isTRUE(input$opt_display_percentage) && !isTRUE(input$opt_zscore)) {
      dt <-
        dt %>% DT::formatCurrency(
          attr(summarized_report, 'mean_column'),
          currency = '',
          digits = 1
        ) %>%
        DT::formatCurrency(
          attr(summarized_report, 'data_columns'),
          currency = '',
          digits = 0
        )
    } else {
      suffix <- ifelse(isTRUE(input$opt_zscore),"","%")
      dt <-
        dt %>% DT::formatString(attr(summarized_report, 'mean_column'), suffix = suffix) %>%
        DT::formatString(attr(summarized_report, 'data_columns'), suffix = suffix)
    }

    dt <- dt %>% DT::formatStyle(
      attr(summarized_report, 'data_columns'),
      background = DT::styleColorBar(summarized_report$Mean, 'lightblue'),
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
      colors = grDevices::colorRampPalette(c("blue", "white", "red"))(100)
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

  output$txt_samples_comparison <- reactive({
    req(input$dt_samples_comparison_rows_selected)
    selected_row <-
      zero_if_na(summarized_report()[input$dt_samples_comparison_rows_selected, ])
    #is_domain <- input$opt_classification_level == "D"
    is_domain <- FALSE
    data_columns <- attr(selected_row, "data_columns")
    taxid <- selected_row[, "Taxonid"]
    sprintf(
      "
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
      ",
      selected_row$Name,
      taxid,
      taxid,
      taxid,
      ifelse(is_domain, "", gsub(
        "\\|._",
        "; ",
        sub("-_root\\|", "", selected_row$Taxonstring)
      )),
      paste0(
        sprintf(
          "<tr><th>%s</th><td align='right'>%s</td><td><a href='%s'>&rarr;  align</a></td></tr>",
          colnames(selected_row)[data_columns],
          selected_row[, data_columns],
          "test"
        ),
        collapse = "\n"
      )
    )
  })

  observeEvent(input$btn_sc_filter, {
    req(input$dt_samples_comparison_rows_selected)
    ## TODO: How to get current choices from selectizeInput?
    taxonstring <-
      summarized_report()[input$dt_samples_comparison_rows_selected, "Taxonstring"]
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
    taxonstring <-
      summarized_report()[input$dt_samples_comparison_rows_selected, "Taxonstring"]
    selected_path <- strsplit(taxonstring, "|", fixed = TRUE)[[1]]
    selected_name <- selected_path[length(selected_path)]

    #input$dt_samples_comparison_search <- selected_name
  })


}

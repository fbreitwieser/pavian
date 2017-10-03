#' UI part of report overview module
#'
#' @param id Shiny namespace id.
#'
#' @return UI elements for report overview module.
#' @export
#' @import shiny
 reportOverviewModuleUI<- function(id) {
  ns <- shiny::NS(id)
  uiOutput(ns("UI"))
}

reportOverviewModuleUI_function <- function(ns) {
  shiny::tagList(
    HTML("This page shows the summary of the classifications in the selected sample set.
         The cells have a barchart that shows the relation of the cell value to other cell values in the same category, 
         with the microbiota columns being a separate category from the rest."),
    #checkboxInput(ns("opt_samples_overview_percent"), label = "Show percentages instead of number of reads", value = FALSE),
    tabsetPanel(id = ns("tabsetpanel"),
                tabPanel("Classification summary", 
                         div(style = 'overflow-x: scroll', DT::dataTableOutput(ns('dt_percent'))),
                         br(),
                         uiOutput(ns("btn_sample_percent_ui"))
                ),
                tabPanel("Raw read numbers", 
                         div(style = 'overflow-x: scroll', DT::dataTableOutput(ns('dt'))))
    ),
    actionLink(ns("btn_go_to_sample_comp"), label="Explore identifications across all samples in the Sample Comparison View.", icon=icon("line-chart"))
    )
  
}

## TODO: use replacedata to replace the data https://github.com/rstudio/DT/issues/168
## this stops flickering in the when displaying percent - but might not work with changing the formatting

#' Shiny modules to display an overview of metagenomics reports
#'
#' @param input Shiny input object.
#' @param output Shiyn output object.
#' @param session Shiny session object.
#' @param sample_data Samples \code{data.frame}.
#' @param reports List of reports.
#' @param datatable_opts Additional datatable opts (mostly $class)
#'
#' @return Report overview module server functionality.
#' @export
#' @import shiny
reportOverviewModule <- function(input, output, session, sample_data, reports, datatable_opts = NULL) {
  #r_state <- list()
  
  output$UI <- renderUI({
    req(reports())
    reportOverviewModuleUI_function(session$ns)
  })
  
  get_samples_summary <- reactive( {
    validate(need(reports(), message = "No data available."))
    
    withProgress({
      samples_summary <- do.call(rbind, lapply(reports(), summarize_report))
      
      ## Add a 'Name' column in front
      samples_summary$Name <- rownames(samples_summary)
      extra_cols <- c("Name")
      samples_summary <- samples_summary[,c(extra_cols, setdiff(colnames(samples_summary),extra_cols))]
      colnames(samples_summary) <- beautify_string(colnames(samples_summary))
      
      samples_summary
    }, message = "Summarizing sample contents ... ")
  })
  
  get_samples_summary_percent <- reactive({
    samples_summary <- get_samples_summary()
    raw_reads_column <- 2
    classified_reads_column <- 3
    microbial_reads_column <- 7
    samples_summary[, classified_reads_column:ncol(samples_summary)] <- signif(100 * sweep(samples_summary[, classified_reads_column:ncol(samples_summary)], 1, samples_summary[, raw_reads_column], `/`),3)
    samples_summary
  })
  
  ## Samples overview output
  output$dt <- DT::renderDataTable({
    samples_summary <- get_samples_summary()
    validate(need(samples_summary, message = "Error in getting samples summary - please re-select the sample set."))
    validate(need(max(samples_summary[,2]) > 110,message="It seems this sample set just reports percentages, and not reads."))
    validate(need(sample_data(), message = "No data available."))
    validate(need(nrow(samples_summary)==nrow(sample_data()), message = "Different number of rows in summary and sample data"))
    
    if(is.null(datatable_opts$extensions)) { dt_extensions <- list() } else { dt_extensions <- datatable_opts$extensions }
    if(is.null(datatable_opts$class)) { dt_class <- "display" } else { dt_class <- datatable_opts$class }
    set_name <- ifelse(is.null(attr(sample_data(), "set_name")), "classification", basename(attr(sample_data(), "set_name")))
    
    ## TODO (sometime): Add buttons to go to sample view from table
    #samples_summary[["action"]] <-
    #  paste0('<a href="#" class="action-button shiny-bound-input" id=view_',1:nrow(samples_summary),'><i class="fa fa-sun-o"></i></a>')
    
    
    DT::datatable(
      samples_summary,
      options=list(buttons = common_buttons(set_name, "summary"), processing = TRUE, 
              columnDefs=list(list(targets = seq(from=1, to=ncol(samples_summary)-1), visible=TRUE, orderSequence = c('desc','asc')))
      ),
      class = dt_class, rownames = FALSE, selection = "single", escape = FALSE,
      extensions = dt_extensions) %>% formatSummaryDT(sample_data = sample_data(), display_percent=FALSE)
  }, server = FALSE)
  
  output$dt_percent <- DT::renderDataTable({
    samples_summary <- get_samples_summary_percent()

    validate(need(samples_summary, message = "Error in getting samples summary - please re-select the sample set."))
    validate(need(sample_data(), message = "No data available."))
    validate(need(nrow(samples_summary)==nrow(sample_data()), message = "Different number of rows in summary and sample data"))
    
    if(is.null(datatable_opts$extensions)) { dt_extensions <- list() } else { dt_extensions <- datatable_opts$extensions }
    if(is.null(datatable_opts$class)) { dt_class <- "display" } else { dt_class <- datatable_opts$class }
    set_name <- ifelse(is.null(attr(sample_data(), "set_name")), "classification", basename(attr(sample_data(), "set_name")))
    req(set_name)
    
    DT::datatable(
      samples_summary,
      options=list(buttons = common_buttons(set_name, "summary"), processing = TRUE, 
              columnDefs=list(list(targets = seq(from=1, to=ncol(samples_summary)-1), visible=TRUE, orderSequence = c('desc','asc')))
      ),
      class = dt_class, rownames = FALSE, selection = "single",
      extensions = dt_extensions) %>% formatSummaryDT(sample_data = sample_data(), display_percent=TRUE)
  }, server = FALSE)
  
  output$btn_sample_percent_ui <- renderUI({
    req(input$dt_percent_rows_selected)
    ns <- session$ns
    actionLink(ns("btn_sample_percent"),
               label = sprintf("See identifications from sample %s in Sample Viewer.", get_samples_summary()[input$dt_percent_rows_selected,"Name"]),
               icon = icon("sun-o"))
  })
  
  rv = reactiveValues(selected_sample=NULL, go_to_sample_comparison=FALSE)
  observeEvent(input$btn_sample_percent, {
    message("Sample button pressed, setting to ", input$dt_percent_rows_selected)
    rv$go_to_sample_comparison = FALSE
    rv$selected_sample <- input$dt_percent_rows_selected
  })
  
  observeEvent(input$btn_go_to_sample_comp, {
    rv$go_to_sample_comparison = TRUE
  })
  
  return(rv)
}



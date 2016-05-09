

#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
alignmentModuleUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel(
      title = "View alignment",
      box(width = 12,
          #shiny::textInput("bam_file","Bam File"),
          shinyFileTree::shinyFileTreeOutput(ns("bam_files_tree")),
          shiny::actionButton(ns("btn_get_alignment"), "Load alignment"),
          shiny::checkboxInput(ns("align_loess"), "Show smoothed LOESS curve"),
          shiny::checkboxInput(ns("align_moving_avg"), "Show moving average", value = TRUE),
          shiny::plotOutput(ns("sample_align"), brush = brushOpts("align_brush", direction = "x")),
          shiny::htmlOutput(ns("txt_align_brush"))
      )
    ),
    tabPanel(
      title = "Create alignment",
      box(width = 12,
          shiny::actionButton(ns("btn_create_alignment"), "Create alignment"),
          shiny::selectizeInput(ns("cbo_assemblies"), choices =
                                  NULL, label = "RefSeq Assemblies"),
          shiny::actionButton(ns("btn_load_assembly_info"), "Load RefSeq assemblies"),
          div(style = 'overflow-x: scroll', DT::dataTableOutput("dt_assembly_info")),
          shiny::actionButton(ns("btn_get_reads"), "Get reads")
      )
    )
  )

}

#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param samples_df
#'
#' @return
#' @export
#'
#' @examples
alignmentModule <- function(input, output, session, samples_df) {
  output$bam_files_tree <- shinyFileTree::renderShinyFileTree({
    shinyFileTree(
      list(
        text = input$cbo_data_dir,
        type = "directory",
        state = list(opened = TRUE),
        children = shinyFileTree::get_list_from_directory(input$cbo_data_dir, ".bam", hide_empty_dirs = TRUE)
      ),
      plugins = c("types")
    )
  })

  nreads <- reactive({
    get_n_reads(input$bam_files_tree_selected)
  })


  plot_pileup_act <- eventReactive(input$btn_get_alignment, {
    str(input$bam_files_tree_selected)
    req(input$bam_files_tree_selected)
    centrifuger::plot_pileup(
      input$bam_files_tree_selected,
      input$align_moving_avg,
      input$align_loess,
      text_size = 4
    )
  })

  output$sample_align <- renderPlot({
    plot_pileup_act()
  })

  output$txt_align_brush <- shiny::renderPrint({
    input$txt_align_brush
  })

  assembly_info <- eventReactive("btn_load_assembly_info", {
    dir.create(getOption("centrifuger.cache_dir"))
    assembly_info_f <-
      paste0(getOption("centrifuger.cache_dir"),
             "/assembly_summary_refseq.txt")
    assembly_info_f <- "/home/fbreitwieser/ai.txt"
    if (!file.exists(assembly_info_f)) {
      withProgress({
        download.file(
          "ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/assembly_summary_refseq.txt",
          assembly_info_f
        )
      }, message = "Getting RefSeq assembly summary")
    }
    read.delim(assembly_info_f)
  })


  output$dt_assembly_info <- DT::renderDataTable({
    ai <- assembly_info()

    req(ai)
    ai$seq_rel_date <- as.Date(ai$seq_rel_date)

    colnames(ai)[1] <- "AC"
    ai$organism_name <- as.character(ai$organism_name)
    ai$infraspecific_name <-
      sub("^strain=", "", ai$infraspecific_name)
    ends_with_infraspecific_name <-
      substr(
        ai$organism_name,
        nchar(ai$organism_name) - nchar(ai$infraspecific_name) + 1,
        nchar(ai$organism_name)
      ) == ai$infraspecific_name
    ai$organism_name[!ends_with_infraspecific_name] <-
      paste(ai$organism_name, ai$infraspecific_name)[!ends_with_infraspecific_name]

    columns <-
      c("AC",
        "taxid",
        "organism_name",
        "assembly_level",
        "seq_rel_date",
        "submitter")
    DT::datatable(
      ai[, columns],
      filter = 'top',
      options = c(common_datatable_opts, list(pagelength = 10)),
      selection = 'single'
    )
  })
}

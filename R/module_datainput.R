#' UI part of pavian data input module
#'
#' @param id Namespace ID
#'
#' @return Shiny UI elements
#' @export
#' @import shiny
#' @import shinydashboard
#' @import rhandsontable
dataInputModuleUI <- function(id) {

  ns <- NS(id)

  shiny::tagList(
    box(width=12,
        title = "Data Input",
        background = "green",
        collapsible = TRUE,
        collapse = TRUE,
        "You can upload Kraken and Centrifuge report files, biom format files, or load the example data set.",
        fileInput(ns("file_upload"), "", multiple = TRUE),
        textInput(ns("txt_data_dir"),label="Directory (on server)",
                  value = system.file("shinyapp","example-data", package = "pavian"),
                  width = "100%"),
        actionButton(ns("btn_reset_server_dir"), "Re-set directory"),
        actionButton(ns("btn_load_server_dir"), "Load directory")
    ),
    br(),
    shinyjs::hidden(
      div(id = ns("sample_set_box"),
      box(width=12,
        selectizeInput(ns("sample_sets"), label = "Select sample set", choices = NULL),
        htmlOutput(ns("info_samples")),
        br(),
        rhandsontable::rHandsontableOutput(ns("table")),
        actionButton(ns("btn_save_table"),"Save table"),
        shinyjs::hidden(textInput(ns("txt_rename_sample_set"), label = "New name")),
        actionButton(ns("btn_rename_sample_set"),"Rename sample set"),
        actionButton(ns("btn_remove_sample_set"),"Remove sample set")
    )))
  )
}


#' Server part of pavian data input module
#'
#' @param input Scoped input.
#' @param output Module output.
#' @param session Shiny session.
#' @param ... Additional arguments for rhandsontable, such as height and width.
#' @param example_dir Directory with report files that can be loaded.
#' @param pattern File name pattern for definition file.
#' @param cache_tree \code{boolean}. Whether the file tree should be cached (currently not implemented).
#'
#' @return Shiny module server function, to be called by callModule.
#' @export
dataInputModule <- function(input, output, session,
                            ...,
                            example_dir = NULL,
                            pattern = "defs.csv$",
                            cache_tree = TRUE) {

  if (is.null(example_dir))
    example_dir <- system.file("shinyapp","example-data", package = "pavian")

  updateTextInput(session, "txt_data_dir", value = example_dir)

  sample_sets <- reactiveValues(val = data.frame())

  read_server_directory <- function(data_dir) {
    shinyjs::show("sample_set_box")
    dirs <- list.dirs(data_dir, recursive = FALSE)
    new_sample_sets <- lapply(list.dirs(data_dir, recursive = FALSE), get_reports_def_df)
    names(new_sample_sets) <- basename(dirs)
    new_sample_sets <- new_sample_sets[! sapply(new_sample_sets, is.null) ]

    new_sample_sets[[basename(data_dir)]] <- get_reports_def_df(data_dir)

    validate(need(new_sample_sets, message = "No sample sets available. Set a different directory"))
    sample_sets$val <<- c(sample_sets$val[!names(sample_sets$val) %in% names(new_sample_sets)], new_sample_sets)
    updateSelectizeInput(session, "sample_sets", choices = names(sample_sets$val), selected = names(new_sample_sets)[1])
  }

  observeEvent(input$btn_load_server_dir, {
    withProgress(message = "Reading server directory ...", {
      read_server_directory(input$txt_data_dir)
    })

  })

  update_sample_set_hot <- reactive({
    req(input$table)
    req(input$sample_sets)
    str(rhandsontable::hot_to_r(input$table))
    sample_sets$val[[input$sample_sets]] <<- rhandsontable::hot_to_r(input$table)
  })


  observeEvent(input$file_upload, {
    #update_sample_set_hot()
    inFile <- input$file_upload

    for (i in seq_along(inFile$datapath)) {
      dirname <- dirname(inFile$datapath[i])
      file.rename(inFile$datapath[i], file.path(dirname, inFile$name[i]))
    }

    updateTextInput(session, "txt_data_dir", value = dirname(inFile$datapath[1]))
    read_server_directory(dirname(inFile$datapath[1]))
  })

  observeEvent(input$btn_read_server_directory, {
    #update_sample_set_hot()
    validate(need(input$txt_data_dir, message = "Need input$txt_data_dir."),
             need(dir.exists(input$txt_data_dir), message = "Need input$txt_data_dir as directory."))

    read_server_directory(input$txt_data_dir)
  })

  get_def_df <- reactive({
    validate(need(sample_sets$val, message = "Need samples sets"))
    sample_sets$val[[input$sample_sets]]
  })

  report_files <- reactive({
    def_df <- get_def_df()
    validate(need(def_df, message = "Need def df."))
    def_df$ReportFilePath
  })

  output$table <- renderRHandsontable({
    def_df <- get_def_df()
    validate(need(def_df, message = "Need def df."))

    def_df$Include[! file.exists(report_files()) ] <- FALSE

    rh <- rhandsontable(def_df, readOnly = TRUE, manualRowMove = TRUE) %>%
      hot_col("Include", readOnly = FALSE) %>%
      hot_col("Name", readOnly = FALSE)
    if ("Class" %in% colnames(def_df))
      rh <- rh %>% hot_col("Class", readOnly = FALSE)

    rh
  })

  observeEvent(input$btn_save_table, {
    update_sample_set_hot()
  })

  currently_renaming_sample_set <- FALSE

  observeEvent(input$btn_rename_sample_set, {
    shinyjs::toggle("txt_rename_sample_set")

    if (currently_renaming_sample_set) {
      selected_item <- names(sample_sets$val) == input$sample_sets
      names(sample_sets$val)[selected_item] <<- input$txt_rename_sample_set
      updateSelectizeInput(session, "sample_sets", choices = names(sample_sets$val), selected = names(sample_sets$val)[selected_item])
    } else {
      updateTextInput(session, "txt_rename_sample_set", value = input$sample_sets)
    }

    currently_renaming_sample_set <<- !currently_renaming_sample_set
  })

  observeEvent(input$btn_remove_sample_set, {
    selected_item <- names(sample_sets$val) == input$sample_sets
    sample_sets$val <<- sample_sets$val[!selected_item]
    updateSelectizeInput(session, "sample_sets", choices = names(sample_sets$val), selected = names(sample_sets$val)[1])
    if (length(sample_sets) == 0) {
      shinyjs::hide("sample_set_box")
    }
  })

  output$info_samples <- renderText({
    sprintf("<span class='background:#00ff00'>Got %s report files. </span>",
            sum(file.exists(report_files())))
  })

  return(function() {
    attr(sample_sets$val, "selected") <<- input$sample_sets
    sample_sets
  })
}


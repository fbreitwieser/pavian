#' UI part of pavian data input module
#'
#' @param id Namespace ID
#'
#' @return Shiny UI elements
#' @export
#' @import shiny
#' @import shinydashboard
#' @import rhandsontable
dataInputModuleUI <- function(id, server_access = TRUE) {

  ns <- NS(id)

  #radio_placeholder <-

  shiny::tagList(
    box(width=12,
        title = "Data Input",
        background = "green",
        collapsible = TRUE,
        collapse = TRUE,
        HTML(
        "Pavian supports Kraken and Centrifuge report files. You can either upload files, select a directory on the server, or load the example data set. The example data is from brain biopsies from <a style='color:white; text-decoration: underline;' href='http://nn.neurology.org/content/3/4/e251.full'>ten patients with suspected infection of the nervous system </a>."),
        {if (isTRUE(server_access)) {
          radioButtons(ns('upload_or_server'), label="", inline=TRUE,
                       choices=c("Upload files"="upload",
                                 "Use data on server"="server",
                                 "Load example data"="example_data"),
                       selected="example_data")
        } },
        fileInput(ns("file_upload"), "Select files on local machine for upload", multiple = TRUE),
        {if (isTRUE(server_access)) {
          shiny::tagList(
            shinyjs::hidden(textInput(ns("txt_data_dir"), label = "Specify directory on machine running Pavian")),
            shinyjs::hidden(actionButton(ns("read_server_dir"), label = "Read directory content", width = "250px"))
            #shinyFiles::shinyDirButton(ns('txt_data_dir'),
            #                           label='Select directory on server',
            #                           title='Please select a directory (on the server)')
          )
        }},
        textOutput(ns("upload_info"))
    ),
    br(),

    box(width=12,
        title = "Available sample sets",
        shinyjs::hidden(div(id = ns("sample_set_box"),
      radioButtons(ns("sample_set_select"), label = "", choices = list(PLACEHOLDER=1)),
      shinyjs::hidden(textInput(ns("txt_rename_sample_set"), label = "New name")),
      actionButton(ns("btn_rename_sample_set"),"Rename selected sample set"),
      actionButton(ns("btn_remove_sample_set"),"Remove selected sample set")
      ),
    hr()),
    shinyjs::hidden(
      div(id = ns("sample_set_table"),
        htmlOutput(ns("info_samples")),
        br(),
        rhandsontable::rHandsontableOutput(ns("table")),
        actionButton(ns("btn_save_table"),"Save table")
      )
    )
    )
  )
}


#' Server part of pavian data input module
#'
#' @param input Scoped input.
#' @param output Module output.
#' @param session Shiny session.
#' @param ... Additional arguments for rhandsontable, such as height and width.
#' @param server_dir Directory with report files that can be loaded.
#' @param pattern File name pattern for definition file.
#' @param cache_tree \code{boolean}. Whether the file tree should be cached (currently not implemented).
#'
#' @return Shiny module server function, to be called by callModule.
#' @export
dataInputModule <- function(input, output, session,
                            ...,
                            server_dirs = c(pavian_lib_dir=system.file("shinyapp", "example-data", package = "pavian"),
                                            root = "/home/fbreitwieser"),
                            pattern = "sample_data.csv$",
                            cache_tree = TRUE) {

  sample_sets <- reactiveValues(val = data.frame())


  ns <- session$ns
  #shinyFiles::shinyDirChoose(input, ns('txt_data_dir'), roots = server_dirs, filetypes = c(""))

  read_error_msg <- reactiveValues(val=NULL)

  output$upload_info <- renderText({
    read_error_msg$val
  })

  read_server_directory <- function(data_dir, sample_set_name = NULL) {
    message("reading files in ", data_dir)
    if (!dir.exists(data_dir)) {
      read_error_msg$val <- paste("Directory ", data_dir, "does not exist.")
      return()
    }
    if (length(list.files(data_dir)) == 0) {
      read_error_msg$val <- paste("No files in directory ", data_dir, ".")
      return()
    }
    read_error_msg$val <- NULL


    if (!is.null(sample_set_name)) {
      old_names <- names(sample_sets$val)
      counter <- 1

      while (paste(sample_set_name,counter) %in% old_names) {
        counter <- counter + 1
      }
      sample_set_name <- paste(sample_set_name, counter)
    }

    new_sample_sets <- list(read_sample_data(data_dir))
    names(new_sample_sets) <- ifelse(!is.null(sample_set_name),
                                     sample_set_name,
                                     basename(data_dir))


    dirs <- list.dirs(data_dir, recursive = FALSE)
    if (length(dirs) > 0) {
      sub_dir_sets <- lapply(list.dirs(data_dir, recursive = FALSE), read_sample_data)
      names(sub_dir_sets) <- paste0(names(new_sample_sets),"/",basename(dirs))
      new_sample_sets <- c(new_sample_sets, sub_dir_sets)
    }
    new_sample_sets <- new_sample_sets[! sapply(new_sample_sets, is.null) ]

    validate(need(new_sample_sets, message = "No sample sets available. Set a different directory"))
    sample_sets$val <<- c(sample_sets$val[!names(sample_sets$val) %in% names(new_sample_sets)], new_sample_sets)
    updateRadioButtons(session, "sample_set_select", choices = names(sample_sets$val), selected = names(new_sample_sets)[1])

    shinyjs::show("sample_set_box")
    shinyjs::show("sample_set_table")
  }

  observeEvent(input$upload_or_server, {
    if (input$upload_or_server == "server") {
      shinyjs::hide("file_upload")
      shinyjs::show("txt_data_dir")
      shinyjs::show("read_server_dir")
    } else if (input$upload_or_server == "upload" ) {
      shinyjs::show("file_upload")
      shinyjs::hide("txt_data_dir")
      shinyjs::hide("read_server_dir")
    } else {
      shinyjs::hide("file_upload")
      shinyjs::hide("txt_data_dir")
      shinyjs::hide("read_server_dir")

      withProgress(message = "Reading example directory ...", {
        read_server_directory(system.file("shinyapp", "example-data", package = "pavian"))
      })
    }
  })

  observeEvent(input$read_server_dir, {
    req(input$txt_data_dir)
    withProgress(message = "Reading server directory ...", {
      read_server_directory(input$txt_data_dir, "Sample set")
    })
  })

  observeEvent(input$btn_load_example_dir, {
    withProgress(message = "Reading example directory ...", {
      read_server_directory(system.file("shinyapp", "example-data", package = "pavian"))
    })
  })

  update_sample_set_hot <- reactive({
    req(input$table)
    req(input$sample_set_select)
    #str(rhandsontable::hot_to_r(input$table))
    sample_sets$val[[input$sample_set_select]] <<- rhandsontable::hot_to_r(input$table)
  })


  observeEvent(input$file_upload, {
    #update_sample_set_hot()
    inFile <- input$file_upload

    for (i in seq_along(inFile$datapath)) {
      dirname <- dirname(inFile$datapath[i])
      file.rename(inFile$datapath[i], file.path(dirname, inFile$name[i]))
    }

    read_server_directory(dirname(inFile$datapath[1]), "Uploaded sample set")
  })

  get_def_df <- reactive({
    validate(need(sample_sets$val, message = "Need samples sets"))
    sample_sets$val[[input$sample_set_select]]
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
      selected_item <- names(sample_sets$val) == input$sample_set_select
      names(sample_sets$val)[selected_item] <<- input$txt_rename_sample_set
      updateRadioButtons(session, "sample_set_select", choices = names(sample_sets$val), selected = names(sample_sets$val)[selected_item])
    } else {
      updateTextInput(session, "txt_rename_sample_set", value = input$sample_set_select)
    }

    currently_renaming_sample_set <<- !currently_renaming_sample_set
  })

  observeEvent(input$btn_remove_sample_set, {
    selected_item <- names(sample_sets$val) == input$sample_set_select
    sample_sets$val <<- sample_sets$val[!selected_item]
    updateRadioButtons(session, "sample_set_select", choices = names(sample_sets$val), selected = names(sample_sets$val)[1])
    if (length(sample_sets$val) == 0) {
      shinyjs::hide("sample_set_box")
      shinyjs::hide("sample_set_table")
    }
  })

  output$info_samples <- renderText({
    sprintf("<span class='background:#00ff00'>Got %s report files. </span>",
            sum(file.exists(report_files())))
  })

  return(function() {
    attr(sample_sets$val, "selected") <<- input$sample_set_select
    sample_sets
  })
}


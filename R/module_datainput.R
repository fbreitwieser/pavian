#' UI part of pavian data input module
#'
#' @param id Namespace ID.
#' @param server_access Whether to allow access to server directories.
#' @param start_with One of 'example_data', 'upload', and 'server' (if server_access is TRUE).
#'
#' @return Shiny UI elements
#' @export
#' @import shiny
#' @import shinydashboard
#' @import rhandsontable
dataInputModuleUI <- function(id, server_access = TRUE, start_with="example_data") {

  ns <- NS(id)

  data_options <- c("Upload files"="upload",
                    "Use data on server"="server",
                    "Load example data"="example_data")
  if (!server_access)
    data_options <- data_options[-2]

  #radio_placeholder <-

  shiny::tagList(
    box(width=12,
        title = "Data Input",
        background = "green",
        collapsible = TRUE,
        collapse = TRUE,
        HTML(
        "Pavian supports Kraken, Centrifuge and MetaPhlAn report files. Please note that currently for Centrifuge you need to run the script <tt>centrifuge-kreport</tt> on the result file to get the correct format! You can either upload files, select a directory on the server, or load the example data set. The example-data/brain-biopsies from <a style='color:white; text-decoration: underline;' href='http://nn.neurology.org/content/3/4/e251.full'>ten patients with suspected infection of the nervous system</a> that were analyzed with Kraken. The example-data/hmp is a couple of samples from the <a style='color:white; text-decoration: underline;' href='http://hmpdacc.org/'>Human Microbiome Project</a> analyzed with MetaPhlAn."),
        {if (isTRUE(server_access)) {
          radioButtons(ns('upload_or_server'), label="", inline=TRUE,
                       choices=data_options,
                       selected=start_with)
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
#' @param pattern File name pattern for definition file.
#' @param cache_tree \code{boolean}. Whether the file tree should be cached (currently not implemented).
#'
#' @return Shiny module server function, to be called by callModule.
#' @export
dataInputModule <- function(input, output, session,
                            #server_dirs = c(pavian_lib_dir=system.file("shinyapp", "example-data", package = "pavian"),
                            #                root = "/home/fbreitwieser"),
                            pattern = "sample_data.csv$",
                            cache_tree = TRUE) {

  sample_sets <- reactiveValues(val = data.frame())


  ns <- session$ns
  #shinyFiles::shinyDirChoose(input, ns('txt_data_dir'), roots = server_dirs, filetypes = c(""))

  read_error_msg <- reactiveValues(val=NULL)

  output$upload_info <- renderText({
    read_error_msg$val
  })

  read_server_directory <- function(data_dir, sample_set_name = NULL,
                                    include_base_dir = T) {
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

    base_name <- ifelse(!is.null(sample_set_name),
                                       sample_set_name,
                                       basename(data_dir))
    new_sample_sets <- list()
    if (include_base_dir) {
      new_sample_sets <- list(read_sample_data(data_dir, ext=NULL))
      names(new_sample_sets) <- base_name
    }

    dirs <- list.dirs(data_dir, recursive = FALSE)
    if (length(dirs) > 0) {
      sub_dir_sets <- lapply(list.dirs(data_dir, recursive = FALSE),
                             read_sample_data,
                             ext=NULL)
      names(sub_dir_sets) <- paste0(base_name,"/",basename(dirs))
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
        read_server_directory(system.file("shinyapp", "example-data", package = "pavian"),
                              include_base_dir = FALSE)
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

  get_sample_data <- reactive({
    validate(need(sample_sets$val, message = "Need samples sets"))
    sample_sets$val[[input$sample_set_select]]
  })

  report_files <- reactive({
    sample_data <- get_sample_data()
    validate(need(sample_data, message = "Need def df."))
    sample_data$ReportFilePath
  })

  output$table <- renderRHandsontable({
    sample_data <- get_sample_data()
    validate(need(sample_data, message = "Need def df."))

    sample_data$FormatOK <- sapply(report_files(),
                          function(x) length(read_report(x, check_file=T)) != 0)
    sample_data$Include[!sample_data$FormatOK] <- FALSE

    #sample_data$FormatOK <- ifelse(sample_data$FormatOK,
    #                               "<font color='green'>&#x2713;</font>",
    #                               "<font color='red'>&#x2717;</font>")

    sample_data <- sample_data[,c("FormatOK",setdiff(colnames(sample_data),"FormatOK"))]

    rh <- rhandsontable(sample_data, readOnly = TRUE, manualRowMove = TRUE) %>%
      hot_col("Include", renderer = "
    function(instance, td, row, col, prop, value, cellProperties) {
      cellProperties.readOnly = !value;
      Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
      return td;
    }") %>%
     hot_col("FormatOK", renderer = "
    function(instance, td, row, col, prop, value, cellProperties) {
      Handsontable.renderers.TextRenderer.apply(this, arguments);
      if (value ) {
        value = '&#x2713';
        td.style.color = 'green';
      } else {
        value = '&#x2717';
        td.style.color = 'red';
        cellProperties.comment = 'The file format does not validate. Pavian supports the outputs from kraken-report, centrifuge-kreport (but not the centrifuge --report-file!), and metaphlan2.py. You can create a valid centrifuge report with centrifuge-kreport -x IDX OUT_FILE.';
      }
      return td;
    }") %>%
      hot_col("Name", readOnly = FALSE)
    if ("Class" %in% colnames(sample_data))
      rh <- rh %>% hot_col("Class", readOnly = FALSE)

    rh %>% hot_table(enableComments = TRUE, highlightRow = TRUE)
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


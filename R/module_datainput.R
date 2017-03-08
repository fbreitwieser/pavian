library(shiny)
library(rappdirs)

serverDataPanel <- function (ns) {
  tabPanel("Use data on server", id="server_dir",
           "Be careful which directory you select - if there are too many files, the process might hang.",
           " Pavian will check the specified directory and its direct children for report files.",br(),
           textInput(ns("txt_data_dir"),  width= "100%",
                     label = "Specify directory on machine running Pavian",
                     value = getOption("pavian.server_dir","")),
           actionButton(ns("read_server_dir"), label = "Read directory content", width = "250px"),
           uiOutput(ns('rud'))
  )
}

exampleDataPanel <- function(ns) {
  tabPanel("Example data",
           HTML("Two example datasets are available: <i>brain-biopsies</i> and <i>hmp-stool</i>. The first set is
        from <a href='http://nn.neurology.org/content/3/4/e251.full'>ten
        patients with suspected infections of the nervous system</a>, analyzed with Kraken. The second set is sequenced stool
        from the <a href='http://hmpdacc.org/'>Human Microbiome Project</a>,
        analyzed with MetaPhlAn. Note that for MetaPhlAn, the values are percentages/abundances rather than reads."),
           br(),br(),
           actionButton(ns("btn_load_example_data"), label = "Load example datasets")
  )
}

uploadFilePanel <- function(ns) {
  tabPanel("Upload files",fileInput(ns("file_upload"), width = "600px", "", multiple = TRUE))
}

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
dataInputModuleUI <- function(id,
                              server_access = TRUE,
                              start_with=getOption("pavian.start_data_input_with","Example data")) {

  ns <- NS(id)

  shiny::tagList(
    box(width=12,
        #collapsible = TRUE,
        #collapse = TRUE,
        HTML(
          "
<h2>Pavian metagenomics data explorer</h2>

<p>
Pavian is a tool for interactive analysis of metagenomics data. You can read more about it in the <a target='blank' href='http://biorxiv.org/content/early/2016/10/31/084715.full.pdf+html'>Preprint</a> or its <a target='blank' href='https://raw.githubusercontent.com/fbreitwieser/pavian/blob/master/inst/doc/pavian-walkthrough.pdf'>vignette</a>. It's built on <a href='https://www.r-project.org/' target='blank'>R</a> and <a target='blank' href='http://shiny.rstudio.com/'>Shiny</a>, and supports <a target='blank' href='https://ccb.jhu.edu/software/kraken/'>Kraken</a>, <a target='blank' href='https://github.com/infphilo/centrifuge'>Centrifuge</a> and <a target='blank' href='https://bitbucket.org/biobakery/metaphlan2'>MetaPhlAn</a> report files. Please note that currently the default Centrifuge report format is not supported. To generate a compatible report, use the script <tt>centrifuge-kreport</tt> that is distributed with Centrifuge. Further note that you can compare Kraken and Centrifuge results, but not Kraken or Centrifuge with MetaPhlAn results, as the naming and taxonomy is different.
</p>

<p>
For help, and to report an issue with the tool, please go to <a target='blank' href='https://github.com/fbreitwieser/pavian'>https://github.com/fbreitwieser/pavian</a>.
</p>")
    ),
    {if (server_access) {
      tabBox(width=12,
             title = "Data Source", selected=start_with,
             uploadFilePanel(ns),
             serverDataPanel(ns),
             exampleDataPanel(ns)
             )
    } else {
      tabBox(width=12,
             title = "Data Source", selected=start_with,
             uploadFilePanel(ns),
             exampleDataPanel(ns)
      )
    }
    },
    uiOutput(ns("upload_info")),
    br(),
    uiOutput(ns("uploaded_sample_sets"))
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
                            #pattern = "sample_data.csv$",
                            cache_tree = TRUE) {

  sample_sets <- reactiveValues(val = list(),         # val is the list of all sample sets
                                selected = NULL,      # selected is just used to initialize the radioButtons in the module
                                selected_set = NULL)  # selected_set is the name of the currently selected set, and is given outwards


  ns <- session$ns
  recently_used_dir_user_config <- file.path(user_config_dir("pavian", expand = FALSE), "recently_used_dirs.txt")
  recently_used_dirs <- reactiveValues(val=NULL)
  if (file.exists(recently_used_dir_user_config)) {
    recently_used_dirs$val <- readLines(recently_used_dir_user_config)
  }



  #shinyFiles::shinyDirChoose(input, ns('txt_data_dir'), roots = server_dirs, filetypes = c(""))

  read_error_msg <- reactiveValues(val_pos=NULL, val_neg=NULL)

  output$upload_info <- renderUI({
    req(!is.null(read_error_msg$val_pos) || !is.null(read_error_msg$val_neg))
    box(width=12,
      div(HTML(read_error_msg$val_pos), style="color:green"),
      div(HTML(paste(read_error_msg$val_neg, collapse=" ")), style="color:red")
    )
  })

  read_server_directory <- function(data_dir, sample_set_name = NULL,
                                    include_base_dir = T) {
    read_error_msg$val_neg <- NULL
    read_error_msg$val_pos <- NULL

    message("reading files in ", data_dir)
    if (!dir.exists(data_dir)) {
      read_error_msg$val_neg <- paste("Directory ", data_dir, "does not exist.")
      return(FALSE)
    }
    if (length(list.files(data_dir)) == 0) {
      read_error_msg$val_neg <- paste("No files in directory ", data_dir, ".")
      return(FALSE)
    }
    n_files <- length(list.files(data_dir))
    max_files <- getOption("pavian.maxFiles", 50)
    if (n_files > max_files) {
      read_error_msg$val_neg <- paste("There are ",n_files," in the directory, but the highest allowed number is ",max_files," files ", data_dir, " - please subdivide the data into smaller directories, or set the option 'pavian.maxFiles' to a higher number (e.g. 'options(pavian.maxFiles=250)').")
      return(FALSE)
    }

    my_sample_sets <- isolate(sample_sets$val)

    if (!is.null(sample_set_name)) {
      old_names <- names(my_sample_sets)
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

    dirs <- grep("^\\.", list.dirs(data_dir, recursive = FALSE), invert = TRUE, value = TRUE)
    n_dirs <- length(dirs)
    max_dirs <- getOption("pavian.maxSubDirs", 25)
    if (n_dirs > max_dirs) {
      read_error_msg$val_neg <- c(read_error_msg$val_neg, paste("There are ",n_dirs," sub-directories in ", data_dir,
                                                                " but the highest allowed number is ",max_dirs,"  - specify individual directories with reports one at a time to load data or set the option 'pavian.maxSubDirs' to a higher number (e.g. 'options(pavian.maxSubDirs=50)')."))
    } else if (length(dirs) > 0) {
      sub_dir_sets <- lapply(dirs, read_sample_data, ext=NULL)
      names(sub_dir_sets) <- paste0(base_name,"/",basename(dirs))
      new_sample_sets <- c(new_sample_sets, sub_dir_sets)
    }

    bad_files <- unlist(sapply(new_sample_sets, attr, "bad_files"))
    sel_bad_sets <- sapply(new_sample_sets, function(x) is.null(x) || nrow(x) == 0)
    bad_sample_set_names <- names(new_sample_sets)[sel_bad_sets]
    new_sample_sets <- new_sample_sets[!sel_bad_sets]

    if (length(new_sample_sets) > 0) {
      read_error_msg$val_pos <- sprintf("Added sample set%s <b>%s</b> with <b>%s</b> valid reports in total.",
                                        ifelse(length(new_sample_sets) == 1, "", "s"),
                                        paste(names(new_sample_sets), collapse="</b>, <b>"),
                                        sum(unlist(sapply(new_sample_sets, function(x) sum(x$FormatOK)))))
    }
    if (length(bad_files) > 0) {
      read_error_msg$val_neg <- c(read_error_msg$val_neg,
                                  sprintf("The following files did not conform the report format: <br/> - <b>%s</b>",
                                          paste(bad_files, collapse="</b><br/> - <b>")))
    }

    if (is.null(read_error_msg$val_pos))
      return(FALSE)

    validate(need(new_sample_sets, message = "No sample sets available. Set a different directory"))
    sample_sets$val <- c(my_sample_sets[!names(my_sample_sets) %in% names(new_sample_sets)], new_sample_sets)
    sample_sets$selected <- names(new_sample_sets)[1]
    return(TRUE)
  }

  output$uploaded_sample_sets <- renderUI({
    req(sample_sets$val)
    req(isolate(sample_sets$selected))
    message("UI")
    box(width=12, collapsible = TRUE,
        title = "Uploaded sample sets",
        status="primary",

        column(6,radioButtons(ns("sample_set_select"), label = NULL,
                              choices = names(sample_sets$val), selected = isolate(sample_sets$selected))),
        column(6,
               shinyjs::hidden(textInput(ns("txt_rename_sample_set"), label = "New name")),
               actionButton(ns("btn_rename_sample_set"),"Rename selected sample set"),
               actionButton(ns("btn_remove_sample_set"),"Remove selected sample set")
        ),
        br(),
        rhandsontable::rHandsontableOutput(ns("table")),
        actionButton(ns("btn_save_table"),"Save table (required to make changes persistent)")
    )
  })

  observeEvent(input$btn_load_example_data, {
    withProgress(message = "Reading example directory ...", {
      read_server_directory(system.file("shinyapp", "example-data", package = "pavian"),
                            include_base_dir = FALSE)
    })
  })

  observeEvent(input$read_server_dir, {
    req(input$txt_data_dir)
    res <- withProgress(message = "Reading server directory ...", {
      read_server_directory(input$txt_data_dir)
    })
    if (res && !input$txt_data_dir %in% recently_used_dirs$val) {
      recently_used_dirs$val <- c(input$txt_data_dir,recently_used_dirs$val)
      writeLines(recently_used_dirs$val, recently_used_dir_user_config)
    }
  })

  output$rud <- renderUI({
    req(recently_used_dirs$val)

    shiny::tagList(br(),
                   "Recently used directories: ",lapply(seq(from=1, to=min(length(recently_used_dirs$val),5)),
           function(i) actionLink(ns(paste0("rud_",i)), recently_used_dirs$val[i])))

  })

  observeEvent(input$rud_1, { updateTextInput(session, "txt_data_dir", value = recently_used_dirs$val[1]) })
  observeEvent(input$rud_2, { updateTextInput(session, "txt_data_dir", value = recently_used_dirs$val[2]) })
  observeEvent(input$rud_3, { updateTextInput(session, "txt_data_dir", value = recently_used_dirs$val[3]) })
  observeEvent(input$rud_4, { updateTextInput(session, "txt_data_dir", value = recently_used_dirs$val[4]) })
  observeEvent(input$rud_5, { updateTextInput(session, "txt_data_dir", value = recently_used_dirs$val[5]) })




  update_sample_set_hot <- reactive({
    req(input$table)
    req(input$sample_set_select)
    #str(rhandsontable::hot_to_r(input$table))
    tryCatch({
      old_df <- sample_sets$val[[input$sample_set_select]]
      new_df <- rhandsontable::hot_to_r(input$table)

      if (!isTRUE(all.equal(old_df, new_df))) {
        sample_sets$val[[input$sample_set_select]] <<- new_df
      }
    }, error = function(e) message("Error calling hot_to_r!"))
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
    validate(need(sample_data, message = "Need sample data for table."))

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
    read_error_msg$val_pos <- NULL
    read_error_msg$val_neg <- NULL
    if (length(sample_sets$val) == 1) {
      sample_sets$val <- list()
      sample_sets$selected <- NULL
    } else {
      sample_sets$val <- sample_sets$val[!selected_item]
      sample_sets$selected <- names(sample_sets$val)[1]
    }
  })

  #output$info_samples <- renderText({
  #  sprintf("<span class='background:#00ff00'>Got %s report files. </span>",
  #          sum(file.exists(report_files())))
  #})

  return(reactive( {
    sample_sets$selected_set <- input$sample_set_select
    sample_sets
  }))
}


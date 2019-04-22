#vim: noai:ts=2:sw=2 
  
library(shiny)
library(rhandsontable)
library(shinyFileTree)

serverDataPanel <- function(ns) {
  tabPanel(
    "Use data on server",
    id = "server_dir",
    " Select a directory in the file browser and press 'Read selected directories' to load it into Pavian.",
    " You may also use wildcards to directly upload specified paths.",
    br(),
    div(id="server_data_dir_div",
    shinyWidgets::searchInput(ns("search_data_dir"),
                              label = "Specify directory on machine running Pavian",
                              value = getOption("pavian.server_dir", ""),
                              btnReset = icon("level-up", lib="glyphicon"),
                              resetValue = NULL,
                              width = "100%",
                              btnSearch = icon("server"))),
    div(style="max-height:400px; overflow-y: scroll",
      shinyFileTree::shinyFileTreeOutput(ns('file_tree'))
    ),
    shinyjs::hidden(actionButton(ns("btn_read_tree_dirs"), "Read selected directories")),
    uiOutput(ns('rud'))
  )
}

exampleDataPanel <- function(ns) {
  tabPanel(
    "Example data",
    HTML(
      "Two example datasets are available: <i>brain-biopsies</i> and <i>hmp-stool</i>. The first set is
      from <a href='http://nn.neurology.org/content/3/4/e251.full'>ten
      patients with suspected infections of the nervous system</a>, analyzed with Kraken. The second set is sequenced stool
      from the <a href='http://hmpdacc.org/'>Human Microbiome Project</a>,
      analyzed with MetaPhlAn. Note that for MetaPhlAn, the values are percentages/abundances rather than reads."
    ),
    br(),
    br(),
    {if (!dir.exists(system.file("shinyapp", "example-data", package = "pavian"))) {
      shinydashboard::box("Example file directory is not present, cannot load example files.")
    } else {
      actionButton(ns("btn_load_example_data"), label = "Load example datasets")
     }
    }
  )
}

uploadFilePanel <- function(ns) {
  tabPanel("Upload files",
           "Upload metagenomics report files from the local computer. If selecting multiple files does not work, please
            try with a different browser. With each sample set, you may also include meta-data with a colon-separated sample_data.csv file 
            that has at least the columns 'Name' and 'ReportFile'.",
           fileInput(
             ns("file_upload"),
             label="",
             placehold = "Upload report files",
             width = "600px",
             multiple = TRUE
           ))
}

#' UI part of pavian data input module
#'
#' @param id Namespace ID.
#' @param server_access Whether to allow access to server directories.
#' @param start_with One of 'Example data', 'Upload files', and 'Use data on server' (if server_access is TRUE).
#'
#' @return Shiny UI elements
#' @export
#' @import shiny
#' @import shinydashboard
#' @import rhandsontable
dataInputModuleUI <- function(id,
                              server_access = getOption("pavian.server_access", default = FALSE),
                              start_with = getOption("pavian.start_data_input_with", "Upload files")) {
  ns <- NS(id)
  
  shiny::tagList(box(
    width = 12,
    #collapsible = TRUE,
    #collapse = TRUE,
    HTML(
      "
      <p>
      Pavian is a tool for interactive analysis of metagenomics classification results. 
    Read more about it in the <a target='blank' href='http://biorxiv.org/content/early/2016/10/31/084715.full.pdf+html'>Preprint</a> or its <a target='blank' href='https://raw.githubusercontent.com/fbreitwieser/pavian/blob/master/inst/doc/pavian-walkthrough.pdf'>vignette</a>. It's built on <a href='https://www.r-project.org/' target='blank'>R</a> and <a target='blank' href='http://shiny.rstudio.com/'>Shiny</a>, and supports <a target='blank' href='https://ccb.jhu.edu/software/kraken/'>Kraken</a>, <a target='blank' href='https://github.com/infphilo/centrifuge'>Centrifuge</a> and <a target='blank' href='https://bitbucket.org/biobakery/metaphlan2'>MetaPhlAn</a> report files. Please note that currently the default Centrifuge report format is not supported. To generate a compatible report, use the script centrifuge-kreport that is distributed with Centrifuge. 
      </p>
      <p>
      For help and more documenation please go to <a target='blank' href='https://github.com/fbreitwieser/pavian'>https://github.com/fbreitwieser/pavian</a>.
      </p>"
    )
  ),
  {
    if (server_access) {
      shinydashboard::tabBox(
        width = 12,
        title = "Data Source",
        selected = start_with,
        uploadFilePanel(ns),
        serverDataPanel(ns),
        exampleDataPanel(ns)
      )
    } else {
      shinydashboard::tabBox(
        width = 12,
        title = "Data Source",
        selected = start_with,
        uploadFilePanel(ns),
        exampleDataPanel(ns)
      )
    }
  },
  uiOutput(ns("upload_info")),
  br(),
  uiOutput(ns("uploaded_sample_sets")))
}


#' Server part of pavian data input module.
#'
#' @param input Scoped input.
#' @param output Module output.
#' @param session Shiny session.
#' @param config_dir Directory for configuration files.
#' @param server_access Whether a directory on the server can be loaded.
#' @param load_server_directory Load server directory.
#' @param load_example_data Load example data.
#' @param pavian_options General options for pavian.
#'
#' @return Shiny module server function, to be called by callModule.
#' @export
dataInputModule <- function(input, output, session,
                            #server_dirs = c(pavian_lib_dir=system.file("shinyapp", "example-data", package = "pavian"),
                            #                root = "/home/fbreitwieser"),
                            config_dir = NULL,
                            server_access = getOption("pavian.server_access", default = FALSE),
                            load_server_directory = getOption("pavian.load_server_directory", default = FALSE),
                            load_example_data = getOption("pavian.load_example_data", default = FALSE),
                            pavian_options = NULL) {
  
  sample_sets <- reactiveValues(val=NULL, selected=NULL) # val is the list of all sample sets
  sample_sets_selected <- NULL # selected is just used to initialize the radioButtons in the module
  
  ns <- session$ns
  
  ## Save and retrieve recently used directories
  recently_used_dirs <- reactiveValues(val = NULL)
  recently_used_dir_user_config <- NULL
  if (!is.null(config_dir) && dir.exists(config_dir)) {
    recently_used_dir_user_config <- file.path(config_dir, "recently_used_dirs.txt")
    if (file.exists(recently_used_dir_user_config)) {
      recently_used_dirs$val <- readLines(recently_used_dir_user_config)
    }
  }

  observeEvent(pavian_options$server_dir, {
    req(pavian_options$server_dir)
    # require(shinyWidgets)
    #req(exists('updateSearchInput', where='package:shinyWidgets', mode='function'))
    tryCatch({
      shinyWidgets::updateSearchInput(session, "search_data_dir", value=pavian_options$server_dir)
    }, error = message)
  })
  
  #shinyFiles::shinyDirChoose(input, ns('search_data_dir'), roots = server_dirs, filetypes = c(""))
  
  read_error_msg <- reactiveValues(val_pos = NULL, val_neg = NULL)
  
  output$upload_info <- renderUI({
    req(!is.null(read_error_msg$val_pos) ||
          !is.null(read_error_msg$val_neg))
    box(width = 12,
        div(HTML(read_error_msg$val_pos), style = "color:green"),
        div(HTML(
          paste(read_error_msg$val_neg, collapse = " ")
        ), style = "color:red"))
  })
  
  
  output$uploaded_sample_sets <- renderUI({
    req(sample_sets$val)
    req(names(sample_sets$val))
    sample_set_names <- sort(names(sample_sets$val))
    box(
      width = 12,
      collapsible = TRUE,
      title = "Available sample sets",
      status = "primary",
      
      column(
        6,
        div(class = "styled-radios",
        radioButtons(
          ns("sample_set_select"),
          label = NULL,
          choices = sample_set_names,
          selected = ifelse(is.null(sample_sets$selected), sample_set_names[1], sample_sets$selected)
        )
        )
      ),
      column(
        6,
        shinyjs::hidden(textInput(ns(
          "txt_rename_sample_set"
        ), label = "New name")),
        actionButton(ns("btn_view_results"), "View results"),
        actionButton(ns("btn_rename_sample_set"), "Rename sample set", icon=icon("pencil")),
        actionButton(ns("btn_remove_sample_set"), label = "Remove sample set", icon=icon("trash"))
      ),
      br(),
      rhandsontable::rHandsontableOutput(ns("table")),
      p("You can specify which samples to include as well as their names. Be sure to save the table to make the changes persistent."),
      actionButton(
        ns("btn_save_table"),
        "Save table"
      )
    )
  })
  
  read_server_directory <- function(...) {
    withProgress({read_server_directory2(...)}, message = "Reading directory on server ...")
  }
  
  read_server_directory2 <-
    function(data_dir, sample_set_name = NULL, ...) {
      sample_sets_val <- isolate(sample_sets$val)
      res <-
        read_server_directory1(data_dir,
                               sample_set_name = sample_set_name,
                               existing_sample_set_names = names(sample_sets_val),
                               ...,
                               display_messages = FALSE)
      read_error_msg$val_pos <- res$error_msg$val_pos
      read_error_msg$val_neg <- res$error_msg$val_neg
      if (is.null(read_error_msg$val_pos)) {
        if (is.null(res$error_msg$val_neg)) {
          read_error_msg$val_neg <- sprintf("Unable to read report files from directory %s.", data_dir)
        }
        return(FALSE)
      }
      
      validate(
        need(res$sample_sets, message = "No sample sets available. Set a different directory")
      )
      
      sample_sets$val <-
        c(sample_sets_val, res$sample_sets[!names(res$sample_sets) %in% names(sample_sets_val)])
      sample_sets$selected <- names(res$sample_sets)[1]
      return(TRUE)
    }
  
  observeEvent(input$btn_load_example_data, {
      read_server_directory(
        system.file("shinyapp", "example-data", package = "pavian"),
        include_base_dir = FALSE
      )
  })
  
  display_tree <- reactiveValues(val = FALSE)

  file_tree_dir <- reactiveValues(val = NULL)

  #observeEvent(input$search_data_dir, {
  #})
  
  output$file_tree <- shinyFileTree::renderShinyFileTree({
    data_dir <- file_tree_dir$val
    req(data_dir)
    req(display_tree$val)
    req(length(data_dir) > 0 && nchar(data_dir) > 0)
    shinyjs::disable("btn_read_tree_dirs")
    shinyjs::show("btn_read_tree_dirs")
    shinyFileTree::shinyFileTree(list(text=data_dir,
                                      state=list(opened=TRUE),
                                      children=shinyFileTree::get_list_from_directory(data_dir,
                                                                                      max_depth=1,
                                                                                      show_dir_info=TRUE)),
                                 opts = shinyFileTree::shinyFileTreeOpts(
                                            animation = FALSE, 
                                            themes.stripes = FALSE,
                                            multiple = TRUE),
                                 plugins = c("wholerow", "types"))
  })

  observeEvent(input$file_tree_dblclick, {
    #message("doubleclick observed")
    fnames <- sub(" \\([0-9]+ f[io].*\\)$", "", input$file_tree_selected)
    if (dir.exists(fnames)) {
    tryCatch({
      shinyWidgets::updateSearchInput(session, "search_data_dir", value = fnames)
      file_tree_dir$val <- fnames
    }, error = message)
    }
  })
  
  observeEvent(input$file_tree_selected, {
    if (length(input$file_tree_selected) == 0) {
      shinyjs::disable("btn_read_tree_dirs")
    } else {
      shinyjs::enable("btn_read_tree_dirs")
    }
  })
  
  observeEvent(input$btn_read_tree_dirs, {
    fnames <- input$file_tree_selected
    fnames <- sub(" \\([0-9]+ f[io].*\\)$", "", fnames)
    if (all(startsWith(fnames, fnames[1]))) {
      fnames <- fnames[1]
    }
    res <- read_server_directory(fnames)
  })
  
  observeEvent(input$search_data_dir_reset, {
    tryCatch({
      fnames <- dirname(input$search_data_dir)
      shinyWidgets::updateSearchInput(session, "search_data_dir", value = fnames)
      file_tree_dir$val <- fnames
    }, error = message)
  })
  
  observeEvent(input$search_data_dir, {
    req(input$search_data_dir)
    do_glob <- !dir.exists(input$search_data_dir)
    display_tree$val <- !do_glob
    if (do_glob && length(Sys.glob(input$search_data_dir) == 0)) {
      shinyjs::addClass(class="red_background", selector="")
    }
    
    if (!isTRUE(display_tree$val)) {
      shinyjs::hide("btn_read_tree_dirs")
      display_tree$val <- FALSE
      res <- read_server_directory(input$search_data_dir, sample_set_name="Server files", glob_files=do_glob)
      if (res && !input$search_data_dir %in% recently_used_dirs$val) {
        recently_used_dirs$val <-
          c(input$search_data_dir, recently_used_dirs$val)
        if (!is.null(recently_used_dir_user_config))
          writeLines(recently_used_dirs$val, recently_used_dir_user_config)
      }
    } else {
      file_tree_dir$val <- input$search_data_dir
    }
  })
  
  output$rud <- renderUI({
    req(recently_used_dirs$val)
    
    shiny::tagList(br(),
                   "Recently used directories: ",
                   lapply(seq(from = 1, to = min(length(
                     recently_used_dirs$val
                   ), 5)),
                   function(i)
                     actionLink(ns(paste0("rud_", i)), recently_used_dirs$val[i])))
    
  })
  
  observeEvent(input$rud_1, {
    shinyWidgets::updateSearchInput(session, "search_data_dir", value = recently_used_dirs$val[1])
  })
  observeEvent(input$rud_2, {
    shinyWidgets::updateSearchInput(session, "search_data_dir", value = recently_used_dirs$val[2])
  })
  observeEvent(input$rud_3, {
    shinyWidgets::updateSearchInput(session, "search_data_dir", value = recently_used_dirs$val[3])
  })
  observeEvent(input$rud_4, {
    shinyWidgets::updateSearchInput(session, "search_data_dir", value = recently_used_dirs$val[4])
  })
  observeEvent(input$rud_5, {
    shinyWidgets::updateSearchInput(session, "search_data_dir", value = recently_used_dirs$val[5])
  })
  
  update_sample_set_hot <- reactive({
    req(input$table)
    req(input$sample_set_select)
    #str(rhandsontable::hot_to_r(input$table))
    tryCatch({
      old_df <- sample_sets$val[[input$sample_set_select]]
      new_df <- rhandsontable::hot_to_r(input$table)
      
      if (!isTRUE(all.equal(old_df, new_df))) {
        sample_sets$val[[input$sample_set_select]] <- new_df
      }
    }, error = function(e)
      dmessage("Error calling hot_to_r!"))
  })
  
  
  observeEvent(input$file_upload, {
    #update_sample_set_hot()
    inFile <- input$file_upload
    
    for (i in seq_along(inFile$datapath)) {
      dirname <- dirname(inFile$datapath[i])
      fname <- file.path(dirname, inFile$name[i])
      file.rename(inFile$datapath[i], fname)
      tryCatch({
      if (grepl(".zip$", fname, ignore.case = T)) {
          if (length(unzip(fname, exdir = dirname)) > 0) { 
            file.remove(fname)
          }
      } else if (grepl(".gz$|.bzip2$|.xz$|.bz2$", fname, ignore.case = T)) {
          if (length(untar(fname, exdir = dirname)) > 0) { 
            file.remove(fname)
          }
      }}, error = message)
    }
    
    read_server_directory(dirname(inFile$datapath[1]), "Uploaded sample set")
  })
  
  get_sample_data <- reactive({
    validate(need(sample_sets$val, message = "Need samples sets"))
    sample_sets$val[[input$sample_set_select]]
  })
  
  output$table <- renderRHandsontable({
    sample_data <- get_sample_data()
    validate(need(sample_data, message = "Need sample data for table."))
    
    #sample_data$FormatOK <- ifelse(sample_data$FormatOK,
    #                               "<font color='green'>&#x2713;</font>",
    #                               "<font color='red'>&#x2717;</font>")
    
    sample_data <-
      sample_data[, c("FormatOK", setdiff(colnames(sample_data), "FormatOK"))]
    
    rh <-
      rhandsontable(sample_data,
                    readOnly = TRUE,
                    manualRowMove = FALSE) %>%
      hot_col(
        "Include", readOnly = FALSE#,
        #renderer = "
        #function(instance, td, row, col, prop, value, cellProperties) {
        #cellProperties.readOnly = !value;
        #Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
        #return td;
        #}"
      ) %>%
      hot_col(
        "FormatOK"#,
        #renderer = "
        #function(instance, td, row, col, prop, value, cellProperties) {
        #  Handsontable.renderers.TextRenderer.apply(this, arguments);
        #  if (value ) { value = '&#x2713'; td.style.color = 'green';
        #  } else      { value = '&#x2717'; td.style.color = 'red';
        #    cellProperties.comment = 'The file format does not validate. Pavian supports the outputs from kraken-report, centrifuge-kreport (but not the centrifuge --report-file!), and metaphlan2.py. You can create a valid centrifuge report with centrifuge-kreport -x IDX OUT_FILE.';
        #  }
        #  return td;
        #}"
      ) %>%
      hot_col("Name", readOnly = FALSE)
    if ("Class" %in% colnames(sample_data))
      rh <- rh %>% hot_col("Class", readOnly = FALSE)
    
    rh %>% hot_table(enableComments = FALSE, highlightRow = TRUE, overflow="hidden", contextMenu=FALSE)
  })
  
  observeEvent(input$btn_save_table, {
    update_sample_set_hot()
  })
  
  currently_renaming_sample_set <- FALSE
  
  observeEvent(input$btn_rename_sample_set, {
    req(sample_sets$val)
    shinyjs::toggle("txt_rename_sample_set")
    
    if (currently_renaming_sample_set) {
      selected_item <- names(sample_sets$val) == input$sample_set_select
      names(sample_sets$val)[selected_item] <<-  input$txt_rename_sample_set
      sample_sets$selected <- input$txt_rename_sample_set
      #updateRadioButtons(
      #  session,
      #  "sample_set_select",
      #  choices = names(sample_sets$val),
      #  selected = names(sample_sets$val)[selected_item]
      #)
    } else {
      updateTextInput(session,
                      "txt_rename_sample_set",
                      value = input$sample_set_select)
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
  
  load_example_data_rv <- reactiveValues(val = load_example_data)
  observeEvent(load_example_data_rv$val, {
    req(load_example_data_rv$val)
    dmessage("Loading example data ...")
    read_server_directory(
      system.file("shinyapp", "example-data", package = "pavian"),
      include_base_dir = FALSE
    )
  }
  )
  
  onBookmark(function(state) {
    state$values$sample_sets_val <- sample_sets$val
  })
  
  onRestore(function(state) {
    sample_sets$val <- state$values$sample_sets_val
  })
  
  return(sample_sets)
}

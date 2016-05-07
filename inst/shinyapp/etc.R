
  report_file_names <- reactive({
    # strip base directory name
    report_files_no_dir <-
      sub(paste0(input$cbo_data_dir, "/"),
          "",
          report_files(),
          fixed = TRUE)

    # strip extension
    report_files_basename <-
      sub(paste0(input$txt_file_ext, "$"), "", report_files_no_dir)

    return(report_files_basename)
  })

  selected_report_files <- reactive({

  })


          #fileInput('upload_file', 'Choose file to upload',
          #  accept = c('text/csv', 'text/comma-separated-values',
          #    'text/tab-separated-values', 'text/plain',
          #    '.csv', '.report'),
          #  multiple = TRUE
          #),
          #textInput(
          #  "txt_file_ext",
          #  "File extension",
          #  value = ".report",
          #  width = "80%"
          #),
          #textInput(
          #  "regex_pattern",
          #  "Pattern to find files - use * as wildcard, and capture the sample name with paranthesis",
          #  value = "(.*).report",
          #  width = "80%"
          #)

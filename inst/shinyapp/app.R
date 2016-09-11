library(shiny)
library(pavian)
library(rhandsontable)
library(magrittr)
library(shinydashboard)
library(shinyjs)

options(shiny.maxRequestSize=50*1024^2)
cache_dir <- tempdir()

common_datatable_opts <- list(saveState = TRUE)

intro <- fluidRow(
  column(width = 8, includeMarkdown(system.file("shinyapp", "intro_data.md", package="pavian"))),
  column(width = 4, includeMarkdown(system.file("shinyapp", "intro_logo.html", package="pavian")))
)

ui <- dashboardPage(skin="blue", title = "Pavian",
  dashboardHeader(title = "",
                  tags$li(class = "dropdown",
                          tags$img(src="baboon2.png")
                  ),
                  tags$li(class = "dropdown",
                          tags$a(href="https://ccb.jhu.edu",
                                 target="_blank",
                                 style = "font-size: 20px;",
                                 tags$b("Pavian metagenomics data explorer"))
                          ),
                  #tags$li(class = "dropdown",
                  #        tags$a(href="https://ccb.jhu.edu", target="_blank",
                  #               "CCB @ JHU")
                  #),
                  #tags$li(class = "dropdown",
                  #        tags$a(href="http://twitter.com/share?url=http://ccb.jhu.edu/pavian&amp;text=Explore metagenomics data with @pavian ", target="_blank", tags$img(icon('twitter')))),
                  tags$li(class = "dropdown",
                          tags$a(href="http://github.com/fbreitwieser/pavian", target="_blank", tags$img(icon('github'))))
                  ),
  dashboardSidebar(
    shinyjs::disabled(selectizeInput("opt_select_dataset", choices=c("Not available"=""), label="Select sample set")),
    shinyjs::disabled(actionButton("remove_cache_files", "Remove cached files ↻")),
    br(),
    #sidebarSearchForm(
    #  textId = "txt_sidebarSearch",
    #  buttonId = "btn_sidebarSearch",
    #  label = "Search ..."
    #),
    sidebarMenu(
      id = "tabs",
      menuItem("Data Input", tabName="Home", icon = icon("cloud-upload"), selected = TRUE),
      menuItemOutput("dy_menu_overview"),
      menuItemOutput("dy_menu_comp"),
      menuItemOutput("dy_menu_sample"),
      menuItem("Alignment viewer", tabName = "Alignment", icon = icon("asterisk")),
      menuItem("About", tabName = "About")),
    uiOutput("sidebartext"),
    br(),
    tags$p(class="sidebartext", "@fbreitw, 2016")
    ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML(paste(readLines(system.file("shinyapp","style.css",package="pavian")),collapse = "\n")))
    ),
    tabItems(
      tabItem("Home",
              intro,
              dataInputModuleUI("datafile")
      ),
      tabItem("Overview",
              reportOverviewModuleUI("overview"),
              uiOutput("view_in_sample_viewer") ### <<<<<< TODO
      ),
      tabItem("Comparison", comparisonModuleUI("comparison")),
      tabItem("Bacteria", comparisonModuleUI("bacteria")),
      tabItem("Viruses", comparisonModuleUI("viruses")),
      tabItem("Sample", sampleModuleUI("sample")),
      tabItem("Alignment", alignmentModuleUI("alignment")),
      tabItem(
        "About",
        intro,
        "This tool was developed by Florian Breitwieser in Steven Salzberg's lab at the Center for Computational Biology at Johns Hopkins Medical Institution. This work was supported by the U.S. National Institutes of Health [R01-HG006677,R01-GM083873]; and by the U.S. Army Research Office [W911NF-1410490].",
        br(),
        br(),
        box(width=12,
            title="Session Information",
            collapsible=TRUE,
            collapsed=FALSE,
            verbatimTextOutput("session_info")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  output$dy_menu_overview <- renderMenu({
    req(input$opt_select_dataset)
    menuItem("Results Overview", tabName="Overview", icon = icon("table"))
  })
  output$dy_menu_comp <- renderMenu({
    req(input$opt_select_dataset)
    menuItem("Comparison", icon = icon("line-chart"),
             menuSubItem("All data", tabName="Comparison"),
             menuSubItem("Bacteria", tabName="Bacteria"),
             menuSubItem("Viruses", tabName="Viruses"),
             menuSubItem("Fungi and Protists", tabName="Fungi_and_Protists")
    )
  })
  output$sidebartext <- renderUI({
    if (is.null(input$opt_select_dataset) || input$opt_select_dataset == "") {
    shiny::tagList(
    br(),
    tags$p(class="sidebartext", "To start exploring metagenomics data, upload a dataset in the 'Data Input' tab."),
    tags$p(class="sidebartext", "Or view alignments and download genomes in the 'Alignment viewer'."))
    br()
    }
  })

  output$dy_menu_sample <- renderMenu({
    req(input$opt_select_dataset)
    menuItem("Sample", tabName="Sample", icon = icon("sun-o"))
  })


  observeEvent(input$opt_select_dataset,{
    if (isTRUE(input$opt_select_dataset == "upload_files")) {
      updateTabItems(session,"tabs","Home")
    } #else {
    #  updateTabItems(session,"tabs","Overview")
    #}
  })

  all_datasets <- callModule(dataInputModule, "datafile", height = 800,
                               example_dir = getOption("pavian.example_dir", system.file("shinyapp", "example-data", package = "pavian")))


  observeEvent(all_datasets(),{
    if (length(all_datasets()) > 0) {
      dataset_names <- names(all_datasets()$val)
      #dataset_names["Upload samples ..."] <- "upload_files"
      shinyjs::enable("opt_select_dataset")
      shinyjs::enable("remove_cache_files")

      updateSelectizeInput(session, "opt_select_dataset", choices = dataset_names, selected = attr(all_datasets(), "selected"))
    } else {
      updateSelectizeInput(session, "opt_select_dataset", choices = c("Not available"=""))
      shinyjs::disable("opt_select_dataset")
      shinyjs::disable("remove_cache_files")
    }
  })

  selected_dataset <- reactive({
    all_datasets()[[input$opt_select_dataset]]
  })

  selected_dataset_glom <- reactive({
    x <- selected_dataset()
    y <- x
    withProgress({
      for (tax_level in rev(colnames(tax_table(x)))) {

      }
    }, message = "Aglommerating data at different taxonomy levels")

  })

  observeEvent(input$remove_cache_files, {
    file.remove(list.files(cache_dir,full.names = T))
  })

  callModule(reportOverviewModule, "overview", selected_dataset, selected_dataset_glom, datatable_opts = common_datatable_opts)
  callModule(comparisonModule, "comparison", selected_dataset, NULL, datatable_opts = common_datatable_opts)
  callModule(comparisonModule, "bacteria", selected_dataset, NULL,
             filter_func = function(x) x[grep("d_Bacteria", x[["taxonstring"]]), , drop=F],
             datatable_opts = common_datatable_opts)
  callModule(comparisonModule, "viruses", selected_dataset, NULL,
             filter_func = function(x) x[grep("Viruses", x[["taxonstring"]]), , drop=F],
             datatable_opts = common_datatable_opts)
  callModule(comparisonModule, "fungi", selected_dataset, NULL,
             filter_func = function(x)
               x[grepl("d_Eukaryota", x[["taxonstring"]]) & !grepl("c_Mammalia", x[["taxonstring"]]), , drop=F],
             datatable_opts = common_datatable_opts)

  callModule(sampleModule, "sample", selected_dataset, NULL, common_datatable_opts)

  callModule(alignmentModule, "alignment", selected_dataset)

  output$session_info <- renderPrint( { sessionInfo() } )
}

shinyApp(ui, server)

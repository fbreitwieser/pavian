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

ui <- dashboardPage(skin="blue",
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
                  tags$li(class = "dropdown",
                          tags$a(href="https://ccb.jhu.edu", target="_blank",
                                 "CCB @ JHU")
                  ),
                  tags$li(class = "dropdown",
                          tags$a(href="http://twitter.com/share?url=http://ccb.jhu.edu/pavian&amp;text=Explore metagenomics data with @pavian ", target="_blank", tags$img(icon('twitter')))),
                  tags$li(class = "dropdown",
                          tags$a(href="http://github.com/fbreitwieser/pavian", target="_blank", tags$img(icon('github'))))
                  ),
  dashboardSidebar(
    shinyjs::disabled(selectizeInput("def_files", choices=c("Not available"=""), label="Select sample set")),
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
    br(),br(),
    br(),br(),
    tags$p(class="sidebartext", "To start exploring metagenomics data, upload a dataset in the 'Data Input' tab."),
    tags$p(class="sidebartext", "Or view alignments and download genomes in the 'Alignment viewer'."),
    br(),
    tags$p(class="sidebartext", "@fbreitwieser, 2016")
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
      tabItem("Fungi_and_Protists", comparisonModuleUI("fungi")),
      tabItem("Sample", sampleModuleUI("sample")),
      tabItem("Alignment", alignmentModuleUI("alignment")),
      tabItem(
        "About",
        intro,
        "This tool was developed by Florian Breitwieser in Steven Salzberg's lab at the Center for Computational Biology at Johns Hopkins Medical Institution. ",
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
    req(input$def_files)
    menuItem("Results Overview", tabName="Overview", icon = icon("table"))
  })
  output$dy_menu_comp <- renderMenu({
    req(input$def_files)
    menuItem("Comparison", icon = icon("line-chart"),
             menuSubItem("All data", tabName="Comparison"),
             menuSubItem("Bacteria", tabName="Bacteria"),
             menuSubItem("Viruses", tabName="Viruses"),
             menuSubItem("Fungi and Protists", tabName="Fungi_and_Protists")
    )
  })

  output$dy_menu_sample <- renderMenu({
    req(input$def_files)
    menuItem("Sample", tabName="Sample", icon = icon("sun-o"))
  })


  observeEvent(input$def_files,{
    if (isTRUE(input$def_files == "upload_files")) {
      updateTabItems(session,"tabs","Home")
    } #else {
    #  updateTabItems(session,"tabs","Overview")
    #}
  })

  sample_sets_df <- callModule(dataInputModule, "datafile", height = 800)

  observeEvent(sample_sets_df(),{
    if (length(sample_sets_df()$val) > 0) {
      message("sample sets df changed!")
      def_files <- names(sample_sets_df()$val)
      #def_files["Upload samples ..."] <- "upload_files"
      shinyjs::enable("def_files")

      updateSelectizeInput(session, "def_files", choices = def_files, selected = attr(sample_sets_df()$val, "selected"))
    } else {
      updateSelectizeInput(session, "def_files", choices = c("Not available"=""))
      shinyjs::disable("def_files")
    }
  })

  samples_df <- reactive({
    res <- sample_sets_df()$val[[input$def_files]]
    res[res$Include, ]
  })

  reports <- reactive({
    validate(
      need("ReportFilePath" %in% colnames(samples_df()), "ReportFilePath not available!"),
      need("Name" %in% colnames(samples_df()), "Name not available!")
    )
    read_reports(samples_df()$ReportFilePath, samples_df()$Name, cache_dir = cache_dir)
  })
  callModule(reportOverviewModule, "overview", samples_df, reports, datatable_opts = common_datatable_opts)
  callModule(comparisonModule, "comparison", samples_df, reports, datatable_opts = common_datatable_opts)
  callModule(comparisonModule, "bacteria", samples_df, reports,
             filter_func = function(x) x[grep("d_Bacteria", x[["taxonstring"]]), , drop=F],
             datatable_opts = common_datatable_opts)
  callModule(comparisonModule, "viruses", samples_df, reports,
             filter_func = function(x) x[grep("Viruses", x[["taxonstring"]]), , drop=F],
             datatable_opts = common_datatable_opts)
  callModule(comparisonModule, "fungi", samples_df, reports,
             filter_func = function(x)
               x[grepl("d_Eukaryota", x[["taxonstring"]]) & !grepl("c_Mammalia", x[["taxonstring"]]), , drop=F],
             datatable_opts = common_datatable_opts)

  callModule(sampleModule, "sample", samples_df, reports, common_datatable_opts)

  callModule(alignmentModule, "alignment", samples_df)

  output$session_info <- renderPrint( { sessionInfo() } )
}

shinyApp(ui, server)

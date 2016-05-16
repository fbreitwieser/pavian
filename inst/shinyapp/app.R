library(shiny)
library(centrifuger)
library(rhandsontable)
library(magrittr)
library(shinydashboard)

common_datatable_opts <- list(saveState = TRUE)

intro <- fluidRow(
  column(width = 8, includeMarkdown(system.file("shinyapp", "intro_data.md", package="centrifuger"))),
  column(width = 4, includeMarkdown(system.file("shinyapp", "intro_logo.html", package="centrifuger")))
)

def_files <- list.files(system.file("shinyapp","example-data",package="pavian"), pattern="defs.csv", recursive=TRUE, full.names=TRUE)
names(def_files) <- basename(dirname(def_files))

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
      selectizeInput("def_files", choices=def_files, label="Select sample set"),
      br(),
    sidebarSearchForm(
      textId = "txt_sidebarSearch",
      buttonId = "btn_sidebarSearch",
      label = "Search ..."
    ),
    sidebarMenu(id="tabs",
      menuItem("Home", tabName="Home"),
      menuItem("Results Overview", tabName="Overview", icon = icon("table")),
      menuItem("Comparison", icon = icon("line-chart"),
               menuSubItem("All data", tabName="Comparison"),
               menuSubItem("Bacteria", tabName="Bacteria"),
               menuSubItem("Viruses", tabName="Viruses"),
               menuSubItem("Fungi and Protists", tabName="Fungi_and_Protists")
      ),
      menuItem("Sample", tabName="Sample", icon = icon("sun-o")),
      menuItem("Alignment (beta)", tabName = "Alignment", icon = icon("asterisk")),
      menuItem("CDC ID (beta)", tabName = "CDC", icon = icon("asterisk")),
      menuItem("About", tabName = "About")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(paste(readLines(system.file("shinyapp","www","style.css",package="centrifuger")),collapse = "\n")))
      ),
    tabItems(
      tabItem("Home",intro),
      tabItem(
        "Data", intro,
        fluidRow(
          dataInputModuleUI("datafile")
        )
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
        #id = "tabs_about",
        intro,
        box(width=12,
            title="Session Information",
            collapsible=TRUE,
            collapsed=TRUE,
            verbatimTextOutput("session_info")
      )
    )
)
  )
)

server <- function(input, output, session) {

    observeEvent(input$def_files,{
      updateTabItems(session,"tabs","Overview") 
    })
  #samples_df <- callModule(dataInputModule, "datafile", height = 800)
  samples_df <- reactive({
    def_df <- read.delim(input$def_files, header = TRUE, sep = ";", stringsAsFactors = FALSE)

    validate(need("ReportFile" %in% colnames(def_df),
                  message = "Required column 'ReportFile' not present in defs.csv"))


    if (!"Include" %in% colnames(def_df))
      def_df <- cbind(Include = TRUE, def_df)

    if ("Class" %in% colnames(def_df))
      def_df$Class <- as.factor(def_df$Class)

    if (!"ReportFilePath" %in% colnames(def_df))
      def_df$ReportFilePath <- file.path(dirname(def_files), def_df$ReportFile)

    def_df

  })
  reports <- reactive({
    validate(
      need("ReportFilePath" %in% colnames(samples_df()), "ReportFilePath not available!"),
      need("Name" %in% colnames(samples_df()), "Name not available!")
    )
    read_reports(samples_df()$ReportFilePath, samples_df()$Name)
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

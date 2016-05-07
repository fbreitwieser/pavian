library(shiny)
library(centrifuger)
library(rhandsontable)
library(magrittr)
library(shinythemes)

common_datatable_opts <- list(saveState = TRUE)

intro <- fluidRow(
      column(width = 8, includeMarkdown(system.file("shinyapp","intro_data.md",package="centrifuger"))),
      column(width = 4, includeMarkdown(system.file("shinyapp","intro_logo.html",package="centrifuger")))
    )

ui <- navbarPage(
  title = "",
  windowTitle = "Pavian metagenomics data explorer",
  #theme = shinytheme("readable"),
  fluid = TRUE,

  tabPanel(
    title = "Program",
    id = "tabs_modules",
	intro,
    tabsetPanel(
      tabPanel(
        title = "Data",
        id = "tabs_data",
        fluidRow(
          dataInputModuleUI("datafile")
        )),
        tabPanel("Overview",
                 reportOverviewModuleUI("overview"),
                 uiOutput("view_in_sample_viewer") ### <<<<<< TODO
                 ),
        tabPanel("Comparison",
                 comparisionModuleUI("comparision"))
      )
  ),
  tabPanel(
    title = "About",
    id = "tabs_about",
	intro,
	fluidRow("ABC")
  )
)

server <- function(input, output, session) {
  samples_df <- callModule(dataInputModule, "datafile", height = 800)
  reports <- reactive({
    validate(
      need("ReportFilePath" %in% colnames(samples_df()), "ReportFilePath not available!"),
      need("Name" %in% colnames(samples_df()), "Name not available!")
    )
    read_reports(samples_df()$ReportFilePath, samples_df()$Name)
  })
  callModule(reportOverviewModule, "overview", samples_df, reports, datatable_opts = common_datatable_opts)
  callModule(comparisionModule, "comparision", samples_df, reports, datatable_opts = common_datatable_opts)

}

shinyApp(ui, server)

library(shiny)
library(pavian)
library(rhandsontable)
library(magrittr)

ui <- navbarPage("DataInputModule",
  tabPanel(
    title = "Data",
    id = "tabs_data",
    fluidRow(
      column(width = 8, includeMarkdown("intro_data.md")),
      column(width = 4, includeMarkdown("intro_logo.html"))
    ),
    fluidRow(
          dataInputModuleUI("datafile")
    )
  )
)

server <- function(input, output, session) {
  callModule(dataInputModule, "datafile", height = 800)
}

shinyApp(ui, server)

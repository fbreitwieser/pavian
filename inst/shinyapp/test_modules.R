library(shiny)
library(pavian)
library(rhandsontable)
library(magrittr)

options(shiny.maxRequestSize=256*1024^2)

ui <- navbarPage("DataInputModule",
  tabPanel(
    title = "Data",
    id = "tabs_data",
    fluidRow(
          dataInputModuleUI("datafile")
    )
  )
)

server <- function(input, output, session) {
  callModule(dataInputModule, "datafile", height = 800)
}

shinyApp(ui, server)

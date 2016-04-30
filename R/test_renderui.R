library(shiny)
library(d3heatmap)
library(shinydashboard)
library(DT)

ui <- dashboardPage(
  dashboardHeader(title="resizing d3heatmap"),
  dashboardSidebar(
    sliderInput("obs", "Number of rows:", min = 10, max = 100, value = 50)
  ),
  dashboardBody(
    tabBox(
      tabPanel("Data",DT::dataTableOutput("dt")),
      tabPanel("Heatmap",uiOutput("hm_o"))
    )
  )
)

server <- function(input, output) {
  dat <- reactive({ matrix(rnorm(input$obs*4),ncol=4) })

  output$dt <- DT::renderDataTable({ DT::datatable(as.data.frame(dat())) })

  output$hm_o <- renderUI({
    d3heatmap::d3heatmapOutput('hm',width="50%", height=paste0(50+length(input$dt_rows_current)*10,"px"))
  })

  output$hm <- renderD3heatmap({
    d3heatmap(dat()[input$dt_rows_current,], Colv=FALSE,
              colors=colorRampPalette(c("blue", "white", "red"))(100))
  })
}

shinyApp(ui = ui, server = server)

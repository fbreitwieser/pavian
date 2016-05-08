library(shiny)
library(centrifuger)
library(rhandsontable)
library(magrittr)
library(shinydashboard)

common_datatable_opts <- list(saveState = TRUE)

intro <- fluidRow(
  column(width = 8, includeMarkdown(system.file("shinyapp","intro_data.md",package="centrifuger"))),
  column(width = 4, includeMarkdown(system.file("shinyapp","intro_logo.html",package="centrifuger")))
)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarSearchForm(
      textId = "txt_sidebarSearch",
      buttonId = "btn_sidebarSearch",
      label = "Search ..."
    ),
    sidebarMenu(
      menuItem("Home", tabName="Home"), #, icon = icon("home")),
      menuItem("Data", tabName="Data", icon = icon("table")),
      menuItem("Results Overview", tabName="Overview", icon = icon("bar-chart")),
      menuItem("Comparison", icon = icon("line-chart"),
               menuSubItem("All data",tabName="Comparison"),
               menuSubItem("Bacteria",tabName="Bacteria"),
               menuSubItem("Viruses",tabName="Viruses"),
               menuSubItem("Fungi and Protists",tabName="Fungi_and_Protists")
               ),
      #menuItem("Alignment", tabName = "tab_alignment", icon = icon("asterisk"))
      menuItem("About", tabName = "About")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        "Data", intro,
        fluidRow(
          dataInputModuleUI("datafile")
        )),
      tabItem("Overview",
              reportOverviewModuleUI("overview"),
              uiOutput("view_in_sample_viewer") ### <<<<<< TODO
      ),
      tabItem("Comparison",comparisonModuleUI("comparison")),
      tabItem("Bacteria",comparisonModuleUI("bacteria")),
      tabItem("Viruses",comparisonModuleUI("viruses")),
      tabItem("Fungi_and_Protists",comparisonModuleUI("fungi")),
      tabItem (
        "About",
        #id = "tabs_about",
        intro,
        fluidRow("ABC")
      )
    )
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
  callModule(comparisonModule, "comparison", samples_df, reports, datatable_opts = common_datatable_opts)
  callModule(comparisonModule, "bacteria", samples_df, reports,
             filter_func = function(x) x[grep("d_Bacteria", x[["taxonstring"]]),,drop=F],
             datatable_opts = common_datatable_opts)
  callModule(comparisonModule, "viruses", samples_df, reports,
             filter_func = function(x) x[grep("Viruses", x[["taxonstring"]]),,drop=F],
             datatable_opts = common_datatable_opts)
  callModule(comparisonModule, "fungi", samples_df, reports,
             filter_func = function(x)
               x[grepl("d_Eukaryota", x[["taxonstring"]]) & !grepl("c_Mammalia", x[["taxonstring"]]),,drop=F],
             datatable_opts = common_datatable_opts)

}

shinyApp(ui, server)

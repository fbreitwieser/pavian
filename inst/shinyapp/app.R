library(shiny)
library(pavian)
library(rhandsontable)
library(magrittr)
library(shinydashboard)
library(shinyjs)
library(DT)
library(shinyBS)

options(shiny.maxRequestSize=50*1024^2)
cache_dir <- tempdir()

common_datatable_opts <- list(saveState = TRUE, searchHighlight = TRUE)

intro <- fluidRow(
  column(width = 11, includeMarkdown(system.file("shinyapp", "intro_data.md", package="pavian"))),
  column(width = 1, includeMarkdown(system.file("shinyapp", "intro_logo.html", package="pavian")))
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
    shinyjs::disabled(selectInput("sample_set_names", choices=c("Not available"=""), label="Select sample set", selectize = TRUE)),
    shinyjs::hidden(shinyjs::disabled(actionButton("btn_remove_cache_files", "Remove cached files ↻"))),
    sidebarSearchForm(
      textId = "txt_sidebarSearch",
      buttonId = "btn_sidebarSearch",
      label = "Search microbes ..."
    ),
    br(),
    sidebarMenu(
      id = "tabs",
      menuItem("Data Input", tabName="Home", icon = icon("cloud-upload"), selected = TRUE),
      menuItemOutput("dy_menu_overview"),
      menuItemOutput("dy_menu_comp"),
      menuItemOutput("dy_menu_sample"),
      menuItem("Alignment viewer", tabName = "Alignment", icon = icon("asterisk")),
      menuItem("About", tabName = "About")),
    uiOutput("sidebartext"),
    div(class = "busy", style="padding-left: 10px;",
        p("Calculation in progress.."),
        img(src="default.gif")
    ),

    br(),
    tags$p(class="sidebartext", style="padding-left: 10px;","@fbreitw, 2016")
    ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style("style.css"),
      tags$script(HTML("
setInterval(function(){
  if ($('html').attr('class')=='shiny-busy') {
    $('div.busy').show()
  } else {
    $('div.busy').hide()
  }
},100);

/*$(document).ready(function() { $('#dy_menu_comp').children[0].click(); })*/
"))
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
    req(input$sample_set_names)
    menuItem("Results Overview", tabName="Overview", icon = icon("table"))
  })
  output$dy_menu_comp <- renderMenu({
    req(input$sample_set_names)
    shiny::tagList(
    menuItem("Comparison", icon = icon("line-chart"),
             menuSubItem("All data", tabName="Comparison"),
             menuSubItem("Bacteria", tabName="Bacteria"),
             menuSubItem("Viruses", tabName="Viruses"),
             menuSubItem("Fungi and Protists", tabName="Fungi_and_Protists")
    )
    )
  })
  output$sidebartext <- renderUI({
    if (is.null(input$sample_set_names) || input$sample_set_names == "") {
    shiny::tagList(
    br(),
    tags$p(class="sidebartext", "To start exploring metagenomics data, upload a dataset in the 'Data Input' tab."),
    tags$p(class="sidebartext", "Or view alignments and download genomes in the 'Alignment viewer'."))
    br()
    }
  })

  output$dy_menu_sample <- renderMenu({
    req(input$sample_set_names)
    menuItem("Sample", tabName="Sample", icon = icon("sun-o"))
  })

  observeEvent(input$btn_sidebarSearch, {

  })


  observeEvent(input$sample_set_names,{
    if (isTRUE(input$sample_set_names == "upload_files")) {
      updateTabItems(session,"tabs","Home")
    } #else {
    #  updateTabItems(session,"tabs","Overview")
    #}
  })

  sample_sets <- callModule(dataInputModule, "datafile")


  observeEvent(sample_sets(),{
    if (length(sample_sets()$val) > 0) {
      sample_set_names <- names(sample_sets()$val)
      #sample_set_names["Upload samples ..."] <- "upload_files"
      shinyjs::enable("sample_set_names")
      shinyjs::enable("btn_remove_cache_files")

      #updateSelectizeInput(session, "sample_set_names", choices = sample_set_names, selected = attr(sample_sets()$val, "selected"))
      updateSelectInput(session, "sample_set_names", choices = sample_set_names, selected = attr(sample_sets()$val, "selected"))
    } else {
      #updateSelectizeInput(session, "sample_set_names", choices = c("Not available"=""))
      updateSelectInput(session, "sample_set_names", choices = c("Not available"=""))
      shinyjs::disable("sample_set_names")
      shinyjs::disable("btn_remove_cache_files")
    }
  })

  sample_data <- reactive({
    res <- sample_sets()$val[[input$sample_set_names]]
    res[res$Include, ]
  })

  observeEvent(input$btn_remove_cache_files, {
    file.remove(list.files(cache_dir,full.names = T))
  })

  reports <- reactive({
    validate(
      need("ReportFilePath" %in% colnames(sample_data()), "ReportFilePath not available!"),
      need("Name" %in% colnames(sample_data()), "Name not available!")
    )
    res <- read_reports(sample_data()$ReportFilePath, sample_data()$Name, cache_dir = cache_dir)
    validate(need(length(res) > 0, message = "There are no valid reports in this sample set!"))
    res
  })

  sample_module_selected <- callModule(sampleModule, "sample", sample_data, reports, common_datatable_opts)

  observeEvent(sample_module_selected(), {
    req(sample_module_selected())
    updateTabItems(session, "tabs", "Comparison")
  })

  callModule(reportOverviewModule, "overview", sample_data, reports, datatable_opts = common_datatable_opts)
  callModule(comparisonModule, "comparison", sample_data, reports, datatable_opts = common_datatable_opts)#, search = sample_module_selected)
  callModule(comparisonModule, "bacteria", sample_data, reports,
             filter_func = function(x) x[grep("[dk]_Bacteria", x[["taxonstring"]]), , drop=F],
             datatable_opts = common_datatable_opts)
  callModule(comparisonModule, "viruses", sample_data, reports,
             filter_func = function(x) x[grep("[dk]_Viruses", x[["taxonstring"]]), , drop=F],
             datatable_opts = common_datatable_opts)
  callModule(comparisonModule, "fungi", sample_data, reports,
             filter_func = function(x)
               x[grepl("d_Eukaryota", x[["taxonstring"]]) & !grepl("p_Chordata", x[["taxonstring"]]), , drop=F],
             datatable_opts = common_datatable_opts)

  callModule(alignmentModule, "alignment", sample_data)

  output$session_info <- renderPrint( { sessionInfo() } )
}

shinyApp(ui, server)

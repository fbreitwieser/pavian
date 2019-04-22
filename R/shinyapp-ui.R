

#' Function building dashboard UI, used in Shiny app
#'
#' @param request request object
#'
#' @return Dashboard page
#' @export
dashboardUI <- function(request) {
  dashboardPage(skin="black", title = "Pavian",
                dashboardHeader(title = "",
                                #tags$li(class = "dropdown",
                                #        tags$img(src=system.file("shinyapp","www", "baboon-outline.png", package="pavian"))
                                #),
                                tags$li(class = "dropdown",
                                        tags$a(href="#",
                                               #target="_blank",
                                               style = "font-size: 20px;",
                                               "Pavian metagenomics data explorer")
                                ),
                                #tags$li(class = "dropdown",
                                #        tags$a(href="https://ccb.jhu.edu", target="_blank",
                                #               "CCB @ JHU")
                                #),
                                #tags$li(class = "dropdown",
                                #          tags$a(href="http://twitter.com/share?url=http://ccb.jhu.edu/software/pavian&amp;text=Explore metagenomics data with #pavian ", target="_blank", tags$img(icon('twitter')))),
                                tags$li(class = "dropdown",
                                        tags$a(href="http://github.com/fbreitwieser/pavian", target="_blank", tags$img(icon('github'))))
                ),
                dashboardSidebar(
                  div(class="hide_when_sidebar_collapsed",
                  shinyjs::hidden(shinyjs::disabled(actionButton("btn_remove_cache_files", "Remove cached files ???"))),
                  shinyjs::hidden(sidebarSearchForm(textId = "txt_sidebarSearch", buttonId = "btn_sidebarSearch", label = "Search ...")),
                  br()),
                  conditionalPanel(
                    condition = "input.sample_set_names != ''",
                    sidebarMenu(
                      id = "tabs",
                      menuItem("Data Input", tabName="Home", icon = icon("cloud-upload"), selected = TRUE),
                      div(class="set_selector hide_when_sidebar_collapsed no_padding", 
                          shinyjs::hidden(selectInput("sample_set_names", choices=NULL, label=NULL, multiple=TRUE, 
                                                      selectize = FALSE, size = 5))),
                      # The following menus are just displayed when a sample set has been loaded
                      shinydashboard::menuItem("Results Overview", tabName="Overview", icon = icon("table")),
                      shinydashboard::menuItem("Sample", tabName="Sample", icon = icon("sun-o")),
                      shinydashboard::menuItem("Comparison", icon = icon("line-chart"), tabName = "Comparison"),
                                               #shinydashboard::menuSubItem("All data", tabName="Comparison"),
                                               #actionLink("show_bacteria","Bacteria and Archaea", tabName="Bacteria")
                                               #shinydashboard::menuSubItem("Viruses", tabName="Viruses"),
                                               #shinydashboard::menuSubItem("Eukaryotes", tabName="Eukaryotes"),
                                               #shinydashboard::menuSubItem("Eukaryotes/Fungi", tabName="Fungi"),
                                               #shinydashboard::menuSubItem("Eukaryotes/Protists", tabName="Protists")
                      menuItem("Alignment viewer", tabName = "Alignment", icon = icon("asterisk")),
                      menuItem("About", tabName = "About")
                    ),
                    div(class="hide_when_sidebar_collapsed", #style = "color:lightblue",
                        br(),
                        uiOutput("bookmarkBtnUI")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.sample_set_names == ''",
                    sidebarMenu(
                      id = "tabs",
                      menuItem("Data Selection", tabName="Home", icon = icon("cloud-upload"), selected = TRUE),
                      # The following menus are just displayed when a sample set has been loaded
                      menuItem("Alignment viewer", tabName = "Alignment", icon = icon("asterisk")),
                      menuItem("About", tabName = "About")
                    ),
                    div(class="hide_when_sidebar_collapsed",
                    br(),
                    tags$p(class="sidebartext", style="padding-left: 10px;color: #b8c7ce; ", "To start exploring metagenomics data, upload a dataset in the 'Data Input' tab."),
                    tags$p(class="sidebartext", style="padding-left: 10px;color: #b8c7ce; ", "Or view alignments and download genomes in the 'Alignment viewer'.")
                    )
                  ),
                  
                  # Show a busy indicator
                  conditionalPanel(
                    condition="($('html').hasClass('shiny-busy'))",
                    div(class = "busy hide_when_sidebar_collapsed", style="padding-left: 10px; color: #b8c7ce; ", 
                        br(),
                        p(img(src="loading.gif"), "Calculation in progress..")
                    )),
                  div(class="hide_when_sidebar_collapsed", 
                  br(),
                  tags$p(class="sidebartext", style="padding-left: 10px;color: #b8c7ce; ",format(Sys.Date(), "@fbreitw, %Y"))
                  )
                ),
                dashboardBody(
                  shinyjs::useShinyjs(),
                  tags$head(includeCSS(system.file("shinyapp", "style.css", package="pavian"))),
                  tags$script(HTML("$('body').addClass('sidebar-mini');")),
                  tabItems(
                    tabItem("Home",
                            dataInputModuleUI("datafile")
                    ),
                    tabItem("Overview",
                            reportOverviewModuleUI("overview"),
                            uiOutput("view_in_sample_viewer") ### <<<<<< TODO
                    ),
                    #tabItem("Alldata"),
                    tabItem("Comparison", comparisonModuleUI("comparison")),
                    #tabItem("Bacteria", comparisonModuleUI("bacteria")),
                    #tabItem("Viruses", comparisonModuleUI("viruses")),
                    #tabItem("Eukaryotes", comparisonModuleUI("eukaryotes")),
                    #tabItem("Fungi", comparisonModuleUI("fungi")),
                    #tabItem("Protists", comparisonModuleUI("protists")),
                    tabItem("Sample", sampleModuleUI("sample")),
                    tabItem("Alignment", alignmentModuleUI("alignment")),
                    tabItem(
                      "About",
                      box(width=12,
                          HTML(
                            "<h2>Pavian metagenomics data explorer</h2>
                                                     
                                                     <p>This tool was developed by Florian Breitwieser in Steven Salzberg's lab at the Center for
                                                     Computational Biology at Johns Hopkins Medical Institution. This work was supported by
                                                     the U.S. National Institutes of Health [R01-HG006677,R01-GM083873]; and by the U.S. Army Research
                                                     Office [W911NF-1410490]. </p>")),
                      br(),
                      br(),
                      box(width=12,
                          title="Session Information",
                          collapsible=TRUE,
                          collapsed=FALSE,
                          verbatimTextOutput("session_info"),
                          verbatimTextOutput("session_info1")
                      )
                    )
                  )
                )
  )
}

navbarpageUI <- function(request) {
  navbarPage("Pavian", id="nav",
             tabPanel("Data Input",
                      dataInputModuleUI("datafile")
             ),
             tabPanel("Results Overview",
                      #reportOverviewModuleUI("overview"),
                      uiOutput("view_in_sample_viewer")
             ),
             tabPanel("Classifications across samples"#,
                      #comparisonModuleUI("alldata")
             )
  )
}

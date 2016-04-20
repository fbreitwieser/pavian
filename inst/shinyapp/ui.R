library(shiny)
library(shinydashboard)
library(centrifuger)
library(shinyFileTree)

# identifications that are considered contaminants and may be filtered by default
host_contaminants=c('s_Homo sapiens')
seq_contaminants=c('s_synthetic construct','u_unclassified','s_Enterobacteria phage phiX174 sensu lato')
microbe_contaminants=c('s_Propionibacterium acnes','s_Escherichia coli','s_Saccharomyces cerevisiae', 's_Ralstonia pickettii')
commoncontaminants <- c()  ## this vector is initially filtered
allcontaminants <- list(Host=host_contaminants,Artificial=seq_contaminants,Microbes=microbe_contaminants)
allcontaminants <- unlist(allcontaminants)
names(allcontaminants) <- NULL

shinyUI(dashboardPage(
  dashboardHeader(title="Metagenomics results viewer"),
  #includeCSS("style.css"),
  #########################################################  SIDEBAR
  dashboardSidebar(
    sidebarSearchForm(textId = "txt_sidebarSearch", buttonId = "btn_sidebarSearch", label = "Search ..."),
      textInput("cbo_data_dir", "Files from server",
                value=system.file("shinyapp/example-data",package = "centrifuger"),
                width="80%"),
      shinyFileTreeOutput("files_tree"),
      fileInput('upload_file', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv','.report'
                ),
                multiple=TRUE
      ),

      textInput("txt_file_ext", "File extension",
                value = ".report", width="80%"),
      textInput("regex_pattern", "Pattern to find files - use * as wildcard, and capture the sample name with paranthesis",
                value = "(.*).report", width="80%")

  ),
  ######################################################### DASHBOARD BODY
  dashboardBody(
  tabsetPanel(
  ###############################################  SAMPLES OVERVIEW
  tabPanel("Samples overview",
    fluidRow(
      box(
          selectizeInput('sample_selector2',
                         label="Select samples",
                         choices=NULL, multiple=TRUE,options=list(maxItems=1500, create=TRUE, plugins = list('drag_drop')),width='100%'),
        width = 8
      ),
      box(
        checkboxInput("opt_samples_overview_percent",label="Show percentages instead of number of reads"),
        width = 4
        )
    ),
    fluidRow(
        box(
          div(style='overflow-x: scroll',DT::dataTableOutput('samples_overview')),
          uiOutput("view_in_sample_viewer"),
          width = 12
        )
    )
  ),
  #############################################################################
  ##  SAMPLE VIEWER
  #############################################################################
  tabPanel("Sample viewer",
    fluidRow(
      column(9,
      div(
          selectInput('sample_selector',
                         label="",
                         choices=NULL, multiple=FALSE,width='100%'),
         style="font-size:100%"),
         conditionalPanel("input.sample_view_ui == 'sunburst'",sunburstR::sunburstOutput("sample_view_sunburst",width="90%")),
         conditionalPanel("input.sample_view_ui == 'sankey'",networkD3::sankeyNetworkOutput("sample_view_sankey",width="90%"))
      ),
      column(3,
             radioButtons("sample_view_ui",label="Display",choices=c("sunburst","sankey")),
             checkboxInput("synchonize_sampleview_table_and_sunburst",
                           label="Synchonize table", value=FALSE),
             checkboxInput("remove_root_hits",
                           label="Hide root hits", value=FALSE),
             selectizeInput('contaminant_selector2', label="Filter contaminants",
                    allcontaminants, selected=commoncontaminants,
                    multiple=TRUE,options=list(maxItems=25, create=TRUE, placeholder='filter contaminants'),
                    width="80%")
      )),
    fluidRow(
      div(style='overflow-x: scroll',DT::dataTableOutput('sample_view'))),
    fluidRow(uiOutput("view_in_samples_comparison"))

  ),
  #############################################################################
  ##  SAMPLES COMPARISON
  #############################################################################
  tabPanel("Samples comparison",
    fluidRow(
      column(5,
        div(
          selectizeInput('sample_selector3',
                         label="",
                         choices=NULL, multiple=TRUE,options=list(maxItems=1500, create=TRUE),width='100%'),
         style="font-size:100%"),
        div(selectizeInput('contaminant_selector3', label="Filter contaminants",
                    allcontaminants, selected=commoncontaminants,
                    multiple=TRUE,options=list(maxItems=25, create=TRUE, placeholder='filter contaminants'),
                    width="100%"))
      ),
      column(1, radioButtons("numeric_display",label=NULL, c("reads","percentage"), "reads" )),
      #column(1, radioButtons("input",label=NULL, c("kraken","centrifuge","blastx-lca","metaphlan"), "kraken" )),
      column(1, radioButtons("classification_level",label=NULL,c("S","G","F","O","C","P","D"),"D"),inline=TRUE),
      #column(1, checkboxInput("update_pubmed", 'Update Pubmed counts', value = FALSE)),
      column(4, #checkboxInput("display_table", 'Display table', value = TRUE),
                checkboxInput("display_heatmap", 'Display heatmap', value = FALSE),
                radioButtons("heatmap_scale", 'Scale',
                            c("none","row","column"),selected="none",inline=TRUE),
                checkboxGroupInput("heatmap_cluster", "Cluster",
                                   choices=c('row','column'), inline = TRUE))

    ),
    fluidRow(
      div(style='overflow-x: scroll',DT::dataTableOutput('dt_samples_comparison'))
    ),
    fluidRow(
      actionButton("btn_sc_filter","Filter"),
      actionButton("btn_sc_gointo","Go Into")
    ),
    fluidRow(
      shiny::htmlOutput("txt_samples_comparison")
    ),
    fluidRow(
      column(1),
      d3heatmap::d3heatmapOutput('samples_comparison_heatmap',width="100%")
    )
  ), ## end tabPanel samples_comparison
  #############################################################################
  ##  SAMPLES CLUSTERING
  #############################################################################
  tabPanel("Clustering",
    fluidRow(
      shiny::plotOutput("cluster_plot")
    )
  ), ## end tabPanel Clustering
  #############################################################################
  ##  ALIGN OF A MICROBE TO ONE SAMPLE
  #############################################################################
  tabPanel("OneMicrobe",
    fluidRow(
      shiny::checkboxInput("align_loess","Show smoothed LOESS curve"),
      shiny::checkboxInput("align_moving_avg","Show moving average",value = TRUE),
      shiny::actionButton("btn_get_reads","Get reads"),
      shiny::actionButton("btn_create_alignment","Create alignment"),
      shiny::actionButton("btn_get_alignment","Load alignment"),
      shiny::textInput("bam_file","Bam File"),
      shiny::selectizeInput("cbo_assemblies",choices=NULL,label="RefSeq Assemblies"),
      shiny::actionButton("btn_load_assembly_info","Load RefSeq assemblies"),
      div(style='overflow-x: scroll',DT::dataTableOutput("dt_assembly_info")),
      shiny::plotOutput("sample_align", brush = brushOpts("align_brush", direction = "x"),),
      shiny::htmlOutput("txt_align_brush")
    )
  )))
))

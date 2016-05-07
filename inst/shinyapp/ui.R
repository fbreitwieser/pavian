library(shiny)
library(shinydashboard)
library(shinyjs)
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
  dashboardHeader(title="Metagenomics results viewer",
                  dropdownMenu(type="notifications",
                               notificationItem(text="Successfully loaded X reports", status="success"),
                               notificationItem(text="Currently aligning X to Y ...", status="info")
                               )
  ),
  #########################################################  SIDEBAR
  dashboardSidebar(
    sidebarSearchForm(textId = "txt_sidebarSearch", buttonId = "btn_sidebarSearch", label = "Search ..."),
      textInput("cbo_data_dir", "Select reports",
                value=system.file("shinyapp/example-data",package = "centrifuger"),
                width="100%"),
      actionButton("btn_set_data_dir","Load directory content"),
      shinyFileTreeOutput("files_tree"),
      selectizeInput('contaminant_selector', label="Filter contaminants",
                    allcontaminants, selected=commoncontaminants,
                    multiple=TRUE,options=list(maxItems=25, create=TRUE, placeholder='filter contaminants'),
                    width="100%"),
      checkboxInput("opt_remove_root_hits",
                    label="Remove root hits", value=FALSE),
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
  	           includeCSS("style.css"),
  useShinyjs(),
  tabsetPanel(id="tabsetPanel_main",
  ###############################################  SAMPLES OVERVIEW
  tabPanel("Samples overview",
    fluidRow(
        box(
          checkboxInput("opt_samples_overview_percent",label="Show percentages instead of number of reads"),
          div(style='overflow-x: scroll',DT::dataTableOutput('dt_samples_overview')),
          uiOutput("view_in_sample_viewer"),
          width = 12
        )
    )
  ),
  ###############################################  SAMPLE VIEWER
  tabPanel("Sample viewer",
    fluidRow(
      box(width=9,
      div(
          selectInput('sample_selector',
                         label="",
                         choices=NULL, multiple=FALSE,width='100%'),
         style="font-size:100%"),
         conditionalPanel("input.sample_view_ui == 'sunburst'",sunburstR::sunburstOutput("sample_view_sunburst",width="90%")),
         conditionalPanel("input.sample_view_ui == 'sankey'",networkD3::sankeyNetworkOutput("sample_view_sankey",width="90%"))
      ),
      box(width=3,
             radioButtons("sample_view_ui",label="Display",choices=c("sunburst","sankey")),
             checkboxInput("synchonize_sampleview_table_and_sunburst",
                           label="Synchonize table", value=FALSE)
      )),
    fluidRow(box(width=12,
      div(style='overflow-x: scroll',DT::dataTableOutput('dt_sample_view'))),
      uiOutput("view_in_samples_comparison"))
  ),
  ###############################################  SAMPLES COMPARISON
  tabPanel("Samples comparison",
    fluidRow(
      box(width=4, selectizeInput("opt_classification_level",label="Taxon level",
          choices=c("Any"="-","Species"="S","Genus"="G","Family"="F","Order"="O","Class"="C","Phylum"="P","Domain"="D"),selected="-")),
      box(width=4,
		  checkboxInput("opt_display_percentage",label="Normalize by total number of reads per sample", value=FALSE),
		  radioButtons("opt_show_reads_stay",label="Display",
choices=c("Number of reads that stay"="reads_stay","Number of reads at level or lower"="reads","both"))
      )
      #column(1, radioButtons("input",label=NULL, c("kraken","centrifuge","blastx-lca","metaphlan"), "kraken" )),
      #column(1, checkboxInput("update_pubmed", 'Update Pubmed counts', value = FALSE)),
    ),
    tabBox(width=12,
      tabPanel("Table",
        div(style='overflow-x: scroll',DT::dataTableOutput('dt_samples_comparison')),
        actionButton("btn_sc_filter","Filter"),
        actionButton("btn_sc_gointo","Go Into"),
        shiny::htmlOutput("txt_samples_comparison")
      ),
      tabPanel("Heatmap",
        fluidRow(
        column(width=8,uiOutput("d3heatmap_samples_comparison")),
        column(width=4,
                radioButtons("heatmap_scale", 'Scale',
                            c("none","row","column"),selected="none",inline=TRUE),
                checkboxGroupInput("heatmap_cluster", "Cluster",
                                   choices=c('row','column'), inline = TRUE)))
      ),
      tabPanel("Samples Clustering",
        fluidRow(shiny::plotOutput("cluster_plot"))
      ) ## end tabPanel Clustering
    )
  ),
  ###############################################  ALIGNMENTS
  tabPanel("Alignments",
    tabBox(width=12,
      tabPanel(title="View alignment",
        shiny::textInput("bam_file","Bam File"),
        shiny::actionButton("btn_get_alignment","Load alignment"),
        shiny::checkboxInput("align_loess","Show smoothed LOESS curve"),
        shiny::checkboxInput("align_moving_avg","Show moving average",value = TRUE),
        collapsible=TRUE
      ),
      tabPanel(title="Create alignment",
        shiny::actionButton("btn_create_alignment","Create alignment"),
        shiny::selectizeInput("cbo_assemblies",choices=NULL,label="RefSeq Assemblies"),
        shiny::actionButton("btn_load_assembly_info","Load RefSeq assemblies"),
        div(style='overflow-x: scroll',DT::dataTableOutput("dt_assembly_info")),
        shiny::actionButton("btn_get_reads","Get reads"),
        shiny::plotOutput("sample_align", brush = brushOpts("align_brush", direction = "x")),
        shiny::htmlOutput("txt_align_brush"),
        collapsible=TRUE
      )
    )
  ),
  ###############################################  SEARCH PAGE
  tabPanel("Search page")
  ))
))

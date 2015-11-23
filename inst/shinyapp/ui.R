library(shiny)
library(centrifuger)

# identifications that are considered contaminants and may be filtered by default
commoncontaminants1=c('s_Homo sapiens','s_synthetic construct','u_unclassified','s_Enterobacteria phage phiX174 sensu lato')
allcontaminants=c(commoncontaminants1,'s_Propionibacterium acnes','s_Escherichia coli','s_Saccharomyces cerevisiae', 's_Ralstonia pickettii')
commoncontaminants <- c()  ## this vector is initially filtered

shinyUI(navbarPage("Metagenomics results viewer",id="main_page",
  #theme = "bootstrap.css",
  tags$head(
    tags$style(HTML("
      .has-feedback .form-control {
        padding-right: 2px;
      }
      .form-control {
        padding: 2px;
        padding-left: 4px;
        height: 25px;
        line-height: 25px;
      }
      .form-control-feedback {
        line-height: 25px;
      }
      table.dataTable tbody th, table.dataTable tbody td,
      table.dataTable thead th, table.dataTable thead td {
        padding: 2px 18px 2px 10px !important;
      }

      /*table.dataTable th.dt-right, table.dataTable td.dt-right {
          word-break: break-all;
      }*/

      /* tooltip for sparkline rendered with bootstrap
         see https://github.com/htmlwidgets/sparkline/issues/4 */
      .jqstooltip {
        -webkit-box-sizing: content-box;
        -moz-box-sizing: content-box;
        box-sizing: content-box;
      }
      div[id=samples_overview]>div>table>thead>tr> th:first-child,
      div[id=samples_overview]>div>table>tbody>tr> td:first-child {
        min-width: 125px;
      }
      div[id=samples_comparison]>div>table>thead>tr> th:first-child,
      div[id=samples_comparison]>div>table>tbody>tr> td:first-child {
        min-width: 125px;
      }
    "))
  ),
  #############################################################################
  ##  DATA PANEL
  #############################################################################
  tabPanel("Data",
    fluidRow(
      fileInput('file1', 'Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.report'
                )
      ),
      selectizeInput("data_dir", "Reports directory",
                     choices=c(system.file("shinyapp/example-data","brain-biopsies",package = "centrifuger"),
                               system.file("shinyapp/example-data","bellybutton-swaps",package = "centrifuger"),
                               "/home/fbreitwieser/projects/centrifuger/cp2",
                               "/home/fbreitwieser/analysis-projects/salzberg-et-al-brain-biome/refseq-reports"),
                     selected=system.file("shinyapp/example-data","brain-biopsies",package = "centrifuger"), 
                     multiple=FALSE,width="80%", options=list(create=TRUE)),
      textInput("file_glob_pattern", "Pattern to find files - use * as wildcard, and capture the sample name with paranthesis",
                value = "%s.report", width="80%"),
      textInput("regex_pattern", "Pattern to find files - use * as wildcard, and capture the sample name with paranthesis",
                value = "(.*).report", width="80%")
    )
  ),
  #############################################################################
  ##  SAMPLES OVERVIEW
  #############################################################################
  tabPanel("Samples overview",
    fluidRow(
        div(
          selectizeInput('sample_selector2',
                         label="No sample directory selected - please update it on the 'Data' tab",
                         choices=NULL, multiple=TRUE,options=list(maxItems=1500, create=TRUE),width='100%'),
         style="font-size:80%"),
        radioButtons("samples_overview_percent",label=NULL, c("reads","percentage"), "reads" ),
        DT::dataTableOutput('samples_overview'),
        uiOutput("view_in_sample_viewer")
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
                         label="No sample directory selected - please update it on the 'Data' tab",
                         choices=NULL, multiple=FALSE,width='100%'),
         style="font-size:80%"),
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
    fluidRow(DT::dataTableOutput('sample_view')),
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
                         label="No sample directory selected - please update it on the 'Data' tab",
                         choices=NULL, multiple=TRUE,options=list(maxItems=1500, create=TRUE),width='100%'),
         style="font-size:80%"),
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
      DT::dataTableOutput('samples_comparison')
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
  )
))

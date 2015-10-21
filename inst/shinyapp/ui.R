library(shiny)
library(centrifugeR)

# identifications that are considered contaminants and may be filtered by default
commoncontaminants=c('s_Homo sapiens','u_unclassified','s_synthetic construct','s_Enterobacteria phage phiX174 sensu lato')
allcontaminants=c(commoncontaminants,'s_Propionibacterium acnes','s_Escherichia coli','s_Saccharomyces cerevisiae', 's_Ralstonia pickettii')

shinyUI(navbarPage("Metagenomics results viewer",
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

      /* tooltip for sparkline rendered with bootstrap
         see https://github.com/htmlwidgets/sparkline/issues/4 */
      .jqstooltip {
        -webkit-box-sizing: content-box;
        -moz-box-sizing: content-box;
        box-sizing: content-box;
      }
      div[id=samples_overview]>div>table>thead>tr> th:first-child,
      div[id=samples_overview]>div>table>tbody>tr> td:first-child {
        min-width: 250px;
      }
      div[id=samples_comparison]>div>table>thead>tr> th:first-child,
      div[id=samples_comparison]>div>table>tbody>tr> td:first-child {
        min-width: 225px;
      }
    "))
  ),
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
      selectizeInput("data_dir", "Data directory on server",
                     choices=c(system.file("data","brain-biopsies",package = "centrifugeR"),
                               system.file("data","bellybutton-swaps",package = "centrifugeR")),
                     selected=system.file("data","brain-biopsies",package = "centrifugeR"), multiple=FALSE,width="80%"),
      textInput("file_glob_pattern", "Pattern to find files - use * as wildcard, and capture the sample name with paranthesis",
                value = "%s.report", width="80%"),
      textInput("regex_pattern", "Pattern to find files - use * as wildcard, and capture the sample name with paranthesis",
                value = "(.*).report", width="80%")
    )
  ),
  tabPanel("Samples overview",
    fluidRow(
        div(
          selectizeInput('sample_selector2',
                         label="No sample directory selected - please update it on the 'Data' tab",
                         choices=NULL, multiple=TRUE,options=list(maxItems=1500, create=TRUE),width='100%'),
         style="font-size:80%"),
        DT::dataTableOutput('samples_overview')
    )
  ),
  tabPanel("Sample viewer",
    fluidRow(
      column(9,
      div(
          selectInput('sample_selector',
                         label="No sample directory selected - please update it on the 'Data' tab",
                         choices=NULL, multiple=FALSE,width='100%'),
         style="font-size:80%"),
      sunburstR::sunburstOutput("sunburst",width="90%")
      ),
      column(3,selectizeInput('contaminant_selector2', label="Filter contaminants",
                    allcontaminants, selected=commoncontaminants,
                    multiple=TRUE,options=list(maxItems=25, create=TRUE, placeholder='filter contaminants'),
                    width="80%")
      )),
    fluidRow(DT::dataTableOutput('sample_view'))

  ),
  tabPanel("Sample comparison",
    fluidRow(
      column(5,
        div(
          selectizeInput('sample_selector3',
                         label="No sample directory selected - please update it on the 'Data' tab",
                         choices=NULL, multiple=TRUE,options=list(maxItems=1500, create=TRUE),width='100%')),
        div(selectizeInput('contaminant_selector3', label="Filter contaminants",
                    allcontaminants, selected=commoncontaminants,
                    multiple=TRUE,options=list(maxItems=25, create=TRUE, placeholder='filter contaminants'),
                    width="100%"))
      ),
      column(1, radioButtons("numeric_display",label=NULL, c("reads","percentage"), "reads" )),
      column(1, radioButtons("input",label=NULL, c("kraken","centrifuge","blastx-lca","metaphlan"), "kraken" )),
      column(1, radioButtons("classification_level",label=NULL,c("S","G","F","O","C","P","D"),"S")),
      #column(1, checkboxInput("update_pubmed", 'Update Pubmed counts', value = FALSE)),
      column(2, checkboxInput("display_heatmap", 'Display Heatmap', value = FALSE),
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
  )
))

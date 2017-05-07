library(shiny)
library(shinydashboard)
library(shinyjs)
library(pavian)
library(shinyFileTree)

convertMenuItem <- function(mi,title) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = title
  mi
}


# identifications that are considered contaminants and may be filtered by default
host_contaminants = c('s_Homo sapiens')
seq_contaminants = c(
  's_synthetic construct',
  'u_unclassified',
  's_Enterobacteria phage phiX174 sensu lato'
)
microbe_contaminants = c(
  's_Propionibacterium acnes',
  's_Escherichia coli',
  's_Saccharomyces cerevisiae',
  's_Ralstonia pickettii'
)
commoncontaminants <- c()  ## this vector is initially filtered
allcontaminants <-
  list(Host = host_contaminants,
       Artificial = seq_contaminants,
       Microbes = microbe_contaminants)
allcontaminants <- unlist(allcontaminants)
names(allcontaminants) <- NULL

shinyUI(navbarPage(
  windowTitle="pavian metagenomics results viewer",
  #########################################################  SIDEBAR
    #includeCSS("style.css"),
    useShinyjs(),
      tabPanel(
        title = "Data",
        fluidRow(
          column(width = 8, includeMarkdown("intro_data.md")),
          column(width = 4, includeHTML("intro_logo.html"))
        ),
        br(),

        box(
          width = 9,
          textInput(
            "cbo_data_dir",
            "Select reports",
            value = system.file("shinyapp/example-data", package = "pavian"),
            width = "100%"
          ),
          shinyFileTree::shinyFileTreeOutput("files_tree"),
          actionButton("btn_set_data_dir", "Load directory content"),
          #fileInput('upload_file', 'Choose file to upload',
          #  accept = c('text/csv', 'text/comma-separated-values',
          #    'text/tab-separated-values', 'text/plain',
          #    '.csv', '.report'),
          #  multiple = TRUE
          #),
          hidden(textInput(
            "txt_file_ext",
            "File extension",
            value = ".report",
            width = "80%"
          )),
          hidden(textInput(
            "regex_pattern",
            "Pattern to find files - use * as wildcard, and capture the sample name with paranthesis",
            value = "(.*).report",
            width = "80%"
          )
        )),
        valueBoxOutput("valuebox_reports_selected", width = 3)
      ),
      tabPanel(
        title = "tab_metagenomes",
        tabsetPanel(selected="Samples comparison",
                    ###############################################  SAMPLES OVERVIEW
                    tabPanel("Samples overview",
                             fluidRow(
                               box(
                                 checkboxInput("opt_samples_overview_percent", label = "Show percentages instead of number of "cladeReads""),
                                 div(style = 'overflow-x: scroll', DT::dataTableOutput('dt_samples_overview')),
                                 uiOutput("view_in_sample_viewer"),
                                 width = 12
                               )
                             )),
                    ###############################################  SAMPLE VIEWER
                    tabPanel(
                      "Sample viewer",
                      fluidRow(
                        box(
                          width = 9,
                          div(
                            selectInput(
                              'sample_selector',
                              label = "",
                              choices = NULL,
                              multiple = FALSE,
                              width = '100%'
                            ),
                            style = "font-size:100%"
                          ),
                          conditionalPanel(
                            "input.sample_view_ui == 'sunburst'",
                            sunburstR::sunburstOutput("sample_view_sunburst", width = "90%")
                          ),
                          conditionalPanel(
                            "input.sample_view_ui == 'sankey'",
                            networkD3::sankeyNetworkOutput("sample_view_sankey", width = "90%")
                          )
                        ),
                        column(3,
                               box(width=NULL,
                                   selectizeInput(
                                     'contaminant_selector',
                                     label = "",
                                     allcontaminants,
                                     selected = commoncontaminants,
                                     multiple = TRUE,
                                     options = list(
                                       maxItems = 25,
                                       create = TRUE,
                                       placeholder = 'Filter clade'
                                     ),
                                     width = "100%"
                                   ),
                                   checkboxInput("opt_remove_root_hits",
                                                 label = "Remove root hits", value = FALSE)),
                               box(
                                 width = NULL,
                                 radioButtons(
                                   "sample_view_ui",
                                   label = "Display",
                                   choices = c("sunburst", "sankey")
                                 ),
                                 checkboxInput(
                                   "synchonize_sampleview_table_and_sunburst",
                                   label = "Synchonize table",
                                   value = FALSE
                                 )
                               )
                        )
                      ),
                      fluidRow(box(
                        width = 12,
                        div(style = 'overflow-x: scroll', DT::dataTableOutput('dt_sample_view'))
                      ),
                      uiOutput("view_in_samples_comparison"))
                    ),
                    ###############################################  SAMPLES COMPARISON
                    tabPanel(
                      "Samples comparison",
                      fluidRow(
                        box(
                          #title = "Options",
                          status = "warning",
                          #collapsible = TRUE,
                          #collapsed = TRUE,
                          width = 8,
                          fluidRow(
                            column(3,
                                   selectizeInput(
                                     "opt_classification_level",
                                     label = "Taxon level",
                                     choices = c(
                                       "Any" = "-",
                                       "Species" = "S",
                                       "Genus" = "G",
                                       "Family" = "F",
                                       "Order" = "O",
                                       "Class" = "C",
                                       "Phylum" = "P",
                                       "Domain" = "D"
                                     ),
                                     selected = "-"
                                   ),
                                   checkboxInput("opt_display_percentage", label = "Show percentages", value =
                                                   FALSE)),
                            column(3,
                                   radioButtons(
                                     "opt_show_taxonReads",
                                     label = "",
                                     choices = c(
                                       "Reads at taxon" = "taxonReads",
                                       "Reads at taxon or lower" = "cladeReads",
                                       "both"
                                     )
                                   )
                            ),
                            column(
                              width = 6,
                              selectizeInput(
                                'contaminant_selector',
                                label = "",
                                allcontaminants,
                                selected = commoncontaminants,
                                multiple = TRUE,
                                options = list(
                                  maxItems = 25,
                                  create = TRUE,
                                  placeholder = 'Filter clade'
                                ),
                                width = "100%"
                              )
                            )
                          )

                        )
                        #column(1, radioButtons("input",label=NULL, c("kraken","centrifuge","blastx-lca","metaphlan"), "kraken" )),
                        #column(1, checkboxInput("update_pubmed", 'Update Pubmed counts', value = FALSE)),
                      ),
                      tabBox(
                        width = 12,
                        tabPanel(
                          "Table",
                          div(style = 'overflow-x: scroll', DT::dataTableOutput('dt_samples_comparison')),
                          actionButton("btn_sc_filter", "Filter"),
                          actionButton("btn_sc_gointo", "Go Into"),
                          shiny::htmlOutput("txt_samples_comparison")
                        ),
                        tabPanel("Heatmap",
                                 fluidRow(
                                   column(width = 8, uiOutput("d3heatmap_samples_comparison")),
                                   column(
                                     width = 4,
                                     radioButtons(
                                       "heatmap_scale",
                                       'Scale',
                                       c("none", "row", "column"),
                                       selected = "none",
                                       inline = TRUE
                                     ),
                                     checkboxGroupInput(
                                       "heatmap_cluster",
                                       "Cluster",
                                       choices = c('row', 'column'),
                                       inline = TRUE
                                     )
                                   )
                                 )),
                        tabPanel("Samples Clustering",
                                 fluidRow(shiny::plotOutput("cluster_plot"))) ## end tabPanel Clustering
                      )
                    )
        )
      ),
      tabPanel(title = "tab_alignment",
              ###############################################  ALIGNMENTS
              tabsetPanel(
                  tabPanel(
                    title = "View alignment",
                    box(width = 12,
                    #shiny::textInput("bam_file","Bam File"),
                    shinyFileTree::shinyFileTreeOutput("bam_files_tree"),
                    shiny::actionButton("btn_get_alignment", "Load alignment"),
                    shiny::checkboxInput("align_loess", "Show smoothed LOESS curve"),
                    shiny::checkboxInput("align_moving_avg", "Show moving average", value = TRUE),
                    shiny::plotOutput("sample_align", brush = brushOpts("align_brush", direction = "x")),
                    shiny::htmlOutput("txt_align_brush")
                    )
                  ),
                  tabPanel(
                    title = "Create alignment",
                    box(width = 12,
                    shiny::actionButton("btn_create_alignment", "Create alignment"),
                    shiny::selectizeInput("cbo_assemblies", choices =
                                            NULL, label = "RefSeq Assemblies"),
                    shiny::actionButton("btn_load_assembly_info", "Load RefSeq assemblies"),
                    div(style = 'overflow-x: scroll', DT::dataTableOutput("dt_assembly_info")),
                    shiny::actionButton("btn_get_reads", "Get "cladeReads"")
                  )
                )
              )

  )
))

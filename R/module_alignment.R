

refseq_assemblies <-
  c("archaea"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/archaea/assembly_summary.txt",
    "bacteria"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/bacteria/assembly_summary.txt",
    "fungi"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/fungi/assembly_summary.txt",
    "invertebrate"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/invertebrate/assembly_summary.txt",
    "plant"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/plant/assembly_summary.txt",
    "protzoa"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/protzoa/assembly_summary.txt",
    "vertebrate_mammalian"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/vertebrate_mammalian/assembly_summary.txt",
    "vertebrate_other"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/vertebrate_other/assembly_summary.txt",
    "viral"="ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq/viral/assembly_summary.txt")

genbank_assemblies <-
  c("archaea"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/archaea/assembly_summary.txt",
    "bacteria"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/bacteria/assembly_summary.txt",
    "fungi"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/fungi/assembly_summary.txt",
    "invertebrate"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/invertebrate/assembly_summary.txt",
    "plant"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/plant/assembly_summary.txt",
    "protzoa"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/protzoa/assembly_summary.txt",
    "vertebrate_mammalian"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/vertebrate_mammalian/assembly_summary.txt",
    "vertebrate_other"="ftp://ftp.ncbi.nlm.nih.gov/genomes/genbank/vertebrate_other/assembly_summary.txt")

ncbi_viral <- c("NCBI Viral Genomes"="http://www.ncbi.nlm.nih.gov/genomes/GenomesGroup.cgi?taxid=10239&cmd=download2")

#assembly_resources = c(refseq_assemblies, genbank_assemblies, ncbi_viral)
assembly_resources = c(refseq_assemblies)

#' UI part of alignment module
#'
#' @param id Shiny namespace id
#'
#' @return UI part of alignment module
#' @export
alignmentModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width=12,
        collapsible=TRUE, collapsed=TRUE,
        title="Instructions",
        "You have to generate a bam alignment file and bai index to be able to view it. In the
second tab 'Download genomes for alignment, you can choose to download the genome from RefSeq
or Genbank.

With bowtie2
"
    ),
    tabsetPanel(
      tabPanel(
        title = "View alignment",
        infoBoxOutput(ns("warn_Rsamtools"), width = 12),
        box(width = 12,
            shiny::fileInput(ns("bam_file"),"Choose BAM and BAI file", accept=c(".bam",".bai"), multiple=TRUE),
            #shinyFileTree::shinyFileTreeOutput(ns("bam_files_tree")),
            shiny::actionButton(ns("btn_get_alignment"), "Show alignment pileup"),
            shiny::checkboxInput(ns("align_loess"), "Show smoothed LOESS curve"),
            shiny::checkboxInput(ns("align_moving_avg"), "Show moving average", value = TRUE),
            shiny::plotOutput(ns("sample_align"), brush = brushOpts(id=ns("align_brush"), direction = "x", resetOnNew = TRUE)),
            shiny::plotOutput(ns("plot_brush"))
        )
      ),
      tabPanel(
        title = "Download genomes for alignment",
        box(width = 12,
            div(class="row-fluid",
                div(class="span6",
                    shiny::selectizeInput(ns("cbo_assemblies"), choices = assembly_resources, selected = "RefSeq bacteria", label = "Genome Assemblies", width="60%")),
                div(class="span6",
                    shiny::actionButton(ns("btn_load_assembly_info"), "Get assembly information",width="35%"))),
            div(style = 'overflow-x: scroll', DT::dataTableOutput(ns("dt_assembly_info"))),
            htmlOutput(ns("dl_genome"))
        )
      )
    )
  )

}

#' Server part of alignment module
#'
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session
#' @param samples_df \code{data.frame} for samples
#'
#' @return Alignment module server functionality
#' @export
#' @import shinydashboard
alignmentModule <- function(input, output, session, samples_df) {

  if (!require(RSamtools, quietly=TRUE)) {
    shinyjs::disable("bam_file")
    shinyjs::disable("btn_get_alignment")
    shinyjs::disable("align_loess")
    shinyjs::disable("align_moving_avg")
  }

  output$warn_Rsamtools <- renderInfoBox({
    if (!require(RSamtools, quietly=TRUE)) {
      valueBox(
        "Functionality requires package Rsamtools",
        "See https://bioconductor.org/packages/release/bioc/html/Rsamtools.html for installation instructions.",
        icon = icon("exclamation-triangle"),
        color = "red", width = 12
      )
    }
  })

  req_bioc <- function(pkg) {
    req(require(pkg))
    validate(need(require(pkg, character.only=TRUE, quietly=TRUE), message=sprintf(
      "%s is needed for this functionality. See https://bioconductor.org/packages/release/bioc/html/%s.html for information on how to install it",
      pkg, pkg)))
  }

  bam_file <- reactive({
    req_bioc("RSamtools")
    bam_file <- system.file("shinyapp","example-data","CP4-JC_polyomavirus.bam", package="pavian")
    if (!is.null(input$bam_file)) {
      validate(need(
        is.data.frame(input$bam_file) &&
          nrow(input$bam_file)== 2  &&
          sum(grepl(".bam$",input$bam_file$name))==1 &&
          sum(grepl(".bai$",input$bam_file$name))==1,
        message="Need both .bam and .bai files"))
      file.rename(input$bam_file$datapath[1], file.path(dirname(input$bam_file$datapath[1]),input$bam_file$name[1]))
      file.rename(input$bam_file$datapath[2], file.path(dirname(input$bam_file$datapath[2]),input$bam_file$name[2]))
      bam_pos <- grep(".bam$",input$bam_file$name)
      bam_file = file.path(dirname(input$bam_file$datapath[bam_pos]),input$bam_file$name[bam_pos])
    }
    bam_file
  })

  plot_pileup_act <- eventReactive(input$btn_get_alignment, {
    req_bioc("RSamtools")
    plot_pileup(pileup(), nreads(), seq_lengths(),
                input$align_loess,
                text_size = 4
    )
  })

  pileup <- reactive({ get_pileup(bam_file(), input$align_moving_avg) })
  pileup_nm <- reactive({ get_pileup(bam_file(), FALSE) })
  nreads <- reactive({ get_nreads(bam_file()) })
  seq_lengths <- reactive({ get_seqlengths(bam_file()) })


  output$sample_align <- renderPlot({
    req_bioc("RSamtools")
    plot_pileup_act()
  })

  ranges <- reactiveValues(x = NULL)

  observe({
    brush <- input$align_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
    } else {
      ranges$x <- NULL
    }
  })

  output$plot_brush <- shiny::renderPlot({
    #req(pileup())
    #req(length(nreads()) == 1)
    req(ranges$x)
    plot_pileup(pileup_nm(), nreads(), seq_lengths(),
                input$align_loess,
                text_size = 4,
                show_step = FALSE
    ) + coord_cartesian(xlim = ranges$x)
  })

  assembly_info <- eventReactive(input$btn_load_assembly_info, {
    url <- input$cbo_assemblies

    # Check that the first two lines are headers, and make them nice
    colClasses = c(
      AC = "character",
      bioproject = "NULL",
      biosample = "NULL",
      wgs_master = "NULL",
      refseq_category = "NULL",
      TaxId = "character",
      Species_TaxID = "character",
      Name = "character",
      Strain = "character",
      isolate = "NULL",
      Version = "character",
      assembly_level = "NULL",
      release_type = "NULL",
      genome_rep = "NULL",
      Date = "Date",
      asm_name = "NULL",
      Submitter = "NULL",
      gbrs_paired_asm = "NULL",
      paired_asm_comp = "NULL",
      URL = "character",
      excluded_from_refseq = "NULL"
    )

    ai <- withProgress({
      # assembly_accession    bioproject  biosample   wgs_master  refseq_category
      # taxid   species_taxid   organism_name   infraspecific_name  isolate
      # version_status  assembly_level  release_type    genome_rep  seq_rel_date    asm_name    submitter   gbrs_paired_asm paired_asm_comp ftp_path    excluded_from_refseq
      utils::read.delim(url, comment.char = "#",
                 colClasses = as.character(colClasses),
                 col.names = names(colClasses),
                 fill = TRUE,
                 header = FALSE
      )
    }, message = "Downloading and parsing assembly info ... ")

    ai <- ai[ai$Version == "latest", setdiff(colnames(ai), "Version")]

    #ai$Strain <- sub("^strain=", "", ai$Strain)
    #ends_with_infraspecific_name <-
    #  substr(ai$Name,
    #         nchar(ai$Name) - nchar(ai$Strain) + 1,
    #         nchar(ai$Name)
    #  ) == ai$Strain
    #ai$Name[!ends_with_infraspecific_name] <-
    #  paste(ai$Name, ai$Strain)[!ends_with_infraspecific_name]

    beautify_colnames(ai)
  })


  output$dt_assembly_info <- DT::renderDataTable({
    ai <- assembly_info()
    req(ai)

    DT::datatable(
      ai,
      filter = 'top'
    )
  })

  output$dl_genome <- renderUI ({
    req(input$dt_assembly_info_rows_selected)
    res <- assembly_info()[input$dt_assembly_info_rows_selected,]

    fname <- paste0(basename(res$URL),"_genomic.fna")
    dl_link <- sprintf("%s/%s.gz", res$URL, fname)

    myname <- sprintf("%s-%s-%s.fna", gsub("[ \\]","_", res$Name), res$TaxId, res$AC)

    shiny::tagList(
      h3(res$Name, res$Strain),
      p(a(dl_link,href=dl_link)),
      p("To download and build an index, execute the following commands:"),
      code(
        sprintf("curl %s | gunzip -c > %s", dl_link, myname),
        sprintf("# Optional: sed -i '/^>/ s/ /_/g' %s", myname),
        sprintf("bowtie2-build %s %s", myname, myname))
    )
  })
}

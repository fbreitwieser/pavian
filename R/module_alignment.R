

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
        collapsible=TRUE, collapsed=FALSE,
        title="Instructions",
        HTML("The alignment viewer requires a <a target='blank' href='https://genome.ucsc.edu/goldenpath/help/bam.html'>BAM alignment file and BAI index</a>.

To generate a BAM file, download a genome of interest, and align to it with an aligner like <a target='blank' href='http://bowtie-bio.sourceforge.net/bowtie2/index.shtml'>Bowtie2</a> or <a target='blank' href='https://github.com/lh3/bwa'>bwa mem</a>. The resulting SAM file can be compressed into a binary BAM file and indexed with <a target='blank' href='http://www.htslib.org'>samtools</a>.
")
    ),
    tabsetPanel(
      tabPanel(
        title = "View alignment",
        uiOutput(ns("warn_Rsamtools"), width = 12),
        box(width = 12,
            fluidRow(
              column(5,shiny::fileInput(ns("bam_file"),"Upload BAM and BAI file", accept=c(".bam",".bai"), multiple=TRUE)),
              column(3,shiny::actionButton(ns("btn_get_alignment"), "Load example data")),
              column(4,shiny::sliderInput(ns("mapq"),"Minimum MAPQ",0,50,0,step=1))),
            shinyjs::hidden(shiny::checkboxInput(ns("align_loess"), "Show smoothed LOESS curve")),
            shinyjs::hidden(shiny::checkboxInput(ns("align_moving_avg"), "Show moving average", value = TRUE)),
            shiny::uiOutput(ns("bam_name")),
            br(),
            DT::dataTableOutput(ns("table")),
            br(),
            shiny::plotOutput(ns("sample_align"), brush = brushOpts(id=ns("align_brush"), direction = "x", resetOnNew = TRUE), height = "200px"),
            shiny::plotOutput(ns("plot_brush"), height = "200px")
        )
      ),
      tabPanel(
        title = "Download genomes for alignment",
        box(width = 12,
            HTML(
            "'Get assembly information' gathers and displays the assembly_summary.txt from the selected domain from <a target='blank' href='ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq'>ftp://ftp.ncbi.nlm.nih.gov/genomes/refseq</a>. An active internet connection is required, and currently no files are cached."),
            br(),
            br(),
            div(class="row-fluid",
                div(class="col-sm-6 col-xs-12",
                    shiny::selectizeInput(ns("cbo_assemblies"), choices = assembly_resources, selected = "RefSeq bacteria", label = NULL, width="100%")),
                div(class="col-sm-6 col-xs-12",
                    shiny::actionButton(ns("btn_load_assembly_info"), "Get assembly information",width="100%"))),
            br(),
            br(),
            DT::dataTableOutput(ns("dt_assembly_info")),
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
#' @param sample_data \code{data.frame} for samples
#' @param datatable_opts Additional options for creating the datatable.
#'
#' @return Alignment module server functionality
#' @export
#' @import shinydashboard
alignmentModule <- function(input, output, session, sample_data, datatable_opts) {

  my_bam_file <- reactiveValues(val = NULL, txt = NULL)

  output$bam_name <- renderUI({
    req(my_bam_file$txt)
    my_bam_file$txt
  })

  pileup <- reactive({
    req(my_bam_file$val)
    my_bam_file$txt <- shiny::tagList("Loaded ", shiny::strong(basename(my_bam_file$val)),". Click on a row to see pileup.")
    unique(get_pileup(my_bam_file$val, min_mapq = input$mapq))
  })

  pileup_brush <- reactive({
    req(pileup())
    req(ranges$x)
    req(input$table_rows_selected)

    selected_row <- seqinfo_df()[input$table_rows_selected, , drop=FALSE]

    mypileup <- pileup() %>%
      dplyr::filter(seqnames %in% selected_row$seqnames &
                      findInterval(pos,ranges$x) == 1)

    sel <- mypileup$count > 0
    validate(need(sum(sel) > 0, message = "No reads in selected region"))

    attr(mypileup, "covered_bp") = tapply(mypileup[sel,"pos"], mypileup[sel,"seqnames"], function(x) length(unique(x)))
    attr(mypileup, "sum_count") = tapply(mypileup[sel,"count"], mypileup[sel,"seqnames"], sum)

    mypileup
  })

  output$warn_Rsamtools <- renderUI({
    if (!requireNamespace("Rsamtools")) {
      shinyjs::disable("bam_file")
      shinyjs::disable("btn_get_alignment")
      shinyjs::disable("align_loess")
      shinyjs::disable("align_moving_avg")

      infoBox(
        "Functionality requires package Rsamtools",
        "See https://bioconductor.org/packages/release/bioc/html/Rsamtools.html for installation instructions.",
        icon = icon("exclamation-triangle"),
        color = "red", width = 12
      )
    }
  })

  req_bioc <- function(pkg) {
    #req(require(pkg, character.only=TRUE))
    validate(need(requireNamespace(pkg), message=sprintf(
      "%s is needed for this functionality. See https://bioconductor.org/packages/release/bioc/html/%s.html for information on how to install it",
      pkg, pkg)))
  }

  observeEvent(input$bam_file, {
    req(input$bam_file)
    message("got BAM file")
    bam_file <- NULL
    if (!is.null(input$bam_file)) {
      if (!is.data.frame(input$bam_file)) {
        my_bam_file$txt <- "Upload is no data.frame."
      } else if (nrow(input$bam_file) != 2) {
        my_bam_file$txt <- shiny::strong(style="color:red;","Please upload exactly two files at once; one BAM file its corresponding BAI file.")
      } else if (sum(grepl(".bam$",input$bam_file$name, ignore.case = T)) !=1 ) {
        my_bam_file$txt <- shiny::strong(style="color:red;","Did not get a file with the extension .bam or .BAM .")
      } else if (sum(grepl(".bai$",input$bam_file$name)) !=1 ) {
        my_bam_file$txt <- shiny::strong(style="color:red;","Did not get a file with the extension .bai or .BAI .")
      } else {
        file.rename(input$bam_file$datapath[1], file.path(dirname(input$bam_file$datapath[1]),input$bam_file$name[1]))
        file.rename(input$bam_file$datapath[2], file.path(dirname(input$bam_file$datapath[2]),input$bam_file$name[2]))
        bam_pos <- grep(".bam$",input$bam_file$name)
        bam_file = file.path(dirname(input$bam_file$datapath[bam_pos]),input$bam_file$name[bam_pos])
      }
    }
    message("BAM file = ",bam_file)
    my_bam_file$val <- bam_file
  })

  observeEvent(input$btn_get_alignment, {
    my_bam_file$val <- system.file("shinyapp","example-data","CP4-JC_polyomavirus.bam", package="pavian")
  })

  seqinfo_df <- reactive({
    req(my_bam_file$val)
    covered_bp <- attr(pileup(),"covered_bp")
    covered_bp[setdiff(names(seq_lengths()),names(covered_bp))] <- 0
    sum_count <- attr(pileup(),"sum_count")
    sum_count[setdiff(names(seq_lengths()),names(sum_count))] <- 0

    seq_info_df <-
      data.frame(seqnames=names(seq_lengths()),
                 genome_size=seq_lengths(),
                 avg_coverage=signif(sum_count/seq_lengths(),3),
                 covered_bp=covered_bp,
                 n_reads=nreads(),
                 avg_mapq=signif(avg_mapq(),3))


    seq_info_df$perc_covered = 100*signif(seq_info_df$covered_bp / seq_info_df$genome_size,3)
    seq_info_df[order(-seq_info_df$n_reads,-seq_info_df$perc_covered), , drop=F]
  })

  seqinfo_df_brush <- reactive({
    req(my_bam_file$val)
    covered_bp <- attr(pileup_brush(),"covered_bp")
    covered_bp[setdiff(names(seq_lengths_range_x()),names(covered_bp))] <- 0
    sum_count <- attr(pileup_brush(),"sum_count")
    sum_count[setdiff(names(seq_lengths_range_x()),names(sum_count))] <- 0

    seq_info_df <- do.call(rbind,lapply(names(seq_lengths()), function(name) {
      data.frame(seqnames=name,
                 genome_size=seq_lengths_range_x()[name],
                 avg_coverage=signif(sum_count[name]/seq_lengths_range_x()[name],3),
                 covered_bp=covered_bp[name],
                 n_reads=nreads_range_x()[name]
      )
    }))

    seq_info_df$perc_covered = 100*signif(seq_info_df$covered_bp / seq_info_df$genome_size,3)
    seq_info_df[order(-seq_info_df$n_reads,-seq_info_df$perc_covered), , drop=F]
  })




  output$table <- DT::renderDataTable({
    req(my_bam_file$val)
    my_title = sprintf("%s-alignment-summary_%s", basename(my_bam_file$val), format(Sys.time(), "%y%m%d"))
    datatable(seqinfo_df(), selection = 'single',
              rownames = FALSE,
              colnames = c("Sequence"="seqnames","Length"="genome_size","# of reads"="n_reads",
                "Covered bp"="covered_bp","Average\ncoverage"="avg_coverage",
                "Average\nMAPQ"="avg_mapq",
                "Percent\ncovered"="perc_covered"),
              extensions = datatable_opts$extensions,
              class=datatable_opts$class,
              options=list(
                buttons = list('pageLength', list(extend='excel',title=my_title) , list(extend='csv', title= my_title), 'copy', 'colvis'),
                columnDefs=list(
      list(targets = c(2:ncol(seqinfo_df()), orderSequence = c('desc', 'asc'))
                              )))) %>%
      DT::formatCurrency(2, currency = '', digits = 0 ) %>%
      DT::formatString(3, suffix = "x") %>%
      DT::formatString(7, suffix = "%")
  })

  #plot_pileup_act <- eventReactive(input$btn_get_alignment, {
  plot_pileup_act <- reactive ({
    req_bioc("Rsamtools")
    req(input$table_rows_selected)
    selected_row <- seqinfo_df()[input$table_rows_selected, , drop=FALSE]
    pileup_res <- pileup() %>% dplyr::filter(seqnames %in% selected_row$seqnames)
    plot_pileup(pileup_res,
                selected_row,
                text_size = 4
    )
  })

  bam2 <- reactive( {
    get_bam2(my_bam_file$val)
  })

  bam2_mapq <- reactive( {
    req(input$mapq)
    bam2() %>% dplyr::filter(mapq >= input$mapq)
  })

  selected_bam <- reactive( {
    req(selected_seq())
    bam2_mapq() %>% dplyr::filter(rname %in% selected_seq())
  })

  seq_lengths <- reactive({ get_seqlengths(my_bam_file$val) })

  selected_seq <- reactive({
    req(input$table_rows_selected)
    seqinfo_df()[input$table_rows_selected,"seqnames"]
  })

  nreads <- reactive({
    bam <- bam2_mapq()
    res <- tapply(bam$qname, bam$rname, function(x) length(unique(x)))
    res[is.na(res)] <- 0
    res
  })

  avg_mapq <- reactive({
    bam <- bam2_mapq()
    res <- tapply(bam$mapq, bam$rname, mean)
    res[is.na(res)] <- 0
    res
  })

  nreads_range_x <- reactive({
    bam <- selected_bam()
    sel <- findInterval(bam$pos, ranges$x)==1
    tapply(bam$qname[sel], bam$rname[sel], function(x) length(unique(x)))
  })

  seq_lengths_range_x <- reactive({
    rr <- ranges$x[2] - ranges$x[1]
    a <- get_seqlengths(my_bam_file$val)
    sapply(a, function(x) rr)
  })


  output$sample_align <- renderPlot({
    req(my_bam_file$val)
    req_bioc("Rsamtools")
    #ranges$x <- NULL
    plot_pileup_act()
  }, res = 72)

  ranges <- reactiveValues(x = NULL)

  observe({
    brush <- input$align_brush
    if (!is.null(brush)) {
      ranges$x <- round(c(brush$xmin, brush$xmax))
    } else {
      ranges$x <- NULL
    }
  })

  output$plot_brush <- shiny::renderPlot({
    #xlim <- ranges$x
    #xlim[1] <- max(0,xlim[1])
    #xlim[2] <- min(seqinfo_df_brush()$genome_size,xlim[2])
    selected_row <- seqinfo_df_brush()[input$table_rows_selected, , drop=FALSE]

    plot_pileup(pileup_brush(), selected_row,
                text_size = 4
    ) #+ coord_cartesian(xlim = xlim)
  }, res=72)

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
      assembly_rank = "NULL",
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
      # version_status  assembly_rank  release_type    genome_rep  seq_rel_date    asm_name    submitter   gbrs_paired_asm paired_asm_comp ftp_path    excluded_from_refseq
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
    my_title <- sprintf("%s-assembly-info-%s", names(refseq_assemblies)[refseq_assemblies==input$cbo_assemblies], format(Sys.time(), "%y%m%d"))
    DT::datatable(
      ai,
      filter = 'bottom',
      extensions = datatable_opts$extensions,
      class=datatable_opts$class,
      options(buttons = list('pageLength', list(extend='excel',title=my_title) , list(extend='csv', title= my_title), 'copy', 'colvis'))
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

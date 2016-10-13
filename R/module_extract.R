
#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
extractModuleUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      box(width = 12, title = "Select sample and tax", background = "green",
        selectInput(
          ns('sample_selector'), label = "Sample",
          choices = NULL, multiple = FALSE,
          width = '100%'
        ),
        selectInput(
          ns('tax_selector'), label = "Tax",
          choices = NULL, multiple = FALSE,
          width = '100%'
        )
      ),
      tabBox(
        tabPanel("Centrifuge results",
                 DT::dataTableOutput(ns('dt'))),
        tabPanel("FASTA",
                 textOutput(ns('fasta')))
      )
    )
  )
}

#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param sample_data
#'
#' @return
#' @export
#'
#' @examples
extractModule <- function(input, output, session, sample_data, reports) {
  observeEvent(reports(), {
    updateSelectInput(session, 'sample_selector',
                      choices = names(reports()),
                      selected = names(reports())[1])
  })


  tbx <- reactive({
    dat <- sample_data()
    if (!"CentrifugeOutFilePath" %in% colnames(dat))
      return()

    cf_out <- dat[dat$Name == input$sample_selector,"CentrifugeOutFilePath"]
    if (!file.exists(cf_out) || !file.exists(paste0(cf_out,".tbi")))
      return()

    return(Rsamtools::TabixFile(cf_out, yieldSize = 100))
  })

  tbx_results <- reactive({
    req(tbx)
    scanTabix(tbx(), GRanges(input$tax_selector, IRanges(c(50), width=100000)))[[1]]
  })

  output$dt <- DT::renderDataTable({
    req(tbx_results())
    read.delim(tbx_results(), header=F,
               col.names = c("readID","seqID","taxID","score","2ndBestScore","hitLength","queryLength","numMatches","readSeq"))
  })

  output$fasta <- renderText({
    req(tbx_results())
    res <- read.delim(tbx_results(), header=F,
               col.names = c("readID","seqID","taxID","score","2ndBestScore","hitLength","queryLength","numMatches","readSeq"))
    sprintf(">%s\n%s", res$readID, res$readSeq)
  })
}


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
      box(width = 12,
          title = "Output",
          textOutput(ns('reads'))
      )
    )
  )
}

#' Title
#'
#' @param input
#' @param output
#' @param session
#' @param samples_df
#'
#' @return
#' @export
#'
#' @examples
extractModule <- function(input, output, session, samples_df) {
  output$reads <- renderText({
    # AAAAAAAa
  })

}

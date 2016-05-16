library(shiny)

dragNdropModuleUI <- function(id) {
  ns <- NS(id)
  shiny::tagsList(
    h3("Drop datasets"),
    div(class="col-xs-12", id="drop-area",ondragover="dragOver(event)",
        ondrop="dropData(event)"),
    tableOutput(ns(table))
  )
}

dragNdropModule <- function(input, output, session) {
  
}

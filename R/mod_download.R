#' download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_download_ui <- function(id){
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadData"), "Download")
  )
}

#' download Server Functions
#'
#' @noRd
mod_download_server <- function(id, .df, .filename){
  stopifnot(is.reactive(.df))
  stopifnot(is.reactive(.filename))

  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$downloadData <- downloadHandler(
      filename = function() paste0(.filename(), ".xlsx"),
      content = function(file) {
        openxlsx::write.xlsx(
          .df(),
          file
        )
      }
    )
  })
}

## To be copied in the UI
# mod_download_ui("download_1")

## To be copied in the server
# mod_download_server("download_1")

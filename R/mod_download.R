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
mod_download_server <- function(id, label, .data, .emp, .dates, .codes){
  stopifnot(is.reactive(.data))
  stopifnot(is.reactive(.emp))
  stopifnot(is.reactive(.dates))
  stopifnot(is.reactive(.codes))

  moduleServer( id, function(input, output, session){
    ns <- session$ns
    .filename = reactive( {
      paste0(
        label, ", ",
        .emp(), ", ",
        glue::glue_collapse(.codes(), "-"), ", ",
        .dates()[1], " to ", .dates()[2],
        ".csv"
      )
    })
    output$downloadData <- downloadHandler(
      filename = .filename(),
      content = function(file) {
        write.csv(.data(), file, row.names = FALSE)
      }
    )
  })
}

## To be copied in the UI
# mod_download_ui("download_1")

## To be copied in the server
# mod_download_server("download_1")

#' file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_file_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' file Server Functions
#'
#' @noRd
mod_file_server <- function(id, label, .emp, .dates, .codes){
  stopifnot(!is.reactive(label))
  stopifnot(is.reactive(.emp))
  stopifnot(is.reactive(.dates))
  stopifnot(is.reactive(.codes))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    .codes_hyphen <- reactive(glue::glue_collapse(.codes(), "-"))
    .date_range <- reactive(glue::glue("{.dates()[1]} to {.dates()[2]}"))

    .filename <- reactive(
      glue::glue("{label}, {.emp()}, {.codes_hyphen()}, {.date_range()}")
    )

  })
}

## To be copied in the UI
# mod_file_ui("file_1")

## To be copied in the server
# mod_file_server("file_1")

#' raw UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_raw_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_download_ui(id),
    reactable::reactableOutput(NS(id, "table"))
  )
}

#' raw Server Functions
#'
#' @noRd
mod_raw_server <- function(id, label, .df, .emp, .dates, .codes){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$table <- reactable::renderReactable(
      pr::style_reactable(.df())
    )

    output$downloadData <- mod_download_server(id = "placeholder",
                                               label = "raw",
                                               .df = .df,
                                               .emp = .emp,
                                               .dates = .dates,
                                               .codes = .codes)

  })
}

## To be copied in the UI
# mod_raw_ui("raw_1")

## To be copied in the server
# mod_raw_server("raw_1")

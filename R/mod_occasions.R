#' occasions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_occasions_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_download_ui(id),
    reactable::reactableOutput(NS(id, "table"))
  )
}

#' occasions Server Functions
#'
#' @noRd
mod_occasions_server <- function(id, label, .data, .emp, .dates, .codes){
  stopifnot(is.reactive(.data))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    .occasions <- reactive({
      .data() |>
        pr::add_occasion() |>
        dplyr::select(code, year, year_half, occasion)
    })

    output$table <- reactable::renderReactable({

      records <- nrow(.occasions()) > 0
      validate(
        need(records, "There are no records for the selected choices.")
      )

      .occasions() |> pr::style_reactable()

      })

    output$downloadData <- mod_download_server(id = "placeholder",
                                               label = "occasions",
                                               .data = .occasions,
                                               .emp = .emp,
                                               .dates = .dates,
                                               .codes = .codes)
  })
}

## To be copied in the UI
# mod_occasions_ui("occasions_1")

## To be copied in the server
# mod_occasions_server("occasions_1")

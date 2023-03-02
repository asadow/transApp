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
mod_occasions_server <- function(id, .df, .emp, .dates, .codes){
  stopifnot(is.reactive(.df))
  stopifnot(is.reactive(.emp))
  stopifnot(is.reactive(.dates))
  stopifnot(is.reactive(.codes))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    .occasions <- reactive(pr::add_occasion(.df()))

    ## Using .to_download instead of .table inside of mod_download_server()
    .to_download <- reactive(label = "Select vars to download",{
      .occasions() |>
        dplyr::select(barg, dept, crew, code, surname,
                      given_names, employee_no,
                      year_half, occasion, hours, hours_day, date)
    })

    .table <- reactive(label = "Select vars to display", {
      .occasions() |>
        dplyr::select(code, surname,
                      given_names, employee_no, year_half,
                      occasion, hours, hours_day, date)

    })

    output$table <- reactable::renderReactable({

      pr::style_reactable(.table())

      })

    .filename <- mod_file_server(NULL, label = id, .emp, .dates, .codes)

    output$downloadData <- mod_download_server(NULL, .to_download, .filename)

    return(.table)

  })
}

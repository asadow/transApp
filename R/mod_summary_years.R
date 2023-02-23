#' summary_years UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_years_ui <- function(id, .df){

  ## Convert factor before max() or min()
  max_year <- .df$year |> as.character() |> as.numeric() |> max()
  min_year <- .df$year |> as.character() |> as.numeric() |> min()

  ns <- NS(id)
  tagList(
    sliderInput(
      inputId = ns("year"),
      label = "Choose years",
      step = 1,
      sep = "",
      min = min_year,
      max = max_year,
      value = c(min_year, max_year)
    )
  )
}

#' summary_years Server Functions
#'
#' @noRd
mod_summary_years_server <- function(id, data){

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    .summary <- reactive({
      data |>
        dplyr::filter(
          date >= input$date[1] & date <= input$date[2]
          & code %in% input$code
          ) |>
        pr::summarize_years() |>
        pr::style_reactable()
      })

    output$table <- reactable::renderReactable(.summary())

  })
}

## To be copied in the UI
# mod_summary_years_ui("summary_years_1")

## To be copied in the server
# mod_summary_years_server("summary_years_1")

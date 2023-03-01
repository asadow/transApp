#' summarise UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summarise_ui <- function(id){
  ns <- NS(id)
  cols <- c("code", "year_half", "occasion")
  tagList(
    selectInput(ns("vars_g"), "Group by", choices = NULL, multiple = TRUE),
    selectInput(ns("vars_s"), "Summarise", choices = NULL, multiple = TRUE),
    tableOutput(ns("summary"))
  )
}

#' summarise Server Functions
#'
#' @noRd
mod_summarise_server <- function(id, .df){
  moduleServer( id, function(input, output, session){
    stopifnot(is.reactive(.df))

    observeEvent(.df(), {
      updateSelectInput(session, "vars_g", choices = names(.df()))
    })

    observeEvent(input$vars_g, {
      df_names <- names(.df())
      updateSelectInput(
        session,
        "vars_s",
        choices = df_names[!df_names %in% input$vars_g]
        )
    })

    ns <- session$ns

    output$summary <- renderTable({
      .df() %>%
        dplyr::group_by(dplyr::across(tidyselect::all_of(input$vars_g))) %>%
        dplyr::summarise(
          dplyr::across(tidyselect::all_of(input$vars_s), mean),
          n = dplyr::n()
          )
    })
  })
}

## To be copied in the UI
# mod_summarise_ui("summarise_1")

## To be copied in the server
# mod_summarise_server("summarise_1")

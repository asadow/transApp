#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  .data <- mod_data_server("data_1", .data = pr::load_sick())

  mod_summary_years_server("data_1", .data = pr::load_sick())

  output$raw_table <- reactable::renderReactable(pr::style_reactable(.data()))


  # output$table <- renderTable(.data())
  # mod_summary_years_server("summary_years_1", .data = pr::load_sick())
  # .data <- mod_summary_years_server("summary_years_1", .data = pr::load_sick())
  # output$table <- DT::renderDT(.data)
  # data <- dataServer("data", emptrans)


  # x <- inputServer("data")

  # calendarServer2("calendar", data)
  # occasionServer("table", data)

  # observeEvent(input$ss, {
  #   shinyscreenshot::screenshot(filename = x$employee)
  # })
}

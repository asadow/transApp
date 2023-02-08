#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  data <- dataServer("data", emptrans)

  # x <- inputServer("data")

  # calendarServer2("calendar", data)
  # occasionServer("table", data)

  # observeEvent(input$ss, {
  #   shinyscreenshot::screenshot(filename = x$employee)
  # })
}

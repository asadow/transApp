#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  raw <- pr::load_sick()
  ## NB can't get mod_summary_years_server to work...

  .selected <- mod_data_server("selected_data", df = raw)
  # mod_summary_years_server("data1", .data = raw_data)
  .occasions <- mod_occasions_server("data1", .data = .selected$.data)

  mod_download_server("data1",
                      "raw",
                      .data = .selected$.data,
                      .emp = .selected$.emp,
                      .date_range = .selected$.date_range,
                      .codes = .selected$.codes)

  mod_download_server("data2",
                      "occasions",
                      .data = .occasions,
                      .emp = .selected$.emp,
                      .date_range = .selected$.date_range,
                      .codes = .selected$.codes)

  output$table <- reactable::renderReactable(
    pr::style_reactable(.selected$.data())
    )


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

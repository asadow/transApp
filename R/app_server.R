#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  raw <- pr::load_sick()

  c(.data, .emp, .dates, .codes) %<-% mod_data_server("selectdata", df = raw)

  mod_occasions_server("occdata", "occasions", .data, .emp, .dates, .codes)
  mod_occasions_server("rawdata", "raw-data", .data, .emp, .dates, .codes)
}

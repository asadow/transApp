#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  raw <- pr::load_sick()

  c(.click, .df, .emp, .dates, .codes) %<-% mod_data_server("selectdata", raw)

  observeEvent(
    .click(),
    updateTabsetPanel(session, "navbars", selected = "Results"),
    label = "Switch Tab on Click"
    )

  .occ <- mod_occasions_server("occasions", .df, .emp, .dates, .codes)
  mod_summarise_server("summary", .occ)


}


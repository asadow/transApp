#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
theme <- bslib::bs_theme(
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#A30808",
  secondary = "#F8D505"
)

app_ui <- function(request) {

  tagList(
    golem_add_external_resources(),
    navbarPage(
      title = "Application for Employee Transactions",
      theme = theme,

      tabPanel(
        "Parameters",
         helpText("All boxes under `Choose` can be clicked on and typed in.
                    Hit Backspace once to remove entries."),
         mod_data_ui("selectdata", df = pr::load_sick())),

      tabPanel(
        "Results",
         tabPanel("Occasions", mod_occasions_ui("occasions"))
        ),

      navbarMenu(
        "Other",
         tabPanel("Data Clean-Up", "There appear to be duplicate transactions."),
        )
      )
  )

}
#
# app_ui <- function(request) {
#
#   tagList(
#     golem_add_external_resources(),
#     fluidPage(
#       titlePanel("Application for Employee Transactions"),
#       helpText("All boxes under `Choose` can be clicked on and typed in.
#                Hit Backspace once to remove entries."),
#       sidebarLayout(
#         sidebarPanel(
#           mod_data_ui("selectdata", df = pr::load_sick())
#           ),
#         mainPanel(
#           tabsetPanel(
#             tabPanel("Occasions", mod_occasions_ui("occasions")),
#             # tabPanel("Raw Data", mod_raw_ui("rawdata"))
#           )
#         )
#       )
#     )
#   )
# }

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "transApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

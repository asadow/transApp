#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      titlePanel("Application for Employee Transactions"),
      helpText("All boxes under `Choose` can be clicked on and typed in.
               Hit Backspace once to remove entries."),
      sidebarLayout(
        sidebarPanel(
          mod_data_ui("selected_data", df = pr::load_sick())
          ),
        mainPanel(
          tabsetPanel(
            # tabPanel(
            #   "Yearly Summary",
            #   reactable::reactableOutput(NS("data1", "table"))
            #   ),
            tabPanel(
              "Occasions",
              mod_download_ui("data2"),
              reactable::reactableOutput(NS("data1", "table"))
            ),
            tabPanel("Raw Data",
                     mod_download_ui("data1"),
                     reactable::reactableOutput("table"))
          )
        )
      )
      # mod_summary_years_ui("summary_years_1", .data = pr::load_sick()),
      # summaryOutputTable("summary_years_1")
      # DT::DTOutput("table")

      # mainPanel(
      #   calendarOutputPlot("calendar"),
      #   occasionOutputTable("table")
      # ),
      # actionButton("ss", "Take a screenshot")
    )
  )
}

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

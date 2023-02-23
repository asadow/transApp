#' data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_ui <- function(id, df){
  stopifnot(!is.reactive(df))
  stopifnot(is.data.frame(df))

  ns <- NS(id)

  ubarg <- sort_unique(df$barg)
  udept <- sort_unique(df$dept_desc) |> na.omit()
  ucrew <- sort_unique(df$crew_desc) |> na.omit()
  ucode <- sort_unique(df$code) |> na.omit()

  # ## Convert factor before max() or min()
  # max_year <- df$year |> as.character() |> as.numeric() |> max()
  # min_year <- df$year |> as.character() |> as.numeric() |> min()

  fluidRow(
    column(3,
           wellPanel(
              selectInput(
                ns("barg"),
                label = "Choose bargaining unit(s)",
                choices =  c("All", as.character(ubarg)),
                # multiple = TRUE,
                selected = "All"
              ),

              selectInput(
                ns("dept_desc"),
                label = "Choose department(s)",
                choices =  c("All", as.character(udept)),
                # multiple = TRUE,
                selected = "All"
              ),

              selectInput(
                ns("crew_desc"),
                label = "Choose crew(s)",
                choices =  c("All", as.character(ucrew)),
                # multiple = TRUE,
                selected = "All"
              ),
           )
    ),
    column(3,
           wellPanel(

            selectInput(
              inputId = ns("code"),
              label = "Choose code(s)",
              choices = ucode,
              multiple = TRUE,
              selected = "SICK"
            ),
            # shinyWidgets::pickerInput(
            #   inputId = ns("code"),
            #   label = "Choose code(s)",
            #   choices = ucode,
            #   options = list(`actions-box` = TRUE),
            #   multiple = TRUE,
            #   selected = "SICK"
            # ),

            dateRangeInput(
              inputId = ns("date"),
              label = "Choose dates",
              startview = "decade",
              start = min(df$date),
              min = min(df$date),
              end = max(df$date),
              max = max(df$date)
            ),

            sliderInput(
              ns("days_range"),
              min = 0,
              max = 100,
              label = "Choose number of days with codes and within dates above",
              value = c(16, 100),
              step = 1
              ),
       ),

    ),
    column(3,
           wellPanel(
             actionButton(ns("find_emps"), "Find employees!"),

             selectInput(
               ns("employee"),
               label = "Choose employee",
               # multiple = TRUE,
               choices = NULL
             )
           )
         )

  )

      # numericInput(
      #   ns("max_days"),
      #   label = "Enter maximum number of sick days to show outliers",
      #   value = 16,
      #   step = 1
      # ),
      # sliderInput(
      #   inputId = ns("year"),
      #   label = "Choose year(s)",
      #   step = 1,
      #   sep = "",
      #   min = min_year,
      #   max = max_year,
      #   value = c(min_year, max_year)
      # ),
      ## NB Not so great as no default dates?
      # shinyWidgets::airDatepickerInput(
      #   inputId = ns("date"),
      #   label = "Choose date(s)",
      #   range = TRUE,
      #   view = "years",
      #   width = "800px",
      #   highlightedDates = c(min_year, max_year),
      #   timepicker = TRUE,
      #   clearButton = TRUE,
      #
      #   # startview = "year",
      #   # start = min_year,
      #   # end = max_year,
      #   minDate = min_year,
      #   maxDate = max_year
      # ),

}

#' data Server Functions
#'
#' @noRd
mod_data_server <- function(id, df){
  stopifnot(!is.reactive(df))
  stopifnot(is.data.frame(df))

  moduleServer( id, function(input, output, session){
    ns <- session$ns

## Hierarchical Select: barg -> dep -> crew -> code -> dates -> days -> employee

    .barg <- reactive({
      chosen <- if(input$barg == "All"){
        sort_unique(df$barg)
        } else {input$barg}
      dplyr::filter(df, barg %in% chosen)
      })

    observeEvent(.barg(), {
      choices <- if(input$barg == "All"){
       c("All", sort_unique(.barg()$dept_desc))
      } else {sort_unique(.barg()$dept_desc)}
      updateSelectInput(inputId = "dept_desc",
                        choices = choices)
    })

    .dept <- reactive({
      chosen <- if(input$dept_desc == "All"){
        sort_unique(df$dept_desc)
      } else {input$dept_desc}
      dplyr::filter(.barg(), dept_desc %in% chosen)
    })

    observeEvent(.dept(), {
      choices <- sort_unique(.dept()$crew_desc)
      updateSelectInput(inputId = "crew_desc",
                        choices = choices,
                        selected = choices[1])
    })

    .crew <- reactive({
      chosen <- if(input$crew_desc == "All"){
        sort_unique(df$crew_desc)
      } else {input$crew_desc}
      dplyr::filter(.dept(), crew_desc %in% chosen)
    })

    observeEvent(.crew(), {
      choices <- sort_unique(.crew()$code)
      updateSelectInput(inputId = "code",
                        choices = choices,
                        selected = choices[1])
    })

    .code <- reactive({
      choices <- input$code
      dplyr::filter(.dept(), code %in% choices)
    })

    observeEvent(.code(), {
      max_date <- max(.code()$date)
      min_date <- min(.code()$date)
      updateDateRangeInput(inputId = "date",
                           min = min_date,
                           start = min_date,
                           end = max_date,
                           max = max_date)
    })

    # .days <- reactive({
    #   .code() |>
    #     dplyr::filter(date >= input$date[1] & date <= input$date[2]) |>
    #     sum_days_per_emp() |>
    #     dplyr::filter(days %in% c(input$days_range[1]:input$days_range[2]))
    # })

    .days <- eventReactive(input$find_emps, {
      .code() |>
        dplyr::filter(date >= input$date[1] & date <= input$date[2]) |>
        sum_days_per_emp() |>
        dplyr::filter(days %in% c(input$days_range[1]:input$days_range[2]))
    })

    observeEvent(.days(), {
      choices <- sort_unique(.days()$employee)
      updateSelectInput(inputId = "employee", choices = choices)
    })

    .df_employee <- reactive({
      .code() |>
        dplyr::filter(
          employee %in% input$employee
          & date >= input$date[1] & date <= input$date[2]
        )
    })

    list(
      .df = .df_employee,
      .employee = reactive(input$employee),
      .dates = reactive(input$date),
      .codes = reactive(input$code)
    )
  })
}

#' mod_data_output
#'
#' @noRd
mod_data_output <- function(id) {
  ns <- NS(id)
  reactable::reactableOutput(ns("table"))
}

## To be copied in the UI
# mod_data_ui("data_1")

## To be copied in the server
# mod_data_server("data_1")

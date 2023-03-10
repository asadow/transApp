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
tagList(
  fluidRow(
    shinyjs::useShinyjs(),
    shinyFeedback::useShinyFeedback(),
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
              label = "Choose number of days with above codes and dates",
              value = c(16, 100),
              step = 1
              ),
       ),

    ),
    column(3,
           wellPanel(
             # actionButton(ns("find_emps"), "Find employees"),

             selectInput(
               ns("employee"),
               label = "Choose employee",
               # multiple = TRUE,
               choices = NULL
             ),

             actionButton(ns("see_results"), "See results!"),

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

# Hierarchical select begins ----------------------------------------------
# barg -> dep -> crew -> code -> dates -> days -> employee

## Repetition starts -------------------------------------------------------

    .barg <- reactive(label = "Filter by Bargaining Unit", {
      chosen <- if(input$barg == "All"){
        sort_unique(df$barg)
        } else {input$barg}
      dplyr::filter(df, barg %in% chosen)
      })

    observe(label = "Update Departments by Bargaining Unit", {
      depts <- .barg()$dept_desc
      freezeReactiveValue(input, "dept_desc")
      choices <- if(input$barg == "All"){
       c("All", sort_unique(depts))
      } else {sort_unique(depts)}
      updateSelectInput(inputId = "dept_desc",
                        choices = choices)
    }) |>
      bindEvent(.barg())

## Repetition ends -------------------------------------------------------

    .dept <- reactive(label = "Filter by Department", {
      chosen <- if(input$dept_desc == "All"){
        sort_unique(df$dept_desc)
      } else {input$dept_desc}
      dplyr::filter(.barg(), dept_desc %in% chosen)
    })

    observe(label = "Update Crew by Department", {
      freezeReactiveValue(input, "crew_desc")
      choices <- sort_unique(.dept()$crew_desc)
      updateSelectInput(inputId = "crew_desc",
                        choices = choices,
                        selected = choices[1])
    }) |>
      bindEvent(.dept())

## -------------------------------------------------------------------------

    .crew <- reactive(label = "Filter by Crew", {
      chosen <- if(input$crew_desc == "All"){
        sort_unique(df$crew_desc)
      } else {input$crew_desc}
      dplyr::filter(.dept(), crew_desc %in% chosen)
    })

    observe(label = "Update Codes by Crew in Department", {
      freezeReactiveValue(input, "code")
      choices <- sort_unique(.crew()$code)
      updateSelectInput(inputId = "code",
                        choices = choices,
                        selected = choices[1])
    }) |>
      bindEvent(.crew())

## -------------------------------------------------------------------------
    .code <- reactive(label = "Filter by Code", {
      choices <- input$code
      dplyr::filter(.crew(), code %in% choices)
    })

    observe(label = "Update Dates by Codes", {
      freezeReactiveValue(input, "date")
      max_date <- max(.code()$date)
      min_date <- min(.code()$date)
      updateDateRangeInput(inputId = "date",
                           min = min_date,
                           start = min_date,
                           end = max_date,
                           max = max_date)
    }) |>
      bindEvent(.code())

## -------------------------------------------------------------------------

    .days <- reactive(label = "Calculate Days With Selections", {
      .code() |>
        dplyr::filter(date >= input$date[1] & date <= input$date[2]) |>
        sum_days_per_emp() |>
        dplyr::filter(days %in% c(input$days_range[1]:input$days_range[2]))
    })

    observe(label = "Update Employee by Days", {
      freezeReactiveValue(input, "employee")
      choices <- sort_unique(.days()$employee)
      if(is_empty(choices)){
        ## NB For some reason,
        ## using updateSelectInput("employee", selected = "", choices = "")
        ## removes the warning message
        ## NB Another option is to hide the employee UI and give a message
        shinyFeedback::showFeedbackWarning(
          "employee",
          "Employees do not match the chosen criteria. Please choose new criteria."
        )
        shinyjs::hide("see_results")
      }
      else{
        shinyFeedback::hideFeedback("employee")
        updateSelectInput(inputId = "employee", choices = choices)
        shinyjs::show("see_results")

      }
    }) |>
      bindEvent(.days())

# Hierarchical select ends ----------------------------------------------

    .df_employee <- reactive(label = "Final Filter", {
      records <- nrow(.days()) > 0
      validate(
        need(records, "No results for the selected criteria.")
      )

      .code() |>
        dplyr::filter(
          employee %in% input$employee
          & date >= input$date[1] & date <= input$date[2]
        )
    })

    list(
      .click = reactive(input$see_results),
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

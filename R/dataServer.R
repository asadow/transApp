
dataServer <- function(id, trans){

  moduleServer(id, function(input, output, session) {

    ubu <- stringr::str_sort(trans$bargaining_unit)

    data_bu <- reactive({
      if(input$bargaining_unit %in% ubu){
        trans |> filter(bargaining_unit == input$bargaining_unit)
      } else{
        trans
      }
    })

    observeEvent(data_bu(), {
      freezeReactiveValue(input, "employee")
      employees_in_bu <- unique(data_bu()$employee) |> sort()
      updateSelectInput(
        inputId = "employee",
        choices = employees_in_bu
      )

    })

    data_employee <- reactive({
      is_code_selected <- input$code != ""
      validate(
        need(is_code_selected, "There are no codes selected.")
      )

      data_bu() |>
        filter(
          employee == input$employee
          & year %in% input$year[1]:input$year[2]
          & code %in% input$code
        )

    })

    reactive(data_employee())

  })

}

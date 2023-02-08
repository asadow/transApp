
dataInput <- function(id, trans){
  ubu <-  stringr::str_sort(unique(trans$bargaining_unit))
  ucodes <-  stringr::str_sort(unique(trans$code)) |> na.omit()

  max_year <- max(trans$year) |> as.character() |> as.numeric()
  min_year <- min(trans$year) |> as.character() |> as.numeric()

  tagList(
    sidebarPanel(
      selectInput(
        NS(id, "bargaining_unit"),
        label = "Choose a bargaining unit",
        choices =  c("All", as.character(ubu)),
        selected = "All"
      ),

      selectInput(
        NS(id, "employee"),
        label = "Choose an employee",
        choices = NULL
      ),

      sliderInput(
        inputId = NS(id, "year"),
        label = "Choose years",
        step = 1,
        sep = "",
        min = min_year,
        max = max_year,
        value = c(min_year, max_year)
      ),

      shinyWidgets::pickerInput(
        inputId = NS(id, "code"),
        label = "Choose codes",
        choices = ucodes,
        options = list(`actions-box` = TRUE),
        multiple = T,
        selected = c("SICK", "LAPB")
      )

    )
  )
}

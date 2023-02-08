#' load_trans
#'
#' @description Function to read the employee transaction data on which the app is based
#'
#' @details The sources of the data are..
#'
#' @return a named list of tibbles with...
#' @noRd

load_trans <- function(){
  employee <- pr::employee |> select(- c(bargaining_unit, department))
  sick_codes <- pr::transaction |> filter(code_g == "SICK") |> pull(code_s)
  alt_sick_codes <- sick_codes |> str_subset("SICK", negate = TRUE)

  activity_codes <- c("AFT", "LTD", "LTDP", "G+")

  ## Warning is ok. Means data does not contain NM or NP code
  sick <- pr::emptrans |>
    mutate(
      code = forcats::fct_collapse(code, SICK = !!alt_sick_codes)
    ) |>
    filter(code %in% c("CV", "SICK", "CSICK")) |>
    select(bargaining_unit, department, employee,
           employee_no, code, crew, date, hours) |>
    mutate(
      year = factor(year(date)),
      days = hours/8
    ) |>
    left_join(pr::department, by = "department") |>
    relocate(description, .after = department) |>
    left_join(employee, by = "employee_no") |>
    filter(activity_code %in% activity_codes)
}

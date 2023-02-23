#' Sum the transaction days by employee.
#'
#' @description `sum_days_per_emp()` summarizes
#' the employee transaction data for one set of transaction codes.
#' @param .data A data frame with column `code` containing one unique value.
#' @return A tibble of employee transaction data.

sum_days_per_emp <- function(.df){
  .df |>
    ## There appear to be some duplicate records, hence distinct()
    dplyr::distinct(employee, date, .keep_all = TRUE) |>
    dplyr::group_by(employee) |>
    dplyr::summarize(days = sum(days))
}

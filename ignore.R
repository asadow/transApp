library(janitor)
library(tidyverse)
library(pr)
library(lubridate)
library(reactable)
library(glue)
library(reactable)
library(crosstalk)
library(htmltools)
library(DT)
#
# sick_codes <- pr::transaction |> filter(code_g == "SICK") |> pull(code_s)
# alt_sick_codes <- sick_codes |> str_subset("SICK", negate = TRUE)
# sick <- read_rds("emptrans.rds") |>
#   mutate(
#     code = fct_collapse(code, SICK = !!alt_sick_codes)
#   ) |>
#   filter(code %in% c("CV", "SICK", "CSICK")) |>
#   select(bargaining_unit, department, employee, employee_no,
#          code, crew, date, hours) |>
#   left_join(prdata::employee, by = "employee_no") |>
#   mutate(
#     year = factor(year(date)),
#     days = hours/hours_day
#   )

# sick_n <- sick |>
#   add_count(
#     employee,
#     date,
#     name = "n" # name of count variable
#   )
#
# <!-- ## Data Checks -->
#
#   <!-- ### One entry per date -->
#
#   <!-- There are 2 entries per date for some employees. Since most rows are for full-time hours (e.g. 8), and it is unreasonable to think someone works 16-hour days, I assume that the 2nd entry is not a second shift but is  a duplicate of the first shift. Hence I remove the duplicate.  -->
#
#   ```{r}
# #| label: data-by-day, filter-only-2022
#
sick <- sick |>
  ## See note above for reasoning behind distinct
  distinct(employee, date, .keep_all = TRUE) |>
  filter(year == 2022)  |>
  rename(
    dept. = department,
    barg. = bargaining_unit
  )


## WRITE A TEST OF DAYS = HOURS/8 or 7 depending on employee?!!

gt16 <- sick |>
  group_by(employee, code) |>
  summarize(
    dates = n(),
    hours = sum(hours),
    days = sum(days),
    date = date
  ) |>
  filter(days > 16) |>
  mutate(across(where(is.numeric), ~ round(.x, 1)))

gt16 <- gt16 |> arrange(desc(hours))

sick_gt16 <- sick |>
  select(employee, crew, code, days, hours, barg., dept., date) |>
  filter(employee %in% gt16$employee) |>
  relocate(c(dept., barg.), .after = last_col()) |>
  arrange(employee, crew, code)

summ <- sick_gt16 |>
  group_by(employee, code, crew, dept., barg.) |>
  summarize(
    days = sum(days),
    hours = sum(hours)
  )

summ <- summ |>
  select(employee, crew, code, days, hours, dept., barg.) |>
  rename_with(str_to_title)

# i_num <- which(map_lgl(summ, is.numeric)) - 1
# names(i_num) <- NULL
# i_chr <- which(map_lgl(summ, is.factor)) - 1
# names(i_chr) <- NULL

dt_formatted <- function(.data){

  datatable(
    .data,
    rownames = FALSE,
    extensions = c('Select', 'RowGroup', 'Buttons', 'SearchPanes'),
    options = list(
      rowGroup = list(dataSrc = 0),
      scroller = TRUE,
      dom = 'PBfrtip',
      buttons = c('copy', 'excel', 'pdf', 'print'),
      columnDefs = list(
        # list(className = 'dt-center', targets = i_chr),
        # list(width = '10px', targets = i_num),
        # list(searchPanes = list(show = FALSE), targets = i_num),
        list(searchPanes = list(controls = FALSE), targets = 1:6),
        list(visible = FALSE, targets = 0)
      )
    ),
    selection = 'none'
  )
}

dt_formatted(summ)

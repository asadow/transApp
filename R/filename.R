filename <- function(label, ext, .emp, .codes, .dates){
  .codes <- glue::glue_collapse(.codes, "-")
  .date_range <- glue::glue("{.dates[1]} to {.dates[2]}")
  .filename <- glue::glue("{label}, {.emp}, {.codes}, {.date_range}, {ext}")
}

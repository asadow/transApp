test_that("output updates when reactive input changes", {
  x <- reactiveVal()
  y <- reactiveVal()
  z <- reactiveVal()
  w <- reactiveVal()
  testServer(
    mod_download_server,
    args = list(
      label = "test", .data = x, .emp = y, .date_range = z, .codes = w
    )
    , {
      x(data.frame(x=c(1:3)))
      y("TEST NAME")
      z(c("2050-12-12", "2051-01-01"))
      w(c("SICK", "VAC", "CV"))
      session$flushReact()
      expect_equal(.filename(),
                   "test, TEST NAME, SICK-VAC-CV, 2050-12-12 to 2051-01-01.csv")

      ns <- session$ns
      expect_true(
        inherits(ns, "function")
      )
      expect_true(
        grepl(id, ns(""))
      )
      expect_true(
        grepl("test", ns("test"))
      )
      # - Testing the setting of inputs
      session$setInputs(.emp = "JOE")
      expect_true(input$.emp == "JOE")

      # - If ever your input updates a reactiveValues
      # - Note that this reactiveValues must be passed
      # - to the testServer function via args = list()
      # expect_true(r$x == 1)
      # - Testing output
      expect_s3_class(.data(), "data.frame")
      expect_type(.emp(), "character")
      expect_type(.date_range(), "character")
      expect_type(.codes(), "character")
      # expect_true(inherits(output$downloadData$html, "html"))
  })
})

test_that("module ui works", {
  ui <- mod_download_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_download_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})


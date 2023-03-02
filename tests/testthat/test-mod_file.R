test_that("output updates when reactive input changes", {
  x <- reactiveVal()
  y <- reactiveVal()
  z <- reactiveVal()
  w <- reactiveVal()
  testServer(
    mod_file_server,
    args = list(
      label = "test", .emp = y, .dates = z, .codes = w
    )
    , {
      y("TEST NAME")
      z(c("2050-12-12", "2051-01-01"))
      w(c("SICK", "VAC", "CV"))
      session$flushReact()
      expect_equal(.filename(),
                   "test, TEST NAME, SICK-VAC-CV, 2050-12-12 to 2051-01-01")

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
      expect_type(.emp(), "character")
      expect_type(.dates(), "character")
      expect_type(.codes(), "character")
      # - If ever your input updates a reactiveValues
      # - Note that this reactiveValues must be passed
      # - to the testServer function via args = list()
      # expect_true(r$x == 1)
      # - Testing output
      # expect_true(inherits(output$tbl$html, "html"))
  })
})

test_that("module ui works", {
  ui <- mod_file_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_file_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})


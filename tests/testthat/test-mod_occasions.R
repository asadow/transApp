test_that("output updates when reactive input changes", {
  x <- reactiveVal()
  y <- reactiveVal()
  z <- reactiveVal()
  w <- reactiveVal()
  testServer(
    mod_occasions_server,
    # Add here your module params
    args = list(.df = x, .emp = y, .dates = z, .codes = w)
    , {
      x(data.frame(x = 1))
      y("TEST NAME")
      z(c("2050-12-12", "2051-01-01"))
      w(c("SICK", "VAC", "CV"))
      session$flushReact()
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
      # Here are some examples of tests you can
      # run on your module
      # - Testing the setting of inputs
      # session$setInputs(x = 1)
      # expect_true(input$x == 1)
      # - If ever your input updates a reactiveValues
      # - Note that this reactiveValues must be passed
      # - to the testServer function via args = list()
      # expect_true(r$x == 1)
      # - Testing output
      # expect_true(inherits(output$tbl$html, "html"))
  })
})

test_that("module ui works", {
  ui <- mod_occasions_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_occasions_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})


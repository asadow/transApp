test_that("output updates when reactive input changes", {
  x <- reactiveVal()
  y <- reactiveVal()
  testServer(
    mod_download_server,
    args = list(.df = x, .filename = y)
    , {
      y("TEST NAME")
      x(data.frame(x = 1))
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

      # - If ever your input updates a reactiveValues
      # - Note that this reactiveValues must be passed
      # - to the testServer function via args = list()
      # expect_true(r$x == 1)
      # - Testing output
      expect_s3_class(.df(), "data.frame")
      expect_type(.filename(), "character")
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


testServer(
  mod_data_server,
  # Add here your module params
  args = list()
  , {
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
    # session$setInputs(barg = "CUPE")
    # expect_equal(input$barg, "CUPE")
    # - If ever your input updates a reactiveValues
    # - Note that this reactiveValues must be passed
    # - to the testServer function via args = list()
    # expect_true(r$x == 1)
    # - Testing output
    # expect_true(inherits(output$tbl$html, "html"))
})

test_that("module ui works", {
  ui <- mod_data_ui(id = "test", .data = pr::load_sick())
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_data_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})


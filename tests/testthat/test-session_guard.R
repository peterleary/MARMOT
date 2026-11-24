# The Explorer must never end a session on an error (the grey screen)

guarded_server <- function(input, output, session) {
  observeEvent <- guarded_observeEvent
  observe <- guarded_observe
  rv <- reactiveValues(after = 0, observed = 0, quiet = 0)
  observeEvent(input$boom, { stop("deliberate error") })
  observeEvent(input$boom, { rv$after <- rv$after + 1 })        # other observers still run
  observeEvent(input$wait, { req(FALSE); rv$quiet <- 1 })       # req() stays silent
  observe({ if (isTRUE(input$boom2)) stop("deliberate error in observe()"); rv$observed <- isolate(rv$observed) + 1 })
  observeEvent(input$early, { return(invisible()); rv$quiet <- 99 })   # return() still returns
}

test_that("an error in an observer is caught and the session carries on", {
  shiny::testServer(guarded_server, {
    expect_message(session$setInputs(boom = 1), "Explorer error in input\\$boom: deliberate error")
    expect_identical(rv$after, 1)
    expect_silent(session$setInputs(wait = 1))
    expect_identical(rv$quiet, 0)
    expect_message(session$setInputs(boom2 = TRUE), "deliberate error in observe")
    session$setInputs(early = 1)
    expect_identical(rv$quiet, 0)
    session$setInputs(boom2 = FALSE)
    expect_gt(rv$observed, 0)
    expect_false(session$isClosed())
  })
})

test_that("the Explorer's server uses the guarded observers", {
  app <- readLines(system.file("app", "app.R", package = "MARMOT"))
  expect_true(any(grepl("observeEvent <- guarded_observeEvent", app, fixed = TRUE)))
  expect_true(any(grepl("observe      <- guarded_observe", app, fixed = TRUE)))
})

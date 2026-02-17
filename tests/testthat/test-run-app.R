test_that("run_app errors with a clear message when app directory is missing", {
  testthat::local_mocked_bindings(
    system.file = function(...) "",
    .package = "base"
  )
  
  expect_error(
    DegreeDayCalc:::run_app(),
    regexp = "Cannot find the Shiny app directory"
  )
})

test_that("run_app calls shiny::runApp when app directory exists", {
  
  testthat::local_mocked_bindings(
    system.file = function(...) tempfile("appdir_"),
    dir.exists = function(path) TRUE,
    .package = "base"
  )
  
  called <- FALSE
  
  testthat::local_mocked_bindings(
    runApp = function(appDir, ...) {
      called <<- TRUE
      expect_true(is.character(appDir))
      invisible(NULL)
    },
    .package = "shiny"
  )
  
  expect_silent(DegreeDayCalc:::run_app())
  expect_true(called)
})

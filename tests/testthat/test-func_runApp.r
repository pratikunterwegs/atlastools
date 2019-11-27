context("shiny app")
library(shinytest)
testthat::test_that("shiny app runs",{
  skip_on_cran()
  
  # appdir <- system.file(package="watlasUtils", "shiny_app")
  expect_pass(testApp("apps/patch_vis_app/", compareImages = FALSE))
})


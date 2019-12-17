context("shiny app")
library(shinytest)
shinytest::installDependencies()
testthat::test_that("shiny app runs",{
  #skip_on_cran()
  
  # appdir <- system.file(package="watlasUtils", "shiny_app")
  shinytest::expect_pass(shinytest::testApp("apps/patch_vis_app/", compareImages = FALSE))
})


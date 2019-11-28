app <- shinytest::ShinyDriver$new("../")
app$snapshotInit("test-func_runTweakApp", screenshot=FALSE)
app$snapshot(items = list(output = FALSE, export = FALSE, input = TRUE))


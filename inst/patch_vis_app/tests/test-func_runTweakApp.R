app <- shinytest::ShinyDriver$new("../")
app$snapshotInit("test-func_wat_run_patch_vis_app", screenshot=FALSE)
app$snapshot(items = list(output = FALSE, export = FALSE, input = TRUE))


app <- ShinyDriver$new("../")
app$snapshotInit("test-func_runTweakApp", screenshot=TRUE)

app$listWidgets()
app$snapshot(items = list(output = FALSE, export = FALSE, input = TRUE))


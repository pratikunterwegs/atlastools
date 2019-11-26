app <- ShinyDriver$new("../")
app$snapshotInit("test-func_runTweakApp")

app$uploadFile(revfile = "../../../../tests/testdata/recdata/recurse435_008.csv") # <-- This should be the path to the file, relative to the app's tests/ directory
app$uploadFile(htfile = "../../../../tests/testdata/htdata/435_008.csv") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(go = "click")
app$snapshot()

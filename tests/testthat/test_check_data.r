context("check data for columns\n")
testthat::test_that("data has expected columns", {

    # make some test data
    testdata <- data.table::data.table(x = as.double(1:1e3),
                                     y = as.double(1:1e3),
                                     time = as.numeric(1:1e3))

    # expect no error
    testthat::expect_silent(object = { 
        atlastools:::atl_check_data(data = testdata,
                                   names_expected = c("x", "y", "time"))
    })

    # expect an error
    testthat::expect_error(expr = { 
        atlastools:::atl_check_data(data = testdata,
                                   names_expected = c("X", "Y", "TIME"))
    })
 })
# make test data

resTimeLimit = 4

testrevdata = tibble::tibble(id = 4e3,
	tidalcycle = 1e3,
	x = rep(c(seq(0, 5e3, 1e3),
	          seq(5e3, 0, -1e3)), each = 100) +
	  runif(1200, 0, 2e2),
	y = rep(rep(c(0,1e3), 6), each = 100) +
	  runif(1200, 0, 2e2),
	time = c(seq(1, 1e4, length.out = length(x))),
	resTime = c(rep(resTimeLimit, 550), rep(2, 100),
							rep(resTimeLimit, 550)))

# create ht data
testhtdata = tibble::tibble(id = 4e3,
	tidalcycle = 1e3,
	time = c(seq(1, 1e4, length.out = nrow(testrevdata))),
	tidaltime = seq(1, 12.41*60, length.out = nrow(testrevdata)))

# write to file for testing
readr::write_csv(testrevdata, "../watlasUtils/tests/testdata/test_revdata.csv")
readr::write_csv(testhtdata, "../watlasUtils/tests/testdata/test_htdata.csv")

# ends here

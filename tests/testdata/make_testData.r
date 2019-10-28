# make test data

resTimeLimit = 4

testrevdata = tibble::tibble(id = 4e3,
	tidalcycle = 1e3,
	x = c(rep(0, 100), rep(0, 100), rep(10, 100),
				rep(10, 100), rep(1, 100)),
	y = c(rep(10, 100), rep(0, 100), rep(0, 100),
				rep(10, 100), rep(11, 100)),
	time = c(seq(1, 1e4, length.out = 500)),
	resTime = c(rep(resTimeLimit, 200), rep(2, 100),
							rep(resTimeLimit, 200)))

# create ht data
testhtdata = tibble::tibble(id = 4e3,
	tidalcycle = 1e3,
	x = c(rep(0, 100), rep(0, 100), rep(10, 100),
				rep(10, 100), rep(1, 100)),
	y = c(rep(10, 100), rep(0, 100), rep(0, 100),
				rep(10, 100), rep(11, 100)),
	time = c(seq(1, 1e4, length.out = 500)),
	tidaltime = seq(1, 12.41*60, length.out = 500))

# write to file for testing
readr::write_csv(testrevdata, "../watlasUtils/tests/testdata/test_revdata.csv")
readr::write_csv(testhtdata, "../watlasUtils/tests/testdata/test_htdata.csv")

# ends here

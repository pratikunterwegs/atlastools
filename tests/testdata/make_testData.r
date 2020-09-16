# make test data

resTimeLimit <- 4

testrevdata <- data.table::CJ(id = 4e3,
	tide_number = 1e3,
	x = rep(c(seq(0, 5e3, 1e3),
	          seq(5e3, 0, -1e3)), each = 100) +
	  runif(1200, 0, 2e1),
	y = rep(rep(c(0, 1e3), 6), each = 100) +
	  runif(1200, 0, 2e1),
	time = c(seq(1, 1e4, length.out = length(x))),
	resTime = c(rep(resTimeLimit, 550), rep(2, 100),
							rep(resTimeLimit, 550)),
	tidaltime = seq(1, 12.41*60, length.out = length(resTime)),
	waterlevel = cospi(pi * seq(1, 12.41 * 60,
	                          length.out = length(tidaltime)) /
	                     (11 * 60 * 2)) * 100)

# write to file for testing
data.table::fwrite(testrevdata, "tests/testdata/test_revdata.csv")

# ends here

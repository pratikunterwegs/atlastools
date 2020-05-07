#' Classify points as residence points based on residence time.
#'
#' @param somedata A data frame which must have a column for the residence time at each point.
#' @param resTimeLimit A numeric giving the time limit in minutes against which residence time is compared.
#'
#' @return A data.frame extension object, which retains only points classified as residence points if residence time is below \code{resTimeLimit} over \code{travelSeg} points.
#' @export
#'

wat_classify_points <- function(somedata,
                             resTimeLimit = 2) {
  # check somedata is a data.frame and has a resTime column
  {
    # check if data frame
    assertthat::assert_that(is.data.frame(somedata),
                            msg = glue::glue('wat_classify_points: input not a dataframe object, has class {stringr::str_flatten(class(somedata), collapse = " ")}!'))

    assertthat::assert_that("resTime" %in% names(somedata),
                            msg = "wat_classify_points: data has no residence time column")
    assertthat::assert_that(min(c(resTimeLimit)) > 1,
                            msg = "wat_classify_points: function arguments are not positive")
  }

  # make datatable to use functions
  if (!data.table::is.data.table(somedata)) {
    data.table::setDT(somedata)
  }

  # handle global variable issues
  resTime <- resTimeBool <- rollResTime <- NULL
  time <- timediff <- type <- x <- y <- npoints <- NULL

  # sort by time
  data.table::setorder(somedata, time)

  # check this has worked
  {
    assertthat::assert_that(min(diff(somedata$time)) >= 0,
                            msg = "data for segmentation is not ordered by time")
  }

  # prep to assign sequence to res patches
  # to each id.tide combination
  # remove NA vals in resTime
  somedata <- somedata[!is.na(resTime),]

  # drop NAs in rolling residence time evaluation
  # essentially the first and last elements will be dropped
  somedata <- somedata[resTime >= resTimeLimit, ]

  # print message if dataframe has few rows
  {
    if (nrow(somedata) < 5) {
      message(glue::glue('\n\n...segmented dataframe has < 5 rows\n\n'))
    }
  }

  return(somedata)

}

# ends here

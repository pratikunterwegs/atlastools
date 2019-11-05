#' classifyPath
#'
#' @param somedata A data frame which must have a column for the residence time at each point.
#' @param resTimeLimit A numeric giving the time limit in minutes against which residence time is compared.
#' @param restimeCol The residence time column name.
#' @param travelSeg A numeric value of the number of fixes, or rows, over which a smoother function is applied.
#' @return A data.frame extension object, which retains only points classified as residence points if residence time is below \code{resTimeLimit} over \code{travelSeg} points.
#' @import data.table
#' @export
#'

funcClassifyPath <- function(somedata,
                             restimeCol = "resTime",
                             resTimeLimit = 2,
                             travelSeg = 5){

  # check somedata is a data.frame and has a resTime column
  {
    assertthat::assert_that("data.frame" %in% class(somedata),
                            msg = "not a dataframe object!")

    assertthat::assert_that(restimeCol %in% names(somedata),
                            msg = "data has no residence time column")
  }

  # make datatable to use functions
  if(is.data.table(somedata) != TRUE) {setDT(somedata)}

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
  # set residence time to 0 or 1 predicated on <= limit in func args
  somedata <- somedata[!is.na(restimeCol)]
  somedata[,resTimeBool:= ifelse(restimeCol < resTimeLimit, F, T)]

  # get breakpoints if the mean over rows of length travelSeg
  # is below 0.5
  # how does this work?
  # a sequence of comparisons, resTime <= resTimeLimit
  # may be thus: c(T,T,T,F,F,T,T)
  # indicating two non-residence points between 3 and 2 residence points
  # the rolling mean over a window of length 3 will be
  # c(1,.67,.33,.33,.33,.67) which can be used to
  # smooth over false negatives of residence
  somedata[,rollResTime:=(zoo::rollmean(resTimeBool, k = travelSeg, fill = NA) > 0.5)]

  # drop NAs in rolling residence time evaluation
  # essentially the first and last elements will be dropped
  somedata <- somedata[!is.na(rollResTime) & resTimeBool == T, ]

  # remove useless somedata columns
  data.table::set(somedata, ,c("rollResTime", "resTimeBool"), NULL)

  # print message if dataframe has few rows
  {
    if(nrow(somedata) < 5){
      print(glue::glue('\n\n...segmented dataframe has < 5 rows\n\n'))
    }
  }

  return(somedata)

}

# ends here

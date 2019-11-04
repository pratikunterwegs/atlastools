#' segPath2
#'
#' @param revdata A string filepath to data in csv format. The data must be the output of recurse analysis, or must include, in addition to X, Y and time columns, a residence time column named resTime, id, and tidalcycle.
#' @param resTimeLimit A numeric giving the time limit in minutes against which residence time is compared.
#' @param travelSeg A numeric value of the number of fixes, or rows, over which a smoother function is applied.
#' @param inferPatches A logical indicating whether residence patches should be inferred from temporal gaps in the data.
#' @param infPatchTimeDiff A numeric duration in seconds, of the minimum time difference between two points, above which, it is assumed worthwhile to examine whether there is a missing residence patch to be inferred.
#' @param infPatchSpatDiff A numeric distance in metres, of the maximum spatial distance between two points, below which it may be assumed few extreme movements took place between them.
#' @param htdata A string filepath to data in csv format. The data must be the output of tidal cycle finding analysis, or must include, in addition to X, Y and time columns, a tidaltime column named tidaltime; also id, and tidalcycle for merging.
#' @param bufferSize A numeric value, in meteres, of the size of the buffer to be constructed around points.
#' @param resPatchTimeDiff A numeric value, in seconds, of the time between two patches for them to be considered independent.
#' @param resPatchSpatDiff Anumeric value, in metres, of the distance between two patches for them to be considered independent.
#'
#' @return A data.frame extension object. This dataframe has the added column \code{resPatch} based on cumulative patch summing. Depending on whether \code{inferPatches = TRUE}, the dataframe has additional inferred points. An additional column is created in each case, indicating whether the data are empirical fixes ('real') or 'inferred'.
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
                            "data has no residence time column")

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

  # assign spat and temp diff columns
  somedata <- somedata[,`:=`(spatdiff = watlasUtils::funcDistance(somedata = somedata, x = "x", y = "y"),
                             tempdiff = as.numeric(c(NA, diff(time))))]

  # first spatial difference is infinity for calculation purposes
  somedata[1,c("spatdiff", "tempdiff")] <- Inf

  # merge points if not spatially or temporally independent
  # compare distance from previous point to buffersize
  somedata <- somedata[,patch := cumsum(spatdiff > (2*resPatchSpatDiff) &
                                          tempdiff > resPatchTimeDiff ) ]

  # count number of points per patch
  somedata <- somedata[,nfixes := .N, by = c("id", "tidalcycle", "patch")]

  # remove patches with 5 or fewer points
  somedata[nfixes > 5, ]

  #

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

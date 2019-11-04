#' classifyPath
#' @param somedata A dataframe of values of any class that is or extends data.frame. The dataframe must contain at least two spatial coordinates, \code{x} and \code{y}, and a temporal coordinate, \code{time}. The names of columns specifying these can be passed as arguments below.
#' @param bufferSize A numeric value specifying the radius of the buffer to be considered around each coordinate point. May be thought of as the distance that an individual can access, assess, or otherwise cover when at a discrete point in space.
#' @param spatIndepLim A numeric value of time in seconds of the time difference between two patches for them to be considered independent.
#' @param tempIndepLim A numeric value of distance in metres of the spatial distance between two patches for them to the considered independent.
#' @return A data.frame extension object. This dataframe has the added column \code{resPatch} based on cumulative patch summing. Depending on whether \code{inferPatches = TRUE}, the dataframe has additional inferred points. An additional column is created in each case, indicating whether the data are empirical fixes ('real') or 'inferred'.
#' @import data.table
#' @export
#'

funcClassifyPath <- function(somedata,
                             bufferSize = 10,
                             spatIndepLim = 50,
                             tempIndepLim = 3600){

  # check somedata is a data.frame and has a resTime column
  {
    assertthat::assert_that("data.frame" %in% class(somedata),
                            msg = "not a dataframe object!")
  }

  # get names and numeric variables
  dfnames <- names(somedata)
  namesReq <- c("id", "tidalcycle", "x", "y", "time", "type")

  # include asserts checking for required columns
  {
    for (i in 1:length(namesReq)) {
      assertthat::assert_that(namesReq[i] %in% dfnames,
                              msg = glue::glue('{namesReq[i]} is required but missing from data!'))
    }
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

  # identify spatial overlap
  {
    # assign spat diff columns
    somedata[,`:=`(spatdiff = watlasUtils::funcDistance(somedata = somedata,
                                                        x = "x", y = "y"))]

    # first spatial difference is infinity for calculation purposes
    somedata[1,c("spatdiff")] <- Inf

    # merge points if not spatially independent
    # compare distance from previous point to buffersize
    somedata <- somedata[,patch := cumsum(spatdiff > (2*resPatchSpatDiff))]
  }

  # count fixes and patch and remove small patches
  {
    # count number of points per patch
    somedata <- somedata[,nfixes := .N, by = c("id", "tidalcycle", "patch")]

    # remove patches with 5 or fewer points
    somedata[nfixes > 5, ]
  }

  # get time mean and extreme points for spatio-temporal independence calc
  {
    patchSummary <- somedata; setDF(patchSummary)

    patchSummary <- dplyr::group_by(patchSummary, id, tidalcycle, patch, type)
    # point count
    patchCount <- dplyr::count(patchSummary, name = "nfixes")
    # summarise mean, first and last
    patchSummary <- dplyr::summarise_at(.tbl = patchSummary,
                                        .vars = dplyr::vars(time, x, y, tidaltime),
                                        .funs = list(start = dplyr::first,
                                                     end = dplyr::last,
                                                     mean = mean))
    # join patch count
    patchSummary <- dplyr::left_join(patchSummary, patchCount,
                                     by = names(patchCount))

    patchSummary <- dplyr::ungroup(patchSummary)

    patchSummary <- dplyr::mutate(patchSummary,
                                  timediff = c(Inf,
                                               as.numeric(diff(time_mean))))
    # get spatial difference from last to first point
    spatdiff <- watlasUtils::funcBwPatchDist(df = patchSummary,
                                            x1 = "x_end", x2 = "x_start",
                                            y1 = "y_end", y2 = "y_start")
    # set spatdiff 1 to Inf
    spatdiff[1] <- Inf
    patchSummary <- dplyr::mutate(patchSummary, spatdiff = spatdiff)

    # assess independence
    patchSummary <- dplyr::mutate(patchSummary,
                                  patch = cumsum(timediff > tempIndepLim |
                                                   spatdiff > spatIndepLim))
  }

  # basic patch metrics for new patches
  {
    patchSummary <- dplyr::group_by(patchSummary, id, tidalcycle, patch)
    patchSummary <- dplyr::summarise(patchSummary,
                                     nfixes = sum(nfixes),
                                     time_mean = mean(time_mean),
                                     time_start = min(time_start),
                                     time_end = max(time_end),
                                     tidaltime_mean = mean(tidaltime_mean),
                                     tidaltime_start = min(tidaltime_start),
                                     tidaltime_end = max(tidaltime_end),
                                     x_mean = mean(x_mean),
                                     x_start = min(x_start),
                                     x_end = max(x_end),
                                     y_mean = mean(y_mean),
                                     y_start = min(y_start),
                                     y_end = max(y_end),
                                     type = dplyr::case_when(
                                       sum(c("inferred","real") %in% type) == 2 ~ "mixed",
                                       length(unique(type)) == 1 ~ unique(type),
                                       TRUE ~ as.character(NA)
                                     ))

  }

  # advanced patch metrics
  {
    patchSummary <- dplyr::ungroup(patchSummary)
    # duration in seconds
    patchSummary <- dplyr::mutate(patchSummary,
                                  duration = (time_end - time_start),
                                  propfixes = nfixes/(duration/3)

    # NEEDS WITHIN AND BETWEEN PATCH DISTANCE AND AREA

                                  )
  }

  return(somedata)

}

# ends here

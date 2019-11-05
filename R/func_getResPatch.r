#' getResPatch
#'
#' @param somedata A dataframe of values of any class that is or extends data.frame. The dataframe must contain at least two spatial coordinates, \code{x} and \code{y}, and a temporal coordinate, \code{time}. The names of columns specifying these can be passed as arguments below.
#' @param bufferSize A numeric value specifying the radius of the buffer to be considered around each coordinate point. May be thought of as the distance that an individual can access, assess, or otherwise cover when at a discrete point in space.
#' @param spatIndepLim A numeric value of time in seconds of the time difference between two patches for them to be considered independent.
#' @param makeSf Whether to return an sf object rather than simply a dataframe, the geometry being the patch outlines.
#' @param tempIndepLim A numeric value of distance in metres of the spatial distance between two patches for them to the considered independent.
#' @return A data.frame extension object. This dataframe has the added column \code{resPatch} based on cumulative patch summing. Depending on whether \code{inferPatches = TRUE}, the dataframe has additional inferred points. An additional column is created in each case, indicating whether the data are empirical fixes ('real') or 'inferred'.
#' @import data.table
#' @export
#'

funcGetResPatch <- function(somedata,
                            bufferSize = 10,
                            spatIndepLim = 50,
                            tempIndepLim = 1800,
                            makeSf = FALSE){

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

  tryCatch(
    {
      # identify spatial overlap
      {
        # assign spat diff columns
        somedata[,`:=`(spatdiff = watlasUtils::funcDistance(df = somedata,
                                                            x = "x", y = "y"))]

        # first spatial difference is infinity for calculation purposes
        somedata[1,c("spatdiff")] <- Inf

        # merge points if not spatially independent
        # compare distance from previous point to buffersize
        somedata <- somedata[,patch := cumsum(spatdiff > (2*bufferSize))]
      }

      # count fixes and patch and remove small patches
      {
        # count number of points per patch
        somedata <- somedata[,nfixes := .N, by = c("id", "tidalcycle", "patch")]

        # remove patches with 5 or fewer points
        somedata <- somedata[nfixes > 5, ]
      }

      # get time mean and extreme points for spatio-temporal independence calc
      {
        setDF(somedata)

        somedata <- dplyr::group_by(somedata, id, tidalcycle, patch, type)
        # nest data to keep for some operations
        somedata <- tidyr::nest(somedata)
        somedata <- dplyr::mutate(somedata,
                                  nfixes = purrr::map_int(data, nrow))
        # summarise mean, first and last
        somedata <- dplyr::mutate(somedata,
                                  patchSummary = purrr::map(data, function(df){
                                    dplyr::summarise_at(.tbl = df,
                                                        .vars = dplyr::vars(time, x, y, tidaltime),
                                                        .funs = list(start = dplyr::first,
                                                                     end = dplyr::last,
                                                                     mean = mean))
                                  }))
        # unnest patch summary
        somedata <- tidyr::unnest(data = somedata, cols = patchSummary)
        # ungroup to prevent within group calcs
        somedata <- dplyr::ungroup(somedata)

        somedata <- dplyr::mutate(somedata,
                                  timediff = c(Inf,
                                               as.numeric(diff(time_mean))))
        # get spatial difference from last to first point
        spatdiff <- watlasUtils::funcBwPatchDist(df = somedata,
                                                 x1 = "x_end", x2 = "x_start",
                                                 y1 = "y_end", y2 = "y_start")
        # set spatdiff 1 to Inf
        spatdiff[1] <- Inf
        somedata <- dplyr::mutate(somedata, spatdiff = spatdiff)
        rm(spatdiff)

        # assess independence
        somedata <- dplyr::mutate(somedata,
                                  patch = cumsum(timediff > tempIndepLim |
                                                   spatdiff > spatIndepLim))
      }

      # basic patch metrics for new patches
      {
        somedata <- dplyr::group_by(somedata, id, tidalcycle, patch, type)
        # select main data
        somedata <- dplyr::select(somedata,
                                  id, tidalcycle, patch, type, data) # might have issues
        # unnest
        somedata <- tidyr::unnest(somedata, cols = data)
        # summarise data by new patch, ungrouping type
        somedata <- dplyr::ungroup(somedata, type)
        somedata <- dplyr::group_by(somedata, id, tidalcycle, patch)
        somedata <- tidyr::nest(somedata)
        # basic metrics by new patch
        somedata <- dplyr::mutate(somedata,
                                  patchSummary = purrr::map(data, function(df){
                                    dplyr::summarise_at(.tbl = df,
                                                        .vars = dplyr::vars(time, x, y, tidaltime),
                                                        .funs = list(start = dplyr::first,
                                                                     end = dplyr::last,
                                                                     mean = mean))
                                  }))
      }
      # advanced metrics on ungrouped data
      {
        somedata <- dplyr::ungroup(somedata)
        # distance in a patch
        somedata <- dplyr::mutate(somedata,
                                  distInPatch = purrr::map_dbl(data, function(df){
                                    sum(watlasUtils::funcDistance(df = df), na.rm = TRUE)
                                  }))
        # distance between patches
        somedata <- tidyr::unnest(somedata, cols = patchSummary)
        somedata <- dplyr::mutate(somedata,
                                  distBwPatch = watlasUtils::funcBwPatchDist(df = somedata,
                                                                             x1 = "x_end", x2 = "x_start",
                                                                             y1 = "y_end", y2 = "y_start"))
        # type of patch
        somedata <- dplyr::mutate(somedata,
                                  type = purrr::map_chr(data, function(df){
                                    a <- ifelse(sum(c("real", "inferred") %in% df$type) == 2,
                                                "mixed", first(df$type))
                                    return(a)
                                    }))

      }
      # even more advanced metrics
      {
        somedata <- dplyr::mutate(somedata,
                                  nfixes = purrr::map_int(data, nrow),
                                  duration = (time_end - time_start),
                                  propfixes = nfixes/(duration/3))
      }

      # true spatial metrics
      {
        somedata <- dplyr::mutate(somedata, polygons = purrr::map(data, function(df){
          p1 <- sf::st_as_sf(df, coords = c("x", "y"))
          p2 <- sf::st_buffer(p1, dist = bufferSize)
          p2 <- sf::st_union(p2)
          return(p2)
        }))

        somedata <- dplyr::mutate(somedata,
                                  area = purrr::map_dbl(polygons, sf::st_area))
      }

      if(makeSf == TRUE){
        polygons <- purrr::reduce(somedata$polygons, c)
        somedata$polygons <- polygons
        somedata <- sf::st_as_sf(somedata, sf_column_name = "polygons")
      }

      print(glue::glue('residence patches of {unique(somedata$id)} in tide {unique(somedata$tidalcycle)} constructed'))

      return(somedata)
    },
    # null error function, with option to collect data on errors
    error= function(e)
    {
      print(glue::glue('\nthere was an error in id_tide combination...
                                  {unique(somedata$id)} {unique(somedata$tidalcycle)}\n'))
      # dfErrors <- append(dfErrors, glue(z$id, "_", z$tidalCycle))
    }
  )

}

# ends here

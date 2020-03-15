#' getResPatch
#'
#' @param somedata A dataframe of values of any class that is or extends data.frame. The dataframe must contain at least two spatial coordinates, \code{x} and \code{y}, and a temporal coordinate, \code{time}. The names of columns specifying these can be passed as arguments below.
#' @param bufferSize A numeric value specifying the radius of the buffer to be considered around each coordinate point. May be thought of as the distance that an individual can access, assess, or otherwise cover when at a discrete point in space.
#' @param spatIndepLim A numeric value of time in minutes of the time difference between two patches for them to be considered independent.
#' @param restIndepLim A numeric value of time in minutes of the difference in residence times between two patches for them to be considered independent.
#' @param tempIndepLim A numeric value of distance in metres of the spatial distance between two patches for them to the considered independent.
#' @param minFixes The minimum number of fixes for a group of spatially-proximate number of ponts to be considered a preliminary residence patch.
#'
#' @return A data.frame extension object. This dataframe has the added column \code{resPatch} based on cumulative patch summing. Depending on whether \code{inferPatches = TRUE}, the dataframe has additional inferred points. An additional column is created in each case, indicating whether the data are empirical fixes ('real') or 'inferred'.
#' @import data.table
#' @export
#'

wat_make_res_patch <- function(somedata,
                            bufferSize = 10,
                            spatIndepLim = 100,
                            tempIndepLim = 30,
                            restIndepLim = 30,
                            minFixes = 3){
  # handle global variable issues
  time <- timediff <- type <- x <- y <- npoints <- NULL
  patch <- nfixes <- id <- tide_number <- patchdata <- tidaltime <- NULL
  patchSummary <- time_start <- time_end <- duration <- nfixes <- NULL
  resTime <- resTime_mean <- resTimeDiff <- area <- NULL
  x_end <- y_end <- x_start <- y_start <- tidaltime_mean <- NULL
  spatdiff <- newpatch <- distInPatch <- distBwPatch <- dispInPatch <- NULL
  waterlevel <- NULL
  # check somedata is a data.frame and has a resTime column
  {
    # check if data frame
    assertthat::assert_that(is.data.frame(somedata),
                            msg = glue::glue('getResPatch: input not a dataframe object, has class {stringr::str_flatten(class(somedata), collapse = " ")}!'))

    assertthat::assert_that(min(as.numeric(diff(somedata$time))) >= 0,
                            msg = "wat_make_res_patch: not ordered in time!")

    assertthat::assert_that(min(c(bufferSize, spatIndepLim, tempIndepLim, minFixes)) > 0,
                            msg = "wat_make_res_patch: function needs positive arguments")

  }

  # convert variable units
  {
    tempIndepLim = tempIndepLim*60
  }

  # get names and numeric variables
  dfnames <- names(somedata)
  namesReq <- c("id", "tide_number", "x", "y", "time", "type", "resTime", "tidaltime")

  # include asserts checking for required columns
  {
    for (i in 1:length(namesReq)) {
      assertthat::assert_that(namesReq[i] %in% dfnames,
                              msg = glue::glue('{namesReq[i]} is required but missing from data!'))
    }
  }

  # make datatable to use functions
  if(is.data.table(somedata) != TRUE) {setDT(somedata)}
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
        somedata[,`:=`(spatdiff = watlastools::wat_simple_dist(df = somedata,
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
        somedata <- somedata[,nfixes := .N, by = c("id", "tide_number", "patch")]

        # remove patches with 2 or fewer points
        somedata <- somedata[nfixes >= minFixes, ]
      }

      # get time mean and extreme points for spatio-temporal independence calc
      {
        setDF(somedata)

        somedata <- dplyr::group_by(somedata, id, tide_number, patch, type)
        # nest data to keep for some operations
        somedata <- tidyr::nest(somedata)
        somedata <- dplyr::mutate(somedata,
                                  nfixes = purrr::map_int(data, nrow))
        # summarise mean, first and last
        somedata <- dplyr::mutate(somedata,
                                  patchSummary = purrr::map(data, function(df){
                                    dplyr::summarise_at(.tbl = df,
                                                        .vars = dplyr::vars(time, x, y, resTime),
                                                        .funs = list(start = dplyr::first,
                                                                     end = dplyr::last,
                                                                     mean = mean))
                                  }))
        # unnest patch summary
        somedata <- tidyr::unnest(data = somedata, cols = patchSummary)
        # ungroup to prevent within group calcs
        somedata <- dplyr::ungroup(somedata)

        # arrange by time
        somedata <- dplyr::arrange(somedata, time_start)

        # get time bewteen start of n+1 and end of n
        somedata <- dplyr::mutate(somedata,
                                  timediff = c(Inf,
                                               as.numeric(time_start[2:length(time_start)] -
                                                            time_end[1:length(time_end)-1])))
        # get spatial difference from last to first point
        spatdiff <- watlastools::wat_bw_patch_dist(df = somedata,
                                                 x1 = "x_end", x2 = "x_start",
                                                 y1 = "y_end", y2 = "y_start")
        # set spatdiff 1 to Inf
        spatdiff[1] <- Inf
        somedata <- dplyr::mutate(somedata, spatdiff = spatdiff)
        rm(spatdiff)

        # get differences in mean residence time
        somedata <- dplyr::mutate(somedata,
                                  resTimeDiff = c(Inf, abs(as.numeric(diff(resTime_mean)))))

        # assess independence
        somedata <- dplyr::mutate(somedata,
                                  patch = cumsum((timediff > tempIndepLim) |
                                                   (spatdiff > spatIndepLim) |
                                                   resTimeDiff > restIndepLim))
      }

      # basic patch metrics for new patches
      {
        somedata <- dplyr::group_by(somedata, id, tide_number, patch, type)
        # select main data
        somedata <- dplyr::select(somedata,
                                  id, tide_number, patch, type, data) # might have issues
        # unnest
        somedata <- tidyr::unnest(somedata, cols = data)
        # summarise data by new patch, ungrouping type
        somedata <- dplyr::ungroup(somedata, type)
        somedata <- dplyr::group_by(somedata, id, tide_number, patch)
        somedata <- tidyr::nest(somedata)
        # basic metrics by new patch
        somedata <- dplyr::mutate(somedata,
                                  patchSummary = purrr::map(data, function(df){
                                    dplyr::summarise_at(.tbl = df,
                                                        .vars = dplyr::vars(time, x, y, tidaltime, resTime, waterlevel),
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
                                    sum(watlastools::wat_simple_dist(df = df), na.rm = TRUE)
                                  }))

        # distance between patches
        somedata <- tidyr::unnest(somedata, cols = patchSummary)
        somedata <- dplyr::mutate(somedata,
                                  distBwPatch = watlastools::wat_bw_patch_dist(df = somedata,
                                                                             x1 = "x_end", x2 = "x_start",
                                                                             y1 = "y_end", y2 = "y_start"))
        # displacement in a patch
        # apply func bw patch dist reversing usual end and begin
        somedata <- dplyr::mutate(somedata,
                                  dispInPatch = sqrt((x_end - x_start)^2 + (y_end - y_start)^2))
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

        # add area and circularity
        somedata <- dplyr::mutate(somedata,
                                  area = purrr::map_dbl(polygons, sf::st_area))
        somedata <- dplyr::mutate(somedata,
                                  circularity = (4*pi*area)/purrr::map_dbl(polygons, function(pgon){
                                    boundary <- sf::st_boundary(pgon)
                                    perimeter <- sf::st_length(boundary)
                                    return(as.numeric(perimeter)^2)
                                  }))
      }
      # remove old nfixes and type
      {
        somedata$data <- purrr::map(somedata$data, function(df){
          df <- dplyr::select(df, -nfixes, -type)
        })
      }
      # filter for low tide
      {
        somedata <- dplyr::filter(somedata, tidaltime_mean %between% tideLims)
        # rename patches
        somedata <- dplyr::mutate(somedata, patch = 1:nrow(somedata))
      }

      # make spatial polygons
      {
        polygons <- purrr::reduce(somedata$polygons, c)
        somedata$polygons <- polygons
        somedata <- sf::st_as_sf(somedata, sf_column_name = "polygons")
      }
    },
    # null error function, with option to collect data on errors
    error= function(e)
    {
      message(glue::glue('\nthere was an error in id_tide combination...
                                  {unique(somedata$id)} {unique(somedata$tide_number)}\n'))
      # dfErrors <- append(dfErrors, glue(z$id, "_", z$tidalCycle))
    }
  )

}

# ends here

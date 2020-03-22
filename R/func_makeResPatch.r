#' Construct residence patches from classified residence data.
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
  waterlevel <- polygons <- NULL
  # check somedata is a data.frame and has a resTime column
  {
    # check if data frame
    assertthat::assert_that(is.data.frame(somedata),
          msg = glue::glue('getResPatch: input not a dataframe object,
          has class {stringr::str_flatten(class(somedata), collapse = " ")}!'))

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
        somedata <- somedata[nfixes >= minFixes | type == "inferred", ]
        somedata[,nfixes:=NULL]
      }

      # get time mean and extreme points for spatio-temporal independence calc
      {
        # nest data
        somedata <- somedata[, .(list(.SD)), by = .(id, tide_number, patch)]
        setnames(somedata, old = "V1", new = "patchdata")
        somedata[,nfixes:=purrr::map_int(patchdata, nrow)]

        # summarise mean, first and last
        somedata[,patchSummary:= lapply(patchdata, function(dt){
          dt <- dt[,.(time, x, y, resTime)]
          dt <- setDF(dt)
          dt <- dplyr::summarise_at(.tbl = dt,
                                    .vars = dplyr::vars(time, x, y, resTime),
                                    .funs = list(start = dplyr::first,
                                                 end = dplyr::last,
                                                 mean = mean))
          return(setDT(dt))
        })]

        # assess independence using summary data
        {
          patchsummary <- somedata[,unlist(patchSummary, recursive = FALSE),
                                   by = .(id, tide_number, patch)]
          somedata[,patchSummary:=NULL]
          # get time bewteen start of n+1 and end of n
          patchsummary[,timediff := c(Inf,
                                      as.numeric(time_start[2:length(time_start)] -
                                                   time_end[1:length(time_end)-1]))]
          # get spatial difference from last to first point
          patchsummary[,spatdiff := c(watlastools::wat_bw_patch_dist(df = patchsummary,
                                                                     x1 = "x_end", x2 = "x_start",
                                                                     y1 = "y_end", y2 = "y_start"))]
          # set spatdiff 1 to Inf
          patchsummary[1,"spatdiff"] <- Inf

          # get differences in mean residence time
          patchsummary[,resTimeDiff := c(Inf, abs(as.numeric(diff(resTime_mean))))]

          # assess independence and assign new patch
          patchsummary[,newpatch := cumsum((timediff > tempIndepLim) |
                                             (spatdiff > spatIndepLim) |
                                             resTimeDiff > restIndepLim)]
        }
          # get cols with old and new patch
          patchsummary <- patchsummary[,.(patch, newpatch)]
      }

      # basic patch metrics for new patches
      {
        # join patchdata to patch summary by new patch
        # expand data to prepare for new patches
        somedata <- somedata[, unlist(patchdata, recursive = FALSE),
                 by = .(id, tide_number, patch)]

        somedata <- merge(somedata, patchsummary, by = "patch")
        somedata[,`:=`(patch=newpatch, newpatch=NULL)]

        # nest data again
        somedata <- somedata[, .(list(.SD)), by = .(id, tide_number, patch)]
        setnames(somedata, old = "V1", new = "patchdata")
        somedata[,nfixes:=lapply(patchdata, nrow)]

        # basic metrics by new patch
        somedata[,patchSummary:= lapply(patchdata, function(dt){
          dt <- dt[,.(time, x, y, resTime, tidaltime, waterlevel)]
          dt <- setDF(dt)
          dt <- dplyr::summarise_all(.tbl = dt,
                                    .funs = list(start = dplyr::first,
                                                 end = dplyr::last,
                                                 mean = mean))
          return(setDT(dt))
        })]
      }

      # advanced metrics on ungrouped data
      {
        # distance in a patch in metres
        somedata[,distInPatch := purrr::map_dbl(patchdata, function(df){
                                    sum(watlastools::wat_simple_dist(df = df), na.rm = TRUE)
                                  })]

        # distance between patches
        tempdata <- somedata[,unlist(patchSummary, recursive = FALSE),
                                 by = .(id, tide_number, patch)]
        somedata[,patchSummary:=NULL]
        somedata[,distBwPatch := watlastools::wat_bw_patch_dist(df = tempdata,
                                                                x1 = "x_end", x2 = "x_start",
                                                                y1 = "y_end", y2 = "y_start")]
        # displacement in a patch
        # apply func bw patch dist reversing usual end and begin
        tempdata[,dispInPatch := sqrt((x_end - x_start)^2 + (y_end - y_start)^2)]
        # type of patch
        somedata[, type := purrr::map_chr(patchdata, function(df){
                                    a <- ifelse(sum(c("real", "inferred") %in% df$type) == 2,
                                                "mixed", first(df$type))
                                    return(a)
                                    })]

      }

      # even more advanced metrics
      {
        tempdata[, duration := (time_end - time_start)]
      }

      # true spatial metrics
      {
        somedata[, polygons := lapply(patchdata, function(df){
          p1 <- sf::st_as_sf(df, coords = c("x", "y"))
          p2 <- sf::st_buffer(p1, dist = bufferSize)
          p2 <- sf::st_union(p2)
          return(p2)
        })]

        # add area and circularity
        somedata[,area := purrr::map_dbl(polygons, sf::st_area)]
        somedata[,`:=`(circularity = (4*pi*area)/purrr::map_dbl(polygons, function(pgon){
          boundary <- sf::st_boundary(pgon)
          perimeter <- sf::st_length(boundary)
          return(as.numeric(perimeter)^2)
        }))]

        # remove polygons
        somedata[,polygons := NULL]
      }

      # remove patch summary from some data and add temp data, then del tempdata
      somedata <- merge(somedata, tempdata, by = c("id", "tide_number", "patch"))

      return(somedata)
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

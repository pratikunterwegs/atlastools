#' Construct residence patches from classified residence data.
#'
#' @param data A dataframe of values of any class that is or extends data.frame. The dataframe must contain at least two spatial coordinates, \code{x} and \code{y}, and a temporal coordinate, \code{time}. The names of columns specifying these can be passed as arguments below.
#' @param buffer_radius A numeric value specifying the radius of the buffer to be considered around each coordinate point. May be thought of as the distance that an individual can access, assess, or otherwise cover when at a discrete point in space.
#' @param lim_spat_indep A numeric value of time in minutes of the time difference between two patches for them to be considered independent.
#' @param lim_rest_indep A numeric value of time in minutes of the difference in residence times between two patches for them to be considered independent.
#' @param lim_time_indep A numeric value of distance in metres of the spatial distance between two patches for them to the considered independent.
#' @param min_fixes The minimum number of fixes for a group of spatially-proximate number of ponts to be considered a preliminary residence patch.
#'
#' @return A data.frame extension object. This dataframe has the added column \code{resPatch} based on cumulative patch summing. Depending on whether \code{inferPatches = TRUE}, the dataframe has additional inferred points. An additional column is created in each case, indicating whether the data are empirical fixes ('real') or 'inferred'.
#' @import data.table
#' @export
#'

wat_make_res_patch <- function(data,
                            buffer_radius = 10,
                            lim_spat_indep = 100,
                            lim_time_indep = 30,
                            lim_rest_indep = 30,
                            min_fixes = 3){
  # handle global variable issues
  time <- time_diff <- type <- x <- y <- npoints <- NULL
  patch <- nfixes <- id <- tide_number <- patchdata <- tidaltime <- NULL
  patch_summary <- time_start <- time_end <- duration <- nfixes <- NULL
  resTime <- resTime_mean <- restime_diff <- area <- NULL
  x_end <- y_end <- x_start <- y_start <- tidaltime_mean <- NULL
  spat_diff <- newpatch <- distInPatch <- distBwPatch <- dispInPatch <- NULL
  waterlevel <- polygons <- NULL

  # check data is a data.frame and has a resTime column
  {
    # check if data frame
    assertthat::assert_that(is.data.frame(data),
          msg = glue::glue('getResPatch: input not a dataframe object,
          has class {stringr::str_flatten(class(data), collapse = " ")}!'))

    assertthat::assert_that(min(as.numeric(diff(data$time))) >= 0,
                            msg = "wat_make_res_patch: not ordered in time!")

    assertthat::assert_that(min(c(buffer_radius, lim_spat_indep, lim_time_indep, min_fixes)) > 0,
                            msg = "wat_make_res_patch: function needs positive arguments")

  }

  # convert variable units
  {
    lim_time_indep = lim_time_indep*60
  }

  # get names and numeric variables
  data_names <- colnames(data)
  names_req <- c("id", "tide_number", "x", "y",
                 "time", "type", "resTime", "tidaltime",
                 attributes_to_get)

  # include asserts checking for required columns
  {
    purrr::walk (names_req, function(nr) {
      assertthat::assert_that(nr %in% data_names,
            msg = glue::glue('{nr} is required but missing from data!'))
    })
  }

  # make datatable to use functions
  if(is.data.table(data) != TRUE) {data.table::setDT(data)}

  # sort by time
  data.table::setorder(data, time)

  # check this has worked
  {
    assertthat::assert_that(min(diff(data$time)) >= 0,
                            msg = "data for segmentation is not ordered by time")
  }

  tryCatch(
    {
      # identify spatial overlap
      {
        # assign spat diff columns
        data[,`:=`(spat_diff = watlastools::wat_simple_dist(data = data,
                                                            x = "x", y = "y"))]

        # first spatial difference is infinity for calculation purposes
        data[1,c("spat_diff")] <- Inf

        # merge points if not spatially independent
        # compare distance from previous point to buffer_radius
        data <- data[,patch := cumsum(spat_diff > (2*buffer_radius))]
      }

      # count fixes and patch and remove small patches
      {
        # count number of points per patch
        data <- data[,nfixes := .N, by = c("id", "tide_number", "patch")]

        # remove patches with 2 or fewer points
        data <- data[nfixes >= min_fixes | type == "inferred", ]
        data[,nfixes:=NULL]
      }

      # get time mean and extreme points for spatio-temporal independence calc
      {
        # nest data
        data <- data[, .(list(.SD)), by = .(id, tide_number, patch)]
        setnames(data, old = "V1", new = "patchdata")
        data[,nfixes := purrr::map_int(patchdata, nrow)]

        # summarise mean, first and last
        data[,patch_summary := lapply(patchdata, function(dt){
          dt2 <- dt[,unlist(lapply(.SD, function(d){
            list(mean = mean(d),
                 start = first(d),
                 end = last(d))
          }), recursive = FALSE), .SDcols = c("x","y","time","resTime")]

          setnames(dt2,
                   str_replace(colnames(dt2), "\\.", "_"))

          return(dt2)
        })]

        # assess independence using summary data
        {
          patch_summary <- data[,unlist(patch_summary, recursive = FALSE),
                                   by = .(id, tide_number, patch)]
          data[,patch_summary := NULL]
          # get time bewteen start of n+1 and end of n
          patch_summary[,time_diff := c(Inf,
                                      as.numeric(time_start[2:length(time_start)] -
                                                   time_end[1:length(time_end)-1]))]
          # get spatial difference from last to first point
          patch_summary[,spat_diff := c(watlastools::wat_bw_patch_dist(data = patch_summary,
                                                                     x1 = "x_end", x2 = "x_start",
                                                                     y1 = "y_end", y2 = "y_start"))]
          # set spat_diff 1 to Inf
          patch_summary[1,"spat_diff"] <- Inf

          # get differences in mean residence time
          patch_summary[,restime_diff := c(Inf, abs(as.numeric(diff(resTime_mean))))]

          # assess independence and assign new patch
          patch_summary[,newpatch := cumsum((time_diff > lim_time_indep) |
                                             (spat_diff > lim_spat_indep) |
                                             restime_diff > lim_rest_indep)]
        }
          # get cols with old and new patch
          patch_summary <- patch_summary[,.(patch, newpatch)]
      }

      # basic patch metrics for new patches
      {
        # join patchdata to patch summary by new patch
        # expand data to prepare for new patches
        data <- data[, unlist(patchdata, recursive = FALSE),
                 by = .(id, tide_number, patch)]

        data <- data.table::merge.data.table(data, patch_summary, by = "patch")
        data[,`:=`(patch=newpatch, newpatch=NULL)]

        # nest data again
        data <- data[, .(list(.SD)), by = .(id, tide_number, patch)]
        setnames(data, old = "V1", new = "patchdata")
        data[,nfixes:=lapply(patchdata, nrow)]

        # basic metrics by new patch
        data[,patch_summary:= lapply(patchdata, function(dt){
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
        data[,distInPatch := purrr::map_dbl(patchdata, function(df){
                                    sum(watlastools::wat_simple_dist(data = df), na.rm = TRUE)
                                  })]

        # distance between patches
        tempdata <- data[,unlist(patch_summary, recursive = FALSE),
                                 by = .(id, tide_number, patch)]
        data[,patch_summary:=NULL]
        data[,distBwPatch := watlastools::wat_bw_patch_dist(data = tempdata,
                                                            x1 = "x_end", x2 = "x_start",
                                                            y1 = "y_end", y2 = "y_start")]
        # displacement in a patch
        # apply func bw patch dist reversing usual end and begin
        tempdata[,dispInPatch := sqrt((x_end - x_start)^2 + (y_end - y_start)^2)]
        # type of patch
        data[, type := purrr::map_chr(patchdata, function(df){
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
        data[, polygons := lapply(patchdata, function(df){
          p1 <- sf::st_as_sf(df, coords = c("x", "y"))
          p2 <- sf::st_buffer(p1, dist = buffer_radius)
          p2 <- sf::st_union(p2)
          return(p2)
        })]

        # add area and circularity
        data[,area := purrr::map_dbl(polygons, sf::st_area)]
        data[,`:=`(circularity = (4*pi*area)/purrr::map_dbl(polygons, function(pgon){
          boundary <- sf::st_boundary(pgon)
          perimeter <- sf::st_length(boundary)
          return(as.numeric(perimeter)^2)
        }))]

        # remove polygons
        data[,polygons := NULL]
      }

      # remove patch summary from some data and add temp data, then del tempdata
      data <- data.table::merge.data.table(data, tempdata, by = c("id", "tide_number", "patch"))

      return(data)
    },
    # null error function, with option to collect data on errors
    error= function(e)
    {
      message(glue::glue('\nthere was an error in {unique(data$id)} {unique(data$tide_number)}\n'))
      # dfErrors <- append(dfErrors, glue(z$id, "_", z$tidalCycle))
    }
  )

}

# ends here

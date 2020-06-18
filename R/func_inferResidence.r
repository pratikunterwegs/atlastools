#' Infer residence patches from gaps in the canonical data.
#'
#' @param data A dataframe of recurse analysis, or must include, in addition to x, y and time columns, a residence time column named resTime, id, and tide_number, a tidaltime column named tidaltime.
#' @param inf_patch_time_diff A numeric duration in minutes, of the minimum time difference between two points, above which, it is assumed worthwhile to examine whether there is a missing residence patch to be inferred.
#' @param inf_patch_spat_diff A numeric distance in metres, of the maximum spatial distance between two points, below which it may be assumed few extreme movements took place between them.
#'
#' @return A data.frame extension object. This dataframe has additional inferred points, indicated by the additional column for empirical fixes ('real') or 'inferred'.
#' @export
#'

wat_infer_residence <- function(data,
                               inf_patch_time_diff = 30,
                               inf_patch_spat_diff = 100){

  # handle global variable issues
  inf_patch<-nfixes<-posId<-resPatch<-resTime<-resTimeBool<-rollResTime <- NULL
  spat_diff <- time <- time_diff <- type <- x <- y <- npoints <- NULL
  duration <- id <- nfixes <- patch <- patchSummary <- NULL
  tide_number <- tidaltime <- time_end <- time_start <- NULL
  waterlevel <- NULL
  # adding the inferPatches argument to prep for inferring
  # residence patches from missing data between travel segments

  # check if data frame
  assertthat::assert_that(is.data.frame(data),
        msg = glue::glue('inferResidence: input not a dataframe object,\\
        has class {stringr::str_flatten(class(data), collapse = " ")}!'))

  # read the file in
  {
    # convert both to DT if not
    if(is.data.table(data) != TRUE) {data.table::setDT(data)}

  }

  # convert argument units
  {
    inf_patch_time_diff = inf_patch_time_diff*60
  }

  # get names and numeric variables
  data_names <- colnames(data)
  names_req <- c("id", "tide_number", "x", "y", "time", "resTime")
  num_vars <- c("x","y","TIME","resTime")

  # include asserts checking for required columns
  {
    purrr::walk (names_req, function(nr) {
      assertthat::assert_that(nr %in% data_names,
          msg = glue::glue('{nr} is required but missing from data!'))
    })
  }

  ## SET THE data IN ORDER OF TIME ##
  data.table::setorder(data, time)

  # check this has worked
  {
    assertthat::assert_that(min(diff(data$time)) >= 0,
                            msg = "data for segmentation is not ordered by time")
  }

  # make a data with id, tide_number and time seq, with missing x and y
  # identify where there are missing segments more than 2 mins long
  # there, create a sequence of points with id, tide, and time in 3s intervals
  # merge with true data
  temp_data <- data[!is.na(time),]
  # get difference in time and space
  temp_data <- temp_data[,`:=`(time_diff = c(diff(time), NA),
                         spat_diff = watlastools::wat_simple_dist(data = temp_data, x = "x", y = "y"))]

  # find missing patches if time_diff is greater than specified (default 30 mins)
  # AND spat_diff is less than specified (100 m)
  temp_data[,inf_patch := cumsum((time_diff > inf_patch_time_diff) &
            (spat_diff < inf_patch_spat_diff))]

  # subset the data to collect only the first two points of an inferred patch
  # these are the first and last points of a travel trajectory
  temp_data[,posId := seq(1, .N), by = "inf_patch"]
  # remove NA patches
  temp_data <- temp_data[posId <= 2 & !is.na(inf_patch),]
  # now count the max posId per patch, if less than 2, merge with next patch
  temp_data[,npoints := max(posId), by = "inf_patch"]
  temp_data[,inf_patch := ifelse(npoints == 2, yes = inf_patch, no = inf_patch+1)]
  temp_data <- temp_data[npoints >= 2,]
  # recount the number of positions, each inferred patch must have minimum 2 pos
  {
    assertthat::assert_that(min(temp_data$npoints) > 1,
                            msg = "some inferred patches with only 1 position")
  }
  # remove unn columns
  data.table::set(temp_data, ,c("posId","npoints"), NULL)

  # add type to real data
  data[,type:="real"]

  # enter this step only if there are 2 or more rows of data between which to infer patches
  if(nrow(temp_data) >= 2)
  {
    # make list column of expected times with 3 second interval
    # assume coordinate is the mean between 'takeoff' and 'landing'
    inf_patch_data <- temp_data[,nfixes:=length(seq(from = min(time, na.rm = T),
                                             to = max(time, na.rm = T), by = 3)),
                         by = c("id", "tide_number", "inf_patch")]

    # an expectation of integer type is created in time
    inf_patch_data <- inf_patch_data[,.(time = mean(time),
                                x = mean(x),
                                y = mean(y),
                                resTime = mean(time_diff)),
                             by = c("id", "tide_number", "inf_patch","nfixes")]

    inf_patch_data <- inf_patch_data[inf_patch > 0,]
    inf_patch_data <- inf_patch_data[,type:="inferred"]

    rm(temp_data); gc()

    # remove inf_patch and nfixes
    data.table::set(inf_patch_data, ,c("inf_patch", "nfixes"), NULL)

    # merge inferred data to empirical data
    data <- data.table::merge.data.table(data, inf_patch_data, by = intersect(names(data),
                                        names(inf_patch_data)), all = TRUE)
  }

  # sort by time
  data.table::setorder(data, time)
  # remove coordidx
  data[,`:=`(coordIdx = NULL, posID = NULL,
           fpt = NULL, revisits = NULL,
           temp_time = NULL)]

  # fill tidal time and waterlevel
  data[,`:=`(tidaltime = nafill(tidaltime, "locf"),
           waterlevel = nafill(waterlevel, "locf"))]

  # check this has worked
  {
    assertthat::assert_that(min(diff(data$time)) >= 0,
                            msg = "data for segmentation is not ordered by time")
  }

  return(data)

}

# ends here

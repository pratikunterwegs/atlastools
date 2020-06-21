#' A function to clean data accessed from the NIOZ WATLAS server.
#'
#' @param data A dataframe object returned by getData.
#' Must contain the columns "X", "Y", "SD", "NBS", "TAG", "TIME";
#' these are the X coordinate, Y coordinate, standard deviation in measurement,
#' number of ATLAS towers that received the signal, the tag number, and
#' the numeric time, in milliseconds from 1970-01-01.
#' @param sd_threshold A threshold value above which rows are removed.
#' @param nbs_min The minimum number of base stations (ATLAS towers) that
#' received tag signal.
#' @param moving_window The size of the moving window for the running
#' median calculation.
#' @param speed_cutoff The maximum speed in kilometres per hour allowed in the
#' raw data. Points with straight line displacement speeds calculated as above
#' this are removed.
#' @param filter_speed Logical specifiying whether to use the speed filter
#' or not.
#'
#' @return A datatable class object (extends data.frame) which has the
#' additional columns posID and ts, which is TIME converted to human
#' readable POSIXct format.
#' @export
#'
wat_clean_data <- function(data,
                           moving_window = 3,
                           nbs_min = 0,
                           sd_threshold = 500000,
                           filter_speed = TRUE,
                           speed_cutoff = 150) {

  SD <- NBS <- TIME <- TAG <- X <- Y <- NULL
  posID <- ts <- X_raw <- Y_raw <- VARX <- VARY <- COVXY <- NULL
  sld <- sld_speed <- NULL

  # check parameter types and assumptions
  assertthat::assert_that("data.frame" %in% class(data),
                          msg = "cleanData: not a dataframe object!")

  # include asserts checking for required columns
  data_names <- colnames(data)
  names_req <- c("X", "Y", "SD", "NBS", "TAG", "TIME", "VARX", "VARY", "COVXY")
  purrr::walk(names_req, function(nr) {
    assertthat::assert_that(nr %in% data_names,
                            msg = glue::glue("cleanData: {nr} is
                         required but missing from data!"))
  })

  # check args positive
  assertthat::assert_that(min(c(moving_window)) > 1,
                          msg = "cleanData: moving window not > 1")
  assertthat::assert_that(min(c(nbs_min)) >= 0,
                          msg = "cleanData: NBS min not positive")
  assertthat::assert_that(min(c(speed_cutoff)) >= 0,
                          msg = "cleanData: speed cutoff not positive")

  # convert both to DT if not
  if (data.table::is.data.table(data) != TRUE) {
    data.table::setDT(data)
  }

  # delete positions with approximated standard deviations above SD_threshold,
  # and below minimum number of base stations (NBS_min)
  data <- data[SD < sd_threshold & NBS >= nbs_min, ]

  prefix_num <- 31001000000

  # begin processing if there is data
  if (nrow(data) > 1) {
    # add position id and change time to seconds
    data[, `:=`(posID = seq_len(nrow(data)),
                   TIME = as.numeric(TIME) / 1e3,
                   TAG = as.numeric(TAG) - prefix_num,
                   X_raw = X,
                   Y_raw = Y)]

    if (filter_speed == TRUE) {
      # filter for insane speeds if asked
      data[, sld := watlastools::wat_simple_dist(data, "X", "Y")]
      data[, sld_speed := sld / c(NA, as.numeric(diff(TIME)))]
      data <- data[sld_speed <= (speed_cutoff / 3.6), ]
      data[, `:=`(sld = NULL, sld_speed = NULL)]
    }

    # make separate timestamp col
    data[, ts := as.POSIXct(TIME, tz = "CET", origin = "1970-01-01")]

    # median filter
    # includes reversed smoothing to get rid of a possible phase shift
    data[, lapply(.SD,
                 function(z) {
                   stats::runmed(rev(stats::runmed(z, moving_window)),
                                 moving_window)}),
         .SDcols = c("X", "Y")]

    ## postprocess (clean) data, start by selecting columns
    data <- data[, .(TAG, posID, TIME, ts, X_raw, Y_raw,
                    NBS, VARX, VARY, COVXY, X, Y, SD)]

    # rename x,y,time to lower case
    setnames(data, old = c("X", "Y", "TAG", "TIME"),
             new = c("x", "y", "id", "time"))

  } else {
    data <- data.table::data.table(matrix(NA, nrow = 0, ncol = 12))
    setnames(data, c("id", "posID", "time", "ts", "X_raw",
                            "Y_raw", "NBS", "VARX", "VARY", "COVXY", "x", "y"))
  }

  assertthat::assert_that("data.frame" %in% class(data),
              msg = "cleanData: cleanded data is not a dataframe object!")

  return(data)
}

# ends here

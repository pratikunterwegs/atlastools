#' Classify points as residence points based on residence time.
#'
#' @param data A data frame which must have a column for the
#' residence time at each point.
#' @param lim_res_time A numeric giving the time limit in minutes
#' against which residence time is compared.
#' @param min_fix_warning Triggers a warning if the data with travel segments
#' removed has fewer than this number of rows.
#' @return A data.frame extension object, which retains only points
#' classified as residence points if residence time is below \code{lim_res_time}
#'  over \code{travelSeg} points.
#' @export
#'

wat_classify_points <- function(data,
                             lim_res_time = 2,
                             min_fix_warning = 5) {
  # check data is a data.frame and has a resTime column
  {
    # check if data frame
    assertthat::assert_that(is.data.frame(data),
                            msg = glue::glue('wat_classify_points: input not a dataframe object, has class {stringr::str_flatten(class(data), collapse = " ")}!'))

    assertthat::assert_that("resTime" %in% names(data),
                            msg = "wat_classify_points: data has no residence time column")
    assertthat::assert_that(min(c(lim_res_time)) > 1,
                            msg = "wat_classify_points: function arguments are not positive")
  }

  # make datatable to use functions
  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }

  # handle global variable issues
  resTime <- NULL
  time <- timediff <- type <- x <- y <- npoints <- NULL

  # sort by time
  data.table::setorder(data, time)

  # check this has worked
  assertthat::assert_that(min(diff(data$time)) >= 0,
                          msg = "data for segmentation is not ordered by time")
  # prep to assign sequence to res patches
  # to each id.tide combination
  # remove NA vals in resTime
  data <- data[!is.na(resTime),]

  # drop NAs in rolling residence time evaluation
  # essentially the first and last elements will be dropped
  data <- data[resTime >= lim_res_time, ]

  # print message if dataframe has few rows
  if (nrow(data) < min_fix_warning) {
    message(glue::glue("segmented dataframe has < 5 rows"))
  }

  return(data)

}

# ends here

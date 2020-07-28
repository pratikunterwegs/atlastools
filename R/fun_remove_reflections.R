#' Remove reflected positions.
#'
#' @author Pratik R. Gupte
#'
#' @param data A dataframe or similar which has previously been cleaned.
#' @param x The name of the X coordinate column.
#' @param y The name of the Y coordinate column.
#' @param time The name of the timestamp column.
#' @param point_angle_cutoff The turning angle (in degrees) above which
#' high instantaneous speeds are considered an anomaly rather than fast transit.
#' @param reflection_speed_cutoff The speed (in m/s) above which an anomaly is
#' detected when combined with a high turning angle.
#' @param est_ref_len How many positions are expected to be in a reflection.
#'
#' @return A dataframe with reflections removed.
#' @export
atl_remove_reflections <- function(data,
                                   x = "x",
                                   y = "y",
                                   time = "time",
                                   point_angle_cutoff = 45,
                                   reflection_speed_cutoff = 40,
                                   est_ref_len = 1000) {
  speed <- NULL
  # check data
  atl_check_data(data, names_expected = c(x, y, time))

  # set order
  data.table::setorderv(data, time)

  # get speed and angle
  data[, `:=`(speed = atlastools::atl_get_speed(data),
              angle = atlastools::atl_turning_angle(data))]

  # prepare a vector of rows to discard
  vec_discard <- integer()

  # identify the last point before an anomaly
  anchor_point <- which(data$speed >=
                          reflection_speed_cutoff &
                          data$angle >= point_angle_cutoff)[1] - 1

  # message
  message(glue::glue("first anchor at {anchor_point}"))

  while (anchor_point < nrow(data) - 1) {

    # the next est_ref_len subsequent points are suspect
    suspect_point <- anchor_point + 1
    suspect_speeds <- data[suspect_point:est_ref_len, speed]

    # get the next highest speed, may be higher
    # use gt not gte else will get first suspect speed
    nx_high_speed <- which(suspect_speeds > data[suspect_point, speed])[1]
    # this gets the next highest speed, which should be the end of the
    # reflection, but may also be the beginning or end of another reflection
    # this method is best applied to small subsets of data or similar
    if (is.na(nx_high_speed)) {
      nx_high_speed <- which(suspect_speeds == sort(suspect_speeds,
                                                    decreasing = TRUE)[2])
    }
    reflection_end <- anchor_point + nx_high_speed + 1

    # when reflections do not end remove all subsequent data
    # this is more likely in smaller subsets
    if (is.na(reflection_end)) {
       reflection_end <- nrow(data)
    }

    # message ref end
    message(glue::glue("reflection ends {reflection_end}"))

    # identify rows to remove
    # may be excessive but works
    vec_discard <- c(vec_discard, seq(anchor_point, reflection_end))

    # set the next anchor
    next_anchor <- which(data$speed[reflection_end:nrow(data)] >=
                           reflection_speed_cutoff &
                           data$angle[reflection_end:nrow(data)] >=
                           point_angle_cutoff)[1] - 1

    if (is.na(next_anchor)) {
      break ()
    } else {
      anchor_point <- reflection_end + next_anchor
      # check for errors in order
      assertthat::assert_that(anchor_point > reflection_end,
                            msg = glue::glue("anchor point {anchor_point} is \\
                            before reflection end {reflection_end}"))
      # message
      message(glue::glue("next anchor is {anchor_point}"))
    }
  }

  # return the rows to be kept
  vec_keep <- setdiff(seq_len(nrow(data)), vec_discard)

  return(data[vec_keep, ])

}

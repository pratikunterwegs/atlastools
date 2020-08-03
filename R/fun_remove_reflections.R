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
                                   reflection_speed_cutoff = 20,
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
  # message(glue::glue("first anchor at {anchor_point}"))

  while (anchor_point < nrow(data) - 1) {

    # the next est_ref_len subsequent points are suspect
    suspect_point <- anchor_point + 1
    # find the max speed after the first anomaly, which is the blink away
    # the next highest should be the blink back
    suspect_speeds <- data[(suspect_point + 1):est_ref_len, speed]
    
    # drop NA here
    suspect_speeds <- stats::na.omit(suspect_speeds)

    # get the next highest speed
    nx_high_speed <- which.max(rank(suspect_speeds))
    # this gets the next highest speed, which should be the end of the
    # reflection, but may also be the beginning or end of another reflection
    if (suspect_speeds[nx_high_speed] < reflection_speed_cutoff) {
      reflection_end <- nrow(data)
      message(glue::glue("remove_reflections: reflection does not end within \\
              {est_ref_len} positions"))
    } else {
      reflection_end <- suspect_point + nx_high_speed + 1 # one added for safety
    }
    
    # message ref end
    # message(glue::glue("reflection ends {reflection_end}"))

    # identify rows to remove
    # may be excessive but works
    vec_discard <- c(vec_discard, seq(anchor_point, reflection_end))

    # set the next anchor
    next_anchor <- which(data$speed[reflection_end:nrow(data)] >=
                           reflection_speed_cutoff &
                           data$angle[reflection_end:nrow(data)] >=
                           point_angle_cutoff)[1]

    if (is.na(next_anchor)) {
      # break the loop if there's no further anomalies
      break ()
    } else {
      anchor_point <- reflection_end + next_anchor
      # check for errors in order
      assertthat::assert_that(anchor_point > reflection_end,
                            msg = glue::glue("anchor point {anchor_point} is \\
                            before reflection end {reflection_end}"))
      # message
      # message(glue::glue("next anchor is {anchor_point}"))
    }
  }

  # return the rows to be kept
  vec_keep <- setdiff(seq_len(nrow(data)), vec_discard)
  return(data[vec_keep, ])

}

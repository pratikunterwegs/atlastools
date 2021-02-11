#' Check data has required columns.
#'
#' An internal function that checks that the data.table has the required
#' columns. Used within most, if not all, other atlastools functions.
#'
#' @author Pratik R. Gupte
#' @param data The tracking data to check for required columns. Must be in the
#' form of a data.frame or similar, which can be handled by the function
#' colnames.
#' @param names_expected The names expected as a character vector.
#' By default, checks for the column names \code{x, y, time}.
#' @examples
#' # basic (and only) use
#' \dontrun{
#' atl_check_data(
#'   data = data,
#'   names_expected = c("x", "y", "time")
#' )
#' }
#'
#' @return None. Breaks if the data does not have required columns.
atl_check_data <- function(data,
                           names_expected = c("x", "y", "time")) {

  # get the colmumn names
  data_names <- colnames(data)

  invisible(
    vapply(names_expected, function(nr) {
      assertthat::assert_that(nr %in% data_names,
        msg = glue::glue("atl_check_data: {nr} is
                         required but missing from data!")
      )
    }, FUN.VALUE = TRUE)
  )
}

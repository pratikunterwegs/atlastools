#' Check data has required columns.
#'
#' @noRd
#' @param data The tracking data to check for required columns.
#' @param names_expected The names expected.
#'
#' @return None. Breaks if the data does not have required columns.
atl_check_data <- function(data,
                           names_expected = c("x", "y", "time")) {

  # get the colmumn names
  data_names <- colnames(data)

  invisible(lapply(names_expected, function(nr) {
    assertthat::assert_that(nr %in% data_names,
      msg = glue::glue("atl_check_data: {nr} is
                         required but missing from data!")
    )
  }))
}

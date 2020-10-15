#' Filter data by position covariates.
#'
#' @param data A dataframe or similar containing the variables to be filtered.
#' @param filters A character vector of filter expressions. An example might be
#' \code{"speed < 20"}. The filtering variables must be in the dataframe.
#' The function will not explicitly check whether the filtering variables are
#' present; this makes it flexible, allowing expressions such as
#' \code{"between(speed, 2, 20)"}, but also something to use at your own risk.
#' A missing filter variables \emph{will} result in an empty data frame.
#'
#' @return A dataframe filtered using the filters specified.
#' @export
atl_filter_covariates <- function(data,
                                  filters = c()) {

  # apply filters as a single evaluated parsed expression
  # first wrap them in brackets
  filters <- vapply(
    X = filters,
    FUN = function(this_filter) {
      sprintf("(%s)", this_filter)
    },
    FUN.VALUE = "character"
  )
  filters <- stringr::str_c(filters, collapse = " & ")
  filters <- parse(text = filters)
  # evaluate the parsed filters
  data <- data[eval(filters), ]

  # check for class and whether there are rows
  assertthat::assert_that("data.frame" %in% class(data),
    msg = "filter_covariates: cleaned data is not a dataframe object!"
  )

  # print warning if all rows are removed
  if (nrow(data) == 0) {
    warning("filter_covariates: cleaned data has no rows remaining!")
  }

  return(data)
}

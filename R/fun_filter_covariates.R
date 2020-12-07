#' Filter data by position covariates.
#'
#' The atlastools function \code{atl\_filter\_covariates} allows convenient filtering of a dataset by any number of logical filters.
#' This function can be used to easily filter timestamps in a range, as well as combine simple spatial and temporal filters.
#' It accepts a character vector of \code{R} expressions that each return a logical vector (i.e., \code{TRUE} or \code{FALSE}).
#' Each filtering condition is interpreted in the context of the dataset supplied, and used to filter for rows that satisfy each of the filter conditions. 
#' Users must make sure that the filtering variables exist in their dataset in order to avoid errors.
#' 
#' @author Pratik R. Gupte
#' @param data A dataframe or similar containing the variables to be filtered.
#' @param filters A character vector of filter expressions. An example might be
#' \code{"speed < 20"}. The filtering variables must be in the dataframe.
#' The function will not explicitly check whether the filtering variables are
#' present; this makes it flexible, allowing expressions such as
#' \code{"between(speed, 2, 20)"}, but also something to use at your own risk.
#' A missing filter variables \emph{will} result in an empty data frame.
#'
#' @return A dataframe filtered using the filters specified.
#' @examples
#' \dontrun{
#' night_data <- atl_filter_covariates(data = dataset,
#'                filters = c("!inrange(hour, 6, 18)"))
#'
#' data_in_area <- atl_filter_covariates(data = dataset,
#'                    filters = c("between(time, t_min, t_max)",
#'                                "between(x, x_min, x_max)"))
#' filtered_data <- atl_filter_covariates(data = data,
#'                    filters = c("NBS > 3",
#'                                "SD < 100",
#'                                "between(day, 5, 8)"))
#' }
#' @export
atl_filter_covariates <- function(data,
                                  filters = c()) {

  # convert to data.table
  if (!is.data.table(data)) {
    data.table::setDT(data)
  }

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

#' Get residence patch data.
#'
#' @param res_patch_data A tibble with a nested list column of the raw data
#' underlying each patch. This column is specified in the
#' argument \code{dataColumn}
#' @param which_data Which data to return, the raw data underlying the patch,
#' or a spatial features object with only the patch summary.
#' @param buffer_radius Spatial buffer radius (in metres) around points when
#' requesting sf based polygons.
#' @return An object of type \code{sf} or \code{tibble} depending on
#' which data is requested.
#' @import data.table
#' @export
#'
atl_get_patch_summary <- function(res_patch_data,
                            which_data = "summary",
                            buffer_radius = 10) {
  id <- tide_number <- patch <- patchdata <- NULL

  # check somedata is a data.frame and has a resTime column
  assertthat::assert_that(is.data.frame(res_patch_data),
              msg = glue::glue("getPatchData: input not a dataframe object, \\
              has class {stringr::str_flatten(class(res_patch_data),
                               collapse = ' ')}!"))
  # convert both to DT if not
  if (!data.table::is.data.table(res_patch_data)) {
    data.table::setDT(res_patch_data)
  }

  # return only summary if requested
  if (which_data == "summary") {
    res_patch_data$patchdata <- NULL
    # get rid of nested list columns
    res_patch_data <- res_patch_data[, lapply(.SD, unlist)]
  }

  # return only spatial summary if requested
  if (which_data %in% c("spatial", "Spatial")) {
    res_patch_data[, polygons := lapply(res_patch_data$patchdata, function(df) {
      p1 <- sf::st_as_sf(df, coords = c("x", "y"))
      p2 <- sf::st_buffer(p1, dist = buffer_radius)
      p2 <- sf::st_union(p2)
      return(p2)
    })]
    res_patch_data$patchdata <- NULL

    # make spatial polygons
    polygons <- purrr::reduce(res_patch_data$polygons, c)
    # temp remove
    res_patch_data[, polygons := NULL]
    # unlist all the list columns
    res_patch_data <- res_patch_data[, lapply(.SD, unlist)]
    # reassign
    res_patch_data$polygons <- polygons
    res_patch_data <- sf::st_as_sf(res_patch_data, sf_column_name = "polygons")
    res_patch_data <- sf::st_cast(res_patch_data, "MULTIPOLYGON")
  }

  # get points if asked
  if (which_data %in% c("points")) {
    res_patch_data <- res_patch_data[, .(id, tide_number, patch, patchdata)]
    res_patch_data <- res_patch_data[, unlist(patchdata, recursive = FALSE),
                         by = .(id, tide_number, patch)]
  }
  return(res_patch_data)
}

# ends here

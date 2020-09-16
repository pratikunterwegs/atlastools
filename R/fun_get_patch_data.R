#' Get residence patch data.
#'
#' @param patch_data A data.frame with a nested list column of the raw data
#' underlying each patch. Since data.frames don't support nested columns,
#' will actually be a data.table or similar extension.
#' @param which_data Which data to return, the raw data underlying the patch,
#' or a spatial features object with only the patch summary.
#' @param buffer_radius Spatial buffer radius (in metres) around points when
#' requesting sf based polygons.
#' @return An object of type \code{sf} or \code{data.table} depending on
#' which data is requested.
#' @import data.table
#' @export
#'
atl_patch_summary <- function(patch_data,
                              which_data = "summary",
                              buffer_radius = 10) {
  id <- patch <- patchdata <- NULL

  # check somedata is a data.frame and has a resTime column
  assertthat::assert_that(is.data.frame(patch_data),
              msg = glue::glue("getPatchData: input not a dataframe object, \\
              has class {stringr::str_flatten(class(patch_data),
                               collapse = ' ')}!"))
  # convert both to DT if not
  if (!data.table::is.data.table(patch_data)) {
    data.table::setDT(patch_data)
  }

  # return only summary if requested
  if (which_data == "summary") {
    patch_data$patchdata <- NULL
    # get rid of nested list columns
    patch_data <- patch_data[, lapply(.SD, unlist)]
  }

  # return only spatial summary if requested
  if (which_data %in% c("spatial", "Spatial")) {
    patch_data[, polygons := lapply(patch_data$patchdata, function(df) {
      p1 <- sf::st_as_sf(df, coords = c("x", "y"))
      p2 <- sf::st_buffer(p1, dist = buffer_radius)
      p2 <- sf::st_union(p2)
      return(p2)
    })]
    patch_data$patchdata <- NULL

    # make spatial polygons
    polygons <- Reduce(c, patch_data$polygons)
    # temp remove
    patch_data[, polygons := NULL]
    # unlist all the list columns
    patch_data <- patch_data[, lapply(.SD, unlist)]
    # reassign
    patch_data$polygons <- polygons
    patch_data <- sf::st_as_sf(patch_data, sf_column_name = "polygons")
    patch_data <- sf::st_cast(patch_data, "MULTIPOLYGON")
  }

  # get points if asked
  if (which_data %in% c("points")) {
    patch_data <- patch_data[, list(id, patch, patchdata)]
    patch_data <- patch_data[, unlist(patchdata, recursive = FALSE),
                         by = list(id, patch)]
  }
  return(patch_data)
}

# ends here

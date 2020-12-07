#' Get residence patch data.
#'
#' @param patch_data A data.frame with a nested list column of the raw data
#' underlying each patch. Since data.frames don't support nested columns,
#' will actually be a data.table or similar extension.
#' @param which_data Which data to return. May be the raw data underlying the patch (\code{which_data = "points"}),
#' or a spatial features (\code{sf-MULTIPOLYGON}) object with patch covariates (\code{which_data = "spatial"}),
#' or a data.table of the patch covariates without the geometry column (\code{which_data = "summary"}).
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
              has class {stringr::str_flatten(class(data),
                               collapse = ' ')}!")
  )
  # work on a copy
  data <- data.table::copy(patch_data)
  # convert both to DT if not
  if (!data.table::is.data.table(data)) {
    data.table::setDT(data)
  }

  # check length of which_data
  assertthat::assert_that(length(which_data) == 1,
    msg = "patch_summary: only one data type at a time"
  )

  # return only summary if requested
  if (which_data == "summary") {
    data$patchdata <- NULL
    # get rid of nested list columns
    data <- data[, lapply(.SD, unlist)]
  } else if (which_data %in% c("spatial", "Spatial")) {
    # return only spatial object if requested
    data[, polygons := lapply(data$patchdata, function(df) {
      p1 <- sf::st_as_sf(df, coords = c("x", "y"))
      p2 <- sf::st_buffer(p1, dist = buffer_radius)
      p2 <- sf::st_union(p2)
      return(p2)
    })]
    data$patchdata <- NULL

    # make spatial polygons
    polygons <- Reduce(c, data$polygons)
    polygons <- sf::st_sfc(polygons)
    # temp remove
    data[, polygons := NULL]
    # unlist all the list columns
    data <- data[, lapply(.SD, unlist)]
    # reassign
    data$polygons <- polygons
    data <- sf::st_as_sf(data, sf_column_name = "polygons")
    data <- sf::st_cast(data, "MULTIPOLYGON")
  } else if (which_data %in% c("points")) {
    # get points if asked
    data <- data[, list(id, patch, patchdata)]
    data <- data[, unlist(patchdata, recursive = FALSE),
      by = list(id, patch)
    ]
  }
  return(data)
}

# ends here

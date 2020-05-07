#' Get derived data from residence patch construction: patch summaries, sf-based patches, or raw points with patch numbers.
#'
#' @param resPatchData A tibble with a nested list column of the raw data underlying each patch. This column is specified in the argument \code{dataColumn}
#' @param whichData Which data to return, the raw data underlying the patch, or a spatial features object with only the patch summary.
#' @param bufferSize Spatial buffer around points when requesting sf based polygons.
#' @return An object of type \code{sf} or \code{tibble} depending on which data is requested.
#' @import data.table
#' @export
#'
wat_get_patch_summary = function(resPatchData,
                            whichData = "summary",
                            bufferSize = 10)
{
  res_patch_data <- resPatchData

  data <- id <- tide_number <- patch <- patchdata <- NULL
  # check somedata is a data.frame and has a resTime column
  {
    assertthat::assert_that(is.data.frame(res_patch_data),
                msg = glue::glue('getPatchData: input not a dataframe object, \\
                has class {stringr::str_flatten(class(res_patch_data), collapse = " ")}!'))
  }

  # convert to data.table
  {
    # convert both to DT if not
    if(data.table::is.data.table(somedata) != TRUE) {data.table::setDT(somedata)}
  }

  # return only summary if requested
  if(whichData == "summary")
  {
    res_patch_data$patchdata <- NULL
    res_patch_data <- res_patch_data[,lapply(.SD, unlist)]
  }

  # return only spatial summary if requested
  if(whichData %in% c("spatial","Spatial"))
  {
    res_patch_data[, polygons := lapply(res_patch_data$patchdata, function(df){
          p1 <- sf::st_as_sf(df, coords = c("x", "y"))
          p2 <- sf::st_buffer(p1, dist = bufferSize)
          p2 <- sf::st_union(p2)
          return(p2)
        })]
    res_patch_data$patchdata <- NULL

    # make spatial polygons
    {
      polygons <- purrr::reduce(res_patch_data$polygons, c)
      res_patch_data$polygons <- polygons
      res_patch_data <- sf::st_as_sf(res_patch_data, sf_column_name = "polygons")
    }
    res_patch_data <- sf::st_cast(res_patch_data, "MULTIPOLYGON")
  }

  if(whichData %in% c("points"))
  {
    res_patch_data <- res_patch_data[, .(id, tide_number, patch, patchdata)]
    res_patch_data <- res_patch_data[, unlist(patchdata, recursive = FALSE),
                         by = .(id, tide_number, patch)]
  }
  return(res_patch_data)
}

# ends here

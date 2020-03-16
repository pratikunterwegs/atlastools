#' getPatchData
#'
#' @param resPatchData A tibble with a nested list column of the raw data underlying each patch. This column is specified in the argument \code{dataColumn}
#' @param whichData Which data to return, the raw data underlying the patch, or a spatial features object with only the patch summary.
#' @param dataColumn Which nested list column contains the data.
#'
#' @return An object of type \code{sf} or \code{tibble} depending on which data is requested.
#' @import data.table
#' @export
#'
wat_get_patch_summary = function(resPatchData,
                            dataColumn = "patchdata",
                            whichData = "summary")
{
  data <- id <- tide_number <- patch <- NULL
  # check somedata is a data.frame and has a resTime column
  {
    assertthat::assert_that(is.data.frame(resPatchData),
                msg = glue::glue('getPatchData: input not a dataframe object,
                has class {stringr::str_flatten(class(resPatchData), collapse = " ")}!'))
    assertthat::assert_that(dataColumn %in% names(resPatchData),
                            msg = "getPatchData: data column not present in input!")

  }

  # return only summary if requested
  if(whichData %in% c("summary", "summary"))
  {
    resPatchData[, dataColumn] <- NULL
    return(resPatchData)
  }

  # return only spatial summary if requested
  if(whichData %in% c("spatial","Spatial"))
  {
    resPatchData[, dataColumn] <- NULL
    # make spatial polygons
    {
      polygons <- purrr::reduce(resPatchData$polygons, c)
      resPatchData$polygons <- polygons
      resPatchData <- sf::st_as_sf(resPatchData, sf_column_name = "polygons")
    }
    resPatchData <- sf::st_cast(resPatchData, "MULTIPOLYGON")
    return(resPatchData)
  }

  if(whichData %in% c("points"))
  {
    resPatchData[,'polygons'] <- NULL
    resPatchData <- resPatchData[, c("id", "tide_number", "patch", dataColumn),
                                 with=FALSE]
    resPatchData <- resPatchData[, unlist(patchdata, recursive = FALSE),
                         by = .(id, tide_number, patch)]
    return(resPatchData)

  }

}

# ends here

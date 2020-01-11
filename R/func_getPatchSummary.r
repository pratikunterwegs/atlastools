#' getPatchData
#'
#' @param resPatchData A tibble with a nested list column of the raw data underlying each patch. This column is specified in the argument \code{dataColumn}
#' @param whichData Which data to return, the raw data underlying the patch, or a spatial features object with only the patch summary.
#' @param dataColumn Which nested list column contains the data.
#'
#' @return An object of type \code{sf} or \code{tibble} depending on which data is requested.
#' @export
#'
wat_get_patch_summary = function(resPatchData,
                            dataColumn = "data",
                            whichData = "summary")
{
  data <- NULL
  # check somedata is a data.frame and has a resTime column
  {
    assertthat::assert_that(is.data.frame(resPatchData),
                            msg = glue::glue('getPatchData: input not a dataframe object, has class {stringr::str_flatten(class(resPatchData), collapse = " ")}!'))
    assertthat::assert_that(dataColumn %in% names(resPatchData),
                            msg = "getPatchData: data column not present in input!")

  }

  # return only summary if requested
  if(whichData %in% c("summary", "summary"))
  {
    resPatchData <- sf::st_drop_geometry(resPatchData)
    resPatchData <- dplyr::select(resPatchData, -data)
    return(resPatchData)
  }

  # return only spatial summary if requested
  if(whichData %in% c("spatial","Spatial"))
  {
    thisdata <- dplyr::select(resPatchData, -data)
    thisdata <- sf::st_as_sf(thisdata, sf_column_name = "polygons")
    thisdata <- sf::st_cast(thisdata, "MULTIPOLYGON")
    return(thisdata)
  }

  if(whichData %in% c("points"))
  {

    this_data <- sf::st_drop_geometry(resPatchData)
    rm(resPatchData)
    # this_data <- dplyr::select(this_data, -polygons)
    this_data <- tidyr::unnest(this_data, cols = dataColumn,
                               names_repair = "universal")
    return(this_data)

  }

}

# ends here

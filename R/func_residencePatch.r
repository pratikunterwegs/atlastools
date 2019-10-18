#' getResPatches
#' @author Pratik R Gupte, \email{p.r.gupte@rug.nl}
#' @param df A dataframe of values of any class that is or extends data.frame. The dataframe must contain at least two spatial coordinates, \code{x} and \code{y}, and a temporal coordinate, \code{time}. The names of columns specifying these can be passed as arguments below.
#' @param x A column name (string) containing X (or longitude) coordinates.
#' @param y A column name (string) containing Y (or latitude) coordinates.
#' @param time A column name (string) containing the timestamp associated with each spatial coordinate pair.
#' @param buffsize A numeric value specifying the radius of the buffer to be drawn around each coordinate point. May be thought of as the distance that an individual can access, assess, or otherwise cover when at a discrete point in space.
#' @param returnSf A logical value of whether the constructed patches should be returned. When true, the \code{sf} object is returned bound to the patch summary dataframe in a list of length two.
#' @return Depending on whether the constructed poylgons are requested, a list object containing as its first element a dataframe of patch summaries, and as its second element an \code{sf MULTIPOLYGON} or \code{POLYGON} (as approporiate) object. The list is returned when \code{returnSf = TRUE}, and only the dataframe of patch summaries when \code{returnSf = FALSE}.
#' @export

funcGetResPatches <- function(df, x = "x", y = "y", time = "time",
                              buffsize = 10.0,
                              returnSf = FALSE){

  # handle global variable complaints
  id <- tidalcycle <- resPatch <- type <- sfdata <- data <- polygons <- NULL
  # more global vars
  area <- start <- timediff <- spatdiff <- indePatch <- tidaltime_mean <- NULL
  # yet more of these variables
  geometry <- X <- Y <- tidaltime <- time_mean <- distInPatch <- nfixes <- time_end <- time_start <- duration <- NULL


  #### check assumptions ####
  # assert df is a data frame
  {
    # check that args are strings
    # check times are numerics
    assertthat::assert_that(is.data.frame(df),
                            is.character(c(x,y,time)),
                            is.numeric(c(df$time)),
                            msg = "argument classes don't match expected arg classes")

    # get names and numeric variables
    dfnames <- names(df)
    namesReq <- c("id", "tidalcycle", "x", "y", "time", "resPatch", "type")

    # include asserts checking for required columns
    {
      for (i in 1:length(namesReq)) {
        assertthat::assert_that(namesReq[i] %in% dfnames,
                                msg = glue::glue('{namesReq[i]} is required but missing from data!'))
      }
    }
  }

  #### pass methods to try catch ####
  # try function and ignore errors for now
  tryCatch(
    {
      # convert to sf points object
      pts <- dplyr::group_by(df, id, tidalcycle, resPatch, type)
      pts <- tidyr::nest(pts)
      # ungroup
      pts <- dplyr::ungroup(pts)
      pts <- dplyr::mutate(pts, sfdata = purrr::map(pts$data, function(dff)
      {
        dff <- sf::st_as_sf(dff, coords = c(x, y))
        # set crs to UTM 31N
        sf::st_crs(dff) <- 32631
        return(dff)
      }))

      # make polygons
      pts <- dplyr::mutate(pts, polygons = purrr::map(pts$sfdata, function(dff)
      {
        # draw a 10 m buffer (arbitrary choice)
        dfpolygon <- sf::st_buffer(dff, buffsize)
        dfpolygon <- dplyr::summarise(dfpolygon)

        return(dfpolygon)
      }))

      # remove sf data
      pts <- dplyr::select(pts, -sfdata)

      # remove point polygons here, MIN polygon size is 5 points
      pts <- dplyr::filter(pts, purrr::map_int(data, nrow) > 5)

      # cast all to multipolygon and then single polygon
      pts <- dplyr::mutate(pts, polygons = purrr::map(polygons, function(dff)
      {
        dff <- sf::st_cast(dff, "MULTIPOLYGON")
        dff <- sf::st_cast(dff, "POLYGON")

        # get area for later filtering
        dff <- dplyr::mutate(dff, area = as.numeric(sf::st_area(dff)))

      }))

      # remove point polygons if real, keeping inferred polygons, which have only a single coord
      pts <- dplyr::mutate(pts, polygons = purrr::map2(polygons, type, function(spatial, kind)
      {
        if(kind == "real"){
          spatial <- dplyr::filter(spatial, area > 100*pi)
        }
        return(spatial)
      }))

      # summarise residence patch data from points
      # add ungroup, dplyr version changes have an effect
      patchSummary <- dplyr::ungroup(pts)
      patchSummary <- dplyr::transmute(patchSummary,
                                       id = id,
                                       tidalcycle = tidalcycle,
                                       type = type,
                                       resPatch = resPatch,
                                       summary = purrr::map(data, function(df)
                                       {
                                         # arrange by time
                                         dff <- dplyr::arrange(df, time)
                                         # get summary of time to determine merging based on temporal proximity
                                         dff <- dplyr::summarise_at(dff, dplyr::vars(time),
                                                                    list(start = dplyr::first,
                                                                         mean = mean))
                                         # return this
                                         return(dff)
                                       }))
      # unnest the data
      patchSummary <- tidyr::unnest(patchSummary, cols = c("summary"))
      # arrange in order of start time for interpatch distances
      patchSummary <- dplyr::arrange(patchSummary, start)
      # needs ungrouping
      patchSummary <- dplyr::ungroup(patchSummary)

      # join summary data with spatial data
      pts <- dplyr::left_join(pts, patchSummary, by = c("id", "tidalcycle", "type", "resPatch"))

      # unnest polygons column to get data
      # this has issues because of incompatible data types - check sf/dplyr/tidyr version
      pts <- tidyr::unnest_legacy(pts, cols = c(polygons), .drop = FALSE)
      # remove polygons col, which is now geometry
      pts <- dplyr::select(pts, -polygons)

      # clear garbage
      gc()

      # make actual polygons object
      pts <- sf::st_as_sf(pts, sf_column_name = "geometry")

      # get distance between polygons in space and time (using MEAN TIME)
      pts <- dplyr::ungroup(pts)
      # the spatial distance is already the edge to edge distance
      # not the centroid to centroid distance
      pts <- dplyr::mutate(pts, spatdiff = c(Inf, as.numeric(sf::st_distance(x = pts[1:nrow(pts)-1,],
                                                                             y = pts[2:nrow(pts),],
                                                                             by_element = T))),
                           # temporal diffs on mean
                           timediff = c(Inf, diff(mean)))

      # identify independent patches
      pts <- dplyr::mutate(pts, indePatch = cumsum(timediff > 3600 | spatdiff > 50))

      # merge polygons by indepatch and handle the underlying data
      sf::st_crs(pts) <- 32631 # setting crs to 31N
      pts <- dplyr::group_by(pts, id, tidalcycle, indePatch)
      # merge polygons
      pts <- dplyr::summarise(pts, data = list(data),
                              # tag patches as mixed if comprised of real and inferred
                              type = ifelse(length(unique(type)) == 2,
                                            "mixed", dplyr::first(type)))

      # may need an ungroup here
      pts <- dplyr::ungroup(pts)
      # get the distinct observations
      pts <- dplyr::mutate(pts, data = purrr::map(data, function(dff)
      {
        dff <- dplyr::bind_rows(dff)
        dff <- dplyr::distinct(dff)
        dff <- dplyr::arrange(df, time)
        dff <- sf::st_as_sf(dff, coords = c("x", "y"))
        sf::st_crs(dff) <- 32631 # setting to UTM 31N

        return(dff)
      }))

      # clip underlying data by polygon boundaries
      pts <- dplyr::mutate(pts, data = purrr::map2(data, geometry, function(dff1, dff2)
      {
        # keep rows contained by polygon
        dff1 <- dff1[unlist(sf::st_contains(dff2, dff1)),]
        # return kept rows
        return(dff1)
      }))

      # get patch summary from underlying data
      # add x,y,time summary
      pts <- dplyr::mutate(pts, patchSummary = purrr::map(data, function(dff)
      {
        # get temporary coordinates before dropping geometry
        tempcoords <- sf::st_coordinates(dff)
        tempcoords <- tibble::as_tibble(tempcoords)

        # add coords to dataframe
        dff <- dplyr::bind_cols(dff, tempcoords)
        dff <- sf::st_drop_geometry(dff)
        dff <- dplyr::arrange(dff, time)
        dff <- dplyr::summarise_at(dff, dplyr::vars(X, Y, time, tidaltime),
                                   list(mean = mean,
                                        start = dplyr::first,
                                        end = dplyr::last))

        return(dff)
      }))

      # add total within patch distance
      pts <- dplyr::mutate(pts, distInPatch = purrr::map_dbl(data, function(dff)
      {
        return(sum(dff$dist, na.rm = T)) # this distance is calculated earlier in the processing
      }))

      # export sf object if requested
      if(returnSf == TRUE){
        # add mean time
        patchSf <- dplyr::mutate(pts, time_mean = purrr::map_dbl(patchSummary, function(thisdata)
        {
          return(thisdata$time_mean)
        }))
        # select some vars
        patchSf <- dplyr::select(patchSf, id, tidalcycle, tidaltime_mean,
                                 patch = indePatch, geometry,
                                 time_mean, type)
        # reorder and renumber patches by time
        patchSf <- dplyr::arrange(patchSf, time_mean)
        patchSf <- dplyr::mutate(patchSf, patch = 1:nrow(patchSf))
      }


      # arrange patches by start time and add between patch distance
      # add area and number of fixes and distance per patch
      # also proportion of expected positions received

      pts <- dplyr::mutate(pts, area = as.numeric(sf::st_area(pts)),
                           nfixes = purrr::map_int(data, nrow),
                           distPerPoint = distInPatch/nfixes)
        # drop geometry
        pts <- sf::st_drop_geometry(pts)
        # remove data column, unnest summary and arrange by time
        pts <- dplyr::select(pts, -data)
        pts <- tidyr::unnest_legacy(pts, cols = c(patchSummary), .drop = TRUE)
        pts <- dplyr::arrange(pts, time_mean)

        # add distance between and duration in SECONDS
        pts <- dplyr::mutate(pts, patch = 1:nrow(pts),
                      distBwPatch = funcBwPatchDist(pts, x1 = "X_end", x2 = "X_start",
                                                      y1 = "Y_end", y2 = "Y_start"),
                      duration = time_end - time_start,
                      propFixes = nfixes/(duration/3))
        pts <- dplyr::select(pts, -indePatch)

      gc();

      # return the patch data as function output
      print(glue::glue('residence patches of {unique(df$id)} in tide {unique(df$tidalcycle)} constructed...'))

      if(returnSf == TRUE){
        return(list(pts, patchSf))
      } else{
        return(pts)
      }
    },
    # null error function, with option to collect data on errors
    error= function(e)
    {
      print(glue::glue('\nthere was an error in id_tide combination...
                                  {unique(df$id)} {unique(df$tidalcycle)}\n'))
      # dfErrors <- append(dfErrors, glue(z$id, "_", z$tidalCycle))
    },
    warning = function(w)
    {
      # do nothing...
    }
  )

}

# ends here

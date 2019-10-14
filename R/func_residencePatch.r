#### function to get residence patches ####

# Code author Pratik Gupte
# PhD student
# MARM group, GELIFES-RUG, NL
# Contact p.r.gupte@rug.nl

#### begin function here ####

#' getResPatches
#' @author Pratik R Gupte, \email{p.r.gupte@rug.nl}
#' @param df A dataframe of values of any class that is or extends data.frame. The dataframe must contain at least two spatial coordinates, `x` and `y`, and a temporal coordinate, `time`. The names of columns specifying these can be passed as arguments below.
#' @param x A column name (string) containing X (or longitude) coordinates.
#' @param y A column name (string) containing Y (or latitude) coordinates.
#' @param time A column name (string) containing the timestamp associated with each spatial coordinate pair.
#' @param buffsize A numeric value specifying the radius of the buffer to be drawn around each coordinate point. May be thought of as the distance that an individual can access, assess, or otherwise cover when at a discrete point in space.
#' @param returnSf A logical value (`TRUE` or `FALSE`) of whether the constructed patches should be returned. When true, the `sf` object is returned bound to the patch summary dataframe in a list of length two.
#' @return Depending on whether the constructed poylgons are requested, a list object containing as its first element a dataframe of patch summaries, and as its second element an `sf` `MULTIPOLYGON` or `POLYGON` (as approporiate) object. The list is returned when `returnSf = TRUE`, and only the dataframe of patch summaries when `returnSf` is `FALSE`.
#' @export TRUE
#'

funcGetResPatches <- function(df, x = "x", y = "y", time = "time",
                              buffsize = 10.0,
                              returnSf = FALSE){

  #### check assumptions ####
  # assert df is a data frame
  {
    # check that args are strings
    # check times are numerics
    assertthat::assert_that(is.data.frame(df),
                            is.character(c(x,y,time,tidaltime)),
                            is.numeric(c(df$time, df$tidaltime)),
                            msg = "argument classes don't match expected arg classes")

    # check that columns are present in data
    assertthat::assert_that(length(base::intersect(c(x,y,time,tidaltime), names(df))) == 4,
                            msg = "wrong column names provided, or df has wrong cols")
  }

  #### pass methods to try catch ####
  # try function and ignore errors for now
  tryCatch(
    {
      # convert to sf points object
      pts = df %>%
        dplyr::group_by(id, tidalcycle, resPatch, type) %>%
        tidyr::nest() %>%
        # make sd
        dplyr::mutate(sfdata = purrr::map(data, function(dff){
          sf::st_as_sf(dff, coords = c("x", "y")) %>%
            # assign crs
            sf::`st_crs<-`(32631)}))#

      # make polygons
      pts = pts %>%
        dplyr::mutate(polygons = purrr::map(sfdata, function(dff){
          # draw a 10 m buffer (arbitrary choice)
          sf::st_buffer(dff, buffsize) %>%
            sf::summarise()})) %>%
        # remove sf data
        dplyr::select(-sfdata)

      # remove point polygons here, MIN polygon size is 5 points
      pts = pts %>%
        dplyr::filter(purrr::map_int(data, nrow) > 5)

      # cast all to multipolygon and then single polygon
      pts = pts %>%
        dplyr::mutate(polygons = purrr::map(polygons, function(dff){
          sf::st_cast(dff, "MULTIPOLYGON") %>%
            sf::st_cast(., "POLYGON") %>%
            # get area for later filtering
            dplyr::mutate(area = as.numeric(st_area(.))) #%>%

        })) %>%
        # remove point polygons if real
        # keeps inferred polygons, which have only a single coord
        dplyr::mutate(polygons = purrr::map2(polygons, type, function(spatial, kind){
          if(kind == "real"){
            spatial = spatial %>%
              dplyr::filter(area > 100*pi)
          }
          return(spatial)
        }))

      # return to summarising residence patch data from points
      patchSummary = pts %>%
        # add ungroup, dplyr version changes have an effect
        dplyr::ungroup() %>%
        dplyr::transmute(id = id,
                  tidalcycle = tidalcycle,
                  type = type,
                  resPatch = resPatch,
                  summary = purrr::map(data, function(df){
                    # arrange by time
                    dff <- dplyr::arrange(df, time)

                    # get summary of time to determine merging based on temporal proximity
                    dff <- dff %>%
                      dplyr::summarise_at(vars(time),
                                   list(start = first,
                                        mean = mean))

                    # return this
                    return(dff)

                  })) %>%
        # unnest the data
        tidyr::unnest(cols = c("summary")) %>%
        # arrange in order of start time for interpatch distances
        dplyr::arrange(start) %>%
        # needs ungrouping
        dplyr::ungroup(.)

      # join summary data with spatial data
      pts = dplyr::left_join(pts, patchSummary)

      # unnest polygons column to get data
      # this has issues because of incompatible data types - check sf/dplyr/tidyr version
      pts = pts %>%
        tidyr::unnest_legacy(cols = c(polygons), .drop = FALSE) %>%
        # remove polygons col, which is now geometry
        dplyr::select(-polygons)

      # clear garbage
      gc()

      # make actual polygons object
      pts =
        pts %>%
        sf::st_as_sf(., sf_column_name = "geometry")

      # get distance between polygons in space and time (using MEAN TIME)
      pts = pts %>%
        # requires ungroup
        dplyr::ungroup(.) %>%
        # the spatial distance is already the edge to edge distance
        # not the centroid to centroid distance
        dplyr::mutate(spatdiff = c(Inf, as.numeric(sf::st_distance(x = pts[1:nrow(pts)-1,],
                                                        y = pts[2:nrow(pts),],
                                                        by_element = T))),
               # temporal diffs on mean
               timediff = c(Inf, diff(mean)))

      # identify independent patches
      pts = pts %>%
        dplyr::mutate(indePatch = cumsum(timediff > 3600 | spatdiff > 50))

      # merge polygons by indepatch and handle the underlying data
      pts =
        pts %>%
        sf::`st_crs<-`(32631) %>%
        dplyr::group_by(id, tidalcycle, indePatch) %>%
        # merge polygons
        dplyr::summarise(data = list(data),
                  # tag patches as mixed if comprised of real and inferred
                  type = ifelse(length(unique(type)) == 2,
                                "mixed", dplyr::first(type))) %>%
        # get the distinct observations
        dplyr::mutate(data = map(data, function(dff){
          dff %>%
            dplyr::bind_rows() %>%
            dplyr::distinct() %>%
            dplyr::arrange(time) %>%
            sf::st_as_sf(coords = c("x", "y")) %>%
            sf::`st_crs<-`(32631)
        })) %>%
        mutate(data = purrr::map2(data, geometry, function(dff1, dff2){
          # keep rows contained by polygon
          dff1 = dff1[unlist(sf::st_contains(dff2, dff1)),]
          # return kept rows
          return(dff1)
        }))

      # get patch summary from underlying data
      pts =
        pts %>%
        # add x,y,time summary
        dplyr::mutate(patchSummary = map(data, function(dff){
          dff %>%
            dplyr::bind_cols(., sf::st_coordinates(dff) %>% tibble::as_tibble()) %>%
            sf::st_drop_geometry() %>%
            dplyr::arrange(time) %>%
            dplyr::summarise_at(dplyr::vars(X, Y, time, tidaltime),
                         list(mean = mean,
                              start = first,
                              end = last))
        })) %>%
        # add total within patch distance
        dplyr::mutate(distInPatch = purrr::map_dbl(data, function(dff){
          sum(dff$dist, na.rm = T) # this distance is calculated earlier in the processing
        }))

      # export sf object if requested
      if(returnSf == TRUE){
        patchSf = pts %>%
          dplyr::mutate(time_mean = purrr::map_dbl(patchSummary, function(thisdata){
            thisdata$time_mean
          })) %>%
          dplyr::select(id, tidalcycle, time_mean,
                                 patch = indePatch, geometry,
                                 time_mean, type) %>%
          # hopefully fixes some patch misordering
          dplyr::arrange(time_mean) %>%
          dplyr::mutate(patch = 1:nrow(.))
      }


      # arrange patches by start time and add between patch distance
      pts =
        pts %>%
        # add area and number of fixes and distance per patch
        # also proportion of expected positions received
        dplyr::mutate(area = as.numeric(sf::st_area(.)),
               nfixes = purrr::map_int(data, nrow),
               distPerPoint = distInPatch/nfixes) %>%

        # drop geometry
        sf::st_drop_geometry() %>%
        # remove data column
        dplyr::select(-data) %>%
        tidyr::unnest_legacy(cols = c(patchSummary), .drop = TRUE) %>%
        dplyr::arrange(time_mean) %>%
        # add distance between and duration in SECONDS
        dplyr::mutate(patch = 1:nrow(.),
               distBwPatch = funcPatchDistance(., x1 = "X_end", x2 = "X_start",
                                               y1 = "Y_end", y2 = "Y_start"),
               duration = time_end - time_start,
               propFixes = nfixes/(duration/3)) %>%
        dplyr::select(-indePatch)

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
    }
  )

}

# ends here

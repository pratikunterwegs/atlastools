#### function to get residence patches ####

# Code author Pratik Gupte
# PhD student
# MARM group, GELIFES-RUG, NL
# Contact p.r.gupte@rug.nl

# currently complains about vectorising geometry cols, but seems to work

# x = "x"; y = "y"; time = "time"; tidaltime = "tidaltime"; buffsize = 10

# use sf
library(tidyverse); library(sf)

#### begin function here ####

funcGetResPatches <- function(df, x = "x", y = "y", time = "time", 
                              tidaltime = "tidaltime",
                              buffsize = 10.0,
                              returnSf = FALSE){
  
  #### check assumptions ####
  # assert df is a data frame
  {
    assertthat::assert_that(is.data.frame(df),
                            is.character(c(x,y,time,tidaltime)), # check that args are strings
                            is.numeric(c(df$time, df$tidaltime)), # check times are numerics
                            msg = "argument classes don't match expected arg classes")
    
    assertthat::assert_that(length(base::intersect(c(x,y,time,tidaltime), names(df))) == 4,
                            msg = "wrong column names provided, or df has wrong cols")
  }
  
  #### pass methods to try catch ####
  # try function and ignore errors for now
  tryCatch(
    {
      # convert to sf points object
      pts = df %>%
        group_by(id, tidalcycle, resPatch, type) %>% 
        nest() %>% 
        # make sd
        mutate(sfdata = map(data, function(dff){
          st_as_sf(dff, coords = c("x", "y")) %>%
            # assign crs
            `st_crs<-`(32631)}))#
      
      # make polygons
      pts = pts %>%
        mutate(polygons = map(sfdata, function(dff){
          # draw a 10 m buffer (arbitrary choice)
          st_buffer(dff, buffsize) %>% 
            summarise()})) %>% 
        # remove sf data
        select(-sfdata)
      
      # remove point polygons here, MIN polygon size is 5 points
      pts = pts %>% 
        filter(map_int(data, nrow) > 5)
      
      # cast all to multipolygon and then single polygon
      pts = pts %>% 
        mutate(polygons = map(polygons, function(dff){
          st_cast(dff, "MULTIPOLYGON") %>% 
            st_cast(., "POLYGON") %>% 
            # get area for later filtering
            mutate(area = as.numeric(st_area(.))) #%>% 
          
        })) %>% 
        # remove point polygons if real
        # keeps inferred polygons, which have only a single coord
        mutate(polygons = map2(polygons, type, function(spatial, kind){
          if(kind == "real"){
            spatial = spatial %>% 
              filter(area > 100*pi)
          }
          return(spatial)
        }))
      
      # return to summarising residence patch data from points
      patchSummary = pts %>%
        # add ungroup, dplyr version changes have an effect
        ungroup() %>% 
        transmute(id = id,
                  tidalcycle = tidalcycle,
                  type = type,
                  resPatch = resPatch,
                  summary = map(data, function(df){
                    # arrange by time
                    dff <- arrange(df, time)
                    
                    # get summary of time to determine merging based on temporal proximity
                    dff <- dff %>% 
                      summarise_at(vars(time),
                                   list(start = first,
                                        mean = mean))
                    
                    # return this
                    return(dff)
                    
                  })) %>%
        # unnest the data
        unnest(cols = c("summary")) %>% 
        # arrange in order of start time for interpatch distances
        arrange(start) %>%
        # needs ungrouping
        ungroup()
      
      # join summary data with spatial data
      pts = left_join(pts, patchSummary)
      
      # unnest polygons column to get data
      # this has issues because of incompatible data types - check sf/dplyr/tidyr version
      pts = pts %>%  
        unnest_legacy(cols = c(polygons), .drop = FALSE) %>% 
        # remove polygons col, which is now geometry
        select(-polygons)
      
      # clear garbage
      gc()
      
      # make actual polygons object
      pts = 
        pts %>% 
        st_as_sf(., sf_column_name = "geometry")
      
      # get distance between polygons in space and time (using MEAN TIME)
      pts = pts %>% 
        # requires ungroup
        ungroup() %>% 
        # the spatial distance is already the edge to edge distance
        # not the centroid to centroid distance
        mutate(spatdiff = c(Inf, as.numeric(st_distance(x = pts[1:nrow(pts)-1,], 
                                                        y = pts[2:nrow(pts),], 
                                                        by_element = T))),
               # temporal diffs on mean
               timediff = c(Inf, diff(mean)))
      
      # identify independent patches
      pts = pts %>%  
        mutate(indePatch = cumsum(timediff > 3600 | spatdiff > 50))  
      
      # merge polygons by indepatch and handle the underlying data
      pts =
        pts %>% 
        `st_crs<-`(32631) %>% 
        group_by(id, tidalcycle, indePatch) %>%
        # merge polygons
        summarise(data = list(data),
                  # tag patches as mixed if comprised of real and inferred
                  type = ifelse(length(unique(type)) == 2, 
                                "mixed", first(type))) %>% 
        # get the distinct observations
        mutate(data = map(data, function(dff){
          dff %>% 
            bind_rows() %>% 
            distinct() %>% 
            arrange(time) %>% 
            st_as_sf(coords = c("x", "y")) %>% 
            `st_crs<-`(32631)
        })) %>% 
        mutate(data = map2(data, geometry, function(dff1, dff2){
          # keep rows contained by polygon  
          dff1 = dff1[unlist(st_contains(dff2, dff1)),]
          # return kept rows
          return(dff1)
        }))
      
      # get patch summary from underlying data
      pts = 
        pts %>% 
        # add x,y,time summary
        mutate(patchSummary = map(data, function(dff){
          dff %>% 
            bind_cols(., st_coordinates(dff) %>% as_tibble()) %>% 
            st_drop_geometry() %>% 
            arrange(time) %>% 
            summarise_at(vars(X, Y, time, tidaltime),
                         list(mean = mean,
                              start = first,
                              end = last))
        })) %>% 
        # add total within patch distance
        mutate(distInPatch = map_dbl(data, function(dff){
          sum(dff$dist, na.rm = T) # this distance is calculated earlier in the processing
        }))
      
      # export sf object if requested
      if(returnSf == TRUE){
        patchSf = pts %>%
          mutate(time_mean = map_dbl(patchSummary, function(thisdata){
            thisdata$time_mean
          })) %>% 
          dplyr::select(id, tidalcycle, time_mean, 
                                 patch = indePatch, geometry,
                                 time_mean, type) %>% 
          # hopefully fixes some patch misordering
          arrange(time_mean) %>% 
          mutate(patch = 1:nrow(.))
      }
      
      
      # arrange patches by start time and add between patch distance
      pts =
        pts %>%
        # add area and number of fixes and distance per patch
        # also proportion of expected positions received
        mutate(area = as.numeric(st_area(.)),
               nfixes = map_int(data, nrow),
               distPerPoint = distInPatch/nfixes) %>%
        
        # drop geometry
        st_drop_geometry() %>%
        # remove data column
        select(-data) %>% 
        unnest_legacy(cols = c(patchSummary), .drop = TRUE) %>% 
        arrange(time_mean) %>% 
        # add distance between and duration in SECONDS
        mutate(patch = 1:nrow(.),
               distBwPatch = funcPatchDistance(., x1 = "X_end", x2 = "X_start",
                                               y1 = "Y_end", y2 = "Y_start"),
               duration = time_end - time_start,
               propFixes = nfixes/(duration/3)) %>% 
        select(-indePatch)
      
      gc();
      
      # return the patch data as function output
      print(glue('residence patches of {unique(df$id)} in tide {unique(df$tidalcycle)} constructed...'))
      
      if(returnSf == TRUE){
        return(list(pts, patchSf))
      } else{
        return(pts)
      }
    },
    # null error function, with option to collect data on errors
    error= function(e)
    {
      print(glue('\nthere was an error in id_tide combination...
                                  {unique(df$id)} {unique(df$tidalcycle)}\n'))
      # dfErrors <- append(dfErrors, glue(z$id, "_", z$tidalCycle))
    }
  )
  
}

# ends here
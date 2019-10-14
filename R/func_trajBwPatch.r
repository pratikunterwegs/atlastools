#### function for multilinestrings from patch jumps ####

library(sf)
library(tidyverse)

funcPatchTraj <- function(df, x1 = "X_end", x2 = "X_start",
                          y1 = "Y_end", y2 = "Y_start"){
  # must assert df has correct columns
  
  # select cols from dfs
  {
    x1 <- pull(df, x1)
    x2 <- pull(df, x2)
    x1 <- x1[1:length(x1)-1]; 
    x2 <- x2[2:length(x2)]}
  {
    y1 <- pull(df, y1)
    y2 <- pull(df, y2)
    y1 <- y1[1:length(y1)-1]; 
    y2 <- y2[2:length(y2)]
  }
  # make temptib
  tempTib = tibble(x1,y1,x2,y2)
  # add matrix as list col
  tempTib = tempTib %>% 
    mutate(ptsMat = pmap(tempTib, function(x1,x2,y1,y2){
      matrix(c(x1,y1,x2,y2), ncol = 2, byrow = T) 
    }))
  
  # make multilinestring from list col
  ml = st_multilinestring(tempTib$ptsMat)
  
  # return ml obj
  return(ml)
}

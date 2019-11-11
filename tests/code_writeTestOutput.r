#### write testoutput to file as map ####

# load libs
library(data.table)
library(ggplot2)
library(dplyr)

# load data

data_files <- list.files("tests/testdata/recdata/", pattern = "recurse", full.names = TRUE)
tide_files <- list.files("tests/testdata/htdata/", full.names = TRUE)

#### infer residence ####
this_data <- list(data_files, tide_files)
this_data <- purrr::map(this_data, purrr::map, readr::read_csv)
this_data <- purrr::map2(this_data[[1]], this_data[[2]], watlasUtils::funcInferResidence)

#### classify path ####
classified_data <- purrr::map(this_data, watlasUtils::funcClassifyPath)

#### get residence patch sf objects ####
patch_data <- purrr::map(classified_data, funcGetResPatch, makeSf = TRUE)

#### combine sf objects and write to shp ####
patch_sum <- do.call(what = sf:::rbind.sf, args = patch_data)

#### get trajectories ####
patch_traj <- purrr::map(patch_data, watlasUtils::funcPatchTraj)
data_traj <- sf::st_drop_geometry(patch_sum) %>%
  dplyr::distinct(id, tidalcycle) %>%
  dplyr::mutate(lines = patch_traj)
patch_traj <- sf::st_as_sf(data_traj, sf_column_name = "lines")

#### plot with tmap ####
library(tmap)
{
  map2 = tm_shape(patch_sum)+
    tm_polygons(col = "grey", style = "cat", alpha = 0.5)+
    tm_text(text = "patch", size = 0.7, col = "black")+
    tm_facets(by = "tidalcycle", free.coords = FALSE)+
    tm_shape(patch_traj)+
    tm_lines(col = "black", style = "cat",
             palette = scico::scico(3, palette = "berlin"))+
    tm_facets(by = "tidalcycle", free.coords = FALSE)
}

{
  png(filename = "tests/testdata/fig_testFunc.png", width = 600, height = 600,
      res = 100)
  map2
  dev.off()
}

#### export data ####
sf::st_crs(patch_sum) <- 32631
sf::st_write(patch_sum, dsn = "tests/testdata/patches", layer = "test_patches",
             driver = "ESRI Shapefile")
sf::st_crs(patch_traj) <- 32631
sf::st_write(patch_traj, dsn = "tests/testdata/patches_traj", layer = "test_patches_traj",
             driver = "ESRI Shapefile", delete_layer =TRUE)

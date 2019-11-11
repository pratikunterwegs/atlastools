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
patch_sum <- purrr::reduce(patch_data, dplyr::bind_rows)

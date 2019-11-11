#### write testoutput to file as map ####

# load libs
library(data.table)
library(ggplot2)

# load data

data_files <- list.files("tests/testdata/recdata/", pattern = "recurse", full.names = TRUE)
tide_files <- list.files("tests/testdata/htdata/", full.names = TRUE)


library(tidyverse)

# read in files
rwanda.files.names <-
  list.files('/Users/ElliotMeador/Downloads/Rwanda_CSVs/')

# read in files
rwanda.files <- map(rwanda.files.names, function(x) {
  read_csv(paste0('/Users/ElliotMeador/Downloads/Rwanda_CSVs/', x))
})


# rename the files
names(rwanda.files) <- rwanda.files.names


# read in the code book
Rwanda.codebook <-
  read_csv("~/Downloads/Rwanda 2013 FTF ZOI PBS Public Release Codebook-Variables .csv")

list2env(rwanda.files,envir = .GlobalEnv)
list2env(rwanda.files,envir = globalenv())

remove(RWANDA_WOMENS_FILE_PR.csv)







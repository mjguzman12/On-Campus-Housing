##########################
##########################
#####IPEDS DATA UNZIP#####
##########################
##########################

rm(list = ls())
library("stringr")

## HD
for (year in 2003:2020) {
  unzip(zipfile = paste0("RawData/IPEDS/data/HD", year, ".zip"),
        overwrite = TRUE,
        exdir = paste0("RawData/IPEDS/data unzipped/", year, "/HD", year))
}

## IC
for (year in 2003:2020) {
  unzip(zipfile = paste0("RawData/IPEDS/data/IC", year, ".zip"),
        overwrite = TRUE,
        exdir = paste0("RawData/IPEDS/data unzipped/", year, "/IC", year))
}

## GR
for (year in 2003:2020) {
  unzip(zipfile = paste0("RawData/IPEDS/data/GR", year, ".zip"),
        overwrite = TRUE,
        exdir = paste0("RawData/IPEDS/data unzipped/", year, "/GR", year))
}

## EF A
for (year in 2003:2020) {
  unzip(zipfile = paste0("RawData/IPEDS/data/EF", year, "A.zip"),
        overwrite = TRUE,
        exdir = paste0("RawData/IPEDS/data unzipped/", year, "/EF", year, "A"))
}

## EF D
for (year in 2003:2020) {
  unzip(zipfile = paste0("RawData/IPEDS/data/EF", year, "D.zip"),
        overwrite = TRUE,
        exdir = paste0("RawData/IPEDS/data unzipped/", year, "/EF", year, "D"))
}

## F_F1A
for (year in 2:19) {
  if (str_length(as.character(year)) == 1) {
    year1 <- paste0("0", year)
  } else {
    year1 <- as.character(year)
  }
  if (str_length(as.character(year + 1)) == 1) {
    year2 <- paste0("0", year + 1)
  } else {
    year2 <- as.character(year + 1)
  }
  year_final = paste0(year1, year2)
  
  unzip(zipfile = paste0("RawData/IPEDS/data/F", year_final, "_F1A.zip"),
        overwrite = TRUE,
        exdir = paste0("RawData/IPEDS/data unzipped/20", year2, "/F", year_final, "_F1A"))
}

## F_F2
for (year in 2:19) {
  if (str_length(as.character(year)) == 1) {
    year1 <- paste0("0", year)
  } else {
    year1 <- as.character(year)
  }
  if (str_length(as.character(year + 1)) == 1) {
    year2 <- paste0("0", year + 1)
  } else {
    year2 <- as.character(year + 1)
  }
  year_final = paste0(year1, year2)
  
  unzip(zipfile = paste0("RawData/IPEDS/data/F", year_final, "_F2.zip"),
        overwrite = TRUE,
        exdir = paste0("RawData/IPEDS/data unzipped/20", year2, "/F", year_final, "_F2"))
}

## F_F3
for (year in 2:19) {
  if (str_length(as.character(year)) == 1) {
    year1 <- paste0("0", year)
  } else {
    year1 <- as.character(year)
  }
  if (str_length(as.character(year + 1)) == 1) {
    year2 <- paste0("0", year + 1)
  } else {
    year2 <- as.character(year + 1)
  }
  year_final = paste0(year1, year2)
  
  unzip(zipfile = paste0("RawData/IPEDS/data/F", year_final, "_F3.zip"),
        overwrite = TRUE,
        exdir = paste0("RawData/IPEDS/data unzipped/20", year2, "/F", year_final, "_F3"))
}



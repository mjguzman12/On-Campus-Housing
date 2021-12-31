############################################
############################################
#####0. ON-CAMPUS HOUSING DATA CLEANING#####
############################################
############################################

rm(list = ls())

## Start by downloading the Code and RawData folders from GitHub.

## create subdirectories
make_dir <- function(dir_name) {
  if (dir.exists(dir_name)) {
    message(paste0('Already have directory: ', dir_name))
  } else if (!dir.exists(dir_name)) {
    message(paste0('Creating directory: ', dir_name))
    dir.create(dir_name)
  }
}

## Set Analysis directory
setwd("[SET DIRECTORY]/Analysis")

make_dir("RawData/IPEDS")
make_dir("RawData/IPEDS/data unzipped")
make_dir("IntermediateData")
make_dir("IntermediateData/IPEDS")
make_dir("IntermediateData/HUD")
make_dir("Output")
make_dir("Output/IPEDS Regressions")

source("Code/IPEDS Data Download.R")
source("Code/IPEDS Data Unzip.R")
source("Code/IPEDS Data Import.R")
source("Code/On-Campus Housing Data Import.R")
source("Code/HUD Data Import.R")




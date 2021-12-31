##############################################
##############################################
#####ON-CAMPUS HOUSING REQUIREMENT IMPORT#####
##############################################
##############################################

rm(list = ls())

library("readr")
library("readxl")
library("stringr")
library("dplyr")
library("tidyr")

###########
###INTRO###
###########
## This script imports the on-campus housing data that I compiled from
## online. I need to put these data into a yearly format.


############
###IMPORT###
############
campus_housing_data <- read_xlsx("RawData/On Campus Housing Requirements.xlsx",
                                 sheet = "On-Campus Housing Requirements") %>%
  rename("previous_req" = "Previous On Campus Housing Requirement (years)",
         "year_of_change" = "Year of Change (Fall of Academic Year)",
         "current_req" = "Current On Campus Housing Requirement (years)")  %>%
  select(unitid, previous_req, year_of_change, current_req) %>%
  filter(!is.na(current_req)) %>%
  mutate(previous_req = case_when(is.na(previous_req) ~ current_req,
                                  TRUE ~ as.numeric(previous_req)))

years_in_sample <- data.frame(year = seq(2003, 2019, by = 1))

campus_housing_data_merged <- campus_housing_data %>%
  full_join(years_in_sample, by = character()) %>%
  mutate(campus_housing_req = case_when(year_of_change > year ~ previous_req,
                                        TRUE ~ as.numeric(current_req))) %>%
  select(unitid, year, campus_housing_req) %>%
  rename(housing_req = campus_housing_req)

write_csv(campus_housing_data_merged, "IntermediateData/On-Campus Housing Requirements.csv")

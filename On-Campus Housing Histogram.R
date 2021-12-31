#####################################
#####################################
#####ON-CAMPUS HOUSING HISTOGRAM#####
#####################################
#####################################

setwd("/Users/matthew/Desktop/Research/On Campus Housing/Analysis")
rm(list = ls())
createdate <- "20211207"

library("readr")
library("dplyr")
library("tidyr")
library("scales")
library("ggplot2")

###########
###INTRO###
###########
## This script creates a histogram of on-campus housing requirements.


############
###IMPORT###
############
ipeds_oncampus_regression_pairs <- read_csv("IntermediateData/IPEDS/IPEDS On-Campus University-Year Pairs.csv")

on_campus_housing_data <- read_csv("IntermediateData/On-Campus Housing Requirements.csv") %>%
  rename(academic_year = year)

ipeds_sample_data <- read_csv("IntermediateData/IPEDS/IPEDS Combined Data.csv") %>%
  left_join(on_campus_housing_data, by = c("unitid", "academic_year")) %>%
  left_join(ipeds_oncampus_regression_pairs, by = c("unitid", "academic_year")) %>%
  filter(flag == 1) %>%
  filter(academic_year == 2017) %>%
  select(unitid, academic_year, instnm, housing_req)

label_data <- ipeds_sample_data %>%
  mutate(uni_count = 1) %>%
  group_by(housing_req) %>%
  summarise(uni_count = sum(uni_count)) %>%
  ungroup() %>%
  mutate(uni_count_label = uni_count) %>%
  mutate(uni_count = uni_count + 1)


##############
###GRAPH IT###
##############
housing_hist <- ggplot(data = ipeds_sample_data, aes(x = housing_req)) +
  geom_histogram(bins = 5, fill = "darkblue") +
  geom_text(data = label_data, aes(x = housing_req, y = uni_count, 
                                   label = uni_count_label), size = 8, fontface = "bold") +
  scale_x_continuous(name = "On-Campus Housing Requirement Length") +
  scale_y_continuous(name = "Count of Universities") +
  theme(plot.title = element_blank()) +
  theme(axis.title.x = element_text(size = 24, face = "bold")) +
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.title.y = element_text(size = 24, face = "bold")) +
  theme(axis.text.y = element_text(size = 18))

housing_hist
ggsave(paste0("Output/", createdate, " On-Campus Housing Histogram.jpg"),
       housing_hist, width = 14.11, height = 7)

housing_hist_pres <- ggplot(data = ipeds_sample_data, aes(x = housing_req)) +
  geom_histogram(bins = 5, fill = "darkblue") +
  geom_text(data = label_data, aes(x = housing_req, y = uni_count, 
                                   label = uni_count_label), size = 8, fontface = "bold") +
  scale_x_continuous(name = "On-Campus Housing Requirement Length") +
  scale_y_continuous(name = "Count of Universities") +
  labs(title = "Histogram of On-Campus Housing Requirements") +
  theme(plot.title = element_text(size = 26, face = "bold", hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 24, face = "bold")) +
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.title.y = element_text(size = 24, face = "bold")) +
  theme(axis.text.y = element_text(size = 18))

housing_hist_pres
ggsave(paste0("Output/", createdate, " On-Campus Housing Histogram Presentation.jpg"),
       housing_hist_pres, width = 14.11, height = 7)

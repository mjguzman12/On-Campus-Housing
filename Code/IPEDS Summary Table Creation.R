###################################
###################################
#####IPEDS SUMMARY DATA TABLES#####
###################################
###################################

rm(list = ls())

library("readr")
library("readxl")
library("dplyr")
library("stringr")
library("tidyr")
library("stargazer")

###########
###INTRO###
###########
## This script creates the IPEDS data summary tables


#############################
###ON-CAMPUS HOUSING TABLE###
#############################
hud_data <- read_csv("IntermediateData/HUD/Fair Market Housing Data.csv") %>%
  mutate(academic_year = fiscal_year + 1) %>%
  select(zip, academic_year, safmr2br)

ipeds_oncampus_regression_pairs <- read_csv("IntermediateData/IPEDS/IPEDS On-Campus University-Year Pairs.csv")

on_campus_housing_data <- read_csv("IntermediateData/On-Campus Housing Requirements.csv") %>%
  rename(academic_year = year)

ipeds_oncampus_basic_summary <- read_csv("IntermediateData/IPEDS/IPEDS Combined Data.csv") %>%
  mutate(zip = str_sub(zip, start = 1, end = 5)) %>%
  left_join(hud_data, by = c("academic_year", "zip")) %>%
  left_join(on_campus_housing_data, by = c("unitid", "academic_year")) %>%
  left_join(ipeds_oncampus_regression_pairs, by = c("unitid", "academic_year")) %>%
  filter(flag == 1) %>%
  mutate(bed_enrollment_ratio = roomcap/efalevel2) %>%
  select(school_type, grad_rate_150, ret_pcf, housing_req, 
         student_services_expenses, auxiliary_expenses, instruction_expenses,
         bed_enrollment_ratio, efalevel2, safmr2br) %>%
  mutate(N = 1) %>%
  rename(retentionrate = ret_pcf, gradrate = grad_rate_150, 
         housingreq = housing_req, stuservexp = student_services_expenses, auxexp = auxiliary_expenses,
         instexp = instruction_expenses, ber = bed_enrollment_ratio) %>%
  group_by(school_type) %>%
  summarise(across(c(retentionrate, gradrate, housingreq, stuservexp, auxexp, 
                     instexp, efalevel2, ber, safmr2br), 
                   .f = list(mean = mean, sd = sd, min = min, max = max), na.rm = TRUE),
            retentionrate_n = sum(N),
            gradrate_n = sum(N),
            housingreq_n = sum(N),
            stuservexp_n = sum(N),
            auxexp_n = sum(N),
            instexp_n = sum(N),
            efalevel2_n = sum(N),
            ber_n = sum(N),
            safmr2br_n = sum(N)) %>%
  ungroup() %>%
  pivot_longer(ends_with(c("_n", "_mean", "_sd", "_min", "_max")),
               names_to = "statistic") %>%
  separate(statistic, sep = "_", into = c("variable", "stat")) %>%
  mutate(value = round(value, 2)) %>%
  pivot_wider(id_cols = c("school_type", "variable"), names_from = stat,
              values_from = value) %>%
  rename("N" = "n", "St. Dev." = "sd", "Mean" = "mean", "Min" = "min",
         "Max" = "max") %>%
  arrange(desc(school_type)) %>%
  select(-school_type)

## On-Campus Housing table
stargazer(as.data.frame(ipeds_oncampus_basic_summary), summary = FALSE, 
          type = "latex", title = "IPEDS On-Campus Housing Summary Table"
          , digits = 2, flip = FALSE, rownames = FALSE,
          out = "Output/IPEDS Regressions/IPEDS On-Campus Housing Data Summary Table.tex")


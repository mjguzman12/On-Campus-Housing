#########################
#########################
#####HUD DATA IMPORT#####
#########################
#########################

rm(list = ls())

library("readr")
library("readxl")
library("stringr")
library("dplyr")
library("tidyr")

###########
###INTRO###
###########
## Import the HUD Fair Market Housing data. These data contain the average
## housing prices of studios, 1 bedroom, 2 bedroom, 3 bedroom, and 4
## bedroom homes. Note that zip codes are only included after 2012.
## Therefore, after importing these, create a list of zip codes and merge
## them back onto the data. This will allow you to merge with the university
## zip codes in the next step.


############
###IMPORT###
############
fmr_fy05 <- read_xls("RawData/HUD/Revised_FY2005_CntLevel.xls") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("safmr0br" = "fmr_0bed",
         "safmr1br" = "fmr_1bed", "safmr2br" = "fmr_2bed", 
         "safmr3br" = "fmr_3bed", "safmr4br" = "fmr_4bed") %>%
  select(msaname, countyname, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  rename("msa" = "msaname") %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2005)

fmr_fy06 <- read_xls("RawData/HUD/County_Town_FMRs_2006.xls") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "areaname", "safmr0br" = "fmr0",
         "safmr1br" = "fmr1", "safmr2br" = "fmr2", 
         "safmr3br" = "fmr3", "safmr4br" = "fmr4") %>%
  select(msa, countyname, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2006)

fmr_fy07 <- read_xls("RawData/HUD/FY2007F_County_Town.xls") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "areaname", "safmr0br" = "fmr0",
         "safmr1br" = "fmr1", "safmr2br" = "fmr2", 
         "safmr3br" = "fmr3", "safmr4br" = "fmr4") %>%
  select(msa, countyname, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2007)

fmr_fy08 <- read_xls("RawData/HUD/FMR_county_fy2008r_rdds.xls") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "areaname", "safmr0br" = "fmr0",
         "safmr1br" = "fmr1", "safmr2br" = "fmr2", 
         "safmr3br" = "fmr3", "safmr4br" = "fmr4") %>%
  select(msa, countyname, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2008)

fmr_fy09 <- read_xls("RawData/HUD/FY2009_4050_Rev_Final.xls") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "areaname", "safmr0br" = "fmr0",
         "safmr1br" = "fmr1", "safmr2br" = "fmr2", 
         "safmr3br" = "fmr3", "safmr4br" = "fmr4") %>%
  select(msa, countyname, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2009)

fmr_fy10 <- read_xls("RawData/HUD/FY2010_4050_Final.xls") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "areaname", "safmr0br" = "fmr0",
         "safmr1br" = "fmr1", "safmr2br" = "fmr2", 
         "safmr3br" = "fmr3", "safmr4br" = "fmr4") %>%
  select(msa, countyname, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2010)

fmr_fy11 <- read_xls("RawData/HUD/FY2011_4050_Final.xls") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "areaname", "safmr0br" = "fmr0",
         "safmr1br" = "fmr1", "safmr2br" = "fmr2", 
         "safmr3br" = "fmr3", "safmr4br" = "fmr4") %>%
  select(msa, countyname, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2011)

fmr_data_0311 <- bind_rows(fmr_fy05, fmr_fy06, fmr_fy07, fmr_fy08,
                           fmr_fy09, fmr_fy10, fmr_fy11)

rm(fmr_fy05, fmr_fy06, fmr_fy07, fmr_fy08,
   fmr_fy09, fmr_fy10, fmr_fy11)

fmr_fy12 <- read_xls("RawData/HUD/small_area_fmrs_fy2012.xls") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "cbsa name", "countyname" = "county name", 
         "safmr0br" = "area_rent_br0", "safmr1br" = "area_rent_br1", 
         "safmr2br" = "area_rent_br2", "safmr3br" = "area_rent_br3", 
         "safmr4br" = "area_rent_br4") %>%
  select(zip, countyname, msa, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2012)

fmr_fy13 <- read_xls("RawData/HUD/small_area_fmrs_fy2013.xls") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "cbsa name", "countyname" = "county name", "safmr0br" = "area_rent_br0",
         "safmr1br" = "area_rent_br1", "safmr2br" = "area_rent_br2", 
         "safmr3br" = "area_rent_br3", "safmr4br" = "area_rent_br4") %>%
  select(zip, msa, countyname, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2013)

fmr_fy14 <- read_xls("RawData/HUD/small_area_fmrs_fy2014.xls") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "cbsa name", "countyname" = "county name", "safmr0br" = "area_rent_br0",
         "safmr1br" = "area_rent_br1", "safmr2br" = "area_rent_br2", 
         "safmr3br" = "area_rent_br3", "safmr4br" = "area_rent_br4") %>%
  select(zip, msa, countyname, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2014)

fmr_fy15 <- read_xls("RawData/HUD/small_area_fmrs_fy2015f.xls") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "cbnsmcnm", "zip" = "zipcode", "countyname" = "cntyname",
         "safmr0br" = "area_rent_br0","safmr1br" = "area_rent_br1", 
         "safmr2br" = "area_rent_br2", "safmr3br" = "area_rent_br3", 
         "safmr4br" = "area_rent_br4") %>%
  select(zip, msa, countyname, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2015)

fmr_fy16 <- read_xlsx("RawData/HUD/final_fy2016_hypothetical_safmrs.xlsx") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "metro_name", "zip" = "zip_code", "countyname" = "county_name",
         "safmr0br" = "area_rent_br0",
         "safmr1br" = "area_rent_br1", "safmr2br" = "area_rent_br2", 
         "safmr3br" = "area_rent_br3", "safmr4br" = "area_rent_br4") %>%
  select(zip, msa, countyname, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2016)

fmr_fy17 <- read_xlsx("RawData/HUD/FY2017_hypothetical_safmrs.xlsx") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "metro_name", "zip" = "zip_code", "safmr0br" = "area_rent_br0",
         "safmr1br" = "area_rent_br1", "safmr2br" = "area_rent_br2", 
         "safmr3br" = "area_rent_br3", "safmr4br" = "area_rent_br4") %>%
  select(zip, msa, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2017)

fmr_fy18 <- read_xlsx("RawData/HUD/fy2018_advisory_safmrs_revised_feb_2018.xlsx") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "hud metro fair market rent area name", "zip" = "zipcode") %>%
  select(zip, msa, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2018)

fmr_fy19 <- read_xlsx("RawData/HUD/fy2019_safmrs_rev.xlsx") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "hud metro fair market rent area name", "zip" = "zipcode") %>%
  select(zip, msa, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2019)

fmr_fy20 <- read_xlsx("RawData/HUD/fy2020_safmrs_rev.xlsx") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "areaname20", "zip" = "zcta", "safmr0br" = "safmr_0br",
         "safmr1br" = "safmr_1br", "safmr2br" = "safmr_2br", 
         "safmr3br" = "safmr_3br", "safmr4br" = "safmr_4br") %>%
  select(zip, msa, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2020)

fmr_fy21 <- read_xlsx("RawData/HUD/fy2021_safmrs_revised.xlsx") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "hud metro fair market rent area name", "zip" = "zipcode") %>%
  select(zip, msa, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2021)

fmr_fy22 <- read_xlsx("RawData/HUD/fy2022_safmrs.xlsx") %>%
  rename_with(~ tolower(gsub("[\r\n]", "", .x))) %>%
  rename("msa" = "hud metro fair market rent area name", "zip" = "zipcode") %>%
  select(zip, msa, safmr0br, safmr1br, safmr2br, safmr3br, safmr4br) %>%
  mutate(safmr0br = as.numeric(as.character(safmr0br)),
         safmr1br = as.numeric(as.character(safmr1br)),
         safmr2br = as.numeric(as.character(safmr2br)),
         safmr3br = as.numeric(as.character(safmr3br)),
         safmr4br = as.numeric(as.character(safmr4br)),
         fiscal_year = 2022)

fmr_data_1222 <- bind_rows(fmr_fy12, fmr_fy13, fmr_fy14, fmr_fy15,
                           fmr_fy16, fmr_fy17, fmr_fy18, fmr_fy19,
                           fmr_fy20, fmr_fy21, fmr_fy22)

rm(fmr_fy12, fmr_fy13, fmr_fy14, fmr_fy15,
   fmr_fy16, fmr_fy17, fmr_fy18, fmr_fy19,
   fmr_fy20, fmr_fy21, fmr_fy22)


############
###APPEND###
############
## using the 2012-2022 data, create a list of the unique zip-county
## combinatinos
zip_code_list <- fmr_data_1222 %>%
  select(zip, countyname) %>%
  filter(!is.na(countyname)) %>%
  arrange(zip, countyname) %>%
  distinct(zip, .keep_all = TRUE)

## merge the zip code list that you just created to the 2003-2011 data
fmr_data_0311_zips <- fmr_data_0311 %>%
  right_join(zip_code_list, by = "countyname") %>%
  filter(!is.na(fiscal_year)) %>%
  distinct(zip, fiscal_year, .keep_all = TRUE)

## remerge the zip code list after renaming the county name. This is
## to avoid cases in which counties change names over time.
fmr_data_1222_zips <- fmr_data_1222 %>%
  rename("countyname_old" = "countyname") %>%
  left_join(zip_code_list, by = "zip") %>%
  filter(!is.na(fiscal_year)) %>%
  distinct(zip, fiscal_year, .keep_all = TRUE)

## cleanup
rm(zip_code_list)

## append
fmr_data <- bind_rows(fmr_data_0311_zips, fmr_data_1222_zips)

## export
write_csv(fmr_data, "IntermediateData/HUD/Fair Market Housing Data.csv")






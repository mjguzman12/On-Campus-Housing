###########################
###########################
#####IPEDS DATA IMPORT#####
###########################
###########################

rm(list = ls())

library("readr")
library("stringr")
library("dplyr")
library("tidyr")

###########
###INTRO###
###########
## This script imports the raw IPEDS data. Here, we are importing several
## types of tables: 
## HD (Documentation)
## IC (Instutitional Characteristics)
## GR (Graduation Rate)
## EF A (Enrollment Data)
## EF D (Retention Data)
## F_F1A (Public School Finances)
## F_F2 (Private School Finances)
## F_F3 (For Profit School Finances)

## We are importing each table for AY2003-AY2020. AY stands for academic
## year where the academic year starts in August of each year.

## In cases where a revised version of the data was produced later, we will
## use the revised version. Also, at the very end, we merge the relevant 
## tables.

###############
###HD IMPORT###
###############
## Import university documentation. This dataset provides the institution
## name, state, zip code, and ic level, or level of the institution. Notably,
## iclevel == 4+ years, iclevel == 2 is at least 2 but less than 4 years,
## and iclevel == 3 is less than 2 years. Missing iclevel is unknown.
hd_import_function <- function(name, ay_yr) {
  data <- read_csv(paste0("RawData/IPEDS/data unzipped/", ay_yr, "/HD", ay_yr, "/", name)) %>%
    rename_all(tolower) %>%
    select(unitid, instnm, stabbr, zip, iclevel) %>%
    mutate(instnm = str_to_title(instnm), academic_year = ay_yr) %>%
    filter(iclevel == 1)
  
  return(data)
}

hd_2003 <- hd_import_function("hd2003.csv", 2003)
hd_2004 <- hd_import_function("hd2004.csv", 2004)
hd_2005 <- hd_import_function("hd2005.csv", 2005)
hd_2006 <- hd_import_function("hd2006.csv", 2006)
hd_2007 <- hd_import_function("hd2007.csv", 2007)
hd_2008 <- hd_import_function("hd2008.csv", 2008)
hd_2009 <- hd_import_function("hd2009.csv", 2009)
hd_2010 <- hd_import_function("hd2010.csv", 2010)
hd_2011 <- hd_import_function("hd2011.csv", 2011)
hd_2012 <- hd_import_function("hd2012.csv", 2012)
hd_2013 <- hd_import_function("hd2013.csv", 2013)
hd_2014 <- hd_import_function("hd2014.csv", 2014)
hd_2015 <- hd_import_function("hd2015.csv", 2015)
hd_2016 <- hd_import_function("hd2016.csv", 2016)
hd_2017 <- hd_import_function("hd2017.csv", 2017)
hd_2018 <- hd_import_function("hd2018.csv", 2018)
hd_2019 <- hd_import_function("hd2019.csv", 2019)
hd_2020 <- hd_import_function("hd2020.csv", 2020)

hd_data <- bind_rows(hd_2003, hd_2004, hd_2005, hd_2006, hd_2007,
                     hd_2008, hd_2009, hd_2010, hd_2011, hd_2012,
                     hd_2013, hd_2014, hd_2015, hd_2016, hd_2017,
                     hd_2018, hd_2019, hd_2020)

rm(hd_2003, hd_2004, hd_2005, hd_2006, hd_2007,
   hd_2008, hd_2009, hd_2010, hd_2011, hd_2012,
   hd_2013, hd_2014, hd_2015, hd_2016, hd_2017,
   hd_2018, hd_2019, hd_2020)


###############
###IC IMPORT###
###############
## Import the institutional characteristics for each university. With this.
## also import the ft_ug (indicator: full time undergraduate students are
## enrolled), room (indicator: institution provides on-campus housing), 
## roomcap (total dorm capacity), roomamt (typical room charge for academic
## year), boardamt (typical board charge for academic year), and rmbrdamt 
## (typical combined room and board charge for academic year)
ic_import_function <- function(name, ay_yr) {
  data <- read_csv(paste0("RawData/IPEDS/data unzipped/", ay_yr, "/IC", ay_yr, "/", name)) %>%
    rename_all(tolower) %>%
    select(unitid, ft_ug, room, roomcap, roomamt, boardamt, rmbrdamt) %>%
    mutate(roomcap = as.numeric(as.character(roomcap)), 
           roomamt = as.numeric(as.character(roomamt)),
           boardamt = as.numeric(as.character(boardamt)),
           rmbrdamt = as.numeric(as.character(rmbrdamt)),
           academic_year = ay_yr)
  
  return(data)
}

ic_2003 <- ic_import_function("ic2003.csv", 2003)
ic_2004 <- ic_import_function("ic2004.csv", 2004)
ic_2005 <- ic_import_function("ic2005.csv", 2005)
ic_2006 <- ic_import_function("ic2006.csv", 2006)
ic_2007 <- ic_import_function("ic2007.csv", 2007)
ic_2008 <- ic_import_function("ic2008_rv.csv", 2008)
ic_2009 <- ic_import_function("ic2009_rv.csv", 2009)
ic_2010 <- ic_import_function("ic2010_rv.csv", 2010)
ic_2011 <- ic_import_function("ic2011_rv.csv", 2011)
ic_2012 <- ic_import_function("ic2012_rv.csv", 2012)
ic_2013 <- ic_import_function("ic2013_rv.csv", 2013)
ic_2014 <- ic_import_function("ic2014_rv.csv", 2014)
ic_2015 <- ic_import_function("ic2015_rv.csv", 2015)
ic_2016 <- ic_import_function("ic2016_rv.csv", 2016)
ic_2017 <- ic_import_function("ic2017_rv.csv", 2017)
ic_2018 <- ic_import_function("ic2018_rv.csv", 2018)
ic_2019 <- ic_import_function("ic2019_rv.csv", 2019)
ic_2020 <- ic_import_function("ic2020.csv", 2020)

ic_data <- bind_rows(ic_2003, ic_2004, ic_2005, ic_2006, ic_2007,
                     ic_2008, ic_2009, ic_2010, ic_2011, ic_2012,
                     ic_2013, ic_2014, ic_2015, ic_2016, ic_2017,
                     ic_2018, ic_2019, ic_2020)

rm(ic_2003, ic_2004, ic_2005, ic_2006, ic_2007,
   ic_2008, ic_2009, ic_2010, ic_2011, ic_2012,
   ic_2013, ic_2014, ic_2015, ic_2016, ic_2017,
   ic_2018, ic_2019, ic_2020)


###############
###GR IMPORT###
###############
## Import graduation rate data. These data report different numbers of
## graduates in long format. The type of value is reported in the "grtype"
## field, and the value for that time is provided in grrace24. Once these
## are imported, create the academic year variable. This is the cohort year
## for these graduation rates, or the year of the data minus 6. Then, keep
## grad types 8 and 9. Grad type 8 is the size of the 4-year adjusted cohort,
## and grad type 9 is the number of 4-year completers within 150% of normal
## time. Notably, grad type 13 provides the number of completers of bachelor's
## degrees within 4 years or less. This could be useful in the future.
gr_import_function_0307 <- function(name, ay_yr) {
  data <- read_csv(paste0("RawData/IPEDS/data unzipped/", ay_yr, "/GR", ay_yr, "/", name)) %>%
    rename_all(tolower) %>%
    select(unitid, grtype, grrace24) %>%
    rename("grtotlt" = "grrace24") %>%
    mutate(academic_year = ay_yr - 6) %>%
    filter(grtype %in% c(8, 9)) %>%
    pivot_wider(names_from = grtype, values_from = grtotlt, names_prefix = "grtype") %>%
    mutate(grad_rate_150 = grtype9/grtype8)
  
  return(data)
}

## Import graduation rate data. These data report different numbers of
## graduates in long format. The type of value is reported in the "grtype"
## field, and the value for that time is provided in grtotlt. Once these
## are imported, create the academic year variable. This is the cohort year
## for these graduation rates, or the year of the data minus 6. Then, keep
## grad types 8 and 9. Grad type 8 is the size of the 4-year adjusted cohort,
## and grad type 9 is the number of 4-year completers within 150% of normal
## time. Notably, grad type 13 provides the number of completers of bachelor's
## degrees within 4 years or less. This could be useful in the future.
gr_import_function_0820 <- function(name, ay_yr) {
  data <- read_csv(paste0("RawData/IPEDS/data unzipped/", ay_yr, "/GR", ay_yr, "/", name)) %>%
    rename_all(tolower) %>%
    select(unitid, grtype, grtotlt) %>%
    mutate(academic_year = ay_yr - 6) %>%
    filter(grtype %in% c(8, 9)) %>%
    pivot_wider(names_from = grtype, values_from = grtotlt, names_prefix = "grtype") %>%
    mutate(grad_rate_150 = grtype9/grtype8)
  
  return(data)
}

gr_2003 <- gr_import_function_0307("gr2003.csv", 2003)
gr_2004 <- gr_import_function_0307("gr2004_rv.csv", 2004)
gr_2005 <- gr_import_function_0307("gr2005_rv.csv", 2005)
gr_2006 <- gr_import_function_0307("gr2006_rv.csv", 2006)
gr_2007 <- gr_import_function_0307("gr2007_rv.csv", 2007)
gr_2008 <- gr_import_function_0820("gr2008_rv.csv", 2008)
gr_2009 <- gr_import_function_0820("gr2009_rv.csv", 2009)
gr_2010 <- gr_import_function_0820("gr2010_rv.csv", 2010)
gr_2011 <- gr_import_function_0820("gr2011_rv.csv", 2011)
gr_2012 <- gr_import_function_0820("gr2012_rv.csv", 2012)
gr_2013 <- gr_import_function_0820("gr2013_rv.csv", 2013)
gr_2014 <- gr_import_function_0820("gr2014_rv.csv", 2014)
gr_2015 <- gr_import_function_0820("gr2015_rv.csv", 2015)
gr_2016 <- gr_import_function_0820("gr2016_rv.csv", 2016)
gr_2017 <- gr_import_function_0820("gr2017_rv.csv", 2017)
gr_2018 <- gr_import_function_0820("gr2018_rv.csv", 2018)
gr_2019 <- gr_import_function_0820("gr2019_rv.csv", 2019)
gr_2020 <- gr_import_function_0820("gr2020.csv", 2020)

gr_data <- bind_rows(gr_2003, gr_2004, gr_2005, gr_2006, gr_2007,
                     gr_2008, gr_2009, gr_2010, gr_2011, gr_2012,
                     gr_2013, gr_2014, gr_2015, gr_2016, gr_2017,
                     gr_2018, gr_2019, gr_2020)

rm(gr_2003, gr_2004, gr_2005, gr_2006, gr_2007,
   gr_2008, gr_2009, gr_2010, gr_2011, gr_2012,
   gr_2013, gr_2014, gr_2015, gr_2016, gr_2017,
   gr_2018, gr_2019, gr_2020)


#################
###EF A IMPORT###
##################
## Import enrollment data for all institutions. Similar to the graduation rate
## data, each row is a category of students. EFALEVEL == 2 corresponds to
## "All students, Undergraduate total", so we'll use that since we're focusing
## on undergrads. Also, as you may have anticipated, EFRace24 is the value for
## each category in efalevel
efa_import_function_0307 <- function(name, ay_yr) {
  data <- read_csv(paste0("RawData/IPEDS/data unzipped/", ay_yr, "/EF", ay_yr, "A/", name)) %>%
    rename_all(tolower) %>%
    select(unitid, efalevel, efrace24) %>%
    rename("eftotlt" = "efrace24") %>%
    mutate(academic_year = ay_yr) %>%
    filter(efalevel %in% c(2)) %>%
    pivot_wider(names_from = efalevel, values_from = eftotlt, names_prefix = "efalevel")
  
  return(data)
}

## Import enrollment data for all institutions. Similar to the graduation rate
## data, each row is a category of students. EFALEVEL == 2 corresponds to
## "All students, Undergraduate total", so we'll use that since we're focusing
## on undergrads. Also, as you may have anticipated, EFTotLt is the value for
## each category in efalevel
efa_import_function_0820 <- function(name, ay_yr) {
  data <- read_csv(paste0("RawData/IPEDS/data unzipped/", ay_yr, "/EF", ay_yr, "A/", name)) %>%
    rename_all(tolower) %>%
    select(unitid, efalevel, eftotlt) %>%
    mutate(academic_year = ay_yr) %>%
    filter(efalevel %in% c(1, 2)) %>%
    pivot_wider(names_from = efalevel, values_from = eftotlt, names_prefix = "efalevel")
  
  return(data)
}

efa_2003 <- efa_import_function_0307("ef2003a.csv", 2003)
efa_2004 <- efa_import_function_0307("ef2004a_rv.csv", 2004)
efa_2005 <- efa_import_function_0307("ef2005a_rv.csv", 2005)
efa_2006 <- efa_import_function_0307("ef2006a_rv.csv", 2006)
efa_2007 <- efa_import_function_0307("ef2007a_rv.csv", 2007)
efa_2008 <- efa_import_function_0820("ef2008a_rv.csv", 2008)
efa_2009 <- efa_import_function_0820("ef2009a_rv.csv", 2009)
efa_2010 <- efa_import_function_0820("ef2010a_rv.csv", 2010)
efa_2011 <- efa_import_function_0820("ef2011a_rv.csv", 2011)
efa_2012 <- efa_import_function_0820("ef2012a_rv.csv", 2012)
efa_2013 <- efa_import_function_0820("ef2013a_rv.csv", 2013)
efa_2014 <- efa_import_function_0820("ef2014a_rv.csv", 2014)
efa_2015 <- efa_import_function_0820("ef2015a_rv.csv", 2015)
efa_2016 <- efa_import_function_0820("ef2016a_rv.csv", 2016)
efa_2017 <- efa_import_function_0820("ef2017a_rv.csv", 2017)
efa_2018 <- efa_import_function_0820("ef2018a_rv.csv", 2018)
efa_2019 <- efa_import_function_0820("ef2019a.csv", 2019)
efa_2020 <- efa_import_function_0820("ef2020a.csv", 2020)

efa_data <- bind_rows(efa_2003, efa_2004, efa_2005, efa_2006, efa_2007,
                     efa_2008, efa_2009, efa_2010, efa_2011, efa_2012,
                     efa_2013, efa_2014, efa_2015, efa_2016, efa_2017,
                     efa_2018, efa_2019, efa_2020)

rm(efa_2003, efa_2004, efa_2005, efa_2006, efa_2007,
   efa_2008, efa_2009, efa_2010, efa_2011, efa_2012,
   efa_2013, efa_2014, efa_2015, efa_2016, efa_2017,
   efa_2018, efa_2019, efa_2020)


#################
###EF D IMPORT###
##################
## Import the retention rate data for each university. Here, we really just
## want the retention rate as an alternative outcome variable. ret_pcf is
## the full-time retention rate from the previous year's cohort, and
## this is computed each year for the previous year
efd_import_function <- function(name, ay_yr) {
  data <- read_csv(paste0("RawData/IPEDS/data unzipped/", ay_yr, "/EF", ay_yr, "D/", name)) %>%
    rename_all(tolower) %>%
    select(unitid, ret_pcf) %>%
    mutate(academic_year = ay_yr - 1, ret_pcf = ret_pcf/100) %>%
    select(unitid, academic_year, ret_pcf)
  
  return(data)
}

efd_2003 <- efd_import_function("ef2003d.csv", 2003)
efd_2004 <- efd_import_function("ef2004d.csv", 2004)
efd_2005 <- efd_import_function("ef2005d.csv", 2005)
efd_2006 <- efd_import_function("ef2006d.csv", 2006)
efd_2007 <- efd_import_function("ef2007d.csv", 2007)
efd_2008 <- efd_import_function("ef2008d.csv", 2008)
efd_2009 <- efd_import_function("ef2009d.csv", 2009)
efd_2010 <- efd_import_function("ef2010d.csv", 2010)
efd_2011 <- efd_import_function("ef2011d.csv", 2011)
efd_2012 <- efd_import_function("ef2012d.csv", 2012)
efd_2013 <- efd_import_function("ef2013d.csv", 2013)
efd_2014 <- efd_import_function("ef2014d.csv", 2014)
efd_2015 <- efd_import_function("ef2015d.csv", 2015)
efd_2016 <- efd_import_function("ef2016d.csv", 2016)
efd_2017 <- efd_import_function("ef2017d.csv", 2017)
efd_2018 <- efd_import_function("ef2018d.csv", 2018)
efd_2019 <- efd_import_function("ef2019d.csv", 2019)
efd_2020 <- efd_import_function("ef2020d.csv", 2020)

efd_data <- bind_rows(efd_2003, efd_2004, efd_2005, efd_2006, efd_2007,
                      efd_2008, efd_2009, efd_2010, efd_2011, efd_2012,
                      efd_2013, efd_2014, efd_2015, efd_2016, efd_2017,
                      efd_2018, efd_2019, efd_2020)

rm(efd_2003, efd_2004, efd_2005, efd_2006, efd_2007,
   efd_2008, efd_2009, efd_2010, efd_2011, efd_2012,
   efd_2013, efd_2014, efd_2015, efd_2016, efd_2017,
   efd_2018, efd_2019, efd_2020)


##################
###F_F1A IMPORT###
##################
## Import public school finance data. These variable names are just labels
## so the renaming in the first step is critical to keep track of things.
## Notably, these data are for the fiscal year, so I set the academic year
## equal to the later year in the fiscal year. I'm not entirely certain that
## that's the best way to connect these data, but it seems like this is a
## reasonable assumption. It is also possible that fiscal years vary by
## universities.
ff1a_import_function1 <- function(name, fy_yr, year) {
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
  data <- read_csv(paste0("RawData/IPEDS/data unzipped/", fy_yr, "/F", year_final, "_F1A/", name)) %>%
    rename_all(tolower) %>%
    select(unitid, f1c011, f1c021, f1c031, f1c051, f1c061, 
           f1c071, f1c081, f1c091, f1c101, f1c111, f1c121, f1c131,
           f1c141, f1c181, f1c191) %>%
    rename("instruction_expenses_pub" = "f1c011",
           "research_expenses_pub" = "f1c021",
           "public_service_expenses_pub" = "f1c031",
           "academic_support_expenses_pub" = "f1c051", 
           "student_services_expenses_pub" = "f1c061",
           "institutional_expenses_pub" = "f1c071",
           "operation_expenses_pub" = "f1c081",
           "depreciation_expenses_pub" = "f1c091",
           "scholarship_expenses_pub" = "f1c101",
           "auxiliary_expenses_pub" = "f1c111", 
           "hospital_expenses_pub" = "f1c121",
           "independent_expenses_pub" = "f1c131",
           "other_expenses_pub" = "f1c141",
           "non_operating_expenses_pub" = "f1c181",
           "total_expenses_pub" = "f1c191") %>%
    mutate(academic_year = fy_yr, school_type_pub = "public") %>%
    select(unitid, academic_year, school_type_pub,
           instruction_expenses_pub, research_expenses_pub,
           public_service_expenses_pub, academic_support_expenses_pub,
           student_services_expenses_pub, institutional_expenses_pub,
           scholarship_expenses_pub,
           auxiliary_expenses_pub, hospital_expenses_pub,
           independent_expenses_pub, other_expenses_pub) %>%
    mutate(total_expenses_pub = instruction_expenses_pub + research_expenses_pub +
           public_service_expenses_pub + academic_support_expenses_pub +
           student_services_expenses_pub + institutional_expenses_pub +
           scholarship_expenses_pub + auxiliary_expenses_pub + 
           hospital_expenses_pub + independent_expenses_pub + 
           other_expenses_pub)
  
  return(data)
}

ff1a_import_function2 <- function(name, fy_yr, year) {
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
  
  data <- read_csv(paste0("RawData/IPEDS/data unzipped/", fy_yr, "/F", year_final, "_F1A/", name)) %>%
    rename_all(tolower) %>%
    select(unitid, f1c011, f1c021, f1c031, f1c051, f1c061, 
           f1c071, f1c101, f1c111, f1c121, f1c131,
           f1c141, f1c191) %>%
    rename("instruction_expenses_pub" = "f1c011",
           "research_expenses_pub" = "f1c021",
           "public_service_expenses_pub" = "f1c031",
           "academic_support_expenses_pub" = "f1c051", 
           "student_services_expenses_pub" = "f1c061",
           "institutional_expenses_pub" = "f1c071",
           "scholarship_expenses_pub" = "f1c101",
           "auxiliary_expenses_pub" = "f1c111", 
           "hospital_expenses_pub" = "f1c121",
           "independent_expenses_pub" = "f1c131",
           "other_expenses_pub" = "f1c141",
           "total_expenses_pub" = "f1c191") %>%
    mutate(academic_year = fy_yr, school_type_pub = "public") %>%
    select(unitid, academic_year, school_type_pub,
           instruction_expenses_pub, research_expenses_pub,
           public_service_expenses_pub, academic_support_expenses_pub,
           student_services_expenses_pub, institutional_expenses_pub,
           scholarship_expenses_pub,
           auxiliary_expenses_pub, hospital_expenses_pub,
           independent_expenses_pub, other_expenses_pub, 
           total_expenses_pub)
  
  return(data)
}

ff1a_2003 <- ff1a_import_function1("f0203_f1a.csv", 2003, 2)
ff1a_2004 <- ff1a_import_function1("f0304_f1a_rv.csv", 2004, 3)
ff1a_2005 <- ff1a_import_function1("f0405_f1a_rv.csv", 2005, 4)
ff1a_2006 <- ff1a_import_function1("f0506_f1a_rv.csv", 2006, 5)
ff1a_2007 <- ff1a_import_function1("f0607_f1a_rv.csv", 2007, 6)
ff1a_2008 <- ff1a_import_function1("f0708_f1a_rv.csv", 2008, 7)
ff1a_2009 <- ff1a_import_function1("f0809_f1a_rv.csv", 2009, 8)
ff1a_2010 <- ff1a_import_function2("f0910_f1a_rv.csv", 2010, 9)
ff1a_2011 <- ff1a_import_function2("f1011_f1a_rv.csv", 2011, 10)
ff1a_2012 <- ff1a_import_function2("f1112_f1a_rv.csv", 2012, 11)
ff1a_2013 <- ff1a_import_function2("f1213_f1a_rv.csv", 2013, 12)
ff1a_2014 <- ff1a_import_function2("f1314_f1a_rv.csv", 2014, 13)
ff1a_2015 <- ff1a_import_function2("f1415_f1a_rv.csv", 2015, 14)
ff1a_2016 <- ff1a_import_function2("f1516_f1a_rv.csv", 2016, 15)
ff1a_2017 <- ff1a_import_function2("f1617_f1a_rv.csv", 2017, 16)
ff1a_2018 <- ff1a_import_function2("f1718_f1a_rv.csv", 2018, 17)
ff1a_2019 <- ff1a_import_function2("f1819_f1a.csv", 2019, 18)
ff1a_2020 <- ff1a_import_function2("f1920_f1a.csv", 2020, 19)

ff1a_data <- bind_rows(ff1a_2003, ff1a_2004, ff1a_2005, ff1a_2006,
                       ff1a_2007, ff1a_2008, ff1a_2009, ff1a_2010,
                       ff1a_2011, ff1a_2012, ff1a_2013, ff1a_2014,
                       ff1a_2015, ff1a_2016, ff1a_2017, ff1a_2018,
                       ff1a_2019, ff1a_2020)

rm(ff1a_2003, ff1a_2004, ff1a_2005, ff1a_2006,
   ff1a_2007, ff1a_2008, ff1a_2009, ff1a_2010,
   ff1a_2011, ff1a_2012, ff1a_2013, ff1a_2014,
   ff1a_2015, ff1a_2016, ff1a_2017, ff1a_2018,
   ff1a_2019, ff1a_2020)


#################
###F_F2 IMPORT###
#################
## Import private school finance data. This is very similar to the public
## school finance data, although the private schools include a field for 
## net_grant_aid instead of scholarship. As before, These variable names
## are just labels so the renaming in the first step is critical to keep
## track of things. Also as before, these data are for the fiscal year,
## so I set the academic year equal to the later year in the fiscal year. 
ff2_import_function <- function(name, fy_yr, year) {
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
  
  data <- read_csv(paste0("RawData/IPEDS/data unzipped/", fy_yr, "/F", year_final, "_F2/", name)) %>%
    rename_all(tolower) %>%
    select(unitid, f2e011, f2e021, f2e031, f2e041, f2e051, f2e061, 
           f2e071, f2e081, f2e091, f2e101, f2e121, f2e131, f2b02) %>%
    rename("instruction_expenses_priv" = "f2e011",
           "research_expenses_priv" = "f2e021",
           "public_service_expenses_priv" = "f2e031",
           "academic_support_expenses_priv" = "f2e041", 
           "student_services_expenses_priv" = "f2e051",
           "institutional_expenses_priv" = "f2e061",
           "auxiliary_expenses_priv" = "f2e071", 
           "net_grant_aid_expenses_priv" = "f2e081", 
           "hospital_expenses_priv" = "f2e091",
           "independent_expenses_priv" = "f2e101",
           "other_expenses_priv" = "f2e121",
           "total_expenses_priv" = "f2e131") %>%
    mutate(academic_year = fy_yr, school_type_priv = "private") %>%
    select(unitid, academic_year, school_type_priv,
           instruction_expenses_priv, research_expenses_priv,
           public_service_expenses_priv, academic_support_expenses_priv,
           student_services_expenses_priv, institutional_expenses_priv,
           auxiliary_expenses_priv, net_grant_aid_expenses_priv,
           hospital_expenses_priv, independent_expenses_priv, 
           other_expenses_priv, total_expenses_priv)
  
  return(data)
}

ff2_2003 <- ff2_import_function("f0203_f2.csv", 2003, 2)
ff2_2004 <- ff2_import_function("f0304_f2_rv.csv", 2004, 3)
ff2_2005 <- ff2_import_function("f0405_f2_rv.csv", 2005, 4)
ff2_2006 <- ff2_import_function("f0506_f2_rv.csv", 2006, 5)
ff2_2007 <- ff2_import_function("f0607_f2_rv.csv", 2007, 6)
ff2_2008 <- ff2_import_function("f0708_f2_rv.csv", 2008, 7)
ff2_2009 <- ff2_import_function("f0809_f2_rv.csv", 2009, 8)
ff2_2010 <- ff2_import_function("f0910_f2_rv.csv", 2010, 9)
ff2_2011 <- ff2_import_function("f1011_f2_rv.csv", 2011, 10)
ff2_2012 <- ff2_import_function("f1112_f2_rv.csv", 2012, 11)
ff2_2013 <- ff2_import_function("f1213_f2_rv.csv", 2013, 12)
ff2_2014 <- ff2_import_function("f1314_f2_rv.csv", 2014, 13)
ff2_2015 <- ff2_import_function("f1415_f2_rv.csv", 2015, 14)
ff2_2016 <- ff2_import_function("f1516_f2_rv.csv", 2016, 15)
ff2_2017 <- ff2_import_function("f1617_f2_rv.csv", 2017, 16)
ff2_2018 <- ff2_import_function("f1718_f2_rv.csv", 2018, 17)
ff2_2019 <- ff2_import_function("f1819_f2_rv.csv", 2019, 18)
ff2_2020 <- ff2_import_function("f1920_f2.csv", 2020, 19)

ff2_data <- bind_rows(ff2_2003, ff2_2004, ff2_2005, ff2_2006, ff2_2007,
                      ff2_2008, ff2_2009, ff2_2010, ff2_2011, ff2_2012,
                      ff2_2013, ff2_2014, ff2_2015, ff2_2016, ff2_2017,
                      ff2_2018, ff2_2019, ff2_2020)

rm(ff2_2003, ff2_2004, ff2_2005, ff2_2006, ff2_2007,
   ff2_2008, ff2_2009, ff2_2010, ff2_2011, ff2_2012,
   ff2_2013, ff2_2014, ff2_2015, ff2_2016, ff2_2017,
   ff2_2018, ff2_2019, ff2_2020)


#################
###F_F3 IMPORT###
#################
## Finally, import the for profit school finance data. I haven't done anything
## with these data yet and I don't plan to at this point.
ff3_import_function1 <- function(name, fy_yr, year) {
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
  
  data <- read_csv(paste0("RawData/IPEDS/data unzipped/", fy_yr, "/F", year_final, "_F3/", name)) %>%
    rename_all(tolower) %>%
    select(unitid, f3e03, f3b02) %>%
    rename("total_expenses_fp" = "f3b02", "student_services_spending_fp" = "f3e03") %>%
    mutate(academic_year = fy_yr) %>%
    select(unitid, academic_year, student_services_spending_fp, total_expenses_fp)
  
  return(data)
}

ff3_import_function2 <- function(name, fy_yr, year) {
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
  
  data <- read_csv(paste0("RawData/IPEDS/data unzipped/", fy_yr, "/F", year_final, "_F3/", name)) %>%
    rename_all(tolower) %>%
    select(unitid, f3b02, f3e03b1) %>%
    rename("total_expenses_fp" = "f3b02", "student_services_spending_fp" = "f3e03b1") %>%
    mutate(academic_year = fy_yr) %>%
    select(unitid, academic_year, student_services_spending_fp, total_expenses_fp)
  
  return(data)
}

ff3_2003 <- ff3_import_function1("f0203_f3.csv", 2003, 2)
ff3_2004 <- ff3_import_function1("f0304_f3_rv.csv", 2004, 3)
ff3_2005 <- ff3_import_function1("f0405_f3_rv.csv", 2005, 4)
ff3_2006 <- ff3_import_function1("f0506_f3_rv.csv", 2006, 5)
ff3_2007 <- ff3_import_function1("f0607_f3_rv.csv", 2007, 6)
ff3_2008 <- ff3_import_function1("f0708_f3_rv.csv", 2008, 7)
ff3_2009 <- ff3_import_function1("f0809_f3_rv.csv", 2009, 8)
ff3_2010 <- ff3_import_function1("f0910_f3_rv.csv", 2010, 9)
ff3_2011 <- ff3_import_function1("f1011_f3_rv.csv", 2011, 10)
ff3_2012 <- ff3_import_function1("f1112_f3_rv.csv", 2012, 11)
ff3_2013 <- ff3_import_function1("f1213_f3_rv.csv", 2013, 12)
ff3_2014 <- ff3_import_function2("f1314_f3_rv.csv", 2014, 13)
ff3_2015 <- ff3_import_function2("f1415_f3_rv.csv", 2015, 14)
ff3_2016 <- ff3_import_function2("f1516_f3_rv.csv", 2016, 15)
ff3_2017 <- ff3_import_function2("f1617_f3_rv.csv", 2017, 16)
ff3_2018 <- ff3_import_function2("f1718_f3_rv.csv", 2018, 17)
ff3_2019 <- ff3_import_function2("f1819_f3_rv.csv", 2019, 18)
ff3_2020 <- ff3_import_function2("f1920_f3.csv", 2020, 19)

ff3_data <- bind_rows(ff3_2003, ff3_2004, ff3_2005, ff3_2006, ff3_2007,
                      ff3_2008, ff3_2009, ff3_2010, ff3_2011, ff3_2012,
                      ff3_2013, ff3_2014, ff3_2015, ff3_2016, ff3_2017,
                      ff3_2018, ff3_2019, ff3_2020)

rm(ff3_2003, ff3_2004, ff3_2005, ff3_2006, ff3_2007,
   ff3_2008, ff3_2009, ff3_2010, ff3_2011, ff3_2012,
   ff3_2013, ff3_2014, ff3_2015, ff3_2016,
   ff3_2017, ff3_2018, ff3_2019, ff3_2020)


###########
###MERGE###
###########
## Merge the data by the unitid. Note that the hd and ic data are unique
## by unitid and year and the gr data are unique by unitid, year, and
## grtype. After merging, combine the finance variables using case_when
## statements, then drop vars that you don't need
merged_data <- hd_data %>%
  inner_join(ic_data, by = c("unitid", "academic_year")) %>%
  inner_join(efa_data, by = c("unitid", "academic_year")) %>%
  inner_join(efd_data, by = c("unitid", "academic_year")) %>%
  left_join(gr_data, by = c("unitid", "academic_year")) %>%
  left_join(ff1a_data, by = c("unitid", "academic_year")) %>%
  left_join(ff2_data, by = c("unitid", "academic_year")) %>%
  mutate(
    school_type = 
      case_when(!is.na(school_type_pub) ~ school_type_pub,
                !is.na(school_type_priv) ~ school_type_priv),
    instruction_expenses = 
      case_when(!is.na(instruction_expenses_pub) ~ instruction_expenses_pub,
                !is.na(instruction_expenses_priv) ~ instruction_expenses_priv),
    research_expenses = 
      case_when(!is.na(research_expenses_pub) ~ research_expenses_pub,
                !is.na(research_expenses_priv) ~ research_expenses_priv),
    public_service_expenses = 
      case_when(!is.na(public_service_expenses_pub) ~ public_service_expenses_pub,
                !is.na(public_service_expenses_priv) ~ public_service_expenses_priv),
    academic_support_expenses = 
      case_when(!is.na(academic_support_expenses_pub) ~ academic_support_expenses_pub,
                !is.na(academic_support_expenses_priv) ~ academic_support_expenses_priv),
    student_services_expenses = 
           case_when(!is.na(student_services_expenses_pub) ~ student_services_expenses_pub,
                     !is.na(student_services_expenses_priv) ~ student_services_expenses_priv),
    institutional_expenses = 
      case_when(!is.na(institutional_expenses_pub) ~ institutional_expenses_pub,
                !is.na(institutional_expenses_priv) ~ institutional_expenses_priv), 
    scholarship_expenses = 
      case_when(!is.na(scholarship_expenses_pub) ~ scholarship_expenses_pub,
                !is.na(net_grant_aid_expenses_priv) ~ net_grant_aid_expenses_priv),
    auxiliary_expenses = 
       case_when(!is.na(auxiliary_expenses_pub) ~ auxiliary_expenses_pub,
                 !is.na(auxiliary_expenses_priv) ~ auxiliary_expenses_priv),
    hospital_expenses = 
      case_when(!is.na(hospital_expenses_pub) ~ hospital_expenses_pub,
                !is.na(hospital_expenses_priv) ~ hospital_expenses_priv),
    independent_expenses = 
      case_when(!is.na(independent_expenses_pub) ~ independent_expenses_pub,
                !is.na(independent_expenses_priv) ~ independent_expenses_priv),
    other_expenses = 
      case_when(!is.na(other_expenses_pub) ~ other_expenses_pub,
                !is.na(other_expenses_priv) ~ other_expenses_priv),
    total_expenses = 
       case_when(!is.na(total_expenses_pub) ~ total_expenses_pub,
                 !is.na(total_expenses_priv) ~ total_expenses_priv)) %>%
  select(
    -school_type_pub, -school_type_priv,
    -instruction_expenses_pub, -instruction_expenses_priv,
    -research_expenses_pub, -research_expenses_priv,
    -public_service_expenses_pub, -public_service_expenses_priv,
    -academic_support_expenses_pub, -academic_support_expenses_priv,
    -student_services_expenses_pub, -student_services_expenses_priv,
    -institutional_expenses_pub, -institutional_expenses_priv,
    -scholarship_expenses_pub, -net_grant_aid_expenses_priv,
    -auxiliary_expenses_pub, -auxiliary_expenses_priv,
    -hospital_expenses_pub, -hospital_expenses_priv,
    -independent_expenses_pub, -independent_expenses_priv,
    -other_expenses_pub, -other_expenses_priv,
    -total_expenses_pub, -total_expenses_priv)

## only keep universities that enroll full-time undergrads, offer
## on-campus housing, and have a non-missing school-type
merged_data_filtered <- merged_data %>%
  filter(ft_ug == 1 & room == 1 & !is.na(school_type))

write_csv(merged_data_filtered, "IntermediateData/IPEDS/IPEDS Combined Data.csv")




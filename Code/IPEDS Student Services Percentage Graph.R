###########################################
###########################################
#####STUDENT SERVICES PERCENTAGE GRAPH#####
###########################################
###########################################

rm(list = ls())

library("readr")
library("dplyr")
library("tidyr")
library("scales")
library("ggplot2")

###########
###INTRO###
###########
## This script creates a simple line graph that is represents aggregated
## student services spending as a percentage of all university spending

############
###IMPORT###
############
ipeds_data <- read_csv("IntermediateData/IPEDS/IPEDS Combined Data.csv") %>%
  select(unitid, instnm, academic_year, school_type, instruction_expenses, 
         research_expenses, public_service_expenses, 
         academic_support_expenses, student_services_expenses, 
         institutional_expenses, scholarship_expenses,
         auxiliary_expenses, hospital_expenses, independent_expenses,
         other_expenses, total_expenses)  %>% 
  mutate(instruction_expenses = case_when(is.na(instruction_expenses) ~ 0,
                                          TRUE ~ as.numeric(instruction_expenses)),
         research_expenses = case_when(is.na(research_expenses) ~ 0,
                                          TRUE ~ as.numeric(research_expenses)),
         public_service_expenses = case_when(is.na(public_service_expenses) ~ 0,
                                          TRUE ~ as.numeric(public_service_expenses)),
         student_services_expenses = case_when(is.na(student_services_expenses) ~ 0,
                                          TRUE ~ as.numeric(student_services_expenses)),
         institutional_expenses = case_when(is.na(institutional_expenses) ~ 0,
                                          TRUE ~ as.numeric(institutional_expenses)),
         scholarship_expenses = case_when(is.na(scholarship_expenses) ~ 0,
                                          TRUE ~ as.numeric(scholarship_expenses)),
         auxiliary_expenses = case_when(is.na(auxiliary_expenses) ~ 0,
                                          TRUE ~ as.numeric(auxiliary_expenses)),
         hospital_expenses = case_when(is.na(hospital_expenses) ~ 0,
                                          TRUE ~ as.numeric(hospital_expenses)),
         independent_expenses = case_when(is.na(independent_expenses) ~ 0,
                                          TRUE ~ as.numeric(independent_expenses)),
         other_expenses = case_when(is.na(other_expenses) ~ 0,
                                          TRUE ~ as.numeric(other_expenses)),
         total_expenses = case_when(is.na(total_expenses) ~ 0,
                                          TRUE ~ as.numeric(total_expenses))) %>%
  filter(total_expenses != 0) %>%
  mutate(
    instruction_percentage = instruction_expenses/total_expenses,
    research_percentage = research_expenses/total_expenses,
    public_service_percentage = public_service_expenses/total_expenses,
    academic_support_percentage = academic_support_expenses/total_expenses,
    student_services_percentage = student_services_expenses/total_expenses,
    institutional_percentage = institutional_expenses/total_expenses,
    scholarship_percentage = scholarship_expenses/total_expenses,
    auxiliary_percentage = auxiliary_expenses/total_expenses,
    hospital_percentage = hospital_expenses/total_expenses,
    independent_percentage = independent_expenses/total_expenses,
    other_percentage = other_expenses/total_expenses,
    student_auxiliary_percentage = (student_services_expenses + auxiliary_expenses)/total_expenses) %>%
  group_by(academic_year) %>%
  summarise(
    instruction_expenses = sum(instruction_expenses), 
    research_expenses = sum(research_expenses),
    public_service_expenses = sum(public_service_expenses), 
    academic_support_expenses = sum(academic_support_expenses), 
    student_services_expenses = sum(student_services_expenses), 
    institutional_expenses = sum(institutional_expenses), 
    scholarship_expenses = sum(scholarship_expenses),
    auxiliary_expenses = sum(auxiliary_expenses), 
    hospital_expenses = sum(hospital_expenses), 
    independent_expenses = sum(independent_expenses),
    other_expenses = sum(other_expenses),
    total_expenses = sum(total_expenses),
    instruction_percentage = mean(instruction_percentage), 
    research_percentage = mean(research_percentage),
    public_service_percentage = mean(public_service_percentage), 
    student_services_percentage = mean(student_services_percentage), 
    institutional_percentage = mean(institutional_percentage), 
    scholarship_percentage = mean(scholarship_percentage),
    auxiliary_percentage = mean(auxiliary_percentage), 
    hospital_percentage = mean(hospital_percentage), 
    independent_percentage = mean(independent_percentage),
    other_percentage = mean(other_percentage),
    student_auxiliary_percentage = mean(student_auxiliary_percentage)) %>%
  ungroup()


##############
###GRAPH IT###
##############
student_expenses_percent_graph <- ggplot(data = ipeds_data, 
                                 aes(x = academic_year)) +
  geom_line(aes(y = student_auxiliary_percentage), color = "#2c7fb8", size = 2) +
  #geom_line(aes(y = student_services_percentage), color = "#2c7fb8", size = 2) +
  #geom_line(aes(y = auxiliary_percentage), color = "red", size = 2) +
  #geom_line(aes(y = hospital_percentage), color = "green", size = 2) +
  #geom_line(aes(y = instruction_percentage), color = "green", size = 2) +
  scale_x_continuous(breaks = seq(from = 2003, to = 2020, by = 2)) +
  scale_y_continuous(breaks = seq(from = 0.25, to = 0.27, by = 0.0025), labels = label_percent(accuracy = 0.01)) +
  theme(plot.title = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(axis.text.x = element_text(size = 18)) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_text(size = 18))

student_expenses_percent_graph
ggsave("Output/Student Services Percentage Graph.jpg",
       student_expenses_percent_graph, width = 14.11, height = 7)


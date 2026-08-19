####################################################################################################################################################################
#
#Assessing methodological differences in estimating exposure to intimate-partner violence against women in Brazil: a multi-source observational analysis
#
####################################################################################################################################################################

### SINAN dataset ### 
library(tidyverse)
library(dplyr)

viva <- read.csv("FILE_PATH/viva2019_complete data.csv")

#2019
viva <- subset(viva_new, year == "2019")

#2020 and 2021 (reference to year of data collection Vitoria study)
#viva <- subset(viva_new, year == "2020" | year == "2021")

#Select sex female and age above 16
vivaw <- subset(viva, sex_id == "2")
vivaw <- subset(vivaw, age >= 16)

#Select a specific STATE
#vivaw <- subset(vivaw, state == "32") #Espirito Santo

#Select a specific CAPITAL VITORIA
#vivaw<- subset(vivaw, clinic_municipality == 320530) 
#viva2019w_event <- subset(vivaw, event_municipality == 320530) 
#viva2019w_resident <- subset(vivaw, resident_municipality == 320530) 

#Select PELOTAS data
#vivaw<- subset(vivaw, clinic_municipality == 431440)

#New variable age groups
vivaw <- mutate(vivaw, age_cat = if_else(age >= 16 & age <= 29, "16-29",
                                         if_else(age >= 30 & age <= 39, "30-39",
                                                 if_else(age >= 40 & age <= 49, "40-49",
                                                         if_else(age >= 50 & age <= 59, "50-59",
                                                                 if_else(age >= 60 & age <= 99, "60-99", "NA"))))))

#New IPV variable: perpetrator spouse, ex_spouse, boyfriend_or_girlfriend, ex_boyfriend_or_ex_girlfriend
vivaw <- mutate(vivaw, ipv = if_else(spouse == "yes", 1,
                                     if_else(ex_spouse == "yes", 1,
                                             if_else(boyfriend_or_girlfriend == "yes", 1,
                                                     if_else(ex_boyfriend_or_ex_girlfriend == "yes", 1, 2)))))      

vivaw <- subset(vivaw, ipv == "1")

#Subset of variables
vivaw <- subset(vivaw, select = c(id, age_cat, physical, sexual))

#Number of notifications as yes for physical and sexual
vivaw <- subset(vivaw, physical == "yes" | sexual == "yes")

#####################################################################################################################
#DISTRIBUTION OF CASES
#####################################################################################################################

#Physical violence

sin_phys_distr <- subset(vivaw, select = c(id, age_cat, physical))
sin_phys_distr <- filter(sin_phys_distr, physical == "yes")

#Change "yes" category to 1 and "no" to 0
sin_phys_distr <- sin_phys_distr %>%
  mutate(physical = ifelse(physical == "yes", 1, 0))

sin_phys_distr <- sin_phys_distr %>%
  mutate(physical = as.numeric(physical))

#Remove NA values in age_cat
sin_phys_distr <- subset(sin_phys_distr, age_cat != "NA")

#Calculate distribution of cases by age groups
sin_phys_distr <- sin_phys_distr %>%
  group_by(age_cat) %>%
  summarise(
    physical_violence_cases = sum(physical, na.rm = TRUE), #Total number of cases in each group
    sample = n() # Total sample size for each group
  ) %>%
  ungroup() %>%
  mutate(
    total_cases = sum(physical_violence_cases), #Total cases across all age groups
    case_distribution = physical_violence_cases / total_cases #Proportion of cases in each age group
  )

sin_phys_distr <- sin_phys_distr %>%
  mutate(
    SE_distribution = sqrt((case_distribution * (1 - case_distribution)) / total_cases), #Standard error
    lower_ci_distribution = case_distribution - 1.96 * SE_distribution, #Lower bound
    upper_ci_distribution = case_distribution + 1.96 * SE_distribution  #Upper bound
  )

#New variable for source
sin_phys_distr <- mutate(sin_phys_distr, source = "2019 SINAN")

#New variable for source Vitoria
sin_phys_distr <- mutate(sin_phys_distr, source = "2020-2021 SINAN")


#Sexual violence
sin_sex_distr <- subset(vivaw, select = c(id, age_cat, sexual))
sin_sex_distr <- filter(sin_sex_distr, sexual == "yes")

#Change "yes" category to 1 and "no" to 0
sin_sex_distr <- sin_sex_distr %>%
  mutate(sexual = ifelse(sexual == "yes", 1, 0))

sin_sex_distr <- sin_sex_distr %>%
  mutate(sexual = as.numeric(sexual))

#Calculate distribution of cases by age groups
sin_sex_distr <- sin_sex_distr %>%
  group_by(age_cat) %>%
  summarise(
    sexual_violence_cases = sum(sexual, na.rm = TRUE), #Total number of cases in each group
    sample = n() # Total sample size for each group
  ) %>%
  ungroup() %>%
  mutate(
    total_cases = sum(sexual_violence_cases), #Total cases across all age groups
    case_distribution = sexual_violence_cases / total_cases #Proportion of cases in each age group
  )

sin_sex_distr <- sin_sex_distr %>%
  mutate(
    SE_distribution = sqrt((case_distribution * (1 - case_distribution)) / total_cases), #Standard error
    lower_ci_distribution = case_distribution - 1.96 * SE_distribution, #Lower bound
    upper_ci_distribution = case_distribution + 1.96 * SE_distribution  #Upper bound
  )

#New variable for source
sin_sex_distr <- mutate(sin_sex_distr, source = "2019 SINAN")

#Save distribution results Brasil
write.csv(sin_phys_distr, file = "FILE_PATH/sin_phys_distr.csv", row.names = FALSE)
write.csv(sin_sex_distr, file = "FILE_PATH/sin_sex_distr.csv", row.names = FALSE)

#Save distribution results Espirito Santo
write.csv(sin_phys_distr, file = "FILE_PATH/sin_es_phys_distr.csv", row.names = FALSE)
write.csv(sin_sex_distr, file = "FILE_PATH/sin_es_sex_distr.csv", row.names = FALSE)

#Save distribution results Vitoria
write.csv(sin_phys_distr, file = "FILE_PATH/sin_vit_phys_distr.csv", row.names = FALSE)

#Save distribution results Pelotas
write.csv(sin_phys_distr, file = "FILE_PATH/sin_pel_phys_distr.csv", row.names = FALSE)



####################################################################################################################################################################
#
#Assessing methodological differences in estimating exposure to intimate-partner violence against women in Brazil: a multi-source observational analysis
#
####################################################################################################################################################################

library(tidyverse)
library(dplyr)
library(haven)

######### Pelotas Cohort Study ######### 

pelotas <- read_dta("FILE_PATH/BRA_PELOTAS_2015_COHORT_2014_AND_2019_FOLLOW_UPS_INTERVIEW_SELECT_VARS_P003_23_C2015_V1_Y2023M03D30.DTA")

#Age variable: add 4 years (because of 4 years follow-up) to be the correct age when IPV questions were asked (2019)
pelotas_ <- mutate(pelotas, age = b_53 + 4)

#New variable age groups:  
pelotas_ <- mutate(pelotas_, age_cat = if_else(age <= 17, "17 ",                              
                                       if_else(age >= 18 & age <= 29, "18-29",                
                                       if_else(age >= 30 & age <= 39, "30-39",                
                                       if_else(age >= 40 & age <= 49, "40-49", "NA")))))  

table(pelotas_$age_cat)

#Exclude age_cat if NA
pelotas_ <- pelotas_[!(pelotas_$age_cat == "NA"), ]

#Exclude age_cat 17 because other data sources is upper 18)
pelotas_ <- pelotas_[!(pelotas_$age == "17"), ]

#Creating variable physical violence in the last 12 months #0 nÃ£o; 1 sim; 8 NSA
pelotas_$f_796_vpi5 <- as.numeric(pelotas_$f_796_vpi5)
pelotas_$f_797_vpi6 <- as.numeric(pelotas_$f_797_vpi6)
pelotas_$f_798_vpi7 <- as.numeric(pelotas_$f_798_vpi7)
pelotas_$f_799_vpi8 <- as.numeric(pelotas_$f_799_vpi8)
pelotas_$f_800_vpi9 <- as.numeric(pelotas_$f_800_vpi9)
pelotas_$f_801_vpi10 <- as.numeric(pelotas_$f_801_vpi10)

pel <- mutate(pel, ipv_phy_cases = if_else(f_796_vpi5 == 1 | f_797_vpi6 == 1 | f_798_vpi7 == 1 | f_799_vpi8 == 1 | f_800_vpi9 ==1 |f_801_vpi10 ==1, 1,
                                   if_else(f_796_vpi5 == 0 | f_797_vpi6 == 0 | f_798_vpi7 == 0 | f_799_vpi8 == 0 | f_800_vpi9 ==0 |f_801_vpi10 == 0,  0, 9)))

#Creating variable sexual violence in the last 12 months #0 nÃo; 1 sim; 8 NSA
pelotas_$f_802_vpi11 <- as.numeric(pelotas_$f_802_vpi11)
pelotas_$f_803_vpi12 <- as.numeric(pelotas_$f_803_vpi12)
pelotas_$f_804_vpi13 <- as.numeric(pelotas_$f_804_vpi13)

pel <- mutate(pel, ipv_sex_cases = if_else(f_802_vpi11 == 1 | f_803_vpi12 == 1 | f_804_vpi13 == 1,  1,
                                   if_else(f_802_vpi11 == 0 | f_803_vpi12 == 0 | f_804_vpi13 == 0, 0, 9)))

pel <- subset(pel, select = -c(age))

#####################################################################################################################
#Prevalence of IPV by age group
#####################################################################################################################

#Physical prevalence and CIs
pel_phys_prev <- pel %>%
  group_by(age_cat) %>%
  summarise(
    physical_violence_cases = sum(ipv_phy_cases, na.rm = TRUE),
    sample = n())

pel_phys_prev <- pel_phys_prev %>%
  mutate(
    prevalence = physical_violence_cases / sample,
    SE = sqrt((prevalence * (1 - prevalence)) / sample),
    lower_ci = prevalence - 1.96 * SE,
    upper_ci = prevalence + 1.96 * SE)

#Sexual prevalence and CIs
pel_sex_prev <- pel %>%
  group_by(age_cat) %>%
  summarise(
    sexual_violence_cases = sum(ipv_sex_cases, na.rm = TRUE),
    sample = n())

pel_sex_prev <- pel_sex_prev %>%
  mutate(
    prevalence = sexual_violence_cases / sample,
    SE = sqrt((prevalence * (1 - prevalence)) / sample),
    lower_ci = prevalence - 1.96 * SE,
    upper_ci = prevalence + 1.96 * SE)

#Save prevalence results
write.csv(pel_phys_prev, file = "FILE_PATH/pel_phys_prev.csv", row.names = FALSE)
write.csv(pel_sex_prev, file = "FILE_PATH/pel_sex_prev.csv", row.names = FALSE)

#####################################################################################################################
#AGE DISTRIBUTION OF CASES
#####################################################################################################################

#Physical violence
pel_phys_distr <- pel %>%
  group_by(age_cat) %>%
  summarise(
    physical_violence_cases = sum(ipv_phy_cases, na.rm = TRUE), #Total number of cases in each group
    sample = n() # Total sample size for each group
  ) %>%
  ungroup() %>%
  mutate(
    total_cases = sum(physical_violence_cases), #Total cases across all age groups
    case_distribution = physical_violence_cases / total_cases #Proportion of cases in each age group
  )

pel_phys_distr <- pel_phys_distr %>%
  mutate(
    SE_distribution = sqrt((case_distribution * (1 - case_distribution)) / total_cases), #Standard error
    lower_ci_distribution = case_distribution - 1.96 * SE_distribution, #Lower bound
    upper_ci_distribution = case_distribution + 1.96 * SE_distribution  #Upper bound
  )

#Sexual violence
pel_sex_distr <- pel %>%
  group_by(age_cat) %>%
  summarise(
    sexual_violence_cases = sum(ipv_sex_cases, na.rm = TRUE), #Total number of cases in each group
    sample = n() # Total sample size for each group
  ) %>%
  ungroup() %>%
  mutate(
    total_cases = sum(sexual_violence_cases), #Total cases across all age groups
    case_distribution = sexual_violence_cases / total_cases #Proportion of cases in each age group
  )

pel_sex_distr <- pel_sex_distr %>%
  mutate(
    SE_distribution = sqrt((case_distribution * (1 - case_distribution)) / total_cases), #Standard error
    lower_ci_distribution = case_distribution - 1.96 * SE_distribution, #Lower bound
    upper_ci_distribution = case_distribution + 1.96 * SE_distribution  #Upper bound
  )

#save distribution results
write.csv(pel_phys_distr, file = "FILE_PATH/pel_phys_distr.csv", row.names = FALSE)
write.csv(pel_sex_distr, file = "/FILE_PATH/pel_sex_distr.csv", row.names = FALSE)


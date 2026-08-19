####################################################################################################################################################################
#
#Assessing methodological differences in estimating exposure to intimate-partner violence against women in Brazil: a multi-source observational analysis
#
####################################################################################################################################################################

######## Data Senado Research 2019 #######

### Opening the packages needed to the analysis

library(dplyr)
library(tidyverse)


#National level
dsr <- read.csv("FILE_PATH/BRA_DATASENADO_DOMESTIC_AND_FAMILY_VIOLENCE_AGAINST_WOMEN_SURVEY_2019_Y2022M10D21.CSV", sep = ";",)

######Variables######

##Subtype of violence - IPV only
#Psychological violence
dsr <- mutate(dsr, ipv_psy_cases = if_else(P30_01 == "1" | P30_02 == "1" | P30_03 == "1" | P30_A_01 == "1" | P30_A_02 =="1",  "1",
                                           if_else(P30_01 == "2" | P30_02 == "2" | P30_03 == "2" | P30_A_01 == "2" | P30_A_02 =="2",  "2", "NA")))

#Physical violence
dsr <- mutate(dsr, ipv_phy_cases = if_else(P30_A_05 == "1" | P30_A_06 == "1", "1",
                                   if_else(P30_A_05 == "2" | P30_A_06 == "2", "2", "NA")))

#Age group - categorical in the original data
#The data is from 16 years old
dsr <- mutate(dsr, age = if_else(idade.wgts == "1", "16-29",  
                         if_else(idade.wgts == "2", "30-39",
                         if_else(idade.wgts == "3", "40-49",
                         if_else(idade.wgts == "4", "50-59",
                         if_else(idade.wgts == "5", "60-99", "NA"))))))


table(dsr$ipv_phy_cases, dsr$age)

dsr <- as.data.table(dsr)

dsr[, W2 := gsub(",", ".", W2)] #Replace comma with dot in the weight variable
dsr[ipv_phy_cases == "NA", ipv_phy_cases := NA] #Convert "NA" strings to NA values


dsr <- dsr %>%
  mutate(
    ipv_phy_cases = as.numeric((ipv_phy_cases)), #Convert to numeric if stored as factor/character
    W2 = as.numeric((W2)) #Convert to numeric if stored as factor/character
  )

################################################################################################################
#IPV prevalence by AGE GROUP
################################################################################################################

###Physical violence
#Calculate Prevalence and CIs
library(survey)
library(data.table)

dsr_phy <- as.data.table(dsr)

dsr_phy[,ipv_phy_cases := ifelse(ipv_phy_cases == "1", 1, 0)]

dsr_phy <- as.data.table(dsr_phy)
dsr_phy[, wgt  := W2/1000000]

sample_size <- sum(dsr_phy$W2, na.rm = TRUE) 
print(sample_size)

#Define the survey design object
survey_design <- svydesign(ids = ~1, data = dsr_phy, weights = ~wgt)

#Create a list of age groups
age_groups <- c("16-29", "30-39", "40-49", "50-59", "60-99")

#Loop through each age group to calculate prevalence and confidence intervals
results <- lapply(age_groups, function(age_group) {
  #Subset the survey design for the current age group
  age_subset <- subset(survey_design, age == age_group)
  
  #Calculate weighted prevalence and confidence interval for ipv_phy_cases
  prevalence_ci <- svymean(~ipv_phy_cases, design = age_subset, na.rm = TRUE)
  
  #Extract prevalence and confidence interval
  prevalence <- coef(prevalence_ci)              #Point estimate
  conf_int <- confint(prevalence_ci)             #Confidence interval
  
  #Return results as a data frame
  data.frame(
    Age_Group = age_group,
    Prevalence = prevalence,
    Lower_95CI = conf_int[1],
    Upper_95CI = conf_int[2]
  )
})

#Calculate sample size and number of cases by age group
result <- dsr_phy %>%
  group_by(age) %>%
  summarize(
    sample_size = n(),               #Total number of observations
    num_cases = sum(ipv_phy_cases),           #Total number of cases
    .groups = "drop"                 #Drop grouping in the final output
  )

#Combine result and s into a single data frame
results <- lapply(results, function(df) {
  #Find the corresponding sample size and number of cases for the current age group
  age_group <- df$Age_Group[1]
  sample_size <- result$sample_size[result$age == age_group]
  num_cases <- result$num_cases[result$age == age_group]
  
  #Add sample size and number of cases to the data frame
  df$Sample_Size <- sample_size
  df$Num_Cases <- num_cases
  return(df)
})

#Combine results into a single data frame
dsr_phy_prev <- do.call(rbind, results)

#Display the results
print(dsr_phy_prev)
dsr_phy_prev <- mutate(dsr_phy_prev, source = "2019 Data Senado Research")

write.csv(dsr_phy_prev, "FILE_PATH/dsr_phy_prev.csv", row.names = FALSE)

################################################################################################################
#Distribution of cases by AGE GROUP considering SAMPLE weights
################################################################################################################

#Install and load the survey package
if (!requireNamespace("survey", quietly = TRUE)) {
  install.packages("survey")
}
library(survey)
library(dplyr)

#Keep in dsr only ipv_phy_cases ==1 
dsr <- dsr %>%
  filter(ipv_phy_cases == 1) %>%
  mutate(age = factor(age, levels = c("16-29", "30-39", "40-49", "50-59", "60-99"))) #Ensure age is a factor with specified levels

#Debugging Step: Verify the conversion
print("Summary of variables after conversion:")
summary(dsr$ipv_phy_cases)
summary(dsr$W2)

#Create a survey design object using the sample weights
survey_design <- svydesign(id = ~1, weights = ~W2, data = dsr)

#Calculate the weighted distribution of cases by age group
dsr_phys_distr <- dsr %>%
  group_by(age) %>%
  summarise(
    weighted_cases = sum(ipv_phy_cases * W2, na.rm = TRUE), #Weighted sum of cases
    sample_size = sum(W2, na.rm = TRUE) # otal weighted sample size for each group
  ) %>%
  ungroup() %>%
  mutate(
    total_weighted_cases = sum(weighted_cases, na.rm = TRUE), #Total weighted cases across all age groups
    case_distribution = weighted_cases / total_weighted_cases #Proportion of weighted cases in each age group
  )

#Calculate standard errors and confidence intervals
dsr_phys_distr_sw <- dsr_phys_distr %>%
  mutate(
    SE_distribution = sqrt((case_distribution * (1 - case_distribution)) / total_weighted_cases), #Standard error
    lower_ci_distribution = case_distribution - 1.96 * SE_distribution, #Lower bound of 95% CI
    upper_ci_distribution = case_distribution + 1.96 * SE_distribution  #Upper bound of 95% CI
  )

write.csv(dsr_phys_distr_sw, "FILE_PATH/dsr_phys_distr_sw.csv", row.names = FALSE)

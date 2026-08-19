####################################################################################################################################################################
#
#Assessing methodological differences in estimating exposure to intimate-partner violence against women in Brazil: a multi-source observational analysis
#
####################################################################################################################################################################

######## Brazilian National Alcohol and Drugs Survey, 2005-2006 #######

### Opening the packages needed to the analysis

library(dplyr)
library(tidyverse)
library(foreign)

### Selecting data to national level
LENAD2005 <- read.dta("FILE_PATH/BRA_LENAD_2005_2006_Y2024M03D13.DTA")
lenad_i <- subset(LENAD2005, a1 == "feminino")
lenad_i <- subset(lenad_i, a4 == "casado(a)/ comp.(a)")


###### Variables ######
#Age group - categorical in the original data
#The data is from 16 years old
lenad_i <- lenad_i[lenad_i$a2a >= 16 & !is.na(lenad_i$a2a), ]

lenad_i <- mutate(lenad_i, age = if_else(a2a >= 16 & a2a <= 29, "16-29",
                                         if_else(a2a >= 30 & a2a <= 39, "30-39",
                                                 if_else(a2a >= 40 & a2a <= 49, "40-49",
                                                         if_else(a2a >= 50 & a2a <= 59, "50-59",
                                                                 if_else(a2a >= 60 & a2a <= 99, "60-99", "NA"))))))

## Subtype of violence - IPV only 
#Physical violence
lenad_i <- mutate(lenad_i, ipv_phy_cases = if_else(cv18 == "sim" | cv19 == "sim" | cv20 == "sim" | cv21 == "sim" | cv22 =="sim" | cv23 == "sim" | cv25 == "sim" | cv26 == "sim",  "1",
                                                   if_else(cv18 == "n<e3>o" | cv19 == "n<e3>o" | cv20 == "n<e3>o" | cv21 == "n<e3>o" | cv22 =="n<e3>o" | cv23 == "n<e3>o" | cv25 == "n<e3>o" | cv26 == "n<e3>o",  "2", "NA")))

#Sexual violence
lenad_i <- mutate(lenad_i, ipv_sex_cases = if_else(cv24 == "sim", "1",
                                                   if_else(cv24 == "n<e3>o", "2", "NA")))


################################################################################################################
#IPV prevalence by AGE GROUP
################################################################################################################

###Physical violence
#Calculate Prevalence and CIs
library(survey)
library(data.table)

lenad_i_phy <- as.data.table(lenad_i)
lenad_i_phy[,ipv_phy_cases := ifelse(ipv_phy_cases == "1", 1, 0)]

lenad_i_phy[, wgt  := weight_4/1000000]

sample_size <- sum(lenad_i_phy$weight_4, na.rm = TRUE) 
print(sample_size)

#Define the survey design object
survey_design <- svydesign(ids = ~1, data = lenad_i_phy, weights = ~wgt)

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
result <- lenad_i_phy %>%
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
lenad2006_phy_prev <- do.call(rbind, results)

#Display the results
print(lenad2006_phy_prev)
lenad2006_phy_prev <- mutate(lenad2006_phy_prev, source = "2006 Brazilian National Alcohol and Drugs Survey")

write.csv(lenad2006_phy_prev, "FILE_PATH/lenad2006_phy_prev.csv", row.names = FALSE)


################################################################################################################
#Distribution of cases by AGE GROUP considering SAMPLE weights
################################################################################################################

#Install and load the survey package
if (!requireNamespace("survey", quietly = TRUE)) {
  install.packages("survey")
}
library(survey)
library(dplyr)

#keep in lenad_i only ipv_phy_cases ==1 
lenad_i <- lenad_i %>%
  filter(ipv_phy_cases == 1) %>%
  mutate(age = factor(age, levels = c("16-29", "30-39", "40-49", "50-59", "60-99"))) # Ensure age is a factor with specified levels

#Ensure `ipv_phy_cases` is numeric
lenad_i <- lenad_i %>%
  mutate(
    ipv_phy_cases = as.numeric(as.character(ipv_phy_cases)), #Convert to numeric if stored as factor/character
    weight_4 = as.numeric(as.character(weight_4)) #Convert to numeric if stored as factor/character
  )

#Verify the conversion
print("Summary of variables after conversion:")
summary(lenad_i$ipv_phy_cases)
summary(lenad_i$weight_4)

#Create a survey design object using the sample weights
survey_design <- svydesign(id = ~1, weights = ~weight_4, data = lenad_i)

#Calculate the weighted distribution of cases by age group
lenad_phys_distr <- lenad_i %>%
  group_by(age) %>%
  summarise(
    weighted_cases = sum(ipv_phy_cases * weight_4, na.rm = TRUE), #Weighted sum of cases
    sample_size = sum(weight_4, na.rm = TRUE) #Total weighted sample size for each group
  ) %>%
  ungroup() %>%
  mutate(
    total_weighted_cases = sum(weighted_cases, na.rm = TRUE), #Total weighted cases across all age groups
    case_distribution = weighted_cases / total_weighted_cases #Proportion of weighted cases in each age group
  )

#Calculate standard errors and confidence intervals
lenad06_phys_distr_sw <- lenad_phys_distr %>%
  mutate(
    SE_distribution = sqrt((case_distribution * (1 - case_distribution)) / total_weighted_cases), #Standard error
    lower_ci_distribution = case_distribution - 1.96 * SE_distribution, #Lower bound of 95% CI
    upper_ci_distribution = case_distribution + 1.96 * SE_distribution  #Upper bound of 95% CI
  )

write.csv(lenad06_phys_distr_sw, "FILE_PATH/lenad06_phys_distr_sw.csv", row.names = FALSE)

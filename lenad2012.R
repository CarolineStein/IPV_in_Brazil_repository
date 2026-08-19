####################################################################################################################################################################
#
#Assessing methodological differences in estimating exposure to intimate-partner violence against women in Brazil: a multi-source observational analysis
#
####################################################################################################################################################################

######## Brazilian National Alcohol and Drugs Survey, 2011-2012 #######

### Opening the packages needed to the analysis

library(dplyr)
library(tidyverse)
library (foreign)

### Selecting data to national level
LENAD2012 <- read.dta("FILE_PATH/LENAD2012.dta")
lenad_ii <- subset(LENAD2012, a1 == "Feminino")
lenad_ii <- subset(lenad_ii, a4 == "Casado(a) ou morando junto(a)")

###### Variables ######
#Age group - categorical in the original data
#The data is from 16 years old
lenad_ii <- mutate(lenad_ii, age_cat = if_else(age >= 16 & age <= 29, "16-29",
                                         if_else(age >= 30 & age <= 39, "30-39",
                                                 if_else(age >= 40 & age <= 49, "40-49",
                                                         if_else(age >= 50 & age <= 59, "50-59",
                                                                 if_else(age >= 60 & age <= 99, "60-99", "NA"))))))

## Subtype of violence - IPV only 
#Physical violence
lenad_ii <- mutate(lenad_ii, ipv_phy_cases = if_else(n10a == "Sim" | n11a == "Sim" | n12a == "Sim" | n13a == "Sim" | n14a =="Sim" | n15a == "Sim" | n17a == "Sim" | n18a == "Sim",  "1",
                                                   if_else(n10a == "N<e3>o" & n11a == "N<e3>o" & n12a == "N<e3>o" & n13a == "N<e3>o" & n14a =="N<e3>o" & n15a == "N<e3>o" & n17a == "N<e3>o" & n18a == "n<e3>o",  "2", "NA")))

#Sexual violence
lenad_ii <- mutate(lenad_ii, ipv_sex_cases = if_else(n16a == "Sim", "1",
                                                   if_else(n16a == "N<e3>o", "2", "NA")))



################################################################################################################
#IPV prevalence by AGE GROUP
################################################################################################################

###Physical violence
#Calculate Prevalence and CIs
library(survey)
library(data.table)

lenad_i_phy <- as.data.table(lenad_ii)
lenad_i_phy[,ipv_phy_cases := ifelse(ipv_phy_cases == "1", 1, 0)]

lenad_i_phy[, wgt  := final_weight/1000000]

sample_size <- sum(lenad_i_phy$final_weight, na.rm = TRUE) 
print(sample_size)

#Define the survey design object
survey_design <- svydesign(ids = ~1, data = lenad_i_phy, weights = ~wgt)

#Create a list of age groups
age_groups <- c("16-29", "30-39", "40-49", "50-59", "60-99")

#Loop through each age group to calculate prevalence and confidence intervals
results <- lapply(age_groups, function(age_group) {
  #Subset the survey design for the current age group
  age_subset <- subset(survey_design, age_cat == age_group)
  
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
  group_by(age_cat) %>%
  summarize(
    sample_size = n(),               #Total number of observations
    num_cases = sum(ipv_phy_cases),           #Total number of cases
    .groups = "drop"                 #Drop grouping in the final output
  )

#Combine result and s into a single data frame
results <- lapply(results, function(df) {
  #Find the corresponding sample size and number of cases for the current age group
  age_group <- df$Age_Group[1]
  sample_size <- result$sample_size[result$age_cat == age_group]
  num_cases <- result$num_cases[result$age_cat == age_group]
  
  #Add sample size and number of cases to the data frame
  df$Sample_Size <- sample_size
  df$Num_Cases <- num_cases
  return(df)
})

#Combine results into a single data frame
lenad12_phy_prev <- do.call(rbind, results)

# Display the results
print(lenad12_phy_prev)
lenad12_phy_prev <- mutate(lenad12_phy_prev, source = "2012 Brazilian National Alcohol and Drugs Survey")

write.csv(lenad12_phy_prev, "FILE_PATH/lenad12_phy_prev.csv", row.names = FALSE)

################################################################################################################
#Distribution of cases by AGE GROUP considering SAMPLE weights
################################################################################################################

#Install and load the survey package
if (!requireNamespace("survey", quietly = TRUE)) {
  install.packages("survey")
}
library(survey)
library(dplyr)

#Keep in lenad only ipv_phy_cases ==1 
lenad_ii <- lenad_ii %>%
  filter(ipv_phy_cases == 1) %>%
  mutate(age = factor(age_cat, levels = c("16-29", "30-39", "40-49", "50-59", "60-99"))) #Ensure age is a factor with specified levels


#Ensure `ipv_phy_cases` is numeric
lenad_ii <- lenad_ii %>%
  mutate(
    ipv_phy_cases = as.numeric(as.character(ipv_phy_cases)), #Convert to numeric if stored as factor/character
    final_weight = as.numeric(as.character(final_weight)) #Convert to numeric if stored as factor/character
  )

#Verify the conversion
print("Summary of variables after conversion:")
summary(lenad_ii$ipv_phy_cases)
summary(lenad_ii$final_weight)

#Create a survey design object using the sample weights
survey_design <- svydesign(id = ~1, weights = ~final_weight, data = lenad_ii)

#Calculate the weighted distribution of cases by age group
lenad_phys_distr <- lenad_ii %>%
  group_by(age) %>%
  summarise(
    weighted_cases = sum(ipv_phy_cases * final_weight, na.rm = TRUE), #Weighted sum of cases
    sample_size = sum(final_weight, na.rm = TRUE) #Total weighted sample size for each group
  ) %>%
  ungroup() %>%
  mutate(
    total_weighted_cases = sum(weighted_cases, na.rm = TRUE), #Total weighted cases across all age groups
    case_distribution = weighted_cases / total_weighted_cases #Proportion of weighted cases in each age group
  )

#Calculate standard errors and confidence intervals
lenad12_phys_distr_sw <- lenad_phys_distr %>%
  mutate(
    SE_distribution = sqrt((case_distribution * (1 - case_distribution)) / total_weighted_cases), # Standard error
    lower_ci_distribution = case_distribution - 1.96 * SE_distribution, # Lower bound of 95% CI
    upper_ci_distribution = case_distribution + 1.96 * SE_distribution  # Upper bound of 95% CI
  )

expanded_sample <- sum(lenad_phys_distr$sample_size)
print(expanded_sample)

#View the results
print(lenad12_phys_distr_sw)

write.csv(lenad12_phys_distr_sw, "FILE_PATH/lenad12_phys_distr_sw.csv", row.names = FALSE)

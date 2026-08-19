####################################################################################################################################################################
#
#Assessing methodological differences in estimating exposure to intimate-partner violence against women in Brazil: a multi-source observational analysis
#
####################################################################################################################################################################

########## Civil Police dataset ##########

library(tidyverse)
library("readxl")

pc_es <- read_excel("FILE_PATH/pc_es.xlsx", sheet = "Vítima", na = "NA")

#Select the capital of Espirito Santo (VITORIA)
pc_es <- subset(pc_es, MUNICÍPIO == "VITORIA")

#Select only women (SEXO = FEMININO)
pc_es_w <- subset(pc_es, SEXO == "FEMININO")

#Select women with =>18 years old
pc_es_w <- pc_es_w %>%
  mutate(IDADE = as.numeric(IDADE))

pc_es_w_adult <- subset(pc_es_w, IDADE >= 18)

#Exclude age without information
pc_es_w_adult <- pc_es_w_adult[!(pc_es_w_adult$IDADE == "Sem Informação"),]

#Age categories
pc_es_w_adult <- mutate(pc_es_w_adult, age = if_else(IDADE >= 18 & IDADE <= 29, "18-29",
                                                     if_else(IDADE >= 30 & IDADE <= 39, "30-39",
                                                             if_else(IDADE >= 40 & IDADE <= 49, "40-49",
                                                                     if_else(IDADE >= 50 & IDADE <= 59, "50-59",
                                                                             if_else(IDADE >= 60 & IDADE <= 99, "60-99", "NA"))))))

#Removing NAs
pc_es_w_adult <- pc_es_w_adult[!(pc_es_w_adult$age == "NA"),]

library("data.table")

#Rename variable TIPO DE INCIDENTE
pc_es_w_adult <- rename(pc_es_w_adult, tipo_de_incidente = "TIPO DE INCIDENTE")

#New variable incident type: translating to English
pc_es_w_adult <- mutate(pc_es_w_adult, incident_type = if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: AMEAÇA", "CRIMES AGAINST THE PERSON: THREAT",
                                                               if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: AMEAÇA: CONTRA MULHER - LEI MARIA PENHA", "CRIMES AGAINST THE PERSON: THREAT: AGAINST WOMEN - MARIA PENHA LAW",
                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: AMEAÇA: PERSEGUIÇÃO", "CRIMES AGAINST PERSON: THREAT: PERSECUTION",
                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: CONSTRANGIMENTO ILEGAL", "CRIMES AGAINST THE PERSON: ILLEGAL CONSTRAINTMENT",
                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: AMEAÇA: VIOLÊNCIA PSICOLÓGICA CONTRA A MULHER", "CRIMES AGAINST THE PERSON: THREAT: PSYCHOLOGICAL VIOLENCE AGAINST WOMEN",
                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: AMEAÇA: ENVOLVENDO AGENTE DE SEGURANÇA PÚBLICA", "CRIMES AGAINST PERSON: THREAT: INVOLVING PUBLIC SECURITY AGENT",
                                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: CONSTRANGIMENTO ILEGAL: CONTRA MULHER - LEI MARIA PENHA", "CRIMES AGAINST THE PERSON: ILLEGAL CONSTRAINT: AGAINST WOMEN - MARIA PENHA LAW",
                                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: LESÃO CORPORAL", "CRIMES AGAINST PERSON: BODILY INJURY",
                                                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: LESÃO CORPORAL: LEVE", "CRIMES AGAINST PERSON: BODILY INJURY: MINOR",
                                                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: LESÃO CORPORAL: LEVE: CONTRA MULHER - LEI MARIA PENHA", "CRIMES AGAINST PERSON: BODILY INJURY: LIGHT: AGAINST WOMAN - MARIA PENHA LAW",
                                                                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: LESÃO CORPORAL: CONTRA MULHER - LEI MARIA PENHA", "CRIMES AGAINST THE PERSON: BODILY INJURY: AGAINST WOMEN - MARIA PENHA LAW",
                                                                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: LESÃO CORPORAL: GRAVE", "CRIMES AGAINST PERSON: BODILY INJURY: SEVERE",
                                                                                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: LESÃO CORPORAL: GRAVE: CONTRA MULHER - LEI MARIA PENHA", "CRIMES AGAINST PERSON: BODILY INJURY: SEVERE: AGAINST WOMEN - MARIA PENHA LAW",
                                                                                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: LESÃO CORPORAL: ENVOLVENDO AGENTE DE SEGURANÇA PÚBLICA", "CRIMES AGAINST PERSON: BODILY INJURY: INVOLVING PUBLIC SECURITY AGENT ",
                                                                                                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: LESÃO CORPORAL: GRAVÍSSIMA", "CRIMES AGAINST PERSON: BODILY INJURY: VERY SEVERE",
                                                                                                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA A PESSOA: LESÃO CORPORAL: GRAVÍSSIMA: CONTRA MULHER - LEI MARIA PENHA", "CRIMES AGAINST PERSON: BODILY INJURY: SEVERE: AGAINST WOMAN - MARIA PENHA LAW",
                                                                                                                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: ESTUPRO DE VULNERÁVEL", "CRIMES AGAINST SEXUAL DIGNITY: RAPE OF A VULNERABLE",
                                                                                                                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: ESTUPRO", "CRIMES AGAINST SEXUAL DIGNITY: RAPE",
                                                                                                                                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: ASSÉDIO SEXUAL", "CRIMES AGAINST SEXUAL DIGNITY: SEXUAL HARASSMENT",
                                                                                                                                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL", "CRIMES AGAINST SEXUAL DIGNITY",
                                                                                                                                                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: TENTATIVA DE ESTUPRO", "CRIMES AGAINST SEXUAL DIGNITY: ATTEMPTED RAPE",
                                                                                                                                                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: OUTROS CRIMES CONTRA OS COSTUMES", "CRIMES AGAINST SEXUAL DIGNITY: OTHER CRIMES AGAINST CUSTOMS",
                                                                                                                                                                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: ATO OBSCENO", "CRIMES AGAINST SEXUAL DIGNITY: OBSCENE ACT",
                                                                                                                                                                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: VIOLAÇÃO SEXUAL MEDIANTE FRAUDE: IMPORTUNAÇÃO SEXUAL", "CRIMES AGAINST SEXUAL DIGNITY: SEXUAL VIOLATION THROUGH FRAUD: SEXUAL IMPORTUNATION",
                                                                                                                                                                                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: ESTUPRO: CONTRA MULHER - LEI MARIA PENHA", "CRIMES AGAINST SEXUAL DIGNITY: RAPE: AGAINST WOMEN - MARIA PENHA LAW",
                                                                                                                                                                                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: CORRUPÇÃO DE MENORES", "CRIMES AGAINST SEXUAL DIGNITY: CORRUPTION OF MINORS",
                                                                                                                                                                                                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: ATENTADO VIOLENTO AO PUDOR", "CRIMES AGAINST SEXUAL DIGNITY: VIOLENT ASSESSMENT OF MODE",
                                                                                                                                                                                                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: TENTATIVA DE ESTUPRO: CONTRA MULHER - LEI MARIA PENHA", "CRIMES AGAINST SEXUAL DIGNITY: ATTEMPTED RAPE: AGAINST WOMEN - MARIA PENHA LA",
                                                                                                                                                                                                                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: FAVORECIMENTO DA PROSTITUIÇÃO", "CRIMES AGAINST SEXUAL DIGNITY: ENVIRONMENT OF PROSTITUTION",
                                                                                                                                                                                                                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: VIOLAÇÃO SEXUAL MEDIANTE FRAUDE", "CRIMES AGAINST SEXUAL DIGNITY: SEXUAL VIOLATION THROUGH FRAUD",
                                                                                                                                                                                                                                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: ATENTADO DO PUDOR MEDIANTE FRAUDE", "CRIMES AGAINST SEXUAL DIGNITY: ATTEMPT THROUGH FRAUD",
                                                                                                                                                                                                                                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: TENTATIVA DE ATENTADO VIOLENTO AO PUDOR", "CRIMES AGAINST SEXUAL DIGNITY: ATTEMPT OF VIOLENT ASSESSMENT OF INDUDENCE",
                                                                                                                                                                                                                                                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: SEDUÇÃO", "CRIMES AGAINST SEXUAL DIGNITY: SEDUCTION",
                                                                                                                                                                                                                                                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: RAPTO", "CRIMES AGAINST SEXUAL DIGNITY: KIDNAPPING",
                                                                                                                                                                                                                                                                                                                                       if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: RAPTO: CONSENSUAL", "CRIMES AGAINST SEXUAL DIGNITY: KIDNAPPING: CONSENSUAL",
                                                                                                                                                                                                                                                                                                                                               if_else(tipo_de_incidente == "CRIMES CONTRA DIGNIDADE  SEXUAL: CASA DE PROSTITUIÇÃO", "CRIMES AGAINST SEXUAL DIGNITY: HOUSE OF PROSTITUTION", "NA")))))))))))))))))))))))))))))))))))))

#New variable incident type numeric
pc_es_w_adult <- mutate(pc_es_w_adult, incident_type_n = if_else(incident_type == "CRIMES AGAINST THE PERSON: THREAT", "1",
                                                                 if_else(incident_type == "CRIMES AGAINST THE PERSON: THREAT: AGAINST WOMEN - MARIA PENHA LAW", "2",
                                                                         if_else(incident_type == "CRIMES AGAINST PERSON: THREAT: PERSECUTION", "3",
                                                                                 if_else(incident_type == "CRIMES AGAINST THE PERSON: ILLEGAL CONSTRAINTMENT", "4",
                                                                                         if_else(incident_type == "CRIMES AGAINST THE PERSON: THREAT: PSYCHOLOGICAL VIOLENCE AGAINST WOMEN", "5",
                                                                                                 if_else(incident_type == "CRIMES AGAINST PERSON: THREAT: INVOLVING PUBLIC SECURITY AGENT", "6",
                                                                                                         if_else(incident_type == "CRIMES AGAINST THE PERSON: ILLEGAL CONSTRAINT: AGAINST WOMEN - MARIA PENHA LAW", "7",
                                                                                                                 if_else(incident_type == "CRIMES AGAINST PERSON: BODILY INJURY", "8",
                                                                                                                         if_else(incident_type == "CRIMES AGAINST PERSON: BODILY INJURY: MINOR", "9",
                                                                                                                                 if_else(incident_type == "CRIMES AGAINST PERSON: BODILY INJURY: LIGHT: AGAINST WOMAN - MARIA PENHA LAW", "10",
                                                                                                                                         if_else(incident_type == "CRIMES AGAINST THE PERSON: BODILY INJURY: AGAINST WOMEN - MARIA PENHA LAW", "11",
                                                                                                                                                 if_else(incident_type == "CRIMES AGAINST PERSON: BODILY INJURY: SEVERE", "12",
                                                                                                                                                         if_else(incident_type == "CRIMES AGAINST PERSON: BODILY INJURY: SEVERE: AGAINST WOMEN - MARIA PENHA LAW", "13",
                                                                                                                                                                 if_else(incident_type == "CRIMES AGAINST PERSON: BODILY INJURY: INVOLVING PUBLIC SECURITY AGENT ", "14",
                                                                                                                                                                         if_else(incident_type == "CRIMES AGAINST PERSON: BODILY INJURY: VERY SEVERE", "15",
                                                                                                                                                                                 if_else(incident_type == "CRIMES AGAINST PERSON: BODILY INJURY: SEVERE: AGAINST WOMAN - MARIA PENHA LAW", "16",
                                                                                                                                                                                         if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: RAPE OF A VULNERABLE", "17",
                                                                                                                                                                                                 if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: RAPE", "18",
                                                                                                                                                                                                         if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: SEXUAL HARASSMENT", "19",
                                                                                                                                                                                                                 if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY", "20",
                                                                                                                                                                                                                         if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: ATTEMPTED RAPE", "21",
                                                                                                                                                                                                                                 if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: OTHER CRIMES AGAINST CUSTOMS", "22",
                                                                                                                                                                                                                                         if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: OBSCENE ACT", "23",
                                                                                                                                                                                                                                                 if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: SEXUAL VIOLATION THROUGH FRAUD: SEXUAL IMPORTUNATION", "24",
                                                                                                                                                                                                                                                         if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: RAPE: AGAINST WOMEN - MARIA PENHA LAW", "25",
                                                                                                                                                                                                                                                                 if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: CORRUPTION OF MINORS", "26",
                                                                                                                                                                                                                                                                         if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: VIOLENT ASSESSMENT OF MODE", "27",
                                                                                                                                                                                                                                                                                 if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: ATTEMPTED RAPE: AGAINST WOMEN - MARIA PENHA LA", "28",
                                                                                                                                                                                                                                                                                         if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: ENVIRONMENT OF PROSTITUTION", "29",
                                                                                                                                                                                                                                                                                                 if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: SEXUAL VIOLATION THROUGH FRAUD", "30",
                                                                                                                                                                                                                                                                                                         if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: ATTEMPT THROUGH FRAUD", "31",
                                                                                                                                                                                                                                                                                                                 if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: ATTEMPT OF VIOLENT ASSESSMENT OF INDUDENCE", "32",
                                                                                                                                                                                                                                                                                                                         if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: SEDUCTION", "33",
                                                                                                                                                                                                                                                                                                                                 if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: KIDNAPPING", "34",
                                                                                                                                                                                                                                                                                                                                         if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: KIDNAPPING: CONSENSUAL", "35",
                                                                                                                                                                                                                                                                                                                                                 if_else(incident_type == "CRIMES AGAINST SEXUAL DIGNITY: HOUSE OF PROSTITUTION", "36", "NA")))))))))))))))))))))))))))))))))))))

#Transform from character to numeric variable incident_type_n
pc_es_w_adult$incident_type_n <- as.numeric(pc_es_w_adult$incident_type_n)

#Creating sexual, physical, ppsychological violence_type category
pc_es_w_adult <- mutate(pc_es_w_adult, violence_type = if_else(incident_type_n <= 7, "psychological",
                                                               if_else(incident_type_n == 8 | incident_type_n == 9 | incident_type_n == 12 | incident_type_n == 14 |
                                                                         incident_type_n == 15 | incident_type_n == 16, "physical",
                                                                       if_else(incident_type_n == 10 | incident_type_n == 11 | incident_type_n == 13, "physical_ipv",
                                                                               if_else(incident_type_n == 17 | incident_type_n == 18 | incident_type_n == 19 |
                                                                                         incident_type_n == 20 | incident_type_n == 21 | incident_type_n == 22 |
                                                                                         incident_type_n == 23 | incident_type_n == 24 | incident_type_n == 25 |
                                                                                         incident_type_n == 27 | incident_type_n == 28 | incident_type_n == 30 |
                                                                                         incident_type_n == 31, "sexual", "")))))
#Select a year
#pc_es_w_adult <- subset(pc_es_w_adult, ANO == 2019)

pc_es_w_adult <- subset(pc_es_w_adult, ANO == 2020 | ANO == 2021)

table(pc_es_w_adult$violence_type, pc_es_w_adult$incident_type_n)

pc_es_w_adult <- subset(pc_es_w_adult, select = c(ANO, age, violence_type))


#####################################################################################################################
#DISTRIBUTION OF CASES
#####################################################################################################################

#Physical violence

pc_phyipv_distr <- subset(pc_es_w_adult, violence_type == "physical_ipv")

#Change "physical_ipv" category to 1
pc_phyipv_distr <- pc_phyipv_distr %>%
  mutate(violence_type = ifelse(violence_type == "physical_ipv", 1, 0))

pc_phyipv_distr <- pc_phyipv_distr %>%
  mutate(violence_type = as.numeric(violence_type))

pc_phyipv_distr <- pc_phyipv_distr %>%
  group_by(age) %>%
  summarise(
    physical_violence_cases = sum(violence_type, na.rm = TRUE), #Total number of cases in each group
    sample = n() # Total sample size for each group
  ) %>%
  ungroup() %>%
  mutate(
    total_cases = sum(physical_violence_cases), #Total cases across all age groups
    case_distribution = physical_violence_cases / total_cases #Proportion of cases in each age group
  )

pc_phyipv_distr <- pc_phyipv_distr %>%
  mutate(
    SE_distribution = sqrt((case_distribution * (1 - case_distribution)) / total_cases), #Standard error
    lower_ci_distribution = case_distribution - 1.96 * SE_distribution, #Lower bound
    upper_ci_distribution = case_distribution + 1.96 * SE_distribution  #Upper bound
  )

#New variable for source Espírito Santo
pc_phyipv_distr <- mutate(pc_phyipv_distr, source = "2019 Civil Police")

#New variable for source Vitoria
pc_phyipv_distr <- mutate(pc_phyipv_distr, source = "2020-2021 Civil Police")


#Sexual violence

pc_sex_distr <- subset(pc_es_w_adult, violence_type == "sexual")

#Change "sexual" category to 1
pc_sex_distr <- pc_sex_distr %>%
  mutate(violence_type = ifelse(violence_type == "sexual", 1, 0))

pc_sex_distr <- pc_sex_distr %>%
  mutate(violence_type = as.numeric(violence_type))

pc_sex_distr <- pc_sex_distr %>%
  group_by(age) %>%
  summarise(
    sexual_violence_cases = sum(violence_type, na.rm = TRUE), #Total number of cases in each group
    sample = n() #Total sample size for each group
  ) %>%
  ungroup() %>%
  mutate(
    total_cases = sum(sexual_violence_cases), #Total cases across all age groups
    case_distribution = sexual_violence_cases / total_cases #Proportion of cases in each age group
  )

pc_sex_distr <- pc_sex_distr %>%
  mutate(
    SE_distribution = sqrt((case_distribution * (1 - case_distribution)) / total_cases), #Standard error
    lower_ci_distribution = case_distribution - 1.96 * SE_distribution, #Lower bound
    upper_ci_distribution = case_distribution + 1.96 * SE_distribution  #Upper bound
  )

#New variable for source
pc_sex_distr <- mutate(pc_sex_distr, source = "2019 Civil Police")

#New variable for source Vitoria
pc_sex_distr <- mutate(pc_sex_distr, source = "2020-2021 Civil Police")


#Save distribution results Espírito Santo
write.csv(pc_phyipv_distr, file = "FILE_PATH/pc_es_phyipv_distr.csv", row.names = FALSE)
write.csv(pc_sex_distr, file = "FILE_PATH/pc_es_sex_distr.csv", row.names = FALSE)

#Save distribution results Vitoria
write.csv(pc_phyipv_distr, file = "FILE_PATH/pc_vit_phyipv_distr.csv", row.names = FALSE)
write.csv(pc_sex_distr, file = "FILE_PATH/pc_vit_sex_distr.csv", row.names = FALSE)


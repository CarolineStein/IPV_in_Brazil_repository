####################################################################################################################################################################
#
#Assessing methodological differences in estimating exposure to intimate-partner violence against women in Brazil: a multi-source observational analysis
#
####################################################################################################################################################################

######## Vitória study ######## 

sexual_vit <- read.csv("FILE_PATH/sexual_vit.csv")
phys_vit <- read.csv("FILE_PATH/phys_vit.csv")

######################
#
#Age distribution of IPV cases in Vitória study
#
######################

#IPV PHYSICAL violence
phys_vit_total <- sum(phys_vit$cases)
phys_vit <- mutate(phys_vit, propor = cases/phys_vit_total)
phys_vit <- mutate(phys_vit, groupval = "Vitória study")

#IPV SEXUAL violence 
sexual_vit_total <- sum(sexual_vit$cases)
sexual_vit <- mutate(sexual_vit, propor = cases/sexual_vit_total)
sexual_vit <- mutate(sexual_vit, groupval = "Vitória study")


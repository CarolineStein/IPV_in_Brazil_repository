####################################################################################################################################################################
#
#Assessing methodological differences in estimating exposure to intimate-partner violence against women in Brazil: a multi-source observational analysis
#
####################################################################################################################################################################

####### Code for IPV in Brazil age distribution of cases figures ####### 

library(ggplot2)
library(dplyr)
library(cowplot)

#Read csv file for Figure 2
fig2_phy_distr_sw <- read.csv("FILE_PATH/fig2_phy_distr_sw.csv")

#Create the bar graph 2A
phys_a_dist <- subset(fig2_phy_distr_sw, groupval == "2006 LENAD" | groupval == "2012 LENAD")
ggplot(phys_a_dist, aes(x = age, y = case_distribution * 100, fill = groupval)) +  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, position = position_dodge(0.9)) + 
  labs(
    title = "A Distribution of physical IPV cases by age group",
    x = "Age Group",
    y = "Distribution of IPV cases (%)",
    fill = "Data source"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("2006 LENAD" = "#87CEEB", 
                               "2012 LENAD" = "#1E90FF", 
                               "2019 PNS" = "#4682B4", 
                               "2019 DSR" = "#0000CD"))+  #, "Group E" = "#4169E1")) +
  #scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_y_continuous(limits = c(0, 75)) + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"),  
    axis.text = element_text(size = 12),  
    legend.position = c(0.85, 0.85),  
    legend.background = element_rect(fill = "white", color = "white"),  
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black") 
  )

#Save figure 2A
ggsave("FILE_PATH/phys_a_dist.png", width = 8, height = 6, dpi = 300)

#Create the bar graph 2B
phys_b_dist <- subset(fig2_phy_distr_sw, groupval == "2019 DSR")
ggplot(phys_b_dist, aes(x = age, y = case_distribution * 100, fill = groupval)) +  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, position = position_dodge(0.9)) +  
  labs(
    title = "B Distribution of physical IPV cases by age group",
    x = "Age Group",
    y = "Distribution of IPV cases (%)",
    fill = "Data source"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("2019 DSR" = "#0000CD"))+  #, "Group E" = "#4169E1")) +
  #scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_y_continuous(limits = c(0, 75)) + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"),  
    axis.text = element_text(size = 12),  
    legend.position = c(0.85, 0.85),  
    legend.background = element_rect(fill = "white", color = "white"),  
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black") 
  )

#Save figure 2B
ggsave("FILE_PATH/phys_b_dist.png", width = 8, height = 6, dpi = 300)

#Create the bar graph 2C
phys_c_dist <- subset(fig2_phy_distr_sw, groupval == "2019 PNS Physical IPV" | groupval == "2019 PNS Sexual IPV")
ggplot(phys_c_dist, aes(x = age, y = case_distribution * 100, fill = groupval)) +  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, position = position_dodge(0.9)) +
  labs(
    title = "C Distribution of physical and sexual IPV cases by age group",
    x = "Age Group",
    y = "Distribution of IPV cases (%)",
    fill = "Data source"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("2019 PNS Physical IPV" = "#4682B4", 
                               "2019 PNS Sexual IPV" = "darkblue"))+  #, "Group E" = "#4169E1")) +
  #scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_y_continuous(limits = c(0, 75)) + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"),  
    axis.text = element_text(size = 12),  
    legend.position = c(0.85, 0.85),  
    legend.background = element_rect(fill = "white", color = "white"),  
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black") 
  )

# Save figure 2C
ggsave("FILE_PATH/phys_c_dist.png", width = 8, height = 6, dpi = 300)

#Combine three figures above in one and save in pdf

# Load your PNG files as images
img1 <- ggdraw() + draw_image("FILE_PATH/phys_a_dist.png")  
img2 <- ggdraw() + draw_image("FILE_PATH/phys_b_dist.png") 
img3 <- ggdraw() + draw_image("FILE_PATH/phys_c_dist.png") 

# Combine the images (set ncol = 2 for horizontal arrangement)
combined_plot <- plot_grid(
  img1, img2,img3,      
  nrow = 3         
)

#Save combined figure as a PDF file
ggsave("FILE_PATH/fig2combined.pdf", combined_plot, width = 12, height = 6)

#################################################################################################################

#Read csv file for Figure 3
fig3_sex_distr <- read.csv("FILE_PATH/fig3_sex_distr.csv")

#Create the bar graph
ggplot(fig3_sex_distr, aes(x = age, y = case_distribution * 100, fill = groupval)) +  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, position = position_dodge(0.9)) +
  labs(
    title = "Distribution of physical and sexual IPV cases by age group",
    x = "Age Group",
    y = "Distribution of IPV cases (%)",
    fill = "Data source"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("2019 SINAN Brazil Physical IPV" = "#4169E1",
                               "2019 SINAN Brazil Sexual IPV" = "#27408B")) +
  #scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_y_continuous(limits = c(0, 75)) + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"),  
    axis.text = element_text(size = 12),  
    legend.position = c(0.85, 0.85),  
    legend.background = element_rect(fill = "white", color = "white"),  
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black") 
  )

#Save figure
ggsave("FILE_PATH/fig3_sex_distr.pdf", width = 12, height = 6)

#################################################################################################################

# Read csv file for Figure S3
figS3_vit_dist <- read.csv("FILE_PATH/figS3_vit_dist.csv")

#Create the bar graph
ggplot(figS3_vit_dist, aes(x = age, y = case_distribution * 100, fill = groupval)) +  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, position = position_dodge(0.9)) + 
  labs(
    #title = "Distribution of physical and sexual IPV cases by age group",
    x = "Age Group",
    y = "Distribution of IPV cases (%)",
    fill = "Data source"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("2020-2021 Vitoria study Physical IPV" = "#90EE90",
                               "2020-2021 Vitoria study Sexual IPV" = "green4")) +
  scale_y_continuous(limits = c(0, 75)) + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"),  
    axis.text = element_text(size = 12),  
    legend.position = c(0.85, 0.85),  
    legend.background = element_rect(fill = "white", color = "white"),  
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black") 
  )

#Save figure
ggsave("FILE_PATH/figS3_vit_dist.png", width = 8, height = 6, dpi = 300)

#################################################################################################################

#Read csv file for Figure S4
fig3b_phy_state <- read.csv("FILE_PATH/figS4_es_sinan_dist.csv")

#Create the bar graph
ggplot(fig3b_phy_state, aes(x = age, y = case_distribution * 100, fill = groupval)) +  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, position = position_dodge(0.9)) + 
  labs(
    #title = "Distribution of physical IPV cases by age group",
    x = "Age Group",
    y = "Distribution of IPV cases (%)",
    fill = "Data source"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("2019 SINAN Espirito Santo Physical IPV" = "#4169E1", 
                               "2019 SINAN Espirito Santo Sexula IPV" = "#27408B")) +
  scale_y_continuous(limits = c(0, 75)) + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"),  
    axis.text = element_text(size = 12),  
    legend.position = c(0.85, 0.85),  
    legend.background = element_rect(fill = "white", color = "white"),  
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black") 
  )

#Save figure
ggsave("FILE_PATH/fig3b_phy_state.png", width = 8, height = 6, dpi = 300)

#################################################################################################################

#Read csv file for Figure S5
figS5_es_pol_dist <- read.csv("FILE_PATH/figS5_es_pol_dist.csv")

#Create the bar graph
ggplot(figS5_es_pol_dist, aes(x = age, y = case_distribution * 100, fill = groupval)) +  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, position = position_dodge(0.9)) + 
  labs(
    #title = "Distribution of sexual IPV cases by age group",
    x = "Age Group",
    y = "Distribution of IPV cases (%)",
    fill = "Data source"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("2019 Civil Police Espirito Santo Physical IPV" = "#FF6A6A", 
                               "2019 Civil Police Espirito Santo Sexual IPV" = "#800000")) +
  scale_y_continuous(limits = c(0, 75)) + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"),  
    axis.text = element_text(size = 12),  
    legend.position = c(0.85, 0.85),  
    legend.background = element_rect(fill = "white", color = "white"),  
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black") 
  )

#Save figure
ggsave("FILE_PATH/figS5_es_pol_dist.png", width = 8, height = 6, dpi = 300)

#################################################################################################################

#Read csv file for Figure S6
figS6_vit_sinan_dist <- read.csv("FILE_PATH/figS6_vit_sinan_dist.csv")

#Create the bar graph
ggplot(figS6_vit_sinan_dist, aes(x = age, y = case_distribution * 100, fill = groupval)) +  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, position = position_dodge(0.9)) + 
  labs(
    #title = "Distribution of physical IPV cases by age group",
    x = "Age Group",
    y = "Distribution of IPV cases (%)",
    fill = "Data source"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("2020-2021 SINAN Vitoria Physical IPV" = "#4169E1")) +
  scale_y_continuous(limits = c(0, 75)) + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"),  
    axis.text = element_text(size = 12),  
    legend.position = c(0.85, 0.85),  
    legend.background = element_rect(fill = "white", color = "white"),  
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black") 
  )

#Save figure
ggsave("FILE_PATH/figS6_vit_sinan_dist.png", width = 8, height = 6, dpi = 300)

#################################################################################################################

#Read csv file for Figure S7
figS7_vit_pol_dist <- read.csv("FILE_PATH/figS7_vit_pol_dist.csv")

#Create the bar graph
ggplot(figS7_vit_pol_dist, aes(x = age, y = case_distribution * 100, fill = groupval)) +  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, position = position_dodge(0.9)) +  
    #title = "Distribution of sexual IPV cases by age group",
    x = "Age Group",
    y = "Distribution of IPV cases (%)",
    fill = "Data source"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("2020-2021 Civil Police Vitoria Physical IPV" = "#FF6A6A", 
                               "2020-2021 Civil Police Vitoria Sexual IPV" = "#800000")) +
  scale_y_continuous(limits = c(0, 75)) + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"),  
    axis.text = element_text(size = 12),  
    legend.position = c(0.85, 0.85),  
    legend.background = element_rect(fill = "white", color = "white"),  
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black") 
  )

#Save figure
ggsave("FILE_PATH/figS7_vit_pol_dist.png", width = 8, height = 6, dpi = 300)

#################################################################################################################

#Read csv file for Figure S8
fig3d_phy_pel <- read.csv("FILE_PATH/fig3d_phy_pel.csv")

# Filter the dataset
fig3d_phy_pel2 <- fig3d_phy_pel %>%
  filter(groupval == "2019 SINAN Pelotas")  

# Create the bar graph
ggplot(fig3d_phy_pel2, aes(x = age, y = case_distribution * 100, fill = groupval)) +  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, position = position_dodge(0.9)) + 
  labs(
    title = "Distribution of physical IPV cases by age group",
    x = "Age Group",
    y = "Distribution of physical IPV cases (%)",
    fill = "Data source"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("2019 SINAN Pelotas" = "#4169E1")) +
  scale_y_continuous(limits = c(0, 75)) + 
  theme(
    plot.title = element_text(size = 16, face = "bold"),  
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"),  
    axis.text = element_text(size = 12),  
    legend.position = c(0.85, 0.85),  
    legend.background = element_rect(fill = "white", color = "white"),  
    legend.title = element_text(size = 10),  
    legend.text = element_text(size = 8),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black") 
  )

# Save figure
ggsave("FILE_PATH/fig3d_phy_pel2.png", width = 8, height = 6, dpi = 300)

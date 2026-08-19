####################################################################################################################################################################
#
#Assessing methodological differences in estimating exposure to intimate-partner violence against women in Brazil: a multi-source observational analysis
#
####################################################################################################################################################################

####### Code for IPV in Brazil prevalence figures ####### 

library(ggplot2)
library(dplyr)
library(cowplot)
install.packages("magick")


#Read csv file for Figure 1
fig1_phys <- read.csv("FILE_PATH/fig1_phys.csv")

fig1_phys[] <- lapply(fig1_phys, function(x) {
  if (is.character(x)) {
    iconv(x, from = "", to = "UTF-8", sub = "")
  } else {
    x
  }
})

#Create the bar graph LENAD A
phys_a <- subset(fig1_phys, groupval == "2006 LENAD" | groupval == "2012 LENAD")
ggplot(phys_a, aes(x = age, y = prev * 100, fill = groupval)) +  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, position = position_dodge(0.9)) + 
  labs(
    title = "A Physical IPV prevalence by age group",
    x = "Age Group",
    y = "Prevalence (%)",
    fill = "Data source"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("2006 LENAD" = "#87CEEB", 
                               "2012 LENAD" = "#1E90FF"))+  #, "Group E" = "#4169E1")) +
  #scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_y_continuous(limits = c(0, 20)) + 
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

#Save the figure
ggsave("FILE_PATH/phys_a.png", width = 8, height = 6, dpi = 300)


#Create the bar graph DSR B
phys_b <- subset(fig1_phys, groupval == "2019 DSR")
ggplot(phys_b, aes(x = age, y = prev * 100, fill = groupval)) +  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, position = position_dodge(0.9)) + 
  labs(
    title = "B Physical IPV prevalence by age group",
    x = "Age Group",
    y = "Prevalence (%)",
    fill = "Data source"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("2019 DSR" = "#0000CD"))+  #, "Group E" = "#4169E1")) +
  #scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_y_continuous(limits = c(0, 20)) + 
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

#Save the figure
ggsave("FILE_PATH/phys_b.png", width = 8, height = 6, dpi = 300)


# Create the bar graph PNS C
phys_c <- subset(fig1_phys, groupval == "2019 PNS Physical IPV" | groupval == "2019 PNS Sexual IPV")
ggplot(phys_c, aes(x = age, y = prev * 100, fill = groupval)) +  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, position = position_dodge(0.9)) +
  labs(
    title = "C Physical and Sexual IPV prevalence by age group",
    x = "Age Group",
    y = "Prevalence (%)",
    fill = "Data source"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("2019 PNS Physical IPV" = "#4682B4", 
                               "2019 PNS Sexual IPV" = "darkblue"))+  #, "Group E" = "#4169E1")) +
  #scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_y_continuous(limits = c(0, 20)) + 
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

#Save the figure
ggsave("FILE_PATH/phys_c.png", width = 8, height = 6, dpi = 300)

#Combine three figures above in one and save in pdf

# Load your PNG files as images
img1 <- ggdraw() + draw_image("FILE_PATH/phys_a.png")  
img2 <- ggdraw() + draw_image("FILE_PATH/phys_b.png") 
img3 <- ggdraw() + draw_image("FILE_PATH/phys_c.png") 

#Combine the images (set ncol = 2 for horizontal arrangement)
combined_plot <- plot_grid(
  img1, img2,img3,      
  nrow = 3         
)

#Save the combined figure as a PDF file
ggsave("FILE_PATH/fig1combined.pdf", combined_plot, width = 12, height = 6)

#################################################################################################################

#Read csv file for Figure S1
figs1_vit <- read.csv("FILE_PATH/figs1_vit.csv")

ggplot(figs1_vit, aes(x = age, y = prev * 100, fill = groupval)) +  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, position = position_dodge(0.9)) +
  labs(
    title = "Physical and Sexual IPV prevalence by age group",
    x = "Age Group",
    y = "Prevalence (%)",
    fill = "Data source"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("2020-2021 Vitoria Physical IPV" = "#90EE90", 
                               "2020-2021 Vitoria Sexual IPV" = "green4")) +
  #scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_y_continuous(limits = c(0, 20)) + 
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

#Save the figure
ggsave("FILE_PATH/figs1_vit.png", width = 8, height = 6, dpi = 300)

#################################################################################################################

# Read csv file for Figure S2
figs2_pel <- read.csv("FILE_PATH/figs2_pel.csv")

ggplot(figs2_pel, aes(x = age, y = prev * 100, fill = groupval)) +  
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, position = position_dodge(0.9)) + 
  labs(
    title = "Physical and sexual IPV prevalence by age group",
    x = "Age Group",
    y = "Prevalence (%)",
    fill = "Data source"
  ) +
  theme_classic() +
  scale_fill_manual(values = c("2019 Pelotas Physical IPV" = "#7FFFD4", 
                               "2019 Pelotas Sexual IPV" = "#458B74")) +
  #scale_y_continuous(labels = scales::percent_format(scale = 1)) + 
  scale_y_continuous(limits = c(0, 20)) + 
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

# Save the figure
ggsave("FILE_PATH/figs2_pel.png", width = 8, height = 6, dpi = 300)

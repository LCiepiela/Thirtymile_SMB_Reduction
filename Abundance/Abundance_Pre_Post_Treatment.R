#Author: Lindsy Ciepiela
#Date: 10/07/2024
#Description: Script contains code for summarizing pre and post treatment fall parr abundance of steelhead

#########Load packages
#Libraries
library(plotly)
library(tidyverse)
library(htmlwidgets)
library("ggplot2")
library("ggpubr")
library("scales")
#######################

#############################
#############################
#Bring in the dataframes
setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/BaseData")
Alldensity <- read.csv("All.30M.CEDF.csv")

#Extract the data that will be used in the SMB BACI experiment
RBT_density <- Alldensity %>% 
  filter (SampleType == "Abundance", 
          AgeClass == "Age0andUp",
          Season == "Fall") %>%
  filter (Section == "MO_below"|
            Section == "MO_above"| 
            Section == "MO_bass"|
            Section == "PF3"|
            Section == "PF5")

unique(RBT_density$BassSection)

RBT_density$Section2 <- NA

#Relabel sections to the BACI design
RBT_density <- RBT_density %>% mutate (Section2 = case_when (
  BassSection == "PF3_pre" ~ "Control", 
  BassSection == "PF3_post" ~ "Control", 
  BassSection == "PF5_pre" ~ "Control", 
  BassSection == "PF5_post" ~ "Control", 
  BassSection == "MO_pre" ~ "Treatment", 
  BassSection == "MO_below" ~ "Below",
  BassSection == "MO_above" ~ "Treatment",
  TRUE ~ Section2
))


#Assign a phase to the data
RBT_density <- RBT_density %>% mutate (Time = case_when (
  Year == 2017 ~ "Pre", 
  Year == 2018  ~ "Pre", 
  Year == 2019 ~ "Pre", 
  Year == 2020 ~ "Impact", 
  Year == 2021 ~ "Impact +1", 
  Year == 2022 ~ "Post", 
  Year == 2023 ~ "Post"
))


RBT_density <- RBT_density %>% select (Year, SiteID, Section2, Time, Density_MeanCE)

#Export the dataframe
setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Abundance")
write.csv(RBT_density, "RBT_density.csv")



##~~~~~~~~~~~~~~~~...............~~~~~~~~~~~~~~~~~~~~~~~...........
##~~~~~~~~~~~~~~~~...............~~~~~~~~~~~~~~~~~~~~~~~...........#
#Start: Main plot for publication
RBT_density  <- RBT_density  %>%
  filter(Section2!= "Below") %>%
  mutate (Time = case_when(
    Time == "Impact +1" ~ "Post", 
    TRUE ~ Time
  ))


RBT_density$Year <- factor(RBT_density$Year, levels = c("2017", "2018", "2019", "2020", "2021", "2022", "2023"))

RBT_density$Section2 <- factor(RBT_density$Section2, levels = c("Control", "Treatment", "Below"))
RBT_density$Time <- factor(RBT_density$Time, levels = c("Pre", "Impact", "Impact +1", "Post"))


#PLOT OPTION 1
RBT_density <- RBT_density %>% filter (Section2 != "Below")

(Plot2 <- ggplot(RBT_density, aes(x = Time, y = Density_MeanCE, shape = Time)) +
  geom_boxplot(alpha = .5) +
  geom_point(aes(shape = Time), size = 3) +
  
   
  facet_wrap(~ Section2, scales = "fixed") +  
  ylim (0, 11.5)+

labs(
  x = "", 
  y = expression(Parr~density~(fish %.% m^-1)),
  fill = "Year",  
  color = "Year"
) +
  
  # THEME
  theme(legend.position = "top", 
          panel.spacing = unit(1, "lines"), # Adjusting spacing between facets for clarity
          aspect.ratio = 1)+ # Ensuring same aspect ratio for consistent box size
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),  
    strip.text = element_blank(),  # Remove facet labels
    legend.position = "top",
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(color = "black", size = 0.5),
    axis.ticks = element_line(color = "black", size = 0.5)
  ) +
    guides(shape = guide_legend(title = NULL)) +  # Remove title from the shape legend
  
  # COLOR
    scale_shape_manual(values = c("Pre" = 5, "Impact" = 17, "Impact +1" = 17 , "Post" = 1 )) +
    
    
  #ANNOTATE 
  geom_text(data = RBT_density %>% filter(Section2 == "Control"), 
            aes(x = Inf, y = Inf, label = "A.1 Control"), 
            hjust = 1.1, vjust = 1.6, size = 4, color = "black") +
  geom_text(data = RBT_density %>% filter(Section2 == "Treatment"), 
            aes(x = Inf, y = Inf, label = "B.1 Treatment"), 
            hjust = 1.1, vjust = 1.6, size = 4, color = "black")
)


setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Abundance")
ggsave("Density_Boxplot_Before_During_After.png", plot = Plot2, width = 7, height = 4, dpi = 300)

#End: Main plot for publication


##~~~~~~~~~~~~~~~~...............~~~~~~~~~~~~~~~~~~~~~~~...........
##~~~~~~~~~~~~~~~~...............~~~~~~~~~~~~~~~~~~~~~~~...........#
# Abundance statistical analysis used in manuscript

head(RBT_density)

RBT_density$Time <- factor(RBT_density$Time, levels = c("Pre", "Impact", "Post"))
RBT_density$Section2 <- factor(RBT_density$Section2, levels = c("Treatment", "Control"))


model <- lm(Density_MeanCE ~ Time * Section2, data = RBT_density)
summary(model)


library(emmeans)
emm <- emmeans(model, ~ Time * Section2)
contrast(emm, interaction = "pairwise")
summary(emm)



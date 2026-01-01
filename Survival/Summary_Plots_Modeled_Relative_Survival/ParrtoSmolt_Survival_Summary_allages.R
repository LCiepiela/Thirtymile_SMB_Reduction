#Lindsy Ciepiela
#10/01/2024
#Script for summarizing modeled and relative survival. 

library(tidyverse)

##########################################
##########################################
#Modeled Survival
#Load databases
setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Survival/Modeled_Survival/Outputs")
#Model runs with 9999 indicate PIT tag data from all years were compiled into one model run
ParrtoSmolt2017 <- readRDS("Parr_to_Smolt_allages2017.rds")
ParrtoSmolt2018 <- readRDS("Parr_to_Smolt_allages2018.rds")
ParrtoSmolt2019 <- readRDS("Parr_to_Smolt_allages2019.rds")
ParrtoSmolt2020 <- readRDS("Parr_to_Smolt_allages2020.rds")
ParrtoSmolt2021 <- readRDS("Parr_to_Smolt_allages2021.rds")
ParrtoSmolt2022 <- readRDS("Parr_to_Smolt_allages2022.rds")
ParrtoSmolt2023 <- readRDS("Parr_to_Smolt_allages2023.rds")

Summary <- rbind(ParrtoSmolt2017, ParrtoSmolt2018, ParrtoSmolt2019, ParrtoSmolt2020, ParrtoSmolt2021, ParrtoSmolt2022, ParrtoSmolt2023)


#Seperate into survival and detection DF
Survival  <- Summary[str_detect(Summary$Parameter, "^sship"),]
Detection <- Summary[str_detect(Summary$Parameter, "^p"),]


#PLOT SURVIVAL
Survival <- Survival %>% separate(MarkSite, c("MarkSite", "Season", "Location"), "_")

# Create new rows for each MarkSite and Season combination with Location = "Mark" and mean = 1
new_rows <- Survival %>%
  distinct(MarkSite, Season, ModelRun,Year) %>%
  mutate(Location = "Mark", mean = 1, X2.5. = NA, X97.5. = NA)  # Add other columns as needed

# Bind the new rows to the original dataframe
Survival <- bind_rows(Survival, new_rows)


unique(Survival$MarkSite)
Survival$MarkSite <- factor(Survival$MarkSite, levels = c("MOabove", "MObelow", "Per2", "Per3", "PF4", "Spring", "Spriing"))
Survival$Season <- factor(Survival$Season, levels = c("Spring", "Fall"))
Survival$Location <- factor(Survival$Location, levels = c("Mark", "JDD", "BON"))
Survival$Year <- factor(Survival$Year, levels = c("2017", "2018", "2019", "2020", "2021", "2022", "2023"))

Survival <- Survival %>% filter(Season == "Fall")

#Run the below code to pull out specific survival  model/site combos to look at allsites on one graph

ggplot(Survival, aes(Location, mean, group = MarkSite, 
                     col = MarkSite, 
                     shape = MarkSite)) +
  geom_point(size = 2.5) +
  geom_line() +
  # Uncomment the following line if you want to include error bars
  # geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), width = 0.1) +
  ylim(0, 1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3)) +
  scale_color_manual(values = c("#252525", "darkgrey", "#377eb8", "#4daf4a", "#e41a1c", "darkblue", "orange")) +  # Add more colors as needed
  scale_shape_manual(values = c(16, 17, 15, 18, 4, 19, 20)) +  # Adjust the number of shapes if needed
  # BASE SETTINGS
  theme_bw() +
  theme(text = element_text(size = 13)) +
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ylab("Survivorship") +
  ggtitle("Parr-to-smolt survival") +
  facet_wrap(~Season + Year)

SurvivalB <- Survival %>% filter(Location == "JDD", 
                                 MarkSite != "Per3",
                                 MarkSite !="PF4")

#Export the dataframe
setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Survival/Summary_Plots_Modeled_Relative_Survival")
write.csv(SurvivalB, "SurvivalB.csv")


(ModSurv <- ggplot(SurvivalB, aes(Year, mean, group = MarkSite, 
                     col = MarkSite, 
                     shape = MarkSite)) +
  geom_point(size = 2.5) +
  geom_line() +
  # Uncomment the following line if you want to include error bars
  # geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), width = 0.1) +
  ylim(0, 1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3)) +
  scale_color_manual(values = c("#252525", "#377eb8", "#4daf4a", "#e41a1c")) +  # Add more colors as needed
  scale_shape_manual(values = c(16, 17, 15, 18, 4)) +  # Adjust the number of shapes if needed
  # BASE SETTINGS
  theme_bw() +
  theme(text = element_text(size = 13)) +
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ylab("Survivorship") +
  ggtitle("Parr-to-smolt survival")
)


###############################
#############################
#Relative survival

#setwd("C:/Users/ciepieli/OneDrive/2024_Thirtymile/R/Relative_Survival")
#write.csv( Surv, "RelativeSurvival_Summary.csv")

(RelSurv <- ggplot(Surv, aes(mark.year, prop_out, group=mark.section, col=mark.section, shape = mark.section))+
  geom_line()+
  geom_point(size = 2.5)+
  labs(title = "Parr-to-smolt relative survival",
       x = "Year", 
       y = "Prop detected in Columbia") +
  theme_bw(base_size = 14) +  # Use theme_minimal with adjusted base text size for readability
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Center and bold title
    axis.title = element_text( size =12),  # Bold axis titles
    strip.text = element_text(size = 12, face = "bold"),  # Facet labels styled for readability
    legend.position = "top",  # Place the legend at the top for better clarity
    #panel.grid = element_blank(),  # Remove grid lines for a cleaner look
    axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust x-axis text if needed
    # Add x and y axis lines
    axis.line = element_line(color = "black", size = 0.5),  # Add axis lines
    # Add x and y axis tick marks
    axis.ticks = element_line(color = "black", size = 0.5)  # Add tick marks
  )+
  scale_color_manual(values = c("#252525", "#377eb8", "#4daf4a", "#e41a1c")) +  # Add more colors as needed
  scale_shape_manual(values = c(16, 17, 15, 18, 4))  # Adjust the number of shapes if needed
)



###############################
###############################
#Combine relative and mod survival data into one dataframe for plotting
setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Survival/Relative_Survival")
Surv <- read.csv("RelativeSurvival_Summary.csv")

head(Surv)
SurvB <- Surv %>% select (mark.year, mark.season, mark.section, prop_out) %>%
  rename (value = prop_out
          )
SurvB$ModelRun <- "Relative"
SurvB$X2.5. <- NA
SurvB$X97.5. <- NA

head(Survival)
SurvivalB <- Survival %>% 
  filter (Location == "JDD") %>%
  select (Year, Season, MarkSite, mean, X2.5., X97.5.) %>%
  rename (mark.year = Year, 
          mark.season = Season, 
          mark.section = MarkSite, 
          value = mean)
SurvivalB$ModelRun <- "Modeled"


SurvCombined <- rbind(SurvivalB, SurvB)
SurvCombined$ModelRun <- factor(SurvCombined$ModelRun, levels = c("Relative", "Modeled"))



SurvCombined <- SurvCombined %>% filter(mark.section != "Per3", 
                                        mark.section != "PF4") %>%
  filter(!(mark.year == "2020" & mark.section == "MObelow"), 
         !(mark.year == "2021" & mark.section == "MObelow"), 
         !(mark.year == "2022" & mark.section == "MObelow"), 
         !(mark.year == "2023" & mark.section == "MObelow")) %>%
  mutate(mark.section = case_when(
    mark.section == "Per2" ~ "Control", 
    mark.section == "MObelow" ~ "Treatment", 
    mark.section == "MOabove" ~ "Treatment"
  ))



SurvCombined$mark.section <- factor(SurvCombined$mark.section, levels = c("Control", "Treatment"))


####################################
###################################
#Primary plot for publication
#PLOT OPTION 1
SurvCombined$ModelRun <- factor(SurvCombined$ModelRun, levels = c("Relative", "Modeled"))

(SurvPlot <- ggplot(SurvCombined, aes(mark.year, value, group = mark.section, col = mark.section, shape = mark.section, linetype = mark.section)) +
    geom_line(size = .5) +
    geom_point(size = 2) +
   # Uncomment the following line if you want to include error bars
    geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), width = 0.05, linetype = "solid", size =.2) +
    
    labs(x = "", 
         y = "Parr-to-smolt survival") +
   
    facet_wrap(~ ModelRun, scales = "free_y", nrow = 2) +
    
    # THEME
    theme_bw(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12), 
      strip.text = element_blank(),  # Remove facet labels
      legend.position = "top",
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line = element_line(color = "black", size = 0.5),  # Keep axis lines
      axis.ticks = element_line(color = "black", size = 0.5)  # Keep tick marks
    ) +
    
    # COLOR AND SHAPE
    scale_color_manual(name = NULL, 
                       values = c("grey", "black"), 
                       labels = c("Control", "Treatment")) +
    scale_shape_manual(name = NULL,
                       values = c(16, 17), 
                       labels = c("Control", "Treatment")) +
    scale_linetype_manual(name = NULL,
                          values = c("dashed", "solid"), 
                          labels = c("Control", "Treatment")) +
   
    
    #ANNOTATE 
    geom_text(data = SurvCombined %>% filter(ModelRun == "Relative"), 
              aes(x = Inf, y = .3, label = "A. Relative"), 
              hjust = 1.2, vjust = 1.2, size = 4, color = "black") +
    geom_text(data = SurvCombined %>% filter(ModelRun == "Modeled"), 
              aes(x = Inf, y = 1.1, label = "B. Modeled"), 
              hjust = 1.2, vjust = 1.2, size = 4, color = "black")+
    
    # #ADD RECTANGLES
     geom_rect(data = SurvCombined %>% filter(ModelRun == "Modeled"& mark.year == 2017),
             aes(xmin = .5, xmax = 3.5, ymin = -.17, ymax = -.04),
             alpha = 0.3,
             color = "black",
             fill = "lightgrey",
               inherit.aes = FALSE) +
    
     geom_rect(data = SurvCombined %>% filter(ModelRun == "Modeled"& mark.year == 2017),
               aes(xmin = 3.5, xmax = 4.5, ymin = -.17, ymax = -.04),
               alpha = 0.3,
               color = "black",
               fill = "lightgrey",
               inherit.aes = FALSE) +
    
     # #geom_rect(data = SurvCombined %>% filter(ModelRun == "Modeled"& mark.year == 2017),
     #           aes(xmin = 4.5, xmax = 5.5, ymin = -.17, ymax = -.04),
     #           alpha = 0.3,
     #           color = "black",
     #           fill = "lightgrey",
     #           inherit.aes = FALSE) +
    
     geom_rect(data = SurvCombined %>% filter(ModelRun == "Modeled" & mark.year == 2017),
               aes(xmin = 4.5, xmax = 7.5, ymin = -0.17, ymax = -0.04),
               alpha = 0.3,
               color = "black",
               fill = "lightgrey",
               inherit.aes = FALSE)

     # Apply alpha via scale_fill_manual
     #scale_fill_manual(values = alpha(c("royalblue2", "red2", "gray"), c(0.3, 0.3, 0.3)))
  )



# Save the plot
setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Survival/Summary_Plots_Modeled_Relative_Survival")
ggsave("Survival.png", plot = SurvPlot, width = 7, height = 5, dpi = 300)

############

#####################################
#####################################
#Test for signifcant differences
#Run for both methods - the predicted survival and the observed relative survival
head(SurvCombined)

#Assign each year to a phase
SurvCombined <- SurvCombined %>%
  mutate(Phase = case_when(
    mark.year %in% c("2017", "2018", "2019") ~ "Pre",
    mark.year == "2020" ~ "Impact",
    mark.year %in% c("2021", "2022", "2023") ~ "Post",
    TRUE ~ NA_character_
  ))



#Modeled Survival
head(SurvCombined)

SurvCombined$Time <- factor(SurvCombined$Phase, levels = c("Pre", "Impact", "Post"))
SurvCombined$Section2 <- factor(SurvCombined$mark.section, levels = c("Treatment", "Control"))

SurvModeled <- SurvCombined %>% filter(
  ModelRun == "Modeled"
)

model <- lm(value ~ Phase * mark.section, data = SurvModeled)
summary(model)

#plot(model)

library(emmeans)
emm <- emmeans(model, ~ Phase * mark.section)
contrast(emm, interaction = "pairwise")
summary(emm)


#Run the stats on the relative survival

SurvRelative <- SurvCombined %>% filter(
  ModelRun == "Relative"
)

model <- lm(value ~ Phase * mark.section, data = SurvRelative)
summary(model)

#plot(model)

library(emmeans)
emm <- emmeans(model, ~ Phase * mark.section)
contrast(emm, interaction = "pairwise")
summary(emm)



head(SurvCombined)




##########
#Take a look at detection efficiency 
head(Detection)
Detection <- Detection %>%
  mutate(MarkSite = gsub("__", "_", MarkSite)) %>%  # Replace "__" with "_"
  separate(MarkSite, c("MarkSite", "Season", "Location"), "_")  # Separate based on "_"


ggplot(Detection, aes(Year, mean, group = Location, 
                      col = Location, 
                      shape = Location)) +
  geom_point(size = 2.5) +
  geom_line() +
  # Uncomment the following line if you want to include error bars
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), width = 0.1) +
  ylim(0, 1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3)) +
  scale_color_manual(values = c("#252525", "#377eb8")) +  # Add more colors as needed
  scale_shape_manual(values = c(16, 17, 15, 18, 4)) +  # Adjust the number of shapes if needed
  
  # BASE SETTINGS
  theme_bw() +
  theme(text = element_text(size = 13)) +
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ylab("Detection Efficiency") +
  ggtitle("Detection Efficiency")




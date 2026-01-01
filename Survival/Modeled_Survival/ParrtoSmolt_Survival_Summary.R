#Lindsy Ciepiela
#10/01/2024
#Script for summarizing traps CJS model output for Thirtymile survival models

library(tidyverse)

#Load databases
setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Survival/Modeled_Survival/Outputs")
#Model runs with 9999 indicate PIT tag data from all years were compiled into one model run

#Note - PIT tagged fish tagged across years were grouped. This provided a large enough sample size to investigate survival based on tagging section. 

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
Survival <- Survival %>%
  separate(MarkSite, c("MarkSite", "Season", "Location"), sep = "_") %>%
  mutate(
    MarkSite = if_else(MarkSite == "Spriing", "Spring", MarkSite)
  )


# Create new rows for each MarkSite and Season combination with Location = "Mark" and mean = 1
new_rows <- Survival %>%
  distinct(MarkSite, Season, ModelRun,Year) %>%
  mutate(Location = "Mark", mean = 1, X2.5. = NA, X97.5. = NA)  # Add other columns as needed

# Bind the new rows to the original dataframe
Survival <- bind_rows(Survival, new_rows)


unique(Survival$MarkSite)
Survival$MarkSite <- factor(Survival$MarkSite, levels = c("MObelow", "MOabove", "Per2", "Per3", "PF4", "Spring"))
Survival$Season <- factor(Survival$Season, levels = c("Spring", "Fall"))
Survival$Location <- factor(Survival$Location, levels = c("Mark", "JDD", "BON"))
Survival$Year <- factor(Survival$Year, levels = c("2017", "2018", "2019", "2020", "2021", "2022", "2023", "9999"))

Survival <- Survival %>% filter(Season == "Fall")

#Run the below code to pull out specific survival  model/site combos to look at allsites on one graph
#Note: 2017-2019 MO below contains samples from below and above the weir. 

ggplot(Survival, aes(Location, mean, group = interaction(MarkSite), 
                     col = interaction(MarkSite), 
                     shape = interaction(MarkSite))) +
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



SurvivalB <- Survival %>% filter(Location == "JDD")

ggplot(SurvivalB, aes(Year, mean, group = MarkSite, 
                     col = MarkSite, 
                     shape = MarkSite)) +
  geom_point(size = 2.5) +
  geom_line() +
  # Uncomment the following line if you want to include error bars
  # geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), width = 0.1) +
  ylim(0, 1) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.3)) +
  scale_color_manual(values = c("#252525", "darkgrey", "#377eb8", "#4daf4a", "#e41a1c")) +  # Add more colors as needed
  scale_shape_manual(values = c(16, 17, 15, 18, 4)) +  # Adjust the number of shapes if needed
  # BASE SETTINGS
  theme_bw() +
  theme(text = element_text(size = 13)) +
  theme(axis.text = element_text(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  ylab("Survivorship") +
  ggtitle("Parr-to-smolt survival")






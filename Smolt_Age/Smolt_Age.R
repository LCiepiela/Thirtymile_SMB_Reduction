#Author: Lindsy Ciepiela
#Date: 10/10/24
#Description: Script for examining smolt age in control and treatment reaches. 


#Load packages
library(tidyverse)
library(ggridges)

###################
###################
#Look at the length distribution of fish captured in the spring and fall in each year - to look for signs of 'carry-over effect' from 2020 removal. 

##Load thirtymile dataframe 

setwd("D:/U_Drive/30mile/R/Juveniles/Outputs")
Primary <- read.csv("Primary30Mile.csv")


#Filter to only include MO, PF3 and PF5
unique(Primary$Section)

Fish <- Primary %>% filter (Section == "MO_below"|
                              Section == "MO_above"| 
                              Section == "MO_bass"|
                              Section == "PF3"|
                              Section == "PF5"|
                              Section == "PF15"|
                              Section == "PF16"|
                              Section == "PU10"|
                              Section == "PU12"|
                              Section == "PU13"|
                              Section == "PU14"|
                              Section == "PF15"|
                              Section == "PF16") %>% 
  filter (Species == "RbT")


#For the purposes of this publication we will combine PF3 and PF5 datasets into one dataset and remove keep MO_below seperate because it is not used in the analysis


Fish <- Fish %>% mutate (Section = case_when (
  Section == "PF3" ~ "Control", 
  Section == "PF5" ~ "Control", 
  Section == "MO_bass" ~ "Treatment", 
  Section == "MO_above" ~ "Treatment", 
  Section == "MO_below" ~ "MO_below",
    Section == "PU10" ~ "Per3_PU",
    Section == "PU12" ~ "Per3_PU",
    Section == "PU13" ~ "Per3_PU",
    Section == "PU14" ~ "Per3_PU",
    Section == "PF15" ~ "Per3_PF",
    Section == "PF16" ~ "Per3_PF",
  TRUE ~ Section
))

unique(Fish$Section)



#for each year in the control and treatment reaches estimate the prop of the pop < 65 mm in the fall (i.e., not available for tagging)
Fish_u65 <- Fish %>% group_by(
  Section, Year, Season) %>%
  summarise(
    total_fish = n(),
    under_65mm = sum(Length < 65, na.rm = TRUE),
    prop_under_65mm = under_65mm / total_fish
  )



# Create the ridge plot

ggplot(Fish[Fish$Season == "Fall",], aes(x = Length, y = factor(Year), fill = Section)) +
  geom_density_ridges(alpha = 0.7, scale = 1.5) +  # Adjust alpha for transparency and scale for ridge height
  theme_minimal() +
  labs(x = "Mark Length (mm)", y = "Year", fill = "Section") +
  scale_fill_viridis_d() +  # Fill palette (can change as needed)
  facet_wrap(~Section)  # Facet by section, plotting different sections separately


Fish <- Fish %>% filter (Section == "Control" | Section == "Treatment")
ggplot(Fish[Fish$Season == "Fall",], aes(x = Length, y = factor(Year), fill = Section)) +
  geom_density_ridges(alpha = 0.7, scale = 1.5) +  # Adjust alpha for transparency and scale for ridge height
  theme_minimal() +
  labs(x = "Mark Length (mm)", y = "Year", fill = "Section") +
  scale_fill_viridis_d() +  # Fill palette (can change as needed)
  facet_wrap(~Section)  # Facet by section, plotting different sections separately


#Look at raw length frequency
ggplot(Fish[Fish$Season == "Fall",], aes(x = Length, fill = Section)) +
  geom_histogram(binwidth = 5, alpha = 0.7, position = "identity") +
  facet_wrap(~Section) +
  theme_minimal() +
  labs(x = "Mark Length (mm)", y = "Count", fill = "Section") +
  scale_fill_viridis_d()+
  geom_vline(xintercept = 65, linetype = "dashed", color = "red", size = 1) +
  facet_wrap (~Year)




#################
#################

#Look to see if we picked up on more fish holding over and waiting to outmigrate in MO. 
setwd("D:/2024_30Mile_Pub/R/Survival")
DF <- readRDS("Thirtymile_RMarkDF.rds")

DF <- DF %>% select (tag.code,
                     mark.section,
                     mark.sample.site,
                     mark.year, 
                     mark.season, 
                     mark.date, 
                     mark.length.mm, 
                     out_dsThirtymile, 
                     Initial.Det.Date, 
                     Initial.Det.Location) %>%
  filter (out_dsThirtymile == 1,
                     mark.season == "Fall")

DF$Initial.Det.Date <- as.Date(DF$Initial.Det.Date)
DF$mark.date <- as.Date(DF$mark.date)


DF$MarktoColumbia <- DF$Initial.Det.Date - DF$mark.date

DF_per3 <- DF %>% filter(MarktoColumbia<800, 
                        mark.section == "Per3")

ggplot(DF_per3, aes(mark.length.mm, MarktoColumbia, col = mark.section))+
  geom_point()+
  facet_wrap(.~mark.year)

DF <- DF %>% filter( 
                    mark.section != "Per3")






ggplot(DF, aes(mark.length.mm, MarktoColumbia, col = mark.section))+
  geom_point()+
  geom_vline(xintercept = 65, linetype = "dashed", color = "red", size = 1) +
  facet_wrap(.~mark.year)



#Calculate the proportion of fish that outmigrated > 365 day post tagging in each section and year. 
Prop_outmigrate_365 <- DF %>%
  group_by(mark.section, mark.year) %>%
  summarise(
    total_fish = n(),
    over_365_days = sum(MarktoColumbia > 365, na.rm = TRUE),
    prop_over_365 = over_365_days / total_fish
  )




#Assign the time to going to the columbia as 1 year or two year

DF$Outmigration_group <- NA
DF <- DF %>% mutate(Outmigration_group = case_when(
  MarktoColumbia < 300 ~ "Year 1", 
  MarktoColumbia > 300 ~ "Year 2"
))

DF$Outmigration_group <- factor(DF$Outmigration_group, levels = c("Year 1", "Year 2"))

ggplot(DF, aes(mark.length.mm, fill = Outmigration_group, group = Outmigration_group)) +
  geom_histogram(position = "identity", alpha = 0.6) +
  theme_minimal() +
  labs(x = "Mark Length (mm)", y = "Count", fill = "Mark to Columbia") +
  scale_fill_viridis_d() + # You can change the fill palette as needed
  facet_wrap (.~ mark.section+mark.year)


#Simplify plot to match publication summary:





ggplot(DF[DF$mark.section == "Per2",], aes(mark.length.mm, fill = Outmigration_group, group = Outmigration_group)) +
  geom_histogram(position = "identity", alpha = 0.6) +
  theme_minimal() +
  labs(x = "Mark Length (mm)", y = "Count", fill = "Mark to Columbia") +
  scale_fill_viridis_d() + # You can change the fill palette as needed
  facet_wrap (.~ mark.section+mark.year)


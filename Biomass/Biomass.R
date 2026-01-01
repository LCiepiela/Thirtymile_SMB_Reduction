#Author: Lindsy Ciepiela
#Description: Script for calculating biomass, generating figures, and conducting statistical tests to evaluate differences in biomass among groups.


#############################
#Bring in the dataframes
#############
library(tidyverse)

setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Abundance")

RBT_density <- read.csv("RBT_density.csv")
RBT_density <- RBT_density %>% filter (Section2 != "Below")

RBT_density <- RBT_density %>%
  mutate (SiteID = case_when(
    SiteID == "MO_A" ~ "MO_full" , 
    SiteID == "MO_C" ~ "MO_full" , 
    SiteID == "MO_C_3" ~ "MO_above1", 
    SiteID == "MO_C_4" ~ "MO_above2", 
    SiteID == "MO_D_2" ~ "MO_above1", 
    SiteID == "MO_D_3" ~ "MO_above2", 
    SiteID == "MO_D_4" ~ "MO_above3", 
    SiteID == "MO_D_5" ~ "MO_above4", 
    SiteID == "MO_E_3" ~ "MO_above1", 
    SiteID == "MO_E_4" ~ "MO_above2", 
    SiteID == "MO_E_5" ~ "MO_above3", 
    SiteID == "MO_F_3" ~ "MO_above1",  
    SiteID == "MO_F_4"  ~ "MO_above2", 
    SiteID == "MO_F_5" ~ "MO_above3",  
    SiteID == "MO_F_6" ~ "MO_above4", 
    SiteID == "PF3_A" ~ "PF3", 
    SiteID == "PF3_B" ~ "PF3", 
    SiteID == "PF5_A" ~ "PF5", 
    SiteID == "PF5_B" ~ "PF5", 
    SiteID == "PF5_D" ~ "PF5" 
  ))


##########################
#########################
#Calculate mean weight

setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/BaseData")
Base <- read.csv("Base_Fish_DF.csv")

unique(Base$SiteID)

Base <- Base %>% 
  filter(Species == "RbT")

#Update the base to follow the sampling scheme for Bass study

Base <- Base %>% mutate(Section = case_when(
  Year == 2020 & SiteID == "MO_C_1" ~ "MO_below", 
  Year == 2020 & SiteID == "MO_C_2" ~ "MO_below", 
  Year == 2020 & SiteID == "MO_C2" ~ "MO_below", 
  Year == 2020 & SiteID == "MO_C_3" ~ "Treatment", 
  Year == 2020 & SiteID == "MO_C_4" ~ "Treatment", 
  
  Year == 2021 & SiteID == "MO_D_1" ~ "MO_below", 
  Year == 2021 & SiteID == "MO_D_2" ~ "Treatment", 
  Year == 2021 & SiteID == "MO_D_3" ~ "Treatment", 
  Year == 2021 & SiteID == "MO_D_4" ~ "Treatment", 
  Year == 2021 & SiteID == "MO_D_5" ~ "Treatment", 
  
  Year == 2022 & SiteID == "MO_E_1" ~ "MO_below", 
  Year == 2022 & SiteID == "MO_E_2" ~ "MO_below", 
  Year == 2022 & SiteID == "MO_E_3" ~ "Treatment", 
  Year == 2022 & SiteID == "MO_E_4" ~ "Treatment", 
  Year == 2022 & SiteID == "MO_E_5" ~ "Treatment", 
  Year == 2022 & SiteID == "MO_OP_Above" ~ "Treatment", 
  Year == 2022 & SiteID == "MO_OP_Below" ~ "MO_below", 
  
  Year == 2023 & SiteID == "MO_F_1" ~ "MO_below", 
  Year == 2023 & SiteID == "MO_F_2" ~ "MO_below", 
  Year == 2023 & SiteID == "MO_F_3" ~ "Treatment", 
  Year == 2023 & SiteID == "MO_F_4" ~ "Treatment", 
  Year == 2023 & SiteID == "MO_F_5" ~ "Treatment",
  Year == 2023 & SiteID == "MO_F_6" ~ "Treatment",
  
  Year == 2017 & Section2 == "Control_Per1" ~ "Treatment", 
  Year == 2018 & Section2 == "Control_Per1" ~ "Treatment", 
  Year == 2019 & Section2 == "Control_Per1" ~ "Treatment", 
  
  
  Section2 == "Control_Per2" ~ "Control", 
  TRUE ~ Section
)) %>% 
  filter (Section2 != "MO_below")

unique(Base$SiteID)

Base <- Base %>% select(Year,SiteID, Season, Species, Length, Weight, Section)
#Assign before, during, residual and after to each year

Base <- Base %>%
  mutate (SiteID = case_when(
    SiteID == "MO_A" ~ "MO_full" , 
    SiteID == "MO_C" ~ "MO_full" , 
    SiteID == "MO_C_3" ~ "MO_above1", 
    SiteID == "MO_C_4" ~ "MO_above2", 
    SiteID == "MO_D_2" ~ "MO_above1", 
    SiteID == "MO_D_3" ~ "MO_above2", 
    SiteID == "MO_D_4" ~ "MO_above3", 
    SiteID == "MO_D_5" ~ "MO_above4", 
    SiteID == "MO_E_3" ~ "MO_above1", 
    SiteID == "MO_E_4" ~ "MO_above2", 
    SiteID == "MO_E_5" ~ "MO_above3", 
    SiteID == "MO_F_3" ~ "MO_above1",  
    SiteID == "MO_F_4"  ~ "MO_above2", 
    SiteID == "MO_F_5" ~ "MO_above3",  
    SiteID == "MO_F_6" ~ "MO_above4", 
    SiteID == "PF3_A" ~ "PF3", 
    SiteID == "PF3_B" ~ "PF3", 
    SiteID == "PF5_A" ~ "PF5", 
    SiteID == "PF5_B" ~ "PF5", 
    SiteID == "PF5_D" ~ "PF5" 
  ))

Base <- Base[!is.na(Base$SiteID),]

Base$Time <- NA

Base <- Base %>%
  mutate(Time = case_when(
    Year == 2017 ~ "Pre", 
    Year == 2018 ~ "Pre", 
    Year == 2019 ~ "Pre", 
    Year == 2020 ~ "Impact", 
    Year == 2021 ~ "Impact +1", 
    Year == 2022 ~ "Post", 
    Year == 2023 ~ "Post"
  ))


# First, create Mean_Weight dataframe with mean weight for each length
Mean_Weight <- Base %>%
  group_by(Length) %>%
  summarize(mean.weight = mean(Weight, na.rm = TRUE))

# Then, join the Base and Mean_Weight dataframes, and replace missing Weight values
Base <- Base %>%
  left_join(Mean_Weight, by = "Length") %>%  # Join on Length column
  mutate(Weight = coalesce(Weight, mean.weight)) %>%  # Fill missing Weight with mean weight
  select(-mean.weight)  # Remove the mean.weight column if not needed


#Calculate the average weight for each sampleing event. 


ggplot(Base, aes(Weight))+
  geom_histogram()+
  facet_wrap(Year~Section)

Weight_Summary <- Base %>% group_by(Time, SiteID, Year) %>%
  summarize (Weight_mean = mean(Weight, na.rm = TRUE), 
             Weight_sd = sd(Weight, na.rm = TRUE), 
             Weight_count = n())

##########
#Merge weight with density

head(RBT_density)
head(Weight_Summary)


Combined <- merge(RBT_density, Weight_Summary, by=c("Year", "Time", "SiteID"))


Combined$grams_per_meter <- Combined$Weight_mean * Combined$Density_MeanCE



#######################
#Add the Impact +1 to the post sampling period
Combined$Time[Combined$Time == "Impact +1"] <- "Post"


Combined$Section2 <- factor(Combined$Section2, levels = c("Control", "Treatment"))
Combined$Time <- factor(Combined$Time, levels = c("Pre", "Impact", "Post"))



(Plot2 <- ggplot(Combined, aes(x = Time, y = grams_per_meter, shape = Time)) +
   geom_boxplot(alpha = .5, outlier.shape = NA) +
   geom_point(aes(shape = Time), size = 3) +
    
    # Color points for 2021
    # geom_point(data = Combined[Combined$Year == "2021", ], 
    #            aes(x = Time, y = grams_per_meter), 
    #            shape = 19, color = "black", size = 3, alpha = .6) +  
   
   labs(
     x = "", 
     y = expression(Parr~biomass~(g %.% m^-1)),
     fill = "Year",  
     color = "Year"
   ) +
   
   # THEME
   theme(legend.position = "top", 
         panel.spacing = unit(1, "lines"), # Adjusting spacing between facets for clarity
         aspect.ratio = 1)+ # Ensuring same aspect ratio for consistent box size
   theme_bw(base_size = 12) +
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
    guides(shape = guide_legend( title = NULL))+
    
    scale_shape_manual(values = c("Pre" = 5, "Impact" = 17, "Impact +1" = 15 , "Post" = 1 )) + 
    facet_wrap(.~Section2)+
   
   # #ANNOTATE
    geom_text(data = Combined %>% filter(Section2 == "Control"),
              aes(x = Inf, y = 125, label = "A.2 Control"),
              hjust = 1.1, vjust = .5, size = 4, color = "black") +
    geom_text(data = Combined %>% filter(Section2 == "Treatment"),
              aes(x = Inf, y = 125, label = "B.2 Treatment"),
              hjust = 1.1, vjust = .5, size = 4, color = "black")
)


setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Biomass")
ggsave("Biomass_Boxplot_Before_During_After.png", plot = Plot2, width = 7, height = 4, dpi = 300)

#Export Biomass
setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Biomass")
write.csv(Combined, "Biomass.csv")


#############
#Alternate Plot option

(Plot2 <- ggplot(Combined, aes(x = Time, y = grams_per_meter, shape = Time)) +
  # Mean and 90% CI
  stat_summary(fun = mean, geom = "point", size = 3, color = "black") +
  stat_summary(fun.data = mean_cl_normal, fun.args = list(conf.int = 0.90), 
               geom = "errorbar", width = 0.2, color = "black") +
  #stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "black") +
  
  # Labels
  labs(
    x = "", 
    y = expression(Fish~biomass~(g %.% m^-1))
  ) +
  
  # Theme
  theme(legend.position = "top", 
        panel.spacing = unit(1, "lines"), # Adjusting spacing between facets for clarity
        aspect.ratio = 1)+ # Ensuring same aspect ratio for consistent box size
  theme_bw(base_size = 12) +
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
  guides(shape = guide_legend( title = NULL))+
  
  # Color and Shape
  scale_color_manual(values = c("Pre" = "blue", "Impact" = "red", "Impact +1" = "darkgray", "Post" = "darkblue")) +
  scale_shape_manual(values = c("Pre" = 1, "Impact" = 17, "Impact +1" = 15, "Post" = 5)) +
  
  # Annotations
  geom_text(data = Combined %>% filter(Section2 == "Control"),
            aes(x = Inf, y = 150, label = "A.1 Control"),
            hjust = 1.1, vjust = .5, size = 4, color = "black") +
  geom_text(data = Combined %>% filter(Section2 == "Treatment"),
            aes(x = Inf, y = 150, label = "B.1 Treatment"),
            hjust = 1.1, vjust = .5, size = 4, color = "black") +
  
  # Facet and Guides
  facet_wrap(. ~ Section2) +
  guides(shape = guide_legend(title = "Time Period"))  # Clearer legend title
)


#Simple length weight curve to look for outliers
Base$Time <- factor(Base$Time, levels = c("Before","During", "Residual", "After"))

ggplot(Base[Base$Section != "MO_below",], aes(Length, Weight, fill = Section, color = Section))+
  geom_point(alpha = .2)+
  facet_wrap(.~Time+Section, ncol=2)

##~~~~~~~~~~~~~~~~...............~~~~~~~~~~~~~~~~~~~~~~~...........
##~~~~~~~~~~~~~~~~...............~~~~~~~~~~~~~~~~~~~~~~~...........#


#####################################
#####################################
#Test for signifcant differences


head(Combined)

Combined$Time <- factor(Combined$Time, levels = c("Pre", "Impact", "Post"))
Combined$Section2 <- factor(Combined$Section2, levels = c("Treatment", "Control"))


model <- lm(grams_per_meter ~ Time * Section2, data = Combined)
summary(model)

plot(model)

library(emmeans)
emm <- emmeans(model, ~ Time * Section2)
contrast(emm, interaction = "pairwise")
summary(emm)




#Lindsy Ciepiela
#10/8/24
library(tidyverse)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Import survival by  year
setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Survival/Summary_Plots_Modeled_Relative_Survival")
Surv <- read.csv("SurvivalB.csv")
Surv$Year <- as.character(Surv$Year)
Surv$Base <- NA
Surv <- Surv %>% 
   filter((MarkSite == "MObelow" & Year == "2017")|
            (MarkSite == "MObelow" & Year == "2018")|
            (MarkSite == "MObelow" & Year == "2019")|
            (MarkSite == "MOabove" & Year == "2020")|
            (MarkSite == "MOabove" & Year == "2021")|
            (MarkSite == "MOabove" & Year == "2022")|
            (MarkSite == "MOabove" & Year == "2023")|
            (MarkSite == "Per2")
            ) %>%
  mutate(Base = case_when(
    Year == "2017" ~ "PrePost", 
    Year == "2018" ~ "PrePost", 
    Year == "2019" ~ "PrePost", 
    Year == "2020" ~ "During", 
    Year == "2021" ~ "PrePost", 
    Year == "2022" ~ "PrePost", 
    Year == "2023" ~ "PrePost", 
  )) %>%
  
  mutate(MarkSite = case_when(
    MarkSite == "MOabove" ~ "Treatment", 
    MarkSite == "MObelow" ~ "Treatment", 
    MarkSite == "Per2" ~ "Control"
  )) %>%
  rename (Section2 = MarkSite) %>%
  select (Year, Section2, Base, mean, X2.5., X97.5.)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Import abundance by year
setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Abundance")
Abund <- read.csv("RBT_density.csv")
Abund$Year <- as.character(Abund$Year)
Abund$Base <- NA

Abund <- Abund %>% 
  filter (Section2 == "Treatment"|
          Section2 == "Control") %>%
  mutate(Base = case_when(
    Time == "Impact" ~ "During", 
    Time == "Pre" ~ "PrePost", 
    Time == "Impact +1" ~ "PrePost", 
    Time == "Post" ~ "PrePost"
  ))

Abund_Summary <- Abund %>% 
  group_by(Year, Base, Section2) %>%
  summarize (Mean_Density_MeanCE = mean(Density_MeanCE), 
             SD_Density_MeanCE = sd(Density_MeanCE), 
             Count_Density_MeanCE = n())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#merge abundance and survival
head(Abund_Summary)
head(Surv)

Abund_Surv <- merge(Abund_Summary, Surv, by = c("Base", "Year", "Section2"))

#Calculate ending population
Abund_Surv$EndingPopulation <- (Abund_Surv$Mean_Density_MeanCE*1000)*Abund_Surv$mean

Abund_Surv$Time <- NA
Abund_Surv <- Abund_Surv %>% 
  mutate (Time = case_when (
    Year == 2017 ~ "Pre", 
    Year == 2018  ~ "Pre", 
    Year == 2019 ~ "Pre", 
    Year == 2020 ~ "Impact", 
    Year == 2021 ~ "Impact +1", 
    Year == 2022 ~ "Post", 
    Year == 2023 ~ "Post"
  ))

#In 2020 we effectively split the MO into two sections when we installed the wier. MO_above was considered the treatment area.

Abund_Surv <- Abund_Surv %>%
  filter(Section2!= "Below") %>%
  mutate (Time = case_when(
    Time == "Impact +1" ~ "Post", 
    TRUE ~ Time
  ))

Abund_Surv$Time <- factor(Abund_Surv$Time, levels = c("Pre", "Impact", "Impact +1", "Post"))

(Base_SmoltProduction <- ggplot(Abund_Surv, aes(x = Time, y = EndingPopulation, shape = Time)) +
    geom_boxplot(alpha = .5) +
    geom_point(aes(shape = Time), size = 3) +
    #ylim (0, 11.5)+
    
    labs(
      x = "", 
      y = expression("Smolts produced" ~ "\u00B7" ~ 1 ~ km^-1 ~ "(thousand)"),
      fill = "Year",  
      color = "Year"
    ) +
    scale_y_continuous(labels = scales::comma_format(scale = 1e-3))+
    
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
  
    scale_shape_manual(values = c("Pre" = 1, "Impact" = 17, "Impact +1" = 5 , "Post" = 5 )) + 
    facet_wrap(.~Section2)+
    
    # #ANNOTATE
    geom_text(data = Abund_Surv %>% filter(Section2 == "Control"),
              aes(x = Inf, y = 4000, label = "A. Control"),
              hjust = 1.1, vjust = .5, size = 4, color = "black") +
    geom_text(data = Abund_Surv %>% filter(Section2 == "Treatment"),
              aes(x = Inf, y = 4000, label = "B. Treatment"),
              hjust = 1.1, vjust = .5, size = 4, color = "black")
)


setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Survival/Theortical_Survival_Abundance")
ggsave("SmoltProduction.png", plot = Base_SmoltProduction, width = 3.35, height = 4, dpi = 300)

# setwd("C:/Users/ciepieli/OneDrive/2024_Thirtymile/R/PublicationFigures")
# ggsave("SmoltProduction_wide.png", plot = Base_SmoltProduction, width = 7, height = 4, dpi = 300)


#Export smolt production
setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Survival/Theortical_Survival_Abundance")
write.csv(Abund_Surv, "Abund_Surv.csv")


#####################################
#####################################
#Test for signifcant differences
head(Abund_Surv)

Abund_Surv$Time <- factor(Abund_Surv$Time, levels = c("Pre", "Impact", "Post"))
Abund_Surv$Section2 <- factor(Abund_Surv$Section2, levels = c("Treatment", "Control"))


model <- lm(EndingPopulation ~ Time * Section2, data = Abund_Surv)
summary(model)

plot(model)

library(emmeans)
emm <- emmeans(model, ~ Time * Section2)
contrast(emm, interaction = "pairwise")
summary(emm)


#####################################
#####################################
#Extract quantiles
Quantile.50 <- quantile(Abund_Surv$EndingPopulation[Abund_Surv$Base == "PrePost"], .5)
Quantile.975 <- quantile(Abund_Surv$EndingPopulation[Abund_Surv$Base == "PrePost"], .975)
Quantile.025 <- quantile(Abund_Surv$EndingPopulation[Abund_Surv$Base == "PrePost"], .025)

mean_endingpop.treat <- mean(Abund_Surv$EndingPopulation[Abund_Surv$Base == "PrePost" & Abund_Surv$Section2 == "Treatment"])
min_endingpop.treat <- min(Abund_Surv$EndingPopulation[Abund_Surv$Base == "PrePost" & Abund_Surv$Section2 == "Treatment"])
max_endingpop.treat <- max(Abund_Surv$EndingPopulation[Abund_Surv$Base == "PrePost" & Abund_Surv$Section2 == "Treatment"])


mean_endingpop.cont <- mean(Abund_Surv$EndingPopulation[Abund_Surv$Base == "PrePost" & Abund_Surv$Section2 == "Control"])
min_endingpop.cont <- min(Abund_Surv$EndingPopulation[Abund_Surv$Base == "PrePost" & Abund_Surv$Section2 == "Control"])
max_endingpop.cont <- max(Abund_Surv$EndingPopulation[Abund_Surv$Base == "PrePost" & Abund_Surv$Section2 == "Control"])


EndingPop_Treatment <- Abund_Surv$EndingPopulation[Abund_Surv$Base == "During" & Abund_Surv$Section2 == "Treatment"]
EndingPop_Cont <- Abund_Surv$EndingPopulation[Abund_Surv$Base == "During" & Abund_Surv$Section2 == "Control"]

#~~~~~~~~~~~~~~~data.frame()#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create a theortical dataframe that computes the ending population based on a range of starting population estimates and survival estimates


#Script for comparing the relationship between fall abundance and survival

# Create a vector of starting populations from 1250 to 7000, incrementing by 250
starting_population <- seq(0, 9000, by = 250)

# Create a vector of survival estimates from 0.05 to 0.75, incrementing by 0.05
survival_estimates <- seq(0.1, 0.7, by = 0.05)

# Create a dataframe using expand.grid to get all combinations of starting populations and survival estimates
DF <- expand.grid(StartingPopulation = starting_population, Survival = survival_estimates)

# Display the first few rows of the dataframe
head(DF)

DF$EndingPopulation <- DF$StartingPopulation * DF$Survival

DF$Survival <- as.factor(DF$Survival)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Plot Option 1 ####
#########

DF$Survival <- as.numeric(as.character(DF$Survival))
#LINE
# Create the heatmap
ggplot(DF, aes(x = Survival, y = StartingPopulation)) +
  geom_tile(aes(fill = EndingPopulation)) +
  scale_fill_gradient(low = "blue", high = "red") +
  
  # Calculate the max StartingPopulation for each Survival where EndingPopulation <= 937
  geom_line(data = DF %>%
              group_by(Survival) %>%
              filter(EndingPopulation <= 937) %>%
              slice_max(EndingPopulation, with_ties = FALSE), 
            aes(x = Survival, y = StartingPopulation), 
            color = "yellow", size = .5) +
  geom_point(data = DF %>%
              group_by(Survival) %>%
              filter(EndingPopulation <= 937) %>%
              slice_max(EndingPopulation, with_ties = FALSE), 
            aes(x = Survival, y = StartingPopulation), 
            color = "yellow", size = 3) +
  
  theme_minimal(base_size = 14) +  # Use theme_minimal with adjusted base text size for readability
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, family = "Arial"),  # Center and bold title
    axis.title = element_text(family = "Arial", size =12),  # Bold axis titles
    strip.text = element_text(size = 12, face = "bold", family = "Arial"),  # Facet labels styled for readability
    legend.position = "top",  # Place the legend at the top for better clarity
    panel.grid = element_blank(),  # Remove grid lines for a cleaner look
    axis.text.x = element_text(angle = 45, hjust = 1),  # Adjust x-axis text if needed
    axis.line = element_line(color = "black", linewidth = 0.5),  # Add axis lines
    axis.ticks = element_line(color = "black", linewidth = 0.5)  # Add tick marks
  ) +
  
  # Add a specific point at Survival = 0.44 and Starting Population = 7000
  annotate("point", x = 0.44, y = 7000, color = "limegreen", size = 4, shape = 15) +
  
  # Add a specific point at Survival = 0.65 and Starting Population = 1250
  annotate("point", x = 0.65, y = 1250, color = "limegreen", size = 4, shape = 15) +
  
  # Increase the number of tick marks on the y-axis
  scale_y_continuous(breaks = seq(min(DF$StartingPopulation), max(DF$StartingPopulation), by = 500)) + 
  scale_x_continuous(breaks = seq(min(DF$Survival), max(DF$Survival), by = 0.1)) + 
  
  # Add labels and title
  labs(
    title = "Ending Population as a Function of Survival and Starting Population",
    x = "Survival Rate",
    y = "Starting Population",
    fill = "Ending Population"
  ) +
  theme_minimal()


#########

# Create the heatmap with a custom color scale of treatment
(Plot1 <- ggplot(DF, aes(x = Survival, y = StartingPopulation)) +
  geom_tile(aes(fill = EndingPopulation)) +
  
  # Define custom color gradient that breaks at 937 with white at the breakpoint
  scale_fill_gradientn(
    colors = c("blue", "white", "red"),  # Blue for low values, white at the break, red for high values
    values = scales::rescale(c(min_endingpop.treat, max_endingpop.treat, max(DF$EndingPopulation))),  # Break at 937
    limits = c(min(DF$EndingPopulation), max(DF$EndingPopulation)),  # Set limits of the color scale
    oob = scales::squish   # Handle out-of-bound values by squishing them into the range
  ) +
  
  # Calculate the max StartingPopulation for each Survival where EndingPopulation <= 937
  geom_line(data = DF %>%
              group_by(Survival) %>%
              filter(EndingPopulation < min_endingpop.treat) %>%
              slice_max(EndingPopulation, with_ties = FALSE), 
            aes(x = Survival, y = StartingPopulation), 
            color = "black", size = .7, 
            lty = "dashed") +
   
   geom_line(data = DF %>%
               group_by(Survival) %>%
               filter(EndingPopulation < max_endingpop.treat) %>%
               slice_max(EndingPopulation, with_ties = FALSE), 
             aes(x = Survival, y = StartingPopulation), 
             color = "black", size = .7, 
             lty = "dashed") +
    geom_line(data = DF %>%
                group_by(Survival) %>%
                filter(EndingPopulation < mean_endingpop.treat) %>%
                slice_max(EndingPopulation, with_ties = FALSE), 
              aes(x = Survival, y = StartingPopulation), 
              color = "black", size = .7, 
              lty = "solid") +
   
   
   # Calculate the max StartingPopulation for each Survival where EndingPopulation <= 937
   geom_line(data = DF %>%
               group_by(Survival) %>%
               filter(EndingPopulation < (min_endingpop.cont-10)) %>%
               slice_max(EndingPopulation, with_ties = FALSE), 
             aes(x = Survival, y = StartingPopulation), 
             color = "grey", size = .7, 
             lty = "dashed") +
   
   geom_line(data = DF %>%
               group_by(Survival) %>%
               filter(EndingPopulation < max_endingpop.cont) %>%
               slice_max(EndingPopulation, with_ties = FALSE), 
             aes(x = Survival, y = StartingPopulation), 
             color = "grey", size = .7, 
             lty = "dashed") +
   geom_line(data = DF %>%
               group_by(Survival) %>%
               filter(EndingPopulation < mean_endingpop.cont) %>%
               slice_max(EndingPopulation, with_ties = FALSE), 
             aes(x = Survival, y = StartingPopulation), 
             color = "grey", size = .7, 
             lty = "solid") +
   
  # Add additional plot elements and formatting
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12),
    legend.position = "right"
  ) +
  
  # Add labels and title
  labs(
    #title = "Ending Population as a Function of Survival and Starting Population",
    x = "Survival Rate",
    y = "Starting Population",
    fill = "Ending Population"
  )+
  
  # Add annotation text for compensatory and additive mortality
  # annotate("text", x = 0.65, y = 7800, 
  #          label = "Additive\nMortality", color = "black", size = 3, hjust = .5) +
  #   
  # annotate("text", x = 0.18, y = 1500, 
  #            label = "Compensatory\nMortality", color = "black", size = 3, hjust = .5) +
  
  # Add key points of interest in limegreen
  annotate("point", x = 0.4265412, y = 8.0498024*1000, color = "black", size = 3, shape = 17) +
   annotate("point", x = 0.2687439, y = 2.7640950*1000, color = "grey", size = 3, shape = 19) +
  
  # Adjust tick marks for clarity
  scale_y_continuous(breaks = seq(min(DF$StartingPopulation), max(DF$StartingPopulation), by = 2000)) + 
  scale_x_continuous(breaks = seq(min(DF$Survival), max(DF$Survival), by = 0.2)) 
)

setwd("C:/Users/ciepieli/OneDrive/2024_Thirtymile/R/PublicationFigures")
ggsave("Theoretical.png", plot = Plot1, width = 7, height = 4, dpi = 300)


#####################
######################
#Alternative plot
# Create the heatmap with a custom color scale
(Plot2 <- ggplot(DF, aes(x = Survival, y = StartingPopulation)) +
    geom_tile(aes(fill = EndingPopulation)) +
    
    # Define custom color gradient that breaks at 800 with an additional color
    scale_fill_gradientn(
      colors = c("blue", "white", "orange", "red"),  # Add "orange" between white and red
      values = scales::rescale(c(min(DF$EndingPopulation), 300, 1000, 3000, max(DF$EndingPopulation))),  # New values to match the colors
      limits = c(min(DF$EndingPopulation), max(DF$EndingPopulation)),  # Set limits of the color scale
      oob = scales::squish , # Handle out-of-bound values by squishing them into the range
      breaks = c(1000,  3000,  5000, 7000),  # Set the breaks for the legend
      labels = c("1000", "3000", "5000", "7000")  # Corresponding labels for the breaks
    ) +
    
   #BEFORE
    # Calculate the max StartingPopulation for each Survival where EndingPopulation <= 937
    geom_line(data = DF %>%
                group_by(Survival) %>%
                filter(EndingPopulation <= 700) %>%
                slice_max(EndingPopulation, with_ties = FALSE), 
              aes(x = Survival, y = StartingPopulation), 
              color = "royalblue2", size = .7, linetype = "dashed") +
   # #geom_point(data = DF %>%
   #               group_by(Survival) %>%
   #               filter(EndingPopulation <= 700) %>%
   #               slice_max(EndingPopulation, with_ties = FALSE), 
   #             aes(x = Survival, y = StartingPopulation), 
   #             color = "royalblue2", size = 1.5) +
   # 
   
   
   #AFTER
   # Calculate the max StartingPopulation for each Survival where EndingPopulation <= 937
   geom_line(data = DF %>%
               group_by(Survival) %>%
               filter(EndingPopulation <= 450.1) %>%
               slice_max(EndingPopulation, with_ties = FALSE), 
             aes(x = Survival, y = StartingPopulation), 
             color = "royalblue2", size = .7, linetype = "dashed") +
   # geom_point(data = DF %>%
   #              group_by(Survival) %>%
   #              filter(EndingPopulation <= 450.1) %>%
   #              slice_max(EndingPopulation, with_ties = FALSE), 
   #            aes(x = Survival, y = StartingPopulation), 
   #            color = "royalblue2", size = 1.5) +
   # 
   
  
   # THEME
   theme(panel.spacing = unit(1, "lines"), # Adjusting spacing between facets for clarity
         aspect.ratio = 1)+ # Ensuring same aspect ratio for consistent box size
   theme_bw(base_size = 12) +
   theme(
     plot.title = element_text(hjust = 0.5, size = 16),
     axis.title = element_text(size = 12),
     strip.text = element_blank(),  # Remove facet labels
     legend.position = "right",
     panel.grid = element_blank(),
     axis.text.x = element_text(angle = 45, hjust = 1),
     axis.line = element_line(color = "black", size = 0.5),
     axis.ticks = element_line(color = "black", size = 0.5)
   ) +
    
    # Add labels and title
    labs(
      x = "Survival Rate",
      y = "Starting Population",
      fill = "Ending Population"
    ) +
    
    # Add annotation text for compensatory and additive mortality
    annotate("text", x = 0.18, y = 1200, 
             label = "Compensatory\nMortality", color = "blue", size = 4, hjust = .5) +
    annotate("text", x = 0.43, y = 5250, 
             label = "Additive\nMortality", color = "red", size = 4, hjust = .5) +
    
    # Add key points of interest
    annotate("point", x = 0.45, y = 8.00 * 1000, color = "red2", size = 4, shape = 17) + #During
    annotate("point", x = 0.7, y = 1 * 1000, color = "royalblue2", size = 4, shape = 17) + #Before
    annotate("point", x = 0.15, y = 3.5 * 1000, color = "gray40", size = 4, shape = 17) + #Residual
    annotate("point", x = 0.45, y = 1 * 1000, color = "royalblue2", size = 4, shape = 17) + #After
    
    
    # Adjust tick marks for clarity
    scale_y_continuous(breaks = seq(min(DF$StartingPopulation), max(DF$StartingPopulation), by = 1000)) + 
    scale_x_continuous(breaks = seq(min(DF$Survival), max(DF$Survival), by = 0.2))
 




)

setwd("C:/Users/ciepieli/OneDrive/2024_Thirtymile/R/PublicationFigures")
ggsave("Theoretical.png", plot = Plot2, width = 7, height = 5, dpi = 300)


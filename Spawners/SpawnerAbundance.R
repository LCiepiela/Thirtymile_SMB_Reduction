#Author: Lindsy Ciepiela
#Date: 10/07/2024
#Description: Script contains code for summarizing redd abundance in control and treatment in thirtymile from 2017-2022
############################################################################
############################################################################
library(tidyverse)

#Load spawners dataframe
setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/BaseData")
Spawners <- read.csv("SpawnerSummaryForR.csv")


#Calculate the density of redds (adult/m)
Spawners$Adult_p_meter <- (Spawners$ReddCount)/Spawners$Length

#Relable siteID for the control/treatment reaches 
head(Spawners)
unique(Spawners$SiteID)

Spawners$Section <- NA
Spawners <- Spawners %>% mutate(Section = case_when(
  SiteID == "MO" ~ "Treatment", 
  SiteID == "PF5" ~ "Control", 
  SiteID == "PF3" ~ "Control", 
  TRUE ~ Section
  ))
  
Spawners$Time <- NA
Spawners <- Spawners %>% mutate (Time = case_when (
  Year == 2017 ~ "Before", 
  Year == 2018  ~ "Before", 
  Year == 2019 ~ "Before", 
  Year == 2020 ~ "During", 
  Year == 2021 ~ "Residual", 
  Year == 2022 ~ "After", 
  Year == 2023 ~ "After"
))

############
#Plot of the total escapement to each site
#Set base plot setting to add to plots: 
#This plot is used in the report'
Lindsy<-theme_bw()+ theme(axis.text.x=element_text(size=11, colour="black", angle=-45, hjust=.2, vjust=.4),
                          axis.text.y=element_text(size=11, colour="black"),
                          strip.text = element_text(size=12,color = "white", face = "bold"),
                          #strip.background = element_rect(fill = "grey20"),
                          axis.title.y=element_text(size=12),
                          axis.title.x=element_text(size=12),
                          #panel.grid.minor = element_blank(),
                          #panel.grid.major = element_blank(),
                          #panel.background = element_rect(fill = "white", color = "grey20"),
                          plot.title = element_text(size=12, face = "bold"),
                          panel.border = element_rect(colour = "black"),
                          plot.margin = margin(t = 20, r = 30, b = 20, l = 30))
head(Spawners)



Spawners$SiteID <- factor(Spawners$SiteID, levels = c("MO", "PF3", "PF4", "PF5", "PU10", "PU11", "PU12", "PU13","PU14", "PU15", "PF15", "PF16", "EF1", "EF2", "EF6", "EU7", "EU9"))

ggplot(Spawners, aes(Year, Adult_p_meter, fill=SiteID))+
  geom_point()+
  geom_line()+
  facet_wrap (.~SiteID, ncol = 3)+
  ylab("Adult per meter") +
  xlab("Year") +
  guides(fill="none")+
  Lindsy


##~~~~~~~~~~~~~~~~...............~~~~~~~~~~~~~~~~~~~~~~~...........
##~~~~~~~~~~~~~~~~...............~~~~~~~~~~~~~~~~~~~~~~~...........#
#Start: Main plot for publication
#PLOT OPTION 1
Spawners <- Spawners %>% filter (Section == "Control"|
                                   Section == "Treatment", 
                                 Year <=2023)




Spawners$Time <- factor(Spawners$Time, levels = c("Before", "During", "Residual", "After"))

(Plot2 <- ggplot(Spawners, aes(x = Time, y = Adult_p_meter, fill = Time, Color = Time)) +
    geom_boxplot(alpha = .5) +
    geom_point(aes(color = Time), alpha = 0.6, size = 3) +
    facet_wrap(~ Section, scales = "fixed") +  
   # ylim (0, 11.5)+
    
    labs(
      x = "", 
      y = "Spawner abundance (fish/m)",
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
      strip.text = element_blank(),  # Remove facet labels
      legend.position = "top",
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line = element_line(color = "black", size = 0.5),
      axis.ticks = element_line(color = "black", size = 0.5)
    ) +
    
    # COLOR
    scale_color_manual(values = c("Before" = "royalblue2", "During" = "red2", "Residual" = "gray", "After" = "royalblue2" )) +
    scale_fill_manual(values = c("Before" = "royalblue2", "During" = "red2", "Residual" = "gray", "After" = "royalblue2")) +
    
    #ANNOTATE 
    geom_text(data = Spawners %>% filter(Section == "Control"), 
              aes(x = Inf, y = Inf, label = "A. Control"), 
              hjust = 1.1, vjust = 1.6, size = 4, color = "black") +
    geom_text(data = Spawners %>% filter(Section == "Treatment"), 
              aes(x = Inf, y = Inf, label = "B. Treatment"), 
              hjust = 1.1, vjust = 1.6, size = 4, color = "black")
)


  

#############
#Pot 2 look at abundance over time.
#Summarize spawners to get the min, mean, and max for control sites
Summary <- Spawners %>% group_by(Section, Year) %>%
  summarize (Adult_p_meter_mean = mean(Adult_p_meter), 
             Adult_p_meter_min = min(Adult_p_meter),
             Adult_p_meter_max = max(Adult_p_meter))


Summary$Year <- factor(Summary$Year, levels = c("2017", "2018", "2019", "2020", "2021", "2022", "2023", "9999"))

Summary$Section <- factor(Summary$Section, levels = c("Control", "Treatment"))

(SpawnerPlot <- ggplot(Summary, aes(Year, Adult_p_meter_mean, group = Section, col = Section, shape = Section, linetype = Section)) +
   geom_line(size = .5) +
   geom_point(size = 2) +
   
   labs(x = "", 
        y = "Spawner density (fish/m)") +
   
   # THEME
   theme_bw(base_size = 14) +
   theme(
     plot.title = element_text(hjust = 0.5, size = 16),
     axis.title = element_text(size = 12),
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
                      values = c(16,17), 
                      labels = c("Control", "Treatment")) +
   scale_linetype_manual(name = NULL,
                         values = c("dashed", "solid"), 
                         labels = c("Control", "Treatment")) 
   
 
#    #ADD RECTANGLES
#    geom_rect(data = Summary %>% filter(Year == 2017), 
#              aes(xmin = .5, xmax = 3.5, ymin = -.005, ymax = 0), 
#              fill = "royalblue2", alpha = 0.3, 
#              inherit.aes = FALSE) +
#    
#    geom_rect(data = Summary %>% filter(Year == 2017), 
#              aes(xmin = 3.5, xmax = 4.5, ymin = -.005, ymax = 0), 
#              fill = "red2", alpha = 0.3, 
#              inherit.aes = FALSE) +
#    
#    geom_rect(data = Summary %>% filter(Year == 2017), 
#              aes(xmin = 4.5, xmax = 5.5, ymin = -.005, ymax = 0), 
#              fill = "gray", alpha = 0.3, 
#              inherit.aes = FALSE) +
#    
#    geom_rect(data = Summary %>% filter(Year == 2017), 
#              aes(xmin = 5.5, xmax = 7.5, ymin = -0.005, ymax = 0),
#              alpha = 0.3,
#              fill = "royalblue2", 
#              inherit.aes = FALSE)+
#    
#    # Apply alpha via scale_fill_manual
#    scale_fill_manual(values = alpha(c("royalblue2", "red2", "gray"), c(0.3, 0.3, 0.3)))
)

# Save the plot
setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Spawners")
ggsave("Spawners.png", plot = SpawnerPlot , width = 8, height = 4, dpi = 300)



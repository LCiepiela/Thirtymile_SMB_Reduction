#Juvenile abundance#############################

#############
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
Combined <- read.csv("RBT_Biomass.csv")

Combined$Section2 <- factor(Combined$Section2, levels = c("Control", "Treatment"))
Combined$Time <- factor(Combined$Time, levels = c("Pre", "Impact", "Impact +1", "Post"))
#Combined <- Combined %>% filter (Section == "Treatment")


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


setwd("D:/2024_30Mile_Pub/R/PublicationFigures")
ggsave("Biomass_Boxplot_Before_During_Residual_After_B.png", plot = Plot2, width = 7, height = 4, dpi = 300)


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


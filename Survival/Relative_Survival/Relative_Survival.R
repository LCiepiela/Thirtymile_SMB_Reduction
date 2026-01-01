###############################
#Lindsy Ciepiela
#Code for calculating relative survival in control and treatment reaches
#############################
#Load packages
library(tidyverse)

#Relative survival

setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Survival/RMark_DataFrame")

# read in raw data file and view it
dat = readRDS("Thirtymile_RMarkDF.rds")
dat$mark.siteseason <- paste(dat$mark.section, dat$mark.season, sep = "_")


###Relative Survival
#Filter to only include data for SMB study
unique(dat$mark.section)

dat0 <- dat %>% filter (mark.section == "MO_above" | 
                          mark.section == "MO_below" |
                          mark.section == "Per2" ,
                          mark.season == "Fall") %>% 
  mutate(mark.section = case_when (
    mark.section == "MO_above" ~ "MOabove", 
    mark.section == "MO_below" ~ "MObelow", 
    TRUE ~ mark.section
  ))

dat0$out.JDDorBON <- 0
dat0 <- dat0  %>% mutate (out.JDDorBON = case_when(
  out.JDD + out.Bonneville >=1 ~ 1, 
  TRUE ~ out.JDDorBON
))

Surv <- dat0 %>% group_by(mark.year, mark.season, mark.section) %>%
  summarize (num_tag = n(), 
             out_dsThirtymile = sum(out_dsThirtymile))

Surv$prop_out <- Surv$out_dsThirtymile/Surv$num_tag

setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Survival/Relative_Survival")
write.csv(Surv, "RelativeSurvival_Summary.csv")



#Author: Lindsy Ciepiela 
#Date: 6/24/2022
#Description: Code for prepping PESCA (PIT) data for survival model. PESCA is a database maintained by ODFW and contains PIT tag data for the John Day Research Office. 

library(tidyverse)

#><((((?>`?.??.???`?.?.???`?...?><((((?>
#Load PIT data
setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/BaseData/PIT_Data")

#Mark database
Mark <- readRDS("marks.rds")
Mark$mark.lifestage <- "juvenile"
Mark$mark.date <- format(Mark$date, format = "%Y-%m-%d")
Mark$mark.capturemethod <- Mark$capture.method


Mark$mark.month <- format(Mark$date, format = "%m")
Mark$mark.year <-  format(Mark$date, format = "%Y")
Mark$mark.jd <-  format(Mark$date, format = "%j")


#Select relevant columns from Marking database
Mark <- Mark%>% rename (
  "mark.length.mm" = "length.mm", 
  "mark.weight.g" = "weight.g", 
  "mark.site" = "site", 
  "mark.site.detail" = "site.detail",
  "mark.sample.site" = "sample.site"
) %>% filter (tag.code != "..........")


#Recap database
Recap<- readRDS("recaptures.rds")
Recap$date <- format(Recap$date, format = "%Y-%m-%d")


Recap <-  Recap %>% filter (tag.code != "..........") %>%
  rename(
    "recap.date" = "date",
    "recap.length.mm" = "length.mm", 
    "recap.weight.g" = "weight.g", 
    "recap.site" = "site", 
    "recap.site.detail" = "site.detail",
    "recap.sample.site" = "sample.site"
  )

#Detections
Detections <- readRDS("detections.rds")
Detections$event.type <- "interrogation"
Detections$date <- format(Detections$obs.time, format = "%Y-%m-%d")
Detections$event.capturemethod <- "interrogation"
Detections <- Detections %>% rename ("event.site" = "obs.site")

unique(Detections$event.site)

#Filter detection to only include detection sites of interest
Detections <- Detections %>%
  filter(event.site %in% c("30M - Thirtymile Crk John Day Basin",
                           
                           "MJ1 - Middle Fork John Day Array",
                           "MJ2 - Middle Fork John Day Ritter",
                           
                           "SJ1 - SF John Day (Mid)", 
                           
                           "JDC - Cottonwood Creek, NF John Day",
                           
                           "JO2 - John Day North Fish Ladder", 
                           "JD1 - John Day River, McDonald Ferry",
                           "JDJ - John Day Dam Juvenile", 
                           "JO1 - John Day South Fish Ladder", 
                           "JO2 - John Day North Fish Ladder",
                           
                           "TD1 - The Dalles East Fish Ladder",
                           "TD2 - The Dalles North Fish Ladder",
                           
                           "B1J - BONNEVILLE PH1 JUVENILE",
                           "B2J - Bonneville PH2 Juvenile",
                           "B2A - BONNEVILLE ADULT WA SHORE", 
                           "BCC - BON PH2 Corner Collector",
                           "BO1 - Bonneville Bradford Is. Ladder", 
                           "BO2 - Bonneville Cascades Is. Ladder", 
                           "BO3 - Bonneville WA Shore Ladder/AFF", 
                           "BO4 - Bonneville WA Ladder Slots", 
                           "B2J - Bonneville PH2 Juvenile" , 
                           "BWL - Bonneville WA Shore Ladder",
                           "BVX - Bonneville PH1 Juvenile (Exp.)", 
                           
                           "TWX - Estuary Towed Array (Exp.)", 
                           "PD8 - Columbia River Estuary rkm 82",
                           "PD7 - Columbia River Estuary rkm 70", 
                           "PD6 - Columbia River Estuary rkm 68",
                           "PD5 - Columbia River Estuary rkm 62"
  )) %>% 
  filter (tag.code != "..........") %>%
  mutate(event.site = 
           case_when(
             event.site == "30M - Thirtymile Crk John Day Basin" & life.stage == "juvenile" ~ "out.Thirtymile",
             
             event.site == "JDC - Cottonwood Creek, NF John Day" & life.stage == "juvenile" ~ "out.Cottonwood",
             
             event.site == "MJ1 - Middle Fork John Day Array" & life.stage == "juvenile" ~ "out.Galena",
             event.site == "MJ2 - Middle Fork John Day Ritter" & life.stage == "juvenile" ~ "out.Ritter",
             
             event.site == "JD1 - John Day River, McDonald Ferry" & life.stage == "juvenile" ~ "out.McDonald",
             
             
             event.site == "JO2 - John Day North Fish Ladder" & life.stage == "juvenile" ~ "out.JDD",
             event.site == "JDJ - John Day Dam Juvenile" & life.stage == "juvenile" ~ "out.JDD",
             event.site == "JO1 - John Day South Fish Ladder" & life.stage == "juvenile" ~ "out.JDD",
             
             event.site == "TD1 - The Dalles East Fish Ladder" & life.stage == "juvenile" ~ "out.Dalles",
             event.site == "TD2 - The Dalles North Fish Ladder" & life.stage == "juvenile" ~ "out.Dalles",
             
             
             event.site == "BO1 - Bonneville Bradford Is. Ladder" & life.stage == "juvenile" ~ "out.Bonneville",
             event.site == "BCC - BON PH2 Corner Collector" & life.stage == "juvenile"    ~ "out.Bonneville",
             event.site == "BO4 - Bonneville WA Ladder Slots" & life.stage == "juvenile"   ~ "out.Bonneville",
             event.site == "B2J - Bonneville PH2 Juvenile" & life.stage == "juvenile"   ~ "out.Bonneville",
             event.site == "BO2 - Bonneville Cascades Is. Ladder" & life.stage == "juvenile"   ~ "out.Bonneville",
             event.site == "BO3 - Bonneville WA Shore Ladder/AFF" & life.stage == "juvenile"   ~ "out.Bonneville",
             
             event.site == "TWX - Estuary Towed Array (Exp.)"     ~ "out.Estuary",
             event.site == "PD8 - Columbia River Estuary rkm 82"     ~ "out.Estuary",
             event.site == "PD7 - Columbia River Estuary rkm 70"     ~ "out.Estuary",
             event.site == "PD6 - Columbia River Estuary rkm 68"     ~ "out.Estuary",
             event.site == "PD5 - Columbia River Estuary rkm 62"     ~ "out.Estuary",
             
             event.site == "BO3 - Bonneville WA Shore Ladder/AFF" & life.stage == "adult"    ~ "in.Bonneville",
             event.site == "BO4 - Bonneville WA Ladder Slots"  & life.stage == "adult"   ~ "in.Bonneville",
             event.site == "BCC - BON PH2 Corner Collector" & life.stage == "adult"    ~ "in.Bonneville",
             event.site == "BO1 - Bonneville Bradford Is. Ladder"  & life.stage == "adult"   ~ "in.Bonneville",
             event.site == "B2J - Bonneville PH2 Juvenile" & life.stage == "adult"    ~ "in.Bonneville",
             event.site == "BO2 - Bonneville Cascades Is. Ladder"  & life.stage == "adult"   ~ "in.Bonneville",
             
             event.site == "NA" ~ "NA", 
             TRUE ~ event.site
           ))

#Keep only distinct rows
Detections <- Detections %>% distinct (event.site, date, tag.code, life.stage, .keep_all = TRUE)

unique(Detections$event.site)
Initial_Col_Det <- Detections %>%
  filter(life.stage == "juvenile", 
         event.site == "out.Bonneville"|
         event.site == "out.JDD" |
           event.site == "out.Dalles"|
           event.site == "out.Estuary"
         ) %>% 
  group_by(tag.code) %>%
  summarize (
    Initial.Det.Date = min(date), 
    Initial.Det.Location = event.site [date ==(min(date))]
  )


#><((((?>`?.??.???`?.?.???`?...?><((((?>

##~~~~~~~~~~~~~~..................~~~~~~~~~~~~~~~~...............
#~~~~~~~~~~~~~~..................~~~~~~~~~~~~~~~~...............
#Start: Create an observation record for each fish

head(Mark)

RMarkDF <- Mark %>%
  select(tag.code, 
         mark.site, 
         mark.site.detail,
         mark.sample.site,
         mark.date, 
         mark.length.mm,
         mark.weight.g, 
         mark.lifestage,
         mark.capturemethod,
         mark.month, 
         mark.year, 
         migration.year,
         mark.jd,
         species)



A.Bonneville <- Detections %>% group_by (tag.code) %>% 
  filter (event.site == "in.Bonneville") %>% 
  select (tag.code)
A.Bonneville$in.Bonneville <- 1
RMarkDF <- merge (A.Bonneville, RMarkDF, by = "tag.code", all.x=TRUE, all.y=TRUE)

Estuary <- Detections %>% group_by (tag.code) %>% 
  filter (event.site == "out.Estuary") %>% 
  select (tag.code)
Estuary$out.Estuary <- 1
RMarkDF <- merge (Estuary, RMarkDF, by = "tag.code", all.x=TRUE, all.y=TRUE)

Bonneville <- Detections %>% group_by (tag.code) %>% 
  filter (event.site == "out.Bonneville") %>% 
  select (tag.code)
Bonneville$out.Bonneville <- 1
RMarkDF <- merge (Bonneville, RMarkDF, by = "tag.code", all.x=TRUE, all.y=TRUE)

Dalles <- Detections %>% group_by (tag.code) %>% 
  filter (event.site == "out.Dalles") %>% 
  select (tag.code)
Dalles$out.Dalles <- 1
RMarkDF <- merge (Dalles, RMarkDF, by = "tag.code", all.x=TRUE, all.y=TRUE)

JDD <- Detections %>% group_by (tag.code) %>% 
  filter (event.site == "out.JDD") %>% 
  select (tag.code)
JDD$out.JDD <- 1
RMarkDF <- merge (JDD, RMarkDF, by = "tag.code", all.x=TRUE, all.y=TRUE)

McDonald <- Detections %>% group_by (tag.code) %>% 
  filter (event.site == "out.McDonald") %>% 
  select (tag.code)
McDonald$out.McDonald <- 1
RMarkDF <- merge (McDonald, RMarkDF, by = "tag.code", all.x=TRUE, all.y=TRUE)

TrapsRecap <- Recap %>% group_by (tag.code) %>% 
  filter (recap.site == "JDARMF" & event.type == "Recapture") %>% 
  select (tag.code)

TrapsRecap$out.MFTrap <- 1
RMarkDF <- merge (TrapsRecap, RMarkDF, by = "tag.code", all.x=TRUE, all.y=TRUE)

Ritter <- Detections %>% group_by (tag.code) %>% 
  filter (event.site == "out.Ritter") %>% 
  select (tag.code)
Ritter$out.Ritter <- 1
RMarkDF <- merge (Ritter, RMarkDF, by = "tag.code", all.x=TRUE, all.y=TRUE)

Galena <- Detections %>% group_by (tag.code) %>% 
  filter (event.site == "out.Galena") %>% 
  select (tag.code)
Galena$out.Galena <- 1
RMarkDF <- merge (Galena, RMarkDF, by = "tag.code", all.x=TRUE, all.y=TRUE)

Thirtymile <- Detections %>% group_by (tag.code) %>% 
  filter (event.site == "out.Thirtymile") %>% 
  select (tag.code)
Thirtymile$out.Thirtymile <- 1
RMarkDF <- merge (Thirtymile, RMarkDF, by = "tag.code", all.x=TRUE, all.y=TRUE) 

Cottonwood <- Detections %>% group_by (tag.code) %>% 
  filter (event.site == "out.Cottonwood") %>% 
  select (tag.code)
Cottonwood$out.Cottonwood <- 1
RMarkDF <- merge (Cottonwood, RMarkDF, by = "tag.code", all.x=TRUE, all.y=TRUE) 


MarkDF <- data.frame(tag.code = unique(Mark$tag.code), mark = 1)
RMarkDF <- merge (MarkDF, RMarkDF, by = "tag.code", all.x=TRUE, all.y=FALSE)



#Remove duplicated tag codes. 
RMarkDF <- RMarkDF[!duplicated(RMarkDF$tag.code),]

#Assign all NA in detection columns a 0  
RMarkDF <- RMarkDF %>% mutate_at(c(3:13), ~replace_na(.,0))

#Add season to RMarkDF JD 197 = July 16
RMarkDF <- RMarkDF %>% mutate (season = case_when( mark.jd < 197 ~ "Spring", 
                                                   mark.jd >= 197 ~ "Fall"))

#Add tag.code size to each detection
RMarkDF$tag.size <- NA

RMarkDF$tag.size[stringr::str_remove(RMarkDF$tag.code, "\\.[^.]*$") == "3D6"] <- "9mm"
RMarkDF$tag.size[stringr::str_remove(RMarkDF$tag.code, "\\.[^.]*$") == "3DD"] <- "12mm"
RMarkDF$tag.size[stringr::str_remove(RMarkDF$tag.code, "\\.[^.]*$") == "384"] <- "9mm"


#Clean up workspace: 
# Remove everything except 'mark', 'detect', and 'final'
rm(list = setdiff(ls(), c("Mark", "RMarkDF", "Recap", "Initial_Col_Det")))



#######################################
######################################
#Extract records for Thirtymile
Thirtymile <- RMarkDF %>% filter (mark.site == "30ML2C")%>%
  select (tag.code, mark.site,mark.sample.site, mark.site.detail, mark.site.detail, mark.date, mark.length.mm, mark.weight.g, mark.month, mark.year, mark.jd, species, mark, out.Thirtymile, out.McDonald, out.JDD, out.Dalles, out.Bonneville, out.Estuary, in.Bonneville)



###############
###############
#Need to identify which of the Thirtymile fish are spring to fall recaps.

Thirtymile.Recap <- Recap %>% 
  filter (recap.site == "30ML2C", 
          species == "Steelhead") %>% 
  select (tag.code, recap.site, recap.sample.site, recap.site.detail, recap.date, recap.length.mm, recap.weight.g)

Thirtymile.Mark <- Mark %>% 
  filter (mark.site == "30ML2C", 
          species == "Steelhead") %>%
  select (tag.code, mark.sample.site, mark.site, mark.site.detail, species, mark.date, mark.jd) %>% 
  mutate (mark.season = case_when( mark.jd < 197 ~ "Spring", 
                                   mark.jd >= 197 ~ "Fall"))


Thirtymile.Mark.Recap <- merge(Thirtymile.Mark, Thirtymile.Recap, by = "tag.code", all.x=TRUE, all.y=TRUE)
Thirtymile.Mark.Recap$mark.date <- as.Date(Thirtymile.Mark.Recap$mark.date)
Thirtymile.Mark.Recap$recap.date <- as.Date(Thirtymile.Mark.Recap$recap.date)


Thirtymile.Spring <- Thirtymile.Mark.Recap %>% filter (mark.season == "Spring")
Thirtymile.Spring$TimeBetweenEvents <- Thirtymile.Spring$recap.date - Thirtymile.Spring$mark.date
Thirtymile.Spring$out.fallrecap <- 0

Thirtymile.Spring <- Thirtymile.Spring  %>% mutate (out.fallrecap = case_when(
  TimeBetweenEvents > 100 &  TimeBetweenEvents < 150  ~ 1, 
  TRUE ~ out.fallrecap
))


Thirtymile.Spring$TimeBetweenEvents <- NULL

Thirtymile.Fall <- Thirtymile.Mark.Recap %>% filter (mark.season == "Fall")
Thirtymile.Fall$out.fallrecap <- 0

#Bring Fall and Spring Sampling Events Back together

Thirtymile.Spring.Fall <- rbind(Thirtymile.Fall, Thirtymile.Spring)

#Remove duplicates
Thirtymile.Spring.Fall <- Thirtymile.Spring.Fall %>%
  group_by(tag.code, mark.site, mark.sample.site, mark.site.detail, species, mark.date, mark.jd, mark.season) %>%
  summarize(out.fallrecap = sum(out.fallrecap, na.rm = TRUE)) %>%
  ungroup() %>%  # Ungroup to remove grouping structure
  mutate(out.fallrecap = case_when(out.fallrecap > 0 ~ 1, TRUE ~ out.fallrecap))

Thirtymile.Spring.Fall <- Thirtymile.Spring.Fall %>%
  select(tag.code, out.fallrecap) 


Thirtymile_RMarkDF <- merge(Thirtymile, Thirtymile.Spring.Fall, by = "tag.code")
Thirtymile_RMarkDF$mark.date <- as.Date(Thirtymile_RMarkDF$mark.date)



###############
###############
#For fall recaps, treat them like fall marks for the survival analysis that only uses fall fish
Thirtymile.Fall.Recap <- Thirtymile.Spring.Fall %>% 
  filter(out.fallrecap == 1)

Recap_Fall <- Recap %>% filter (tag.code %in% Thirtymile.Fall.Recap$tag.code) %>%
  select (tag.code, recap.sample.site, recap.site.detail, recap.date, recap.length.mm, recap.weight.g) %>%
  rename (mark.site.detail = recap.site.detail, 
          mark.sample.site = recap.sample.site,
          mark.date = recap.date, 
          mark.length.mm = recap.length.mm, 
          mark.weight.g = recap.weight.g
          )

Recap_Fall$mark.date <- as.Date(Recap_Fall$mark.date)
Recap_Fall$mark.month <- format(Recap_Fall$mark.date, format = "%m")
Recap_Fall$mark.year <-  format(Recap_Fall$mark.date, format = "%Y")
Recap_Fall$mark.jd <-  format(Recap_Fall$mark.date, format = "%j")
Recap_Fall <- Recap_Fall %>% 
  filter (mark.month == "10") %>%
  distinct(tag.code, .keep_all = TRUE) #Remove duplicates

Thirtymile_RMarkDF <- Thirtymile_RMarkDF %>%
  distinct(tag.code, .keep_all = TRUE) #Remove duplicates
  
  
  
#Update information for the fall recap fish. 
# Perform the left join and update the relevant columns
Thirtymile_RMarkDF <- Thirtymile_RMarkDF %>%
  left_join(Recap_Fall %>% select(tag.code, mark.sample.site, mark.site.detail, mark.date, mark.length.mm, mark.weight.g, mark.month, mark.year, mark.jd), 
            by = "tag.code") %>%
  # Coalesce to keep the original values where Recap_Fall doesn't have a match
  mutate(
    mark.site.detail = coalesce(mark.site.detail.y, mark.site.detail.x),
    mark.sample.site = coalesce(mark.sample.site.y, mark.sample.site.x),
    mark.date = coalesce(mark.date.y, mark.date.x),
    mark.length.mm = coalesce(mark.length.mm.y, mark.length.mm.x),
    mark.weight = coalesce(mark.weight.g.y, mark.weight.g.x), 
    mark.month = coalesce(mark.month.y, mark.month.x), 
    mark.year = coalesce(mark.year.y, mark.year.x),  
    mark.jd = coalesce(mark.jd.y, mark.jd.x)
  ) %>%
  # Remove the old columns with .x and .y suffixes
  select(-ends_with(".x"), -ends_with(".y"))

# Now Thirtymile_RMarkDF contains the updated values

#################
#################
###############
###############


#Add a column to Thirtymile_RMarkDF that specifies the section 
Thirtymile_RMarkDF <-  Thirtymile_RMarkDF %>% mutate (mark.section = case_when(
  mark.sample.site == "PF16" ~ "Per3",
  mark.sample.site == "PU10" ~ "Per3",
  mark.sample.site == "PF5" ~ "Per2",
  mark.sample.site == "PF3" ~ "Per2",
  mark.sample.site == "PF15" ~ "Per3",
  mark.sample.site == "MO_OP_Above" ~ "Per1",
  mark.sample.site == "PU13" ~ "Per3",
  mark.sample.site == "MO" ~ "Per1",
  mark.sample.site == "PF4" ~ "Per2",
  mark.sample.site == "PU12" ~ "Per3",
  mark.sample.site == "PU14" ~ "Per3",
  mark.sample.site == "PW18" ~ "Wil",
  mark.sample.site == "EW17" ~ "Wil",
  mark.sample.site == "PW19" ~ "Wil",
  mark.sample.site == "EF6" ~ "Eph",
  mark.sample.site == "EF2" ~ "Eph",
  mark.sample.site == "MO_OP_Below" ~ "Per1",
  mark.sample.site == "PW_Census" ~ "Wil", 
  mark.sample.site == "Thirtymile.0003.B" ~ "Eph", 
  mark.sample.site == "Thirtymile.0003.A" ~ "Eph", 
  mark.sample.site == "EF2" ~ "Eph", 
  mark.sample.site == "Eph" ~ "Eph",
  mark.sample.site == "Sniption" ~ "Eph",
  mark.sample.site == "EF" ~ "Eph",
)) %>% 
  mutate (mark.season = case_when( mark.jd < 197 ~ "Spring", 
                                   mark.jd >= 197 ~ "Fall"))%>%
  filter (!is.na(mark.section)) #Remove any fish that weren't captured by the above mark.sections


########################
#######################
#Add column specififying if fish were observed DS of Thirtymile (ie successful out migration)
Thirtymile_RMarkDF$out_dsThirtymile <- 0

Thirtymile_RMarkDF <- Thirtymile_RMarkDF %>% mutate (out_dsThirtymile = case_when (
  out.McDonald == 1 ~ 1, 
  out.JDD == 1 ~ 1, 
  out.Dalles == 1 ~ 1, 
  out.Bonneville == 1 ~ 1, 
  out.Estuary == 1 ~ 1, 
  TRUE ~ out_dsThirtymile))


###################
###################
#Merge the initial Columbia river detections with the Thirtymile dataframe
Thirtymile_RMarkDF <- merge(Thirtymile_RMarkDF, Initial_Col_Det, all.x=TRUE, by = c("tag.code"))



##############
##############
#Add column for running survival analysis on MO_Below and MO_Above seperate
unique(Thirtymile_RMarkDF$mark.site.detail[Thirtymile_RMarkDF$mark.year == 2023 & Thirtymile_RMarkDF$mark.section == "Per1" & Thirtymile_RMarkDF$mark.season == "Fall"])

unique(Thirtymile_RMarkDF$mark.site.detail)


unique(Thirtymile_RMarkDF$mark.section)

Thirtymile_RMarkDF <- Thirtymile_RMarkDF %>% mutate(mark.section = case_when(
  mark.year == 2020 & mark.site.detail == "MO_C_1" ~ "MO_below", 
  mark.year == 2020 & mark.site.detail == "MO_C_2" ~ "MO_below", 
  mark.year == 2020 & mark.site.detail == "MO_C2" ~ "MO_below", 
  mark.year == 2020 & mark.site.detail == "MO_C_3" ~ "MO_above", 
  mark.year == 2020 & mark.site.detail == "MO_C_4" ~ "MO_above", 
  
  mark.year == 2021 & mark.site.detail == "MO_D_1" ~ "MO_below", 
  mark.year == 2021 & mark.site.detail == "MO_D_2" ~ "MO_above", 
  mark.year == 2021 & mark.site.detail == "MO_D_3" ~ "MO_above", 
  mark.year == 2021 & mark.site.detail == "MO_D_4" ~ "MO_above", 
  mark.year == 2021 & mark.site.detail == "MO_D_5" ~ "MO_above", 
  
  mark.year == 2022 & mark.site.detail == "MO_E_1" ~ "MO_below", 
  mark.year == 2022 & mark.site.detail == "MO_E_2" ~ "MO_below", 
  mark.year == 2022 & mark.site.detail == "MO_E_3" ~ "MO_above", 
  mark.year == 2022 & mark.site.detail == "MO_E_4" ~ "MO_above", 
  mark.year == 2022 & mark.site.detail == "MO_E_5" ~ "MO_above", 
  mark.year == 2022 & mark.site.detail == "MO_OP_Above" ~ "MO_above", 
  mark.year == 2022 & mark.site.detail == "MO_OP_Below" ~ "MO_below", 
  
  mark.year == 2023 & mark.site.detail == "MO_F_1" ~ "MO_below", 
  mark.year == 2023 & mark.site.detail == "MO_F_2" ~ "MO_below", 
  mark.year == 2023 & mark.site.detail == "MO_F_3" ~ "MO_above", 
  mark.year == 2023 & mark.site.detail == "MO_F_4" ~ "MO_above", 
  mark.year == 2023 & mark.site.detail == "MO_F_5" ~ "MO_above",
  mark.year == 2023 & mark.site.detail == "MO_F_6" ~ "MO_above",
  
  mark.year == 2017 & mark.section == "Per1" ~ "MO_below", 
  mark.year == 2018 & mark.section == "Per1" ~ "MO_below", 
  mark.year == 2019 & mark.section == "Per1" ~ "MO_below", 
  
  
  mark.site.detail == "PF4_A" ~ "PF4", 
  mark.site.detail == "PF4_B" ~ "PF4", 
  mark.site.detail == "PF4_USC_A" ~ "PF4", 
  TRUE ~ mark.section
))

#Check for duplicates
duplicates <- Thirtymile_RMarkDF %>%
  filter(duplicated(tag.code) | duplicated(tag.code, fromLast = TRUE))


setwd("D:/2024_30Mile_Pub/R/Thirtymile_SMB_reduction/Survival/RMark_DataFrame")
saveRDS(Thirtymile_RMarkDF, "Thirtymile_RMarkDF.rds")
Thirtymile <- readRDS("Thirtymile_RMarkDF.rds")

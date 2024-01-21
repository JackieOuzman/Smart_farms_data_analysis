## MERGE THE REGRESSION DATA FOR ALL SITES

library(tidyverse)

Coomandook <- read.csv("X:/Therese_Jackie/smart_farms/sites/Coomandook/Coomandook_pen_vs_em_2023.csv", skip=2)

################################################################################
Coomandook <- Coomandook %>% rename(
  Peak_Resistance = "max..Peak..resistance.value.up.to.50cm"    , #a
  Depth_to_peak = "location.in.the.profile.of.first.peak..up.to.50cm."   , #b 
  Depth_to_2.5MPa ="The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.50cm"  , #c
  Area_under_curve_to_2.5MPa ="Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.", #d
  Area_under_curve_to_50cm ="X0.50" #e
)
Coomandook <- Coomandook %>% select (-X, -notes, -X.1 )
Coomandook <- Coomandook %>% mutate(site = "Coomandook")
################################################################################

Boordaroo <- read.csv("X:/Therese_Jackie/smart_farms/sites/Boordaroo/Boordaroo_pen_2023_for_analysis.csv", skip=2)

Boordaroo <- Boordaroo %>% rename(
  Peak_Resistance = "max..Peak..resistance.value.up.to.50cm"    , #a
  Depth_to_peak = "location.in.the.profile.of.first.peak..up.to.50cm."   , #b 
  Depth_to_2.5MPa ="The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.50cm"  , #c
  Area_under_curve_to_2.5MPa ="Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.", #d
  Area_under_curve_to_50cm ="X0.50" #e
)
names(Boordaroo)
Boordaroo <- Boordaroo %>% mutate(site = "Boordaroo")
Boordaroo <- Boordaroo %>% select (-X, -X.1 ,-notes)
################################################################################



################################################################################
Charlies <- read.csv("X:/Therese_Jackie/smart_farms/sites/Bute_Charlie/Penetrometer/Bute_Site_Charlie_2023_pen for anlaysis.csv", skip=2)
Charlies <- Charlies %>% rename(
  Peak_Resistance = "max..Peak..resistance.value.up.to.50cm"    , #a
  Depth_to_peak = "location.in.the.profile.of.first.peak..up.to.50cm."   , #b 
  Depth_to_2.5MPa ="The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.50cm"  , #c
  Area_under_curve_to_2.5MPa ="Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.", #d
  Area_under_curve_to_50cm ="X0.50", #e
  EM05m = "Shallow" ,                                                              
  EM1m = "Deep"
)
names(Charlies)
Charlies <- Charlies %>% select (-X, -"Sam_ID"  , - "Distianct_to.Rip_area",  -"If_Inside_rip_area")
Charlies <- Charlies %>% mutate(site = "Charlies")
##############################################################################



##############################################################################
Walpeup <- read.csv("X:/Therese_Jackie/smart_farms/sites/Walpeup/Walpeup_EM_PEN_for _analysis.csv", skip=2)
Walpeup <- Walpeup %>% rename(
  Peak_Resistance = "max..Peak..resistance.value.up.to.50cm"    , #a
  Depth_to_peak = "location.in.the.profile.of.first.peak..up.to.50cm."   , #b 
  Depth_to_2.5MPa ="The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.50cm"  , #c
  Area_under_curve_to_2.5MPa ="Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.", #d
  Area_under_curve_to_50cm ="X0.50", #e
  EM05m = "EM_05m_PRE" ,                                                              
  EM1m = "EM_1m_PRED",
  Group = Insertion
)
names(Walpeup)
#Walpeup <- Walpeup %>% select (-X, -"Sam_ID"  , - "Distianct_to.Rip_area",  -"If_Inside_rip_area")
Walpeup <- Walpeup %>% mutate(site = "Walpeup")



##############################################################################
Bute <- read.csv("X:/Therese_Jackie/smart_farms/sites/Bute/EM_pts_Pen_for_analysis.csv", skip=2)
names(Bute)
Bute <- Bute %>% rename(
  Peak_Resistance = "max..Peak..resistance.value.up.to.50cm"    , #a
  Depth_to_peak = "location.in.the.profile.of.first.peak..up.to.50cm."   , #b 
  Depth_to_2.5MPa ="The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.50cm"  , #c
  Area_under_curve_to_2.5MPa ="Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.", #d
  Area_under_curve_to_50cm ="X0.50" ,
  Group = Grid_no.,
  `X50.72.5` =`X50.70` 
)
names(Bute)

Bute <- Bute %>% mutate(site = "Bute")

names(Bute)
names(merge_step3)
merge_step <- rbind(Boordaroo,Coomandook, Charlies , Walpeup, Bute) ## SITES WITH EM
str(merge_step)
merge_step <-merge_step %>% filter(!is.na(Group))


write.csv(merge_step,
          file = "X:/Therese_Jackie/smart_farms/sites/merge_step_EM_sites_Pen_all.csv",
          row.names = FALSE)





Lameroo <- read.csv("X:/Therese_Jackie/smart_farms/sites/Wikarie/Lameroo Pentrometer 1.3.23/Lameroo Pentrometer March 2023 Jaxs.csv", skip=0)
names(Lameroo)
Lameroo <- Lameroo %>% rename(
  Peak_Resistance = "max..Peak..resistance.value.up.to.50cm"    , #a
  Depth_to_peak = "location.in.the.profile.of.first.peak..up.to.50cm."   , #b 
  Depth_to_2.5MPa ="The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.50cm"  , #c
  Area_under_curve_to_2.5MPa ="Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.", #d
  Area_under_curve_to_50cm ="X0.50" ,
  Median.resistance.value.for.the.profile = medium.resistance.value.for.the.profile
 
)

Lameroo <- Lameroo %>% mutate(site = "Lameroo")
Lameroo <- Lameroo %>% select(-comments)

Lameroo$Peak_Resistance   <- as.double(Lameroo$Peak_Resistance )
Lameroo$Depth_to_peak     <- as.double(Lameroo$Depth_to_peak)  
Lameroo$Depth_to_2.5MPa <- as.double(Lameroo$Depth_to_2.5MPa)
Lameroo$Area_under_curve_to_2.5MPa  <- as.double(Lameroo$Area_under_curve_to_2.5MPa)


Kooloonong <- read.csv("X:/Therese_Jackie/smart_farms/sites/Kooloonong/data_processing_jaxs/Penetrometer2022/Kooloonong penetrometer readings 2022.csv", skip=2)

names(Kooloonong)

Kooloonong <- Kooloonong %>% rename(
  Peak_Resistance = "max..Peak..resistance.value.up.to.50cm"    , #a
  Depth_to_peak = "location.in.the.profile.of.first.peak..up.to.50cm."   , #b
  Depth_to_2.5MPa ="The.depth.when.resistance.first.exceeds.2.5MPa.to.depth.of.50cm"  , #c
  Area_under_curve_to_2.5MPa ="Sum.the.area.of.the.curve.until.2.5MPa.is.first.reached..up.to.50cm.", #d
  Area_under_curve_to_50cm ="X0.50", #e
  ECa1 ="ECa_1",
  ECa2 ="ECa_2",
  ECa3 ="ECa_3" ,
  ECa4 ="ECa_4" , 
  
  Tillage  = Krig_Tilla,
  D2I = "Krig_D2I"
  
)

Kooloonong <- Kooloonong %>% mutate(site = "Kooloonong")
Kooloonong <- Kooloonong %>% select(-comments)
names(Kooloonong)
names(Lameroo)
merge_step_TopSoil <- rbind(Kooloonong,Lameroo) ## SITES WITH Top soil mapper


write.csv(merge_step_TopSoil,
          file = "X:/Therese_Jackie/smart_farms/sites/merge_step_Top_soil_sites_Pen_all.csv",
          row.names = FALSE)

